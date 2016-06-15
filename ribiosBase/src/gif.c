/*****************************************************************************
* (c) Copyright 2012-2013 F.Hoffmann-La Roche AG                             *
* Contact: bioinfoc@bioinfoc.ch, Detlef.Wolf@Roche.com.                      *
*                                                                            *
* This file is part of BIOINFO-C. BIOINFO-C is free software: you can        *
* redistribute it and/or modify it under the terms of the GNU Lesser         *
* General Public License as published by the Free Software Foundation,       *
* either version 3 of the License, or (at your option) any later version.    *
*                                                                            *
* BIOINFO-C is distributed in the hope that it will be useful, but           *
* WITHOUT ANY WARRANTY; without even the implied warranty of                 *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
* Lesser General Public License for more details. You should have            *
* received a copy of the GNU Lesser General Public License along with        *
* BIOINFO-C. If not, see <http://www.gnu.org/licenses/>.                     *
*****************************************************************************/
/** @file gif.c
    @brief Creates GIF graphics.
    With the help of
    Die grosse Welt der Grafikformate
    Grafikprogrammierung unter Windows und WindowNT
    Thomas W. Lipp
    Synergy book ISBN 3-9803718-0-8.
    Module prefix gif_
*/
#include <R.h>

#include "log.h"
#include "format.h"
#include "hlrmisc.h"
#include "rofutil.h"
#include "bitmap.h"
#include "gif.h"

/// structure to hold a GIf picture
typedef struct {
  unsigned int uFormat; //!< the format GIF87A or GIF89A
  long bGlobalColorMap; //!< the color map
  long bInterlaced; //!< whether interlaced or not
  long bGrayscale24; //!< whether 24 bit grayscale or not
} GifStruct;

/// one of the formats
#define GIF87A 87
/// one of the formats
#define GIF89A 89
/// used for encoding
#define NO_LZW_CODE 4096
/// used for encoding
#define PREFIXTABLE 12

static GifStruct gif;

static FILE *fp;
static unsigned char *lpRaster;
static unsigned char *lpFill;
static unsigned int *lpPrefixTable;
static unsigned char *lpStack;
static unsigned int uDataSize;
static unsigned int uCodeSize;
static unsigned int uCodeMask;
static unsigned int uClear;
static unsigned int uEOI;
static unsigned int uPrefix[4096];
static unsigned char cSuffix[4096];
static unsigned char cStack[4096];
static unsigned int uAvail;
static unsigned int uOldCode;
static unsigned int transparent = -1;

static long decodeLZW (unsigned int uCode) {
  static unsigned char cFirstChar;
  unsigned int uInCode;

  if (uCode == uClear) {
    uCodeSize = uDataSize + 1;
    uCodeMask = (1 << uCodeSize) - 1;
    uAvail = uClear + 2;
    uOldCode = NO_LZW_CODE;
    return 1;
  }
  if (uCode > uAvail)
    die ("decodeLZW: Invalid LZW code %d\n",uCode);
  if (uOldCode == NO_LZW_CODE) {
    cFirstChar = cSuffix[uCode];
    *lpFill++ = cFirstChar;
    uOldCode = uCode;
    return 1;
  }
  uInCode = uCode;
  if (uCode == uAvail) {
    *lpStack++ = cFirstChar;
    uCode = uOldCode;
  }
  while (uCode > uClear) {
    *lpStack++ = cSuffix[uCode];
    uCode = uPrefix[uCode];
  }
  cFirstChar = cSuffix[uCode];
  *lpStack++ = cFirstChar;
  uPrefix[uAvail] = uOldCode;
  cSuffix[uAvail] = cFirstChar;
  if (uAvail < 4096)
    uAvail++;
  if (((uAvail & uCodeMask) == 0) && (uAvail < 4096)) {
    uCodeSize++;
    uCodeMask += uAvail;
  }
  uOldCode = uInCode;
  do {
    *lpFill++ = *--lpStack;
  }
  while (lpStack > cStack);
  return 1;
}

static long readRaster (unsigned int uWidth,unsigned int uHeight) {
  static unsigned char cBuffer[256];
  register unsigned int uBits = 0;
  unsigned char uCount = 0;
  register unsigned long dwDatum = 0l;
  register unsigned char *lpByte;
  register unsigned int uCode = 0;
  long bResult;
  unsigned char tempChar;

  lpFill = lpRaster;
  uDataSize = 0;
  fread (&tempChar,1,1,fp);
  uDataSize = tempChar;
  uClear = 1 << uDataSize;
  uEOI = uClear + 1;
  uAvail = uClear + 2;
  uOldCode = NO_LZW_CODE;
  uCodeSize = uDataSize + 1;
  uCodeMask = (1 << uCodeSize) - 1;
  for (uCode=0;uCode<uClear;uCode++) {
    uPrefix[uCode] = NO_LZW_CODE;
    cSuffix[uCode] = (unsigned char)uCode;
  }
  lpStack = cStack;
  bResult = 1;
  fread (&uCount,1,1,fp);
  while (uCount > 0) {
    if (fread (cBuffer,1,uCount,fp) != uCount)
      REprintf("Error in reading loop\n");
    if (bResult) {
      for (lpByte=cBuffer;uCount-- > 0;lpByte++) {
        dwDatum += (unsigned long)*lpByte << uBits;
        uBits += 8;
        while (uBits >= uCodeSize) {
          uCode = (unsigned int)dwDatum & uCodeMask;
          dwDatum >>= uCodeSize;
          uBits -= uCodeSize;
          if (uCode == uEOI)
            goto exitloop;
          bResult = decodeLZW (uCode);
          if (!bResult)
            uCount = 0;
        }
      }
    }
    uCount = 0;
    fread (&uCount,1,1,fp);
  }
  if (lpFill != lpRaster + (unsigned long)uWidth * uHeight)
    die ("readRaster: Error in LZW part of GIF image");
exitloop:
  return bResult;
}

static void rasterizeGIFtoDIB (unsigned char *lpBits,unsigned int uRow,
                               unsigned int uWidth,unsigned int uCopy,
                               unsigned int uBitPerPixel) {
  unsigned char *lpScanline;
  register unsigned int u;

  lpScanline = lpRaster + (unsigned long)uRow * (unsigned long)uWidth;
  switch (uBitPerPixel) {
    case 1:
      for (u=0;u<uCopy / 8;u++) {
        *lpBits = *lpScanline++ << 7;
        *lpBits |= *lpScanline++ << 6;
        *lpBits |= *lpScanline++ << 5;
        *lpBits |= *lpScanline++ << 4;
        *lpBits |= *lpScanline++ << 3;
        *lpBits |= *lpScanline++ << 2;
        *lpBits |= *lpScanline++ << 1;
        *lpBits++ |= *lpScanline++;
      }
      if (uCopy & 7) {
        *lpBits = (unsigned char)0;
        for (u=uCopy & 7;u!=0;u--)
          *lpBits |= (*lpScanline++ << u) + 7 - (uCopy & 7);
      }
      break;
    case 4:
      for (u=0;u<uCopy / 2;u++) {
        *lpBits = *lpScanline++ << 4;
        *lpBits++ |= *lpScanline++;
      }
      if (uWidth & 1)
        *lpBits = *lpScanline++ << 4;
      break;
    case 8:
      for (u=0;u<uCopy;u++)
        *lpBits++ = *lpScanline++;
      break;
  }
}

static long readImage (BitmapObject bmo) {
  unsigned char *lpBits;
  unsigned short wLeft;
  unsigned short wTop;
  unsigned short wWidth;
  unsigned short wHeight;
  unsigned char cIDStatus;
  unsigned char cBuffer[3];
  long bResult;
  register unsigned int uRow;
  register unsigned int u;
  unsigned int uColors;
  unsigned int uCopy;
  unsigned long dwIncrement;
  unsigned long dwSize;
  unsigned char tempByte;
  unsigned char *raster;
  int *hInterlaceTable;

  fread (&tempByte,1,1,fp);
  wLeft = tempByte;
  fread (&tempByte,1,1,fp);
  wLeft += 256 * tempByte;
  fread (&tempByte,1,1,fp);
  wTop = tempByte;
  fread (&tempByte,1,1,fp);
  wTop += 256 * tempByte;
  fread (&tempByte,1,1,fp);
  wWidth = tempByte;
  fread (&tempByte,1,1,fp);
  wWidth += 256 * tempByte;
  fread (&tempByte,1,1,fp);
  wHeight = tempByte;
  fread (&tempByte,1,1,fp);
  wHeight += 256 * tempByte;
  fread (&cIDStatus,1,1,fp);
  if (cIDStatus & 0x80) {
    uColors = 1 << ((cIDStatus & 0x07) + 1);
    if (uColors > (unsigned int)(1 << bmo->bitCount))
      die ("readImage: Wrong color information in image descriptor");
    bmo->clrImportant = uColors;
    for (u=0;u<uColors;u++) {
      fread (cBuffer,1,3,fp);
      bmo->colTable[u].red = cBuffer[0];
      bmo->colTable[u].green = cBuffer[1];
      bmo->colTable[u].blue = cBuffer[2];
    }
  }
  else
    if (!bmo->clrImportant)
      die ("readImage: GIF picture does not contain color map");
  dwSize = (unsigned long)wWidth * (unsigned long)wHeight + 256l;
  raster = (unsigned char *)hlr_malloc (dwSize);
  if (!raster)
    die ("readImage: No memory for raster");
  lpRaster = raster;
  bResult = readRaster (wWidth, wHeight);
  if (!bResult) {
    hlr_free (raster);
    return 0;
  }
  lpBits = bmo->bitmap;
  dwIncrement = ((bmo->width * bmo->bitCount + 31) / 32) * 4;
  wLeft = (wLeft * bmo->bitCount / 8) * 8 / bmo->bitCount;
  if ((long)(wTop + wHeight) > bmo->height) {
    if ((long)wTop < bmo->height)
      wHeight = (unsigned short)(unsigned int)(bmo->height - wTop);
    else {
      hlr_free (raster);
      return 1;
    }
  }
  if ((long)(wLeft + wWidth) > bmo->width) {
    if ((long)wLeft < bmo->width)
      uCopy = (unsigned int)(bmo->width - wLeft);
    else {
      hlr_free (raster);
      return 1;
    }
  }
  else
    uCopy = wWidth;
  lpBits += wLeft * bmo->bitCount / 8;
  lpBits += dwIncrement * (bmo->height - wHeight - wTop);
  if (cIDStatus & 0x40) {
    hInterlaceTable = (int *)hlr_malloc (wHeight * sizeof (int));
    if (!hInterlaceTable)
      die ("readImage: No memory for interlace table");
    uRow = 0;
    for (u=wTop;u<(unsigned int)(wTop + wHeight);u += 8)
      hInterlaceTable[u] = uRow++;
    for (u=wTop + 4;u<(unsigned int)(wTop + wHeight);u += 8)
      hInterlaceTable[u] = uRow++;
    for (u=wTop + 2;u<(unsigned int)(wTop + wHeight);u += 4)
      hInterlaceTable[u] = uRow++;
    for (u=wTop + 1;u<(unsigned int)(wTop + wHeight);u += 2)
      hInterlaceTable[u] = uRow++;
    for (uRow=wHeight;uRow > 0;) {
      rasterizeGIFtoDIB (lpBits,hInterlaceTable[--uRow],
                         wWidth,uCopy,bmo->bitCount);
      lpBits += dwIncrement;
    }
    hlr_free (hInterlaceTable);
  }
  else {
    for (uRow=wHeight;uRow>0;) {
      rasterizeGIFtoDIB (lpBits,--uRow,wWidth,uCopy,
                         bmo->bitCount);
      lpBits += dwIncrement;
    }
  }
  hlr_free (raster);
  return 1;
}

static void readExtension (void) {
  unsigned char cCode;
  unsigned char cCount;
  unsigned char cBuffer[256];

  fread (&cCode,1,1,fp);
  while (fread (&cCount,1,1,fp) ? cCount : 0)
  fread (cBuffer,1,cCount,fp);
}

BitmapObject gif_read (char *fn) {
  /**
     Reads a gif from a file
     @param[in] fn - the name of the file
     @return a BitmapObject
  */
  unsigned char cBuffer[6];
  unsigned char cSDStatus;
  unsigned char cByte;
  unsigned char cBackgrnd;
  long bResult;
  long bQuit = 0;
  unsigned short wWidth;
  unsigned short wHeight;
  unsigned int uBitPerPixel;
  unsigned int uColors;
  unsigned int u;
  unsigned char tempByte;
  BitmapObject bmo;

  fp = fopen (fn,"r");
  if (fp == NULL)
    die ("gif_read: Cannot open GIF file %s",fn);
  transparent = -1;
  fread (cBuffer,1,6,fp);
  if (!strStartsWithC ((char *)cBuffer,"GIF"))
    die ("gif_read: Not a GIF image");
  if (strNEqual ((char *)(cBuffer+3),"87a",3))
    gif.uFormat = GIF87A;
  else if (strNEqual ((char *)(cBuffer+3),"89a",3))
    gif.uFormat = GIF89A;
  else
    die ("gif_read: Unknown GIF Version");
  fread (&tempByte,1,1,fp);
  wWidth = tempByte;
  fread (&tempByte,1,1,fp);
  wWidth += 256 * tempByte;
  fread (&tempByte,1,1,fp);
  wHeight = tempByte;
  fread (&tempByte,1,1,fp);
  wHeight += 256 * tempByte;
  fread (&cSDStatus,1,1,fp);
  fread (&cBackgrnd,1,1,fp);
  fread (&cByte,1,1,fp);
  uBitPerPixel = (cSDStatus & 0x07) + 1;
  if (uBitPerPixel != 1) {
    if (uBitPerPixel <= 4)
      uBitPerPixel = 4;
    else
      uBitPerPixel = 8;
  }
  bmo = bm_create (wWidth,wHeight,uBitPerPixel);
  if (bmo == NULL)
    die ("gif_read: Could not create bitmap");
  if (cSDStatus & 0x80) {
    gif.bGlobalColorMap = 1;
    uColors = 1 << ((cSDStatus & 0x07) + 1);
    bmo->clrImportant = uColors;
    for (u=0;u<uColors;u++) {
      fread (cBuffer,1,3,fp);
      bmo->colTable[u].red = cBuffer[0];
      bmo->colTable[u].green = cBuffer[1];
      bmo->colTable[u].blue = cBuffer[2];
    }
  }
  do {
    cByte = '\0';
    if (fread (&cByte,1,1,fp) != 1)
      die ("gif_read: Unexpected end of GIF file");
    switch (cByte) {
      case '\0':
        break;
      case ',' :
        bResult = readImage (bmo);
        if (!bResult)
          bQuit = 1;
        break;
      case ';' :
        bQuit = 1;
        break;
      case '!' :
        readExtension ();
        break;
      default :
        warn ("Illegal GIF block!");
        bQuit = 1;
        break;
    }
  }
  while (!bQuit);
  fclose (fp);
  return bmo;
}

static void writeCode (unsigned int uCode,unsigned int uLength) {
  static unsigned char cBuffer[256];
  static unsigned char uByte;
  static unsigned int uOffset;
  unsigned long dwShift;
  unsigned int uBits;
  unsigned int u;

  if (uCode == NO_LZW_CODE && uLength == 0) {
    for (u=1;u<256;u++)
      cBuffer[u] = (unsigned char)0;
    uOffset = 0;
    uByte = 1;
    return;
  }
  uBits = uOffset + uLength;
  dwShift = (unsigned long)uCode;
  dwShift <<= uOffset;
  cBuffer[uByte] |= (dwShift & 0x000000ff);
  if (uByte == 255 && uBits >= 8) {
    cBuffer[0] = (unsigned char)255;
    fwrite (cBuffer,1,256,fp);
    for (u=1;u<256;u++)
      cBuffer[u] = (unsigned char)0;
    uByte = 0;
  }
  if (uBits >= 8)
    uByte++;
  if (uBits > 8)
    cBuffer[uByte] = (unsigned char)((dwShift & 0x0000ff00) >> 8);
  if (uByte == 255 && uBits >= 16) {
    cBuffer[0] = (unsigned char)255;
    fwrite (cBuffer,1,256,fp);
    for (u=1;u<256;u++)
      cBuffer[u] = (unsigned char)0;
    uByte = 0;
  }
  if (uBits >= 16)
    uByte++;
  if (uBits > 16)
    cBuffer[uByte] = (unsigned char)((dwShift & 0x00ff0000) >> 16);
  uOffset = uBits & 7;
  if (uCode == uEOI) {
    if (uOffset == 0)
      uByte--;
    cBuffer[0] = uByte;
    fwrite (cBuffer,1,uByte + 1,fp);
    cBuffer[0] = (unsigned char)0;
    fwrite (cBuffer,1,1,fp);
  }
}

static unsigned int getCodeLength (unsigned int uCode) {
  if (uCode & 2048)
    return 12;
  if (uCode & 1024)
    return 11;
  if (uCode & 512)
    return 10;
  if (uCode & 256)
    return 9;
  if (uCode & 128)
    return 8;
  if (uCode & 64)
    return 7;
  if (uCode & 32)
    return 6;
  if (uCode & 16)
    return 5;
  if (uCode & 8)
    return 4;
  return 3;
}

static void encodeLZW (unsigned int uCode)
{
  static unsigned int uCurrentPrefix;
  static unsigned char cCurrentSuffix;
  unsigned int uFound;
  long bFound;
  long bPrefixTableFull;
  register unsigned int u;

  if (uCode == uClear) {
    uCurrentPrefix = NO_LZW_CODE;
    uCodeSize = uDataSize + 1;
    writeCode (NO_LZW_CODE,0);
    writeCode (uCode,uCodeSize);
    return;
  }
  if (uCode == uEOI) {
    writeCode (uCurrentPrefix,uCodeSize);
    writeCode (uCode,uCodeSize);
    return;
  }
  cCurrentSuffix = (unsigned char)uCode;
  bFound = 0;
  bPrefixTableFull = 1;
  if (uCurrentPrefix != NO_LZW_CODE) {
    for (u=0;u<PREFIXTABLE && bPrefixTableFull;u++) {
      uFound = lpPrefixTable[uCurrentPrefix * PREFIXTABLE + u];
      if (uFound != NO_LZW_CODE) {
        if (cSuffix[uFound] == cCurrentSuffix) {
          bFound = 1;
          u = PREFIXTABLE;
        }
      }
      else
        bPrefixTableFull = 0;
    }
    if (!bPrefixTableFull)
      uFound = NO_LZW_CODE;
  }
  if (uCurrentPrefix == NO_LZW_CODE || (!bFound && bPrefixTableFull)) {
    uFound = NO_LZW_CODE;
    if (uCurrentPrefix == NO_LZW_CODE)
      u = 0;
    else
      u = uClear + 2;
    while (uFound == NO_LZW_CODE && u < uAvail)
      if (uPrefix[u] == uCurrentPrefix &&
          cSuffix[u] == cCurrentSuffix)
        uFound = u;
      else
        u++;
  }
  if (uFound != NO_LZW_CODE)
    uCurrentPrefix = uFound;
  else {
    if (uAvail == NO_LZW_CODE) {
      writeCode (uCurrentPrefix,uCodeSize);
      uAvail = uClear + 2;
      for (u=0;u<uAvail;u++) {
        uPrefix[u] = NO_LZW_CODE;
        cSuffix[u] = u;
      }
      for (u=0;u<(unsigned int)4096 * PREFIXTABLE;u++)
        lpPrefixTable[u] = NO_LZW_CODE;
      writeCode (uClear,uCodeSize);
      uCodeSize = uDataSize + 1;
      uCurrentPrefix = uCode;
    }
    else {
      uPrefix[uAvail] = uCurrentPrefix;
      cSuffix[uAvail] = cCurrentSuffix;
      for (u=0;u<PREFIXTABLE;u++)
        if (lpPrefixTable[uCurrentPrefix * PREFIXTABLE + u] ==
            NO_LZW_CODE) {
          lpPrefixTable[uCurrentPrefix * PREFIXTABLE + u] = uAvail;
          u = PREFIXTABLE;
        }
      writeCode (uCurrentPrefix,uCodeSize);
      uCodeSize = getCodeLength (uAvail);
      uAvail++;
      uCurrentPrefix = cCurrentSuffix;
    }
  }
}

static long writeRaster (unsigned int uWidth,unsigned int uHeight) {
  unsigned int uCode;
  unsigned int uX;
  unsigned int uY;
  unsigned int *hPrefixTable;

  hPrefixTable = (unsigned int *)hlr_malloc (4096 * PREFIXTABLE * sizeof (unsigned int));
  if (!hPrefixTable)
    die ("writeRaster: No memory for prefix table");
  lpPrefixTable = hPrefixTable;
  uClear = 1 << uDataSize;
  uEOI = uClear + 1;
  uAvail = uClear + 2;
  for (uCode=0;uCode<uAvail;uCode++) {
    uPrefix[uCode] = NO_LZW_CODE;
    cSuffix[uCode] = uCode;
  }
  for (uCode=0;uCode<(unsigned int)4096 * PREFIXTABLE;uCode++)
    hPrefixTable[uCode] = NO_LZW_CODE;
  encodeLZW (uClear);
  for (uY=0;uY<uHeight;uY++) {
    for (uX=0;uX<uWidth;uX++)
      encodeLZW (*lpRaster++);
  }
  encodeLZW (uEOI);
  hlr_free (hPrefixTable);
  return 1;
}

static void rasterizeDIBtoGIF (unsigned char *lpBits,unsigned int uRow,
                               unsigned int uWidth,unsigned int uBitPerPixel) {
  unsigned char *lpScanline;
  register unsigned int u;

  lpScanline = lpRaster + (unsigned long)uRow * (unsigned long)uWidth;
  switch (uBitPerPixel) {
    case 1:
      for (u=0;u<uWidth / 8;u++) {
        *lpScanline++ = (*lpBits >> 7) & 1;
        *lpScanline++ = (*lpBits >> 6) & 1;
        *lpScanline++ = (*lpBits >> 5) & 1;
        *lpScanline++ = (*lpBits >> 4) & 1;
        *lpScanline++ = (*lpBits >> 3) & 1;
        *lpScanline++ = (*lpBits >> 2) & 1;
        *lpScanline++ = (*lpBits >> 1) & 1;
        *lpScanline++ = *lpBits++ & 1;
      }
      for (u=uWidth & 7;u!=0;u--)
        *lpScanline++ = ((*lpBits >> u) + 7 - (uWidth & 7)) & 1;
      break;
    case 4:
      for (u=0;u<uWidth / 2;u++) {
        *lpScanline++ = (*lpBits >> 4) & 0x0F;
        *lpScanline++ = *lpBits++ & 0x0F;
      }
      if (uWidth & 1)
        *lpScanline = (*lpBits >> 4) & 0x0F;
      break;
    case 8:
      for (u=0;u<uWidth;u++)
        *lpScanline++ = *lpBits++;
      break;
  }
}

static long writeImage (BitmapObject bmo) {
  unsigned char *lpBits;
  unsigned short wLeft;
  unsigned short wTop;
  unsigned short wWidth;
  unsigned short wHeight;
  unsigned char cIDStatus;
  unsigned char cImageSep;
  unsigned char cBuffer[3];
  long bResult;
  register unsigned int uRow;
  register unsigned int u;
  unsigned int uColors;
  unsigned long dwIncrement;
  unsigned long dwSize;
  unsigned char tempByte;
  unsigned char *raster;
  int *hInterlaceTable;

  cImageSep = (unsigned char)',';
  wLeft = 0;
  wTop = 0;
  wWidth = (unsigned short)(unsigned int)bmo->width;
  wHeight = (unsigned short)(unsigned int)bmo->height;
  cIDStatus = (unsigned char)((!gif.bGlobalColorMap ? 128 : 0) +
                              (gif.bInterlaced ? 64 : 0) +
                              bmo->bitCount - 1);
  fwrite (&cImageSep,1,1,fp);
  tempByte = wLeft;
  fwrite (&tempByte,1,1,fp);
  tempByte = wLeft >> 8;
  fwrite (&tempByte,1,1,fp);
  tempByte = wTop;
  fwrite (&tempByte,1,1,fp);
  tempByte = wTop >> 8;
  fwrite (&tempByte,1,1,fp);
  tempByte = wWidth;
  fwrite (&tempByte,1,1,fp);
  tempByte = wWidth >> 8;
  fwrite (&tempByte,1,1,fp);
  tempByte = wHeight;
  fwrite (&tempByte,1,1,fp);
  tempByte = wHeight >> 8;
  fwrite (&tempByte,1,1,fp);
  fwrite (&cIDStatus,1,1,fp);
  if (!gif.bGlobalColorMap) {
    if (bmo->clrUsed)
      uColors = (unsigned int)bmo->clrUsed;
    else
      uColors = (unsigned int)1 << bmo->bitCount;
    for (u=0;u<uColors;u++) {
      cBuffer[0] = bmo->colTable[u].red;
      cBuffer[1] = bmo->colTable[u].green;
      cBuffer[2] = bmo->colTable[u].blue;
      fwrite (cBuffer,1,3,fp);
    }
    if (bmo->clrUsed) {
      cBuffer[0] = (unsigned char)0;
      cBuffer[1] = (unsigned char)0;
      cBuffer[2] = (unsigned char)0;
      for (u = (unsigned int)bmo->clrUsed;
           u < (unsigned int)(1 << bmo->bitCount);
           u++)
        fwrite (cBuffer,1,3,fp);
    }
  }
  dwSize = (unsigned long)wWidth * (unsigned long)wHeight;
  raster = (unsigned char *)hlr_malloc (dwSize);
  if (!raster)
    die ("writeImage: No memory for raster");
  lpRaster = raster;
  if (bmo->clrUsed)
    uColors = (unsigned int)bmo->clrUsed;
  else
    uColors = (unsigned int)(1 << bmo->bitCount);
  lpBits = bmo->bitmap;
  dwIncrement = ((bmo->width * bmo->bitCount + 31) / 32) * 4;
  if (gif.bInterlaced) {
    hInterlaceTable = (int *)hlr_malloc (wHeight * sizeof (int));
    if (!hInterlaceTable)
      die ("writeImage: No memory for interlace table");
    uRow = 0;
    for (u=0;u<wHeight;u+=8)
      hInterlaceTable[u] = uRow++;
    for (u=4;u<wHeight;u+=8)
      hInterlaceTable[u] = uRow++;
    for (u=2;u<wHeight;u+=4)
      hInterlaceTable[u] = uRow++;
    for (u=1;u<wHeight;u+=2)
      hInterlaceTable[u] = uRow++;
    for (uRow=wHeight;uRow>0;) {
      rasterizeDIBtoGIF (lpBits,hInterlaceTable[--uRow],wWidth,
                         bmo->bitCount);
      lpBits += dwIncrement;
    }
    hlr_free (hInterlaceTable);
  }
  else {
    for (uRow=wHeight;uRow>0;) {
      rasterizeDIBtoGIF (lpBits,--uRow,wWidth,bmo->bitCount);
      lpBits += dwIncrement;
    }
  }
  if (bmo->bitCount == 1)
    uDataSize = 2;
  else
    uDataSize = bmo->bitCount;
  tempByte = uDataSize;
  fwrite (&tempByte,1,1,fp);
  bResult = writeRaster (wWidth,wHeight);
  hlr_free (raster);
  return bResult;
}

void gif_write (BitmapObject bmo,char *fn) {
  /**
     Writes a big to a file.
     @param[in] bmo - the BitmapObject
     @param[in] fn - the file name (- means stdout)
  */
  unsigned char cBuffer[10];
  unsigned char cSDStatus;
  unsigned char cByte;
  unsigned char cBackgrnd;
  long bResult;
  unsigned short wWidth;
  unsigned short wHeight;
  unsigned int uColors;
  unsigned int u;
  unsigned char tempByte;

  if (bmo == NULL)
    die ("gif_write: No bitmapObject, use gif_read () or gif_bitmapCreate ()");
  if (bmo->bitCount == 24)
    gif.bGrayscale24 = 1;
  else
    gif.bGrayscale24 = 0;
  if (strEqual (fn,"-"))
    fp = stdout;
  else
    fp = hlr_fopenWrite (fn);
  if (gif.uFormat == GIF87A)
    fwrite ("GIF87a",1,6,fp);
  else if (gif.uFormat == GIF89A)
    fwrite ("GIF89a",1,6,fp);
  if ((gif.uFormat != GIF87A) && (gif.uFormat != GIF89A))
    fwrite ("GIF87a",1,6,fp);
  wWidth = bmo->width;
  wHeight = bmo->height;
  cSDStatus = ((gif.bGlobalColorMap ? 128 : 0) +
               ((bmo->bitCount - 1) << 4) +
               (bmo->bitCount - 1));
  cBackgrnd = 0;
  cByte = 0;
  tempByte = wWidth;
  fwrite (&tempByte,1,1,fp);
  tempByte = wWidth >> 8;
  fwrite (&tempByte,1,1,fp);
  tempByte = wHeight;
  fwrite (&tempByte,1,1,fp);
  tempByte = wHeight >> 8;
  fwrite (&tempByte,1,1,fp);
  fwrite (&cSDStatus,1,1,fp); // status byte of screen descriptor
  fwrite (&cBackgrnd,1,1,fp); // color index of background
  fwrite (&cByte,1,1,fp); // 0 byte
  if (gif.bGlobalColorMap) {
    if (bmo->clrUsed)
      uColors = bmo->clrUsed;
    else
      uColors = 1 << bmo->bitCount;
    for (u=0;u<uColors;u++) {
      cBuffer[0] = bmo->colTable[u].red;
      cBuffer[1] = bmo->colTable[u].green;
      cBuffer[2] = bmo->colTable[u].blue;
      fwrite (cBuffer,1,3,fp);
    }
    if (bmo->clrUsed) {
      cBuffer[0] = 0;
      cBuffer[1] = 0;
      cBuffer[2] = 0;
      for (u=bmo->clrUsed;u<((unsigned int)1 << bmo->bitCount);u++)
        fwrite (cBuffer,1,3,fp);
    }
  }
  if (transparent != -1) {
    cBuffer[0] = '!';
    cBuffer[1] = 0xf9;
    cBuffer[2] = 4;
    cBuffer[3] = 1;
    cBuffer[4] = 0;
    cBuffer[5] = 0;
    cBuffer[6] = transparent;
    cBuffer[7] = 0;
    fwrite (cBuffer,1,8,fp);
  }
  bResult = writeImage (bmo);
  if (fwrite (";",1,1,fp) != 1)
    bResult = 0;
  fclose (fp);
  if (!bResult)
    die ("gif_write: something went wrong");
}

void gif_interlaceSet (int i) {
  /**
     Sets interlace mode
     @param[in] i - 0 no interlace; 1 interlaced
  */
  gif.bInterlaced = (i != 0);
}

void gif_globalColormapSet (int g) {
  /**
     Determines whether there is a global color map
     @param[in] g - 1 if yes, 0 if not
  */
  gif.bGlobalColorMap = (g != 0);
}

void gif_transparentColorSet (int c) {
  /**
     Sets the color index of the transparent color
     @param[in] c - colormap index of transparent color,
                    -1 mans no transparent color
  */
  transparent = c;
}
