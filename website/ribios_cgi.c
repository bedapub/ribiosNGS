/*
Jitao David Zhang <jitao_david.zhang@roche.com>
For code updates, see NEWS in the directory
*/

#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "format.h"
#include "log.h"
#include "html.h"
#include "hlrclock.h"
#include "rofutil.h"
#include "htmlfile.h"
#include "linestream.h"

#include "prpidefs.h"
#include "appconf.h"

#define TESTCGI_DEBUG 
#define TESTCGI_VERB 5
#ifdef TESTCGI_DEBUG
#define DD(a,b) {if (TESTCGI_VERB>=a) {printf b;fflush(stdout);}}
#else
#define DD(a,b)
#endif

#define LOG_DEBUG 
#define LOG_VERB 9
#ifdef LOG_DEBUG
#define LL(a,b) {if (LOG_VERB>=a) {fprintf b;fflush(flog);}}
#else
#define LL(a,b)
#endif

#define CHIP_TYPE_HEAD	"<div title=\"Chip type for feature annotation\" class=\"slabel\">" \
	   	        "<label for=\"chiptype\">Annotation of input tags</label></div>" \
	 		"<div class=\"sright\">" \
 		        "<select name=\"chiptype\" id=\"chiptype\" class=\"required chiptypeBox\" onChange=\"chiptypeOnchange();\">" \
		        "<option value=\"\" id=\"noanno\" selected=\"selected\" disabled=\"disabled\">Select annotation here</option>" \
		        "<option value=\"GeneID\">Entrez Gene ID</option>" \
		        "<option value=\"GeneSymbol\">Official Gene Symbol</option>" \
		        "<option value=\"dashline\" disabled=\"disabled\">---------------</option>"
#define CHIP_TYPE_TAIL "</select></div>"
#define NL "\n"

//#define straEqual(item, val) strEqual(string(item), val)
//#define straDup(stringa) hlr_strdup(string(stringa))

/*something to be ignored */
#pragma set woff 1174
#pragma reset woff 1174

/* global variables */
bool isSavefile=FALSE;

char *gPid=NULL; // PID as identifier of session, produced when the frontpage is called
char *gUptagsFile=NULL;
char *gDowntagsFile=NULL;
char *gAnnUptagsFile=NULL;
char *gAnnDowntagsFile=NULL;

char *gOutputFile=NULL;
char *gAction=NULL;
char *gChiptype=NULL;
char *gDataset=NULL;
char *gSavefileName=NULL;

/* Structures */
struct AnnoStat {
  unsigned int featNo;
  unsigned int uniqGeneNo;
  unsigned int naFeatNo;
};

static void doAnalysis (char *fn){
  // test input exist file
// Stringa anyfile = stringCreate(30);
// stringPrintf(anyfile, "%s/%s", APP_TEMP_DIR, gUptagsFile);
// if(hlr_queryPlainFile(string(anyfile)) != NULL) {
//   printError("No data has been uploaded",
//		       "Use either the text box or file upload button to upload a matrix for biosHeatmap.");
// } else {
//   Stringa comm = stringCreate(100);
//   Stringa convert = stringCreate(100);
//   Stringa output_tiff = stringCreate(30);
//   Stringa output_png = stringCreate(30);
//   Stringa output_log = stringCreate(30);
//   Stringa rmoutput = stringCreate(30);
//   Stringa atfile = stringCreate(30);
//
//   char* gAt;
//   
//   FILE* fp;
//   int commres;
// 
//   // run the R script
//   stringPrintf(output_png, "%s.png", gOutputFile);
//   stringPrintf(output_tiff, "%s.tiff", gOutputFile);
//   stringPrintf(output_log, "%s.log.txt", gOutputFile);
// 
//   stringPrintf(comm, "%s %s/biosHeatmap.Rscript -infile %s/%s -outfile %s/%s "
//		 "-scale %s -colors %s -naColor %s -symbreaks %s "
//		 "%s %s -dendrogram %s -dist %s -hclust %s %s %s "
//		 "-main %s -xlab %s -ylab %s -cexRow %s -cexCol %s -colorKeyTitle %s "
//		 "-width %s -height %s -margins %s %s -zlimLo %s -zlimHi %s ",
//		 APP_RSCRIPT, APP_BIN_DIR, APP_TEMP_DIR, gUptagsFile, APP_TEMP_DIR, gOutputFile,
//		 cale,pcolpal, pnacol, psymbr,
//		 prowv, pcolv, pdendro, pdist, phclust, prevc, psymm,
//		 pmain, pxlab, pylab, pcexrow, pcexcol, pcolmain,
//		 pwidth, pheight, pcolmar, prowmar, pzlimlo, pzlimhi);
//   char *rep= hlr_strdup(string(comm));
//   stringAppendf(comm, "1> /dev/null 2> %s/%s",APP_TEMP_DIR, string(output_log));
// 
//
//   /*
//     FILE *fp;
//     fp = hlr_fopenWrite("/DATA/tmp/bios_hm_debug");
//     fprintf(fp, "%s\n", string(comm));
//     fclose(fp);
//   */
//   commres = system(string(comm));
//   stringPrintf(atfile,"%s/bios_hm_%d.at",
//		 APP_TEMP_DIR, getpid());
//   gAt = hlr_strdup(string(atfile));
// 
//   if(commres != 0) {
//     stringClear(anyfile);
//     stringPrintf(anyfile, "%s/%s",
//		   APP_WEB_TMP_DIR, string(output_log));
//     printRscriptError(commres, string(anyfile));
//   
//     fp = hlr_fopenWrite(gAt);
//     fprintf(fp, "rm -f %s/%s %s/%s %s",
//	      APP_TEMP_DIR, gUptagsFile,
//	      APP_TEMP_DIR, string(output_log),
//	      gAt);
//     fclose(fp);
//     stringPrintf(rmoutput,"at -f %s now +24 hours &> /dev/null",gAt);
//   } else {
//     stringPrintf(convert, "convert %s/%s %s/%s",
//		   APP_TEMP_DIR, gOutputFile,
//		   APP_TEMP_DIR, string(output_png));
//     system(string(convert));
//if (ENABLE_TIFF>=1)
//     stringClear(convert);
//     stringPrintf(convert, "convert -density 300 %s/%s -compress zip %s/%s",
//		   APP_TEMP_DIR, gOutputFile,
//		   APP_TEMP_DIR, string(output_tiff));
//     system(string(convert));
//endif      
//
//     puts("<html><BODY>");
//     printResults(APP_WEB_TMP_DIR, string(output_png),gOutputFile, string(output_tiff), rep);
//   
//     fp = hlr_fopenWrite(gAt);
//     fprintf(fp, " rm -f %s/%s %s/%s %s/%s %s/%s %s/%s %s",
//	      APP_TEMP_DIR, gUptagsFile,
//	      APP_TEMP_DIR, string(output_log),
//	      APP_TEMP_DIR, string(output_png),
//	      APP_TEMP_DIR, gOutputFile,
//	      APP_TEMP_DIR, string(output_tiff),
//	      gAt);
//     fclose(fp);
//     stringPrintf(rmoutput,"at -f %s now +1 hours &> /dev/null",gAt);
//   }
// 
//   // start the "at" job
//   system(string(rmoutput));
//   stringDestroy(comm);
//   stringDestroy(convert);
//   stringDestroy(output_png);
//   stringDestroy(output_tiff);
//   stringDestroy(output_log);
//   stringDestroy(rmoutput);
//   stringDestroy(atfile);
// }
// stringDestroy(anyfile);

}


char* filepath(char* dir, char* file) {
  Stringa fullname=stringCreate(25);
  char* fn;
  if(file == NULL) {
    return("");
  } else {
    stringPrintf(fullname, "%s/%s", dir, file);
    fn=hlr_strdup(string(fullname));
    stringDestroy(fullname);
    return(fn);
  }
}

static void doSavefile() { // supports only files in the APP_RESOURCE directory
  LineStream ls= ls_createFromFile(filepath(APP_RESOURCE, 
					    gSavefileName));
  char* line;
  cgiContentDispositionSet(gSavefileName);
  cgiHeader("application/x-download");
  while(line=ls_nextLine(ls)) {
    puts(line);
  }
  ls_destroy(ls);
}
char* pidFile(char* path, char* format) {
  Stringa fullname = stringCreate(25);
  if(path != NULL)
    stringPrintf(fullname,"%s/",path);
  
  stringAppendf(fullname, format, gPid);
  char* fn=hlr_strdup(string(fullname));
  stringDestroy(fullname);
  return(fn);
}

char* getUptagsFileName()  {return(pidFile(APP_TEMP_DIR, "cdr_uptags_%s.in"));}
char* getAnnUptagsFileName()  {return(pidFile(APP_TEMP_DIR, "cdr_annotated_uptags_%s.in"));}
char* getDowntagsFileName()  {return(pidFile(APP_TEMP_DIR,"cdr_downtags_%s.in"));}
char* getAnnDowntagsFileName()  {return(pidFile(APP_TEMP_DIR,"cdr_annotated_downtags_%s.in"));}


static int doAnnotation() {
  const char* libpath="LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"
    "/opt/oracle/client/10/run_1/lib;export LD_LIBRARY_PATH;";
  
  Texta rmFiles=textCreate(20);
  Stringa temp=stringCreate(100);
  char* atFile=pidFile(APP_TEMP_DIR, "cdr_%s.at");
  int annRes=0;
  FILE *fp;

  gAnnUptagsFile=getAnnUptagsFileName();
  gAnnDowntagsFile=getAnnDowntagsFileName();
  
  stringPrintf(temp, 
	       "%s %s %s -infile %s %s -chiptype %s -outfile %s %s",
	       libpath,
	       APP_RSCRIPT,
	       filepath(APP_BIN_DIR, APP_ANNOTATION_SCRIPT),
	       gUptagsFile, gDowntagsFile,
	       gChiptype,
	       gAnnUptagsFile, gAnnDowntagsFile);
  textAdd(rmFiles, gAnnUptagsFile);
  textAdd(rmFiles, gAnnDowntagsFile);
  textAdd(rmFiles, gUptagsFile);
  textAdd(rmFiles, gDowntagsFile);
 
  annRes= system(string(temp));
  if(annRes!=0) {
    return(1);
  } 

  stringClear(temp);
  textAdd(rmFiles, atFile);
  textJoin(temp, " ", rmFiles);
  fp = hlr_fopenWrite(atFile);
  fprintf(fp, "rm -f ");
  fprintf(fp, string(temp));
  fclose(fp);

  stringClear(temp);
  stringPrintf(temp, "at -f %s now +48 hours &> /dev/null", atFile);
  system(string(temp));

  arrayDestroy(rmFiles);
  stringDestroy(temp);
  return(0);
}

static void printHeader(char *title, char *up,
			short useADgallery){
  puts("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" NL
       "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">" NL
       "<head>" NL
       "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>");
  printf("<title>%s</title>",title);
  
  /* Roche/RONET specific JS/CSS */
  puts("<link rel=\"stylesheet\" type=\"text/css\" href=\"http://bioinfo.bas.roche.com:8080/apps/gti/bioinfo2007.css\">" NL
       "<link rel=\"stylesheet\" type=\"text/css\" href='" RONET_CSS "'/>");

  /* jQuery specific JS */
  puts("<script type=\"text/javascript\" src=\"" JQUERY_JS "\"></script>");

  /* jQuery-UI specific JS/CSS */
  puts("<link rel=\"stylesheet\" type=\"text/css\" href=\"" JQUERY_UI_CSS "\" rel=\"Stylesheet\"/>"  NL
       "<script type=\"text/javascript\" src=\"" JQUERY_UI_DEV "/jquery.ui.core.js\"></script>" NL
       "<script src=\"" JQUERY_UI_DEV "/jquery.ui.widget.js\"></script>" NL
       "<script src=\"" JQUERY_UI_DEV "/jquery.ui.tabs.js\"></script>" NL
       "<script src=\"" JQUERY_UI_DEV "/jquery.ui.accordion.js\"></script>");
       
  /* APP_URL */
  puts("<link rel=\"stylesheet\" type=\"text/css\" href=\"" APP_CSS "\"/>" NL
       "<script src=\"" APP_JS "\"></script>");

  if(useADgallery>0){
    puts("<script type=\"text/javascript\" src=\"" JQUERY_ADGALLERY "/jquery.ad-gallery.js\"></script>"
	 "<link rel=stylesheet type=\"text/css\" href=\"" JQUERY_ADGALLERY "/jquery.ad-gallery.css\"/>"
	 "<script type=\"text/javascript\" src=\"" APP_URL "/bios_hm_adgallery.js\"></script>"
	 "<link rel=stylesheet type=\"text/css\" href=\"" APP_URL "/bios_hm_adgallery.css\"/>");
  }
       
  /* following script must be written here because of macro expansion*/
  puts("</head>"
       "<BODY>" /* ending will be in main program*/
       "<div>"
       "<div class='bih_headerleft'>BEDA<br><span class='bih_headersubtitle'>" APP_RELEASE "</span></div>"
       "<img class='bih_gradient' src='" APP_WEB_BANNER "'/ style=\"float:left;position:absolute;left:18%;\">");
  puts("<div class='ronet_headercenter' style=\"left:16%;position:relative;\">");
  puts(title);
  puts("</div>");
  puts("<div class='bih_headerright'></div>"
       "<div><img class='bih_rochelogo' src=http://bioinfo.bas.roche.com:8080/images/tiny_roche_logo.gif></div>");
  /*
  puts("<div class='bih_menubarleftlogo'></div>");
  puts("<div class='bih_menubarleftuserid'></div>");
  puts("<div class='bih_menubarmiddle'>&nbsp;&nbsp;&nbsp;</div>");
  */

  puts("<div class='bih_menubarright' style=\"background:#EFEFEF;position:aboslute;left:0px;color:black;height:15px;width:100%;padding:4px;vertical-align:middle;\">");
  printf("<div style=\"text-align:right\"><a href='%s' class=bih_menubar style=\"color:black;\">Up</a> ", up);
  puts("| <a href='" APP_URL "NEWS' class=bih_menubar style=\"color:black;\" target=_blank>News</a> "
       "| <a href='" APP_CGI "help' class=bih_menubar style=\"color:black;\" target=bios_hm_help>Help & Tutorial</a> "
       "| <a href='mailto:jitao_david.zhang@roche.com&subject=biosHeatmap Support' style=\"color:black;\""
       "class=bih_menubar>Contact</a>&nbsp;&nbsp;&nbsp;</div></div>");
  puts("</div>");
  /* start mainscreen */
  puts("<div class='mainscreen' id='mainscreen'>");
}

static void printMan(){
  LineStream ls;
  char *line;
  printHeader("biosHeatmap - Help & Tutorial", APP_CGI, 1);
  ls=ls_createFromFile(APP_HELP);
  while(line=ls_nextLine(ls)) {
    puts (line);
  }
  ls_destroy(ls);
}

static void printError(char* msg, char* details) {
  printHeader("biosHeatmap - Error", APP_CGI,0);
  printf("<H4>Error: %s</H4><P>%s</P>", msg, details);
  puts("<a href=\"" APP_CGI "\" class=\"fg-button fg-button-icon-left ui-state-default ui-corner-all\">"
       "<span style=\"float:left;\" class=\"ui-icon ui-icon-arrowthick-1-w\"></span>Back</a>"
       "</div>");
}

/*
static void printRscriptError(int exit_code, char* logfile) {
  printHeader("biosHeatmap - Error", APP_CGI,0);
  puts("<h4>Error: Unexpected data format or other errors</h4>");
  printf("<p>Sorry, there was a failure in plotting the heatmap (exit code: %d).</p>", exit_code);
  puts("<ul>");
  printf("<li><a href=\"%s\" target=\"_blank\">View the log file</a></li>",
	 logfile);
  puts("<li><a href=\"" APP_CGI "help\" target=\"_blank\">See solutions for common problems</a></li>");
  printf("<li><a href='mailto:jitao_david.zhang@roche.com&subject=[biosHeatmap] Support on errors&body=Log%%20file %s'>"
	 "Contact the developer via email</a></li>",
	 logfile);
  puts("</ul>");
  puts("<a href=\"" APP_CGI "\" class=\"fg-button fg-button-icon-left ui-state-default ui-corner-all\">"
       "<span style=\"float:left;\" class=\"ui-icon ui-icon-arrowthick-1-w\"></span>Back</a>"
       "</div>");
}

static void printResults(char* dir, char* png_file, char* pdf_file, char* tiff_file, char* command) {
  printHeader("biosHeatmap", APP_CGI,0);
  printf("<p><img src=\"%s/%s\" alt=\"PNG preview\" height=\"400px\"></p>"
	 "<p><a href=\"%s/%s\" target=\"_blank\">Download heatmap as PDF</a></p>",
	 dir, png_file,
	 dir, pdf_file);
#if (ENABLE_TIFF>=1)
  printf("<p><a href=\"%s/%s\" target=\"_blank\">Download heatmap as TIFF</a></p>",
    	 dir, tiff_file);
#endif 
  printf("<p>Command to reproduce: <input value=\"%s\" size=120 readonly></p>",command);
  puts("</div>");
}

static char* protectArg(Stringa arg) {
  Stringa tmp;
  tmp = stringCreate(25);
  stringPrintf(tmp, "'%s'", string(arg));
  return(string(tmp));
}
*/

static char* optsFromFile(char* filename) {
  LineStream ls;
  char *line;
  Stringa multiVal = stringCreate(100);
  ls=ls_createFromFile(filename);
  while(line=ls_nextLine(ls)) {
    stringAppendf(multiVal,
		  "<option value=\"%s\">%s</option>",
		 line, line);
  }
  ls_destroy(ls);
  return(string(multiVal));
}

void printUploadIcons(char* exampleFile, char* removeId, char* removeFunc) {
  printf( "<div class=\"uploadIcons\">"
          "<a href=\"" APP_CGI "savefile=%s\" target=\"_blank\">"
          "<span title=\"Download example file\" class=\"ui-icon ui-icon-document iconItem\"></span>"
          "</a>"
          "<span title=\"Remove selected file\" id=\"%s\" class=\"ui-icon ui-icon-close iconItem removeIcon\" onClick=\"%s();\"></span>"
          "</div>" NL,
          cgiEncodeW(exampleFile),
          removeId,
          removeFunc);
}


static void parsePairs (Stringa item, Stringa value,
                        Stringa fileName,Stringa contentType){
  char *iPtr,*vPtr;

  iPtr = string(item);
  vPtr = string(value);

  if (stringLen (value) == 0)
    return;
  
  if (strEqual (iPtr,"uptags") || strEqual (iPtr,"uptagsFile")) {
    FILE *fp;
    gUptagsFile = getUptagsFileName();
    if(arrayMax(value)==0) return;
    fp=hlr_fopenWrite(gUptagsFile);
    fprintf(fp, "%s", string(value));
    fclose(fp);
  } else  if (strEqual(iPtr, "downtags") || strEqual(iPtr, "downtagsFile")) {
    FILE *fp;
    gDowntagsFile = getDowntagsFileName();
    if(arrayMax(value)==0) return;
    fp=hlr_fopenWrite(gDowntagsFile);
    fprintf(fp, "%s", string(value));
    fclose(fp);
  }
  /* other parameters */
  else if (strEqual(iPtr, "action")) { gAction=hlr_strdup(vPtr); }
  else if (strEqual(iPtr, "chiptype")) { gChiptype=hlr_strdup(vPtr);}
  else if (strEqual(iPtr, "dataset")) { 
    if(gDataset == NULL) {
      gDataset=hlr_strdup(vPtr);
    } else {
      Stringa tmp=stringCreate(50);
      stringPrintf(tmp, "%s+%s", gDataset, vPtr);
      gDataset=hlr_strdup(string(tmp));
      stringDestroy(tmp);
    }
  } 
  else if (strEqual(iPtr, "savefile")) {
    isSavefile=TRUE;
    gSavefileName=hlr_strdup(vPtr);
  } else if (strEqual(iPtr, "gPid")) {gPid=hlr_strdup(vPtr);
  } else {
    die ("Unexpected variable '%s'",string(item)); 
  }
}

static void postProcessPairs() {
  gUptagsFile = pidFile(APP_TEMP_DIR, "cdr_uptags_%s.in");
  gDowntagsFile = pidFile(APP_TEMP_DIR, "cdr_downtags_%s.in");
}

static void printDataset(char* setname, 
			 char* setlabel, char* info, 
			 int selected) {
  char *b= selected ? "checked" : "";
  printf("<span class=\"dataset\" title=\"%s\">"
	 "<input class=\"dataset\" type=\"checkbox\" onClick=\"datasetOnchange();\"" 
	 "name=\"dataset\" value=\"%s\" %s>%s</input>"
	 "</span>",
	 info, setname, b, setlabel);
}

static void printFrontpage () {
  /* header */
  printHeader("",APP_PARENT,0); 
  puts("<div id=\"cdrHolder\">");

  puts("Website in construction. In any questions please contact jitao_david.zhang@roche.com");

  puts("<h3>Web-portal of RIBIOS: Interfacing R and the BIOS System</h3>"
       "<ul>"
       "<li>News</li>"
       "<li>Packages overview</li>"
       "  <ul>"
       "  <li>Dependencies</li>"
       "  <li>Daily build</li>"
       "  </ul>"
       "<li>Examples and tutorials</li>"
       "<li>RIBIOS Resources</li>"
       "  <ul>"
       "  <li>command-line tools</li>"
       "  <li>web tools</li>"
       "  </ul>"
       "</ul>"
       "<div id=\"cdrHolder\">");
  
  puts("</div>" // mainscreen
       "</body>"
       "</html>");
}

struct AnnoStat printAnnTable (char* filename, char* divClass, char* label) {
  LineStream ls=ls_createFromFile(filename);
  char* line;
  Texta gids=textCreate(100);
  int cnt=0;int nng=0;
  struct AnnoStat res;

  Texta lc=textCreate(6);
  printf("<div class=\"%s\">", divClass);
  printf("<span class=\"uploadLabel\"><label>%s</label></span>", label);
  puts("<table class=\"datatable\" cellspacing=\"0\" border=\"1px 0 0 1px\">");
  printf("<caption class=\"uploadLabel\"></caption>");
  puts("<thead>"
       "<tr><th>ProbeID</th><th>GeneID</th><th>GeneSymbol</th><th>GeneName</th></tr>"
       "</thead><tbody>");
  ls_nextLine(ls); // skip header
  while(line=ls_nextLine(ls)) {
    lc = textStrtokP(line, "\t\"");
    if(arrayMax(lc)>1) {
      printf("<tr><th>%s</th><td>%s</td><td>%s</td><td>%s</td></tr>",
	     textItem(lc, 0), textItem(lc, 1), textItem(lc, 2), textItem(lc, 3));
      textAdd(gids, textItem(lc, 1));
      if(strEqual(textItem(lc, 1), "NA")) nng++;
      cnt++;
    }
  }
  puts("</tbody></table>");
  puts("</div>");
  ls_destroy(ls);
  arrayDestroy(lc);
  
  textUniqKeepOrder(gids);

  res.featNo=cnt;
  res.uniqGeneNo=arrayMax(gids);
  res.naFeatNo=nng;
  return(res);
}

void printAnnStat (struct AnnoStat* stat, char* label, char* divClass) {
  float noper= stat->naFeatNo*100.0/stat->featNo;
 printf("<div class=\"%s\">"
	"<span class=\"uploadLabel datasetLabel\"><label>%s statistics</label></span>"
	"<div class=\"annostatTbl\">"
	"<table>"
	"<tr><td class=\"annostatLabel\">Input features</td><td align=\"left\">%d</td></tr>"
	"<tr><td class=\"annostatLabel\">... annotated</td><td align=\"left\">%d (%.1f%%)</td></tr>"
	"<tr><td class=\"annostatLabel\">... not annotated</td><td align=\"left\">%d (%.1f%%)</td></tr>"
	"<tr><td class=\"annostatLabel\">Unique GeneIDs</td><td align=\"left\">%d</td></tr>"
	"</table>"
	"</div></div>",
	divClass, label,
	stat->featNo, 
	stat->featNo - stat->naFeatNo, 100-noper,
	stat->naFeatNo,noper,
	stat->uniqGeneNo);

}

static void printDatasets() {
  Texta datasets=textCreate(10);
  int i;
  
  datasets=textStrtokP(gDataset, "+");
  puts("<div class=\"twoleft annPageDatasets\">");
  puts("<span class=\"uploadLabel datasetLabel\"><label for=\"annPageDatasetList\">Datasets used for repositioning</label></span>");
  puts("<ul id=\"annPageDatasetList\">");
  for(i=0; i<arrayMax(datasets);i++) {
    printf("<li>%s</li>", textItem(datasets, i));
  }
  puts("</ul></div>");
  textDestroy(datasets);
}

static void printAnnotationPage() {
  
  char* annUp=getAnnUptagsFileName();
  char* annDown=getAnnDowntagsFileName();
  struct AnnoStat upstat, downstat;

  /* header */
  printHeader("Computational Drug Reposition",APP_PARENT,0); 
  puts("<div id=\"cdrHolder\">");
  puts("<form id=\"form\" action=" 
       APP_CGI 
       "execute method=\"post\" enctype=\"multipart/form-data\">");
  puts("<input type=\"hidden\" name=\"action\" value=\"execute\"/>");  
  printf("<input type=\"hidden\" name=\"gPid\" value=\"%s\"/>", gPid);
  printf("<input type=\"hidden\" name=\"chiptype\" value=\"%s\"/>", gChiptype);
  printf("<input type=\"hidden\" name=\"dataset\" value=\"%s\"/>", gDataset);

  /* print annotation tables */  
  puts("<div class=\"scrollTableDiv\">");
  upstat=printAnnTable(annUp, "tbldiv twoleft gradientRed", "Up tags");
  downstat=printAnnTable(annDown, "tbldiv tworight gradientGreen", "Down tags");
  puts("</div>");

  puts("<div class=\"annostat\">");
  printAnnStat(&upstat, "Up tags", "twoleft annostat");
  printAnnStat(&downstat, "Down tags", "tworight annostat");
  puts("</div>");

  /* print dataset to be used */
  printDatasets();

  puts("<input type=\"submit\" class=\"buttons\" onclick=\"doSubmit();\" "
       "id=\"excuteButton\" value=\"Run reposition now\"/>");
  puts("<div class=\"wait\" id=\"waitIcon\"><img src='" APP_URL "images/ajax-loader.gif'/><label class=\"waitmsg\">&nbsp;Algorithm starting...</label></div>");
  
  /* print annotated features */
  puts("</div>" // APP holder
       "</form>"
       "</div>" // mainscreen
       "</body>"
       "</html>");
}

static void printResultPage() {
  /* header */
  printHeader("Computational Drug Reposition",APP_PARENT,0); 

  puts("Results:");
  puts("</div>"
       "</body>"
       "</html>");
}


int main(int argc, char *argv[]) {
  int first=1;
  time_t seconds;
  Stringa item = stringCreate (100);
  Stringa value = stringCreate (100); 
  Stringa command = stringCreate (100); 
  Stringa fileName =stringCreate(20);
  Stringa contentType=stringCreate(20);
  Stringa tmp = stringCreate(100); 

  cgiInit();
  if (strlen(getenv("QUERY_STRING")) > 0)  cgiGet2Post();
  
  if (argc > 1) {
    if (strEqual (argv[1],"doupload") || strEqual(argv[1], "execute")) {
      cgiMpInit ();
      while (cgiMpNext(item,value,fileName,contentType))
        parsePairs (item,value,fileName,contentType);
    } else if (strEqual (argv[1],"help")) {
      printMan();
      return 0;
    } else {
      die("Unrecognized request");
    }
  } else { 
    first=1;
    cgiGetInit();
    while (cgiGetNextPair (&first,item,value))
      parsePairs (item,value,fileName,contentType);
  }
  
  if(isSavefile) {
    doSavefile();
    return(0);
  }
  cgiHeader ("text/html");   // alternative could be cgiHeader ("application/vnd.ms-excel");
  
  postProcessPairs();

  if(gAction == NULL) {
    seconds=time(NULL);
    stringPrintf(tmp,"%1d%d", seconds/60, getpid());
    gPid=hlr_strdup(string(tmp));
    printFrontpage();
  } else if(strEqual(gAction, "doupload")){
    if(doAnnotation(gUptagsFile, gDowntagsFile)==0)
      printAnnotationPage();
    else
      printError("Annotation failed",
		 "msg pending");
  } else if (strEqual(gAction, "execute")) {
    doAnalysis(gUptagsFile);
    printResultPage();
  } 

  stringDestroy(item);
  stringDestroy(value);
  stringDestroy(fileName);
  stringDestroy(contentType);
  stringDestroy(tmp);
  
  return hlr_system(string(command),0);
}


