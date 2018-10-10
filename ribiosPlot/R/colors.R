## colorpanel function was created in the gplots package (CRAN)
## using with the GPL-2 license
isOdd <- function(x) x%%2 == 1
colorpanel <- function (n, low, mid, high) {
    if (missing(mid) || missing(high)) {
        low <- col2rgb(low)
        if (missing(high)) 
            high <- col2rgb(mid)
        else high <- col2rgb(high)
        red <- seq(low[1, 1], high[1, 1], length = n)/255
        green <- seq(low[3, 1], high[3, 1], length = n)/255
        blue <- seq(low[2, 1], high[2, 1], length = n)/255
    }
    else {
        isodd <- isOdd(n)
        if (isodd) {
            n <- n + 1
        }
        low <- col2rgb(low)
        mid <- col2rgb(mid)
        high <- col2rgb(high)
        lower <- floor(n/2)
        upper <- n - lower
        red <- c(seq(low[1, 1], mid[1, 1], length = lower), seq(mid[1, 
            1], high[1, 1], length = upper))/255
        green <- c(seq(low[3, 1], mid[3, 1], length = lower), 
            seq(mid[3, 1], high[3, 1], length = upper))/255
        blue <- c(seq(low[2, 1], mid[2, 1], length = lower), 
            seq(mid[2, 1], high[2, 1], length = upper))/255
        if (isodd) {
            red <- red[-(lower + 1)]
            green <- green[-(lower + 1)]
            blue <- blue[-(lower + 1)]
        }
    }
    rgb(red, blue, green)
}

## colors for factors
## fbrewer returns a vector of colors matching a factor, and its base colors
fcbrewer <- function(factor, panel="Set1") {
  if(!is.factor(factor)) factor <- factor(factor)
  nlevel <- nlevels(factor)
  if(is.function(panel)) {
    cols <- do.call(panel, list(nlevel))
  } else if (is.character(panel) && length(panel)==1 && exists(panel) && is.function(panelf <- get(panel))) {
    cols <- do.call(panelf, list(nlevel))
  } else if (is.character(panel)) {
    if(length(panel)==1 && panel %in% rownames(RColorBrewer::brewer.pal.info)) {
      ncol <-  bound(nlevel, 3L, brewer.pal.info[panel, "maxcolors"])
      cols <- RColorBrewer::brewer.pal(ncol, panel)
      if (nlevel < 3) {
        cols <- cols[1:nlevel]
      } else if (nlevel > ncol) {
        cols <- grDevices::colorRampPalette(cols)(nlevel)
      }
    } else {
      haltifnot(nlevel==length(panel),
                msg=sprintf("panel length (%d) must match the number of factor levels (%d)",
                  length(panel), nlevel))
      cols <- panel
    }
  } else {
    stop("panel must be either (a) a valid brewer.pal color name (see brewer.pal.info), (b) a function or function name that generates colors, or (c) a vector of colors that match the number of levels of the input factor")
  }

  
  names(cols) <- levels(factor)
  fullcols <- cols[as.integer(factor)]
  return(fcol(fullcols, base=cols))
}

## .factorLevels returns the color used along the levels, whereas .factor return 1:1 mapping from levels to colors
brewer.pal.factorLevels <- function(factor, name="Greys") {
  .Deprecated("fcbrewer")
  nlevel <- nlevels(factor)
  ncol <- bound(nlevel, 3L, brewer.pal.info[name, "maxcolors"])
  cols <- brewer.pal(ncol, name)
  if(nlevel<3) {
    cols <- cols[1:nlevel]
  } else if (nlevel>ncol) {
    cols <- colorRampPalette(cols)(nlevel)
  }
  names(cols) <- levels(factor)
  return(cols)
}

brewer.pal.factor <- function(factor, name="Greys") {
  .Deprecated("fcbrewer")
  colbase <- brewer.pal.factorLevels(factor=factor, name=name)
  if(is.null(colbase)) return(NULL)
  return(colbase[factor])
}

## colors



##-------------------- three-color (or more) systems --------------------##

RIBIOS_BLUEREDS <- c("#2166AC", "#D1E5F0", "white", "#FDDBC7", "#B2182B")
royalbluered <- function(n) colorRampPalette(RIBIOS_BLUEREDS,
                                             space="Lab")(n)
royalredblue <- function(n) colorRampPalette(rev(RIBIOS_BLUEREDS),
                                             space="Lab")(n)

RIBIOS_BLUEGRAYREDS <- c("#2166AC", "gray", "#B2182B")

royalbluegrayred <- function(n) colorRampPalette(RIBIOS_BLUEGRAYREDS,
                                                 space="Lab")(n)
royalredgrayblue <- function(n) colorRampPalette(rev(RIBIOS_BLUEGRAYREDS),
                                                 space="Lab")(n)

turyeb <- function(n) colorpanel(n, "turquoise1", "yellow", "black")

## following functions were created in the gplots package (CRAN):
## redgreen, bluered, greenred, redblue
## using with the GPL-2 license
redgreen <- function(n) colorpanel(n, "red", "black", "green")
greenred <- function(n) colorpanel(n, "green", "black", "red")
bluered <- function(n) colorpanel(n, "blue", "white", "red")
redblue <- function(n) colorpanel(n, "red", "white", "blue")
blueblackred <- function(n) colorpanel(n, "blue", "black", "red")
cyanblackyellow <- function(n) colorpanel(n, "#9BD0EC", "black", "#FCF6C3")
yellowblackcyan <- function(n) colorpanel(n, "#FCF6C3", "black", "#9BD0EC")
redblackblue <- function(n) colorpanel(n, "red", "black", "blue")
blackredyellow <- function(n) colorRampPalette(c("black", "darkred", "red2", "goldenrod1", "yellow"),
                                               space="Lab")(n)
blackgoldred <- function(n) colorpanel(n, "black", "goldenrod1", "red2")
magentayellow <- function(n) colorRampPalette(c("magenta2", "black", "yellow"), space="Lab")(n)
yellowmagenta <- function(n) colorRampPalette(c("yellow", "black", "magenta2"), space="Lab")(n)

##-------------------- l3 color systems --------------------##
whiteblueblackheat <- function(n) colorRampPalette(c("white", "blue", "blue3", "black", "red3", "red", "yellow"))(n)
heat <- function(n)  colorRampPalette(c("transparent", blues9, "black", "red3", "red", "yellow"))(n)

##-------------------- two-color systems --------------------##
blackyellow <- function(n) colorpanel(n, "black", "yellow")
yellowblack <- function(n) colorpanel(n, "yellow", "black")

RIBIOS_WHITEBLUES <- c("#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3")
whiteblue <- function(n) colorRampPalette(RIBIOS_WHITEBLUES,
                                          space="Lab")(n)

RIBIOS_WHITEREDS <- c("#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D")
whitered <- function(n) colorRampPalette(RIBIOS_WHITEREDS,
                                         space="Lab")(n)

blackred <- function(n) colorpanel(n, "black", "red")
blackgreen <- function(n) colorpanel(n, "black", "green")

whiteblack <- function(n) rev(blackwhite(n))
blackwhite <- function(n) gray(seq(0, 1, 1/(n-1)))

##-------------------- Display functions --------------------##
twocolor.panels <- function() {
  return(c("blackyellow", "yellowblack",
           "whiteblue", "whitered",
           "blackred", "blackgreen", "whiteblack", "blackwhite",
           "magentayellow", "yellowmagenta"))
}
threecolor.panels <- function() {
  return(c("royalbluered", "royalredblue",
           "royalbluegrayred","royalredgrayblue",
           "turyeb",
           "redgreen", "greenred",
           "bluered", "redblue",
           "redblackblue", "blueblackred",
           "cyanblackyellow", "yellowblackcyan",
           "heat.colors",
           "blackredyellow", "blackgoldred", 
           "whiteblueblackheat", "heat"))
}


display.colorpanels <- function(panel.names, nc) {
  nc <- as.integer(pmax(pmin(nc, 100), 3))
  np=length(panel.names)
  oldpar <- par(mgp=c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim=c(0, nc), ylim=c(0,length(panel.names)), type="n",
       bty="n",axes=FALSE, bty="n", xlab="", ylab="")
  for(i in seq(along=panel.names)) {
    curcols <- eval(call(panel.names[i], nc))
    rect(xleft=0:(nc-1), ybottom=i-1, xright=1:nc, ytop=i-0.2,col=curcols, border="lightgray")
    text(rep(-0.1, np), (1:np)-0.6, labels=panel.names, xpd=TRUE, adj=1)
  }
}

display.twocolor.panels <- function (nc=20) {
  display.colorpanels(twocolor.panels(), nc)
}

display.threecolor.panels <- function (nc=20) {
  display.colorpanels(threecolor.panels(), nc)
}
