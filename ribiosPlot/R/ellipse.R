#' Convert radian to degree values
#'
#' @param x Radian value
#' 
#' @return Degree value
#' @examples
#' radian2degree(2*pi)
#' radian2degree(-.5*pi)
radian2degree <- function(x) x/pi*180

#' Convert degree to radian values
#'
#' @param x Degree value
#' 
#' @return Radian value
#' @examples
#' 
#' degree2radian(90)
#' degree2radian(-225)
degree2radian <- function(x) x/180*pi

#' Add an ellipse in an existing plot
#'
#' @param x0 x-coordinate of the ellipse center
#' @param y0 y-coordinate of the ellipse center
#' @param a Length of semi-major axis
#' @param b Length of semi-minor axis
#' @param alpha Rotation of the ellipse with regard to the X-axis in radian
#' @param length How many points are generated to simulate the ellipse
#' @param col Ellipse border color
#' @param fill Ellipse fill color
#' @param border Equivalent to \code{col}
#'
#' @return Invisible coordinates of points on the ellipse
#' @examples
#'
#' if(interactive()) {
#'   plot.new()
#'   plot.window(xlim=c(-1, 1), ylim=c(-1,1))
#'   ellipseCols <- heat.colors(11)
#'   ellipse(0, 0, a=1, b=0.5, alpha=0)
#'   for(i in 1:11) {
#'     ellipse(0, 0, a=1, b=0.5, alpha=degree2radian(i*15), col=ellipseCols[i])
#'   }
#'   ellipse(0, 0, a=1, b=1, col="black", lwd=2)
#'   ellipse(0, 0, a=0.5, b=0.5, fill="steelblue")
#' }
ellipse <- function(x0=0, y0=0, a=1, b=2,
                    alpha=0, length=1000,
                    col=NULL, fill=NA, border, ...) {
    if(!missing(border))
        col <- border
    theta <- seq(0, 2*pi, length=length)
    x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
    y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
    polygon(x,y, border=col, col=fill, ...)
    return(invisible(cbind(x,y)))
}

confEllipseParams <- function(x,y=NULL, conf=0.95) {
    xy <- xy.coords(x,y)
    x <- xy$x
    y <- xy$y
    scaleX <- scale(x)
    scaleY <- scale(y)
    scaleMean <- c(attr(scaleX, "scaled:center"),
                   attr(scaleY, "scaled:center"))
    chisqCrit <- qchisq(conf, df=2L)
    covMat <- cov(cbind(x,y))
    covEigen <- eigen(covMat)
    covLargestEigenVector <- covEigen$vectors[,1]
    covOrient <- atan(covLargestEigenVector[2]/covLargestEigenVector[1])
    if(covOrient<0)  covOrient <- covOrient+2*pi
    covHalfMajorAxis <- sqrt(chisqCrit*covEigen$values[1])
    covHalfMinorAxis <- sqrt(chisqCrit*covEigen$values[2])
    res <- list(mean=scaleMean,
                a=covHalfMajorAxis,
                b=covHalfMinorAxis,
                alpha=covOrient)
    return(res)
}

#' Plot confidence ellipse based on two-dimenstional data
#'
#' @param x either a matrix of two columns, or a numeric vector
#' @param y0 either a numeric vector of the same length as \code{x}, or NULL
#' @param conf Confidence interval of the ellipse
#' @param ... Parameters passed to \code{ellipse}
#'
#' @return Invisible coordinates of points on the ellipse
#' @examples
#'
#' if(interactive()) {
#'   testX <- rnorm(100, mean=1, sd=2)
#'   testY <- rnorm(100, mean=2, sd=3)
#'   plot(testX, testY, pch=16, xlim=c(-5,7), ylim=c(-7,11))
#'   confEllipse(testX, testY, col="red", lwd=1)
#'   confEllipse(testX, testY, conf=0.99, col="red", lwd=2)
#'   confEllipse(testX, testY, conf=0.9, col="red", lwd=0.5)
#' }
#'
#' if(interactive() & require("MASS")) {
#'   testMVR <- mvrnorm(n=100, mu=c(2,3), Sigma=matrix(c(1, 0.65, 0.65, 1), nrow=2))
#'   plot(testMVR, pch=16, xlim=c(-2,6), ylim=c(0,6))
#'   confEllipse(testMVR, col="orange")
#'   confEllipse(testMVR, conf=0.99, col="red")
#'   confEllipse(testMVR, conf=0.9, col="lightblue")
#' }
#' 
confEllipse <- function(x, y=NULL, conf=0.95, ...)  {
    params <- confEllipseParams(x=x, y=y, conf=conf)
    ellipse(x0=params$mean[1], y0=params$mean[2],
            a=params$a, b=params$b,
            alpha=params$alpha, ...)
}
