## PIF/RIF algorithm by Hudson and Reverter
pifVec <- function(x, y) {
  xm <- mean(x, na.rm=TRUE)
  ym <- mean(y, na.rm=TRUE)
  return(0.5*(xm+ym)*(xm-ym))
}
pifMat <- function(x, y) {
  xm <- rowMeans(x, na.rm=TRUE)
  ym <- rowMeans(y, na.rm=TRUE)
  return(0.5*(xm+ym)*(xm-ym))
}


rif1Vec <- function(xi, yi,
                    xj, yj,
                    method=c("spearman", "pearson", "kendall")) {
  ## xi, yi: expression of regulator i in two conditions
  ## xj, yj: expression of DE gene j in two conditions
  method <- match.arg(method)
  pifj <- pifVec(xj,yj)
  xCij <- cor(xi, xj, use="complete.obs", method=method)
  yCij <- cor(yi, yj, use="complete.obs", method=method)
  dCij2 <- (xCij - yCij)^2
  return(pifj*dCij2)
}
rif1Mat <- function(xReg, yReg,
                    xDe, yDe,
                    method=c("spearman", "pearson", "kendall")) {
  ## xReg, yReg: expression matrix of regulators in two conditions
  ## xDe, yDe: expression matrix of DE genes in two conditions
  haltifnot(nrow(xReg)==nrow(yReg) & nrow(xDe)==nrow(yDe),
            msg="xReg(xDe) and yReg(yDe) must each have the same number of rows (genes)")
  haltifnot(ncol(xReg)==ncol(xDe) & ncol(yReg)==ncol(yDe),
            msg="xReg(yReg) and xDe(yReg) must have the same number of columns (samples)")
            
  method <- match.arg(method)
  pifs <- pifMat(xDe, yDe)
  xCij <- cor(t(xDe), t(xReg), use="complete.obs", method=method)
  yCij <- cor(t(yDe), t(yReg), use="complete.obs", method=method)
  dC2 <- (xCij-yCij)^2
  return(colMeans(pifs * dC2, na.rm=TRUE))            
}


rif2Vec <- function(xi, yi,
                    xj, yj,
                    method=c("spearman", "pearson", "kendall")) {
  ## xi, yi: expression of regulator i in two conditions
  ## xj, yj: expression of DE gene j in two conditions
  method <- match.arg(method)
  Exj <- mean(xj, na.rm=TRUE)
  rx <- cor(xi, xj, use="complete.obs", method=method)
  Eyj <- mean(yj, na.rm=TRUE)
  ry <- cor(yi, yj, use="complete.obs", method=method)
  return((Exj*rx)^2-(Eyj*ry)^2)
}

rif2Mat <- function(xReg, yReg,
                    xDe, yDe,
                    method=c("spearman", "pearson", "kendall")) {
  ## xReg, yReg: expression matrix of regulators in two conditions
  ## xDe, yDe: expression matrix of DE genes in two conditions
  haltifnot(nrow(xReg)==nrow(yReg) & nrow(xDe)==nrow(yDe),
            msg="xReg(xDe) and yReg(yDe) must each have the same number of rows (genes)")
  haltifnot(ncol(xReg)==ncol(xDe) & ncol(yReg)==ncol(yDe),
            msg="xReg(yReg) and xDe(yReg) must have the same number of columns (samples)")
            
  method <- match.arg(method)
  xDeMean <- rowMeans(xDe, na.rm=TRUE)
  yDeMean <- rowMeans(yDe, na.rm=TRUE)
  rx <- cor(t(xDe), t(xReg), use="complete.obs", method=method)
  ry <- cor(t(yDe), t(yReg), use="complete.obs", method=method)
  return(colMeans((rx*xDeMean)^2-(ry*yDeMean)^2, na.rm=TRUE))
}
