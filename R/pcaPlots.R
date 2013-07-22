plotPCAloading <- function(loadings, x=1L, y=2L, circle=FALSE, title="", subtitle="",...) {
  plot(loadings[,x],loadings[,y],
       xlim=c(-1,1),ylim=c(-1,1),
       xlab=paste("PC ",x,sep=""),
       ylab=paste("PC ",y,sep=""),
       pch=15,cex=1.3,...)
  grid()
  abline(h=0)
  abline(v=0)
  arrows(0,0,loadings[,x],loadings[,y],lty=3)
  text(loadings[,x],loadings[,y],rownames(loadings),cex=1, adj=c(1.2,1), xpd=T)
  title(main=title,outer=TRUE)
  title(sub=subtitle,outer=TRUE,line=-0.5)
  if(circle && require("plotrix")) {
    plotrix::draw.circle(x=0,y=0,radius=1,border="grey",lwd=2)
  }
}

plotPCAscores <- function(scores, class, legendX, legendY, title="",...) {
  colbase <- brewer.pal.factorLevels(class, name="Set1")
  cols <- colbase[class]
  symbol <- rep(c(15:18,1:4),5L)
  vsym <- symbol[as.numeric(as.factor(class))]
  
  par(mfrow=c(1,2),oma=c(0,0,2,4))
  plot(scores$x[,1],scores$x[,2],
       pch=vsym,
       cex=2,
       col=cols,
       ylab=sprintf("PC2 (%2.1f%%)",summary(scores)$importance[2,2]*100),
       ylab=sprintf("PC1 (%2.1f%%)",summary(scores)$importance[2,1]*100),
       main="Front View",...)
  abline(h=0,v=0,lty=2)
  grid()
  
  par(mfrow=c(1,2),oma=c(0,4,2,0))
  plot(scores$x[,3],scores$x[,2],
       pch=vsym,
       cex=2,
       col=cols,
       ylab=sprintf("PC2 (%2.1f%%)",summary(scores)$importance[2,2]*100),
       xlab=sprintf("PC3 (%2.1f%%)",summary(scores)$importance[2,3]*100),
       main="Side View",...)
  abline(h=0,v=0,lty=2)
  grid()
  
  par(xpd=NA)   # This allows the legend to be printed outside the plot region
  legend(legendX,legendY,
         levels(class),
         pch=symbol[levels(class)],
         col=colbase,
         bty="n",
         cex=1, pt.cex=2,
         title="")
  title(title,outer=TRUE)
  par(xpd=F)
}

plotPCAscores2 <- function(scores,class1,class2,heading="",
                           mar=c(5.1,4.1,4.1,2.1),oma=c(0,0,2,8),
                           pch=c(15:18,1:4),cex=2,col=NULL,bg=NULL,
                           pch2=1:4,cex2=1,col2=NULL,bg2=NULL,
                           leg.x1,leg.y1,leg.x2,leg.y2,leg.cex=1,
                           leg.title1="",leg.title2="",bty="",
                           leg.pt.cex=1,
                           mark=FALSE,id.num,id,lab.cex=0.8,...) {
 
}
