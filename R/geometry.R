xy2i = function(x,y) {
    .Deprecated("xy2indices",
                msg = paste("xy2i is deprecated and will be removed in the next BioC release.\n",
                "Use xy2indices in the affy package instead.", sep=""))
    y*1048+x+1
}
i2xy = function(i) {
    .Deprecated("indices2xy",
                msg = paste("i2xy is deprecated and will be removed in the next BioC release.\n",
                "Use indices2xy in the affy package instead.", sep=""))
    r=cbind((i-1)%%1048,(i-1)%/%1048); colnames(r)=c('x','y'); return(r)
}
