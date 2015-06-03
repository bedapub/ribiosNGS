library(ribiosDemo)
##dyn.load("/apps/bi/apps/ribios/ribiosDemo/src/bios_nalimov.so")

  
x <- c(30.41,30.05,30.49,29.22,30.40,30.42)
nalimov(x, sig="0.95")
nalimov(x, sig="0.99")
nalimov(x, sig="0.995")
