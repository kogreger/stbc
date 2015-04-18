##
## ggplot2Helper.R
##
## Version 0.1.20140715
## Author: Konstantin Greger
##
##
## This file contains a few helper functions to ease working with ggplot 
## within R. Under the hood it requires the 'ggplot2' package, so you should 
## run 'install.packages("ggplot2")' to make sure everything works.
##
## None of the functions have any error handing, so they will crash (or worse) 
## is used improperly. Use on your own risk!
##

## extractGgplot2Legend extracts the legend object of an existing ggplot2
## plot. It takes a ggplot2 object and returns a legend object.
## HT: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
##
extractGgplot2Legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}