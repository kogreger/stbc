## initialize
library(dplyr)
library(igraph)
pngWidth <- 1024
pngHeight <- 768
pngUnits <- "px"
pngRes <- 90
pngPointSize <- 10

## color bar legend
# (cf. https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/)
legend.col <- function(col, lev = NA) {
    opar <- par
    n <- length(col)
    bx <- par("usr")
    box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
                bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
    box.cy <- c(bx[3], bx[3])
    box.sy <- (bx[4] - bx[3]) / n
    xx <- rep(box.cx, each = 2)
    par(xpd = TRUE)
    for(i in 1:n) {
        yy <- c(box.cy[1] + (box.sy * (i - 1)),
                box.cy[1] + (box.sy * (i)),
                box.cy[1] + (box.sy * (i)),
                box.cy[1] + (box.sy * (i - 1)))
        polygon(xx, yy, col = col[i], border = col[i])
    }
    if(!is.na(lev)) {
        par(new = TRUE)
        plot(0, 0, type = "n",
             ylim = c(min(lev), max(lev)),
             yaxt = "n", ylab = "",
             xaxt = "n", xlab = "",
             frame.plot = FALSE)    
        axis(side = 4, las = 2, tick = FALSE, line = .25)
    }
    par <- opar
}

## load data
# select a.gid id, b.id nd1, c.id nd2
# from edges a, nodes b, nodes c 
# where a.nd1 = b.ndno and a.nd2 = c.ndno
edges <- read.csv("data/edges.csv", 
                  sep = ";", 
                  quote = "\"", 
                  dec = ".")
# select id, ST_X(ST_Centroid(the_geom)) x, ST_Y(ST_Centroid(the_geom)) y 
# from public.nodes order by undno
nodes <- read.csv("data/nodes.csv", 
                  sep = ";", 
                  quote = "\"", 
                  dec = ".")

## preprocess data
edges <- edges %>% 
    tbl_df()
nodes <- nodes %>% 
    tbl_df()

## build network graph
edgeList <- matrix(cbind(edges$nd1, edges$nd2), 
                   ncol = 2)
g <- graph.edgelist(edgeList, directed=FALSE)
V(g)$size = 5
V(g)$x = nodes$x
V(g)$y = nodes$y
V(g)$label = NA
V(g)$frame.color = rgb(74, 74, 74, maxColorValue = 255)
E(g)$color = rgb(74, 74, 74, maxColorValue = 255)
plot(g)

## calculate degree centrality
V(g)$color = degree(g)
palette(rev(heat.colors(max(V(g)$color))))
png(filename = "plots/ex_degree.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
legend.col(col = rev(heat.colors(max(V(g)$color))))
dev.off()

## calculate betweenness centrality
V(g)$color = (betweenness(g) / max(betweenness(g))) * 100
palette(rev(heat.colors(max(V(g)$color))))
png(filename = "plots/ex_betweenness.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
legend.col(col = rev(heat.colors(max(V(g)$color))))
dev.off()

## calculate closeness centrality
V(g)$color = (closeness(g) / max(closeness(g))) * 100
palette(rev(heat.colors(max(V(g)$color))))
png(filename = "plots/ex_closeness.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
legend.col(col = rev(heat.colors(max(V(g)$color))))
dev.off()

## calculate Eigenvector centrality
V(g)$color = evcent(g, scale = TRUE)$vector * 100
palette(rev(heat.colors(max(V(g)$color))))
png(filename = "plots/ex_eigenvector.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
legend.col(col = rev(heat.colors(max(V(g)$color))))
dev.off()