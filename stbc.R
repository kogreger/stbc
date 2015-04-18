## initialize
# libraries
library(dplyr)
library(dplyrExtras)
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(reshape2)
library(RPostgreSQL)
library(scales)
library(stringr)

# helper files
source("psqlHelper.R")
source("ggplot2Helper.R")

# database connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 host = "localhost", 
                 port = "1111", 
                 user = "postgres", 
                 password = "postgres", 
                 dbname = "maindb")


## plot parameters
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




## plot parameters
theme_set(theme_bw())
pngWidth <- 512
pngHeight <- 768
pngUnits <- "px"
pngRes <- 120
pngPointSize <- 10

## load modal split data
query <- "SELECT means, COUNT(*) FROM persontrip.subtrip GROUP BY means ORDER BY means"
modalsplit <- psqlQuery(con, query)
modalsplit <- modalsplit %>% 
    filter(means <= 12) %>% 
    mutate_if(., means == 1, tmode = "walk") %>% 
    mutate_if(., means == 2, tmode = "bicycle") %>% 
    mutate_if(., means %in% c(3, 4), tmode = "motorbike") %>% 
    mutate_if(., means == 5, tmode = "taxi") %>% 
    mutate_if(., means %in% c(6, 7), tmode = "car") %>% 
    mutate_if(., means == 8, tmode = "truck") %>% 
    mutate_if(., means %in% c(9, 10), tmode = "bus") %>% 
    mutate_if(., means %in% c(11, 12), tmode = "train") %>% 
    group_by(tmode) %>% 
    summarise(n = sum(count)) %>% 
    select(tmode, n)
# manually apply spatially filtered the data for now
modalsplit$n[modalsplit$tmode == "bicycle"] = 69416
modalsplit$n[modalsplit$tmode == "bus"] = 26632
modalsplit$n[modalsplit$tmode == "car"] = 37664
modalsplit$n[modalsplit$tmode == "motorbike"] = 5960
modalsplit$n[modalsplit$tmode == "taxi"] = 6143
modalsplit$n[modalsplit$tmode == "train"] = 274588
modalsplit$n[modalsplit$tmode == "truck"] = 8748
modalsplit$n[modalsplit$tmode == "walk"] = 463411

## plot modal split data
g <- ggplot(modalsplit, 
            aes(x = factor(tmode), 
                y = n)) + 
    geom_bar(aes(fill = tmode), 
             stat = "identity") + 
    scale_x_discrete(limits=rev(modalsplit$tmode), 
                     name = "") + 
    scale_y_continuous(name = "") + 
    scale_fill_brewer(palette="Set1") + 
    coord_flip() + 
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank(), 
         legend.position = "none")
png(filename = "plots/modalsplit.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
dev.off()



## plot parameters
theme_set(theme_bw())
pngWidth <- 1024
pngHeight <- 500
pngUnits <- "px"
pngRes <- 120
pngPointSize <- 10


## load train station usage data
query <- "SELECT gid, name, dep_bus, dep_car, dep_lcar, 
       dep_truck, dep_pbus, dep_taxi, dep_mbike, dep_smbike, dep_bike, 
       dep_walk, dep_plane, dep_ship, dep_other, dep_unknwn, dep_all, 
       arr_bus, arr_car, arr_lcar, arr_truck, arr_pbus, arr_taxi, arr_mbike, 
       arr_smbike, arr_bike, arr_walk, arr_plane, arr_ship, arr_other, 
       arr_unknwn, arr_all, stations, trtotal, tr00, tr01, tr02, 
       tr03, tr04, tr05, tr06, tr07, tr08, tr09, tr10, tr11, tr12, tr13, 
       tr14, tr15, tr16, tr17, tr18, tr19, tr20, tr21, tr22, tr23, tr_rel00, 
       tr_rel01, tr_rel02, tr_rel03, tr_rel04, tr_rel05, tr_rel06, tr_rel07, 
       tr_rel08, tr_rel09, tr_rel10, tr_rel11, tr_rel12, tr_rel13, tr_rel14, 
       tr_rel15, tr_rel16, tr_rel17, tr_rel18, tr_rel19, tr_rel20, tr_rel21, 
       tr_rel22, tr_rel23, pass00, pass01, pass02, pass03, pass04, pass05, 
       pass06, pass07, pass08, pass09, pass10, pass11, pass12, pass13, 
       pass14, pass15, pass16, pass17, pass18, pass19, pass20, pass21, 
       pass22, pass23
  FROM mlit.commutertokyo;"
usage <- psqlQuery(con, query)
transfer_volume <- usage %>% 
    select(name, starts_with("tr_rel")) %>% 
    melt(id=c("name")) %>% 
    mutate(variable = as.integer(str_sub(variable, -2, -1))) %>% 
    tbl_df()
passenger_volume <- usage %>% 
    select(name, starts_with("pass")) %>% 
    melt(id=c("name")) %>% 
    mutate(variable = as.integer(str_sub(variable, -2, -1))) %>% 
    tbl_df()

## plot train station usage data
# relative hourly commuter volume
g <- ggplot(transfer_volume, 
            aes(x = variable, 
                y = value, 
                group = name)) + 
    geom_line(alpha = .1) + 
    scale_x_discrete(name = "hour") + 
    scale_y_continuous(labels = percent, 
                       name = "percent of daily commuter volume") + 
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          legend.position = "none")
png(filename = "plots/transfer_volume.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
dev.off()

# absolute hourly commuter volume
g <- ggplot(passenger_volume, 
            aes(x = variable, 
                y = value, 
                group = name)) + 
    geom_line(alpha = .1) + 
    scale_x_discrete(name = "hour") + 
    scale_y_continuous(name = "hourly commuter volume") + 
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          legend.position = "none")
png(filename = "plots/passenger_volume.png",
    width = pngWidth, height = pngHeight, units = pngUnits, 
    res = pngRes, pointsize = pngPointSize)
plot(g)
dev.off()