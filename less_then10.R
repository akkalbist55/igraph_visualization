library(dplyr)
library(tidyr)
getwd()
setwd("/home/akkal/RProject/IGRAPH/")

Data <- read.table("data.csv", header=T, sep=",")
Data

#Data <- Data[which(Data$Volume > 10),]  #volume selected condition
Q <- with(Data, table(Wildlife, arrest_district))
adj <- crossprod(Q, Q)      #make adjacency matrix from table csv
#write.table("adj", "bist.csv")
adj

#links2 <- crossprod(Q, Q) #make adjacency matrix from table csv

adj <- as.matrix(adj)

adj[ adj<50 ] <- 0

adj[pivot] <- 0


library(igraph)


library(circlize)


res = chordDiagram(x =adj,
                   annotationTrack = "grid", # labels will be plotted later
                   annotationTrackHeight = 0.05, # height for the annotation 'grid'
                   # preAllocateTracks = 2,
                   preAllocateTracks = list(
                     list(track.height = 0.02),
                     list(track.height = 0.02)),# pre allocate a track and later the sector labels will be added
                   directional = 0, 
                   #order = def,  # the countries are ordered by region first then countries
                   #grid.border = grid.border,
                   #grid.col = col,
                   #white line all the way around
                   
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE,
                   transparency = 0)



circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[1] - uy(1, "mm"),
                CELL_META$sector.index, facing = "reverse.clockwise", niceFacing = TRUE,
                adj = c(0.5, 0.5),  cex = 0.5)
  }
)



