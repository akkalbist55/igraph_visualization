getwd()
setwd("/home/akkal/RProject/NEW_R/")

Data <- read.table("data.csv", header=T, sep=",")
Data

Q <- with(Data, table(Year,arrest_district))
adj <- crossprod(Q, Q)      #make adjacency matrix from table
adj


write.table("adj", "bist.csv")




library(circlize)


res = chordDiagram(x = adj,
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
    
    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[1] - uy(2, "mm"),
                CELL_META$sector.index, facing = "reverse.clockwise", niceFacing = TRUE,
                adj = c(1.8, 0.5),  cex = 0.2)
  }
)




circos.trackPlotRegion(ylim = c(0, 1), factors = adj$region, track.height=0.1,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "vertical_right", "vertical_left")
                         aa = c(1, 0.5)
                         if(theta < 90 || theta > 270)  aa =c(0, 0.5)
                         
                         #plot country labels
                         circos.text(x=mean(xlim), y=1.7, labels=name, direction = dd, cex=0.6,  adj = aa)
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = df1$rcol[i], border=df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3, 
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(from=0,to=floor(df1$xmax)[i],by=5), 
                                     minor.ticks=1, labels.away.percentage = 0.15)
                       })

##


res = chordDiagram(x = adj,
                   annotationTrack = "grid", # labels will be plotted later
                   annotationTrackHeight = 0.05, # height for the annotation 'grid'
                   preAllocateTracks = 0.1, # pre allocate a track and later the sector labels will be added
                   directional = 0, 
                   #order = def,  # the countries are ordered by region first then countries
                   #grid.border = grid.border,
                   #grid.col = col,
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE,
                   transparency = 0)
circos.track(ylim = c(0, 1), factors = factors, bg.border = NA,
             panel.fun = function(x, y) {
               i = get.cell.meta.data("sector.numeric.index")
               if(i %in% c(1, 6, 7, 8)) add_yang_yao() else add_yin_yao()
             }, track.height = 0.1)



circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[1] - uy(2, "mm"),
                CELL_META$sector.index, facing = "reverse.clockwise", niceFacing = TRUE,
                adj = c(1.8, 0.5),  cex = 0.5)
  }
)


