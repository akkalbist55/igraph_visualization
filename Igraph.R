library(igraph)

getwd()
setwd("/home/akkal/RProject/NEW_R/")
Data <- read.table("Tiger.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

library(dplyr)

Q <- with(Data, table(Year, arrest_district))
links2 <- crossprod(Q, Q) #make adjacency matrix from table csv
links2 <- as.matrix(links2)

#Network simple
ig <- graph.adjacency(links2, mode="undirected", weighted=TRUE )
plot(ig)
a <- subset(ig, ig$weight > 3)
aa <- ig[ig > 5]

#Network Edge weight
E(ig)$width <- E(ig)$weight/5
plot(ig) 

#change arrow size and ege color:
E(ig)$arrow.size <- 15
E(ig)$edge.color <- "gray80"
E(ig)$w <-  E(ig)$weight>=5
plot(ig) 


#plot with Color
plot(ig, edge.color="blue", vertex.color="gray50") 

# circular plot
l <- layout_in_circle(ig)
plot(ig, layout=l, edge.color="blue", vertex.label.cex = 0.7, vertex.color="gray50", edge.label=round(E(ig)$weight>=5))



plot(ig, vertex.color = 'lightblue', vertex.size=1, edge.color="darkgreen", edge.label=round(E(ig)$weight>=5),
     vertex.label.font=5, edge.label.font =5, edge.label.cex = 1, layout = l,
     vertex.label.cex = 0.5)



legend(x=1.1, y=1.1, c("Tiger Data"), pch=10,
       col="#777777", pt.bg=colrs, pt.cex=12.5, bty="n", ncol=1)
