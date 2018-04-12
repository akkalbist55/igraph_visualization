library(dplyr)
getwd()
setwd("/home/akkal/RProject/IGRAPH/")
Data <- read.table("data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Q <- with(Data, table(Wildlife, arrest_district))
links2 <- crossprod(Q, Q) #make adjacency matrix from table csv

links2 <- as.matrix(links2)

links2[ links2<10 ] <- 0

library(igraph)

#Network simple
ig <- graph.adjacency(links2, mode="undirected", weighted=TRUE)
plot(ig)


#Network Edge weight
E(ig)$width <- E(ig)$weight/75
plot(ig) 

#change arrow size and ege color:
E(ig)$arrow.size <- 15
E(ig)$edge.color <- "gray80"
E(ig)$w <-  E(ig)$weight > 10
E(ig)$w
plot(ig) 

#plot with Color
plot(ig, edge.color="blue", vertex.color="gray50") 

# circular plot
l <- layout_in_circle(ig)
plot(ig, layout=l, edge.color="blue", vertex.label.cex = 0.7, vertex.color="gray50", edge.label=round(E(ig)$weight>=10))



plot(ig, vertex.color = 'lightblue', vertex.size=15, edge.color="darkgreen",
     vertex.label.font=1, edge.label.font =5, edge.label.cex = 0.3, layout = l,
     vertex.label.cex = 0.5)




#after Filter plot tree igraph
plot(ig, layout=layout_as_tree)
plot(ig, layout=layout_as_tree(ig, flip.y=TRUE))
plot(ig, layout=layout_as_tree(ig, circular=TRUE))


















getwd()
setwd("/home/akkal/RProject/IGRAPH/")

Data <- read.table("all_data.csv", header=T, sep=",")
Data

Data <- Data[which(Data$Volume > 10),]  #volume selected condition
Q <- with(Data, table(Wildlife, arrest_district))
adj <- crossprod(Q, Q)      #make adjacency matrix from table csv
#write.table("adj", "bist.csv")
ig <- graph.adjacency(adj, mode="undirected", weighted=TRUE)
plot(ig)
E(ig)$arrow.size <- 15
E(ig)$edge.color <- "gray80"
E(ig)$w <-  E(ig)$weight > 10
E(ig)$w
plot(ig) 
#edge balanced
E(ig)$width <- E(ig)$weight/5
plot(ig) 

#plot with Color
plot(ig, edge.color="blue", vertex.color="gray50") 

# circular plot
l <- layout_in_circle(ig)
plot(ig, layout=l, edge.color="blue", vertex.label.cex = 0.7, vertex.color="gray50", edge.label=round(E(ig)$weight>=10))



plot(ig, vertex.color = 'lightblue', vertex.size=15, edge.color="darkgreen", edge.label=round(E(ig)$weight>=5),
     vertex.label.font=1, edge.label.font =5, edge.label.cex = 0.5, layout = l,
     vertex.label.cex = 0.5)

