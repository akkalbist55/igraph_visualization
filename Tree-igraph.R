library(igraph)

getwd()
setwd("/home/akkal/RProject/IGRAPH/")
Data <- read.table("Leopard.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
Q <- with(Data, table(Year, arrest_district))
links2 <- crossprod(Q, Q) #make adjacency matrix from table csv
links2 <- as.matrix(links2) #make a adjacency table to adjacency matrix

#ig <- graph.adjacency(links2, mode="upper", weighted=TRUE )
## various modes for weighted graphs, with some tests
#nzs <- function(ig) sort(ig [ig!=0])

#ommit weight more then 10
#links2[ links2<10 ] <- 0
ig <- graph_from_adjacency_matrix((links2 + t(links2))/2, weighted=TRUE, mode="undirected")








plot(ig)
E(ig)$arrow.size <- 15
E(ig)$edge.color <- "gray80"
E(ig)$w <-  E(ig)$weight > 10
plot(ig) 
#edge balanced
E(ig)$width <- E(ig)$weight/5
plot(ig) 

#plot with Color
plot(ig, edge.color="blue", vertex.color="gray50") 

# circular plot
l <- layout_in_circle(ig)
plot(ig, layout=l, edge.color="blue", vertex.label.cex = 0.7, vertex.color="gray50")



plot(ig, vertex.color = 'lightblue', vertex.size=15, edge.color="darkgreen",
     vertex.label.font=1, edge.label.font =5, edge.label.cex = 0.5,
     vertex.label.cex = 0.5)


















layout_as_tree(ig, root = numeric(), circular = FALSE,
               rootlevel = numeric(), mode = "out", flip.y = TRUE)
#tree plot
plot(ig, layout=layout_as_tree)
plot(ig, layout=layout_as_tree(ig, flip.y=FALSE))
plot(ig, layout=layout_as_tree(ig, tree  circular=TRUE))

