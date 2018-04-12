library(dplyr)
getwd()
setwd("/home/akkal/RProject/IGRAPH/")
Data <- read.table("Leopard.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


Q <- with(Data, table(Year, arrest_district))
links2 <- crossprod(Q, Q) #make adjacency matrix from table csv
links2 <- as.matrix(links2)

links2

#ig <- graph.adjacency(links2, mode="upper", weighted=TRUE )

## various modes for weighted graphs, with some tests
#nzs <- function(ig) sort(ig [ig!=0])

#ommit weight more then 10
links2[ links2<10 ] <- 0
ig <- graph_from_adjacency_matrix((links2 + t(links2))/2, weighted=TRUE,
                                  mode="undirected")

plot(ig)

E(ig)$width <- E(ig)$weight/10
#E(ig)$arrow.size <- 15
#E(ig)$edge.color <- "gray80"
#E(ig)$w <-  E(ig)$weight<=5
#E(ig)$w=FALSE
plot(ig, edge.color="blue", vertex.color="gray50") 

# circular plot
l <- layout_in_circle(ig)
plot(ig, layout=l, edge.color="blue", vertex.label.cex = 0.7, vertex.color="gray50", edge.label=round(E(ig)$weight>=5))


#circular plot with attributes
plot(ig, vertex.color = 'lightblue', vertex.size=10, edge.color="darkgreen",
     vertex.label.font=2, edge.label.font =2, edge.label.cex = 0.7, layout = l, 
     vertex.label.cex = 0.5)

#tree <- make_tree(20, 3)
plot(ig, layout=layout_as_tree)
plot(ig, layout=layout_as_tree(ig, flip.y=TRUE))
plot(ig, layout=layout_as_tree(ig, circular=TRUE))

#tree2 <- make_tree(10, 3) + make_tree(10, 2)
plot(ig, layout=layout_as_tree)
plot(ig, layout=layout_as_tree(ig, root=c(1,11),
                                  rootlevel=c(2,1)))




