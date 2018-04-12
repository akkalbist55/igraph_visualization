library(dplyr)
getwd()
setwd("/home/akkal/RProject/IGRAPH/")
Data <- read.table("Leopard.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


Q <- with(Data, table(Year, arrest_district))
links2 <- crossprod(Q, Q) #make adjacency matrix from table csv
links2 <- as.matrix(links2)

links2

ig <- graph.adjacency(links2, mode="upper", weighted=TRUE )

## various modes for weighted graphs, with some tests
nzs <- function(ig) sort(ig [ig!=0])

#ommit weight more then 10
links2[ links2<10 ] <- 0
g3 <- graph_from_adjacency_matrix((links2 + t(links2))/2, weighted=TRUE,
                                  mode="undirected")

plot(g3)


#Network simple
ig <- graph.adjacency(links2, mode="undirected", weighted=TRUE )
plot(ig)
a <- subset(ig, ig$weight > 3)
aa <- ig[ig > 5]
#Network Edge weight
E(ig)$width <- E(ig)$weight/10
plot(ig) 


#change arrow size and ege color:
E(ig)$arrow.size <- 15
E(ig)$edge.color <- "gray80"
E(ig)$w <-  E(ig)$weight<=5
E(ig)$w=FALSE
plot(ig) 


#plot with Color
plot(ig, edge.color="blue", vertex.color="gray50") 

# circular plot
l <- layout_in_circle(ig)
plot(ig, layout=l, edge.color="blue", vertex.label.cex = 0.7, vertex.color="gray50", edge.label=round(E(ig)$weight>=5))



plot(ig, vertex.color = 'lightblue', vertex.size=10, edge.color="darkgreen", edge.label=round(E(ig)$weight>=5),
     vertex.label.font=2, edge.label.font =2, edge.label.cex = 0.7, layout = l, 
     vertex.label.cex = 0.5)
