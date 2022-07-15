library(igraph)
library(igraphdata)
library(tcltk2)
library(Frames2)
library(tables)
library(CINNA)
library(signnet)
edges = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s1-edges.csv") 
nodes = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s1-nodes.csv")

graph = graph.data.frame(edges, directed = FALSE, vertices= nodes)

graph<-simplify(graph, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph)
ecount(graph)

#EIGENVECTOR Centrality
V(graph)$eigenvector<- evcent(graph)$vector
 
plot(graph, layout = layout_on_sphere,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "pink", vertex.color = "green", 
     edge.curved = 0.3, pch = 1, vertex.size = (V(graph)$eigenvector/ max(V(graph)$eigenvector))*10, 
     edge.width = 0.05)

#DEGREE CENTRALITY
deg <- degree(graph, mode = "all")
deg <- as.data.frame(deg)
l <- layout.davidson.harel(graph)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

V(graph)$size = degree(graph, mode = "all")/5
plot(graph, layout = l*1.5, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "pink", vertex.size = V(graph)$size, vertex.label.cex = 0.6 )

G.degree <- degree(graph, mode = "all")
occurdeg = as.vector(table(G.degree))
occurdeg = occurdeg/sum(occurdeg)
p_deg = occurdeg/sum(occurdeg)
prob_deg = rev(cumsum(rev(p_deg)))
degree_measure = as.numeric(names(table(G.degree)))
plot(degree_measure,prob_deg,log = "xy",type="o", col="purple")

G.degree.histogram <- as.data.frame(table(G.degree))
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
plot(G.degree.histogram, log = "xy", type = "o", xlab = "Degree", ylab = "Count")



#BETWEENNESS CENTRALITY
be = betweenness(graph, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph)$weight)
V(graph)$shape <- "circle"
V(graph)$color <- "pink"
plot(graph, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "pink", vertex.color = "green", 
     edge.curved = 0.3, pch = 1, vertex.size = (be/max(be))*10, 
     edge.width = 0.05)
max(be) #NED

#CLOSENESS CENTRALITY
cl = closeness(graph, mode = "out", weights = E(graph)$weight) 
plot(graph, layout = layout.davidson.harel,
     vertex.color = "green", edge.curved=.8, 
     pch = 7,edge.color = "pink", vertex.size = (cl/max(cl))*6, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl) #NED

#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc <-  fastgreedy.community(graph, weights=E(graph)$weight)
membership(fc)
communities(fc)
length(fc) #number of communities 
sizes(fc)
plot(fc, graph, layout = layout.davidson.harel, 
     vertex.size= (be/max(be))*20, vertex.label.cex = 0.6, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph)$community)
#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc <- cluster_louvain(graph, weights = E(graph)$weight)
membership(lc)
communities(lc)
plot(lc, graph, vertex.size = (be/max(be))*10, vertex.label.cex = 0.6
     , edge.curved = 0.5, edge.color = "pink",
     pch = 7, vertex.color=rainbow(7, alpha=0.6)[lc$membership])

clus_coe <- transitivity(graph, type = "local")
ver_deg <- degree(graph)
ver_df <- data.frame(clus_coe, ver_deg)
v_deg <- aggregate(clus_coe ~ ver_deg, data = ver_df ,mean)
plot(v_deg, col="blue", log="xy", xlab="vertices degree", ylab="Average transitivity")
plot(ver_deg, clus_coe, col = 4, xlab="degree", ylab="Cluster coefficient   /   transitivity")

     


#For Season 2

edges1 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s2-edges.csv") 
nodes1 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s2-nodes.csv")

graph1 = graph.data.frame(edges1, directed = FALSE, vertices= nodes1)

graph1<-simplify(graph1, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph1)
ecount(graph1)

#EIGENVECTOR Centrality
V(graph1)$eigenvector<- evcent(graph1)$vector

plot(graph1, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "pink", vertex.color = "green", 
     edge.curved = 0.3, pch = 1, vertex.size = (V(graph1)$eigenvector/ max(V(graph1)$eigenvector))*10, 
     edge.width = 0.05)
#DEGREE CENTRALITY
deg1 <- degree(graph1, mode = "all")
deg1 <- as.data.frame(deg1)
l1<- layout.davidson.harel(graph1)
l1 <- norm_coords(l1, ymin=-1, ymax=1, xmin=-1, xmax=1)
V(graph1)$size = degree(graph1, mode = "all")/5
plot(graph1, layout = l1*1.3, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "pink", vertex.size = V(graph1)$size*1.5, rescale = FALSE,
      vertex.label.cex = 0.6 )

G.degree7 <- degree(graph7, mode = "all")
occur7deg = as.vector(table(G.degree7))
occur7deg = occur7deg/sum(occur7deg)
p_deg7 = occur7deg/sum(occur7deg)
prob_deg7 = rev(cumsum(rev(p_deg7)))
degree_measure7 = as.numeric(names(table(G.degree7)))
plot(degree_measure7,prob_deg7,log = "xy",type="o", col="purple")

G.degree.histogram <- as.data.frame(table(G.degree7))
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
plot(G.degree.histogram, log = "xy", type = "o")

#BETWEENNESS CENTRALITY
be1 = betweenness(graph1, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph1)$weight)
V(graph1)$shape <- "circle"
V(graph1)$color <- "pink"
plot(graph1, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "red", vertex.color = "yellow", 
     edge.curved = 0.3, pch = 1, vertex.size = (be1/max(be1))*10, 
     edge.width = 0.05)
max(be1) 

#CLOSENESS CENTRALITY
cl1 = closeness(graph1, mode = "out", weights = E(graph1)$weight) 
plot(graph1, layout = layout.davidson.harel,
     vertex.color = "yellow", edge.curved=.8, 
     pch = 7,edge.color = "magenta", vertex.size = (cl1/max(cl1))*10, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl1) #Daenerys
cl1

#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc1 <-  fastgreedy.community(graph1, weights=E(graph1)$weight)
membership(fc1)
communities(fc1)
sizes(fc1)
plot(fc1, graph1, layout = layout.davidson.harel, 
     vertex.size= (be1/max(be1))*20, vertex.label.cex = 0.6, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph1)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc1 <- cluster_louvain(graph1, weights = E(graph1)$weight)
membership(lc1)
communities(lc1)
sizes(lc1)
plot(lc1, graph1, vertex.size = (be1/max(be1))*10, vertex.label.cex = 0.6
     , layout = layout.davidson.harel , edge.curved = 0.5, edge.color = "black",
     pch = 7, vertex.color=rainbow(6, alpha=0.6)[lc1$membership])

clus_coe1 <- transitivity(graph1, type = "local")
ver_deg1 <- degree(graph1)
ver_df1 <- data.frame(clus_coe1, ver_deg1)
v_deg1 <- aggregate(clus_coe1 ~ ver_deg1, data = ver_df1 ,mean)
plot(v_deg1, col="blue", log="xy", xlab="vertices degree", ylab="Average transitivity")
plot(ver_deg1, clus_coe1, col = 4, xlab="degree", ylab="Cluster coefficient   /   transitivity")




#FOR SEASON 3
edges2 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s3-edges.csv") 
nodes2 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s3-nodes.csv")

graph2 = graph.data.frame(edges2[, -1], directed = FALSE, vertices= nodes2)

graph2<-simplify(graph2, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph2)
ecount(graph2)
deg <- degree(graph, mode = "all")
deg <- as.data.frame(deg)
l <- layout.davidson.harel(graph)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

V(graph)$size = degree(graph, mode = "all")/5
plot(graph, layout = l*1.5, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "pink", vertex.size = V(graph)$size, vertex.label.cex = 0.6 )

#DEGREE CENTRALITY
deg2 <- degree(graph2, mode = "all")
deg2 <- as.data.frame(deg2)
l2 <- layout.davidson.harel(graph2)
l2 <- norm_coords(l2, ymin=-1, ymax=1, xmin=-1, xmax=1)

V(graph2)$size = degree(graph2, mode = "all")/5
plot(graph2, layout = l*1.5, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "pink", vertex.size = V(graph2)$size, vertex.label.cex = 0.6 )


#EIGENVECTOR Centrality
V(graph1)$eigenvector<- evcent(graph1)$vector

plot(graph1, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "pink", vertex.color = "green", 
     edge.curved = 0.3, pch = 1, vertex.size = (V(graph1)$eigenvector/ max(V(graph1)$eigenvector))*10, 
     edge.width = 0.05)

#BETWEENNESS CENTRALITY
be1 = betweenness(graph1, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph1)$weight)
V(graph1)$shape <- "circle"
V(graph1)$color <- "pink"
plot(graph1, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "red", vertex.color = "yellow", 
     edge.curved = 0.3, pch = 1, vertex.size = (be1/max(be1))*10, 
     edge.width = 0.05)
max(be1) 

#CLOSENESS CENTRALITY
cl1 = closeness(graph1, mode = "out", weights = E(graph1)$weight) 
plot(graph1, layout = layout.davidson.harel,
     vertex.color = "yellow", edge.curved=.8, 
     pch = 7,edge.color = "magenta", vertex.size = (cl1/max(cl1))*10, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl1) #Daenerys
cl1

#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc1 <-  fastgreedy.community(graph1, weights=E(graph1)$weight)
membership(fc1)
communities(fc1)
sizes(fc1)
plot(fc1, graph1, layout = layout.davidson.harel, 
     vertex.size= (be1/max(be1))*20, vertex.label.cex = 0.6, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph1)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc1 <- cluster_louvain(graph1, weights = E(graph1)$weight)
membership(lc1)
communities(lc1)
sizes(lc1)
plot(lc1, graph1, vertex.size = (be1/max(be1))*10, vertex.label.cex = 0.6
     , layout = layout.davidson.harel , edge.curved = 0.5, edge.color = "black",
     pch = 7, vertex.color=rainbow(6, alpha=0.6)[lc1$membership])




#FOR SEASON 4

edges3 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s4-edges.csv") 
nodes3 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s4-nodes.csv")

graph3 = graph.data.frame(edges3, directed = FALSE, vertices= nodes3)

graph3<-simplify(graph3, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph3)
ecount(graph3)

#DEGREE CENTRALITY
deg3 <- degree(graph3, mode = "all")
deg3 <- as.data.frame(deg3)
l3 <- layout.davidson.harel(graph3)
l3 <- norm_coords(l3, ymin=-1, ymax=2, xmin=-1, xmax=1)

V(graph3)$size = degree(graph3, mode = "all")/5
plot(graph3, layout = l3*3.5, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "pink", vertex.size = V(graph3)$size, vertex.label.cex = 0.6)

G.degree3 <- degree(graph3, mode = "all")
occurdeg3 = as.vector(table(G.degree3))
occurdeg3 = occurdeg3/sum(occurdeg3)
p_deg3 = occurdeg3/sum(occurdeg3)
prob_deg3 = rev(cumsum(rev(p_deg3)))
degree_measure3 = as.numeric(names(table(G.degree3)))
plot(degree_measure3,prob_deg3,log = "xy",type="o", col="purple")

G.degree.histogram3 <- as.data.frame(table(G.degree3))
G.degree.histogram3[,1] <- as.numeric(G.degree.histogram3[,1])
plot(G.degree.histogram3, log = "xy", type = "o", xlab = "Degree", ylab = "Count")

#EIGENVECTOR Centrality
V(graph3)$eigenvector<- evcent(graph3)$vector

plot(graph3, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "orange", vertex.color = "green", 
     edge.curved = 0.3, pch = 10, vertex.size = (V(graph3)$eigenvector/ max(V(graph3)$eigenvector))*8, 
     edge.width = 0.05)

#BETWEENNESS CENTRALITY
be3 = betweenness(graph3, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph3)$weight)

plot(graph3, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "dark green", vertex.color = "orange", 
     edge.curved = 0.3, pch = 10, vertex.size = (be3/max(be3))*10, 
     edge.width = 0.05)
max(be3) 

#CLOSENESS CENTRALITY
cl3 = closeness(graph3, mode = "out", weights = E(graph3)$weight) 
plot(graph3, layout = layout.davidson.harel,
     vertex.color = "yellow", edge.curved=.8, 
     pch = 7,edge.color = "orange", vertex.size = (cl3/max(cl3))*10, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl3) #Daenerys
cl3

#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc3 <-  fastgreedy.community(graph3, weights=E(graph3)$weight)
membership(fc3)
communities(fc3)
sizes(fc3)
plot(fc3, graph3, layout = layout.davidson.harel, 
     vertex.size= (be3/max(be3))*15, vertex.label.cex = 0.5, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph3)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc3 <- cluster_louvain(graph3, weights = E(graph3)$weight)
membership(lc3)
communities(lc3)
sizes(lc3)
plot(lc3, graph3, vertex.size = (be3/max(be3))*10, vertex.label.cex = 0.6
     , layout = layout.davidson.harel , edge.curved = 0.5, edge.color = "black",
     pch = 7, vertex.color=rainbow(6, alpha=0.6)[lc3$membership])

clus_coe3 <- transitivity(graph3, type = "local")
ver_deg3 <- degree(graph3)
ver_df3 <- data.frame(clus_coe3, ver_deg3)
v_deg3 <- aggregate(clus_coe3 ~ ver_deg3, data = ver_df3 ,mean)
plot(v_deg3, col="blue", log="xy", xlab="vertices degree", ylab="Average transitivity")
plot(ver_deg3, clus_coe3, col = 4, xlab="degree", ylab="Cluster coefficient   /   transitivity")




#FOR SEASON 5

edges4 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s5-edges.csv")
nodes4 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s5-nodes.csv")
graph4 = graph.data.frame(edges4, directed =FALSE, vertices = nodes4)
graph4<-simplify(graph4, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph4)
ecount(graph4)



#EIGENVECTOR Centrality
V(graph3)$eigenvector<- evcent(graph3)$vector

plot(graph3, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "orange", vertex.color = "green", 
     edge.curved = 0.3, pch = 10, vertex.size = (V(graph3)$eigenvector/ max(V(graph3)$eigenvector))*8, 
     edge.width = 0.05)

#BETWEENNESS CENTRALITY
be3 = betweenness(graph3, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph3)$weight)

plot(graph3, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "dark green", vertex.color = "orange", 
     edge.curved = 0.3, pch = 10, vertex.size = (be3/max(be3))*10, 
     edge.width = 0.05)
max(be3) 

#CLOSENESS CENTRALITY
cl3 = closeness(graph3, mode = "out", weights = E(graph3)$weight) 
plot(graph3, layout = layout.davidson.harel,
     vertex.color = "yellow", edge.curved=.8, 
     pch = 7,edge.color = "orange", vertex.size = (cl3/max(cl3))*10, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl3) #Daenerys
cl3

#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc3 <-  fastgreedy.community(graph3, weights=E(graph3)$weight)
membership(fc3)
communities(fc3)
sizes(fc3)
plot(fc3, graph3, layout = layout.davidson.harel, 
     vertex.size= (be3/max(be3))*15, vertex.label.cex = 0.5, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph3)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc3 <- cluster_louvain(graph3, weights = E(graph3)$weight)
membership(lc3)
communities(lc3)
sizes(lc3)
plot(lc3, graph3, vertex.size = (be3/max(be3))*10, vertex.label.cex = 0.6
     , layout = layout.davidson.harel , edge.curved = 0.5, edge.color = "black",
     pch = 7, vertex.color=rainbow(6, alpha=0.6)[lc3$membership])





#FOR SEASON 6
edges5 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s6-edges.csv")
nodes5 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s6-nodes.csv")
graph5 = graph.data.frame(edges5, directed =FALSE, vertices = nodes5)
graph5 <- simplify(graph5, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph5)
ecount(graph5)

#DEGREE CENTRALITY
deg5 <- degree(graph5, mode = "all")
deg5 <- as.data.frame(deg5)
l5 <- layout.davidson.harel(graph5)
l5 <- norm_coords(l5, ymin=-1, ymax=2, xmin=-1, xmax=1)

V(graph5)$size = degree(graph5, mode = "all")/5
plot(graph5, layout = l5*3.5, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "orange", vertex.size = V(graph5)$size, vertex.label.cex = 0.6, vertex.label = NA)


#EIGENVECTOR Centrality
V(graph5)$eigenvector<- evcent(graph5)$vector

plot(graph5, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "red", vertex.color = "orange", 
     edge.curved = 0.3, pch = 10, vertex.size = (V(graph5)$eigenvector/ max(V(graph5)$eigenvector))*8, 
     edge.width = 0.05)

#BETWEENNESS CENTRALITY
be5 = betweenness(graph5, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph5)$weight)

plot(graph5, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "green", vertex.color = "magenta", 
     edge.curved = 0.3, pch = 10, vertex.size = (be5/max(be5))*10, 
     edge.width = 0.05)
max(be5) 

#CLOSENESS CENTRALITY
cl5 = closeness(graph5, mode = "out", weights = E(graph5)$weight) 
plot(graph5, layout = layout.davidson.harel,
     vertex.color = "yellow", edge.curved=.8, 
     pch = 7,edge.color = "orange", vertex.size = (cl5/max(cl5))*10, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl5) #Daenerys


#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc5 <-  fastgreedy.community(graph5, weights=E(graph5)$weight)
membership(fc5)
communities(fc5)
sizes(fc5)
plot(fc5, graph5, layout = layout.davidson.harel, 
     vertex.size= (be5/max(be5))*15, vertex.label.cex = 0.6, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph5)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc5 <- cluster_louvain(graph5, weights = E(graph5)$weight)
membership(lc5)
communities(lc5)
sizes(lc5)
plot(lc5, graph5, vertex.size = (be5/max(be5))*10, vertex.label.cex = 0.6
     , layout = layout.davidson.harel , edge.curved = 0.5, edge.color = "black",
     pch = 7, vertex.color=rainbow(6, alpha=0.6)[lc5$membership])

clus_coe5 <- transitivity(graph5, type = "local")
ver_deg5 <- degree(graph5)
ver_df5 <- data.frame(clus_coe5, ver_deg5)
v_deg5 <- aggregate(clus_coe5 ~ ver_deg5, data = ver_df5 ,mean)
plot(v_deg5, col="blue", log="xy", xlab="vertices degree", ylab="Average transitivity")
plot(ver_deg5, clus_coe5, col = 4, xlab="degree", ylab="Cluster coefficient   /   transitivity")





#FOR SEASON 7
edges6 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s7-edges.csv")
nodes6 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s7-nodes.csv")
graph6 = graph.data.frame(edges6, directed =FALSE, vertices = nodes6)
graph6 <- simplify(graph6, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph6)
ecount(graph6)

#DEGREE CENTRALITY
deg6 <- degree(graph6, mode = "all")
deg6 <- as.data.frame(deg6)
l6 <- layout.davidson.harel(graph6)
l6 <- norm_coords(l6, ymin=-1, ymax=2, xmin=-1, xmax=1)

V(graph6)$size = degree(graph6, mode = "all")/5
plot(graph6, layout = l6*3.5, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "light blue", vertex.size = V(graph6)$size, vertex.label.cex = 0.6)

#EIGENVECTOR Centrality
V(graph6)$eigenvector<- evcent(graph6)$vector

plot(graph6, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "light blue", vertex.color = "red", 
     edge.curved = 0.3, pch = 10, vertex.size = (V(graph6)$eigenvector/ max(V(graph6)$eigenvector))*8, 
     edge.width = 0.05)

#BETWEENNESS CENTRALITY
be6 = betweenness(graph6, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph6)$weight)

plot(graph6, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "light blue", vertex.color = "orange", 
     edge.curved = 0.3, pch = 10, vertex.size = (be6/max(be6))*10, 
     edge.width = 0.05)
max(be6) 

#CLOSENESS CENTRALITY
cl6 = closeness(graph6, mode = "out", weights = E(graph6)$weight) 
plot(graph6, layout = layout.davidson.harel,
     vertex.color = "orange", edge.curved=.8, 
     pch = 7,edge.color = "cyan", vertex.size = (cl6/max(cl6))*10, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl6) #Daenerys


#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc6 <-  fastgreedy.community(graph6, weights=E(graph6)$weight)
membership(fc6)
communities(fc6)
sizes(fc6)
plot(fc6, graph6, layout = layout.davidson.harel, 
     vertex.size= (be6/max(be6))*20, vertex.label.cex = 0.6, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph6)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc6 <- cluster_louvain(graph6, weights = E(graph6)$weight)
membership(lc6)
communities(lc6)
sizes(lc6)
plot(lc6, graph6, vertex.size = (be6/max(be6))*20, vertex.label.cex = 0.6
     , layout = layout.davidson.harel , edge.curved = 0.5, edge.color = "black",
     pch = 7, vertex.color=rainbow(5, alpha=0.6)[lc6$membership])

clus_coe6 <- transitivity(graph6, type = "local")
ver_deg6 <- degree(graph6)
ver_df6 <- data.frame(clus_coe6, ver_deg6)
v_deg6 <- aggregate(clus_coe6 ~ ver_deg6, data = ver_df6 ,mean)
plot(v_deg6, col="blue", log="xy", xlab="vertices degree", ylab="Average transitivity")
plot(ver_deg6, clus_coe6, col = 4, xlab="degree", ylab="Cluster coefficient   /   transitivity")




#FOR SEASON 8

edges7 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s8-edges.csv")
nodes7 = read.csv("C:\\Mehul\\UIC\\SECOND SEMESTER\\IDS 564 (SOCIAL)\\Project\\Project pictures\\CSV\\got-s8-nodes.csv")
graph7 = graph.data.frame(edges7, directed =FALSE, vertices = nodes7)
graph7 <- simplify(graph7, remove.multiple=TRUE, remove.loops=TRUE)
vcount(graph7)
ecount(graph7)

#DEGREE CENTRALITY
deg7 <- degree(graph7, mode = "all")
deg7 <- as.data.frame(deg7)
l7 <- layout.davidson.harel(graph7)
l7 <- norm_coords(l7, ymin=-1, ymax=2, xmin=-1, xmax=1)

V(graph7)$size = degree(graph7, mode = "all")/5
plot(graph7, layout = l7*10, edge.curved = 0.5, 
     edge.arrow.size=0.25, edge.arrow.mode = "-", 
     vertex.color = "light green", vertex.size = V(graph7)$size/max(V(graph7)$size)*10, vertex.label.cex = 0.6)

G.strength<- strength(graph7)
occur1 = as.vector(table(G.strength))
occur1 = occur1/sum(occur1)
p_strength = occur1/sum(occur1)
prob_tie = rev(cumsum(rev(p_strength)))
strength_measure = as.numeric(names(table(G.strength)))
plot(strength_measure, prob_tie,log = "xy",type="o", col="green")

G.degree3 <- degree(graph3, mode = "all")
occurdeg3 = as.vector(table(G.degree3))
occurdeg3 = occurdeg3/sum(occurdeg3)
p_deg3 = occurdeg3/sum(occurdeg3)
prob_deg3 = rev(cumsum(rev(p_deg3)))
degree_measure3 = as.numeric(names(table(G.degree3)))
plot(degree_measure3,prob_deg3,log = "xy",type="o", col="purple")

G.degree.histogram7 <- as.data.frame(table(G.degree7))
G.degree.histogram7[,1] <- as.numeric(G.degree.histogram7[,1])
plot(G.degree.histogram7, log = "xy", type = "o", xlab = "Degree", ylab = "Count")

#EIGENVECTOR Centrality
V(graph7)$eigenvector<- evcent(graph7)$vector

plot(graph7, layout = layout.davidson.harel,
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     edge.color = "light green", vertex.color = "red", 
     edge.curved = 0.3, pch = 10, vertex.size = (V(graph7)$eigenvector/ max(V(graph7)$eigenvector))*5, 
     edge.width = 0.05)

#BETWEENNESS CENTRALITY
be7 = betweenness(graph7, normalized=T,  directed = TRUE, nobigint= TRUE, weights = E(graph7)$weight)

plot(graph7, layout = layout.davidson.harel,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     edge.color = "light green", vertex.color = "orange", 
     edge.curved = 0.3, pch = 10, vertex.size = (be7/max(be7))*10, 
     edge.width = 0.05)
max(be7) 

G.between7 <- betweenness(graph7, directed = FALSE, weights = NULL)
occur7be = as.vector(table(G.between7))
occur7be = occur7be/sum(occur7be)
p_between7 = occur7be/sum(occur7be)
prob_be7 = rev(cumsum(rev(p_between7)))
between_measure7 = as.numeric(names(table(G.between7)))
plot(between_measure7,prob_be7,log = "xy",type="o", col="purple")

#CLOSENESS CENTRALITY
cl7 = closeness(graph7, mode = "out", weights = E(graph7)$weight) 
plot(graph7, layout = layout.davidson.harel,
     vertex.color = "magenta", edge.curved=.8, 
     pch = 7,edge.color = "cyan", vertex.size = (cl7/max(cl7))*30, 
     edge.width = 0.05, vertex.label.cex = 0.7) 
max(cl7) #Daenerys


#COMMUNITY DETECTION VIA FASTGREEDY ALGORITHM
fc7 <-  fastgreedy.community(graph7, weights=E(graph7)$weight)
membership(fc7)
communities(fc7)
sizes(fc7)
plot(fc7, graph7, layout = layout_with_kk, 
     vertex.size= (be7/max(be7))*15, vertex.label.cex = 0.5, 
     vertex.shape= "circle", edge.curved=0.5, 
     vertex.color= V(graph7)$community)

#COMMUNITY DETECTION VIA LOUVIAN CLUSTER
lc7 <- cluster_louvain(graph7, weights = E(graph7)$weight)
membership(lc7)
communities(lc7)
sizes(lc7)
plot(lc7, graph7, vertex.size = (be7/max(be7))*15, vertex.label.cex = 0.6
     , layout = layout_with_kk , edge.curved = 0.5, edge.color = "red",
     pch = 7, vertex.color=rainbow(6, alpha=0.6)[lc7$membership])




clus_coe7 <- transitivity(graph7, type = "local")
ver_deg7 <- degree(graph7)
ver_df7 <- data.frame(clus_coe7, ver_deg7)
v_deg7 <- aggregate(clus_coe7 ~ ver_deg7, data = ver_df7 ,mean)
plot(v_deg7, col="blue", log="xy", xlab="vertices degree", ylab="Average transitivity")
plot(ver_deg7, clus_coe7, col = 4, xlab="degree", ylab="Cluster coefficient   /   transitivity")

