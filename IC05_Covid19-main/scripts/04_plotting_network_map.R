library(tidyr)
library(stringr)
library(dplyr)
library(quanteda)


library(RColorBrewer)
library(igraph)
library(visNetwork)#? installer
library(ggraph)# ? installer

nodes_5G <- read.csv2("nodes_5G.csv", sep = ',')
edges_5G <- read.csv2("edges_5G.csv", sep = ',')


###Cr?ation des noeuds ou sommets
vertices<-data.frame(
  name=union(unique(b$authorName),unique(b$isReplyToName)),
  id=seq(1:256))
vertices<-vertices[-60,]#on enl?ve un valeur manquante
###Cr?ation d'un objet graph
mygraph<-graph_from_data_frame(edges_5G,directed=F,vertices=nodes_5G)
print(mygraph, e=T, v=T)

###Etude des propri?t?s structurelles du graph : 
nodes_5G$deg<-degree(mygraph, mode="out")
table(nodes_5G$deg)
# Partage en sous-communaut?. 
communities<-cluster_louvain(mygraph) ## on utilise l'algorithme de d?tection de communaut? de louvain
membership(communities)
vertices$group<-membership(communities)
table(vertices$group)

##Visualisation du graphe
palette<-rainbow(27)
my_color<-palette[as.numeric(as.factor(vertices$group))]

plot(mygraph, 
     vertex.size=vertices$deg/4,
     vertex.label=vertices$name,
     vertex.label.color=my_color,
     vertex.label.cex=0.3,
     vertex.label.dist=1,
     vertex.color=my_color,
     edge.color="grey50",
     edge.width=0.5)
