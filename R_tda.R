library(TDA)
library(TDAmapper)
library(rgexf)
# Importar datos y modifical clases
breastcell <- read.table("/home/luispolanco/Documentos/breastcellsdata.txt", header=T)
n=nrow(breastcell)
d=ncol(breastcell)
breastcell=cbind(breastcell,rep(0,n))
for(i in 1:n){
  ifelse(breastcell[i,d] == 2, breastcell[i,d+1]<- 1 , NA)
  ifelse(breastcell[i,d] == 4, breastcell[i,d+1]<- 2, NA)
}

dist_m=as.matrix(dist(breastcell[,1:d],diag=TRUE,upper=TRUE))

M1D = mapper1D(dist_m, filter_values = 2 * cos(0.5 * (1:100)), num_intervals = 10, percent_overlap = 50, num_bins_when_clustering = 10)
adj_m = M1D[[1]]
num_nodes = nrow(adj_m)

#Crear grafo
my_graph = new.gexf.graph(defaultedgetype = "undirected", meta = list(creator="LP", description="Mi grapho con TDAmapper", keywords="gexf graph, NodosChile, R, rgexf"))

#Agregar nodos
for(i in 1:num_nodes){
  my_graph = add.gexf.node(my_graph, id=i, label=as.character(i), start=NULL, end=NULL, vizAtt=list(color=NULL, position=NULL, size=NULL, shape=NULL, image=NULL), atts=NULL)
}

for (j in 1:num_nodes){
  for (k in 1:j){
    if(adj_m[j,k]==1)
      my_graph = add.gexf.edge(my_graph, j, k, id=paste(c(as.character(j),'-',as.character(k)),collapse=''), type=NULL, label=paste(c(as.character(j),'-',as.character(k)),collapse=''), start=NULL, end=NULL, weight=1, vizAtt = list(color=NULL, thickness=NULL, shape=NULL), atts=NULL, digits = getOption("digits"))
  }
}

print(my_graph, file = "/home/luispolanco/Documentos/R_TDA/my_graph.gexf")