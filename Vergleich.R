library("igraph")
elBundesliga<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Jugend-Edgelist.csv",header=T, as.is=T, sep=",")
nlBundesliga<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Jugend-Nodelist.csv", header=T, as.is=T, sep=",")
maBundesliga <- as.matrix(elBundesliga)
bundesliga <-graph_from_data_frame(d=maBundesliga,vertices=nlBundesliga, directed=T)
bundesliga

m <- make_ego_graph(bundesliga,order = 1, c("FC Bayern Muenchen Jugend"))      #in c("") die jeweilige id des Knoten eintragen #oder order= 2
plot(m[[1]],edge.arrow.size=0.1, edge.label.size=.1, edge.label=E(m[[1]])$time, edge.label.cex=c(1), edge.color="lightgrey",vertex.size=5, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(1))
