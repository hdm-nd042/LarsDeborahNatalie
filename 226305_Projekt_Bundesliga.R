#### AllinOne

# Laden vom igraph- und visNetwork Paket
library("igraph")
library("visNetwork")

# Einlesen der Edge- und Nodelist
elBundesliga<- read.csv("AllinOne - Edgelist.csv",header=T, as.is=T)
nlBundesliga<- read.csv("AllinOne - Nodelist.csv", header=T, as.is=T)

# Prüfen der Daten
head(elBundesliga)
head(nlBundesliga)

# Umwandeln der Edgelist in eine Matrix
maBundesliga <- as.matrix(elBundesliga)

# Erstellung des igraph Objekts
bundesliga <-graph_from_data_frame(d=maBundesliga,vertices=nlBundesliga, directed=T)

# Ausgabe des igraph-Objekts
bundesliga

# Einfache Visualisierung 
plot(bundesliga)
deg <- degree(bundesliga)
V(bundesliga)$size <- deg*3
V(bundesliga)$color <- "lightblue"
E(bundesliga)$width <- E(bundesliga)$weight*1.5
E(bundesliga)$color <- "lightblue"
E(bundesliga)$arrow.size <- .4
plot(bundesliga)

# Netzwerkmaße berechnen
is_connected(bundesliga)
components(bundesliga)
edge_density(bundesliga)
triad_census(bundesliga)
distances(bundesliga, v = V(bundesliga), to = V(bundesliga))

# Akteurmaße berechnen
degree(bundesliga)
degree(bundesliga, V(bundesliga)$type == "2", mode = c("in"), loops = TRUE, normalized = TRUE)
eigen_centrality(bundesliga)
coreness(bundesliga)

# Verbesserte Visualisierung. Unterscheidung zwischen Spieler und Vereinen mittels Farben
vcolorbundesliga <-vcount(bundesliga)
vcolorbundesliga[V(bundesliga)$type == "1"] <-"green"
vcolorbundesliga[V(bundesliga)$type == "2"] <-"gold"
coords <- layout_with_kk(bundesliga)*0.22
plot(bundesliga, edge.arrow.size=0.1, edge.label=E(bundesliga)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorbundesliga,vertex.size= ifelse(V(bundesliga)$type=="1",degree(bundesliga)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coords, rescale = FALSE, ylim=c(-1.5,2.2),xlim=c(-0.5,0.5))

# Visualisierung mit dem visNetwork-Paket
V(bundesliga)$color <- vcolorbundesliga
V(bundesliga)$frame.color <- "transparent"
V(bundesliga)$label.color <- "black"
E(bundesliga)$label <- E(bundesliga)$time
visIgraph(bundesliga, type = "full")

# Erstellung und Visualisierung des Teilnetzwerkes 2011 
bundesliga2011 <- subgraph.edges(bundesliga, E(bundesliga)[time=="2011"])
bundesliga2011
vcolorbundesliga2011 <-vcount(bundesliga2011)
vcolorbundesliga2011[V(bundesliga2011)$type == "1"] <-"green"
vcolorbundesliga2011[V(bundesliga2011)$type == "2"] <-"gold"
coords2011 <- layout_with_kk(bundesliga2011)*0.4
plot(bundesliga2011, edge.arrow.size=0.1, edge.label=E(bundesliga2011)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorbundesliga2011,vertex.size= ifelse(V(bundesliga2011)$type=="1",degree(bundesliga2011)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coords2011, rescale = FALSE, ylim=c(-1.5,2.2),xlim=c(-0.5,0.5))
visIgraph(bundesliga2011, type = "full")

# Egonetzwerk Jerome Boateng

# Frage: Beim Ausführen des Plot-Befehls erscheint das Ego-Netzwerk von Boateng, jedoch mit falschen Edge Labels. Woran liegt das ?
# edge.label=Boateng[[1]]$time || Boateng, da wir seine edges wollen und nicht die der Bundesliga
Boateng <- make_ego_graph(bundesliga, order=1, c("Jerome Agyenim Boateng"))
plot(Boateng[[1]],  edge.arrow.size=0.1, vertex.color="green", vertex.size=2, edge.color = "lightgrey", edge.label=Boateng[[1]]$time,edge.label.cex=c(1), vertex.label.color="black", vertex.frame.color="transparent")


##### Fc Bayer Muenchen und VfB Stuttgart

elBayern<- read.csv("FC Bayern Muenchen - Edgelist.csv",header=T, as.is=T)
nlBayern<- read.csv("FC Bayern Muenchen - Nodelist.csv", header=T, as.is=T)

elVfB<- read.csv("VfB Stuttgart - Edgelist.csv",header=T, as.is=T)
nlVfB<- read.csv("VfB Stuttgart - Nodelist.csv", header=T, as.is=T)

# Prüfen der Daten
head(elBayern)
head(nlBayern)

head(elVfB)
head(nlVfB)

# Umwandeln der Edgelist in eine Matrix
maBayern <- as.matrix(elBayern)
maVfB<- as.matrix(elVfB)

# Erstellung des igraph Objekts
Bayern <-graph_from_data_frame(d=maBayern,vertices=nlBayern, directed=T)
VfB <-graph_from_data_frame(d=maVfB,vertices=nlVfB, directed=T)

# Ausgabe des igraph-Objekts
Bayern
VfB

# Visualisierung
vcolorBayern <-vcount(Bayern)
vcolorBayern[V(Bayern)$type == "1"] <-"green"
vcolorBayern[V(Bayern)$type == "2"] <-"gold"
V(Bayern)$color <- vcolorBayern
V(Bayern)$frame.color <- "transparent"
V(Bayern)$label.color <- "black"
E(Bayern)$label <- E(Bayern)$time
coordsBayern<- layout_with_kk(Bayern)*0.3
visIgraph(Bayern, type = "full")


vcolorVfB <-vcount(VfB)
vcolorVfB[V(VfB)$type == "1"] <-"lightgrey"
vcolorVfB[V(VfB)$type == "2"] <-"red"
V(VfB)$color <- vcolorVfB
V(VfB)$frame.color <- "transparent"
V(VfB)$label.color <- "black"
E(VfB)$label <- E(VfB)$time
coordsVfB<- layout_with_kk(VfB)*0.25
visIgraph(VfB, type = "full")


plot(Bayern, edge.arrow.size=0.1, edge.label=E(Bayern)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorBayern,vertex.size= ifelse(V(Bayern)$type=="1",degree(Bayern)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coordsBayern, rescale = FALSE, ylim=c(-2.2,2.2),xlim=c(-0.5,2.2))
plot(VfB, edge.arrow.size=0.1, edge.label=E(VfB)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorVfB,vertex.size= ifelse(V(VfB)$type=="1",degree(VfB)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coordsVfB, rescale = FALSE, ylim=c(-2.2,2.2),xlim=c(-0.5,2.2))
####  Mueller und Gomez

# Laden von Paketen
library(png)
library(tidygraph)
library(ggraph)
library(grid)

# Einlesen der Edge- und Nodelist
elMueller<- read.csv("Mueller - Edgelist.csv",header=T, as.is=T)
nlMueller<- read.csv("Mueller - Nodelist.csv", header=T, as.is=T)

elGomez<- read.csv("Gomez - Edgelist.csv",header=T, as.is=T)
nlGomez<- read.csv("Gomez - Nodelist.csv", header=T, as.is=T)

# Prüfen der Daten
head(elMueller)
head(nlMueller)

head(elGomez)
head(nlGomez)

#tidygraph Objekte
mueller <- tbl_graph(
  nodes = nlMueller, edges = elMueller, directed = TRUE
)

gomez <- tbl_graph(
  nodes = nlGomez, edges = elGomez, directed = TRUE
)

#Arc diagram Mueller
ggraph(mueller, layout = "linear" ) + 
  geom_edge_arc(aes(label = time)) + #aes, damit sich darauffolgende argumente auf das label beziehen, bsp. colour
  geom_node_text(aes(label = id), size = 3, repel = TRUE)+ #Vereinnamen #repel=True damit nicht überlappen
  theme_graph()

#Arc diagram Gomez
ggraph(gomez, layout = "linear" ) + 
  geom_edge_arc(aes(label = time)) + #aes, damit sich darauffolgende argumente auf das label beziehen, bsp. colour
  geom_node_text(aes(label = id), size = 3, repel = FALSE)+ #Vereinnamen #repel=True damit nicht überlappen
  theme_graph()

# Arc diagram mit "Beispiel" Hintergrundbild
ima <- readPNG("C:/Users/ds160/Desktop/226305_Projekt_Bundesliga_final/FC_Bayern_Muenchen.png")
ggraph(mueller, layout = "linear") + 
  annotation_custom(rasterGrob(ima, width = unit(0.25,"npc"), height = unit(0.25,"npc")),-Inf, Inf, -Inf, Inf)+
  geom_edge_arc(aes(label = time)) + #aes, damit sich darauffolgende argumente auf das label beziehen, bsp. colour
  geom_node_text(aes(label = id), size = 5, repel = TRUE)+ #Vereinnamen #repel=True damit nicht überlappen
  theme_graph()


