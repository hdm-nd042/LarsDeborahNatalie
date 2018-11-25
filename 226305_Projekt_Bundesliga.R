#############AllinOne#############

# Laden vom igraph- und visNetwork Paket
library("igraph")
library("visNetwork")
# Achtung, durch den folgenden Befehl wir die Ausgabe in der Konsole auf 1000000 Zeilen erweitert. Dies kann bei einigen im Skript folgenden Befehlen zu langen Ladezeiten oder Absturz von R-Studio führen
options(max.print=1000000)
# Einlesen der Edge- und Nodelist
elBundesliga<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/AllinOne%20-%20Edgelist.csv",header=T, as.is=T, sep=",")
nlBundesliga<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/AllinOne%20-%20Nodelist.csv", header=T, as.is=T, sep=",")

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
gorder(bundesliga)

# Verbesserte Visualisierung. Unterscheidung zwischen Spieler und Vereinen mittels Farben
vcolorbundesliga <-vcount(bundesliga)
vcolorbundesliga[V(bundesliga)$type == "1"] <-"green"
vcolorbundesliga[V(bundesliga)$type == "2"] <-"gold"
coords <- layout_with_kk(bundesliga)*0.1
plot(bundesliga, edge.arrow.size=0.1, edge.label=E(bundesliga)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorbundesliga,vertex.size= ifelse(V(bundesliga)$type=="1",degree(bundesliga)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coords, rescale = FALSE, ylim=c(-1.8,2.2),xlim=c(-0.5,0.5))

# Gesamtnetzwerk ohne Labels
plot(bundesliga, vertex.label= NA, edge.arrow.size=0.1,layout = coords, rescale = FALSE, ylim=c(-1.8,2.2),xlim=c(-0.5,0.5))

#Komponentenanalyse
is_connected(bundesliga)
# True -> Bei dem Netzwerk handelt es sich um eine Komponente


# Clusteranalyse
gc <- cluster_walktrap(bundesliga)
#Modulaity, beschreibt wie weit die Knoten im Netzwerk voneinander entfernt sind
modularity(gc)
# Weist die Mitgliedschaft in die Communities zu
membership(gc)
plot(gc, bundesliga,vertex.label =NA, edge.arrow.size=0.1,edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorbundesliga,vertex.size= ifelse(V(bundesliga)$type=="2",degree(bundesliga)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coords, rescale = FALSE, ylim=c(-2,3),xlim=c(-0.5,0.5))

#Cluster mit Label der Bundesliga Vereine
plot(gc, bundesliga,vertex.label = ifelse(degree(bundesliga) > 20, V(bundesliga)$name, NA), edge.arrow.size=0.1,edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorbundesliga,vertex.size= ifelse(V(bundesliga)$type=="2",degree(bundesliga)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.3),layout = coords, rescale = FALSE, ylim=c(-2,3),xlim=c(-0.5,0.5))

#Dyaden
dyad_census(bundesliga)

#Tryaden
triad_census(bundesliga) 
clique_num(bundesliga)


# Visualisierung mit dem visNetwork-Paket
V(bundesliga)$color <- vcolorbundesliga
V(bundesliga)$frame.color <- "transparent"
V(bundesliga)$label.color <- "black"
E(bundesliga)$label <- E(bundesliga)$time

# Achtung, aufgrund der Größe des Netzwerkes dauert die Durchführung des folgenden Befehles lange und kann zum Abbruch von R Studio führen
visIgraph(bundesliga, type = "full")

# VisNetwork Visualisierung mit besserer Performance
visNetwork(nlBundesliga, elBundesliga) %>%
  visIgraphLayout()

# Erstellung und Visualisierung des Teilnetzwerkes 2011 
bundesliga2011 <- subgraph.edges(bundesliga, E(bundesliga)[time=="2011"])
bundesliga2011
vcolorbundesliga2011 <-vcount(bundesliga2011)
vcolorbundesliga2011[V(bundesliga2011)$type == "1"] <-"green"
vcolorbundesliga2011[V(bundesliga2011)$type == "2"] <-"gold"
coords2011 <- layout_with_kk(bundesliga2011)*0.1
plot(bundesliga2011, edge.arrow.size=0.1, edge.label=E(bundesliga2011)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorbundesliga2011,vertex.size= ifelse(V(bundesliga2011)$type=="1",degree(bundesliga2011)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coords2011, rescale = FALSE, ylim=c(-1.5,2.2),xlim=c(-0.5,0.5))

#Visualisierung des Teilnetzwerkes 2011 mit visNetwork
visIgraph(bundesliga2011, type = "full")

#############Egonetzwerk Michael Esser#############
#Berechnung der Degrees der Vereine der 1. Bundesliga, optional max(), min()
degBundesligaSpieler<- degree(bundesliga)[V(bundesliga)$type=="1"]
degBundesligaSpieler

Esser <- make_ego_graph(bundesliga, order=1, c("Michael Esser"))
plot(Esser[[1]],  edge.arrow.size=0.1, vertex.color="green", vertex.size=2, edge.color = "lightgrey", edge.label=E(Esser[[1]])$time,edge.label.cex=c(1), vertex.label.color="black", vertex.frame.color="transparent")
#############Fc Bayer Muenchen und VfB Stuttgart#############
# Visualisierung vom FC Bayern Muenchen Netzwerk und VFB Stuttgart Netzwerk
elBayern<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/FC%20Bayern%20Muenchen%20-%20Edgelist.csv",header=T, as.is=T, sep=",")
nlBayern<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/FC%20Bayern%20Muenchen%20-%20Nodelist.csv", header=T, as.is=T, sep=",")

elVfB<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/VfB%20Stuttgart%20-%20Edgelist.csv",header=T, as.is=T, sep=",")
nlVfB<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/VfB%20Stuttgart%20-%20Nodelist.csv", header=T, as.is=T, sep=",")

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

# Einfache Visualisierung
vcolorBayern <-vcount(Bayern)
vcolorBayern[V(Bayern)$type == "1"] <-"green"
vcolorBayern[V(Bayern)$type == "2"] <-"gold"
V(Bayern)$color <- vcolorBayern
V(Bayern)$frame.color <- "transparent"
V(Bayern)$label.color <- "black"
E(Bayern)$label <- E(Bayern)$time
coordsBayern<- layout_with_kk(Bayern)*0.3
plot(Bayern, edge.arrow.size=0.1, edge.label=E(Bayern)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorBayern,vertex.size= ifelse(V(Bayern)$type=="1",degree(Bayern)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coordsBayern, rescale = FALSE, ylim=c(-4,2.2),xlim=c(-0.5,2.2))


vcolorVfB <-vcount(VfB)
vcolorVfB[V(VfB)$type == "1"] <-"lightgrey"
vcolorVfB[V(VfB)$type == "2"] <-"red"
V(VfB)$color <- vcolorVfB
V(VfB)$frame.color <- "transparent"
V(VfB)$label.color <- "black"
E(VfB)$label <- E(VfB)$time
coordsVfB<- layout_with_kk(VfB)*0.25
plot(VfB, edge.arrow.size=0.1, edge.label=E(VfB)$time, edge.label.cex=c(0.5), edge.color="lightgrey", vertex.color=vcolorVfB,vertex.size= ifelse(V(VfB)$type=="1",degree(VfB)/3,3),vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(0.5),layout = coordsVfB, rescale = FALSE, ylim=c(-2.2,2.2),xlim=c(-0.5,2.2))

# Bessere Visualisierung
visIgraph(Bayern, type = "full")
visIgraph(VfB, type = "full")


#############Mueller und Gomez#############
# Vergleich der Transferhistorie von Mueller und Gomez
# Laden von Paketen
library(png)
library(tidygraph)
library(ggraph)
library(grid)

# Einlesen der Edge- und Nodelist
elMueller<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Mueller%20-%20Edgelist.csv",header=T, as.is=T)
nlMueller<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Mueller%20-%20Nodelist.csv", header=T, as.is=T)

elGomez<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Gomez%20-%20Edgelist.csv",header=T, as.is=T, sep=",")
nlGomez<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Gomez%20-%20Nodelist.csv", header=T, as.is=T, sep=",")

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
  geom_edge_arc(aes(label = time)) + 
  geom_node_text(aes(label = id), size = 3, repel = TRUE)+
  theme_graph()

#Arc diagram Gomez
ggraph(gomez, layout = "linear" ) + 
  geom_edge_arc(aes(label = time)) + 
  geom_node_text(aes(label = id), size = 2, repel = FALSE)+
  theme_graph()

# Arc diagram mit Hintergrundbild
link <-"https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Images/Bayern/Profis/22.png"
download.file(link,'link.jpg', mode = 'wb')
im <-readPNG("link.jpg",native=TRUE)
ggraph(mueller, layout = "linear") + 
  annotation_custom(rasterGrob(im, width = unit(0.25,"npc"), height = unit(0.5,"npc")),-Inf, Inf, -Inf, Inf)+
  geom_edge_arc(aes(label = time))+
  geom_node_text(aes(label = id), size = 5, repel = TRUE)+
  theme_graph()

linkTwo <-"https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Images/Gomez.png"
download.file(linkTwo,'linkTwo.jpg', mode = 'wb')
imTwo <-readPNG("linkTwo.jpg",native=TRUE)
ggraph(gomez, layout = "linear") + 
  annotation_custom(rasterGrob(imTwo, width = unit(0.25,"npc"), height = unit(0.5,"npc")),-Inf, Inf, -Inf, Inf)+
  geom_edge_arc(aes(label = time))+
  geom_node_text(aes(label = id), size = 5, repel = TRUE)+
  theme_graph()

#############Kaderschmiede#############
elBundesligaJugend<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Jugend-Edgelist.csv",header=T, as.is=T, sep=",")
nlBundesligaJugend<- read.csv("https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Jugend-Nodelist.csv", header=T, as.is=T, sep=",")
maBundesligaJugend <- as.matrix(elBundesligaJugend)
bundesligaJugend <-graph_from_data_frame(d=maBundesligaJugend,vertices=nlBundesligaJugend, directed=T)
bundesligaJugend
vcount(bundesligaJugend)
deg<- degree(bundesligaJugend)
deg

#Berechnung der Degrees der Jugendvereine, optional max(), min()
degJugendvereine<- degree(bundesligaJugend, mode="out")[V(bundesligaJugend)$detail=="3"]
degJugendvereine

#Berechnung der Degrees der Vereine der 1. Bundesliga, optional max(), min()
degBundesligaVereine<- degree(bundesligaJugend)[V(bundesligaJugend)$detail=="6"]
degBundesligaVereine

#Berechnung der Degrees aller Vereine welche nicht in der 1 Bundesliga sind, optional max(), min()
degNichtBundesligaVereine<- degree(bundesligaJugend)[V(bundesligaJugend)$detail=="4"]
degNichtBundesligaVereine

# Erstellung des Sub-Netzwerk FC Bayern Muenchen
Bayern <- subgraph.edges(bundesligaJugend, E(bundesligaJugend)[to("FC Bayern Muenchen")])
gorder(Bayern)

# Erstellung und Visualisierung des aktuellen Kaders des FC Bayern Muenchen
Bayern2018 <- delete_vertices(Bayern, V(Bayern)[label =="Holger Badstuber","Sebastian Rudy", "Alessandro Schoepf", "Felix Goetze","Nils Petersen","Mario Gomez Garcia","Sinan Kurt","Mitchell Elijah Weiser","Diego Armando Valentin Contento","Sebastian Rode","Michael Rensing","Mario Goetze","Claudio Miguel Pizarro Bosio","Marco Friedl","Thomas Kraft"])
gorder(Bayern2018)
plot(Bayern2018, edge.arrow.size=0.1, edge.label.size=.1, edge.label=E(Bayern2018)$time, edge.label.cex=c(1), edge.color="lightgrey",vertex.size=5, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(1))

# Erstellung und Visualisierung des Sub-Netzwerk FC Bayern Muenchen Jugend
JugendBayern <- subgraph.edges(bundesligaJugend, E(bundesligaJugend)[to("FC Bayern Muenchen Jugend")])
plot(JugendBayern, edge.arrow.size=0.1, edge.label.size=.1, edge.label=E(JugendBayern)$time, edge.label.cex=c(1), edge.color="lightgrey",vertex.size=5, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(1))

# Erstellung des Sub-Netzwerk FC Augsburg
Ausgburg <- subgraph.edges(bundesligaJugend, E(bundesligaJugend)[to("FC Augsburg")])
gorder(Ausgburg)

# Erstellung und Visualisierung des aktuellen Kaders des FC Augsburg
Ausgburg2018 <- delete_vertices(Ausgburg, V(Ausgburg)[label =="Paul Verhaegh","Dominik Kohr","Takashi Usami","Alexander Esswein","Matthias Ostrzolek","Ibrahima Traore", "Marwin Hitz","Erik Thommy","Baba Abdul Rahman","Kevin Vogt","Sebastian Langkamp"])
gorder(Ausgburg2018)
plot(Ausgburg2018, edge.arrow.size=0.1, edge.label.size=.1, edge.label=E(Ausgburg2018)$time, edge.label.cex=c(1), edge.color="lightgrey",vertex.size=5, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(1))

# Erstellung und Visualisierung des Sub-Netzwerk FC Augsburg Jugend
JugendAugsburg <- subgraph.edges(bundesligaJugend, E(bundesligaJugend)[to("FC Augsburg Jugend")])
plot(JugendAugsburg, edge.arrow.size=0.1, edge.label.size=.1, edge.label=E(JugendAugsburg)$time, edge.label.cex=c(1), edge.color="lightgrey",vertex.size=5, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", vertex.label.cex=c(1))


###Visualisierung der Mannschaften mit dem visNetwork-Paket

# FC Bayern Muenchen aktueller Kader

# Daten für das VisNetwork generieren
dataBayern2018 <- toVisNetworkData(Bayern2018)

# Bild Pfad festlegen
pathImagesBayern2018 <- "https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Images/Bayern/Profis/"
nodesBayern2018 <- data.frame(dataBayern2018$nodes,
                              font.size = 10,
                              shape = c("circularImage"),
                              image = paste0(pathImagesBayern2018,1:23,".png"))

edgesBayern2018 <- data.frame(dataBayern2018$edges, label=dataBayern2018$edges$time, font.size = 7)

# Netzwerk visualsieren 
visNetwork(nodesBayern2018,edgesBayern2018, width = "100%",height = "700px") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE), color = list(highlight = "red")) %>%
  visLayout(randomSeed = 12)

# FC Bayern Muenchen Jugend
dataJugendBayern <- toVisNetworkData(JugendBayern)

# Bild Pfad festlegen
pathImagesJugendBayern <- "https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Images/Bayern/Jugend/"
nodesJugendBayern <- data.frame(dataJugendBayern$nodes,
                                font.size = 10,
                                shape = c("circularImage"),
                                image = paste0(pathImagesJugendBayern,1:18,".png"))

edgesJugendBayern <- data.frame(dataJugendBayern$edges, label=dataJugendBayern$edges$time, font.size = 7)

# Netzwerk visualsieren 
visNetwork(nodesJugendBayern,edgesJugendBayern, width = "100%",height = "700px") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE), color = list(background = "lightgrey", highlight = "red")) %>%
  visLayout(randomSeed = 12)

# FC Augsburg aktueller Kader

# Daten für das VisNetwork generieren
dataAugsburg2018 <- toVisNetworkData(Ausgburg2018)

# Bild Pfad festlegen
pathImagesAugsburg2018  <- "https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Images/Augsburg/Profis/"
nodesAugsburg2018 <- data.frame(dataAugsburg2018$nodes,
                                font.size = 10,
                                shape = c("circularImage"),
                                image = paste0(pathImagesAugsburg2018,1:32,".png"))

edgesAugsburg2018 <- data.frame(dataAugsburg2018$edges, label=dataAugsburg2018$edges$time, font.size = 7)

# Netzwerk visualsieren 
visNetwork(nodesAugsburg2018,edgesAugsburg2018, width = "100%",height = "700px") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE), color = list(highlight = "red")) %>%
  visLayout(randomSeed = 12)

# FC Augsburg Jugend

# Daten für das VisNetwork generieren
dataJugendAugsburg <- toVisNetworkData(JugendAugsburg)

# Bild Pfad festlegen
pathImagesJugendAugsburg  <- "https://raw.githubusercontent.com/hdm-nd042/LarsDeborahNatalieDaniel/master/Images/Augsburg/Jugend/"
nodesJugendAugsburg <- data.frame(dataJugendAugsburg$nodes,
                                  font.size = 10,
                                  shape = c("circularImage"),
                                  image = paste0(pathImagesJugendAugsburg,1:10,".png"))

edgesJugendAugsburg <- data.frame(dataJugendAugsburg$edges, label=dataJugendAugsburg$edges$time, font.size = 7)

# Netzwerk visualsieren 
visNetwork(nodesJugendAugsburg,edgesJugendAugsburg, width = "100%",height = "700px") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE), color = list(background = "lightgrey",highlight = "red")) %>%
  visLayout(randomSeed = 12)

