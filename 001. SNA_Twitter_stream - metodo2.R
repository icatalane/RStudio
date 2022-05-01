# title: "Ver Trends"
# author: Iulen IbÃ¡Ã±ez BaÃ±os (iulen.ibanez@datacy.es)
#                             
# last modified: oct 2020
# output: -

# 0. LED ----
# Antes de nada, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Carga de librerias y paquetes
# install.packages("ROAuth")
# install.packages("streamR")
# install.packages("openssl")
# install.packages("RCurl")
# install.packages("rjson")
# install.packages("ndjson")
# install.packages("bitops")
# install.packages("RJSONIO")
# install.packages("twitteR")
# install.packages("httr")

library(ROAuth)
library(streamR)
library(stringr)
library(quanteda)
library(Matrix)
library(topicmodels)
library(tidytext)
library(tm)
library(openssl)
# 
# library(RCurl)
# library(rjson)
# library(ndjson)
# library(bitops)
# library(RJSONIO)
# library(twitteR)
# library(httr)
# install.packages("graphTweets")
# install.packages("rtweet")

library(rtweet)
library(tidyverse)
library(igraph) # for plot
library(graphTweets)
library(dplyr)

# 1. Autorizacion de la app----
# El nombre que le asgnamos a la app en el formulario de autorización
appname <- "RTWEET"

consumer_key <- "0PTl4oqZnOnFU4EaKnq8EJeEN"
## consumer secret (en el ejemplo no es un clave real, usar la verdadera)
consumer_secret <- "qWePw8fNBBRxcRpRTjZRbzrxxNRvHNN6O4WgLhQS61hC9cwPVG"
## consumer key (en el ejemplo no es una clave real, usar la verdadera)
access_token <- "279639492-wijUHyQz68AXgFILwcKXXVfsMvSJCbY7cCB9w7I8"
## consumer secret (en el ejemplo no es un clave real, usar la verdadera)
access_secret <- "P3LMjZ60E4BWsEWE49AJnT4WTdJpFNGwsmyJWL0obdEb9"



twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token, 
  access_secret = access_secret)


tweets <- search_tweets(q = "Trump OR Biden OR Elections2020", n = 10000, include_tweets=TRUE)

###################################################################################################################
# LISTA DE VALORES DEVUELTOS
###################################################################################################################
# screen name <- retwiteador
# user_ID <- ID twiteador
# text <- tweet
# mentions_screen_name <- twiteador
# mentions_user_ID <- ID twiteador
# created_at<- fecha/hora de RT
# source <- origen de escritura de tweet
# display_text_width <- caracteres empleados
# retweet_created_at <- fecha creacion tweet
# retweet_text <- text retwiteado
# lang <- idioma
###################################################################################################################

# Llamamos node1 a quien retwitea (retwiteador)
tweets$node1 <- tweets$screen_name

# Llamamos node2 a quien es retwiteado (retwiteado)
# tweets$mentions_screen_name<-unlist(tweets$mentions_screen_name, use.names = TRUE)
tweets$node2<- gsub('.*RT @([a-zA-Z0-9_]+):? ?.*', tweets$text, repl="\\1")
partido <- reshape2::colsplit(tweets$text, ":", names = c("node2", "resto"))
tweets$node2<-partido$node2
tweets$node2 <- stringr::str_replace_all(tweets$node2, "RT @", "")


# 1.1. Creo el dataframe con los  vertices----
# Calculo el numero de tweets de cada usuario y los retweets que ha alcanzado
vertices <- tweets %>%
  dplyr::group_by(node2, retweet_text) %>%
  dplyr::summarise(n = n(), rtwCount = sum(retweet_count)) %>%
  dplyr::group_by(node2) %>%
  dplyr::summarise(tweets = sum(n), rtwCount = sum(rtwCount))

colnames(vertices)[1] <- "name"

# Completo el dataframe con los usuarios que no han twiteado nada "original"
vertices.df1 <- as.data.frame(tweets$node1)
vertices.df1 <- unique(vertices.df1)
colnames(vertices.df1)[1] <- "name"

tweets$node2<- unlist(tweets$node2, recursive = TRUE, use.names = TRUE)


vertices.df2 <- as.data.frame(tweets$node2)
vertices.df2 <- unique(vertices.df2)
colnames(vertices.df2)[1] <- "name"

vertices.df <- rbind(vertices.df1, vertices.df2)
vertices.df <- unique(vertices.df)

vertices.df <- vertices.df %>%
  dplyr::left_join(vertices, by = c("name" = "name"))

# Convireto los NA en valores numericos (0)
for (i in 1:nrow(vertices.df)) {
  vertices.df$tweets[i] <- ifelse(is.na(vertices.df$tweets[i]) == TRUE, 0, vertices.df$tweets[i])
  vertices.df$rtwCount[i] <- ifelse(is.na(vertices.df$rtwCount[i]) == TRUE, 0, vertices.df$rtwCount[i])
}

# 1.2. Creo el dataframe con los edges----
edges.df <- tweets %>%
  dplyr::group_by(node1, node2) %>%
  dplyr::summarise(edgeW = n()) %>%
  dplyr::ungroup()

colnames(edges.df)[1] <- "from"
colnames(edges.df)[2] <- "to"

# 2. Data model----
# 2.1. Creo el objeto grafo----
g <- graph_from_data_frame(d = edges.df, vertices = vertices.df, directed = TRUE)

# 2.2. Ploteo el grafo----
plot(g)

V(g)$color <- ifelse(
  V(g)$tweets > 10, "purple", "white"
)

plot(g, vertex.label.color = "black")


tweets %>%
  gt_edges(text, screen_name, status_id) %>%
  gt_graph() %>%
  plot()

# popularidad de usuarios
options(scipen = 20)
ggplot(tweets) +
  geom_histogram(aes(x = followers_count))

#  TOP5 usuarios mas populares
tweets %>% 
  top_n(5, followers_count) %>% 
  arrange(desc(followers_count)) %>% 
  select(screen_name, followers_count, location, text)

# tweets más populares
ggplot(filter(tweets, !is_retweet))+
  geom_histogram(aes(x = retweet_count))

#  Identifiquemos el tweet original más que sumó más retweets:
tweets %>% 
  filter(!is_retweet) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  select(screen_name, retweet_count, followers_count, location, text)

# hora del día que se publican los tweets
ts_plot(tweets, "minutes")

# Probamos extraer el top 10 de lugares más frecuentes, eliminando los tweets de
# usuarios sin datos en su atributo "location".
tweets %>%
  filter(location != "", !is.na(location)) %>% 
  count(location) %>% 
  top_n(10, n) %>% 
  ggplot() +
  geom_col(aes(x = reorder(location, n), y = n)) + 
  coord_flip() +
  labs(title = "Procedencia de los usuarios",
       x = "ubicación",
       y = "cantidad")
# 
# "escuchar" el stream de Twitter por un minuto (60 segundos)
captura_streaming <- stream_tweets(q = "Trump OR Biden OR Elections2020", timeout = 120)

# Capturando tweets por períodos prolongados
terminos_de_busqueda <- "Trump OR Biden OR Elections2020"

#una semana: 60 segundos * 60 * 24 * 7
tiempo <- 60 * 60 * 24 * 7

# El archivo donde guardar los resultados en disco (tendrá formato json, así que lo usamos en el nombre de archivo)
archivo <- "busqueda_tweets_DT_VP.json"


stream_tweets(q = terminos_de_busqueda,
              timeout = tiempo,
              file_name = archivo,
              parse = FALSE)

# en el paso anterior definimos que el nombre de archivo es "busqueda_tweets_DT_VP.json"
tweets <- parse_stream("busqueda_tweets_DT_VP.json")



# METODO 1 #########################################################################################3
# # Al ejecutar la siguiente orden, se abrira una ventana del navegador. En ella aparecera
# # un codigo que debemos copiar y pegar en la consola de RStudio.
# # download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# 
# # Mediante el siguiente comando guardamos el token de autenticacion
# # para usarlo en futuras sesiones con streamR. En la carpeta donde lo guardemos
# # no puede haber ningun otro archivo.
# save(my_oauth, file="credentials/twitter-token.Rdata")
# 
# # 2. Captura de datos (Tweets)----
# # Cargamos nuestras credenciales
# load("credentials/twitter-token.Rdata")
# 
# # Captura de tweets filtrando por palabra clave o hashtag
# # Podemos indicar uno o varios terminos.
# # Podemos (y debemos) limitar el tiempo de captura (en segundos)
# filterStream(file.name="covid19.json", track=c("covid19", "lacovid", "coronavirus"),
#              timeout=120, oauth=my_oauth)
# 
# # 1h = 3600s; 2h = 7200s, 3h = 10800s; 4h = 14400s; 5h = 18000s; 6h = 21600s.
# 
# # 3. Inspecciond e datos capturados----
# # Lo abrimos en R con el "parseTweets"
# tweets <- parseTweets("covid19.json")
# 
# # 3.1. Variables disponibles----
# str(tweets)
# # En los tweets hay dos datos geograficos siempre: 
# #   * `lat`/`lon` (para los tweets geolocalizados) 
# #   * `place_lat` and `place_lon` (para los tweets con informacion de lugar). 
# # Trabajaremos con cualquiera de los dos, con el que estÃ© disponible.
# tweets$lat <- ifelse(is.na(tweets$lat), tweets$place_lat, tweets$lat)
# tweets$lon <- ifelse(is.na(tweets$lon), tweets$place_lon, tweets$lon)
#  FIN METODO 1 ####################################################################################

# 3.2. Hashtags mas populares----
# Vamos a utilizar expresiones regulares para la limpieza de los textos
ht <- str_extract_all(tweets$text, "#(\\d|\\w)+")
ht <- unlist(ht)
head(sort(table(ht), decreasing = TRUE))
sort(table(ht), decreasing = TRUE)
htdf <- as.data.frame(sort(table(ht), decreasing = TRUE))

# 3.3. Visualizacion de las palabras----
# Creamos una nube de palabras (wordcloud)
twcorpus <- corpus(as.character(tweets$text))
twdfm <- dfm(twcorpus, remove_punct = TRUE, remove_number=TRUE, remove=c(
  stopwords("spanish"), stopwords("english"),"t.co", "#", "@","https", "rt", "q", 
"http", "k", "can", "d", "x", "si"), verbose=TRUE)
textplot_wordcloud(twdfm, rot.per=0, scale=c(3.5, .75), max.words=150)

# 4. Exportacion de datos----
write.csv(tweets, file="covid19_22sep2020.csv", row.names=FALSE)

###############################################################################################
################### CORREGIR DESDE AQUI #########################################################
###############################################################################################
# Probando diferentes layouts (circle, fruchterman-reingold, kamada-kawai, grid, lgl, tree)
plot(g, layout = layout.circle(g))

# No vemos nada, es una red compleja; necesitamos otro tipo de visualizaciones y necesitamos
# recurrir a las matematicas

# 2.3. Parametros del grafo----
is.directed(g)
is.weighted(g)

# 3. Parametrizacion----
# 3.1. Parametros de los nodos (vertices)----
# Existe una linea (al menos un retweet) entre vmm7773 y cnt1910
# muestra si hay relación entre "A" y "B"
g["vmm7773", "cnt1910"]
g["ciudadanatw", "vmm7773"]
g["vmm7773", "ciudadanatw"]

# Mostrar todas las lineas (todos ls retweets) de o hacia vmm77:
incident(g, "vmm7773", mode = c("all"))

# Identificando vecinos
neighbors(g, "vmm7773", mode = c("all"))

# Identificando vecinos en comun para 2 nodos
x <- neighbors(g, "CNT_Andalucia", mode = c("all"))
y <- neighbors(g, "cnt1910", mode = c("all"))
intersection(x,y)

# 3.1.1. Caminos (paths)----
# Vertices mas alejados
farthest_vertices(g) # distancia = 5

# Diametro de la red
get_diameter(g) #6

# Identificando vertices alcanzables con x pasos desde un vertice determinado
ego(g, 2, "vmm7773", mode = c("all"))

# 3.1.2. Identificando vertices influyentes o importantes----
# 3.1.2.1 Grado (numero de conexiones)----
dout <- degree(g, mode = "out")
vertices.df$dout <- dout
din <- degree(g, mode = "in")
vertices.df$din <- din
vertices.df$dtot <- vertices.df$din + vertices.df$dout

# 3.1.2.2 Betweenness----
# Con que frecuencia un vertice se encuentra en el camino mas corto entre 2 vertices
# cualesquiera de la red
# Cuan critico es cada vertice en el flujo de informacion a traves de la red
# Nodos con un betweenness alto son puentes clave entre diferentes partes de la red.
# Nodos con un betweenness bajo no son relevantes para la conectividad global de la red.

btw <- betweenness(g, directed = TRUE)
vertices.df$btw <- btw
btwN <- betweenness(g, directed = TRUE, normalized = TRUE)
vertices.df$btwN <- btwN

# 3.1.2.3 Eigenvector Centrality----
# Cuan bien conectado esta un vertice
# Nodos con un eigenvector cengtrality alto son nodos conectados con muchos nodos pero,
# esoecialmente, con  otros nodos a su vez muy conectados con otros.
# Esto es, estan cnectados con nodos relevantes. Google...
eigen <- eigen_centrality(g)$vector
vertices.df$eigen <- eigen

# 3.2. Estcructura global de la red----
# 3.2.1. Densidad----
# Density
# Proporcion de edges que existen en la red sobre el total de edges potenciales que
# existirian si todos los vertices estuvieran conectados unos con otros.
# Para un directed, vertices/((nodos*nodos)-nodos)
# Nos da una idea de cuan interconectada esta una red
edge_density(g) #0.0003227227 <- 0,032% red poco conectada

# 3.2.2. Longitud media de los caminos----
# Average path length
# La media de las longitudes de los caminos mas cortos (shortest paths) entre todos los
# pares de nodos de la red
# Cuanto menor sea la distancia, mas interconectada esta la red y el flujo entre vertices
# es mas facil
mean_distance(g, directed = TRUE) # 1.629542

# 3.2.3. Transitividad----
# Transitivity
# Mide la probabilidad de que los vertices adyacentes a un vertice dado esten conectados
# Cuan probable es que las personas a las que yo retwiteo, se retwiteen entre si
transitivity(g) #0.001870885
transitivity(g, type="global")
transitivity(as.undirected(g, mode="collapse"))

# Tambien podemos calcular la transitividad local de un vertice (nodo)
transitivity(g,
             vids = "CroNocturnas",
             type = "local")

# A nivel de nodo
transitivity(g, type="local")
# De otra forma
for (i in 1:nrow(vertices.df)) {
  vertices.df$trans[i] <- transitivity(g,
                                       vids = vertices.df$name[i],
                                       type = "local")
}

# 3.2.4. Cliques----
# Red o subred donde todos los vertices estan conectados entre si.
# Todos los triangulos estan cerrados
largest_cliques(g) # 5 de 4 nodos (vertices)
max_cliques(g) # cliques con un numero de nodos inferior al maximo

# 3.2.5. Assortativity----
# Cuan probable es que dos vertices que tengan un atributo en comun se relacionen entre si.
# Si el atributo es categorico, deberemos convertirlo en numerico primero.
# Se asocian los vertices de manera aleatoria o se asocian con vertices similares
# Si supieramos, por ejemplo, la edad, la afiliacion politica, su peso, sus ingresos, etc.
assortativity(g, values) # en este caso no tenemos un atributo para poder ejecutarlo

# 3.2.6. Assortativity degree----
# Mide si los vertices con un grado alto se conectan preferentemente con otros vertices con
# grado alto
assortativity.degree(g, directed = TRUE) # -0.1033601 los nodos altamente conectados no 
# se conectan preferiblemente con nodos altamente conectados

# 3.2.7. Reciprocity----
# Otra forma de medir la cercania de las relaciones en una red directed
# La proporcion de edges que son simetricos: la proporcion de edges de salida que tambien
# tienen un edge de entrada
reciprocity(g) #0.006679655

# 3.2.8. Community detection----
# Grupos dentro de los cuales la conectividad entre sus nodos es mayor que la conectividad con
# elresto de nodos de la red
# Reduzco mi red
rts2 <- rts %>%
  dplyr::filter(retweetCount >= 2000)

vertices.2 <- rts2 %>%
  dplyr::group_by(node2, text) %>%
  dplyr::summarise(n = n(), rtwCount = sum(retweetCount)) %>%
  dplyr::group_by(node2) %>%
  dplyr::summarise(tweets = sum(n), rtwCount = sum(rtwCount)) %>%
  dplyr::ungroup()

colnames(vertices.2)[1] <- "name"

# Completo el dataframe con los usuarios que no han twiteado nada "original"
vertices.2.df1 <- as.data.frame(rts2$node1)
vertices.2.df1 <- unique(vertices.2.df1)
colnames(vertices.2.df1)[1] <- "name"

vertices.2.df2 <- as.data.frame(rts2$node2)
vertices.2.df2 <- unique(vertices.2.df2)
colnames(vertices.2.df2)[1] <- "name"

vertices.df.2 <- rbind(vertices.2.df1, vertices.2.df2)
vertices.df.2 <- unique(vertices.df.2)

vertices.df.2 <- vertices.df.2 %>%
  dplyr::left_join(vertices.2, by = c("name" = "name"))

# Convierto los NA en valores numericos (0)
for (i in 1:nrow(vertices.df.2)) {
  vertices.df.2$tweets[i] <- ifelse(is.na(vertices.df.2$tweets[i]) == TRUE, 0, vertices.df.2$tweets[i])
  vertices.df.2$rtwCount[i] <- ifelse(is.na(vertices.df.2$rtwCount[i]) == TRUE, 0, vertices.df.2$rtwCount[i])
}

edges.df.2 <- rts2 %>%
  dplyr::group_by(node1, node2) %>%
  dplyr::summarise(edgeW = n()) %>%
  dplyr::ungroup()

colnames(edges.df.2)[1] <- "from"
colnames(edges.df.2)[2] <- "to"

g2 <- graph_from_data_frame(d = edges.df.2, vertices = vertices.df.2, directed = TRUE)

# Grafo covid19
rtsCov19 <- twcovid19[grep("RT @", twcovid19$text),]
rtsCov19$node1 <- rtsCov19$screenName
partido2 <- reshape2::colsplit(rtsCov19$text, ":", names = c("node2", "resto"))
rtsCov19$node2 <- partido2$node2
rtsCov19$node2 <- stringr::str_replace_all(rtsCov19$node2, "RT @", "")
rtsCov19 <- rtsCov19 %>%
  dplyr::filter(retweetCount >= 500)

vertices.3 <- rtsCov19 %>%
  dplyr::group_by(node2, text) %>%
  dplyr::summarise(n = n(), rtwCount = sum(retweetCount)) %>%
  dplyr::group_by(node2) %>%
  dplyr::summarise(tweets = sum(n), rtwCount = sum(rtwCount)) %>%
  dplyr::ungroup()

colnames(vertices.3)[1] <- "name"

# Completo el dataframe con los usuarios que no han twiteado nada "original"
vertices.3.df1 <- as.data.frame(rtsCov19$node1)
vertices.3.df1 <- unique(vertices.3.df1)
colnames(vertices.3.df1)[1] <- "name"

vertices.3.df2 <- as.data.frame(rtsCov19$node2)
vertices.3.df2 <- unique(vertices.3.df2)
colnames(vertices.3.df2)[1] <- "name"

vertices.df.3 <- rbind(vertices.3.df1, vertices.3.df2)
vertices.df.3 <- unique(vertices.df.3)

vertices.df.3 <- vertices.df.3 %>%
  dplyr::left_join(vertices.3, by = c("name" = "name"))

# Convireto los NA en valores numericos (0)
for (i in 1:nrow(vertices.df.3)) {
  vertices.df.3$tweets[i] <- ifelse(is.na(vertices.df.3$tweets[i]) == TRUE, 0,
                                    vertices.df.3$tweets[i])
  vertices.df.3$rtwCount[i] <- ifelse(is.na(vertices.df.3$rtwCount[i]) == TRUE, 0,
                                      vertices.df.3$rtwCount[i])
}

# Aniadimos un atributo
vertices.df.3$ntweets <- ifelse(vertices.df.3$tweets <=5, "bajo", 
                                ifelse(vertices.df.3$tweets <=10, "medio", "alto"))
edges.df.3 <- rtsCov19 %>%
  dplyr::group_by(node1, node2) %>%
  dplyr::summarise(edgeW = n()) %>%
  dplyr::ungroup()

colnames(edges.df.3)[1] <- "from"
colnames(edges.df.3)[2] <- "to"

g3 <- graph_from_data_frame(d = edges.df.3, vertices = vertices.df.3, directed = TRUE)

# 3.2.8.1. Fast-Greedy----
# Intenta crear comunidades cada vez mas grandes. 
fastgreedy.community(g) # no sirve para directed networks

# 3.2.8.2. Edge-Betweenness
x <- edge.betweenness.community(as.undirected(g)) #301 grupos
x
x2 <- edge.betweenness.community(as.undirected(g2))
x2
x3 <- edge.betweenness.community(as.undirected(g3)) #32 grupos
x3

# Para extraer los parametros de los grupos
length(x)
sizes(x)
membership(x)[5]

# Ploteamos los diferentes grafos
plot(x, g, edge.arrow.size=.1, vertex.size=rts$retweetCount * 0.0003,
     vertex.label.color="black",vertex.label.cex=0.5, vertex.label.dist=0, arrow.color="black",
     vertex.shape="circle",vertex.label.family="Helvetica",vertex.label.font=1,edge.color="black",
     main="NadaNosPara8M")

plot(x2, g2, edge.arrow.size=.1, vertex.size=rts2$retweetCount * 0.0003,
     vertex.label.color="black",vertex.label.cex=0.5, vertex.label.dist=0, arrow.color="black",
     vertex.shape="circle",vertex.label.family="Helvetica",vertex.label.font=1,edge.color="black",
     main="NadaNosPara8M")

plot(x3, g3, edge.arrow.size=.1, vertex.size=rtsCov19$retweetCount * 0.0002,
     vertex.label.color="black",vertex.label.cex=0.5, vertex.label.dist=0, arrow.color="black",
     vertex.shape="circle",vertex.label.family="Helvetica",vertex.label.font=1,edge.color="black",
     main="covid19 24/sep/2020")

# 3.2.8.2. Intermediacion----
# Convierto mi red directed en undirected
net.sym <- as.undirected(g3, mode= "collapse", edge.attr.comb=list(weight="sum", "ignore"))

ceb <- cluster_edge_betweenness(net.sym)
dendPlot(ceb, mode="hclust")
class(ceb)
length(ceb)
membership(ceb)
modularity(ceb)
crossing(ceb, g3)

cfg <- cluster_fast_greedy(as.undirected(g3))
plot(cfg, as.undirected(g3))

# Ahora que hemos creado el grafo g3, el de la covid, vamos a visualizar hubs y autoridades
# 3.1.2.4. Hubs y autoridades----
hs <- hub_score(g3, weights=NA)$vector
as <- authority_score(g3, weights=NA)$vector
par(mfrow=c(1,2))
plot(g3, vertex.size=hs*50, main="Hubs")
plot(g3, vertex.size=as*30, main="Authorities")

# 4. Otras visualizaciones----
# 4.1. threejs----
gjs1 <- graphjs(g, vertex.size = 0.15, main = "NadaNosPara8M")
# gjs1

# Aniadimos atributos
g <- set_vertex_attr(g, "label", value = V(g)$name)
ec <- as.numeric(eigen_centrality(g)$vector)
v <- ec * 2

gjs2 <- graphjs(g, vertex.size = v, main = "NadaNosPara8M")
gjs2

Degree_centrality
gjs_degree <- graphjs(g, vertex.size = vertices.df$dtot * 0.01, main = "NadaNosPara8M-degree")
gjs_degree

# PageRank_centrality
gjs_pageR <- graphjs(g, vertex.size = vertices.df$eigen, main = "NadaNosPara8M-eigen")
gjs_pageR

# Betweenness_centrality
gjs_btw <- graphjs(g, vertex.size = vertices.df$btw * 0.005, main = "NadaNosPara8M-betweenness")
gjs_btw

# Covid
gjs_covid19 <- graphjs(g3, vertex.size = 1,
                       vertex.color = c("red", "orange", "green")[as.factor(vertices.df.3$ntweets)],
                       main = "covid19 24/sep/2020")

# Aniado etiquetas a los nodos
g3 <- set_vertex_attr(g3, "label", value = V(g3)$name)

gjs_covid19 <- graphjs(g3, vertex.size = 1,
                       vertex.color = c("red", "orange", "green")[as.factor(vertices.df.3$ntweets)],
                       main = "covid19 24/sep/2020")

# Pintamos los nodos del diametro en otro color
vcol <- rep("gray40", vcount(g3))
diam <- get_diameter(g3, directed=T)
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(g))
ecol[E(g3, path=diam)] <- "orange"

plot(g3, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0) # nos e ve mucho

gjs_covid19_2 <- graphjs(g3, vertex.size = 1, vertex.color = vcol, edge.color=ecol,
                         edge.arrow.mode=0, main = "covid19 24/sep/2020")

# Salvamos los grafos en html
saveWidget(gjs1, file="NadaNosPara8M.html")
saveWidget(gjs2, file="NadaNosPara8M_eigen.html")
saveWidget(gjs_degree, file="NadaNosPara8M_degree.html")
saveWidget(gjs_pageR, file="NadaNosPara8M_pagaRank.html")
saveWidget(gjs_btw, file="NadaNosPara8M_betweenness.html")
saveWidget(gjs_covid19, file="covid19_24sep2020.html")
saveWidget(gjs_covid19_2, file="covid19_24sep2020_2.html")

# 5. Nodos influyentes----
# Â¿Nos quedamos con los top10 segun estos 3 criterios?
influencers_degree <- vertices.df %>%
  dplyr::arrange(desc(dtot)) %>%
  dplyr::mutate(topDegree = seq(1,nrow(vertices.df),1))
influencers_degree[1:10,]

influencers_pageR <- vertices.df %>%
  dplyr::arrange(desc(eigen)) %>%
  dplyr::mutate(topPageR = seq(1,nrow(vertices.df),1))
influencers_pageR[1:10,]

influencers_btw <- vertices.df %>%
  dplyr::arrange(desc(btw)) %>%
  dplyr::mutate(topBtw = seq(1,nrow(vertices.df),1))
influencers_btw[1:10,]

# 6. Bots----
# La definicion de bots genera cierta controversia y no hay un acuerdo claro en el mundo
# de los medios sociales.
# En general, podemos asumir que se trata de cuentas que generan contenido de forma 
# muy automatizada.
# Â¿Podemos detectarlos?
# Vamos a centrarnos en cuentas que no tuitean nada original y que retwitean mucho
vertices.df %>% 
  dplyr::filter(tweets == 0) %>% 
  dplyr::arrange(desc(dout))

# Vamos a amalizar a VientosOrihuela y a Algonfdez con Botometer
# (https://botometer.osome.iu.edu/)

# 6. Random graphs----
# Para poner en contexto los valores obtenidos para mi red
# Generamos algoritmos con caracteristicas similares a las de mi red, de manera aleatoria:
# Mismo numero de vertices y densidad aproximadamente igual
erdos.renyi.game(n = gorder(g), p.or.m = edge_density(g), type = "gnp")

# Generate 1000 random graphs based on the original network - e.g. with the same number of 
# vertices and approximate density.
# Calculate the average path length of the original network.
# Calculate the average path length of the 1000 random networks.
# Determine how many random networks have an average path length greater or less than the
# original network's average path length.
gl <- vector('list',1000)

for(i in 1:1000){
  
  gl[[i]] <- erdos.renyi.game(
    n = gorder(g), 
    p.or.m = edge_density(g), 
    type = "gnp"
  ) 
}

gl.apls <- unlist( lapply(gl, mean_distance, directed = FALSE))

hist(gl.apls, breaks = 20, main = "NadaNosPara8M - dist.media")
abline(v = mean_distance(g, directed=FALSE), col = "red", lty = 3, lwd=2)
# La distancia media de mi red es sustancialmente mas baja que la que cabria esperar en una 
# red de sus caracteristicas
gd.apls <- unlist( lapply(gl, edge_density))
hist(gd.apls, breaks = 20, main = "NadaNosPara8M - densidad")
abline(v = edge_density(g), col = "red", lty = 3, lwd=2)
