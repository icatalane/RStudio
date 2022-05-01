# author: Inigo Catalan
# date: Oct, 2020

# Limpiamos el workspace
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# # LibrerÃ­as y paquetes
# install.packages("kableExtra")
# install.packages("textdata")

# # Vamos a cargar las librerÃ­as necesarias
library(rwhatsapp)   
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP
library(ggimage)# EMOJI RANKING
library(tidytext)
library(stopwords)
library(rvest)
library(textdata)


miChat <- rwa_read("Data/chat.txt") # PREPARACIÓN DE DATOS PARA ANÁLISIS POR DATE/TIME
Chat_completo <- miChat ## Copia para filtros

miChat <-Chat_completo

# SEGMENTACIÓN POR MES
miChat <- miChat %>%  
  mutate(day = date(time)) %>%                   # crea la columna dIa
  filter(day >= "2015-04-15" & day <= "2020-10-05")

miChat$day<-as.Date(as.character(miChat$day))
miChat$estacion<- ifelse(miChat$day > dmy(21032015) & miChat$day < dmy(21062015),"Primavera 2015",
                    ifelse(miChat$day > dmy(22062015) & miChat$day < dmy(22092015),"Verano 2015",
                    ifelse(miChat$day > dmy(23092015) & miChat$day < dmy(20122015),"Otoño 2015",
                    ifelse(miChat$day > dmy(20122015) & miChat$day < dmy(20032016),"Invierno 2015",
                    ifelse(miChat$day > dmy(22062016) & miChat$day < dmy(23092016),"Primavera 2016",
                    ifelse(miChat$day > dmy(22062016) & miChat$day < dmy(22092016),"Verano 2016",
                    ifelse(miChat$day > dmy(23092016) & miChat$day < dmy(20122016),"Otoño 2016",
                    ifelse(miChat$day > dmy(20122016) & miChat$day < dmy(20032017),"Invierno 2016",
                    ifelse(miChat$day > dmy(22062017) & miChat$day < dmy(23092017),"Primavera 2017",
                    ifelse(miChat$day > dmy(22062017) & miChat$day < dmy(22092017),"Verano 2017",
                    ifelse(miChat$day > dmy(23092017) & miChat$day < dmy(20122017),"Otoño 2017",
                    ifelse(miChat$day > dmy(20122017) & miChat$day < dmy(20032018),"Invierno 2017",
                    ifelse(miChat$day > dmy(22062018) & miChat$day < dmy(23092018),"Primavera 2018",
                    ifelse(miChat$day > dmy(22062018) & miChat$day < dmy(22092018),"Verano 2018",
                    ifelse(miChat$day > dmy(23092018) & miChat$day < dmy(20122018),"Otoño 2018",
                    ifelse(miChat$day > dmy(20122018) & miChat$day < dmy(20032019),"Invierno 2018",
                    ifelse(miChat$day > dmy(22062019) & miChat$day < dmy(23092019),"Primavera 2019",
                    ifelse(miChat$day > dmy(22062019) & miChat$day < dmy(22092019),"Verano 2019",
                    ifelse(miChat$day > dmy(23092019) & miChat$day < dmy(20122019),"Otoño 2019",
                    ifelse(miChat$day > dmy(20122019) & miChat$day < dmy(20032020),"Invierno 2019",
                    ifelse(miChat$day > dmy(22062020) & miChat$day < dmy(23092020),"Primavera 2020",
                    ifelse(miChat$day > dmy(22062020) & miChat$day < dmy(22092020),"Verano 2020",
                    ifelse(miChat$day > dmy(23092020) & miChat$day < dmy(20122020),"Otoño 2020",
                    ifelse(miChat$day > dmy(20122020) & miChat$day < dmy(20032021), "Invierno 2020","0"
                ))))))))))))))))))))))))
  
#############################################################
# NECESARIO REVISAR FECHAS CON VALOR 0!!!
################################################################

  miChat$estacion<-as.factor(miChat$estacion)
  miChat <- na.omit(miChat)
  miChat <- subset(miChat, estacion!=0)
  miChat<- subset(miChat, author!= "Rumberos")
   
  


# COLOR PALETTE - no necesario. Añadir "scale_fill_manual(values=paleta.estaciones) +" antes de ylab para añadir 9 colores
# paleta.estaciones <- brewer.pal(9,"Set1")[c(7,5,1,3,4,2,6,8)]

# VERIFYING HOW MANY MESSAGES WERE SENT DURING THE PERIOD OF TIME
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  
  ylab("Número de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")
ggsave("Mensajes_por_día_estacion.png")

# Vision en un calendario
# install.packages("openair")
# library(openair)
# calendarPlot(miChat, pollutant = n, year = 2020, annotate = "date", cols= "heat")


# Comparación según las estaciones del año
# solo sale verano 2015 porque no coge el ifelse no coge bien las fechas 
miChat %>% 
  group_by(day) %>% 
  count(estacion) %>%
  ggplot(aes(x = estacion, y = n, fill=day)) +
  geom_bar(stat = "identity") +
  
  ylab("Número de mensajes") + xlab("Estación") +
  ggtitle("Mensajes por estación", "Frecuencia por estación del año") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
         legend.position = "right")+
  coord_flip()




# MESSAGES PER DAY OF THE WEEK
miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(estacion, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Número de mensajes por día de la semana", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")
ggsave("Mensajes_por_día_semana.png")

# KEEP THE WEEKEND OF THE WEEK AND RENAME THEM
diasemana <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado","domingo")
names(diasemana) <- 1:7# MENSAJES POR HORA DEL DÍA
miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
 
  ylab("Número de mensajes") + xlab("Horario") +
  ggtitle("Número de mensajes por hora del día", "Frecuencia según estación del año") +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom",
         panel.spacing.x=unit(0.0, "lines"))
ggsave("Mensajes_por_día_semana2.png")

# CHANGE THE NAME OF USERS FOR CONFIDENTIALITY
# levels(miChat$author)[1] <- "Carmen"
# levels(miChat$author)[2] <- "Inigo"
# levels(miChat$author)[3] <- "Laura"
# levels(miChat$author)[4] <- "Marci"

# MESSAGES PER USER
miChat %>%
  mutate(day = date(time)) %>%
  group_by(estacion) %>% 
  count(author) %>% 
  ggplot(aes(x = reorder(author, n), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +

  ylab("Número total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("Número total de mensajes por usuario.", "¿Quién es más comunicativo? Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")
ggsave("Mensajes_por_autor.png")

# LIBRARY FOR EMOJI PNG IMAGE FETCH FROM https://abs.twimg.com
plotEmojis <- miChat %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>% 
  count(emoji, emoji_name) %>% 

# PLOT TOP 30 EMOJIS
  top_n(30, n) %>% 
  arrange(desc(n)) %>% # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0( "https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)),".png")) 
  )


# PLOT OF THE RANKING OF MOST USED EMOJIS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.045) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ylab("Número de veces que el emoji fue usado") +
                                             xlab("Emoji y significado") +
                                             ggtitle("Emojis más utilizados de manera general", "Emojis más usados por todos") +
                                             coord_flip() +
                                             theme_minimal() +
                                             theme()
 
ggsave("Emojis.png")

# EMOJI RANK PER USER
library(ggplot2)
plotEmojis <- miChat %>%
unnest(c(emoji, emoji_name)) %>%
mutate( emoji = str_sub(emoji, end = 1)) %>% # 
count(author, emoji, emoji_name, sort = TRUE) %>%

# PLOT TOP 8 EMOJIS PER USER
group_by(author) %>%
top_n(n = 8, n) %>%
slice(1:8) %>% 
                                             
# CREATE AN IMAGE URL WITH THE EMOJI UNICODE
mutate( emoji_url = map_chr(emoji, 
   ~paste0("https://abs.twimg.com/emoji/v2/72x72/",as.hexmode(utf8ToInt(.x)),".png")) )

# PLOT DATA
plotEmojis %>% 
ggplot(aes(x = reorder(emoji, -n), y = n)) +
geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +

# USE TO FETCH AN EMOJI PNG IMAGE https://abs.twimg.com
  
geom_image(aes(image=emoji_url), size=.13) +
  ylab("Número de veces que se usó el emoji") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 5, scales = "free") +
  ggtitle("Emojis más usados en la conversación, por usuario") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave("Emojis_por_autor.png")

# REMOVE WORDS WITHOUT RELEVANT MEANING, SUCH AS PRONOUNS, ETC.
remover_palabras <- c(stopwords(language = "es"), "https" , "s", "status", "twitter.com"
                      "multimedia", "y", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa", "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una", "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas", "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú")

# WORD COUNT
miChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras) %>% 
  count(word) %>% 

# PLOT TOP 30 MOST USED WORDS IN CONVERSATION
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ggtitle("Palabras más usadas en la conversación de manera general") +
                                             xlab("Palabras") +
                                             ylab("Número de veces que se usó la palabra") +
                                             coord_flip() +
                                             theme_minimal()
ggsave("Palabras.png")

# WORD COUNT PER USER
miChat %>%
 unnest_tokens(input = text,
 output = word) %>%
 filter(!word %in% remover_palabras) %>%
 count(author, word, sort = TRUE) %>%

                                               
# TOP 20 MOST USED WORDS BY USER
group_by(author) %>%
 top_n(n = 20, n) %>%
 slice(1:20) %>%
 ungroup() %>% 
 arrange(author, desc(n)) %>% 
 mutate(order=row_number()) %>% 
 ggplot(aes(x = reorder(word, n), y = n, fill = author, color = author)) +
    geom_col(show.legend = FALSE, width = .1) +
    geom_point(show.legend = FALSE, size = 3) +
    xlab("Palabras") +
    ylab("Número de veces que se usó la palabra") +
    coord_flip() +
    facet_wrap(~author, ncol = 3, scales = "free") +
    ggtitle("Palabras más usadas por usuario en la conversación") +
                                             theme_minimal()
ggsave("Palabras_por_user.png")

# VOCABULARY DIVERSITY
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Diversidad léxica") +
  xlab("Usuario") +
  ggtitle("Diversidad de léxico en la conversación") +
  coord_flip()
ggsave("diversidad_por_user.png")

# # UNIQUE WORDS FROM HER
# palabras_unicas_ella <- miChat %>%
#   unnest_tokens(input = text,
#                 output = word) %>%
#   filter(author == "Carmen Acosta") %>%  
#   count(word, sort = TRUE)miChat %>%
#   unnest_tokens(input = text,
#                 output = word) %>%
#   filter(author == "Laura Acosta") %>% 
#   count(word, sort = TRUE) %>% 
#   filter(!word %in% palabras_unicas_ella$word) %>% 
#   
#   
#   # SELECT ONLY WORDS THAT NO ONE ELSE USES
#   top_n(n = 15, n) %>%
#   ggplot(aes(x = reorder(word, n), y = n)) +
#   geom_col(show.legend = FALSE) +
#   ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
#   coord_flip() +
#   ggtitle("Top de palabras únicas usadas por Carmen")
# 
# 
# palabras_unicas_ella <- miChat %>%
#   unnest_tokens(input = text,
#                 output = word) %>%
#   filter(author != "Ella") %>%  
#   count(word, sort = TRUE)miChat %>%
#   unnest_tokens(input = text,
#                 output = word) %>%
#   filter(author == "Carmen Acosta") %>% 
#   count(word, sort = TRUE) %>% 
#   filter(!word %in% palabras_unicas_ella$word) %>% 
#   # SELECT ONLY WORDS THAT NO ONE ELSE USES
#   top_n(n = 15, n) %>%
#   ggplot(aes(x = reorder(word, n), y = n)) +
#   geom_col(show.legend = FALSE) +
#   ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
#   coord_flip() +
#   ggtitle("Top de palabras únicas usadas por Ella")
# 
# # UNIQUE WORDS FROM HIM
# palabras_unicas_el <- miChat %>%
#   unnest_tokens(input = text,
#                 output = word) %>%
#   filter(author != "Él") %>%  
#   count(word, sort = TRUE)miChat %>%
#   unnest_tokens(input = text,
#                 output = word) %>%
#   filter(author == "Él") %>% 
#   count(word, sort = TRUE) %>% 
#   filter(!word %in% palabras_unicas_el$word) %>% 
#   # SELECT ONLY WORDS THAT NO ONE ELSE USES
#   top_n(n = 15, n) %>%
#   ggplot(aes(x = reorder(word, n), y = n)) +
#   geom_col(show.legend = FALSE) +
#   ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
#   coord_flip() +
#   ggtitle("Top de palabras únicas usadas por Él")

# ANALISIS DE SENTIMIENTOS
# WE USE THE RVEST PACKAGE

# HTML PAGE FETCH EMOJI SENTIMENT RANKING 1.0
url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
doc <- read_html(url_base)
# SEARCH EMOJI TABLE AND PROCESS
tabla_emojis <- doc %>% 
  html_node("#myTable") %>% 
  html_table() %>% 
  as_tibble()

# GET FEELING SCORE AND CLEAN UP
sentimiento_emoji <- tabla_emojis %>% 
  select(1,6:9) %>% 
  set_names("char", "negativo","neutral","positivo","sent.score")

# EXTRACT EMOJI AND UNITE WITH FEELING
emoji_chat <- miChat %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  inner_join(sentimiento_emoji, by=c("emoji"="char"))

# PREVIEW VIEW 
emoji_chat %>% 
  select(-source, -day, -estacion) %>% 
  slice(1207:1219) %>% 
  kable() %>% 
  kable_styling(font_size = 10)

# OCCURRENCES OF FEELINGS BY EMOJIS, BY USER
emoji_sentimiento_usuarios <- emoji_chat %>% 
  group_by(author) %>% 
  summarise(
    positivo=mean(positivo),
    negativo=mean(negativo),
    neutral=mean(neutral),
    balance=mean(sent.score)
  ) %>% 
  arrange(desc(balance))

# DATA FORMAT FOR PLOTING
emoji_sentimiento_usuarios %>% 
  mutate( negativo  = -negativo,
          neutral.positivo =  neutral/2,
          neutral.negativo = -neutral/2) %>% 
  select(-neutral) %>% 
  gather("sentiment","mean", -author, -balance) %>% 
  mutate(sentiment = factor(sentiment, levels = c("negativo", "neutral.negativo", "positivo", "neutral.positivo"), ordered = T)) %>% 
  ggplot(aes(x=reorder(author,balance), y=mean, fill=sentiment)) +
  geom_bar(position="stack", stat="identity", show.legend = F, width = .5) +
  scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
  ylab(" - Negativo / Neutral / Positivo +") + xlab("Usuario") +
  ggtitle("Análisis de sentimientos por usuario","Basado en el puntaje promedio de sentimientos por emojis") +
  coord_flip() +
  theme_minimal()
ggsave("sentimientos.png")


# GET POSITIVE / NEGATIVE FROM LEXICON PACKAGE
lexico_negpos  <- get_sentiments("afinn") # INTENSIDAD DE VALOR

# PREVIEW OF THE LEXICON FORMAT
lexico_negpos %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 11)

# PREVIEW WHAT ARE THE POSSIBLE VALUES
table(lexico_negpos$value) %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 11)


# EXTRACT EMOJIS
emoji_sentimiento_score <- miChat %>%
  select( emoji, emoji_name) %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  
  distinct() %>% 
  unnest_tokens(input=emoji_name, output=emoji_words) %>% 
  inner_join(lexico_negpos, by=c("emoji_words"="word"))

# CREATE TABLE OF 3 COLUMNS
bind_cols(
  slice(emoji_sentimiento_score, 01:10),
  slice(emoji_sentimiento_score, 11:20),
  slice(emoji_sentimiento_score, 21:30)
) %>% 
  kable() %>% 
  kable_styling(full_width = F, font_size = 11)


# EXTRACT EMOJIS
emoji_chat <- miChat %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ":.*"))

# TOKENIZE THE EMOJI NAME
emoji_chat <- emoji_chat %>% 
  select(author, emoji_name) %>% 
  unnest_tokens(input=emoji_name, output=emoji_words)

# JOIN WITH LEXICON
usuario_summary <- emoji_chat %>% 
  inner_join(lexico_negpos, by=c("emoji_words"="word")) %>% 
  count(author, value) %>% 
  group_by(author) %>% 
  mutate(mean=n/sum(n)) %>% 
  ungroup()

# COLORS AND GRAPH
reordenar_niveles <- c(-3,-2,-1,3,2,1)
colores <- c("#d7191c","#fdae61","#ffffbf","#1a9641","#a6d96a","#ffffbf")
mis_colores <- brewer.pal(5,"RdYlGn")[c(1,2,3,5,4,3)]

# PLOTING
usuario_summary %>% 
  mutate( mean = ifelse(value<0, -mean, mean)) %>% 
  group_by(author) %>% 
  mutate( balance = sum(mean)) %>% 
  ungroup() %>% 
  mutate( value = factor(value, levels = reordenar_niveles, ordered=T)) %>% 
  ggplot(aes(x=reorder(author,balance), y=mean, fill=value)) +
  geom_bar(stat="identity",position="stack", show.legend = F, width = .5) +
  scale_fill_manual(values = mis_colores) +
  xlab("Usuario") + ylab("Escala de netagivo a positivo") +
  coord_flip() +
  ggtitle("Análisis de sentimientos por uso de emojis", "Uso de package Lexicon") +
  theme_minimal()

ggsave("sentimientos_emojis.png")

# GET ANOTHER LEXICON WITH NAME OF FEELINGS
lexico_sentimientos <- get_sentiments("nrc") # EXTRAER EMOJIS
emoji_emocion <- miChat %>%
  select( emoji, emoji_name) %>% 
  unnest(c( emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>%  
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  
  unnest_tokens(input=emoji_name, output=emoji_words) %>% 
  inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>%# REMOVER CLASIFICACIÓN NEGATIVA/POSITIVA 
  filter(!sentiment %in% c("negative","positive")) %>% 
  
  # KEEP ONLY THE 4 MOST FREQUENT EMOJI FOR EACH FEELING
  count(emoji, emoji_words, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(4,n) %>% 
  slice(1:4) %>% 
  ungroup() %>% 
  select(-n)

# PUTTING TABLES TOGETHER
bind_cols(
  slice(emoji_emocion, 01:16),
  slice(emoji_emocion, 17:32)
) %>% 
  kable() %>% 
  kable_styling(full_width = F, font_size = 11)

# JOIN WITH EMOJIS
sentimiento_chat <- emoji_chat %>% 
  inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>%
  # REMOVE POSITIVE / NEGATIVE CLASSIFICATION 
  filter(!sentiment %in% c("negative","positive"))

# PLOT OF MOSTLY EXPRESSED EMOTIONS
sentimiento_chat %>% 
  count(sentiment) %>% 
  ggplot(aes(x=reorder(sentiment,n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  coord_flip() +
  ylab("Número de veces expresado") + xlab("Emoción") +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Emoción expresada con mayor frecuencia","Expresado por uso de emojis") +
  theme_minimal()
ggsave("emociones_frecuencia.png")


# EMOTIONS PLOT PER USER
sentimiento_chat %>% 
  count(author, sentiment) %>% 
  left_join(filter(lexico_sentimientos, sentiment %in% c("negative","positive")),by=c("sentiment"="word")) %>% 
  rename( sentimiento = sentiment.y) %>% 
  mutate( sentimiento = ifelse(is.na(sentimiento), "neutral", sentimiento)) %>% 
  mutate( sentimiento = factor(sentimiento, levels = c("negative", "neutral", "positive"), ordered=T) ) %>% 
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentimiento)) +
  geom_col() +
  scale_fill_manual(values = c("#d7191c","#fdae61", "#1a9641")) +
  ylab("Número de veces expresado") +
  xlab("Emoción") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free_x") +
  ggtitle("Emociones mayormente expresadas por usuario", "Expresado por uso de emojis") + 
  theme_minimal() + theme(legend.position = "bottom")
ggsave("emociones_user.png")

