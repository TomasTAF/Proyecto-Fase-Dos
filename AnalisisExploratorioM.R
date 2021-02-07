#Librerias

library(devtools)
#Ahora instalamos spotifyr
devtools::install_github('charlie86/spotifyr')

#Cargamos otras paqueterías necesarias
library(spotifyr)
library(tidyverse)
library(knitr)
require(devtools)
require(knitr)
require(spotifyr)
require(ggplot2)
require(devtools)
require(ggridges)
#Ingresamos las credenciales necesarias para acceder a la API
Sys.setenv(SPOTIFY_CLIENT_ID = '61b78d5fcb4a4c5cabb57f74e6da8654')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'c3b045f9dfc94f02879fe61511f2d85f')

access_token <- get_spotify_access_token()

#Comenzamos a utilizar las herramientas de la paquetería spotifyr
#Creamos un dataset llamado beatles accediendo a las características de audio de esta banda
beatles <- get_artist_audio_features('the beatles')
#Observamos el conteo de notas en las que fueron compuestas las canciones de los beatles
#Importante resaltar el uso de kable() como una herramienta de estilo para tablas
beatles %>%
  count(key_mode, sort = TRUE) %>%
  head(20) %>% kable()


#Obtenemos el análisis de audio de un track en específico
Tusa <- get_track_audio_analysis('7k4t7uLgtOxPwTpFmtJNTY')
View(Tusa)

##Data Cleaning y Set up

#Obtener las caracterìsticas de las canciones top de 2020 - 2000 en el mundo 
Top2020<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX7Jl5KP2eZaS')
Top2019<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWVRSukIED0e9')
Top2018<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXe2bobNYDtW8')
Top2017<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWTE7dVUebpUW')
Top2016<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX8XZ6AUo9R4R')
Top2015<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX9ukdrXQLJGZ')
Top2014<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0h0QnLkMBl4')
Top2013<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX3Sp0P28SIer')
Top2012<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0yEZaMOXna3')
Top2011<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXcagnSNtrGuJ')
Top2010<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXc6IFF23C9jj')
Top2009<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX4UkKv8ED8jp')
Top2008<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWYuGZUE4XQXm')
Top2007<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX3j9EYdzv2N9')
Top2006<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX1vSJnMeoy3V')
Top2005<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWWzQTBs5BHX9')
Top2004<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWTWdbR13PQYH')
Top2003<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXaW8fzPh9b08')
Top2002<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0P7PzzKwEKl')
Top2001<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX9Ol4tZWPH6V')
Top2000<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWUZv12GM5cFk')

listaMun <- list(Top2020, Top2019, Top2018, Top2017, Top2016, Top2015, Top2014, Top2013, Top2012, Top2011, Top2010, Top2009, Top2008, Top2007, Top2006, Top2005, Top2004, Top2003, Top2002, Top2001, Top2000)
dataMundo <- do.call(rbind, listaMun)
head(dataMundo)
names(dataMundo)

#Vemos que hay muchas columnas innecesarias para el análisis, por lo tanto reducimos nuestro
# dataset, seleccionando aquellas que parecen ser importantes.

variables.vector <-c("danceability" ,"energy" ,"loudness"  , "mode", "speechiness" ,"instrumentalness","valence",   "tempo",
                        "track.popularity","track.name", "track.album.release_date",  "key_name", "mode_name", "key_mode", "track.explicit" )

dataMundoSubset <- dataMundo %>%
  select(variables.vector)

#Formato de fecha
## Separamos por mes y por año, para tener mayor poder de análisis
dataMundoSubset <- dataMundoSubset %>% mutate(track.album.release_date = as.Date(dataMundoSubset$track.album.release_date, '%Y-%m-%d'))%>% mutate(release.month =  format(track.album.release_date, '%m'), release.year =  format(track.album.release_date, '%Y'))

summary(dataMundoSubset)


#Obtener las caracterìsticas de las canciones top de 2020, 2019 y 2018 en México
Top2020M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXdJSo3JjzjFs')
Top2019M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWWQFIXDQWjhE')
Top2018M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXe0di5KlqRqB')

listaMex <- list(Top2020M, Top2019M, Top2018M)
dataMexico <- do.call(rbind, listaMex)
head(dataMexico)
#Nuevamente, seleccionamos solo las columnas que nos importan

dataMexicoSubset <- dataMexico %>%
  select(variables.vector)

#Agregamos la fecha en formato adecuado
dataMexicoSubset <- dataMexicoSubset %>% mutate(track.album.release_date = as.Date(dataMexicoSubset$track.album.release_date, '%Y-%m-%d'))%>% mutate(release.month =  format(track.album.release_date, '%m'), release.year =  format(track.album.release_date, '%Y'))

#Finalmente, vemos el summary para México
summary(dataMexicoSubset)

#Analisis exploratorio

#Obtengamos el data numerico de cada dataframe
dataMexicoNumerica <- dataMexicoSubset %>% select(-c("track.name", "month.release", "year.release", "track.album.release_date"))
dataMundoNumerica <- dataMundoSubset %>% select(-c("track.name", "month.release", "year.release", "track.album.release_date"))


#Correlaciones
library(corrplot)
## Mexico
correlationsMexico <- cor(dataMexicoNumerica)
corrplot.mixed(correlationsMexico, lower.col = "black", number.cex = .7)
## Mundo
correlationsMundo <- cor(dataMundoNumerica)
corrplot.mixed(correlationsMundo, lower.col = "black", number.cex = .7)

boxplot(dataMexicoNumerica)

###Preguntas
library(ggthemes)

#¿Podemos notar la depresion estacional en la valencia de las canciones lanzadas?
ggplot(dataMexicoSubset, aes(x =valence , y = release.month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Valencia", option = "C") +
  labs(
    title = 'Valencia atraves de los meses',
    subtitle = '¿Podemos notar la depresión estacional?'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank())
## En el mundo
ggplot(dataMexicoSubset, aes(x =valence , y = release.month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Valencia", option = "C") +
  labs(
    title = 'Valencia atraves de los meses',
    subtitle = '¿Podemos notar la depresión estacional?'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank())


#¿Somos más felices? Cambio de la valencia a traves de los años

ggplot(dataMundoSubset, aes(x =valence , y = release.year, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Valencia", option = "D") +
  labs(
    title = 'Valencia atraves de los años',
    subtitle = '¿Somos más felices?'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank())

##Relación de la danzabilidad y la popularidad a lo largo de los años
ggplot(dataMundoSubset, aes(x= danceability, track.popularity)) +
  geom_point(aes(color = track.explicit))+
  labs(x ="Danzabilidad", y= "Popularidad",
    title = 'Danzabilidad vs popularidad a lo largo de los años',
    subtitle = '¿Que tan importante es que se baile para que pegue?'
  ) + facet_wrap(~release.year) + 
  scale_color_brewer(type = "qual", palette = "Set2") +
  geom_smooth() +
  theme_hc(style = 'darkunica') 

ggplot(dataMundoSubset, aes(x= speechiness, track.popularity)) +
  geom_point(aes(color = track.explicit))+
  labs(x ="Speechiness", y= "Popularidad",
       title = 'Speechiness vs popularidad a lo largo de los años',
       subtitle = '¿Que tan importante son las palabras en una canción?'
  ) + facet_wrap(~release.year) + 
  scale_color_brewer(type = "qual", palette = "Set2") +
  geom_smooth() +
  theme_hc(style = 'darkunica') 

#Pendientes
#Checar NA's

#Preguntas
#Como ha sido el cambio de valencia atraves de los años
#Depresion estacional se puede ver en la reproduccion musical
#Como ha cambiado la duracion de canciones a lo largo del tiempo

#Graficos
# ggridges con años y danzabilidad
# boxplots
# algo chido con ggplot
# comentario 
