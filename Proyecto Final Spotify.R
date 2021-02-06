{ # Librerías

# install.packages('devtools') # Para descargar spotifyr
library(devtools) # Instalación de spotifyr
# devtools::install_github('charlie86/spotifyr')

library(spotifyr)
library(tidyverse)
library(knitr)

# Librerias de graficación
# install.packages('ggridges')
# install.packages('ggjoy')
library(ggjoy)
library(ggridges)
library(ggplot2)
}

#Ingresamos las credenciales necesarias para acceder a la API
Sys.setenv(SPOTIFY_CLIENT_ID = '72654f91e8424dc8b4924dc4cdf3674a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '968caf037e674bb0a89533fde2831f25')

access_token <- get_spotify_access_token()

# Uso de herramientas de spotifyr
# Creación del conjunto de datos «beatles» accediendo a las características de audio
beatles <- get_artist_audio_features('the beatles')

# Conteo de notas con las que se compusieron sus canciones
# Nótese el uso de kable() como una herramienta de estilo para tablas
beatles %>%
  count(key_mode, sort = TRUE) %>%
  head(20) %>% kable()

# Obtención de la «valencia» de las canciones de Joy Division
# la cual es una medida de «positividad» propia de spotify
joy <- get_artist_audio_features('joy division')
joy %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) %>% 
  kable()

View(joy)

# Graficación la valencia de cada pista
ggplot(joy, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() 

#Obtenemos el análisis de audio de un track en específico
Tusa <- get_track_audio_analysis('7k4t7uLgtOxPwTpFmtJNTY')
View(Tusa)

#Obtener las caracterìsticas de las canciones top de 2020, 2019 y 2018 en México
Top2020M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXdJSo3JjzjFs')
View(Top2020M)
Top2019M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWWQFIXDQWjhE')
View(Top2019M)
Top2018M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXe0di5KlqRqB')
View(Top2018M)

nlistaM <- list(Top2020M, Top2019M, Top2018M)
lapply(nlistaM, str)
dataM <- do.call(rbind, nlistaM)
View(dataM)

{# Obtener las caracterìsticas de las canciones top de 2020 - 2000 en el mundo 
Top2020<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX7Jl5KP2eZaS')
View(Top2020)
Top2019<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWVRSukIED0e9')
View(Top2019)
Top2018<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXe2bobNYDtW8')
View(Top2018)
Top2017<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWTE7dVUebpUW')
View(Top2017)
Top2016<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX8XZ6AUo9R4R')
View(Top2016)
Top2015<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX9ukdrXQLJGZ')
View(Top2015)
Top2014<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0h0QnLkMBl4')
View(Top2014)
Top2013<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX3Sp0P28SIer')
View(Top2013)
Top2012<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0yEZaMOXna3')
View(Top2012)
Top2011<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXcagnSNtrGuJ')
View(Top2011)
Top2010<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXc6IFF23C9jj')
View(Top2010)
Top2009<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX4UkKv8ED8jp')
View(Top2009)
Top2008<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWYuGZUE4XQXm')
View(Top2008)
Top2007<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX3j9EYdzv2N9')
View(Top2007)
Top2006<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX1vSJnMeoy3V')
View(Top2006)
Top2005<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWWzQTBs5BHX9')
View(Top2005)
Top2004<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWTWdbR13PQYH')
View(Top2004)
Top2003<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXaW8fzPh9b08')
View(Top2003)
Top2002<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0P7PzzKwEKl')
View(Top2002)
Top2001<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX9Ol4tZWPH6V')
View(Top2001)
Top2000<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWUZv12GM5cFk')
View(Top2000)

nlista <- list(Top2020, Top2019, Top2018, Top2017, Top2016, Top2015, Top2014, Top2013, Top2012, Top2011, Top2010, Top2009, Top2008, Top2007, Top2006, Top2005, Top2004, Top2003, Top2002, Top2001, Top2000)
lapply(nlista, str)
data <- do.call(rbind, nlista)
View(data)
}

# Relació entre la bailabilidad y la popularidad de la canción
ggplot(data, aes(x=danceability, y=track.popularity, color=playlist_name)) + geom_point()

#Observamos el promedio de la pupularidad en cada año 
ggplot(data) + 
  geom_bar(aes(x=playlist_name, y=(track.popularity)), stat = "summary", fun="mean")

#Creamos un modelo de regresión lineal múltiple describiendo la popularidad de estas canciones explicadas por sus características musicales
attach(data)
Modelo1<- lm(track.popularity ~ danceability + energy + loudness + speechiness + acousticness + instrumentalness + liveness + valence + tempo + track.explicit)
summary(Modelo1)
plot(track.popularity, danceability, xlab = "Popularidad", ylab= "Bailaviliad")
res <- cor(track.popularity,danceability,energy)