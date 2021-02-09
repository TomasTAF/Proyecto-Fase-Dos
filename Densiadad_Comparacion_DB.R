#Proyecto BEDU 
#Acceso a la API de Spotify
#Primero instalamos devtools para poder descargar spotifyr
#install.packages('devtools')
library(devtools)
#Ahora instalamos spotifyr
#devtools::install_github('charlie86/spotifyr')
#Cargamos otras paqueterías necesarias
library(spotifyr)
library(tidyverse)
library(knitr)

#Para renombrar
#install.packages("gdata")
library(gdata)

#Ingresamos las credenciales necesarias para acceder a la API
Sys.setenv(SPOTIFY_CLIENT_ID = '72654f91e8424dc8b4924dc4cdf3674a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '968caf037e674bb0a89533fde2831f25')

access_token <- get_spotify_access_token()

#Comenzamos a utilizar las herramientas de la paquetería spotifyr
#Creamos un dataset llamado beatles accediendo a las características de audio de esta banda
beatles <- get_artist_audio_features('the beatles')
#Observamos el conteo de notas en las que fueron compuestas las canciones de los beatles
#Importante resaltar el uso de kable() como una herramienta de estilo para tablas
#view(beatles)
beatles %>%
  count(key_mode, sort = TRUE) %>%
  head(20) %>% kable()

#Ahora obtenemos la "valencia" de las canciones de la banda Joy Division, una medida de 
#positividad creada por Spotify
joy <- get_artist_audio_features('joy division')
joy %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) %>% 
  kable()
#View(joy)
#Graficamos la valencia de cada pista utilizando el paquete ggridges para instalar ggjoy
#install.packages('ggridges')
#install.packages('ggjoy')
library(ggjoy)
library(ggridges)
#Cargamos ggplot en caso de no estar activo en la sesión
library(ggplot2)
ggplot(joy, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() 

#Obtenemos el análisis de audio de un track en específico
Tusa <- get_track_audio_analysis('7k4t7uLgtOxPwTpFmtJNTY')
Tusa

#Obtener las caracterìsticas de las canciones top de 2020, 2019 y 2018 en México
Top2020M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXdJSo3JjzjFs')
#View(Top2020M)
Top2019M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWWQFIXDQWjhE')
#View(Top2019M)
Top2018M<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXe0di5KlqRqB')
#View(Top2018M)

nlistaM <- list(Top2020M, Top2019M, Top2018M)
lapply(nlistaM, str)
dataM <- do.call(rbind, nlistaM)
#View(dataM)

#Obtener las caracterìsticas de las canciones top de 2020 - 2000 en el mundo 
Top2020<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX7Jl5KP2eZaS')
#View(Top2020)
Top2019<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWVRSukIED0e9')
#View(Top2019)
Top2018<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXe2bobNYDtW8')
#View(Top2018)
Top2017<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWTE7dVUebpUW')
#View(Top2017)
Top2016<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX8XZ6AUo9R4R')
#View(Top2016)
Top2015<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX9ukdrXQLJGZ')
#View(Top2015)
Top2014<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0h0QnLkMBl4')
#View(Top2014)
Top2013<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX3Sp0P28SIer')
#View(Top2013)
Top2012<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0yEZaMOXna3')
#View(Top2012)
Top2011<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXcagnSNtrGuJ')
#View(Top2011)
Top2010<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXc6IFF23C9jj')
#View(Top2010)
Top2009<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX4UkKv8ED8jp')
#View(Top2009)
Top2008<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWYuGZUE4XQXm')
#View(Top2008)
Top2007<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX3j9EYdzv2N9')
#View(Top2007)
Top2006<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX1vSJnMeoy3V')
#View(Top2006)
Top2005<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWWzQTBs5BHX9')
#View(Top2005)
Top2004<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWTWdbR13PQYH')
#View(Top2004)
Top2003<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DXaW8fzPh9b08')
#View(Top2003)
Top2002<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX0P7PzzKwEKl')
#View(Top2002)
Top2001<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DX9Ol4tZWPH6V')
#View(Top2001)
Top2000<- get_playlist_audio_features(playlist_uris ='37i9dQZF1DWUZv12GM5cFk')
#View(Top2000)

nlista <- list(Top2020, Top2019, Top2018, Top2017, Top2016, Top2015, Top2014, Top2013, Top2012, Top2011, Top2010, Top2009, Top2008, Top2007, Top2006, Top2005, Top2004, Top2003, Top2002, Top2001, Top2000)
lapply(nlista, str)
data <- do.call(rbind, nlista)
View(data)
#Observamos la relaciòn entre la bailabilidad y la popularidad de la canción
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

view(data)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
promediopop<-mean(track.popularity)
promedioexp<-mean(track.explicit)
promedioexp
promediopop
#Bailavilidad realcionada con la energia
plot(energy, danceability, xlab = "Bailaviliad", ylab = "Energia")
#Promedio explicitas y popularidad
plot(promediopop, promedioexp, xlab = "Popularidad", ylab = "Explicitas")
#Fechas cancniones explicitas
plot(track.album.release_date,track.explicit)
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
data
dataD<- data %>% select(playlist_name,danceability,energy,key,loudness,mode,speechiness,acousticness,
               instrumentalness,liveness,valence,tempo,time_signature,track.available_markets,track.duration_ms,
               track.explicit,track.name,track.popularity,track.album.name,track.album.album_type,track.album.release_date)

numdataD<- dataD %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,
                            tempo,track.duration_ms,track.popularity,track.album.release_date)
numdataD
#Pares
de<- dataD(danceability,energy)



#Densidad
ggplot(dataD,aes(x=energy, fill=playlist_name))+geom_density(col=NA,alpha=0.35)
ggplot(dataD,aes(x=danceability, fill=playlist_name))+geom_density(col=NA,alpha=0.35)
ggplot(dataD,aes(x=loudness, fill=playlist_name))+geom_density(col=NA,alpha=0.35)
ggplot(dataD,aes(x=acousticness, fill=playlist_name))+geom_density(col=NA,alpha=0.35)
ggplot(dataD,aes(x=speechiness, fill=playlist_name))+geom_density(col=NA,alpha=0.35)
ggplot(dataD,aes(x=valence, fill=playlist_name))+geom_density(col=NA,alpha=0.35)
ggplot(dataD,aes(x=track.duration_ms, fill=playlist_name))+geom_density(col=NA,alpha=0.35)


view(dataD)

#Data PinkFLoyd
pf <- get_artist_audio_features('pink floyd')
pf
#Data The fray
tf<- get_artist_audio_features('the fray')
tf

#Renombrando para juntar
pf<-rename.vars(pf,from="album_release_date" ,to="track.album_release_date")
pf<-rename.vars(pf,from="duration_ms" ,to="track.duration_ms")
pf<-rename.vars(pf,from="track_name" ,to="track.album.name")

tf<-rename.vars(tf,from="album_release_date" ,to="track.album_release_date")
tf<-rename.vars(tf,from="duration_ms" ,to="track.duration_ms")
tf<-rename.vars(tf,from="track_name" ,to="track.album.name")

#Filtrando Another Brick in the Wall
brick<- pf %>% filter(track_id=="3DZEAAlv1tV7dlzNG1IBkQ")
brick<- brick %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,
                     tempo,track.duration_ms,track.album.name)
#Filtrando The Fray
tf<- tf %>% filter(track_id=="3URt4lJQlANOstDBAWQJA7")
tf<- tf %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,
                         tempo,track.duration_ms,track.album.name)

tf
brick


#Data Blinding lights
bl<-data %>% filter(track.id=="0VjIjW4GlUZAMYd2vXMi3b")
bl<-bl %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,
                   tempo,track.duration_ms,track.album.name)
#,track.album.release_date)
bl

#Juntando los datos
juntas <- list(brick,bl,tf)
lapply(juntas, str)
juntas <- rbind(brick,bl,tf)
View(juntas)

#Comparacion entre tres canciones
ggplot(juntas, aes(y=danceability, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))

ggplot(juntas, aes(y=energy, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))

ggplot(juntas, aes(y=loudness, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))

ggplot(juntas, aes(y=acousticness, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))

ggplot(juntas, aes(y=instrumentalness, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))

ggplot(juntas, aes(y=liveness, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))

ggplot(juntas, aes(y=valence, x=track.album.name)) + 
  geom_bar(position="dodge", stat="identity",aes(fill=track.album.name))
