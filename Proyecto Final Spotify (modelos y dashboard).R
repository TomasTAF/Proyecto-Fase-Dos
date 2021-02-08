#Proyecto BEDU 
#Acceso a la API de Spotify
#Primero instalamos devtools para poder descargar spotifyr
#install.packages('devtools')
library(devtools)
#Ahora instalamos spotifyr
devtools::install_github('charlie86/spotifyr')
#Cargamos otras paqueterías necesarias
library(spotifyr)
library(tidyverse)
library(knitr)

#Ingresamos las credenciales necesarias para acceder a la API
Sys.setenv(SPOTIFY_CLIENT_ID = '72654f91e8424dc8b4924dc4cdf3674a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '968caf037e674bb0a89533fde2831f25')

access_token <- get_spotify_access_token()

#Comenzamos a utilizar las herramientas de la paquetería spotifyr
#Creamos un dataset llamado beatles accediendo a las características de audio de esta banda
beatles <- get_artist_audio_features('the beatles')
#Observamos el conteo de notas en las que fueron compuestas las canciones de los beatles
#Importante resaltar el uso de kable() como una herramienta de estilo para tablas
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
View(joy)
#Graficamos la valencia de cada pista utilizando el paquete ggridges para instalar ggjoy
install.packages('ggridges')
install.packages('ggjoy')
library(ggjoy)
library(ggridges)
#Cargamos ggplot en caso de no estar activo en la sesión
library(ggplot2)
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

#Obtener las caracterìsticas de las canciones top de 2020 - 2000 en el mundo 
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
#Observamos la relaciòn entre la bailabilidad y la popularidad de la canción
ggplot(data, aes(x=danceability, y=track.popularity, color=playlist_name)) + geom_point()

#Observamos el promedio de la popularidad en cada año 
ggplot(data) + 
  geom_bar(aes(x=playlist_name, y=(track.popularity)), stat = "summary", fun="mean")

#Creamos un modelo de regresión lineal múltiple describiendo la popularidad de estas canciones explicadas por sus características musicales
attach(data)
Modelo1<- lm(track.popularity ~ danceability + energy + loudness + speechiness + acousticness + instrumentalness + liveness + valence + tempo + track.explicit +track.duration_ms)
summary(Modelo1)
ggplot(data,aes(y=track.popularity,x=energy))+geom_point()+geom_smooth(method="lm")
plot(track.popularity, danceability, xlab = "Popularidad", ylab= "Bailaviliad")
res <- cor(track.popularity,danceability,energy)


#Creamos un modelo logístico para determinar si un track ya popular lo será entre sus similares
datalog<-data[,c(2,(6:16),31,33,36,37)]
View(datalog)
#Para hacer un modelo logístico, necesitamos que la variable a modelar sea dicotómica (cero o uno), lo que hacemos creando una nueva varaible que nos dice si la popularidad del track es mayor al promedio
mean(datalog$track.popularity)
track.popularitylog<- if_else(datalog$track.popularity >= mean(datalog$track.popularity),1,0)
datalog2 <- cbind(datalog,track.popularitylog)
View(datalog2)
#Especificamos el modelo logístico
Modelo2<-glm(datalog2$track.popularitylog ~ datalog2$danceability + datalog2$energy + datalog2$loudness + datalog2$speechiness + datalog2$acousticness + datalog2$instrumentalness + datalog2$liveness + datalog2$valence + datalog2$tempo + datalog2$track.explicit + datalog2$track.duration_ms,family = "binomial")
summary(Modelo2)
install.packages("pscl")
library(pscl)
pR2(Modelo2)

#Dividir datos en set de entrenamiento y set de predicción
set.seed(2)
samplelog<-sample(nrow(datalog2),1025)
trainlog<-datalog2[samplelog,]
predictlog<-datalog2[-samplelog,]
Direction_testing <- trainlog$track.popularitylog

#predecimos la probabilidad de ser un top hit 
Modelo3<-glm(trainlog$track.popularitylog ~ trainlog$danceability + trainlog$energy + trainlog$loudness + trainlog$speechiness + trainlog$acousticness + trainlog$instrumentalness + trainlog$liveness + trainlog$valence + trainlog$tempo + trainlog$track.explicit + trainlog$track.duration_ms, data=trainlog, family = "binomial")
model_pred_probs <- predict(Modelo3,trainlog, type = "response")
model_pred_Direction=rep("0",1025)
model_pred_Direction[model_pred_probs>0.6] = "1"
#Matriz de confusión 
table(model_pred_Direction, Direction_testing)
#Calculamos la precisiòn del modelo 
(605)/(605+324)

#Matriz de correlación para el dashboard
datacorr<-datalog[,-c(1,15)]
mdatacorr<-cor(datacorr)
install.packages("corrplot")
library(corrplot)
corrplot(mdatacorr)

#Preparamos los datos para el dashboard
datadash<-datalog[,c(15,16,(2:14),1)]

