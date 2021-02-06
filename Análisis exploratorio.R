# Librerías ####
library(ggplot2)
library(dplyr)

# Selección de las variables de interés ####
variables.vector <- c("danceability",
                      "energy",
                      "loudness",
                      "mode",
                      "speechiness",
                      "instrumentalness",
                      "valence",
                      "tempo",
                      "track.duration_ms",
                      "track.explicit",
                      "track.popularity",
                      "track.name",
                      "key_name",
                      "mode_name",
                      "key_mode")

mundial <- dataM %>% select(variables.vector)
mexico <- data %>% select(variables.vector)


# Nombre de las variables ####
names(mexico)
names(mundial)

# danceability
# energy
# loudness
# mode
# speechiness
# instrumentalness
# valence
# tempo
# track.duration_ms
# track.explicit
# track.popularity
# track.name
# key_name
# mode_name
# key_mode 

# Preguntas de orientación ####
# - Pandemia y valencia en top 100
# - Dividir por países?
# - Ver global
# - track_popularity == ranking?
# - Lenguaje explícito y su correlación
# - Speechness y su correlación con la popularidad por area
# - Regiones cerca del ecuador y lejos
# - Perfil de la canción mas popular cada año
# - Formula secreta para sacar un hit**

# Valencia en pandemía ####

# 2020
mexico[1:50,] %>%
    ggplot() + 
    aes(valence) +
    geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
    ggtitle("Histograma de valencia 2020") +
    ylab("Frecuencia") +
    xlab("Valencia") + 
    theme_light()
    
    
# 2018 y 2019    
mexico[51:nrow(mexico),] %>%
    ggplot() + 
    aes(valence) +
    geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
    ggtitle("Histograma de valencia 2020") +
    ylab("Frecuencia") +
    xlab("Valencia") + 
    theme_light()

# Valencia mundial en pandemía ####
# 2020
mundial[1:50,] %>%
    ggplot() + 
    aes(valence) +
    geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
    ggtitle("Histograma de valencia 2020") +
    ylab("Frecuencia") +
    xlab("Valencia") + 
    theme_light()
    
    
# 2019   
mexico[51:150,] %>%
    ggplot() + 
    aes(valence) +
    geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
    ggtitle("Histograma de valencia 2020") +
    ylab("Frecuencia") +
    xlab("Valencia") + 
    theme_light()

# Popularidad mundial con respecto a otras variables ####
    
# Danzabilidad
ggplot(mundial, aes(x = danceability, y = track.popularity)) + 
    geom_point() + 
    labs(x = 'Danzabilidad', y = 'Popularidad')
        
# Energía
ggplot(mundial, aes(x = energy, y = track.popularity)) + 
        geom_point() + 
        labs(x = 'Energía', y = 'Popularidad')
        
# Volumen
ggplot(mundial, aes(x = loudness, y = track.popularity)) + 
        geom_point() + 
        labs(x = 'Volumen', y = 'Popularidad')
    
# Comprobación de ideas básicas ####
    
# Volumen contra energía
ggplot(mundial, aes(x = loudness, y = energy)) + 
    geom_point() + 
    labs(x = 'Energía', y = 'Volumen')
    
# Danzabilidad contra energía
ggplot(mundial, aes(x = energy, y = danceability)) + 
    geom_point() + 
    labs(x = 'Energía', y = 'Danzabilidad')
    
# Habla contra instrumental
ggplot(mundial, aes(x = speechiness, y = instrumentalness)) + 
    geom_point() + 
    labs(x = 'Habla', y = 'Instrumental')

# Popularidad mexicana con respecto a otras variables ####

# Danzabilidad
ggplot(mexico, aes(x = danceability, y = track.popularity)) + 
    geom_point() + 
    labs(x = 'Danzabilidad', y = 'Popularidad')

# Energía
ggplot(mexico, aes(x = energy, y = track.popularity)) + 
    geom_point() + 
    labs(x = 'Energía', y = 'Popularidad')

# Volumen
ggplot(mexico, aes(x = loudness, y = track.popularity)) + 
    geom_point() + 
    labs(x = 'Volumen', y = 'Popularidad')
