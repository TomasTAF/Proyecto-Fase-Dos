# Librerías ####
library(ggplot2)
library(dplyr)
library(corrplot)

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


# Nombre de las variables
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

# Valencia en México pandémico ####

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

# Valencia en el mundo pandémico ####
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
    
# Popularidad mexicana con respecto a otras variables ####

# Danzabilidad
ggplot(mexico, 
       aes(x = danceability, 
           y = track.popularity,
           col = mode_name)) + 
    geom_point() +  
    labs(x = 'Danzabilidad', y = 'Popularidad')

# Energía
ggplot(mexico,
       aes(x = energy, 
           y = track.popularity,
           col = mode_name)) + 
    geom_point() + 
    labs(x = 'Energía', y = 'Popularidad')

# Volumen
ggplot(mexico, 
       aes(x = loudness,
           y = track.popularity,
           col = mode_name)) + 
    geom_point() + 
    labs(x = 'Volumen', y = 'Popularidad')

# Correlaciones ####
# Selección de columnas numéricas
numericas.columnas <- c("danceability",
                        "energy",
                        "loudness",
                        "speechiness",
                        "instrumentalness",
                        "valence",
                        "tempo",
                        "track.duration_ms",
                        "track.popularity")

mexico.numericas <- mexico %>% select(numericas.columnas)
mundial.numericas <- mundial %>% select(numericas.columnas)

# Correlogramas
mexico.cor <- cor(mexico.numericas)
corrplot(mexico.cor)

mundial.cor <- cor(mundial.numericas)
corrplot(mundial.cor)

# Comprobación de ideas básicas ####

# Volumen contra energía
ggplot(mundial,
       aes(x = loudness,
           y = energy,
           col = mode_name)) + 
    geom_point() + 
    labs(x = 'Energía', y = 'Volumen')

# Danzabilidad contra energía
ggplot(mundial,
       aes(x = energy,
           y = danceability,
           col = mode_name)) + 
    geom_point() + 
    labs(x = 'Energía', y = 'Danzabilidad')

# Habla contra instrumental
ggplot(mundial,
       aes(x = speechiness, 
           y = instrumentalness,
           col = mode_name)) + 
    geom_point() + 
    labs(x = 'Habla', y = 'Instrumental')

# Valencia contra danzabilidad
ggplot(mundial,
       aes(x = energy, 
           y = valence,
           col = mode_name)) + 
    geom_point() + 
    labs(x = 'Habla', y = 'Instrumental')

# Valencia y modo ####

ggplot(mexico, aes(x=mode_name, y=valence)) + 
    geom_bar(stat = 'identity')

# Nótese que lo anterior se puede deber a...
# - mayor número de canciones en mayor
# - las que están en mayor tienen más valencia

# Histograma del nombre del modo
ggplot(mexico) + 
    aes(mode_name) +
    geom_histogram(stat='count') 

# Nótese que efectivamente hay más canciones en mayor