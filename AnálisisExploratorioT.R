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
                      "track.album.release_date",
                      "mode_name",
                      "key_mode")

mundial <- read.csv('México.csv')
mexico <- read.csv('Mundo.csv')

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
    aes(valence,
        fill = cut(valence, 100)) +
    geom_histogram(binwidth = 0.06,
                   show.legend = FALSE) + 
    ggtitle("Histograma de valencia 2020") +
    ylab("Frecuencia") +
    xlab("Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

    
# 2019
mexico[51:150,] %>%
    ggplot() + 
    aes(valence, 
        fill = cut(valence, 100)) +
    geom_histogram(binwidth = 0.08,
                   show.legend = FALSE) + 
    ggtitle("Histograma de valencia 2019") +
    ylab("Frecuencia") +
    xlab("Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

# 2018
mexico[151:200,] %>%
    ggplot() + 
    aes(valence, 
        fill = cut(valence, 100)) +
    geom_histogram(binwidth = 0.08,
                   show.legend = FALSE) + 
    ggtitle("Histograma de valencia 2018") +
    ylab("Frecuencia") +
    xlab("Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Valencia en el mundo pandémico ####
# 2020
mundial[1:50,] %>%
    ggplot() + 
    aes(valence,
        fill = cut(valence, 100)) +
    geom_histogram(binwidth = 0.1,
                   show.legend = FALSE) + 
    ggtitle("Histograma de valencia 2020") +
    ylab("Frecuencia") +
    xlab("Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

    
    
# 2019   
mundial[51:150,] %>%
    ggplot() + 
    aes(valence,
        fill = cut(valence, 100)) +
    geom_histogram(binwidth = 0.09,
                   show.legend = FALSE) + 
    ggtitle("Histograma de valencia 2019") +
    ylab("Frecuencia") +
    xlab("Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))



# 2018   
mundial[151:250,] %>%
    ggplot() + 
    aes(valence,
        fill = cut(valence, 100)) +
    geom_histogram(binwidth = 0.09,
                   show.legend = FALSE) + 
    ggtitle("Histograma de valencia 2018") +
    ylab("Frecuencia") +
    xlab("Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

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
           y = energy)) + 
    geom_point(col = "#58D3F7") +
    theme_gray()+
    ggtitle('Volumen contra enegía')+
    labs(x = 'Energía',
         y = 'Volumen')+
    theme_dark()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))    

# Danzabilidad contra energía
ggplot(mundial)+
       aes(x = energy,
           y = danceability)+
    geom_point(col = "#F7BE81") +
    theme_gray()+
    ggtitle('Bailabilidad contra energía')+
    labs(x = 'Energía',
         y = 'Bailabilidad')+
    theme_dark()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Valencia contra danzabilidad
ggplot(mundial,
       aes(x = danceability, 
           y = valence)) +
    geom_point(col = "#F7BE81") +
    theme_gray()+
    ggtitle('Valencia contra bailabilidad')+
    labs(x = 'Bailabilidad',
         y = 'Valencia')+
    theme_dark()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Habla contra instrumental
ggplot(mundial,
       aes(x = speechiness, 
           y = instrumentalness)) + 
    geom_point(col = "#58FA58") +
    theme_gray()+
    ggtitle('Habla contra instrumental')+
    labs(x = 'Habla',
         y = 'Instrumental')+
    theme_dark()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

# Valencia y modo ####

ggplot(mexico) + 
    aes(x=mode_name,
        y=valence, 
        fill = cut(valence, 100)) +
    geom_bar(stat = 'identity',
             show.legend = FALSE)+
    ggtitle("Valencia según el modo") +
    labs(x = "Modo",y = "Valencia")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

# Nótese que lo anterior se puede deber a...
# - mayor número de canciones en mayor
# - las que están en mayor tienen más valencia

# Histograma del nombre del modo
ggplot(mexico) + 
    aes(mode_name,
        fill = cut(valence, 100)) +
    geom_histogram(stat='count',
                   show.legend = FALSE) + 
    ggtitle("Histograma de los modos") +
    ylab("Frecuencia") +
    xlab("Modo")+
    theme_gray()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Nótese que efectivamente hay más canciones en mayor
ggplot(mundial, aes(x =valence , y = key_name, fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
    scale_fill_viridis_c(name = "Valencia", option = "D") +
    labs(
        title = 'Titulo',
        subtitle = 'Subtitulo'
    ) +
    theme_ridges(font_size = 13, grid = TRUE) + 
    theme(axis.title.y = element_blank())

ggplot(mundial, aes(x=valence, color =key_name ,fill=key_name)) + geom_histogram(aes(y=..density..),alpha = 0.5)

