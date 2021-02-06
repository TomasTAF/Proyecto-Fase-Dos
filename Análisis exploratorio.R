{ # Librerías
library(ggplot2)
library(dplyr)
}

# Renombrar conjunto de datos
mexico <- dataM
mundial <- data


{# Nombre de las variables
names(mexico)[1:30]
names(mundial)[1:30]

# "playlist_id"                    
# "playlist_name"                 
# "playlist_img"                   
# "playlist_owner_name"           
# "playlist_owner_id"
# "danceability"                  
# "energy"
# "key"                           
# "loudness"
# "mode"                          
# "speechiness"
# "acousticness"                  
# "instrumentalness"
# "liveness"                      
# "valence"
# "tempo"                         
# "track.id"
# "analysis_url"                  
# "time_signature"
# "added_at"                      
# "is_local"
# "primary_color"                 
# "added_by.href"
# "added_by.id"                   
# "added_by.type"
# "added_by.uri"                  
# "added_by.external_urls.spotify"
# "track.artists"                 
# "track.available_markets"
# "track.disc_number"
# "added_at"
}

{# Preguntas a las que se orientan los gráficos
# - Pandemia y valencia en top 100
# - Dividir por países?
# - Ver global
# - track_popularity == ranking?
# - Lenguaje explícito y su correlación
# - Speechness y su correlación con la popularidad por area
# - Regiones cerca del ecuador y lejos
# - Perfil de la canción mas popular cada año
# - Formula secreta para sacar un hit**
}

ggplot(mundial, aes(x = energy, y = loudness,col = time_signature)) + 
        geom_point()ggplot(mundial, aes(x = energy, y = loudness,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Energía', y = 'Volumen', colour = 'Signatura de compás')
         + 
        labs(x = 'Energía', y = 'Volumen', colour = 'Signatura de compás')
        { # Histogramas

ggplot(mundial, aes(x = energy, y = loudness,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Energía', y = 'Volumen', colour = 'Signatura de compás')
            {# Valencia de la pandemia mexicana
    
    # Para el 2020
    mexico[1:50,] %>%
        ggplot() + 
        aes(valence) +
        geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
        ggtitle("Histograma de valencia 2020") +
        ylab("Frecuencia") +
        xlab("Valencia") + 
        theme_light()
    
    
    # Años 2018 y 2019    
    mexico[51:nrow(mexico),] %>%
        ggplot() + 
        aes(valence) +
        geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
        ggtitle("Histograma de valencia 2020") +
        ylab("Frecuencia") +
        xlab("Valencia") + 
        theme_light()
    } # La valencia ha bajado en México
    
    {# Valencia de la pandemia mundial
    
    # Para el 2020
    mundial[1:50,] %>%
        ggplot() + 
        aes(valence) +
        geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
        ggtitle("Histograma de valencia 2020") +
        ylab("Frecuencia") +
        xlab("Valencia") + 
        theme_light()
    
    
    # Años 2019   
    mexico[51:150,] %>%
        ggplot() + 
        aes(valence) +
        geom_histogram(binwidth = 0.09, col="black", fill = "blue") + 
        ggtitle("Histograma de valencia 2020") +
        ylab("Frecuencia") +
        xlab("Valencia") + 
        theme_light()
    } # Da la impresión de tambien haber bajado, pero hay muy pocos datos
    
    # Comparar las medias en diferentes años a nivel mundial y nacional
}

{ # Gráficos de dispersión
    
    { # Mostrar la relación entre la popularidad y otras variables
    
    # Se opta por los datos a nivel mundial
    
        {# Danzabilidad
        ggplot(mundial, aes(x = danceability, y = track.popularity,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Danzabilidad', y = 'Popularidad', colour = 'Signatura de compás')
        }
        
        {# Energía
        ggplot(mundial, aes(x = energy, y = track.popularity,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Energía', y = 'Popularidad', colour = 'Signatura de compás')
        }
        
        {# Volumen
        ggplot(mundial, aes(x = loudness, y = track.popularity,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Volumen', y = 'Popularidad', colour = 'Signatura de compás')
        }
        
    }
    
    {# Comprobación de ideas básicas
        
        { # Volumen contra energía
        ggplot(mundial, aes(x = energy, y = loudness,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Energía', y = 'Volumen', colour = 'Signatura de compás')
        }
        
        { # Danzabilidad contra energía
        ggplot(mundial, aes(x = energy, y = danceability,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Energía', y = 'Danzabilidad', colour = 'Signatura de compás')
        }
        
        { # Habla contra instrumental
        ggplot(mundial, aes(x = speechiness, y = instrumentalness,col = time_signature)) + 
        geom_point() + 
        labs(x = 'Habla', y = 'Instrumental', colour = 'Signatura de compás')
        }
        
    }
}

{ # Diagramas de caja
}

{ # Gráficos de barras
}
