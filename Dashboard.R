## app.R ##

library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)


#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
        
        dashboardPage(skin = "green",
            
            dashboardHeader(title = "Qué piensa Spotify de"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Top tracks (2020-2000)", tabName = "data_table", icon = icon("table")),
                    menuItem("Correlación", tabName = "img", icon = icon("file-picture-o"))
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Histograma
                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                titlePanel("Histograma de las variables del data set mtcars"), 
                                selectInput("x", "Seleccione el valor de X",
                                            choices = names(mtcars)),
                                
                                selectInput("zz", "Selecciona la variable del grid", 
                                            
                                            choices = c("cyl", "vs", "gear", "carb")),
                                box(plotOutput("plot1", height = 250)),
                                
                                box(
                                    title = "Controls",
                                    sliderInput("bins", "Number of observations:", 1, 30, 15)
                                )
                            )
                    ),
                    
                    # Dispersión
                    tabItem(tabName = "graph", 
                            fluidRow(
                                titlePanel(h3("Gráficos de dispersión")),
                                selectInput("a", "Selecciona el valor de x",
                                            choices = names(mtcars)),
                                selectInput("y", "Seleccione el valor de y",
                                            choices = names(mtcars)),
                                selectInput("z", "Selecciona la variable del grid", 
                                            choices = c("cyl", "vs", "gear", "carb")),
                                box(plotOutput("output_plot", height = 300, width = 460) )
                                
                            )
                    ),
                    
                    
                    
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Características identificadas por Spotify de los Top Tracks")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    tabItem(tabName = "img",
                            fluidRow(
                                titlePanel(h3("Imágen de calor para la correlación de las variables")),
                                img( src = "corrspotify.png", 
                                     height = 600, width = 800)
                            )
                    )
                    
                )
            )
        )
    )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    
    #Gráfico de Histograma
    output$plot1 <- renderPlot({
        
        x <- mtcars[,input$x]
        bin <- seq(min(x), max(x), length.out = input$bins + 1)
        
        ggplot(mtcars, aes(x, fill = mtcars[,input$zz])) + 
            geom_histogram( breaks = bin) +
            labs( xlim = c(0, max(x))) + 
            theme_light() + 
            xlab(input$x) + ylab("Frecuencia") + 
            facet_grid(input$zz)
        
        
    })
    
    # Gráficas de dispersión
    output$output_plot <- renderPlot({ 
        
        ggplot(mtcars, aes(x =  mtcars[,input$a] , y = mtcars[,input$y], 
                           colour = mtcars[,input$z] )) + 
            geom_point() +
            ylab(input$y) +
            xlab(input$a) + 
            theme_linedraw() + 
            facet_grid(input$z)  #selección del grid
        
    })   
    
    #Data Table
    output$data_table <- renderDataTable( {datadash}, 
                                          options = list(aLengthMenu = c(20,50,100,200), scrollX = TRUE,
                                                         iDisplayLength = 20)
    )
    
}


shinyApp(ui, server)