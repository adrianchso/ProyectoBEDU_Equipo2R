library(dplyr)
#install.packages("treemap")
library(treemap)
library(ggplot2)
#install.packages("shinydashboard")
library(shinydashboard)

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

datosOaxaca <- read.csv("data/datosOaxaca.csv")
cultivosSiniestrados <- read.csv("data/topCultivosSiniestrados.csv")

#Rankings
rankingTotal <- read.csv("data/rankingTotal.csv")
rankingPorcentual <- read.csv("data/rankingPorcentual.csv")
rankingPorcentual$Porcentual <- rankingPorcentual$Porcentual * 100
#rankingPorcentual$Porcentual <- as.character(rankingPorcentual$Porcentual)

#Lluvias
dflluvias <- read.csv("data/lluviasOax.csv")
lluvias <- normalize(dflluvias)
lluvias <- ts(lluvias, start = 2000)

#Datos de maiz
dfOaxaca <- read.csv("data/dfOaxaca.csv")
dfmaiz <- filter(dfOaxaca, dfOaxaca$Nomcultivo== "Maiz grano")
dfmaiz <- select(dfmaiz, Anio, totalsin)
maiz <- normalize(dfmaiz$totalsin)
maiz <- ts(maiz, start = 2000)


nombresMunicipio <- unique(datosOaxaca$Nommunicipio)


ui <- dashboardPage(
    
    dashboardHeader(title = "EQUIPO 2"),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Inicio", tabName = "inicio", icon = icon("home")),
            
            menuItem("Municipios", tabName = "municipios", icon = icon("building")),
            
            menuItem("Cultivos", tabName = "cultivos", icon = icon("tractor")),
            
            menuItem("Series de tiempo", tabName = "timeserie", icon = icon("chart-line")),
            
            menuItem("Regresion lineal", tabName = "linealregression", icon = icon("external-link")),
            
            menuItem("Pronostico", tabName = "prediction", icon = icon("bullhorn"))
            
        )
        
    ),
    
    dashboardBody(
        
        
        tabItems(
            
            tabItem(tabName = "inicio",
                    
                    
                     fluidRow(
                         
                         box(solidHeader = TRUE, status = "primary", width = 12,
                             
                             h1("BEDU - Programacion R", align = "center"),
                             h2("Equipo 2", align = "center"),
                             h3("Integrantes", align = "center", style = "bold"),
                             h4("Adrian Chavez Soule", align = "center"),
                             h4("Andres Burjand Torres Reyes", align = "center"),
                             h4("Esthela Lizeth Soto Lara", align = "center"),
                             h4("Oswaldo Vazquez Aparicio", align = "center")
                             
                         ),
                         
                         box(title = "Objetivo", solidHeader = TRUE, status = "primary", width = 12,
                             
                             
                             column(8,
                                    
                                    tags$ul(
                                        
                                        tags$li("Demostrar que existe una correlacion entre el aumento de las lluvias 
                                                y la cantidad de hectareas de cultivo siniestradas."),
                                        
                                        tags$li("Intentar predecir la cantidad de hectareas siniestradas en el futuro 
                                                a corto plazo para que el gobierno de Oaxaca tenga estimados 
                                                mas realistas de las cosechas futuras"),
                                        
                                        tags$li("Estudiar las perdidas de los cultivos en el estado de Oaxaca")
                                        
                                    )
                                    
                                    ),
                             
                             column(4, align = "right",
                                    
                                    tags$img(src='images/cultivo.jpg', height='250', width='350'),
                                    
                                    #tags$img(src='https://cdn.pixabay.com/photo/2014/09/09/19/07/corn-field-440338_1280.jpg',height='250',width='350')
                                    
                                    
                                    )
                             
                             ),
                         
                         box(title = "Obtencion de datos", solidHeader = TRUE, status = "primary", width = 12,
                             
                             tags$a(href="https://smn.conagua.gob.mx/es/climatologia/temperaturas-y-lluvias/resumenes-mensuales-de-temperaturas-y-lluvias", 
                                    h3("CONAGUA - Resumenes Mensuales de Temperaturas y Lluvia")),
                             
                             tags$a(href="http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php", 
                                    h3("SIAP - Datos Abiertos")),
                             
                             
                             )
                         
                     )

                    
            ),
            
            tabItem(tabName = "municipios",
                    
                    
                    fluidRow(
                        
                        box(title = "Municipios", solidHeader = TRUE, status = "primary", width = 12,
                            
                            selectInput("nomMunicipio", "Selecciona el nombre de un municipio", 
                                        choices = nombresMunicipio),
                            
                            splitLayout(
                                
                                plotOutput("timeSerieSiniestrada"),
                                
                                plotOutput("cultivosMunicipio")
                            )
                            
                            
                            ),
                        
                        box(title = "Top 5 de Municipios con hectareas siniestradas", solidHeader = TRUE, status = "primary",width = 12, 
                            
                            tabBox(title = "Mas hectareas siniestradas", side = "right", height = "250px",
                                   
                                   tabPanel("En total", 
                                            
                                            column(12, align = "center",
                                                   
                                                   tableOutput("rankingTailTotal")
                                                   
                                            )
                                            
                                    ),
                                   
                                   tabPanel("Por hectarea sembrada", 
                                            
                                            column(12, align = "center",
                                            
                                                tableOutput("rankingTailPorcentual")
                                            
                                            )
                                            
                                    )
                                   
                            ),
                            
                            tabBox(title = "Menos hectareas siniestradas", side = "right", height = "250px",
                                   
                                   
                                   tabPanel("En total", 
                                            
                                            
                                            column(12, align = "center",
                                                   
                                                   tableOutput("rankingHeadTotal")
                                                   
                                            )
                                            
                                    ),
                                   
                                   tabPanel("Por hectarea sembrada", 
                                            
                                            
                                            column(12, align = "center",
                                                   
                                            
                                                   tableOutput("rankingHeadPorcentual")
                                                          
                                            )
                                                    
                                    )
                                   
                            )
                            
                        )
                        
                    )
                    
                    
                    
            ),
            
            tabItem(tabName = "cultivos",
                    
                 
                        
                        box(title = "Top cultivos con siniestros", solidHeader = TRUE, status = "primary", width = 12,
                            
                            column(6, align="center", 
                                   
                                   dataTableOutput("topCultivosSiniestrados")
                                   
                                   ),
                            
                            
                            column(6, align="center", 
                                   
                                   plotOutput("pieCultivosSiniestrados")
                                   
                            )
        
                        )
                            
                    
            ),
            
            tabItem(tabName = "timeserie",
                    
                    box(title = "Series de tiempo", solidHeader = TRUE, status = "primary", width = 12,
                        
                        p("A continuacion se muestran las series de tiempo de las lluvias promedio registradas
                          en el Estado de Oaxaca y las cosechas perdidas de maiz en el mismo."),
                        
                        fluidRow(
                            
                            column(4, align = "center", 
                                   
                                   selectInput("graph", "Selecciona grafica", 
                                                                    choices = c("Lluvias", "Maiz grano", "Ambas"))
                                   
                                   ),
                            
                            
                            column(8, align = "center",
                                   
                                   plotOutput("timeSerie")
                                   
                                   )
                            
                            
                        )
                        
                    )
                    
            ),
            
            tabItem(tabName = "linealregression",
                    
                    box(title = "Regression Lineal", solidHeader = TRUE, status = "primary", width = 12,
                        
                        
                        column(6, align = "center",
                               
                               plotOutput("linearRegression")
                               
                               ),
                        
                        column(6, align = "center",
                               
                               fluidRow(
                                   
                                   
                                   #tags$img(src='https://i.ibb.co/ngRRkk5/coefficients.jpg', height='250', width='500'),
                                   
                                   tags$img(src='images/coefficients.jpeg', height='250', width='500'),
                                   
                                   h2("-------------------------------"),
                                   
                                   tags$img(src='images/varianceTable.jpeg', height='150', width='500'),
                                   
                                   #tags$img(src='https://i.ibb.co/Fs3NVqz/variance-Table.jpg', height='150', width='500'),
                                        
                                        
                                        )
                               
                               
                               
                               #tableOutput("varianceTable")
                               
                               )
                        
                        )
                    
            ),
            
            tabItem(tabName = "prediction",
                    
                    
                    box(title = "Pronosticos", solidHeader = TRUE, status = "primary", width = 12,
                        
                        column(6, align = "center",
                               
                               plotOutput("predictionHectareas")
                               
                        ),
                        
                        column(6, align = "center",
                               
                               plotOutput("comparisonPredictions")
                               
                        )
                        
                        )
                    
                    )
            
        )
        
    )
    
    
)

server <- function(input, output) {

    
    output$timeSerieSiniestrada <- renderPlot({
        
        datosMunicipio <- datosOaxaca[datosOaxaca$Nommunicipio == input$nomMunicipio,]
        
        cantidadSiniestrada <- datosMunicipio %>%
            group_by(Anio) %>%
            summarise(Siniestrada = sum(Siniestrada))
        
        timeSerieSiniestrada <- ts(cantidadSiniestrada$Siniestrada, start = 2003, end = 2019)
        
        plot(timeSerieSiniestrada, ylab = "Hectareas siniestradas", xlab = "Anios", 
             main = "Cantidad Sinientrada en el Municipio", 
             sub = "Perido de 2003 - 2019")
        
    })
    output$cultivosMunicipio <- renderPlot({
        
        datosMunicipio <- datosOaxaca[datosOaxaca$Nommunicipio == input$nomMunicipio,]
        
        cantidadSembrada <- datosMunicipio %>%
            group_by(Nomcultivo) %>%
            summarise(Sembrada = sum(Sembrada))
        
        slices <- cantidadSembrada$Sembrada
        lbls <- cantidadSembrada$Nomcultivo
        
        
        treemap(cantidadSembrada,

                # data
                index="Nomcultivo",
                vSize="Sembrada",
                type="index",

                # Main
                title="Cultivos sembrados en Municipio",
                fontsize.title = 20,
                palette="Dark2",

                # Borders:
                border.col=c("black"),
                border.lwds=1,

                # Labels
                fontsize.labels=0.5,
                fontcolor.labels="white",
                fontface.labels=1,
                bg.labels=c("transparent"),
                align.labels=c("left", "top"),
                overlap.labels=0.5,
                inflate.labels=T

        )
        
    })
    
    numTop <- 5
    output$rankingHeadTotal <- renderTable({head(rankingTotal, n = numTop)})
    output$rankingTailTotal <- renderTable({tail(rankingTotal, n = numTop)})
    output$rankingHeadPorcentual <- renderTable({head(rankingPorcentual, n = numTop)})
    output$rankingTailPorcentual <- renderTable({tail(rankingPorcentual, n = numTop)})
    
    output$topCultivosSiniestrados <- renderDataTable({cultivosSiniestrados}, options = list(pageLength = 10))
    output$pieCultivosSiniestrados <- renderPlot({
        maizSiniestrado <- cultivosSiniestrados[1,]
        noMaizSiniestrado <- cultivosSiniestrados[2:35,]
        
        slices = c(maizSiniestrado$Siniestrada, sum(noMaizSiniestrado$Siniestrada))
        labels = c("Maiz grano", "Otros cultivos")
        
        
        colors <- c("cadetblue2", "deepskyblue4")
        
        pie(slices, labels = labels, col = colors, main="Toneladas siniestradas de cultivos en Oaxaca, Mx", radius = 1)
        
    })
    
    output$timeSerie <- renderPlot({
        
        data <- lluvias
        color <- c("cornflowerblue")
        title <- "Luvias en la region (Oaxaca)"
        legend <- c("Lluvias")
        
        
        if(input$graph == "Maiz grano"){
            data <- maiz
            color <- c("darkolivegreen3")
            title <- "Hectareas siniestradas de Maiz grano en Oaxaca"
            legend <- c("Maiz Siniestrado")
        }
        if(input$graph == "Ambas"){
            data <- cbind(lluvias, maiz)
            title = "Maiz grano siniestrado y lluvias en Oaxaca"
            color <- c("cornflowerblue", "darkolivegreen3")
            legend <- c("Lluvias", "Maiz Siniestrado")
        }
        
        ts.plot(data,lty = 1, lwd = 2, col = color, 
             main = title, xlab = "Anios", 
             ylab = "Magnitud (Datos Normalizados)", sub = "Periodo 2000- 2019")
        legend("topleft", legend = legend, col= color, pch=1)
        
    })
    
    output$linearRegression <- renderPlot({
        
        scatter <- cbind(hectareas = dfmaiz$totalsin, dflluvias)
        scatter <- as.data.frame(scatter)
        attach(scatter)
        analisis <- lm(hectareas ~ Lluvias)
        summary(analisis)
        anova(analisis)
        int.pred <- predict(analisis, interval = "prediction", level = 0.95)
        newscatter <- cbind(scatter,int.pred)
        head(newscatter)
        ggplot(newscatter, aes(x = Lluvias, y = hectareas)) + 
            geom_point() +
            geom_smooth(method ='lm', se = T, color = "brown1") + 
            theme_light() + 
            labs(x = "Magnitud de Lluvias (mm)", y = "Hectareas Siniestradas de Maiz") +
            ggtitle(expression(atop("Visualizacion de Intervalos: Confianza y Pronostico", 
                                    atop(italic("Periodo 2000 - 2019"), "")))) +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(angle=-30, hjust=0, vjust= 1)) +
            geom_line(aes(y = lwr), color = "chartreuse3", lty = 2, lwd = 1.5) + 
            geom_line(aes(y = upr), color = "chartreuse3", lty = 2, lwd = 1.5)
        
    })
    
    output$predictionHectareas <- renderPlot({
        ts.maiz <- ts(dfmaiz[,2], start = 2000, end= 2019)
        tiempo <- 1:length(ts.maiz)
        hec.sin.rl <- lm(log(ts.maiz) ~ tiempo + I(tiempo^2))
        pq <- c(0, 0, 0)
        akaike <- Inf
        for(i in 0:2)for(j in 0:2){
            modelo <- arima(resid(hec.sin.rl), order = c(i, 0, j))
            fit.aic <- AIC(modelo)
            if(fit.aic < akaike){
                pq <- c(i, 0, j)
                arma <- arima(resid(hec.sin.rl), order = pq)
                akaike <- fit.aic
            }
        }
        tiempo.p <- seq(length(ts.maiz)+1, length = 3)
        datos <- data.frame(Time = tiempo.p, periodo = rep(1,3))
        pronos.lm <- predict(hec.sin.rl, datos)
        pronos.arma <- predict(arma, n.ahead = 3)
        hec.sin.pred <- ts(exp(pronos.lm[18:20] + pronos.arma$pred), start = 2019)
        ts.plot(cbind(ts.maiz, hec.sin.pred), lty = 1:2, 
                col = c("blue", "red"), xlab = "Tiempo", 
                ylab = "Hectareas",
                main = "Prediccion de Hectareas Siniestradas de Maiz",
                sub = "Prediccion de los proximos 3 anios")
    })
    output$comparisonPredictions <- renderPlot({
        ts.maiz <- ts(dfmaiz[,2], start = 2000, end= 2019)
        tiempo <- 1:length(ts.maiz)
        hec.sin.rl <- lm(log(ts.maiz) ~ tiempo + I(tiempo^2))
        pq <- c(0, 0, 0)
        akaike <- Inf
        for(i in 0:2)for(j in 0:2){
            modelo <- arima(resid(hec.sin.rl), order = c(i, 0, j))
            fit.aic <- AIC(modelo)
            if(fit.aic < akaike){
                pq <- c(i, 0, j)
                arma <- arima(resid(hec.sin.rl), order = pq)
                akaike <- fit.aic
            }
        }
        tiempo.p <- seq(length(ts.maiz)+1, length = 3)
        datos <- data.frame(Time = tiempo.p, periodo = rep(1,3))
        pronos.lm <- predict(hec.sin.rl, datos)
        pronos.arma <- predict(arma, n.ahead = 3)
        hec.sin.pred <- ts(exp(pronos.lm[18:20] + pronos.arma$pred), start = 2019)
        
        
        prono_lluvia <- 1850.2
        nueva_estim <- -164490.49+156.46*prono_lluvia
        ts.plot(cbind(ts.maiz, hec.sin.pred), lty = 1:2, lwd = 1:2, 
                col = c("blue", "red"), xlab = "Tiempo", 
                ylab = "Hectareas",
                main = "Prediccion de Hectareas Siniestradas de Maiz: ARMA vs. Regresion",
                sub = "Prediccion puntual 2020") 
        points(x = 2020, y = nueva_estim, col = "red", pch = 16)
        abline(h=152500, lty = 3, col = "darkgoldenrod2")
        abline(h=120000, lty = 3, col = "darkgoldenrod2")
    })
    
}

shinyApp(ui, server)
