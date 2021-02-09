setwd("D:\\ACS\\BEDU\\ProyectoR")

library(dplyr)

######

#datos <- lapply(dir(), read.csv) 
#datos <- lapply(datos, select, Nomestado, Anio, Nommodalidad, Nomcultivo, Sembrada, Siniestrada) 

#datos <- do.call(rbind, datos)

datos <- read.csv("dfOaxaca.csv")
datos
AgriOaxaca <- filter(datos, datos$Nomestado == "Oaxaca", datos$Nommodalidad == "Temporal")


AgriOaxacasin <- AgriOaxaca %>%  group_by(Anio,Nomcultivo) %>% summarize(totalsin = sum(Siniestrada))

dfOaxaca <- as.data.frame(AgriOaxacasin)

#Eliminar renglones no importantes 
dfOaxaca <- filter(dfOaxaca, dfOaxaca$totalsin != 0)

#write.csv(dfOaxaca, "dfOaxaca.csv")

dfOaxaca <- read.csv("dfOaxaca.csv")

dfmaiz <- filter(dfOaxaca, dfOaxaca$Nomcultivo== "Maiz grano")

dfmaiz <- select(dfmaiz, Anio, totalsin)


#dfOaxaca.a√±o <- dfOaxaca %>% group_by(Anio) %>% summarize(totalsin = sum(totalsin))

#ts.Agria√±o <- ts(dfOaxaca.a√±o[,2], start=2000, end = 2019)

ts.maiz <- ts(dfmaiz[,2], start = 2000, end= 2019)

#Normalizar los datos para graficar ambas series de tiempo en la misma gr·fica

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



data.Oax <- read.csv("lluviasOax.csv")
data.Oax.norm <- normalize(data.Oax)
data.Oax.ts <- ts(data.Oax.norm, start = 2000)


dfmaiz.norm <- normalize(dfmaiz$totalsin)
dfmaiz.ts <- ts(dfmaiz.norm, start = 2000)

#Gr·ficas de series de tiempo (conjunta e individuales)

plot(data.Oax.ts,lty = 1, lwd = 2, col = "darkolivegreen3", 
     main = "Hect·reas siniestradas de MaÌz", xlab = "AÒos", 
     ylab = "Magnitud (Datos Normalizados)", sub = "Periodo 2000- 2019")

plot(dfmaiz.ts,lty = 1, lwd = 2, col = "cornflowerblue", 
     main = "Luvias en la regiÛn (Oaxaca)", xlab = "AÒos", 
     ylab = "Magnitud (Datos Normalizados)", sub = "Periodo 2000- 2019")

ts.plot(cbind(dfmaiz.ts,data.Oax.ts), lty = 1:1, lwd = 2:2 ,
        col = c("darkolivegreen3","cornflowerblue"), main = "EvoluciÛn de los FenÛmenos",
        xlab = "AÒos", ylab = "Magnitud (Datos Normalizados)", sub = "Periodo 2000- 2019")
legend("topleft", legend = c("MaÌz Siniestrado","Luvias"),
       col= c("darkolivegreen3","cornflowerblue"), pch=1)

#Gr·fica de dispersiÛn con la lÌnea de regresiÛn

library(ggplot2)
scatter <- cbind(hectareas = dfmaiz$totalsin,data.Oax)
scatter <- as.data.frame(scatter)
ggplot(scatter, aes(x = Lluvias, y = hectareas)) + 
  geom_point() +
  geom_smooth(method ='lm', se = F, color = "brown1") + 
  theme_light() + 
  labs(x = "Magnitud de Lluvias (mm)", y = "Hect·reas Siniestradas de MaÌz") +
  ggtitle(expression(atop("Mapa de DispersiÛn con LÌnea de MÌnimos Cuadrados", 
                          atop(italic("Periodo 2000 - 2019"), "")))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=-30, hjust=0, vjust= 1))

#Gr·fica de pastel

Oaxaca.cult <- dfOaxaca %>%  group_by(Nomcultivo) %>% summarize(totalcult = sum(totalsin))
maiz <- filter(Oaxaca.cult, Oaxaca.cult$Nomcultivo == "Maiz grano") %>% 
  select(totalcult) %>% rename(total = totalcult)
no.maiz<- filter(Oaxaca.cult, Oaxaca.cult$Nomcultivo != "Maiz grano") %>% 
  summarise(total = sum(totalcult))
pal.pie <- rbind(maiz, no.maiz)

pie(pal.pie$total, labels = c("Maiz Grano", "Otros"), 
    col = c("cadetblue2", "deepskyblue4"), 
    main = "Toneladas siniestradas de cultivos en Oaxaca, Mx", radius = 1)

# An·lisis de los datos: regresiÛn lineal

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
  labs(x = "Magnitud de Lluvias (mm)", y = "Hect·reas Siniestradas de MaÌz") +
  ggtitle(expression(atop("VisualizaciÛn de Intervalos: Confianza y PronÛstico", 
                          atop(italic("Periodo 2000 - 2019"), "")))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=-30, hjust=0, vjust= 1)) +
  geom_line(aes(y = lwr), color = "chartreuse3", lty = 2, lwd = 1.5) + 
  geom_line(aes(y = upr), color = "chartreuse3", lty = 2, lwd = 1.5)

# PredicciÛn usando ARMA y Akaike

ts.maiz
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
pq
tiempo.p <- seq(length(ts.maiz)+1, length = 3)
datos <- data.frame(Time = tiempo.p, periodo = rep(1,3))
pronos.lm <- predict(hec.sin.rl, datos)
pronos.arma <- predict(arma, n.ahead = 3)

hec.sin.pred <- ts(exp(pronos.lm[18:20] + pronos.arma$pred), start = 2019)
ts.plot(cbind(ts.maiz, hec.sin.pred), lty = 1:2, 
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Hect·reas",
        main = "PredicciÛn de Hect·reas Siniestradas de MaÌz",
        sub = "PredicciÛn de los prÛximos 3 aÒos") 

# PredicciÛn usando resultados de la regresiÛn lineal

prono_lluvia <- 1850.2
nueva_estim <- -164490.49+156.46*prono_lluvia
nueva_estim
ggplot(newscatter, aes(x = Lluvias, y = hectareas)) + 
  geom_point() +
  geom_smooth(method ='lm', se = F, color = "brown1") + 
  theme_light() + 
  labs(x = "Magnitud de Lluvias (mm)", y = "Hect·reas Siniestradas de MaÌz") +
  ggtitle(expression(atop("PronÛstico Puntual y de Intervalo para 2020", 
                          atop(italic("Periodo 2000 - 2019"), "")))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=-30, hjust=0, vjust= 1)) +
  geom_line(aes(y = lwr), color = "chartreuse3", lty = 2, lwd = 1.5) + 
  geom_line(aes(y = upr), color = "chartreuse3", lty = 2, lwd = 1.5) +
  geom_point(aes(x = prono_lluvia,y = nueva_estim), color = "blue", shape = 19, size =2) + 
  geom_segment(aes(x = prono_lluvia, y = 86000, xend = prono_lluvia, yend = 163000),
               col = "blue", lwd = 1)


ts.plot(cbind(ts.maiz, hec.sin.pred), lty = 1:2, lwd = 1:2, 
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Hect·reas",
        main = "PredicciÛn de Hect·reas Siniestradas de MaÌz: ARMA vs. RegresiÛn",
        sub = "PredicciÛn puntual 2020") 
points(x = 2020, y = nueva_estim, col = "red", pch = 16)
abline(h=152500, lty = 3, col = "darkgoldenrod2")
abline(h=120000, lty = 3, col = "darkgoldenrod2")
