
#Inicialmente cargo la base de datos que proviene de Stata

library(haven)
CNA <- read_dta("D:/Climate change/Codes/S06A(Cultivos)_Wjo_v1.dta")
View(CNA)

#Sumo para la variable clave el área sembrada
tabla_1 <- aggregate(AREA_SEMBRADA ~ clave + COD_VEREDA + P_S6P46, data=CNA, sum)

#Validación que los dos totales coinciden
sum(CNA$AREA_SEMBRADA)
sum(tabla_1$AREA_SEMBRADA)


#Guardar el data frame en excel
write.csv(tabla_1, "prueba123")

# Se totaliza el area total sembrada por vereda
library(data.table)
Tot_V <- as.data.table(tabla_1)
Tot_V[, area_total_vereda := sum(AREA_SEMBRADA), by=c('COD_VEREDA')]
Tot_V
sort(Tot_V,decreasing = FALSE, na.last = NA)

# Ahora se calcula la participación de cada cultivo en las respectivas veredas, 
Tot_V$Pond=Tot_V$AREA_SEMBRADA/Tot_V$area_total_vereda


#Determino las ponderaciones al cuadrado
# Ahora se calcula la participación de cada cultivo en las respectivas veredas, 
Tot_V$Pond_cuad=Tot_V$Pond*Tot_V$Pond



#agrego al dataframe el Herfindal index (HI)=??pond_cuad por veredas
Tot_V2 <- as.data.table(Tot_V)
Tot_V2[, Herfindal_index := sum(Pond_cuad), by=c('COD_VEREDA')]
Tot_V2

#Calculo el Crop Diversification Index (CDI) por vereda
Tot_V2$CDI=1-Tot_V2$Herfindal_index


#validaciones que las ponderaciones esten OK 
sum(Tot_V$Pond)
sum(Tot_V$Pond_cuad)
sum(Tot_V2$Herfindal_index)
sum(Tot_V2$CDI)
