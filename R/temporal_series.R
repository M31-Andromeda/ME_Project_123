
paquetes <- c("MASS", "mice", "tidyverse", "corrplot", "ranger", "plotly",
              "gplots", "rstatix", "rcompanion", "car", "forecast", "pROC")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df <- read.csv("data/raw/student_daily_attendance.csv")

df.01M539 <- df[df$School.DBN == "01M539", ]

# ANÁLISIS DE ESTACIONARIEDAD

# Calculamos el porcentaje de asistencia como variable a predecir
df.01M539$PercentageOfAttendance = df.01M539$Present / df.01M539$Enrolled
attendance <- ts(df.01M539$PercentageOfAttendance, start=1, end=178, frequency=5)
plot(attendance)

# Dibujamos los componentes de la serie
plot(decompose(attendance)) 

# Dibujamos un boxplot en periodos de 10 muestras para comprobar si la variancia es constante
periodos <- ceiling(seq_along(df.01M539$PercentageOfAttendance) / 10)
boxplot(df.01M539$PercentageOfAttendance ~ periodos,
        xlab = "Periodo (10 muestras)", ylab = "PercentageOfAttendance",
        main = "PercentageOfAttendance en periodos de 10 muestras")

# En los componentes se ve que la escala de la estacionalidad es muy pequeña

##-- ACF y PACF de la serie temporal
par(mfrow=c(1,2))
acf(attendance, ylim=c(-1,1), lagmax = 40)
pacf(attendance, ylim=c(-1,1), lagmax = 40)

##-- Ajustamos SARIMA
sarima <- arima(attendance,
                         order    = c(1,1,0), 
                         seasonal = list(order = c(1,1,0), 
                                         period = 89), include.mean = FALSE)
res <- residuals(sarima)

plot(res, main = 'sarima')

##-- ACF i PACF
par(mfrow=c(1,2))
acf(res, ylim=c(-1,1), lag.max = 40)
pacf(res, ylim=c(-1,1), lag.max = 40)
