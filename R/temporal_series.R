
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

#   Calculamos el porcentaje de asistencia para incluirlo en el análisis
df.01M539$PercentageOfAttendance = df.01M539$Present / df.01M539$Enrolled
attendance <- ts(df.01M539$PercentageOfAttendance, start=1, end=178, frequency=5)
plot(attendance)

# Dibujamos los componentes de la serie
plot(decompose(attendance)) 

##-- ACF y PACF
par(mfrow=c(1,2))
acf(attendance, ylim=c(-1,1), lagmax = 40)
pacf(attendance, ylim=c(-1,1), lagmax = 40)

##-- Ajustamos SARIMA
sarima <- arima(df.01M539,
                         order    = c(3,1,3), 
                         seasonal = list(order = c(1,1,1), 
                                         period = 12), include.mean = FALSE)
