
paquetes <- c("MASS", "mice", "tidyverse", "corrplot", "ranger", "plotly",
              "gplots", "rstatix", "rcompanion", "car", "forecast", "pROC")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df <- read.csv("data/raw/student_daily_attendance.csv")

df01M015 <- df[df$School.DBN == "01M015", ]

presence <- ts(df01M015$Present, start=1, end=178, frequency=5)
plot(presence)

##-- Dibuixem les components de la sèrie
plot(decompose(presence)) 

##-- ACF i PACF
par(mfrow=c(1,2))
acf(presence, ylim=c(-1,1), lagmax = 40)
pacf(presence, ylim=c(-1,1), lagmax = 40)
