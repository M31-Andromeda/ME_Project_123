paquetes <- c("MASS", "mice", "tidyverse", "corrplot", "ranger", "plotly",
              "gplots", "rstatix", "rcompanion", "car", "forecast", "pROC")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df <- read.csv("data/raw/student_daily_attendance.csv")

exportar_pdf <- FALSE

# Hay unos cuantos outliers por tratar
summary(df)
boxplot(df$Enrolled)
hist(df$Enrolled, breaks=60)
quantile(df$Enrolled,probs=c(0.025,0.975))
