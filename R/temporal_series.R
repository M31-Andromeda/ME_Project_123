
paquetes <- c("mice", "tidyverse", "corrplot", "ranger", "plotly", "gplots", "rstatix", "MASS", "rcompanion", "car", "forecast")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df2 <- read.csv("data/raw/student_exam_performance.csv")
