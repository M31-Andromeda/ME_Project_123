
paquetes <- c("mice", "tidyverse", "corrplot", "ranger", "plotly", "gplots", "rstatix", "MASS", "rcompanion", "car", "forecast")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

#!!CAMBIA A TU UBI DE DATA-SET!!
df2 <- read.csv("C:/Users/macar/Documents/ME/student_learning_interaction_dataset.csv", na.strings = c("", "NA"))
