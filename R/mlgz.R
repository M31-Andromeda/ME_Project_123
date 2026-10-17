paquetes <- c("mice", "tidyverse", "corrplot", "ranger", "plotly", "gplots", "rstatix", "MASS", "rcompanion", "car", "forecast")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df_final <- readRDS("data/processed/cleaned.rds")
