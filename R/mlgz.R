
paquetes <- c("mice", "tidyverse", "corrplot", "ranger", "plotly", "gplots", "rstatix", "MASS", "rcompanion", "car", "forecast")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df_final <- readRDS("data/processed/cleaned.rds")

#=========================================
#Ajustamos los modelos
#=========================================

                  

# Modelo nulo 
m_num_null <- glm(exam_score ~ 1, family = gaussian(link = "identity"), data   = df_final)

# Modelo Gaussiano (link identidad)
m_num_gauss <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                     assignment_completion_rate + time_management_score +
                     family_income + exam_difficulty,
                   family = gaussian(link = "identity"),
                   data   = df_final)

# Modelo Gamma (link log)

#La familia Gamma solo esta definida para valores estrictamente superiores a 0
df_final$exam_score_gamma <- ifelse(df_final$exam_score <= 0,
                                    0.001,
                                    df_final$exam_score)
m_num_gamma <- glm(exam_score_gamma ~ previous_exam_score + attendance_percentage +
                     assignment_completion_rate + time_management_score +
                     family_income + exam_difficulty,
                   family = Gamma(link = "log"),
                   data   = df_final)


# --- Evaluación de los modelos -----------------------------------------
# Comparación por AIC ## no usamos BIC ya que buscamos predicción, no explicabilidad
AIC(m_num_gauss, m_num_gamma)

# Pseudo-R2 de la devianza: 1 - Residual Deviance / Null Deviance
pseudo_r2 <- function(m) 1 - m$deviance / m$null.deviance
pseudo_r2(m_num_gauss)
pseudo_r2(m_num_gamma)

# Test F: dignificancia dels models
anova(m_num_null, m_num_gauss, test = "F")

# --- Validación gráfica: residuos de Pearson vs predictor lineal (eta) -------
par(mfrow = c(1, 2))

plot(predict(m_num_gauss, type = "link"),
     residuals(m_num_gauss, type = "pearson"),
     xlab = expression(hat(eta)), ylab = "Residuos de Pearson",
     main = "Gaussiano (identidad)")
abline(h = 0, lty = 2, col = "red")

plot(predict(m_num_gamma, type = "link"),
     residuals(m_num_gamma, type = "pearson"),
     xlab = expression(hat(eta)), ylab = "Residuos de Pearson",
     main = "Gamma (log)")
abline(h = 0, lty = 2, col = "red")

par(mfrow = c(1, 1))

