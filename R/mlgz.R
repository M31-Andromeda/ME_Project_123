paquetes <- c("MASS", "mice", "tidyverse", "corrplot", "ranger", "plotly",
              "gplots", "rstatix", "rcompanion", "car", "forecast", "pROC")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df_final <- readRDS("data/processed/cleaned.rds")
stopifnot(!anyNA(df_final), all(df_final$exam_score > 0))

#=========================================
#Ajustamos los modelos
#=========================================

# Modelo Gaussiano (link identidad)
m_num_gauss <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                     assignment_completion_rate + time_management_score +
                     family_income + exam_difficulty,
                   family = gaussian(link = "identity"),
                   data   = df_final)

# Modelo nulo Gaussiano (link identidad)
m_num_null <- glm(exam_score ~ 1, family = gaussian(link = "identity"), data   = df_final)



# Modelo Gamma (link log)
m_num_gamma <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                     assignment_completion_rate + time_management_score +
                     family_income + exam_difficulty,
                   family = Gamma(link = "log"),
                   data   = df_final)
# Modelo nulo Gamma (link log)
m_num_null_gamma <- glm(exam_score ~ 1, family = Gamma(link = "log"), data   = df_final)




# 2. Comparación numérica (AIC/BIC y Pseudo-R2) -----------------------------------
cat("\n--- COMPARACIÓN DE CRITERIOS DE INFORMACIÓN (AIC) ---\n")
print(AIC(m_num_gauss, m_num_gamma))
print(BIC(m_num_gauss, m_num_gamma))

# Pseudo-R2 de la devianza
pseudo_r2 <- function(m) 1 - (m$deviance / m$null.deviance)

cat("\n--- PSEUDO-R2 DE LA DEVIANZA ---\n")
cat("Pseudo-R2 Gaussiano:", round(pseudo_r2(m_num_gauss), 4), "\n")
cat("Pseudo-R2 Gamma:    ", round(pseudo_r2(m_num_gamma), 4), "\n")

# 3. Test F de significación global respecto al modelo nulo --------------------
cat("\n--- TEST F GLOBAL (GAUSSIANO VS NULO) ---\n")
print(anova(m_num_null, m_num_gauss, test = "F"))

cat("\n--- TEST F GLOBAL (GAMMA VS NULO) ---\n")
print(anova(m_num_null_gamma, m_num_gamma, test = "F"))

# 4. Validación gráfica: residuos de Pearson vs predictor lineal (eta) ---------
par(mfrow = c(1, 2))

plot(predict(m_num_gauss, type = "link"),
     residuals(m_num_gauss, type = "pearson"),
     xlab = expression(hat(eta)),
     ylab = "Residuos de Pearson",
     main = "Gaussiano (identidad)",
     col  = alpha("black", 0.3),
     pch  = 16)
abline(h = 0, lty = 2, col = "red", lwd = 2)

plot(predict(m_num_gamma, type = "link"),
     residuals(m_num_gamma, type = "pearson"),
     xlab = expression(hat(eta)),
     ylab = "Residuos de Pearson",
     main = "Gamma (log)",
     col  = alpha("black", 0.3),
     pch  = 16)
abline(h = 0, lty = 2, col = "red", lwd = 2)

par(mfrow = c(1, 1))

summary(m_num_gauss)

# ==============================================================================
# DEPURACIÓN Y SELECCIÓN DEL MODELO GLM NUMÉRICO DEFINITIVO (exam_score)
# ==============================================================================

# 1. Modelo completo
# (Añadimos family_income a pesar de su significatividad p = 0.803 (L) y excluimos variables con VIF > 10)
m_num_full <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                    assignment_completion_rate + time_management_score +
                    family_income + study_hours_per_day + exam_difficulty +
                    practice_tests_completed + revision_frequency,
                  family = gaussian(link = "identity"),
                  data   = df_final)

# 2. drop1 (test F)
drop1_res <- drop1(m_num_full, test = "F")
print(drop1_res)

# 3. Modelo definitivo simplificado (m_num_def)
# (Mantenemos solo las covariables e interacciones con p < 0.05 en drop1)
m_num_def <- glm(exam_score ~ previous_exam_score + attendance_percentage +      
                   time_management_score +
                   study_hours_per_day + exam_difficulty +
                   practice_tests_completed,
                 family = gaussian(link = "identity"),
                 data   = df_final)

# 4. Contraste de modelos anidados (simplificado vs completo)
cat("\n--- CONTRASTE DE MODELOS ANIDADOS (TEST F DE DEVIANZAS) ---\n")
test_f_anidado <- anova(m_num_def, m_num_full, test = "F")
print(test_f_anidado)

# 5. Métricas numéricas de validación del modelo definitivo

summary(m_num_def)
# 6. Validación gráfica: residuos de Pearson vs predictor lineal (eta)
par(mfrow = c(1, 1))
plot(predict(m_num_def, type = "link"),
     residuals(m_num_def, type = "pearson"),
     xlab = expression(hat(eta) ~ "(Predictor Lineal)"),
     ylab = "Residuos de Pearson (r_P)",
     main = "Modelo Gaussiano Definitivo: Residuos de Pearson vs Predictor Lineal",
     col  = alpha("black", 0.2),
     pch  = 16)
abline(h = 0, lty = 2, col = "red", lwd = 2)













