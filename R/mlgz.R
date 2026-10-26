paquetes <- c("tidyverse", "car", "pROC")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df_final <- readRDS("data/processed/cleaned.rds")
stopifnot(!anyNA(df_final), all(df_final$exam_score > 0))

# ==============================================================================
# MODELO LINEAL GENERALIZADO DE RESPUESTA CONTINUA (exam_score)
# ==============================================================================


# 1.ajustamos los modelos-------------------------------------------------------

# Modelo Gaussiano (link identidad)
m_num_gauss <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                     assignment_completion_rate + time_management_score +
                     family_income + study_hours_per_day + exam_difficulty +
                     practice_tests_completed + exam_preparation_days +
                     sleep_hours + stress_level + exam_anxiety_level,
                   family = gaussian(link = "identity"),
                   data   = df_final)

# Modelo nulo Gaussiano (link identidad)
m_num_null <- glm(exam_score ~ 1, family = gaussian(link = "identity"), data   = df_final)



# Modelo Gamma (link log)
m_num_gamma <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                     assignment_completion_rate + time_management_score +
                     family_income + study_hours_per_day + exam_difficulty +
                     practice_tests_completed + exam_preparation_days +
                     sleep_hours + stress_level + exam_anxiety_level,
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

#----------SELECCIÓN Y DEPURACIÓN DEL MODELO DEFINITIVO----------

# 1. Modelo completo
# (Añadimos family_income a pesar de su no significatividad p = 0.803 (L) y excluimos variables con VIF > 10)
m_num_full <- glm(exam_score ~ previous_exam_score + attendance_percentage +
                    assignment_completion_rate + time_management_score +
                    family_income + study_hours_per_day + exam_difficulty +
                    practice_tests_completed + exam_preparation_days +
                    sleep_hours + stress_level + exam_anxiety_level,
                  family = gaussian(link = "identity"),
                  data   = df_final)

# 2. drop1 (test F)
drop1_res <- drop1(m_num_full, test = "F")
print(drop1_res)
cat("Pseudo-R2 full:", round(pseudo_r2(m_num_full), 4), "\n")


# 3. Modelo definitivo simplificado (m_num_def)
m_num_def <- glm(exam_score ~ previous_exam_score + attendance_percentage +      
                   time_management_score +
                   study_hours_per_day + exam_difficulty +
                   practice_tests_completed + exam_preparation_days +
                   sleep_hours + stress_level + exam_anxiety_level,
                 family = gaussian(link = "identity"),
                 data   = df_final)

# 4. Test-F ANOVA, Anidado
cat("\n--- Test-F ANOVA, Anidado ---\n")
test_f_anidado <- anova(m_num_def, m_num_full, test = "F")
print(test_f_anidado)

# 5. Summary + Pseudo-R²

summary(m_num_def)
cat("\n")
cat("Pseudo-R2 def:", round(pseudo_r2(m_num_def), 4), "\n")
cat("\n")
vif(m_num_def)
cat("\n")
drop2_res <- drop1(m_num_def, test = "F")
print(drop2_res)


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


# ==============================================================================
# MODELO LINEAL GENERALIZADO DE RESPUESTA BINARIA (pass_status)
# ==============================================================================

# 1. Ajuste con los tres enlaces candidatos (Logit, Probit, Cloglog)
m_bin_logit <- glm(pass_status ~ previous_exam_score + attendance_percentage +
                     time_management_score + study_hours_per_day +
                     exam_difficulty + practice_tests_completed +
                     exam_preparation_days + sleep_hours +
                     stress_level + exam_anxiety_level,
                   family = binomial(link = "logit"),
                   data   = df_final)

m_bin_probit <- glm(pass_status ~ previous_exam_score + attendance_percentage +
                      time_management_score + study_hours_per_day +
                      exam_difficulty + practice_tests_completed +
                      exam_preparation_days + sleep_hours +
                      stress_level + exam_anxiety_level,
                    family = binomial(link = "probit"),
                    data   = df_final)

m_bin_clog <- glm(pass_status ~ previous_exam_score + attendance_percentage +
                    time_management_score + study_hours_per_day +
                    exam_difficulty + practice_tests_completed +
                    exam_preparation_days + sleep_hours +
                    stress_level + exam_anxiety_level,
                  family = binomial(link = "cloglog"),
                  data   = df_final)

# 2. AIC y BIC
cat("\n--- COMPARACIÓN DE FUNCIONES DE ENLACE BINARIAS ---\n")
tabla_links <- data.frame(
  Enlace = c("Logit (Canónico)", "Probit", "Cloglog"),
  AIC    = c(AIC(m_bin_logit), AIC(m_bin_probit), AIC(m_bin_clog)),
  BIC    = c(BIC(m_bin_logit), BIC(m_bin_probit), BIC(m_bin_clog))
)
print(tabla_links)

# 3. Test Chi-cuadrado
m_bin_null <- glm(pass_status ~ 1,
                  family = binomial(link = "logit"),
                  data   = df_final)

cat("\n--- TEST CHI-CUADRADO GLOBAL (LOGIT VS NULO) ---\n")
print(anova(m_bin_null, m_bin_logit, test = "Chisq"))


# ----------EVALUACIÓN MARGINAL, ODDS RATIOS Y ROC PARA EL MODELO LOGÍSTICO-----

# 1.(drop1 con test chi-cuadrado)
cat("\n--- EVALUACIÓN MARGINAL CON drop1() (TEST CHI-CUADRADO) ---\n")
print(drop1(m_bin_logit, test = "Chisq"))

# 2. Odds ratios (OR = exp(beta)) con intervalos de confianza del 95%
cat("\n--- ODDS RATIOS (OR) E INTERVALOS DE CONFIANZA DEL 95% ---\n")
or_tabla <- exp(cbind(OR = coef(m_bin_logit), confint.default(m_bin_logit)))
print(round(or_tabla, 4))

# 3. Matriz de confusión (punto de corte p = 0.5)
prob_pred  <- predict(m_bin_logit, type = "response")
clase_pred <- ifelse(prob_pred >= 0.5, 1, 0)

tabla_confusion <- table(Observado = df_final$pass_status,
                         Predicho  = clase_pred)

cat("\n--- MATRIZ DE CONFUSIÓN (Cutoff = 0.5) ---\n")
print(tabla_confusion)

precisión <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
cat("Precisión Global (Accuracy):", round(precisión * 100, 2), "%\n")

# 4. Curva ROC y área bajo la curva (AUC)
roc_obj <- roc(df_final$pass_status, prob_pred)

cat("\n--- ÁREA BAJO LA CURVA ROC (AUC) ---\n")
cat("AUC:", round(auc(roc_obj), 4), "\n")


# Gráfico de la curva ROC
par(mfrow = c(1, 1))
plot(roc_obj,
     main = paste("Curva ROC - Modelo Logístico (AUC =",
                  round(auc(roc_obj), 4), ")"),
     col  = "blue",
     lwd  = 2,
     legacy.axes = TRUE)
grid()

#---GRAFICO DE CALIBRACIÓN POR DECILES DE PROBABILIDAD PREDICHA----------------ç

# 1. Asegurar la conversión numérica correcta del objetivo (1 = Aprobado, 0 = Suspenso)
obs_num <- ifelse(df_final$pass_status %in% c(1, "Pass", "TRUE"), 1, 0)

# 2. Agrupación por deciles de probabilidad predicha
calibracion <- data.frame(prob = prob_pred, obs = obs_num) %>%
  mutate(decil = ntile(prob, 10)) %>%
  group_by(decil) %>%
  summarise(
    predicha  = mean(prob),
    observada = mean(obs),
    n         = n()
  )

# Imprimir la tabla de calibración en consola
cat("\n--- TABLA DE CALIBRACIÓN POR DECILES ---\n")
print(calibracion)

# 3. Gráfico de calibración
par(mfrow = c(1, 1))
plot(calibracion$predicha, calibracion$observada,
     pch  = 16,
     col  = "blue",
     cex  = 1.5,
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Probabilidad predicha (media del decil)",
     ylab = "Proporción observada de aprobados",
     main = "Gráfico de Calibración por Deciles (Modelo Logístico)")

# Diagonal de referencia perfecta (y = x)
abline(0, 1, lty = 2, col = "red", lwd = 2)
grid()


# 1. Comprobación de poda: ¿aportan algo las variables excluidas?
m_bin_full <- update(m_bin_logit, . ~ . + family_income +
                       assignment_completion_rate)

cat("\n--- TEST DE PODA BINARIA (MODELO REDUCIDO VS COMPLETO) ---\n")
print(anova(m_bin_logit, m_bin_full, test = "Chisq"))

# 2. Multicolinealidad
cat("\n--- MULTICOLINEALIDAD (VIF) ---\n")
print(vif(m_bin_logit))

# 3. Sensibilidad y especificidad
sens <- tabla_confusion[2, 2] / sum(tabla_confusion[2, ])
espe <- tabla_confusion[1, 1] / sum(tabla_confusion[1, ])

cat("Sensibilidad (Aprobados bien clasificados):", round(sens, 4), "\n")
cat("Especificidad (Suspensos bien clasificados):", round(espe, 4), "\n")

# Asignación del Modelo Binario Definitivo
m_bin_def <- m_bin_logit


#===========================================
#MODELOS FINALES
#===========================================

# ==============================================================================
#  GRÁFICOS DE RESIDUOS (MODELO BINARIO)
# ==============================================================================
df_residuos <- data.frame(
  Predictor = predict(m_bin_def, type = "link"),
  Residuos = residuals(m_bin_def, type = "deviance")
)

p_residuos <- ggplot(df_residuos, aes(x = Predictor, y = Residuos)) +
  geom_point(color = "#2A9D8F", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "#E63946", se = FALSE, linewidth = 1.2) +
  labs(title = "Residuos de Devianza",
       x = "Predictor Lineal",
       y = "Residuos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_residuos)


# ==============================================================================
# TEST DE YOUDEN Y GRÁFICO DE DENSIDAD
# ==============================================================================
coords_youden <- coords(roc_obj, x = "best", best.method = "youden", 
                        ret = c("threshold", "specificity", "sensitivity"))
print(coords_youden)

clase_pred_youden <- ifelse(prob_pred >= coords_youden$threshold, 1, 0)
table(Observado = df_final$pass_status, Predicho = clase_pred_youden)

df_densidad <- data.frame(
  Probabilidad = prob_pred,
  Clase = as.factor(ifelse(df_final$pass_status %in% c(1, "Pass", "TRUE"), 
                           "Aprobados", "Suspensos"))
)


ggplot(df_densidad, aes(x = Probabilidad, fill = Clase)) +
  geom_density(alpha = 0.7, color = "black") +
  geom_vline(xintercept = coords_youden$threshold, 
             color = "black", linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = coords_youden$threshold - 0.08, y = 1.5, 
           label = round(coords_youden$threshold, 2), 
           color = "black", size = 4.5) +
  labs(title = "Probabilidades por Clase",
       x = "Probabilidad",
       y = "Densidad") +
  scale_fill_manual(values = c("Aprobados" = "#32CD32", "Suspensos" = "#FF6347")) +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# ==============================================================================
# QQ-PLOT (MODELO CONTINUO)
# ==============================================================================
df_qq <- data.frame(Residuos = rstandard(m_num_def))

p_qq <- ggplot(df_qq, aes(sample = Residuos)) +
  stat_qq(color = "#1D3557", alpha = 0.8, size = 2) +
  stat_qq_line(color = "#E63946", linewidth = 1) +
  labs(title = "QQ-Plot Normalidad",
       x = "Cuantiles Teóricos",
       y = "Cuantiles de los Residuos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_qq)


# ==============================================================================
# GRÁFICOS DE VALORES PREDICHOS VS REALES
# ==============================================================================

# 1 Predicción del Modelo Continuo
df_pred_cont <- data.frame(
  Real = df_final$exam_score,
  Predicho = predict(m_num_def, type = "response")
)

p_pred_cont <- ggplot(df_pred_cont, aes(x = Real, y = Predicho)) +
  geom_point(color = "#1D3557", alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "#E63946", linetype = "dashed", linewidth = 1) +
  labs(title = "MGLZ Continuo",
       x = "Puntuación Real",
       y = "Puntuación Predicha") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_pred_cont)


# 2 Predicción del Modelo Binario
df_pred_bin <- data.frame(
  Probabilidad = prob_pred,
  Estado = as.factor(ifelse(df_final$pass_status %in% c(1, "Pass", "TRUE"), 
                            "Aprobados", "Suspensos"))
)

p_pred_bin <- ggplot(df_pred_bin, aes(x = Estado, y = Probabilidad, fill = Estado)) +
  geom_boxplot(alpha = 0.8, color = "black", outlier.color = "gray30", outlier.alpha = 0.6) +
  geom_hline(yintercept = coords_youden$threshold, 
             color = "black", linetype = "dashed", linewidth = 0.6) +
  labs(title = "Clasificación Binaria",
       x = "Estado Real",
       y = "Probabilidad Predicha") +
  scale_fill_manual(values = c("Aprobados" = "#32CD32", "Suspensos" = "#FF6347")) +
  theme_minimal() +
  theme(legend.position = "none", # Ocultamos leyenda porque el eje X ya lo explica
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_pred_bin)

