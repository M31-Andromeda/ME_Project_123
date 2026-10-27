paquetes <- c("MASS", "mice", "tidyverse", "corrplot", "ranger", "plotly",
              "gplots", "rstatix", "rcompanion", "car", "forecast", "pROC")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df <- read.csv("data/raw/student_exam_performance.csv", na.strings = c("", "NA"))

SEMILLA        <- 123
set.seed(SEMILLA)
exportar_pdf <- TRUE

#============================
#BLOQUE 1 AGRUPAR VARIABLES 
#============================

vars_binarias <- c("private_tuition", "internet_access", "device_availability")

vars_nominales <- c("gender", "school_type", "urban_rural", "study_environment", "study_method")

vars_ordinales_low_high <- c("class_participation", "study_consistency",
                             "motivation_level", "educational_app_usage")

vars_ordinales_otras <- c("notes_quality", "education_level", "parent_education",
                          "family_income", "exam_difficulty", "sleep_quality",
                          "revision_frequency", "break_frequency")

columnas_horas <- c("study_hours_per_day", "self_study_hours", "online_learning_hours",
                    "online_course_hours", "daily_screen_time", "physical_activity_hours", "sleep_hours")

vars_numericas <- c("age", "previous_exam_score", "previous_gpa", "attendance_percentage",
                    "assignment_completion_rate", "study_hours_per_day", "self_study_hours",
                    "online_learning_hours", "practice_tests_completed", "sleep_hours",
                    "daily_screen_time", "physical_activity_hours", "stress_level",
                    "online_course_hours", "exam_preparation_days", "questions_attempted",
                    "questions_correct", "time_management_score", "exam_anxiety_level")

vars_categoricas <- c(vars_binarias, vars_nominales, vars_ordinales_low_high, vars_ordinales_otras)

#========================
#BLOQUE 2 LIMPIEZA DE DATOS 
#========================
df_clean <- df %>%
  dplyr::select(-student_id) %>% # Eliminamos el ID
  mutate(
    # 1. Convertir Nominales y Binarias a factor simple
    across(all_of(c(vars_nominales, vars_binarias)), as.factor),
    
    # 2. Convertir Ordinales agrupadas
    across(all_of(setdiff(vars_ordinales_low_high, "educational_app_usage")), 
           ~ factor(.x, levels = c("Low", "Medium", "High"), ordered = TRUE)),
    educational_app_usage = factor(educational_app_usage, levels = c("Low", "Moderate", "High"), ordered = TRUE),
    
    # 3. Convertir las 9 Ordinales restantes con niveles únicos y específicos
    notes_quality = factor(notes_quality, levels = c("Poor", "Average", "Excellent"), ordered = TRUE),
    education_level = factor(education_level, levels = c("High School", "Undergraduate", "Postgraduate"), ordered = TRUE),
    parent_education = factor(parent_education, levels = c("High School", "Associate", "Bachelor", "Master", "Doctorate"), ordered = TRUE),
    family_income = factor(family_income, levels = c("Low", "Lower-Middle", "Middle", "Upper-Middle", "High"), ordered = TRUE),
    exam_difficulty = factor(exam_difficulty, levels = c("Easy", "Medium", "Hard"), ordered = TRUE),
    sleep_quality = factor(sleep_quality, levels = c("Poor", "Fair", "Good", "Excellent"), ordered = TRUE),
    revision_frequency = factor(revision_frequency, levels = c("Rarely", "Weekly", "Daily"), ordered = TRUE),
    break_frequency = factor(break_frequency, levels = c("Rarely", "Occasionally", "Frequently"), ordered = TRUE),
    performance_grade = factor(performance_grade, levels = c("F", "D", "C", "B", "A"), ordered = TRUE),
    pass_status = as.factor(pass_status)
  )

#====================================
#BLOQUE 3 LIMPIEZA DE VALORES IMPOSIBLES 
#====================================
df_clean <- df_clean %>%
  mutate(
    # Errores de tiempo (horas): Reemplazar directamente por NA
    across(all_of(columnas_horas), ~ if_else(.x > 24, NA_real_, .x)),
    sleep_hours = if_else(sleep_hours == 0, NA_real_, sleep_hours),
    
    # Errores de exámenes: Reemplazar directamente por NA
    questions_correct = if_else(questions_correct > questions_attempted, NA_real_, questions_correct)
  )

##MODIFICACIÓN------ quitamos gente con un 0 en la nota, (son solo 6 personas, ya que la Gamma, no puede predecir con 0)
num_ceros <- sum(df_clean$exam_score <= 0)
cat("Número de valores <= 0 en exam_score:", num_ceros, "\n")

df_clean <- subset(df_clean, exam_score > 0)
##MODIFICAciÓN

#====================================
#BLOQUE 4 IMPUTACION POR MICE 
#====================================

df_clean <- droplevels(df_clean)

df_imputado <- complete(mice(df_clean, m = 1, maxit = 5, seed = SEMILLA))

#=========================================================
#FASE 1 Análisis Descriptivo y Gráfico Individual (Univariado)
#=========================================================
df_final <- df_imputado

cat("Generando gráficos univariados numéricos")
if(exportar_pdf)  pdf("output/Fase1_Histogramas_Numericas.pdf", width = 8, height = 6)
for (var in vars_numericas) {
  p <- ggplot(df_final, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = paste("Distribución de", var), x = var, y = "Densidad")
  print(p)
}
if(exportar_pdf) dev.off()

cat("Generando Box-plots univariados")
if(exportar_pdf) pdf("output/Fase1_Boxplots_Numericas.pdf", width = 8, height = 6)
for (var in vars_numericas) {
  p <- ggplot(df_final, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightgreen", color = "darkgreen", outlier.color = "red", outlier.size = 2, outlier.alpha = 0.6) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(title = paste("Box-plot de", var), y = var, x = "")
  print(p)
}
if(exportar_pdf) dev.off()

cat("Generando gráficos univariados categóricos")
if(exportar_pdf) pdf("output/Fase1_Bar_plots_Categoricas.pdf", width = 8, height = 6) # Activar para exportar
for (var in vars_categoricas) {
  p <- ggplot(df_final, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar(color = "black", alpha = 0.8) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(title = paste("Frecuencia de", var), x = var, y = "Conteo")
  print(p)
}
if(exportar_pdf) dev.off()

#===========================
# FASE 2 DEPURACIÓN DE REDUNDANCIAS 
#=============================
cat("Generando Heatmap Numérico (Pearson)")

matriz_num <- cor(df_final %>% dplyr::select(all_of(c(vars_numericas, "exam_score"))), 
                  use = "complete.obs", method = "pearson")

if(exportar_pdf) pdf("output/Fase2_Heatmap_Numerico.pdf", width = 12, height = 12)
corrplot(matriz_num, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.6, title = "Correlación Pearson (Predictoras + Target)", 
         mar = c(0,0,2,0), diag = TRUE)
if(exportar_pdf) dev.off()

cat("Generando Matriz de Asociación Categórica (V de Cramér)")

cramerVMatrix <- function(data) {
  v <- names(data)
  m <- matrix(1, length(v), length(v), dimnames = list(v, v))
  for (i in seq_along(v)) for (j in seq_along(v)) if (i < j)
    m[i, j] <- m[j, i] <- suppressWarnings(
      rstatix::cramer_v(table(data[[i]], data[[j]]), correct = FALSE))
  m
}

matriz_cramer <- cramerVMatrix(df_final %>% dplyr::select(all_of(vars_categoricas)))

if(exportar_pdf) pdf("output/Fase2_Heatmap_CramerV.pdf", width = 12, height = 12)
corrplot(matriz_cramer, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.6, 
         is.corr = FALSE,          # Indicamos que no es Pearson
         col.lim = c(0, 1),        # Cramér va de 0 a 1
         title = "Asociación V de Cramér (Predictoras Categóricas)", 
         mar = c(0,0,2,0), diag = TRUE)
if(exportar_pdf) dev.off()

#=============================================
#FASE 3 ANÁLISIS BIVARIADO E INFERENCIA ESTADÍSTICA
#=============================================
 
cat("BLOQUE A.1: Numérico vs exam_score (Scatterplots + Pearson)")

if(exportar_pdf) pdf("output/Fase3_exam_score_Scatterplots.pdf", width = 8, height = 6)
for (var in vars_numericas) {
  
  cor_val <- cor(df_final[[var]], df_final$exam_score, use = "complete.obs")
  
  # Gráfico de Dispersión con Regresión Lineal
  p <- ggplot(df_final, aes(x = .data[[var]], y = exam_score)) +
    geom_point(alpha = 0.05, color = "steelblue") + 
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    theme_minimal() +
    labs(title = paste("Dispersión:", var, "vs exam_score\nCorrelación (r) =", round(cor_val, 3)),
         x = var, y = "Exam Score")
  
  print(p)
}
dev.off()

cat("BLOQUE A.2: Categórico vs exam_score (Boxplots + ANOVA + Post-Hoc)")

if(exportar_pdf) pdf("output/Fase3_exam_score_Boxplots_ANOVA.pdf", width = 10, height = 6)
for (var in vars_categoricas) {
  # Prueba ANOVA
  formula_anova <- as.formula(paste("exam_score ~", var))
  modelo_aov <- aov(formula_anova, data = df_final)
  resumen_aov <- summary(modelo_aov)
  p_valor <- resumen_aov[[1]][["Pr(>F)"]][1]
  
  # Filtro y Post-Hoc
  if (!is.na(p_valor) && p_valor < 0.001) {
    cat("\n[ANOVA SIGNIFICATIVO] exam_score ~", var, "(p < 0.001)\n")
    print(resumen_aov)
    
    cat("  -> Ejecutando Post-Hoc...\n") 
    print(TukeyHSD(modelo_aov))
  }
  
  # Box_plot
  p <- ggplot(df_final, aes(x = .data[[var]], y = exam_score, fill = .data[[var]])) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(title = paste("Boxplot:", var, "vs exam_score"), x = var, y = "Exam Score")
  
  print(p)
}
if(exportar_pdf) dev.off()


# ------------------------------------------------------------------------------
# BLOQUE B: PREDICTORES vs PASS_STATUS (Target Binario)
# ------------------------------------------------------------------------------
cat("BLOQUE B.1: Numérico vs pass_status (Boxplots + T-Student)")

if(exportar_pdf) pdf("output/Fase3.2_pass_status_Boxplots_TTest.pdf", width = 8, height = 6)
for (var in vars_numericas) {
  # Prueba T de Student
  formula_t <- as.formula(paste(var, "~ pass_status"))
  test_t <- t.test(formula_t, data = df_final)
  
  if (!is.na(test_t$p.value) && test_t$p.value < 0.001) {
    cat("\n[T-TEST SIGNIFICATIVO]", var, "vs pass_status (p < 0.001)\n")
    print(test_t)
  }
  
  # Box-plot
  p <- ggplot(df_final, aes(x = pass_status, y = .data[[var]], fill = pass_status)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
    theme_minimal() +
    scale_fill_manual(values = c("Fail" = "salmon", "Pass" = "lightgreen")) +
    labs(title = paste("Boxplot:", var, "según pass_status"), x = "Pass Status", y = var)
  
  print(p)
}
if(exportar_pdf) dev.off()

cat("BLOQUE B.2: Categórico vs pass_status (Mosaic Plots + Chi-Cuadrado)")

if(exportar_pdf) pdf("output/Fase3.2_pass_status_MosaicPlots.pdf", width = 10, height = 8)
for (var in vars_categoricas) {
  # Tabla de Contingencia y Prueba Chi-Cuadrado
  tabla_cont <- table(df_final[[var]], df_final$pass_status)
  test_chi <- suppressWarnings(chisq.test(tabla_cont))
  
  if (!is.na(test_chi$p.value) && test_chi$p.value < 0.001) {
    cat("\n[CHI-CUADRADO SIGNIFICATIVO]", var, "vs pass_status (p < 0.001)\n")
    print(test_chi)
  }
  
  # Mosaic-plot
  mosaicplot(tabla_cont, 
             main = paste("Mosaic Plot:", var, "vs pass_status"), 
             color = c("salmon", "lightgreen"), 
             las = 2, 
             xlab = var, ylab = "Pass Status")
}
if(exportar_pdf) dev.off()

# =======================================================================
# FASE 4: DETECCIÓN DE VARIABLES CONFUSORAS Y COLINEALIDAD MULTIVARIANTE
# =======================================================================
cat("BLOQUE 4: Cálculo de VIF ")

todas_las_predictoras <- c(vars_numericas, vars_categoricas)

todas_las_predictoras <- setdiff(todas_las_predictoras,
                                 c("exam_score", "pass_status",
                                   "performance_grade", "performance_level"))

formula_sombra <- reformulate(todas_las_predictoras, response = "exam_score")
modelo_sombra  <- lm(formula_sombra, data = df_final)

#Calculamos el VIF
vif_resultados <- vif(modelo_sombra)

# Formatear salida para leerla fácilmente
if(is.matrix(vif_resultados)) {
  vif_valores <- vif_resultados[, 1]
} else {
  vif_valores <- vif_resultados
}

cat("\nVariables con VIF Alto (Riesgo de Multicolinealidad > 5):\n")
vif_altos <- vif_valores[vif_valores > 5]
if(length(vif_altos) > 0) {
  print(vif_altos)
  cat("\n ADVERTENCIA: Hay colinealidad. Deberemos evaluar estas variables.\n")
} else {
  cat("Ninguna variable supera el umbral de VIF=5. No hay colinealidad grave.\n")
}

cat("\nTop 10 variables con mayor VIF:\n")
print(head(sort(vif_valores, decreasing = TRUE), 10))


# ==================================
# FASE 5: ANÁLISIS DE INTERACCIONES 
# ==================================

# Bloque A: Categórica x Categórica
pares_cat_cat <- list(
  c("family_income", "private_tuition"),
  c("urban_rural", "internet_access"),
  c("study_environment", "study_method"),
  c("motivation_level", "exam_difficulty"),
  c("school_type", "class_participation"),
  c("device_availability", "educational_app_usage")
)

# Bloque B: Numérica x Categórica
pares_num_cat <- list(
  c("study_hours_per_day", "sleep_quality"),
  c("previous_exam_score", "private_tuition"),
  c("online_learning_hours", "internet_access"),
  c("stress_level", "motivation_level"),
  c("attendance_percentage", "school_type"),
  c("practice_tests_completed", "revision_frequency"),
  c("daily_screen_time", "study_environment"),
  c("exam_anxiety_level", "gender"),
  c("physical_activity_hours", "break_frequency")
)

# -------------------------------------------------------
# 5.2 ESCENARIO A: Categórica x Categórica (ANOVA 2 Vías)
# -------------------------------------------------------
cat(" BLOQUE 5.2: Categórica x Categórica ")

if(exportar_pdf) pdf("output/Fase5_A_Cat_vs_Cat_Interaction.pdf", width = 10, height = 7)

for (par in pares_cat_cat) {
  cat1 <- par[1]
  cat2 <- par[2]
  
  # Modelo ANOVA de doble vía
  formula_int <- as.formula(paste("exam_score ~", cat1, "*", cat2))
  modelo_aov <- aov(formula_int, data = df_final)
  resumen_aov <- summary(modelo_aov)
  
  # Extraemos el p-valor de la interacción para el filtro (sigue en la 3ra fila)
  p_int <- resumen_aov[[1]][["Pr(>F)"]][3]
  
  # Filtro: Imprime el summary COMPLETO solo si la interacción es < 0.05
  if (!is.na(p_int) && p_int < 0.05) {
    cat("\n[INTERACCIÓN SIGNIFICATIVA]", cat1, "x", cat2, "\n")
    print(resumen_aov) 
  }
  
  # Gráfico
  p <- ggplot(df_final, aes(x = .data[[cat1]], y = exam_score, group = .data[[cat2]], color = .data[[cat2]])) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(title = paste("Interacción:", cat1, "x", cat2),
         subtitle = paste("P-valor ANOVA interacción:", format.pval(p_int, digits = 3)),
         x = cat1, y = "Media Exam Score", color = cat2)
  print(p)
}
if(exportar_pdf) dev.off()

# ------------------------------------------------------------------------------
# 5.3 ESCENARIO B: Numérica x Categórica 
# ------------------------------------------------------------------------------
cat("BLOQUE 5.3: Numérica x Categórica")

if(exportar_pdf) pdf("output/Fase5_Num_vs_Cat.pdf", width = 10, height = 7)

for (par in pares_num_cat) {
  num_var <- par[1]
  cat_var <- par[2]
  
  # Modelo  (ANOVA doble vía)
  formula_mod <- as.formula(paste("exam_score ~", num_var, "*", cat_var))
  modelo_aov <- aov(formula_mod, data = df_final)
  resumen_aov <- summary(modelo_aov)
  
  p_int <- resumen_aov[[1]][["Pr(>F)"]][3]
  
  # Filtro e impresión completa
  if (!is.na(p_int) && p_int < 0.05) {
    cat("\n[MODERACIÓN SIGNIFICATIVA]", num_var, "x", cat_var, "\n")
    print(resumen_aov)
  }
  
  # Gráfico
  p <- ggplot(df_final, aes(x = .data[[num_var]], y = exam_score, color = .data[[cat_var]])) +
    geom_point(alpha = 0.03) + 
    geom_smooth(method = "lm", se = FALSE, size = 1.2) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(title = paste("Efecto Moderador:", num_var, "según", cat_var),
         subtitle = paste("P-valor interacción:", format.pval(p_int, digits = 3)),
         x = num_var, y = "Exam Score", color = cat_var)
  print(p)
}
if(exportar_pdf) dev.off()

# Guardar versión para trabajar en R 
saveRDS(df_final, file = "data/processed/cleaned.rds")
