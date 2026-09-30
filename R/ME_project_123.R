
if (!require("mice")) {
  install.packages("mice")
  library(mice)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("gplots")) {
  install.packages("gplots")
  library(gplots)
}
if (!require("corrplot")) {
  install.packages("corrplot")
  library(corrplot)
}
if (!require("rstatix")) {
  install.packages("rstatix")
  library(rstatix)
}
if (!require("MASS")) {
  install.packages("MASS")
  library(MASS)
}

df <- read.csv("C:/Users/santi/Desktop/clases/ME/proyecto/Data-Set/student_exam_performance.csv", na.strings = c("", "NA"))

#============================
#BLOQUE 1 AGRUPAR VARIABLES 
#============================

# (4 Variables Binarias)
vars_binarias <- c("private_tuition", "internet_access", "pass_status", "device_availability")

# (5 Variables Nominales - Sin orden lógico)
vars_nominales <- c("gender", "school_type", "urban_rural", "study_environment", "study_method")

# (5 Variables Ordinales con niveles: Low/Medium/High)
vars_ordinales_low_high <- c("class_participation", "study_consistency", 
                             "motivation_level", "educational_app_usage", "performance_level")

# (Variables dependientes del tiempo para el Bloque 3)
columnas_horas <- c("study_hours_per_day", "self_study_hours", "online_learning_hours",
                    "online_course_hours", "daily_screen_time", "physical_activity_hours", "sleep_hours")
#========================
#BLOQUE 2 LIMPIEZA DE DATOS 
#========================
df_clean <- df %>%
  select(-student_id) %>% # Eliminamos el ID
  mutate(
    # 1. Convertir Nominales y Binarias a factor simple
    across(all_of(c(vars_nominales, vars_binarias)), as.factor),
    
    # 2. Convertir Ordinales agrupadas
    across(all_of(vars_ordinales_low_high), 
           ~ factor(.x, levels = c("Low", "Medium", "High"), ordered = TRUE)),
    
    # 3. Convertir las 9 Ordinales restantes con niveles únicos y específicos
    notes_quality = factor(notes_quality, levels = c("Poor", "Average", "Excellent"), ordered = TRUE),
    education_level = factor(education_level, levels = c("High School", "Undergraduate", "Postgraduate"), ordered = TRUE),
    parent_education = factor(parent_education, levels = c("High School", "Associate", "Bachelor", "Master", "Doctorate"), ordered = TRUE),
    family_income = factor(family_income, levels = c("Low", "Middle", "High"), ordered = TRUE),
    exam_difficulty = factor(exam_difficulty, levels = c("Easy", "Medium", "Hard"), ordered = TRUE),
    sleep_quality = factor(sleep_quality, levels = c("Poor", "Fair", "Excellent"), ordered = TRUE),
    revision_frequency = factor(revision_frequency, levels = c("Rarely", "Weekly", "Daily"), ordered = TRUE),
    break_frequency = factor(break_frequency, levels = c("Rarely", "Occasionally", "Frequently"), ordered = TRUE),
    performance_grade = factor(performance_grade, levels = c("F", "D", "C", "B", "A"), ordered = TRUE)
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

#====================================
#BLOQUE 4 IMPUTACION POR MICE 
#====================================

df_clean <- droplevels(df_clean)

df_imputado <- complete(mice(df_clean, m = 1, maxit = 5))




