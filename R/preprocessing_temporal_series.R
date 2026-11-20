paquetes <- c("MASS", "mice", "tidyverse", "corrplot", "ranger", "plotly",
              "gplots", "rstatix", "rcompanion", "car", "forecast", "pROC",
              "ggplot2")
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
ggplot(df, aes(x=School.DBN, y=Enrolled)) + 
  geom_boxplot(fill="lightblue")

# Buscamos la etiqueta de School.DBN con más apariciones y la de mayor rango de fechas
dates <- as.Date(as.character(df$Date), format = "%Y%m%d")
dates_by_school <- split(dates, df$School.DBN)

max_count <- 0
best_range_for_freq <- -Inf
most_freq_labels <- character()

max_range <- -Inf
best_count_for_range <- 0
largest_range_labels <- character()

for (label in names(dates_by_school)) {
  school_dates <- dates_by_school[[label]]
  n <- length(school_dates)
  date_range <- as.numeric(max(school_dates) - min(school_dates))

  if (n > max_count || (n == max_count && date_range > best_range_for_freq)) {
    max_count <- n
    best_range_for_freq <- date_range
    most_freq_labels <- label
  } else if (n == max_count && date_range == best_range_for_freq) {
    most_freq_labels <- c(most_freq_labels, label)
  }

  if (date_range > max_range || (date_range == max_range && n > best_count_for_range)) {
    max_range <- date_range
    best_count_for_range <- n
    largest_range_labels <- label
  } else if (date_range == max_range && n == best_count_for_range) {
    largest_range_labels <- c(largest_range_labels, label)
  }
}

cat("Most frequent School.DBN (tie-break by Date range):", most_freq_labels[1],
    "(count =", max_count, ", range =", best_range_for_freq, "days, ties =", length(most_freq_labels), ")\n")
cat("Largest Date range School.DBN (tie-break by count):", largest_range_labels[1],
    "(range =", max_range, "days, count =", best_count_for_range, ", ties =", length(largest_range_labels), ")\n")

# Entre los empates restantes de cada grupo, elegimos el de mayor promedio de Enrolled
pick_highest_avg_enrolled <- function(labels, enrolled_by_school) {
  avgs <- vapply(labels, function(lbl) mean(enrolled_by_school[[lbl]], na.rm = TRUE), numeric(1))
  best_avg <- max(avgs)
  list(labels = labels[avgs == best_avg], avg = best_avg)
}

enrolled_by_school <- split(df$Enrolled, df$School.DBN)
best_freq_enrolled <- pick_highest_avg_enrolled(most_freq_labels, enrolled_by_school)
best_range_enrolled <- pick_highest_avg_enrolled(largest_range_labels, enrolled_by_school)

cat("Highest avg Enrolled among most frequent ties:",
    paste(best_freq_enrolled$labels, collapse = ", "),
    "(avg Enrolled =", best_freq_enrolled$avg, ")\n")
cat("Highest avg Enrolled among largest range ties:",
    paste(best_range_enrolled$labels, collapse = ", "),
    "(avg Enrolled =", best_range_enrolled$avg, ")\n")

# Analizamos los datos de la escuela seleccionada
df.01M539 <- df[df$School.DBN == "01M539", ]

#   Calculamos el porcentaje de asistencia para incluirlo en el análisis
df.01M539$PercentageOfAttendance = df.01M539$Present / df.01M539$Enrolled

summary(df.01M539)
boxplot(df.01M539$Enrolled)
hist(df.01M539$Enrolled, breaks=30)
hist(df.01M539$PercentageOfAttendance, breaks=60)
