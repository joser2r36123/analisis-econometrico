# Instalar y cargar paquetes necesarios
install.packages("wooldridge")  # Solo es necesario la primera vez
library(wooldridge)

# Cargar la base de datos BWGHT (peso de nacimiento y tabaquismo)
data("bwght")

# Mostrar las primeras filas de la base
head(bwght)

# Resumen estadístico
summary_stats <- data.frame(
  Variable = names(bwght),
  Media = sapply(bwght, mean, na.rm = TRUE),
  Desviacion = sapply(bwght, sd, na.rm = TRUE),
  Varianza = sapply(bwght, var, na.rm = TRUE),
  Min = sapply(bwght, min, na.rm = TRUE),
  Max = sapply(bwght, max, na.rm = TRUE)
)
print(summary_stats)

# Matriz de correlación
cor_matrix <- cor(bwght, use = "pairwise.complete.obs")
print(cor_matrix)

# Modelo de regresión lineal simple: Peso de nacimiento vs cigarrillos fumados por la madre
modelo_simple <- lm(bwght ~ cigs, data = bwght)
summary(modelo_simple)

# Agregar dos variables adicionales: Edad de la madre y educación
modelo_multiple <- lm(bwght ~ cigs + mage + faminc, data = bwght)
summary(modelo_multiple)

# Probar con logaritmos (log de ingresos familiares)
modelo_log <- lm(bwght ~ cigs + mage + log(faminc), data = bwght)
summary(modelo_log)

# Guardar los resultados en un archivo de texto
sink("resultados.txt")
print(summary_stats)
print(cor_matrix)
print(summary(modelo_simple))
print(summary(modelo_multiple))
print(summary(modelo_log))
sink()

# Mensaje final
print("Análisis completado. Revisa el archivo resultados.txt para ver los detalles.")
