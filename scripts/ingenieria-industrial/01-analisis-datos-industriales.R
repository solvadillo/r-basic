# =============================================================================
# ANÁLISIS DE DATOS PARA INGENIERÍA INDUSTRIAL
# Curso de Introducción al Análisis de Datos con R
# Autor: Basado en el curso de Juan Gabriel Gomila & María Santos
# =============================================================================
#
# Bienvenido, Ingeniero Industrial.
# En este script aprenderás a aplicar análisis de datos en contextos reales de
# ingeniería industrial: control de procesos, análisis de calidad, optimización
# de producción y más.
#
# Estructura del script:
#   1. Estadística descriptiva aplicada a datos de proceso
#   2. Visualización de datos industriales
#   3. Gráficas de control (Control Charts) - SPC
#   4. Capacidad de proceso (Cp y Cpk)
#   5. Correlación y regresión para optimización de procesos
# =============================================================================


# =============================================================================
# SECCIÓN 1: ESTADÍSTICA DESCRIPTIVA APLICADA A DATOS DE PROCESO
# =============================================================================
#
# CONTEXTO REAL:
# Imagina que trabajas en una planta de manufactura que fabrica ejes metálicos.
# La especificación del diámetro es 50 mm ± 0.05 mm.
# Se toman 5 mediciones por hora durante 20 horas de producción (100 datos).
#
# OBJETIVO: Describir el comportamiento del proceso con estadísticas clave.

# Simulamos datos reales de un proceso de torneado (diámetro en mm)
set.seed(42)  # Fijamos la semilla para reproducibilidad

# Proceso ESTABLE (bajo control estadístico)
n_total <- 100
diametros <- rnorm(n_total, mean = 50.00, sd = 0.015)  # sd = 0.015 mm

cat("=== ESTADÍSTICAS DEL PROCESO DE TORNEADO ===\n")

# Medidas de tendencia central
media_diam    <- mean(diametros)
mediana_diam  <- median(diametros)
cat("Media del diámetro:   ", round(media_diam, 4), "mm\n")
cat("Mediana del diámetro: ", round(mediana_diam, 4), "mm\n")

# Medidas de dispersión
sd_diam       <- sd(diametros)
varianza_diam <- var(diametros)
rango_diam    <- diff(range(diametros))
cat("Desviación estándar:  ", round(sd_diam, 4), "mm\n")
cat("Varianza:             ", round(varianza_diam, 6), "mm²\n")
cat("Rango total:          ", round(rango_diam, 4), "mm\n")

# Percentiles — muy útiles para entender la distribución del proceso
percentiles <- quantile(diametros, probs = c(0.25, 0.50, 0.75, 0.90, 0.99))
cat("\nPercentiles del diámetro:\n")
print(round(percentiles, 4))

# Coeficiente de variación (CV) — medida de variabilidad relativa
# Un CV < 1% en manufactura indica un proceso muy consistente
cv <- (sd_diam / media_diam) * 100
cat("\nCoeficiente de Variación: ", round(cv, 4), "%\n")

# Resumen completo con summary()
cat("\nResumen estadístico completo:\n")
print(summary(diametros))


# =============================================================================
# SECCIÓN 2: VISUALIZACIÓN DE DATOS INDUSTRIALES
# =============================================================================
#
# Visualizar los datos es el PRIMER paso en cualquier análisis de datos.
# "Un gráfico vale más que mil números."

# Organizamos los datos en subgrupos de 5 (como se toman en producción)
n_subgrupos <- 20
tam_subgrupo <- 5
datos_matriz <- matrix(diametros, nrow = tam_subgrupo, ncol = n_subgrupos)
# Cada columna = un subgrupo (una hora de producción)

# --- Histograma del proceso ---
hist(diametros,
     main  = "Histograma de Diámetros - Proceso de Torneado",
     xlab  = "Diámetro (mm)",
     ylab  = "Frecuencia",
     col   = "steelblue",
     border = "white",
     breaks = 15)
# Líneas de especificación
abline(v = 50.05, col = "red",   lwd = 2, lty = 2)   # Límite Superior Especificación (LSE)
abline(v = 49.95, col = "red",   lwd = 2, lty = 2)   # Límite Inferior Especificación (LIE)
abline(v = 50.00, col = "green", lwd = 2, lty = 1)   # Valor nominal
legend("topright",
       legend = c("LSE / LIE (±0.05 mm)", "Nominal (50 mm)"),
       col    = c("red", "green"),
       lty    = c(2, 1),
       lwd    = 2)

# INTERPRETACIÓN:
# Si la distribución está centrada en el nominal y casi toda la campana
# queda DENTRO de los límites de especificación, el proceso es capaz.

# --- Diagrama de caja (Boxplot) ---
# Muy útil para detectar valores atípicos (outliers) en datos de proceso
boxplot(diametros,
        main   = "Boxplot - Diámetros del Proceso de Torneado",
        ylab   = "Diámetro (mm)",
        col    = "lightblue",
        horizontal = FALSE)
abline(h = 50.05, col = "red", lty = 2, lwd = 2)
abline(h = 49.95, col = "red", lty = 2, lwd = 2)

# --- Serie temporal de medias por subgrupo ---
# Permite ver tendencias o cambios en el proceso a lo largo del tiempo
medias_subgrupos <- colMeans(datos_matriz)
plot(1:n_subgrupos, medias_subgrupos,
     type = "b",
     pch  = 19,
     col  = "steelblue",
     main = "Medias por Subgrupo - Serie de Tiempo",
     xlab = "Número de Subgrupo (hora)",
     ylab = "Media del Diámetro (mm)")
abline(h = mean(diametros), col = "blue",  lwd = 2)         # Línea central
abline(h = 50.05,           col = "red",   lwd = 2, lty = 2) # LSE
abline(h = 49.95,           col = "red",   lwd = 2, lty = 2) # LIE
legend("topright",
       legend = c("Media del proceso", "Límites de especificación"),
       col    = c("blue", "red"),
       lty    = c(1, 2),
       lwd    = 2)


# =============================================================================
# SECCIÓN 3: GRÁFICAS DE CONTROL (STATISTICAL PROCESS CONTROL - SPC)
# =============================================================================
#
# Las gráficas de control son una de las herramientas más importantes en
# ingeniería industrial. Permiten distinguir entre:
#   - Variación COMÚN (natural, inherente al proceso)
#   - Variación ESPECIAL (causas asignables, señales de problemas)
#
# Las más usadas son:
#   - Gráfica X-barra (medias): monitorea el centrado del proceso
#   - Gráfica R (rangos): monitorea la dispersión del proceso

# --- Cálculo de estadísticas para las gráficas de control ---
medias_sg  <- colMeans(datos_matriz)           # Media de cada subgrupo
rangos_sg  <- apply(datos_matriz, 2, function(x) diff(range(x)))  # Rango de cada subgrupo

# Constantes de control para n=5 (tablas de Shewhart - estándar industrial)
# Fuente: AIAG Manual de SPC
n  <- tam_subgrupo  # tamaño de subgrupo = 5
A2 <- 0.577         # para n=5: factor para límites de la gráfica X-barra
D3 <- 0             # para n=5: factor límite inferior gráfica R
D4 <- 2.114         # para n=5: factor límite superior gráfica R

# --- Gráfica X-barra ---
X_doble_barra <- mean(medias_sg)    # Gran media (center line)
R_barra       <- mean(rangos_sg)    # Media de rangos (para calcular límites)

UCL_xbarra <- X_doble_barra + A2 * R_barra   # Upper Control Limit
LCL_xbarra <- X_doble_barra - A2 * R_barra   # Lower Control Limit

cat("\n=== GRÁFICA DE CONTROL X-BARRA ===\n")
cat("Gran Media (CL):             ", round(X_doble_barra, 4), "mm\n")
cat("Límite Superior Control (UCL):", round(UCL_xbarra, 4), "mm\n")
cat("Límite Inferior Control (LCL):", round(LCL_xbarra, 4), "mm\n")

# Graficar X-barra
plot(1:n_subgrupos, medias_sg,
     type = "b",
     pch  = 19,
     col  = "steelblue",
     ylim = range(c(medias_sg, UCL_xbarra + 0.005, LCL_xbarra - 0.005)),
     main = "Gráfica de Control X-barra\n(Diámetros - Proceso de Torneado)",
     xlab = "Número de Subgrupo",
     ylab = "Media del Diámetro (mm)")
abline(h = X_doble_barra, col = "blue", lwd = 2)           # Línea central
abline(h = UCL_xbarra,    col = "red",  lwd = 2, lty = 2)  # UCL
abline(h = LCL_xbarra,    col = "red",  lwd = 2, lty = 2)  # LCL
# Resaltar puntos fuera de control
fuera_xbarra <- which(medias_sg > UCL_xbarra | medias_sg < LCL_xbarra)
if (length(fuera_xbarra) > 0) {
  points(fuera_xbarra, medias_sg[fuera_xbarra], col = "red", pch = 17, cex = 1.5)
  cat("⚠ Subgrupos FUERA de control en X-barra:", fuera_xbarra, "\n")
} else {
  cat("✓ Proceso bajo control estadístico (todos los puntos dentro de límites)\n")
}
legend("topright",
       legend = c("Medias", "CL (gran media)", "UCL / LCL (±3σ)"),
       col    = c("steelblue", "blue", "red"),
       lty    = c(1, 1, 2),
       pch    = c(19, NA, NA),
       lwd    = 2)

# --- Gráfica R (Rangos) ---
UCL_R <- D4 * R_barra   # Upper Control Limit para R
LCL_R <- D3 * R_barra   # Lower Control Limit para R (=0 cuando D3=0)

cat("\n=== GRÁFICA DE CONTROL R ===\n")
cat("Media de Rangos (CL): ", round(R_barra, 4), "mm\n")
cat("UCL_R:                ", round(UCL_R, 4),   "mm\n")
cat("LCL_R:                ", round(LCL_R, 4),   "mm\n")

plot(1:n_subgrupos, rangos_sg,
     type = "b",
     pch  = 19,
     col  = "darkorange",
     ylim = c(0, max(rangos_sg, UCL_R) * 1.2),
     main = "Gráfica de Control R (Rangos)\n(Diámetros - Proceso de Torneado)",
     xlab = "Número de Subgrupo",
     ylab = "Rango del Subgrupo (mm)")
abline(h = R_barra, col = "blue", lwd = 2)          # Línea central
abline(h = UCL_R,   col = "red",  lwd = 2, lty = 2) # UCL
abline(h = LCL_R,   col = "red",  lwd = 2, lty = 2) # LCL (=0 para n=5)
fuera_R <- which(rangos_sg > UCL_R | rangos_sg < LCL_R)
if (length(fuera_R) > 0) {
  points(fuera_R, rangos_sg[fuera_R], col = "red", pch = 17, cex = 1.5)
  cat("⚠ Subgrupos FUERA de control en R:", fuera_R, "\n")
} else {
  cat("✓ Variabilidad del proceso bajo control estadístico\n")
}


# =============================================================================
# SECCIÓN 4: CAPACIDAD DE PROCESO (Cp y Cpk)
# =============================================================================
#
# Los índices de capacidad de proceso responden la pregunta clave:
# "¿Es mi proceso capaz de cumplir las especificaciones del cliente?"
#
# Cp  = mide si el proceso CABE dentro de las especificaciones (precisión)
# Cpk = mide si el proceso está CENTRADO además de caber (precisión + exactitud)
#
# Regla de oro en la industria:
#   Cp / Cpk < 1.00 → Proceso NO capaz (produce defectos)
#   Cp / Cpk ≥ 1.33 → Proceso CAPAZ (aceptable en la mayoría de industrias)
#   Cp / Cpk ≥ 1.67 → Proceso MUY CAPAZ (excelente, Six Sigma approach)

# Especificaciones del cliente (del ejemplo anterior)
LSE <- 50.05   # Límite Superior de Especificación
LIE <- 49.95   # Límite Inferior de Especificación
nominal <- 50.00

# Estimación de sigma del proceso a partir de R-barra
# (estimación estándar Shewhart, más robusta que la desviación muestral directa)
d2 <- 2.326  # constante para n=5
sigma_proceso <- R_barra / d2

# Cálculo de índices de capacidad
Cp  <- (LSE - LIE) / (6 * sigma_proceso)
Cpu <- (LSE - mean(diametros)) / (3 * sigma_proceso)  # capacidad superior
Cpl <- (mean(diametros) - LIE) / (3 * sigma_proceso)  # capacidad inferior
Cpk <- min(Cpu, Cpl)                                   # capacidad real

cat("\n=== ANÁLISIS DE CAPACIDAD DE PROCESO ===\n")
cat("Especificaciones:  LIE =", LIE, "| Nominal =", nominal, "| LSE =", LSE, "\n")
cat("Media del proceso: ", round(mean(diametros), 4), "mm\n")
cat("Sigma del proceso: ", round(sigma_proceso, 4),  "mm\n\n")
cat("Cp  =", round(Cp,  3), "—", ifelse(Cp >= 1.33, "Proceso CAPAZ", "Proceso NO capaz"), "\n")
cat("Cpu =", round(Cpu, 3), "\n")
cat("Cpl =", round(Cpl, 3), "\n")
cat("Cpk =", round(Cpk, 3), "—",
    ifelse(Cpk >= 1.33, "Proceso CENTRADO Y CAPAZ",
    ifelse(Cpk >= 1.00, "Proceso marginalmente capaz", "Proceso NO capaz / NO centrado")), "\n")

# --- Estimación del porcentaje de piezas fuera de especificación ---
# Asumiendo distribución normal
prob_fuera <- (pnorm(LIE, mean(diametros), sigma_proceso) +
               (1 - pnorm(LSE, mean(diametros), sigma_proceso))) * 100
cat("\nPiezas fuera de especificación estimadas:", round(prob_fuera, 4), "%\n")
cat("Equivalente a", round(prob_fuera * 10000, 1), "piezas por millón (PPM)\n")

# Gráfica de capacidad
x_seq <- seq(LIE - 0.05, LSE + 0.05, length.out = 300)
y_dens <- dnorm(x_seq, mean(diametros), sigma_proceso)

plot(x_seq, y_dens,
     type = "l",
     lwd  = 2,
     col  = "steelblue",
     main = paste0("Análisis de Capacidad de Proceso\nCp = ", round(Cp, 3),
                   "  |  Cpk = ", round(Cpk, 3)),
     xlab = "Diámetro (mm)",
     ylab = "Densidad")
# Áreas fuera de especificación (rojo)
x_bajo <- x_seq[x_seq <= LIE]
polygon(c(x_bajo, rev(x_bajo)),
        c(dnorm(x_bajo, mean(diametros), sigma_proceso), rep(0, length(x_bajo))),
        col = rgb(1, 0, 0, 0.3), border = NA)
x_alto <- x_seq[x_seq >= LSE]
polygon(c(x_alto, rev(x_alto)),
        c(dnorm(x_alto, mean(diametros), sigma_proceso), rep(0, length(x_alto))),
        col = rgb(1, 0, 0, 0.3), border = NA)
abline(v = LSE,     col = "red",   lwd = 2, lty = 2)
abline(v = LIE,     col = "red",   lwd = 2, lty = 2)
abline(v = nominal, col = "green", lwd = 2, lty = 1)
abline(v = mean(diametros), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Distribución proceso", "Especificaciones (LSE/LIE)",
                  "Nominal", "Media proceso"),
       col    = c("steelblue", "red", "green", "blue"),
       lty    = c(1, 2, 1, 3),
       lwd    = 2)


# =============================================================================
# SECCIÓN 5: CORRELACIÓN Y REGRESIÓN PARA OPTIMIZACIÓN DE PROCESOS
# =============================================================================
#
# CONTEXTO REAL:
# Estás analizando cómo la temperatura de un horno afecta la dureza de
# un material tratado térmicamente. Tienes datos de 30 corridas de producción.
# Temperatura: 780 a 920 °C   |   Dureza: escala HRC (Rockwell C)

set.seed(7)
temperatura <- seq(780, 920, length.out = 30) + rnorm(30, 0, 5)
# Relación real: dureza aumenta linealmente con temperatura (con ruido)
dureza <- 25 + 0.12 * temperatura + rnorm(30, 0, 3)

datos_proceso <- data.frame(temperatura = round(temperatura, 1),
                            dureza       = round(dureza, 1))

cat("\n=== ANÁLISIS DE CORRELACIÓN Y REGRESIÓN ===\n")
cat("Primeras 6 observaciones:\n")
print(head(datos_proceso))

# --- Coeficiente de correlación de Pearson ---
r_pearson <- cor(datos_proceso$temperatura, datos_proceso$dureza)
cat("\nCoeficiente de correlación de Pearson (r):", round(r_pearson, 4), "\n")
cat("Interpretación:")
if (abs(r_pearson) >= 0.9) {
  cat(" Correlación muy fuerte\n")
} else if (abs(r_pearson) >= 0.7) {
  cat(" Correlación fuerte\n")
} else if (abs(r_pearson) >= 0.5) {
  cat(" Correlación moderada\n")
} else {
  cat(" Correlación débil\n")
}

# --- Modelo de regresión lineal ---
modelo <- lm(dureza ~ temperatura, data = datos_proceso)
cat("\nCoeficientes del modelo de regresión:\n")
print(round(coef(modelo), 4))

r_cuadrado <- summary(modelo)$r.squared
cat("\nCoeficiente de determinación R² =", round(r_cuadrado, 4), "\n")
cat("El modelo explica el", round(r_cuadrado * 100, 2), "% de la variabilidad en la dureza.\n")

# --- Gráfica de dispersión con recta de regresión ---
plot(datos_proceso$temperatura, datos_proceso$dureza,
     pch  = 19,
     col  = "steelblue",
     main = paste0("Temperatura vs Dureza - Regresión Lineal\nR² = ",
                   round(r_cuadrado, 4)),
     xlab = "Temperatura del Horno (°C)",
     ylab = "Dureza (HRC)")
abline(modelo, col = "red", lwd = 2)
legend("topleft",
       legend = c("Observaciones", paste0("Regresión: Dureza = ",
                  round(coef(modelo)[1], 2), " + ",
                  round(coef(modelo)[2], 4), " × Temp")),
       col    = c("steelblue", "red"),
       pch    = c(19, NA),
       lty    = c(NA, 1),
       lwd    = 2)

# --- Predicción con el modelo ---
cat("\n--- Predicciones con el modelo ---\n")
nuevas_temps <- data.frame(temperatura = c(800, 850, 900))
predicciones <- predict(modelo, newdata = nuevas_temps,
                        interval = "prediction", level = 0.95)
resultado_pred <- cbind(nuevas_temps, round(predicciones, 2))
names(resultado_pred) <- c("Temperatura (°C)", "Dureza Predicha (HRC)",
                           "LI 95%", "LS 95%")
print(resultado_pred)

cat("\n--- Diagnóstico del modelo ---\n")
cat("Los residuos deben tener media ≈ 0 y distribución normal.\n")
cat("Media de residuos:", round(mean(residuals(modelo)), 6), "\n")

# Gráfica de residuos para verificar supuestos
par(mfrow = c(1, 2))
plot(fitted(modelo), residuals(modelo),
     pch  = 19,
     col  = "steelblue",
     main = "Residuos vs Valores Ajustados",
     xlab = "Valores ajustados",
     ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2, lty = 2)

qqnorm(residuals(modelo), col = "steelblue", pch = 19,
       main = "Q-Q Plot de Residuos\n(Verificación de Normalidad)")
qqline(residuals(modelo), col = "red", lwd = 2)
par(mfrow = c(1, 1))

cat("\nSi los residuos en el Q-Q Plot siguen la línea roja,\n")
cat("el supuesto de normalidad del modelo es válido.\n")


# =============================================================================
# RESUMEN FINAL - FLUJO DE TRABAJO EN ANÁLISIS DE DATOS INDUSTRIALES
# =============================================================================
cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║       FLUJO DE TRABAJO - ANÁLISIS DE DATOS INDUSTRIALES      ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ 1. DEFINIR el problema (¿qué quiero saber?)                  ║\n")
cat("║ 2. RECOLECTAR datos representativos del proceso               ║\n")
cat("║ 3. EXPLORAR con estadísticas descriptivas y gráficas          ║\n")
cat("║ 4. ANALIZAR con herramientas de SPC / capacidad / regresión  ║\n")
cat("║ 5. INTERPRETAR resultados en contexto del proceso            ║\n")
cat("║ 6. TOMAR DECISIONES basadas en datos (DMAIC, Lean, Six Sigma)║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
