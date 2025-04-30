# Interpolación de alta precisión para vientos
interpolar_viento <- function(tiempos, vmax_nudos) {
  # Convertir a m/s para trabajar en unidades continuas
  vmax_ms <- nudos_a_ms(vmax_nudos)
  
  # 1. Detectar cambios significativos (umbral de 2.572 m/s = 5 nudos)
  cambios <- which(abs(diff(vmax_ms)) > 2.4) # Margen pequeño para errores de redondeo
  
  # 2. Crear segmentos entre puntos de cambio significativo
  segmentos <- unique(c(1, cambios + 1, length(tiempos)))
  
  # 3. Interpolar cada segmento con spline cúbico
  vmax_interp_ms <- numeric(length(tiempos))
  
  for (i in 1:(length(segmentos)-1)) {
    idx <- segmentos[i]:segmentos[i+1]
    
    # Si el segmento tiene suficiente longitud, usar spline
    if (length(idx) > 3) {
      spline_fit <- smooth.spline(tiempos[idx], vmax_ms[idx], nknots = min(4, length(idx)))
      vmax_interp_ms[idx] <- predict(spline_fit, tiempos[idx])$y
    } else {
      # Para segmentos cortos, interpolación lineal
      approx_fun <- approxfun(tiempos[idx], vmax_ms[idx], method = "linear")
      vmax_interp_ms[idx] <- approx_fun(tiempos[idx])
    }
  }
  
  # 4. Aplicar restricción de monotonicidad en segmentos ascendentes/descendentes
  for (i in 1:(length(cambios)+1)) {
    seg_start <- ifelse(i == 1, 1, cambios[i-1]+1)
    seg_end <- ifelse(i == length(cambios)+1, length(tiempos), cambios[i])
    
    # Determinar si el segmento es creciente, decreciente o plano
    trend <- sign(vmax_ms[seg_end] - vmax_ms[seg_start])
    
    if (trend > 0) {
      # Forzar interpolación monótona creciente
      vmax_interp_ms[seg_start:seg_end] <- cummax(vmax_interp_ms[seg_start:seg_end])
    } else if (trend < 0) {
      # Forzar interpolación monótona decreciente
      vmax_interp_ms[seg_start:seg_end] <- cummin(vmax_interp_ms[seg_start:seg_end])
    }
  }
  
  # 5. Convertir de vuelta a nudos y redondear a 1 decimal (0.1 nudos de precisión)
  vmax_interp_nudos <- round(ms_a_nudos(vmax_interp_ms), 1)
  
  return(vmax_interp_nudos)
}
