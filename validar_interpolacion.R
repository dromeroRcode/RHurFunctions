# Función para validar la interpolación
validar_interpolacion <- function(ciclone) {
  # Interpolar a alta resolución (cada 15 minutos)
  interp_alta <- interpolar_trayectoria(ciclone, res_horas = 0.25)
  
  # Datos originales
  original <- ciclone %>%
    st_drop_geometry() %>%
    mutate(
      datetime = ymd_hms(paste(fecha, "00:00:00")),
      vmax_nudos = ms_a_nudos(vmax),
      tipo = "Original"
    )
  
  # Datos interpolados en los mismos tiempos originales
  interp_original <- interp_alta %>%
    st_drop_geometry() %>%
    filter(datetime %in% original$datetime) %>%
    mutate(tipo = "Interpolado")
  
  # Combinar para comparación
  comparacion <- bind_rows(original, interp_original) %>%
    select(datetime, vmax_nudos, tipo)
  
  # Calcular métricas de error
  error <- interp_original %>%
    left_join(original %>% select(datetime, vmax_orig = vmax_nudos), by = "datetime") %>%
    summarise(
      MAE = mean(abs(vmax_nudos - vmax_orig)),
      RMSE = sqrt(mean((vmax_nudos - vmax_orig)^2)),
      Correlacion = cor(vmax_nudos, vmax_orig)
    )
  
  return(list(
    comparacion = comparacion,
    error = error
  ))
}
