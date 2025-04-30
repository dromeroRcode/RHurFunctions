# Función de interpolación completa
interpolar_trayectoria <- function(ciclone, res_horas = 1) {
  df <- ciclone %>%
    st_drop_geometry() %>%
    mutate(
      datetime = ymd_hms(paste(fecha, "00:00:00")),
      lon = st_coordinates(ciclone)[,1],
      lat = st_coordinates(ciclone)[,2],
      vmax_nudos = ms_a_nudos(vmax) # Convertir a nudos
    ) %>%
    select(datetime, lon, lat, pres, vmax, vmax_nudos)
  
  full_seq <- seq(min(df$datetime), max(df$datetime), by = paste(res_horas, "hour"))
  
  # Interpolar posición con splines cúbicos
  approx_pos <- list(
    lon = splinefun(as.numeric(df$datetime), df$lon, method = "natural"),
    lat = splinefun(as.numeric(df$datetime), df$lat, method = "natural")
  )
  
  # Interpolar presión con spline monótona
  approx_pres <- splinefun(as.numeric(df$datetime), df$pres, method = "monoH.FC")
  
  # Interpolar viento con método mejorado
  vmax_interp <- interpolar_viento_preciso(as.numeric(df$datetime), df$vmax_nudos)
  
  # Ajustar a la secuencia temporal completa
  vmax_interp_full <- approx(
    x = as.numeric(df$datetime),
    y = vmax_interp,
    xout = as.numeric(full_seq),
    method = "linear"
  )$y
  
  # Crear dataframe interpolado
  interpolado <- tibble(datetime = full_seq) %>%
    mutate(
      lon = approx_pos$lon(as.numeric(datetime)),
      lat = approx_pos$lat(as.numeric(datetime)),
      pres = approx_pres(as.numeric(datetime)),
      vmax_nudos = vmax_interp_full,
      vmax = nudos_a_ms(vmax_nudos), # Convertir a m/s
      fecha = as.Date(datetime),
      hora = hour(datetime)
    )
  
  st_as_sf(interpolado, coords = c("lon", "lat"), crs = 4326)
}
