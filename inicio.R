## descarga y lectura de datos

load(file = "datos.RData")

x <- round(100*sum(deter_zona[,3])/sum(deter_zona[,2]),1)

color_posacum_zona <- case_when(
  x <= 10 ~ "green",
  x > 10 & x <= 30 ~ "orange",
  x > 30 ~ "red")


## mapa zona

load(file = "mapa.Rdata")

sf::st_crs(mapa_parti) <- 4326

mapa_parti <- mapa_parti %>% 
  left_join(acum_zona, by = c("NAME_2"="partido")) %>% 
  replace_na(replace = list(pos = 0))  %>% 
  rename(Partido = "NAME_2",
         Casos = "pos",
         Laboratorio  = "lab",
         Epidemiologia = "epi",
         Activos = "act",
         Sospechosos = "sosp",
         Fallecidos  = "falle")

## ver si conviene calcular inc y mort en auto_data.R

datos_mapa <- mapa_parti

st_geometry(datos_mapa) <- NULL
