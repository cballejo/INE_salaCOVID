## descarga de dropbox y lectura de datos

# fija
response <- GET(url = "https://www.dropbox.com/s/5ofemtvkg3zlcu0/datos.RData?dl=1")

load(rawConnection(response$content))

rm(response)

#token <- readRDS("token.rds")

# loadData <- function(){
# response <- GET(url = "https://www.dropbox.com/s/5ofemtvkg3zlcu0/datos.RData?dl=1")
# 
# load(rawConnection(response$content))
# 
# rm(response)
# }

###

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


datos_mapa <- mapa_parti

st_geometry(datos_mapa) <- NULL


# GnYlRd <- function(x) rgb(colorRamp(c("#63be7b", "#ffeb84", "#f87274"))(x), maxColorValue = 255)
# 
# 
# falle_tabla <- fallecidos %>% rename(Partido = "residencia_departamento_nombre") %>% 
#   mutate(Mes = lubridate::month(fecha_fallecimiento)) %>% 
#   count(Partido, Mes) %>% 
#   tidyr::complete(Partido, Mes, fill = list(NA)) %>% 
#   replace_na(list(n = 0)) %>% 
#   arrange(Mes) %>% 
#   group_by(Partido) %>% 
#   mutate(mort_acum = cumsum(n)) %>% 
#   left_join(pob) %>%
#   mutate(n = round(mort_acum/Poblacion_2020*1000000,2)) %>% 
#   select(-Poblacion_2020, -mort_acum) %>% 
#   pivot_wider(names_from = Mes, values_from = n) %>% 
#   replace_na(replace = list(`3` = 0, `4` = 0, `5` = 0,
#                             `6` = 0, `7` = 0, `8` = 0, `9` = 0, 
#                             `10` = 0, `11` = 0)) %>% arrange(desc(`11`))  
# 
# mfalle <- as.matrix(falle_tabla[,-1])
# 
# rownames(mfalle) <- falle_tabla$Partido