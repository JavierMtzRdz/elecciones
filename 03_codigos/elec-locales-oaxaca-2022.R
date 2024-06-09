##################################################################
##          Proyecto: Mapas de resultados locales 2022          ##
##################################################################
##
## Descripción:    Este script analiza los resultados electorales de
##                 las elecciones locales de 2022. 
##
## Autor:          Javier Mtz.  
##
## Fecha creac.:   2022-06-05
##
## Email:          javier.mtz.rd@gmail.com
##
## ---------------------------
## Notas:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl,
               readxl, scales, mytidyfunctions, 
               sf, rgdal, leaflet)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

zm <- read_csv("01_datos_brutos/ZM_2015.csv",
               locale = locale(date_names = "es", 
                               encoding = "ISO-8859-1")) %>% 
  clean_names() %>% 
  count(cve_zm, nom_zm, cve_mun, nom_mun) %>% 
  select(-n) %>% 
  full_join(read_csv("https://raw.githubusercontent.com/diegovalle/download-maps12/master/ife.to.inegi.csv") %>% 
              clean_names() %>% 
              transmute(cve_mun = sprintf("%05d", id_inegi),
                        cve_ine = sprintf("%05d", id_ife)),
            by = "cve_mun")

# Oaxaca -----
## Cargar datos ----

### Mapa ----
last_carpeta <- list.files("01_datos_brutos/resultados/oaxaca") 

df_carpeta <- tibble(carpetas = last_carpeta) %>% 
  mutate(hora = substr(last_carpeta,
                                  1, 13),
         hora = str_remove(hora, "_"),
         hora = as.numeric(hora))


carpeta_selec <- df_carpeta %>% 
  filter(hora == max(hora)) %>% 
  pull(carpetas) 

fecha <-  paste0(substr(carpeta_selec, 1, 4),
         "/",
         substr(carpeta_selec, 5, 6),
         "/",
         substr(carpeta_selec, 7, 8)) %>% 
  parse_date(format = "%Y/%m/%d")

hora <- paste0(substr(carpeta_selec, 10, 11),
               ":",
               substr(carpeta_selec, 12, 13),
               " h")


  


candidatos_oax <- read_csv(paste0("01_datos_brutos/resultados/oaxaca/",
                                  carpeta_selec,
                                  "/OAX_GUB_Candidatura_2022.csv")) %>% 
  clean_names() %>% 
  separate_rows(partido_ci,
           sep = "-",
           convert = T) %>% 
  filter(partido_ci != "SUM") %>% 
  group_by(candidatura_propietaria) %>% 
  mutate(partidos_tot = paste(partido_ci, collapse = "-")) %>% 
  select(-id_estado)
  

dataoax <- read_csv(paste0("01_datos_brutos/resultados/oaxaca/",
                           carpeta_selec,
                           "/OAX_GUB_2022.csv"), 
                    skip = 2,
                    n_max = 1) %>% 
  clean_names()

computos_oax <- read_csv(paste0("01_datos_brutos/resultados/oaxaca/",
                                carpeta_selec,
                                "/OAX_GUB_2022.csv"), 
                         skip = 4) %>% 
  clean_names()


computos_oax_sec <- computos_oax %>% 
  group_by(id_estado, estado,
           id_distrito_local, distrito_local,
           seccion) %>% 
  summarise(across(pan:nulos, ~ sum(as.numeric(.x), 
                                    na.rm = T))) %>%
  ungroup() %>% 
  pivot_longer(-c(id_estado, estado,
                  id_distrito_local, distrito_local,
                  seccion),
               names_to = "partido",
               values_to = "votos") %>% 
  mutate(partido = str_to_upper(partido),
         partido_merge = str_remove(partido, "C_"),
         partido_merge = paste0(partido_merge, "_partido"),
         partido_merge = str_extract(partido_merge, "^(.*?)\\_"),
         partido_merge = str_remove_all(partido_merge, "_")) %>% 
  full_join(candidatos_oax,
            by =c("partido_merge" = "partido_ci")) %>% 
  select(-partido_merge) %>% 
  mutate(partidos_tot = ifelse(is.na(partidos_tot),
                               partido, partidos_tot),
         partidos_tot = str_replace_all(partidos_tot, "_", " ")) %>% 
  group_by(id_estado, estado,
           id_distrito_local, distrito_local,
           seccion, candidatura_propietaria, partidos_tot) %>% 
  summarise(votos = sum(votos, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(id_estado, estado,
           id_distrito_local, distrito_local,
           seccion) %>% 
  mutate(total_votos = sum(votos, na.rm = T),
         votos_por = votos/total_votos,
         lugar = rank(-votos, ties.method = "first"),
         ganador = ifelse(min(lugar, na.rm = T) == lugar &
                            votos > 0, 
                          "Ganador", "Perdedor"),
         ventaja = ifelse(ganador == "Ganador",
                          votos_por-lead(votos_por), NA),
         seccion = str_remove(seccion, "'"),
         id_distrito_local = str_remove(id_distrito_local, "'")) %>% 
  arrange(id_estado, estado,
          id_distrito_local, distrito_local,
          seccion,
          lugar) %>% 
  ungroup()


map_mun_oax <- st_read("01_datos_brutos/shapefiles/20/MUNICIPIO.shp") %>%
  clean_names() %>% 
  mutate(cve_ine = paste0(entidad,
                          sprintf("%03d", municipio))) %>% 
  left_join(zm, 
            by = "cve_ine") 

map_sec_oax <- st_read("01_datos_brutos/shapefiles/20_OAXACA/Seccion.shp") %>%
  clean_names() %>%
  st_transform("+init=epsg:4326") %>% 
  mutate(seccion = sprintf("%04d", seccion),
         municipio = sprintf("%03d", municipio),
         entidad = sprintf("%02d", entidad)) %>% 
  left_join(computos_oax_sec %>% 
              # filter(str_detect(partido, "MC") ) %>% 
              filter(ganador == "Ganador"),
            by = c("seccion" = "seccion")) %>% 
  mutate(cve_ine = paste0(entidad,
                          municipio),
         partidos_tot = ifelse(partidos_tot == "NA" |
                                 is.na(partidos_tot),
                               "SIN DATOS", partidos_tot)) %>% 
  left_join(zm, 
            by = "cve_ine") 

map_sec_oax %>%
  st_set_geometry(NULL) %>%
  count(partidos_tot,
        sort = T)

map_sec_oax %>%
  ggplot() +
  geom_sf(aes(fill = partidos_tot,
              alpha = ventaja),
          color = "grey30", size = 0.05) +
  geom_sf(data =  map_mun_oax,
          fill = NA,
          size = 0.1,
          colour = "grey40") +
  coord_sf() +
  theme_void() +
  theme(text = element_text(family = "Lato"), 
        plot.title = element_text(hjust = 0.5, 
                                  
                                  size = 14, face = "bold", color = "grey20"), 
        plot.subtitle = element_text(hjust = 0.5, 
                                     size = 12, color = "gray50"),
        plot.caption = element_text(color = "gray50", 
                                    size = 10,
                                    hjust = 0.1)) +
  theme(legend.margin = margin(0, 6, 0, 6)) +
  scale_alpha_continuous(labels = percent) +
  scale_fill_manual(values =  c("PT-PV-PUP-MOR" = "#874a53",
                                "PRI-PRD" = "#2d5e29",
                                "PVEM" = "#bcd789",
                                "PAN" = "#5877ac",
                                "NULOS" = "#b0b0b0", 
                                "SIN DATOS" = "grey80")) +
  labs(title = "Elección a gobernador de Oaxaca",
       subtitle = paste0("Resultados al corte de las ",
                         hora,
                         " del ",
                         format(fecha, "%d de %B"),
                         "\n Porcentaje de actas contabilizadas: ",
                         round(dataoax$porcentaje_actas_contabilizadas,
                               2), "%"),
       fill = "Ganador",
       alpha = "Ventaja",
       caption = "Elaboración con datos del IEEPCO | @javiermtzrd") 

ggsave(paste0("02_graficas/resultados-oax.png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

ggsave(paste0("02_graficas/resultados-oax.jpg"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

## Por zona metropolitana ----
zonas_met_oax <- unique(na.omit(zm[substr(zm$cve_mun, 1, 2) == "20",]$nom_zm))


zm_map_oax <- map_sec_oax %>% 
  filter(!is.na(nom_zm)) %>% 
  group_by(cve_zm, nom_zm) %>% 
  summarise(seccion = 1)


for (i in zonas_met_oax) {
  
  polig <- st_coordinates(zm_map_oax %>%
                            filter(nom_zm %in% i)) %>%
    data.frame() %>%
    transmute(x_lon = X,
              y_lat = Y)
  
  min_lat <- min(polig$y_lat)
  max_lat <- max(polig$y_lat)
  dif_lat <- max_lat-min_lat
  
  min_lon <- min(polig$x_lon)
  max_lon <- max(polig$x_lon)
  dif_lon <- max_lon-min_lon
  
  if (dif_lat > dif_lon) {
    
    tasa_lon <- (dif_lon/dif_lat)
    
    mult <- ((200/130)/tasa_lon)*dif_lon
    
    
    add_x <- mult - dif_lon
    
    add_y <-  0
    
  } else {
    
    tasa_lat <- (dif_lat/dif_lon)
    
    mult <- ((200/130)/tasa_lat)*dif_lat
    
    
    add_y <- mult - dif_lat
    
    add_x <-  0
    
  }
    
  
  map_sec_oax %>%
    ggplot() +
    geom_sf(aes(fill = partidos_tot,
                alpha = ventaja),
            color = "grey60", size = 0.05) +
    geom_sf(data =  map_mun_oax,
            fill = NA,
            size = 0.1,
            colour = "grey40") +
    geom_sf(data =  zm_map_oax %>%
              filter(nom_zm %in% i),
            fill = NA,
            size = 0.5,
            colour = "grey20") +
    coord_sf(ylim = c(min_lat, 
                      max_lat),
             xlim = c(min_lon - add_x /2, 
                      max_lon + add_x /2)) +
    theme_void() +
    theme(text = element_text(family = "Lato"), 
          plot.title = element_text(hjust = 0.5, 
                                    
                                    size = 14, face = "bold", color = "grey20"), 
          plot.subtitle = element_text(hjust = 0.5, 
                                       size = 12, color = "gray50"),
          plot.caption = element_text(color = "gray50", 
                                      size = 10,
                                      hjust = 0.1)) +
    theme(legend.margin = margin(0, 6, 0, 6)) +
    scale_alpha_continuous(labels = percent) +
    scale_fill_manual(values =  c("PT-PV-PUP-MOR" = "#874a53",
                                  "PRI-PRD" = "#2d5e29",
                                  "PVEM" = "#bcd789",
                                  "PAN" = "#5877ac",
                                  "NULOS" = "#b0b0b0", 
                                  "SIN DATOS" = "grey80")) +
    labs(title = paste0("Resultados por sección de la elección a gobernador de Oaxaca,\n zona metropolitana de ", i),
         subtitle = paste0("Resultados al corte de las ",
                           hora,
                           " del ",
                           format(fecha, "%d de %B"),
                           "\n Porcentaje de actas contabilizadas: ",
                           round(dataoax$porcentaje_actas_contabilizadas,
                                 2), "%"),
         fill = "Ganador",
         alpha = "Ventaja",
         caption = "Elaboración con datos del IEEPCO | @javiermtzrd") 
  
  ggsave(paste0("02_graficas/resultados-",
                make_clean_names(i),
                ".png"),
         bg = "transparent",
         width = 200,                  # Ancho de la gráfica
         height =130,
         units = "mm")
  
  ggsave(paste0("02_graficas/resultados-",
                make_clean_names(i),
                ".jpg"),
         bg = "transparent",
         width = 200,                  # Ancho de la gráfica
         height =130,
         units = "mm")
  
}
