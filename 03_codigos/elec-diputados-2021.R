### -------------------------------------- ###
###     Gráficas resultados elecciones     ###
### -------------------------------------- ###

# Este script realiza las gráficas para analizar los resultados de las elecciones de 2021
# Contacto: javier.martinez@contralacorrupcion.mx

# Setup ----
## Paquetes a utilizar -----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales,stringi,
               highcharter,
               sf, rgdal, leaflet)

pacman::p_load(naniar, hablar)
 
## Funciones importantes ----
mi_tema <- function(...) {
  theme_minimal() +
    theme(axis.line = element_line(size = 0.3),
          plot.title = element_text(hjust=0.5, 
                                    size = 14, face = "bold", 
                                    color = "grey20"),
          plot.subtitle = element_text(hjust=0.5,
                                       size = 12,
                                       color = "gray50"),
          plot.caption =  element_text(color = "gray50",
                                       size=10, 
                                       hjust=0),
          panel.grid = element_line(linetype = 2,
                                    size = 0.3,
                                    color = "gray90"),
          # panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="gray95", 
                                          linetype="blank"),
          panel.border = element_rect(color = "gray95",
                                      fill=NA),
          rect = element_rect(fill = "transparent"),
          ...)
}


{
  partidos <- c("MORENA",
                "PAN",
                "PRI",
                "PVEM",
                "PRD",
                "PT",
                "RSP",
                "MC",
                "PAN-PRD-MC",
                "PAN-PRD",
                "PRI-PVEM-PANAL",
                "FxM",
                "FXM",
                "PES",
                "JHH",
                "MORENA-PT",
                "MORENA-PT-PES",
                "PT-PVEM",
                "VxM",
                "PAN-PRI-PRD",
                "INDEP.",
                "SIN PARTIDO",
                "SP",
                "PANAL",
                "Dato no determinante",
                "NULOS",
                "DATOS NO REGISTRADOS",
                "ELIGE")
  
  show_col(alpha(c("#d66050", 
                   "#2d5e29"), 0.75))
  
  colores_partidos <- c("MORENA" = "#874a53",
                        "PAN" = "#5877ac",
                        "PRI" = "#2d5e29",
                        "PVEM" = "#bcd789",
                        "PRD" = "#F5DA57",
                        "PT" = "#d14457",
                        "RSP" = "#bb4e5f",
                        "MC" = "#e89547",
                        "PAN-PRD-MC" = "#3c88b5", 
                        "PAN-PRD" = "#6dabb7",
                        "PRI-PVEM-PANAL" = "#508d4b", ##
                        "FxM" = "#d68fb9",
                        "FXM" = "#d68fb9",
                        "PES" = "#75529c",
                        "JHH" = "#9b2a24",
                        "MORENA-PT" = "#AC4755",
                        "MORENA-PT-PES" = "#AC4755",
                        "PT-PVEM" = "#C1B27D",
                        "VxM" = "#65b0dc",
                        "PAN-PRI-PRD" = "#65b0dc",
                        # "PRI-PRD" = "#DE7F52",
                        "INDEP." = "#7f7f7f",
                        "SIN PARTIDO" = "#7f7f7f",
                        "SP" = "#7f7f7f",
                        "PANAL" = "#6cbcf5",
                        "Dato no determinante" = "#b0b0b0",
                        "NULOS" = "#b0b0b0",
                        "DATOS NO REGISTRADOS" = "#636363",
                        "ELIGE" = "#c6336c")

  show_col(alpha(colores_partidos, 0.75))
  
  pal <- leaflet::colorFactor(palette = colores_partidos, 
                     levels = partidos)
}



# Cargar datos relevantes ----
## Cargar datos geográficos de elecciones ----
# distritos <- st_read("01_datos_brutos/electoral_2020/DISTRITO_FEDERAL.shp") %>%
#   clean_names() %>%
#   st_transform("+init=epsg:4326")
# 
# distritos <- rmapshaper::ms_simplify(input = as(distritos, 'Spatial')) %>%
#   st_as_sf()
# 
# st_write(distritos,
#          "01_datos_brutos/electoral_2020/distritos_simp.shp",
#          delete_dsn = T)

distritos <- st_read("01_datos_brutos/electoral_2020/distritos_simp.shp") 

entidad_dto <- distritos %>% 
  group_by(entidad) %>% 
  summarise(tipo = sum(tipo))


# Cargar datos coaliciones ----
dto_jhh <- read_xlsx("01_datos_brutos/Distritos coalición.xlsx",
                     sheet = "JHH") %>% 
  clean_names() %>% 
  mutate(origen_partidario = str_remove_all(origen_partidario, "\r"), 
         origen_partidario = str_replace_all(origen_partidario, "\n", " "),
         origen_partidario = str_squish(origen_partidario),
         grupo_parlamentario = str_remove_all(grupo_parlamentario, "\r"), 
         grupo_parlamentario = str_replace_all(grupo_parlamentario, "\n", " "),
         grupo_parlamentario = str_squish(grupo_parlamentario),
         partido_coalicion = "JHH",
         cabecera_distrital = str_remove_all(cabecera_distrital, "\r"), 
         cabecera_distrital = str_replace_all(cabecera_distrital, "\n", " "),
         cabecera_distrital = str_squish(cabecera_distrital),
         cabecera_distrital = str_to_upper(stri_trans_general(cabecera_distrital,
                                                              "Latin-ASCII")),
         distrito = paste0(id_dtto, "-", cabecera_distrital)) %>% 
  rename(number = id)

dto_vaxmx <- read_xlsx("01_datos_brutos/Distritos coalición.xlsx",
                       sheet = "Va x Mx") %>% 
  clean_names() %>% 
  mutate(grupo_parlamentario = str_remove_all(grupo_parlamentario, "\r"), 
         grupo_parlamentario = str_replace_all(grupo_parlamentario, "\n", " "),
         grupo_parlamentario = str_squish(grupo_parlamentario),
         origen_partidario = grupo_parlamentario,
         partido_coalicion = "VxM",
         cabecera_distrital = str_remove_all(cabecera_distrital, "\r"), 
         cabecera_distrital = str_replace_all(cabecera_distrital, "\n", " "),
         cabecera_distrital = str_squish(cabecera_distrital),
         cabecera_distrital = str_to_upper(stri_trans_general(cabecera_distrital,
                                                              "Latin-ASCII")),
         distrito = paste0(id_dtto, "-", cabecera_distrital)) %>% 
  select(-id)



dtos_coal_21 <- dto_jhh %>% 
  transmute(nombre_estado,
            id_dtto,
            partido_coalicion,
            grupo_parlamentario) %>% 
  bind_rows(dto_vaxmx %>% 
              transmute(nombre_estado,
                        id_dtto,
                        partido_coalicion,
                        grupo_parlamentario)) %>% 
  mutate(nombre_estado = str_remove_all(nombre_estado, "\r"), 
         nombre_estado = str_replace_all(nombre_estado, "\n", " "),
         nombre_estado = str_squish(nombre_estado),
         grupo_parlamentario = recode(grupo_parlamentario,
                                      "PARTIDO DEL TRABAJO" = "PT",
                                      "PARTIDO VERDE ECOLOGISTA DE MÉXICO" = "PVEM",
                                      "PARTIDO ACCIÓN NACIONAL" = "PAN",
                                      "PARTIDO DE LA REVOLUCIÓN DEMOCRÁTICA" = "PRD",
                                      "PARTIDO REVOLUCIONARIO INSTITUCIONAL" = "PRI")) 


sheets_coal_18 <- excel_sheets("01_datos_brutos/Coaliciones 2018.xlsx")

dtos_coal_18 <- map_df(sheets_coal_18, 
       function(x){
         read_excel("01_datos_brutos/Coaliciones 2018.xlsx",
                    sheet = x) %>% 
           clean_names()
       })


# Mapa distritos 2018 ----
## Computos distritales 2018 ----

computos_candidatos_dist_2018 <- read_delim("01_datos_brutos/Computos/20180708_2130_CW_diputaciones/diputaciones_candidaturas_2018.csv", 
                                        "|", escape_double = FALSE,
                                        skip = 1,
                                        locale = locale(encoding = "ISO-8859-1")) %>% 
  clean_names() %>% 
  mutate(partido_ci = case_when(str_detect(partido_ci, "CI") ~ "CAND_IND",
                                T ~ partido_ci)) %>% 
  group_by(estado, distrito,
           partido_ci) %>% 
  mutate(num = 1,
         num = cumsum(num), 
         partido_ci  = case_when(str_detect(partido_ci, 
                                            "CAND_IND") ~ paste0("CAND_IND", 
                                                                 "_0", num),
                                 T ~ partido_ci)) %>% 
  ungroup() %>% 
  count(estado, distrito, partido_ci,
        candidatura_propietaria) %>% 
  mutate(partido_ci = case_when(str_detect(partido_ci, "MOVIMIENTO") ~ "MC",
                                str_detect(partido_ci, "NUEVA") ~ "PANAL",
                                str_detect(partido_ci, "ENCUENTRO") ~ "PES",
                                # str_detect(partido_ci, "IND") ~ paste0(),
                                
                                T ~ partido_ci),
         candidatura_propietaria = str_to_title(candidatura_propietaria),
         candidatura_propietaria = str_replace(candidatura_propietaria, " De ", " de "),
         candidatura_propietaria = str_replace(candidatura_propietaria, " Del ", " del "),
         candidatura_propietaria = str_replace(candidatura_propietaria, " La ", " la ")) %>% 
  select(-n) %>% 
  arrange(estado, distrito, candidatura_propietaria, partido_ci)



  # group_by(estado, distrito, candidatura_propietaria) %>% 
  # mutate(num = 1/n()) %>% 
  # group_by(estado, distrito) %>% 
  # mutate(num = cumsum(num)) %>% 
  # group_by(estado, distrito, candidatura_propietaria) %>% 
  # mutate(num = max(num)) %>% 
  # ungroup()

#### Número de partidos y coaliciones
computos_candidatos_dist_2018 %>% 
  group_by(estado, distrito, candidatura_propietaria) %>% 
  summarise(partido_ci = paste0(partido_ci, collapse = "-")) %>% 
  ungroup() %>% 
  count(partido_ci) 

computos_candidatos_dist_2018 %>% 
  filter(str_detect(partido_ci, "CI")) %>% 
  group_by(estado, distrito, candidatura_propietaria) %>% 
  count(partido_ci) 

computos_distritales_2018 <- read_delim("01_datos_brutos/Computos/20180708_2130_CW_diputaciones/diputaciones.csv", 
                                        "|", escape_double = FALSE,
                                        skip = 5,
                                        locale = locale(encoding = "ISO-8859-1")) %>% 
  clean_names() %>% 
  mutate(nombre_estado = str_to_title(nombre_estado),
         nombre_estado = str_replace(nombre_estado, " De ", " de "),
         nombre_distrito = str_to_title(nombre_distrito),
         nombre_distrito = str_replace(nombre_distrito, " De ", " de "),
         nombre_distrito = str_replace(nombre_distrito, " Del ", " del "),
         nombre_distrito = str_replace(nombre_distrito, " La ", " la ")) %>% 
  dplyr::na_if("-") %>%
  convert(num(pan:vn)) %>% 
  retype() 


res_comp_dist_2018 <- computos_distritales_2018 %>% 
  group_by(id_estado, nombre_estado,
           id_distrito, nombre_distrito) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>%
  ungroup() %>% 
  select(id_estado, nombre_estado,
         id_distrito, nombre_distrito,
         pan:vn) %>% 
  pivot_longer(-c(id_estado, nombre_estado,
                  id_distrito, nombre_distrito),
               names_to = "partido",
               values_to = "votos") %>% 
  mutate(partido = case_when(str_detect(partido, "movimiento") ~ "MC",
                             str_detect(partido, "nueva") ~ "PANAL",
                             str_detect(partido, "encuentro") ~ "PES",
                             T ~ partido),
         partido = str_to_upper(partido)) %>% 
  full_join(computos_candidatos_dist_2018,
            by =c("id_estado" = "estado",
                  "id_distrito" = "distrito",
                  "partido" = "partido_ci")) %>% 
  mutate(candidatura_propietaria = case_when(partido %in% c("crn",
                                                            "vn") ~ partido,
                                             T ~ candidatura_propietaria)) %>% 
  filter(!is.na(candidatura_propietaria)) %>%
  arrange(id_estado, nombre_estado, id_distrito, nombre_distrito,
          candidatura_propietaria, partido) %>% 
  group_by(id_estado, nombre_estado, id_distrito, nombre_distrito,
           candidatura_propietaria) %>% 
  summarise(votos = sum(votos, na.rm = T),
            partido = paste0(partido, collapse = "-")) %>% 
  ungroup() %>% 
  group_by(id_estado, nombre_estado,
         id_distrito, nombre_distrito) %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T),
         ganador = ifelse(max(votos, na.rm = T) == votos, 
                          "Ganador", "Perdedor"),
         partido = recode(partido, "MC-PAN-PAN_MC-PAN_PRD-PAN_PRD_MC-PRD-PRD_MC" = "PAN-PRD-MC",
                          "MORENA-MORENA_PES-PES-PT-PT_MORENA-PT_MORENA_PES-PT_PES" = "MORENA-PT-PES",
                          "PANAL-PRI-PRI_NA-PRI_PVEM-PRI_PVEM_NA-PVEM-PVEM_NA" = "PRI-PVEM-PANAL",
                          "CAND_IND_01" = "INDEP.",
                          "CAND_IND_02" = "INDEP."),
         lugar = rank(-votos, ties.method = "first")) %>% 
  arrange(id_estado, nombre_estado,
          id_distrito, nombre_distrito,
          lugar) %>% 
  mutate(ventaja = ifelse(ganador == "Ganador",
                          votos_por-lead(votos_por), NA)) %>% 
  ungroup() %>% 
  mutate(entidad = str_to_upper(nombre_estado)) %>% 
  full_join(dtos_coal_18 %>% 
              select(entidad, 
                     dto,
                     grupo_parlamentario,
                     partido_coalicion), 
            by = c("entidad",
                   "partido" = "partido_coalicion",
                   "id_distrito" = "dto")) %>% 
  mutate(grupo_parlamentario = ifelse(is.na(grupo_parlamentario),
                          partido, grupo_parlamentario))

res_comp_dist_2018 %>% count(partido)

res_comp_dist_2018 %>% 
  filter(!is.na(grupo_parlamentario)) %>% view


res_comp_dist_2018 %>% 
  filter(ganador == "Ganador") %>% 
  write_xlsx("04_datos_generados/ganadores_dip_2018.xlsx")
## Datos geográficos con computos 2018 ----
distritos_18 <- distritos %>% 
  full_join(res_comp_dist_2018 %>% 
              filter(ganador == "Ganador"),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito")) %>% 
  full_join(computos_distritales_2018 %>% 
              group_by(id_estado,
                       id_distrito) %>% 
              summarise(total_votos_calculados = sum(as.numeric(total_votos_calculados),
                                                     na.rm = T),
                        lista_nominal = sum(as.numeric(lista_nominal_casilla),
                                            na.rm = T),
                        participación = total_votos_calculados/lista_nominal),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito"))

res_comp_dist_2018 %>% 
  filter(ganador == "Ganador") %>% 
  count(partido)
  summarise(sum(n))


## Mapa de resultados distritales 2018 ----
### Leyendas ----

labels_18 <- paste0("<strong>",
                    "Dto. ",
                 as.character(distritos_18$distrito_f), "-", 
                 as.character(distritos_18$nombre_distrito), ", ",
                 "<br>",
                 as.character(distritos_18$nombre_estado), "<br>") %>% 
  lapply(htmltools::HTML)
  
  popup_18 <- paste0("<strong>",
                      "Dto. ",
                      as.character(distritos_18$distrito_f), "-", 
                      as.character(distritos_18$nombre_distrito), ", ",
                      "<br>",
                      as.character(distritos_18$nombre_estado), 
                      "</strong>", 
                      "<br>", "<b>", "Ganador: ", "</b>",
                     as.character(distritos_18$partido), 
                      "<br>", "<b>", "Candidato gandore: ", "</b>",
                     as.character(distritos_18$candidatura_propietaria), " (", 
                     as.character(percent(distritos_18$votos_por, accuracy = 0.1)), ")",
                      "<br>", "<b>", "Ventaja vs. 2do lugar: ", "</b>", as.character(percent(distritos_18$ventaja, accuracy = 0.1)), 
                     "<br>","<b>", "Participación: ", "</b>",
                     as.character(percent(distritos_18$participación, 
                                          accuracy = 0.1))) %>% 
    lapply(htmltools::HTML)

mapa_distritos_18 <- leaflet(distritos_18) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=4, maxZoom=10)) %>% 
  addPolygons(
    fillColor = ~pal(partido),
    weight = 0.4,
    opacity = 1,
    color = "white",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    popup = popup_18,
    popupOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"),
    label = labels_18,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador", # Title
    position = "bottomleft") %>% 
  addPolylines(data = entidad_dto,
              weight = 1.5,
              opacity = 1,
              color = "#525252",
              dashArray = "",
              group = "Entidad") 

mapa_distritos_18

 
htmlwidgets::saveWidget(mapa_distritos_18,
                        "mapa_distritos_18.html")




# Mapa distritos 2021 ----
## Computos distritales 2021 ----
computos_candidatos_dist_2021 <- read_delim("01_datos_brutos/Computos/20210611_1000_CW_diputaciones/diputaciones_candidaturas_2021.csv", 
                                            "|", escape_double = FALSE, 
                                            locale = locale(date_names = "es", 
                                                            encoding = "ISO-8859-1"),
                                            trim_ws = TRUE, 
                                            skip = 1) %>% 
  clean_names() %>% 
  filter(!is.na(candidatura_propietaria)) %>% 
  count(estado, distrito, partido_ci,
        candidatura_propietaria) %>% 
  mutate(partido_ci = case_when(str_detect(partido_ci, "MOVIMIENTO") ~ "MC",
                                str_detect(partido_ci, "NUEVA") ~ "PANAL",
                                str_detect(partido_ci, "ENCUENTRO") ~ "PES",
                                str_detect(partido_ci, "FS X") ~ "FXM",
                                str_detect(partido_ci, "CI_") ~ "INDEP.",
                                T ~ partido_ci),
         partido_ci = str_replace_all(partido_ci, "-", "_"),
         candidatura_propietaria = str_to_title(candidatura_propietaria),
         candidatura_propietaria = str_replace(candidatura_propietaria, " De ", " de "),
         candidatura_propietaria = str_replace(candidatura_propietaria, " Del ", " del "),
         candidatura_propietaria = str_replace(candidatura_propietaria, " La ", " la ")) %>% 
  select(-n) %>% 
  arrange(estado, distrito, candidatura_propietaria, partido_ci)

computos_candidatos_dist_2021
# group_by(estado, distrito, candidatura_propietaria) %>% 
# mutate(num = 1/n()) %>% 
# group_by(estado, distrito) %>% 
# mutate(num = cumsum(num)) %>% 
# group_by(estado, distrito, candidatura_propietaria) %>% 
# mutate(num = max(num)) %>% 
# ungroup()

#### Número de partidos y coaliciones
computos_candidatos_dist_2021 %>% 
  group_by(estado, distrito, candidatura_propietaria) %>% 
  summarise(partido_ci = paste0(partido_ci, collapse = "-")) %>% 
  ungroup() %>% 
  count(partido_ci) 


computos_distritales_2021 <- read_delim("01_datos_brutos/Computos/20210611_1000_CW_diputaciones/diputaciones.csv", 
                                        "|", escape_double = FALSE, 
                                        locale = locale(date_names = "es", 
                                                        encoding = "ISO-8859-1"),
                                        trim_ws = TRUE, 
                                      skip = 5,
                                      guess_max = 180000,
                                      na = c("-", "Ilegible", "Sin dato")) %>% 
  clean_names() %>% 
  mutate(nombre_estado = str_to_title(nombre_estado),
         nombre_estado = str_replace(nombre_estado, " De ", " de "),
         nombre_distrito = str_to_title(nombre_distrito),
         nombre_distrito = str_replace(nombre_distrito, " De ", " de "),
         nombre_distrito = str_replace(nombre_distrito, " Del ", " del "),
         nombre_distrito = str_replace(nombre_distrito, " La ", " la "))  %>% 
  convert(num(pan:votos_nulos)) 
    
res_comp_dist_2021 <- computos_distritales_2021 %>% 
  # filter(tipo_acta == "Diputados MR",
  #        contabilizada == 1) %>% 
  # filter(!str_detect(observaciones, "legible")) %>% 
  group_by(id_estado, nombre_estado,
           id_distrito, nombre_distrito) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>%
  ungroup() %>% 
  select(id_estado, nombre_estado,
         id_distrito,
         nombre_distrito,
         pan:votos_nulos) %>% 
  pivot_longer(-c(id_estado, nombre_estado,
                  id_distrito, 
                  nombre_distrito),
               names_to = "partido",
               values_to = "votos") %>% 
  mutate(partido = case_when(str_detect(partido, "movimiento") ~ "MC",
                             str_detect(partido, "nueva") ~ "PANAL",
                             str_detect(partido, "encuentro") ~ "PES",
                             str_detect(partido, "fs x") ~ "FXM",
                             str_detect(partido, "ci") ~ "INDEP.",
                             T ~ partido),
         partido = str_to_upper(partido)) %>% 
  full_join(computos_candidatos_dist_2021,
            by =c("id_estado" = "estado",
                  "id_distrito" = "distrito",
                  "partido" = "partido_ci")) %>% 
mutate(candidatura_propietaria = case_when(partido %in% c("CANDIDATO_A_NO_REGISTRADO_A",
                                                          "VOTOS_NULOS") ~ partido,
                                           T ~ candidatura_propietaria)) %>% 
  filter(!is.na(candidatura_propietaria)) %>%
  arrange(id_estado, nombre_estado, id_distrito, nombre_distrito,
          candidatura_propietaria, partido) %>% 
  group_by(id_estado, nombre_estado, id_distrito, nombre_distrito,
           candidatura_propietaria) %>% 
  summarise(votos = sum(votos, na.rm = T),
            partido = paste0(partido, collapse = "-")) %>% 
  ungroup() %>% 
  group_by(id_estado, nombre_estado,
           id_distrito, nombre_distrito) %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T),
         ganador = ifelse(max(votos, na.rm = T) == votos, 
                          "Ganador", "Perdedor"),
         partido = case_when(str_detect(partido, "MORENA") &
                               str_detect(partido, "PT") &
                               str_detect(partido, "PVEM") ~ "JHH",
                             str_detect(partido, "PAN") &
                               str_detect(partido, "PRI") &
                               str_detect(partido, "PRD") ~ "VxM",
                             T ~ partido),
         lugar = rank(-votos, ties.method = "first")) %>% 
  arrange(id_estado, nombre_estado, id_distrito, nombre_distrito,
          lugar) %>% 
  mutate(ventaja = ifelse(ganador == "Ganador",
                          votos_por-lead(votos_por), NA)) %>% 
  ungroup() %>% 
  mutate(nombre_estado_upper = str_to_upper(nombre_estado)) %>% 
  full_join(dtos_coal_21, 
            by = c("nombre_estado_upper" = "nombre_estado",
                   "partido" = "partido_coalicion",
                   "id_distrito" = "id_dtto")) %>% 
  mutate(partido_or = partido,
         partido = ifelse(is.na(grupo_parlamentario),
                   partido, grupo_parlamentario))

res_comp_dist_2021 %>% 
  # filter(ganador == "Ganador") %>% 
  group_by(partido) %>% 
  summarise(votos = sum(votos, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T)) %>% 
  adorn_totals() %>% 
  mutate(votos = comma(votos),
         votos_por = percent(votos_por))

# ## Datos geográficos con computos 2021 ----

distritos_21 <- distritos %>%
  full_join(res_comp_dist_2021 %>%
              filter(ganador == "Ganador"),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito")) %>% 
  full_join(computos_distritales_2021 %>% 
              group_by(id_estado,
                       id_distrito) %>% 
              summarise(total_votos_calculados = sum(as.numeric(total_votos_calculados),
                                                   na.rm = T),
                        lista_nominal = sum(as.numeric(lista_nominal_casilla),
                                                     na.rm = T),
                        participación = total_votos_calculados/lista_nominal),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito"))



## Mapa de resultados distritales 2021 ----
### Leyendas ----

labels_21 <- paste0("<strong>",
                    "Dto. ",
                    as.character(distritos_21$distrito_f), "-", 
                    as.character(distritos_21$nombre_distrito), ", ",
                    "<br>", 
                    as.character(distritos_21$nombre_estado), 
                    "<br>") %>% 
  lapply(htmltools::HTML)
  
  
  popup_21 <- paste0("<strong>",
                      "Dto. ",
                      as.character(distritos_21$distrito_f), "-", 
                      as.character(distritos_21$nombre_distrito), ", ",
                      "<br>",
                      as.character(distritos_21$nombre_estado), 
                      "</strong>", 
                      "<br>", "<b>", "Ganador: ", "</b>", as.character(distritos_21$partido), 
                      "<br>", "<b>", "Candidato ganador: ", "<br>",
                      "</b>", as.character(distritos_21$candidatura_propietaria), 
                      " (",
                      as.character(percent(distritos_21$votos_por,
                                           accuracy = 0.1)), ")",
                      "<br>", "<b>", "Ventaja vs. 2do lugar: ", 
                      "</b>", as.character(percent(distritos_21$ventaja, 
                                                   accuracy = 0.1)), "<br>",
                     "<b>", "Participación: ", "</b>",
                     as.character(percent(distritos_21$participación,
                                          accuracy = 0.1))) %>% 
    lapply(htmltools::HTML)
  
  

mapa_distritos_21 <- leaflet(distritos_21) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=4, maxZoom=10)) %>% 
  addPolygons(
    fillColor = ~pal(partido),
    weight = 0.4,
    opacity = 1,
    color = "white",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    popup = popup_21,
    popupOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"),
    label = labels_21,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador", # Title
    position = "bottomleft") %>% 
  addPolylines(data = entidad_dto,
               weight = 1.5,
               opacity = 1,
               color = "#525252",
               dashArray = "",
               group = "Entidad") 

mapa_distritos_21

htmlwidgets::saveWidget(mapa_distritos_21, "02_graficas/mapa_distritos_21.html")

# Mapas 2018-2021 ----

mapa_distritos_18_21 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=4, maxZoom=10)) %>% 
  addPolygons(
    data = distritos_21,
    fillColor = ~pal(partido),
    weight = 0.4,
    opacity = 1,
    color = "white",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    popup = popup_21,
    popupOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"),
    label = labels_21,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto"),
    group = 'Elecciones 2021',
    options = list(zIndex = 199)) %>% 
  addPolygons(
    data = distritos_18,
    fillColor = ~pal(partido),
    weight = 0.4,
    opacity = 1,
    color = "white",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    popup = popup_18,
    popupOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"),
    label = labels_18,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto"),
    group = 'Elecciones 2018', 
    options = list(zIndex = 200)) %>% 
  addLegend(data = tibble(partido = partidos[!str_detect(partidos, 
                                                          "-|Dato no determinante|RS|FxM|FXM|JHH|VxM|ELIGE|INDEP.|NO REGISTRADOS|NULOS")]) %>% 
              arrange(partido), # Legend options
    pal = pal, # Previously defined palette
    values = ~partido, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador", # Title
    position = "bottomleft",
    group = 'Elecciones 2021') %>% 
  addPolylines(data = entidad_dto,
               weight = 1.5,
               opacity = 1,
               color = "#525252",
               dashArray = "",
               options = list(zIndex = 300)) %>% 
  addLayersControl(baseGroups = c('Elecciones 2021', 'Elecciones 2018'),
                   options = layersControlOptions(collapsed = F),
                   position = 'topright') %>% 
  htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")



mapa_distritos_18_21

htmlwidgets::saveWidget(mapa_distritos_18_21, "02_graficas/mapa_distritos_18_21.html")

# pacman::p_load(mapview)
# mapshot(mapa_distritos_21, file = '02_graficas/mapa_distritos_21.png'


# Mapa estatales 2018 ----
## Computos estatales 2018 ----

datos_entidades <- read_excel("01_datos_brutos/datos estatales.xlsx") 


## Datos geográficos con computos 2018 ----

entidades_antes <- entidad_dto %>% 
  mutate(cv_inegi_edo = sprintf("%02d", entidad)) %>% 
  select(-entidad) %>% 
  full_join(datos_entidades, by = "cv_inegi_edo") %>% 
  # full_join(datos_elecciones, by = "cv_inegi_edo") %>% 
  mutate(partido_actualizado = ifelse(is.na(partido_electo),
                                      gob_antes_partido,
                                      partido_electo),
         partido_actualizado_completo = ifelse(is.na(partido_entero),
                                      gob_antes_partido,
                                      partido_entero))


## Mapa de resultados estatales 2018 ----
### Leyendas ----

labels_ent_18 <- paste0("<strong>",
                    as.character(entidades_antes$entidad), "<br>",
                    ifelse(entidades_antes$elecciones == "Elecciones",
                           paste0("Elige gobernador"),
                           ""),
                    "</strong>", 
                    "<br>",
                    "<b>", "Partido gobernante: ", "</b>", as.character(entidades_antes$gob_antes_partido), 
                    "<br>", "<b>", "Gobernador: ",
                    "</b>", as.character(entidades_antes$gob_antes), 
                    "<b>") %>% 
  lapply(htmltools::HTML)




mapa_entidades_18 <- leaflet() %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(minZoom=4, maxZoom=9)) %>% 
  addPolygons(data = entidades_antes %>% 
                filter(elecciones != "Elecciones"),
              fillColor = ~pal(gob_antes_partido),
              weight = 0.5,
              opacity = 1,
              color = "white",
              # dashArray = "1",
              dashArray = "",
              fillOpacity = 0.75,
              highlight = highlightOptions(
                weight = 2.5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.75,
                bringToFront = F,
                sendToBack = T),
              label = labels_ent_18[!str_detect(labels_ent_18,
                                                "Elige gobernador")],
              labelOptions = labelOptions(
                interactive = F,
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "8px",
                opacity = 0.7,
                direction = "auto"),
              group = 'Sin elecciones',
              options = list(zIndex = 200)) %>% 
  addLegend(data = entidades_antes, # Legend options
            pal = pal, # Previously defined palette
            values = ~gob_antes_partido, # Values from data frame 
            opacity = 0.95,
            title = "Partido ganador", # Title
            position = "bottomleft") %>% 
  addPolygons(data = entidades_antes %>% 
                filter(elecciones == "Elecciones"),
              fillColor = ~pal(gob_antes_partido),
              weight = 1.5,
              opacity = 1,
              color = "#525252",
              dashArray = "1",
              # dashArray = "",
              fillOpacity = 0.75,
              highlight = highlightOptions(
                weight = 2.5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.75,
                bringToFront = T,
                sendToBack = F),
              label = labels_ent_18[str_detect(labels_ent_18,
                                               "Elige gobernador")],
              labelOptions = labelOptions(
                interactive = F,
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "8px",
                opacity = 0.7,
                direction = "auto"),
              group = 'Con elecciones',
              options = list(zIndex = 199)) %>% 
  leaflet.minicharts::addMinicharts(-110.829915,
                                    15.204202, 
                                    chartdata = entidades_antes %>% 
                                      st_set_geometry(NULL) %>% 
                                      count(gob_antes_partido,
                                            sort = T) %>% 
                                      pivot_wider(values_from = n, 
                                                  names_from = gob_antes_partido),
                                    legend = F,
                                    colorPalette = c("#2d5e29",
                                                     "#5877ac",
                                                     "#874a53",
                                                     "#7f7f7f",
                                                     "#e89547",
                                                     "#6dabb7",
                                                     "#75529c",
                                                     "#F5DA57"),
                                    width = 110, height = 110,
                                    layerId = 'Gobernadores <br>por partido') %>% 
  addLayersControl(overlayGroups = c('Con elecciones',
                                     'Sin elecciones'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  htmlwidgets::onRender("
    function(el, x) {
      this.on('overlaylayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")

mapa_entidades_18

htmlwidgets::saveWidget(mapa_entidades_18, "02_graficas/mapa_entidades_18.html")


## Mapa de resultados estatales 2021 ----
### Leyendas ----

labels_ent_21 <- paste0("<strong>",
                        as.character(entidades_antes$entidad), 
                        "<br>",
                        "</strong>", 
                        
                        ifelse(entidades_antes$elecciones == "Elecciones",
                               
                               paste0("<br>", "<b>", "Partido ganador: ", "</b>", as.character(entidades_antes$partido_entero), 
                                      "<br>", "<b>", "Candidato ganador: ", "</b>", as.character(entidades_antes$candidato_ganador), 
                                      # "<br>", "<b>", "Porcentaje de votos: ", "</b>", as.character(entidades_antes$votos_por_conteo), "<b>",
                                      
                                      "<b>"),
                               
                               paste0("<br>",
                                      "<b>", "Partido gobernante: ", "</b>", as.character(entidades_antes$gob_antes_partido), 
                                      "<br>", "<b>", "Gobernador: ",
                                      "</b>", as.character(entidades_antes$gob_antes), 
                                      "<b>")
                               
                        )) %>% 
  
  lapply(htmltools::HTML)




mapa_entidades_21 <- leaflet() %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=4, maxZoom=10)) %>% 
  addLayersControl(overlayGroups = c('Con elecciones',
                                     'Sin elecciones'),
                   options = layersControlOptions(collapsed = F,
                                                  autoZIndex = F),
                   position = 'topright') %>%
  addPolygons(data = entidades_antes %>% 
                filter(elecciones != "Elecciones"),
    fillColor = ~pal(gob_antes_partido),
    weight = 0.5,
    opacity = 1,
    color = "white",
    # dashArray = "1",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2.5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = F,
      sendToBack = T),
    label = labels_ent_21[!str_detect(labels_ent_21,
                                     "ganador")],
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto"),
    group = 'Sin elecciones',
    options = list(zIndex = 180)) %>% 
  addPolygons(data = entidades_antes %>% 
                filter(elecciones == "Elecciones"),
              fillColor = ~pal(partido_electo),
              weight = 1.5,
              opacity = 1,
              color = "#525252",
              dashArray = "1",
              # dashArray = "",
              fillOpacity = 0.75,
              highlight = highlightOptions(
                weight = 2.5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.75,
                bringToFront = T,
                sendToBack = F),
              label =labels_ent_21[str_detect(labels_ent_21,
                                              "ganador")],
              labelOptions = labelOptions(
                interactive = F,
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "8px",
                opacity = 0.7,
                direction = "auto"),
              group = 'Con elecciones',
              options = list(zIndex = 200)) %>%
  addLegend(data = entidades_antes, # Legend options
            pal = pal, # Previously defined palette
            values = ~partido_actualizado, # Values from data frame 
            opacity = 0.95,
            title = "Partido ganador", # Title
            position = "bottomleft") %>% 
  leaflet.minicharts::addMinicharts(-110.829915,
                                    15.204202, 
                                    chartdata = entidades_antes %>% 
                                      st_set_geometry(NULL) %>% 
                                      count(partido_actualizado,
                                            sort = T) %>% 
                                      pivot_wider(values_from = n, 
                                                  names_from = partido_actualizado),
                                    legend = F,
                                    colorPalette = c("#874a53",
                                                     "#5877ac",
                                                     "#2d5e29",
                                                     "#e89547",
                                                     "#6dabb7",
                                                     "#75529c",
                                                     "#C1B27D"),
                                    width = 110, height = 110,
                                    layerId = 'Gobernadores <br>por partido') %>%
  
  htmlwidgets::onRender("
    function(el, x) {
      this.on('overlaylayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")
 

mapa_entidades_21

htmlwidgets::saveWidget(mapa_entidades_21, "02_graficas/mapa_entidades_21.html")


# Mapa CdMx 2015 ----
## Cargar datos CdMX ----

# cdmx_dta <- st_read("01_datos_brutos/00_muns/00mun.shp") %>%
#   clean_names() %>%
#   filter(cve_ent == "09") %>%
#   st_transform("+init=epsg:4326") %>%
#   st_collection_extract("POLYGON")
# 
# st_write(cdmx_dta,
#          "01_datos_brutos/00_muns/cdmx_dta.shp",
#          delete_dsn = T)

cdmx_dta <- st_read("01_datos_brutos/00_muns/cdmx_dta.shp") %>% 
  full_join(read_xlsx("01_datos_brutos/Alcaldes CdMx.xlsx"),
            by = c("cvegeo" = "clave_inegi"))


## Mapa de resultados distritales 2018 ----
### Leyendas ----

labels_cdmx_15 <- paste0("<strong>",
                    as.character(cdmx_dta$municipio), 
                    "</strong>", 
                    "<br>", "<b>", "Partido gobernante: ", "</b>", as.character(cdmx_dta$partido_2015), "<br>") %>% 
  lapply(htmltools::HTML)


mapa_cdmx_dta_15 <- leaflet(cdmx_dta) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=9, maxZoom=15)) %>% 
  addPolygons(
    fillColor = ~pal(partido_2015),
    weight = 1,
    opacity = 1,
    color = "#d9d9d9",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    label = labels_cdmx_15,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido_2015, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador (2015)", # Title
    position = "bottomleft") 

mapa_cdmx_dta_15

htmlwidgets::saveWidget(mapa_cdmx_dta_15, "02_graficas/mapa_cdmx_dta_15.html")


# Mapa CdMx 2018 ----
## Mapa de resultados distritales 2018 ----
### Leyendas ----

labels_cdmx_18 <- paste0("<strong>",
                         as.character(cdmx_dta$municipio), 
                         "</strong>", 
                         "<br>", "<b>", "Partido gobernante: ", "</b>", as.character(cdmx_dta$partido_2018), "<br>") %>% 
  lapply(htmltools::HTML)




mapa_cdmx_dta_18 <- leaflet(cdmx_dta) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=9, maxZoom=15)) %>% 
  addPolygons(
    fillColor = ~pal(partido_2018),
    weight = 1,
    opacity = 1,
    color = "#d9d9d9",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    label = labels_cdmx_18,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido_2018, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador (2018)", # Title
    position = "bottomleft") 

mapa_cdmx_dta_18

htmlwidgets::saveWidget(mapa_cdmx_dta_18, "02_graficas/mapa_cdmx_dta_18.html")


# Mapa CdMx 2021 ----
## Cargar computo 2021 ----
computos_candidatos_cdmx_2021 <- read_csv("01_datos_brutos/Computos/P municipaless y alcaldes/20210607_2000_PREP_ALC_CDMX/alcaldias_candidato.csv") %>% 
  clean_names() %>% 
  filter(!is.na(candidatura_propietaria)) %>% 
  count(alcaldia, partido_ci,
        candidatura_propietaria) %>%
  mutate(alcaldia = str_to_title(alcaldia),
         alcaldia = str_replace(alcaldia, " De ", " de "),
         alcaldia = str_replace(alcaldia, " Del ", " del "),
         alcaldia = str_replace(alcaldia, " La ", " la ")) %>% 
  mutate(partido_ci = case_when(str_detect(partido_ci, "MOVIMIENTO") ~ "MC",
                                str_detect(partido_ci, "NUEVA") ~ "PANAL",
                                str_detect(partido_ci, "ENCUENTRO") ~ "PES",
                                str_detect(partido_ci, "Sin Partido") ~ str_to_upper(make_clean_names(stri_trans_general(candidatura_propietaria,
                                                                                                                         "Latin-ASCII"))),
                                T ~ partido_ci),
         partido_ci = str_replace_all(partido_ci, "-", "_"),
         candidatura_propietaria = str_to_title(candidatura_propietaria),
         candidatura_propietaria = str_replace(candidatura_propietaria, " De ", " de "),
         candidatura_propietaria = str_replace(candidatura_propietaria, " Del ", " del "),
         candidatura_propietaria = str_replace(candidatura_propietaria, " La ", " la "),) %>% 
  select(-n) %>% 
  arrange(alcaldia, candidatura_propietaria, partido_ci) %>% 
  left_join(tibble(partido_ci = c("MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "MORENA_PT_PVEM",
                                  "PAN_PRI_PRD",
                                  "PAN_PRI_PRD",
                                  "PAN_PRI_PRD",
                                  "PAN_PRI_PRD",
                                  "PAN_PRI_PRD",
                                  "PAN_PRI_PRD",
                                  "PAN_PRI_PRD",
                                  "PRI_PRD",
                                  "PRI_PRD",
                                  "PRI_PRD",
                                  "PT_MORENA",
                                  "PT_MORENA",
                                  "PT_MORENA"),
                   partidos = c("MORENA_PT_PVEM",
                                "MORENA_PT",
                                "PT_PVEM",
                                "MORENA_PVEM",
                                "MORENA",
                                "PT",
                                "PVEM",
                                "PVEM_PT_MORENA",
                                "PVEM_PT",
                                "PVEM_MORENA",
                                "PT_MORENA",
                                "PAN_PRI_PRD",
                                "PAN_PRI",
                                "PRI_PRD",
                                "PAN_PRD",
                                "PAN",
                                "PRI",
                                "PRD",
                                "PRI_PRD",
                                "PRI",
                                "PRD",
                                "PT_MORENA",
                                "PT",
                                "MORENA")),
            by = "partido_ci") %>% 
  mutate(partido_ci = ifelse(is.na(partidos),
                              partido_ci,
                              partidos)) %>% 
  select(-partidos)






# group_by(estado, distrito, candidatura_propietaria) %>% 
# mutate(num = 1/n()) %>% 
# group_by(estado, distrito) %>% 
# mutate(num = cumsum(num)) %>% 
# group_by(estado, distrito, candidatura_propietaria) %>% 
# mutate(num = max(num)) %>% 
# ungroup()

#### Número de partidos y coaliciones

computos_cdmx_2021 <- read_csv("01_datos_brutos/Computos/P municipaless y alcaldes/20210607_2000_PREP_ALC_CDMX/CDMX_ALC_2021.csv", 
                                      skip = 4) %>% 
  clean_names() %>% 
  mutate(alcaldia = str_to_title(alcaldia),
         alcaldia = str_replace(alcaldia, " De ", " de "),
         alcaldia = str_replace(alcaldia, " Del ", " del "),
         alcaldia = str_replace(alcaldia, " La ", " la ")) %>% 
  dplyr::na_if("-") %>%
  convert(num(pan:nulos)) %>% 
  retype() 
  


res_comp_cdmx_2021 <- computos_cdmx_2021 %>% 
  group_by(alcaldia) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>%
  ungroup() %>% 
  select(alcaldia,
         pan:nulos) %>% 
  pivot_longer(-c(alcaldia),
               names_to = "partido",
               values_to = "votos") %>% 
  mutate(partido = case_when(str_detect(partido, "movimiento") ~ "MC",
                             str_detect(partido, "nueva") ~ "PANAL",
                             str_detect(partido, "encuentro") ~ "PES",
                             T ~ partido),
         partido = str_to_upper(partido),
         alcaldia = recode(alcaldia,
                           "Coyoacan" = "Coyoacán",
                           "Cuauhtemoc" = "Cuauhtémoc",
                           "Gustavo A. Madero" = "Gustavo A Madero", 
                           "Tlahuac" = "Tláhuac")) %>% 
  filter(votos != 0) %>% 
  full_join(computos_candidatos_cdmx_2021,
            by =c("alcaldia" = "alcaldia",
                  "partido" = "partido_ci")) %>% 
  mutate(candidatura_propietaria = case_when(partido %in% c("NO_REGISTRADOS",
                                                            "NULOS") ~ partido,
                                             T ~ candidatura_propietaria)) %>% 
  arrange(alcaldia,
          candidatura_propietaria, partido) %>% 
  group_by(alcaldia,
           candidatura_propietaria) %>% 
  summarise(votos = sum(votos, na.rm = T),
            partido = paste0(partido, collapse = "_")) %>% 
  ungroup() %>% 
  group_by(alcaldia) %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T),
         ganador = ifelse(max(votos, na.rm = T) == votos, 
                          "Ganador", "Perdedor"),
         partido = case_when(partido == "MORENA_MORENA_PT_MORENA_PT_PVEM_MORENA_PVEM_PT_PT_MORENA_PT_PVEM_PVEM_PVEM_MORENA_PVEM_PT_PVEM_PT_MORENA" ~ "MORENA-PT-PVEM",
                             partido == "MORENA_PT_PT_MORENA" ~ "MORENA-PT",
                             partido == "PAN_PAN_PRD_PAN_PRI_PAN_PRI_PRD_PRD_PRI_PRI_PRD" ~ "PAN-PRI-PRD",
                             partido == "PRD_PRI_PRI_PRD" ~ "PRI-PRD",
                             T ~ partido),
         partido = str_replace_all(partido, "_", "-"),
         # partido = case_when(str_detect(partido, "MORENA") &
         #                       str_detect(partido, "PT") &
         #                       str_detect(partido, "PVEM") ~ "JHH",
         #                     str_detect(partido, "PAN") &
         #                       str_detect(partido, "PRI") &
         #                       str_detect(partido, "PRD") ~ "VxM",
         #                     T ~ partido),
         lugar = rank(-votos, ties.method = "first")) %>% 
  arrange(alcaldia,
          lugar) %>% 
  mutate(ventaja = ifelse(ganador == "Ganador",
                          votos_por-lead(votos_por), NA))



res_comp_cdmx_2021 %>% 
  ungroup() %>% 
  count(partido) 


res_comp_cdmx_2021 %>% 
  mutate(alcaldia = recode(alcaldia,
                           "Alvaro Obregon" = "Álvaro Obregón",
                           "Benito Juarez" = "Benito Juárez",
                           "Gustavo A Madero" = "Gustavo A. Madero")) %>% 
  filter(ganador == "Ganador") %>% 
  count(partido)

cdmx_dta_21 <- cdmx_dta %>% 
  full_join(res_comp_cdmx_2021 %>% 
              mutate(alcaldia = recode(alcaldia,
                                       "Alvaro Obregon" = "Álvaro Obregón",
                                       "Benito Juarez" = "Benito Juárez",
                                       "Gustavo A Madero" = "Gustavo A. Madero")) %>% 
              filter(ganador == "Ganador"), by = c("municipio" = "alcaldia")) %>% 
  full_join(computos_cdmx_2021 %>% 
              group_by(alcaldia) %>% 
              mutate(alcaldia = recode(alcaldia,
                                       "Alvaro Obregon" = "Álvaro Obregón",
                                       "Benito Juarez" = "Benito Juárez",
                                       "Gustavo A Madero" = "Gustavo A. Madero",
                                       "Coyoacan" = "Coyoacán",
                                       "Cuauhtemoc" = "Cuauhtémoc",
                                       "Tlahuac" = "Tláhuac")) %>% 
              summarise(actas = n(),
                        actas_contabilizadas = sum(as.numeric(contabilizadas),
                                                   na.rm = T),
                        actas_contabilizadas_por = actas_contabilizadas/actas),
            by = c("municipio" = "alcaldia"))
  # mutate(partido_2021 = sample(x = c("MORENA",
  #                                      "PAN",
  #                                      "PRI",
  #                                      "PVEM",
  #                                      "PRD",
  #                                      "PT",
  #                                      "RSP",
  #                                      "MC",
  #                                      "FxM",
  #                                      "PES",
  #                                      "JHH",
  #                                      "VxM",
  #                                      "INDEP."),
  #                                prob = c(0.3,
  #                                         0.07,
  #                                         0.07,
  #                                         0.02,
  #                                         0.01,
  #                                         0.02,
  #                                         0.01,
  #                                         0.03,
  #                                         0.01,
  #                                         0.01,
  #                                         0.25,
  #                                         0.2,
  #                                         0.0),
  #                                size = max(row_number()),
  #                                replace = TRUE),
  #        votos_por = runif(max(row_number()), 0.3, 0.6),
  #        candidatura_propietaria = "Fulano")

## Mapa de resultados distritales 2021 ----
### Leyendas ----

popup_cdmx_21 <- paste0("<strong>",
                         as.character(cdmx_dta_21$municipio), 
                         "</strong>", 
                         "<br>","<br>", "<b>", "Actas contabilizadas: ", "</b>",  as.character(percent(cdmx_dta_21$actas_contabilizadas_por, accuracy = 0.1)),
                         "<br>", "<b>", "Partido que va ganando: ", "</b>", as.character(cdmx_dta_21$partido), 
                         "<br>", "<b>", "Candidato que va ganando: ", "<br>",
                         "</b>", as.character(cdmx_dta_21$candidatura_propietaria), 
                         "<br>", "<b>", "Porcentaje de votos: ", "</b>", as.character(percent(cdmx_dta_21$votos_por, accuracy = 0.1)), 
                         "<br>", "<b>", "Ventaja vs. 2do lugar: ", "</b>", as.character(percent(cdmx_dta_21$ventaja, accuracy = 0.1)), "<br>") %>% 
  lapply(htmltools::HTML)

labels_cdmx_21 <- paste0("<strong>",
                         as.character(cdmx_dta_21$municipio), 
                         "</strong>", 
                         "<br>", "<b>", "Partido ganador: ", "</b>", as.character(cdmx_dta_21$partido)) %>% 
  lapply(htmltools::HTML)




mapa_cdmx_dta_21 <- leaflet(cdmx_dta_21) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=9, maxZoom=15)) %>% 
  addPolygons(
    fillColor = ~pal(partido),
    weight = 1,
    opacity = 1,
    color = "#d9d9d9",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    popup = popup_cdmx_21,
    popupOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"),
    label = labels_cdmx_21,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador (2021)", # Title
    position = "bottomleft") 

mapa_cdmx_dta_21

htmlwidgets::saveWidget(mapa_cdmx_dta_21, "02_graficas/mapa_cdmx_dta_21.html")


# Mapa CDMX 2015-2021 ----

mapa_cdmx_15_21 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=9, maxZoom=12)) %>% 
  addPolygons(data = cdmx_dta_21,
    fillColor = ~pal(partido),
    weight = 1,
    opacity = 1,
    color = "#d9d9d9",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    popup = popup_cdmx_21,
    popupOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"),
    label = labels_cdmx_21,
    labelOptions = labelOptions(
      interactive = F,
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "8px",
      opacity = 0.7,
      direction = "auto"),
    group = 'Elecciones locales 2021',
    options = list(zIndex = 199)) %>% 
  addPolygons(data = cdmx_dta,
              fillColor = ~pal(partido_2018),
              weight = 1,
              opacity = 1,
              color = "#d9d9d9",
              # dashArray = "3",
              dashArray = "",
              fillOpacity = 0.75,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.75,
                bringToFront = T,
                sendToBack = T),
              label = labels_cdmx_18,
              labelOptions = labelOptions(
                interactive = F,
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "8px",
                opacity = 0.7,
                direction = "auto"),
              group = 'Elecciones locales 2018',
              options = list(zIndex = 20)) %>% 
  addPolygons(data = cdmx_dta,
              fillColor = ~pal(partido_2015),
              weight = 1,
              opacity = 1,
              color = "#d9d9d9",
              # dashArray = "3",
              dashArray = "",
              fillOpacity = 0.75,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.75,
                bringToFront = T,
                sendToBack = T),
              label = labels_cdmx_18,
              labelOptions = labelOptions(
                interactive = F,
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "8px",
                opacity = 0.7,
                direction = "auto"),
              group = 'Elecciones locales 2015',
              options = list(zIndex = 2001)) %>% 
  addLegend(data = tibble(partido = partidos[!str_detect(partidos, 
                                                         "Dato no determinante|RS|FxM|FXM|JHH|VxM|ELIGE|INDEP.|NO REGISTRADOS|NULOS|PVEM|PAN-PRD|PES|MORENA-PT-PES|PT-PVEM|PANAL")& 
                                               partidos!= "PT"]) %>% 
              arrange(partido), # Legend options
            pal = pal, # Previously defined palette
            values = ~partido, # Values from data frame 
            opacity = 0.95,
            title = "Partido ganador", # Title
            position = "bottomleft",
            group = 'Elecciones 2021') %>% 
  # addPolylines(data = mu,
  #              weight = 1.5,
  #              opacity = 1,
  #              color = "#525252",
  #              dashArray = "",
  #              options = list(zIndex = 300)) %>% 
  addLayersControl(baseGroups = c('Elecciones locales 2021', 
                                  'Elecciones locales 2018',
                                  'Elecciones locales 2015'),
                   overlayGroups = "minicharts",
                   options = layersControlOptions(collapsed = F),
                   position = 'topright') %>% 
  addMinicharts(-98.841869,
                19.285330, 
    chartdata = cdmx_dta %>% 
      st_set_geometry(NULL) %>% 
      count(partido_2015,
            sort = T) %>% 
      pivot_wider(values_from = n, 
                  names_from = partido_2015),
    legend = F,
    colorPalette = c("#F5DA57",
                     "#874a53",
                     "#2d5e29",
                     "#5877ac"),
    width = 60, height = 60,
    layerId = 'minicharts') %>% 
  # updateMinicharts(-98.841869,
  #               19.285330, 
  #               chartdata = cdmx_dta %>% 
  #                 st_set_geometry(NULL) %>% 
  #                 count(partido_2018,
  #                       sort = T) %>% 
  #                 pivot_wider(values_from = n, 
  #                             names_from = partido_2018),
  #               legend = F,
  #               colorPalette = c("#F5DA57",
  #                                "#874a53",
  #                                "#2d5e29",
  #                                "#5877ac"),
  #               width = 60, height = 60,
  #               layerId = 'Elecciones local 2018') %>% 
  htmlwidgets::onRender("$('#minicharts').click(function(event) {
                        event.preventDefault();
                        if(map.hasLayer(minicharts)) {
                          $(this)map.removeLayer('selected');
                          map.removeLayer(minicharts);
                        } else {
                          map.addLayer(minicharts);        
                          $(this).addClass('selected');
                        }
                        });")

  htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")


mapa_cdmx_15_21
pacman::p_load(leaflet.minicharts)


htmlwidgets::saveWidget(mapa_cdmx_15_21, "02_graficas/mapa_cdmx_15_21.html")

# Gráficas distritos 2018 ----

res_comp_dist_2018 %>% 
  count(partido)

distritos_18_res <- res_comp_dist_2018 %>% 
  # st_set_geometry(NULL) %>% 
  group_by(partido) %>% 
  mutate(dtos_ganados = ifelse(ganador == "Ganador", 1, 0)) %>% 
  summarise(dtos_ganados = sum(dtos_ganados, na.rm = T),
            votos = sum(votos, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T)) %>% 
  arrange(-dtos_ganados) %>% 
  mutate(colores = pal(partido))

## Dráficas por distritos ----
distritos_18_bar <- distritos_18_res %>%
  filter(dtos_ganados > 0) %>% 
  hchart("column", 
         hcaes(x = partido, 
               y = dtos_ganados,
               color = colores),
         colorByPoint = TRUE,
         # dataLabels = list(align = "center",
         #                   enabled = TRUE),
         name = "Dtos.") %>% 
  hc_yAxis(gridLineDashStyle = "shortdash",
           title = list(text = "Dtos. ganados",
                        margin = 10, 
                        align = "high",
                        style = list(
                          fontWeight = "bold"))) %>% 
  hc_xAxis(title = NULL) %>% 
  hc_title( text = "Distritos de mayoría relativa que ganó cada partido",
            margin = 30,
            align = "center",
            style = list(color = "#333333", 
                         fontWeight = "bold",
                         useHTML = TRUE)) %>% 
  hc_subtitle( text = "Elecciones federales de 2018",
               margin = 20,
               align = "center",
               style = list(color = "#7f7f7f",
                            # fontWeight = "bold",
                            useHTML = TRUE)) %>%
  hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))

distritos_18_bar
show_col(alpha("#7f7f7f", 0))

htmlwidgets::saveWidget(distritos_18_bar,
                        "02_graficas/distritos_18_bar.html")

## Dráficas por porcentaje de votación ----

distritos_18_bar_por <- distritos_18_res %>%
  arrange(-votos) %>% 
  mutate(votos_por = round(votos_por*100, 1)) %>% 
  hchart("column", 
         hcaes(x = partido, 
               y = votos_por,
               color = colores),
         # dataLabels = list(align = "center",
         #                   enabled = TRUE),
         name = "Dtos.") %>% 
  hc_yAxis(gridLineDashStyle = "shortdash",
           title = list(text = "Porcentaje de votación",
                        margin = 10, 
                        align = "high",
                        style = list(
                          fontWeight = "bold")),
           labels = list(format = "{value}%")) %>% 
  hc_xAxis(title = NULL) %>% 
  hc_add_theme(hc_theme(chart = list(backgroundColor = "transparent"))) %>% 
  hc_tooltip(
    pointFormat = str_c(
      "<b>",
      "{point.y}%",
      "</b>"
    ),
    useHTML = TRUE
  )

distritos_18_bar_por

htmlwidgets::saveWidget(distritos_18_bar_por,
                        "02_graficas/distritos_18_bar_por.html")

# Gráficas distritos 2021 ----

distritos_21_res <- distritos_21 %>% 
  st_set_geometry(NULL) %>% 
  group_by(partido) %>% 
  mutate(dtos_ganados = ifelse(ganador == "Ganador", 1, 0)) %>% 
  summarise(dtos_ganados = sum(dtos_ganados, na.rm = T),
            votos = sum(votos, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T)) %>% 
  arrange(-dtos_ganados) %>% 
  mutate(colores = pal(partido))



distritos_21_bar <- distritos_21_res %>%
  hchart("column", 
         hcaes(x = partido, 
               y = dtos_ganados,
               color = colores),
         colorByPoint = TRUE,
         # dataLabels = list(align = "center",
         #                   enabled = TRUE),
         name = "Dtos.") %>% 
  hc_yAxis(gridLineDashStyle = "shortdash",
           title = list(text = "Dtos. ganados",
                        margin = 10, 
                        align = "high",
                        style = list(
                          fontWeight = "bold"))) %>% 
  hc_xAxis(title = NULL) %>% 
  hc_title( text = "Distritos de mayoría relativa por el partido al que<br>le corresponde el distrito",
            margin = 30,
            align = "center",
            style = list(color = "#333333", 
                         fontWeight = "bold",
                         useHTML = TRUE)) %>% 
  hc_subtitle( text = "Elecciones federales de 2021",
            margin = 20,
            align = "center",
            style = list(color = "#7f7f7f", 
                         # fontWeight = "bold",
                         useHTML = TRUE)) %>% 
  hc_add_theme(hc_theme(chart = list(backgroundColor = "#ffffff00")))

distritos_21_bar

htmlwidgets::saveWidget(distritos_21_bar,
                        "02_graficas/distritos_21_bar.html")


## Dráficas por porcentaje de votación ----

distritos_21_bar_por <- distritos_21_res %>%
  arrange(-votos_por) %>% 
  mutate(votos_por = round(votos_por*100, 1)) %>% 
  hchart("column", 
         hcaes(x = partido, 
               y = votos_por,
               color = colores),
         # dataLabels = list(align = "center",
         #                   enabled = TRUE),
         name = "Dtos.") %>% 
  hc_yAxis(gridLineDashStyle = "shortdash",
           title = list(text = "Porcentaje de votación",
                        margin = 10, 
                        align = "high",
                        style = list(
                          fontWeight = "bold")),
           labels = list(format = "{value}%")) %>% 
  hc_xAxis(title = NULL) %>% 
  hc_add_theme(hc_theme(chart = list(backgroundColor = "transparent"))) %>% 
  hc_tooltip(
    pointFormat = str_c(
      "<b>",
      "{point.y}%",
      "</b>"
    ),
    useHTML = TRUE
  )

distritos_21_bar_por

htmlwidgets::saveWidget(distritos_21_bar_por,
                        "02_graficas/distritos_21_bar_por.html")


# Sankey de distritos ----
evol_camara <- res_comp_dist_2021 %>% 
  ungroup() %>% 
  filter(ganador == "Ganador") %>% 
  transmute(id_estado, 
            nombre_estado,
            id_distrito,
            nombre_distrito,
            partido_21 = partido) %>% 
  left_join(res_comp_dist_2018 %>% 
              ungroup() %>% 
              filter(ganador == "Ganador") %>% 
              transmute(id_estado, 
                        id_distrito,
                        partido_18 = partido),
            by = c("id_estado", "id_distrito")) %>% 
  count(partido_18,
        partido_21) 

write_xlsx(evol_camara, "04_datos_generados/evol_camara.xlsx")


show_col(colores_partidos)
evol_camara %>% 
  mutate(partido_21 = factor(partido_21, 
                             levels = evol_camara %>% 
                               group_by(partido_21) %>% 
                               summarise(n =sum(n)) %>% 
                               arrange(n) %>% 
                               .$partido_21)) %>% 
  arrange(-n) %>% 
  mutate(partido_21 = paste0(partido_21, " ")) %>% 
  hchart("sankey", 
         name = "Dtos. ganados",
         hcaes(from = partido_18, 
               to = partido_21,
               weight = n), 
         borderWidth = 1,
         color = "#333333") %>% 
  
  hc_add_theme(hc_theme(chart = list(backgroundColor = "transparent",
                                     margin = c(60, 80, 10, 80)))) %>% 
  hc_plotOptions(sankey = list(
    curveFactor = 0.55,
    # centerInCategory = T,
    nodePadding = 2,
    visible = T,
    nodeWidth = 90,
    opacity  = 0.85,
    linkOpacity = 0.55)) %>% 
  hc_colors(colors = c("#874a53",
                       "#874a53",
                       "#5877ac",
                       "#5877ac",
                       "#d14457",
                       "#75529c",
                       "#d14457",
                       "#bcd789",
                       "#2d5e29",
                       "#e89547",
                       "#e89547",
                       "#2d5e29",
                       "#F5DA57",
                       "#F5DA57",
                       "#bcd789",
                       "#6cbcf5")) %>% 
  hc_title(text = "Distribución de distritos ganados por partido (2018-2021)",
            margin = 30,
            align = "center",
            style = list(color = "#333333", 
                         fontSize = 18,
                         fontWeight = "bold",
                         useHTML = TRUE)) %>% 
  hc_subtitle( text = "Distritos de mayoría relativa de la Cámara de Diputados",
               margin = 20,
               align = "center",
               style = list(color = "#7f7f7f", 
                            fontSize = 16,
                            # fontWeight = "bold",
                            useHTML = TRUE)) %>% 

  htmlwidgets::saveWidget("02_graficas/sankey_distritos_18_21.html")
  



library(ggalluvial)
library(stringr)
library(ggrepel)

evol_camara %>% 
  mutate(id_fil = row_number()) %>% 
  pivot_longer(-c(id_fil, n),
               names_to = "eleccion",
               values_to = "grupo") %>% 
  mutate(eleccion = recode(eleccion,
                           "partido_18" = "2018",
                           "partido_21" = "2021")) %>% 
  # ggplot(aes(y = n,
  #            axis1 = partido_18, 
  #            axis2 = partido_21,
  #            fill = partido_18)) +
  ggplot(aes(alluvium = id_fil, 
             x = eleccion,
             y = n,
             stratum = grupo,
             fill = grupo)) +
  geom_stratum(color = NA,
               alpha = 0.65,
               width = 0.25) +
  geom_flow(width = 0.25) +
  scale_fill_manual("",
                    values = colores_partidos) +
  # stat_alluvium(geom = "text", 
  #               aes(label = grupo))
  # geom_text_repel(stat = "stratum",
  #           aes(label = grupo),
  #           force = 1,
  #           direction = "y") +
  mi_tema(legend.position = "top") +
  scale_x_discrete(expand = c(.08, .08)) +
  labs(title = "Ganadores de los distritos de mayoría relativa",
    # subtitle = "Porcentaje de personas de acuerdo con el \nestablecimiento de un gobierno militar",
    x = element_blank(),
    y = element_blank(),
    caption = "Elaboración con datos del Instituto Nacional Electoral"
  )  +
  ggsave(paste0("02_graficas/sankey_distritos",
                ".png"),
         bg = "transparent",
         width = 135,                  # Ancho de la gráfica
         height =120,
         units = "mm")


# Mapa por secciones ----
# Resumen por secciones ----
res_comp_secc_2021 <- computos_distritales_2021 %>% 
  # filter(tipo_acta == "Diputados MR",
  #        contabilizada == 1) %>% 
  group_by(id_estado, nombre_estado,
           id_distrito, nombre_distrito,
           seccion) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>%
  ungroup() %>% 
  select(id_estado, nombre_estado,
         id_distrito, nombre_distrito,
         seccion,
         pan:votos_nulos) %>% 
  pivot_longer(-c(id_estado, nombre_estado,
                  id_distrito, nombre_distrito,
                  seccion),
               names_to = "partido",
               values_to = "votos") %>% 
  mutate(partido = case_when(str_detect(partido, "movimiento") ~ "MC",
                             str_detect(partido, "nueva") ~ "PANAL",
                             str_detect(partido, "encuentro") ~ "PES",
                             T ~ partido),
         partido = str_to_upper(partido)) %>% 
  full_join(computos_candidatos_dist_2021,
            by =c("id_estado" = "estado",
                  "id_distrito" = "distrito",
                  "partido" = "partido_ci")) %>% 
  mutate(candidatura_propietaria = case_when(partido %in% c("CANDIDATO_A_NO_REGISTRADO_A",
                                                            "VOTOS_NULOS") ~ partido,
                                             T ~ candidatura_propietaria)) %>% 
  filter(!is.na(candidatura_propietaria)) %>%
  arrange(id_estado, nombre_estado, id_distrito, nombre_distrito, seccion,
          candidatura_propietaria, partido) %>% 
  group_by(id_estado, nombre_estado, id_distrito, nombre_distrito, seccion,
           candidatura_propietaria) %>% 
  summarise(votos = sum(votos, na.rm = T),
            partido = paste0(partido, collapse = "-")) %>% 
  ungroup() %>% 
  group_by(id_estado, nombre_estado,
           id_distrito, nombre_distrito, seccion) %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T),
         
         lugar = rank(-votos, ties.method = "first"),
         ganador = ifelse(min(lugar, na.rm = T) == lugar &
                            votos > 0 & 
                            seccion > 0, 
                          "Ganador", "Perdedor"),
         partido = case_when(str_detect(partido, "MORENA") &
                               str_detect(partido, "PT") &
                               str_detect(partido, "PVEM") ~ "JHH",
                             str_detect(partido, "PAN") &
                               str_detect(partido, "PRI") &
                               str_detect(partido, "PRD") ~ "VxM",
                             T ~ partido)) %>% 
  arrange(id_estado, nombre_estado, id_distrito, nombre_distrito,
          lugar) %>% 
  mutate(ventaja = ifelse(ganador == "Ganador",
                          votos_por-lead(votos_por), NA),
         partido = recode(partido,
                          "NO_REGISTRADOS" = "NO REGISTRADOS",
                          "CAND_IND_3" = "INDEP.")) %>% 
  ungroup() %>% 
  mutate(nombre_estado = str_to_upper(nombre_estado)) %>% 
  full_join(dtos_coal_21, 
            by = c("nombre_estado",
                   "partido" = "partido_coalicion",
                   "id_distrito" = "id_dtto")) %>% 
  mutate(grupo_parlamentario = ifelse(is.na(grupo_parlamentario),
                          partido, grupo_parlamentario))

  
  
  res_comp_secc_2021 %>% 
    # filter(str_detect(partido, "MC") ) %>% 
    filter(ganador == "Ganador") %>% 
    count(id_estado, nombre_estado,
          id_distrito, nombre_distrito, seccion, 
          sort = T) 
  
  res_comp_secc_2021 %>% 
    # filter(str_detect(partido, "MC") ) %>% 
    filter(ganador == "Ganador") %>% 
    count(partido, 
          sort = T) 

secciones_geo <- st_read("01_datos_brutos/electoral_2020/SECCION.shp") %>%
  clean_names() %>%
  st_transform("+init=epsg:4326") %>% 
  mutate(seccion = sprintf("%04d", seccion)) %>% 
  # rmapshaper::ms_simplify(input = as(distritos, 'Spatial'),
  #                         keep = 0.7) %>%
  # st_as_sf() %>%
  left_join(res_comp_secc_2021 %>% 
              # filter(str_detect(partido, "MC") ) %>% 
              filter(ganador == "Ganador"),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito",
                   "seccion" = "seccion"))

# Resumen municipal ----
secciones_res <-  st_read("01_datos_brutos/electoral_2020/SECCION.shp")  %>% 
  st_set_geometry(NULL) %>% 
  clean_names() %>%
  mutate(seccion = sprintf("%04d", seccion)) %>% 
  left_join(res_comp_secc_2021,
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito",
                   "seccion" = "seccion"))

secciones_mun <- secciones_res %>% 
  group_by(entidad, municipio, partido) %>% 
  summarise(votos = sum(votos, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(entidad, municipio) %>% 
  mutate(votos_por = votos/sum(votos, na.rm = T))


secciones_mun %>% 
  write_xlsx("04_datos_generados/resumen-municipal.xlsx")

secciones_mun %>% count(entidad, municipio)

# mun_geo <- secciones_geo %>% 
#   group_by(municipio) %>% 
#   summarise(seccion) %>% 
#   rmapshaper::ms_simplify(input = as(distritos, 'Spatial')) %>%
#   st_as_sf()
#   
# st_write(mun_geo,
#          "01_datos_brutos/electoral_2020/mun_geo_simp.shp",
#          delete_dsn = T)

## Mapa de resultados secciones 2021 ----
### Leyendas ----
labels_secc_21 <- paste0("<strong>",
                         "Sec. ",
                         as.character(secciones_geo$seccion), ", ",
                    "Dto. ",
                    as.character(secciones_geo$distrito_f), "-", 
                    as.character(secciones_geo$distrito_federal), ", ",
                    "<br>", "<br>",
                    as.character(secciones_geo$estado), 
                    "</strong>", 
                    "<br>", "<b>", "Ganador: ", "</b>", as.character(secciones_geo$partido), 
                    "<br>", "<b>", "Porcentaje de votos: ", "</b>",
                    as.character(percent(secciones_geo$votos_por,
                                         accuracy = 0.1)), 
                    "<br>", "<b>", "Ventaja vs. 2do lugar: ", 
                    "</b>", as.character(percent(secciones_geo$ventaja, 
                                                 accuracy = 0.1)), "<br>") %>% 
  lapply(htmltools::HTML)



mapa_secc_21 <- leaflet(secciones_geo) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    fillColor = ~pal(partido),
    weight = 0.4,
    opacity = 1,
    color = "white",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    label = labels_secc_21,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido, # Values from data frame 
    opacity = 0.95,
    title = "Partido ganador", # Title
    position = "bottomleft") %>% 
  addPolylines(data = mun_geo,
               weight = 1,
               opacity = 1,
               color = "#7f7f7f",
               dashArray = "",
               group = "Municipio") %>% 
  addPolylines(data = entidad_dto,
               weight = 1.5,
               opacity = 1,
               color = "#525252",
               dashArray = "",
               group = "Entidad") 

mapa_secc_21

htmlwidgets::saveWidget(mapa_secc_21, "02_graficas/mapa_secc_21.html")


## Mapa de resultados secciones CdMx 2021 ----
### Leyendas ----
scciones_cdmx <- secciones_geo %>% 
  filter(nombre_estado == "Ciudad de México",
         !is.na(geometry))


labels_secc_cdmx_21 <- paste0("<strong>",
                         "Sec. ",
                         as.character(scciones_cdmx$seccion), ", ",
                         "Dto. ",
                         as.character(scciones_cdmx$distrito_f), "-", 
                         as.character(scciones_cdmx$distrito_federal), ", ",
                         "<br>", 
                         as.character(scciones_cdmx$estado), 
                         "</strong>", 
                         "<br>", "<br>", "<b>", "Ganador: ", "</b>", as.character(scciones_cdmx$partido), 
                         "<br>", "<b>", "Porcentaje de votos: ", "</b>",
                         as.character(percent(scciones_cdmx$votos_por,
                                              accuracy = 0.1)), 
                         "<br>", "<b>", "Ventaja vs. 2do lugar: ", 
                         "</b>", as.character(percent(scciones_cdmx$ventaja, 
                                                      accuracy = 0.1)), "<br>") %>% 
  lapply(htmltools::HTML)


mun_cdmx <- scciones_cdmx %>% 
  group_by(municipio) %>% 
  summarise(seccion = sum(seccion))

edo_cdmx <- scciones_cdmx %>% 
  summarise(seccion = sum(seccion))


mapa_secc_cdmx_21 <- leaflet(scciones_cdmx) %>%
  # Opcion para anadir imagenes o mapas de fondo (tiles)
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom=9, maxZoom=15)) %>% 
  addPolygons(
    fillColor = ~pal(partido),
    weight = 0.4,
    opacity = 1,
    color = "white",
    # dashArray = "3",
    dashArray = "",
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.75,
      bringToFront = T,
      sendToBack = T),
    label = labels_secc_cdmx_21,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addLegend( # Legend options
    pal = pal, # Previously defined palette
    values = ~partido, # Values from data frame 
    opacity = 0.95,
    title = "Partido federal ganador", # Title
    position = "bottomleft") %>% 
  addPolylines(data = mun_cdmx,
               weight = 1,
               opacity = 1,
               color = "#525252",
               dashArray = "",
               group = "Municipio") %>% 
  addPolylines(data = edo_cdmx,
               weight = 1.5,
               opacity = 1,
               color = "#525252",
               dashArray = "",
               group = "Entidad") 

mapa_secc_cdmx_21

htmlwidgets::saveWidget(mapa_secc_cdmx_21, "02_graficas/mapa_secc_cdmx_21.html")

# Gráfica de waffle de la distribución del congreso ----
## Cargar proyección ----
cd <- read_excel("01_datos_brutos/Distribución preoyección JM.xlsx") %>% 
  clean_names() %>% 
  filter(tipo_de_diputacion == "Cámara") %>% 
  mutate(colores = pal(partido))

## Resultados 2021 (highcharter) ----

hchart(cd %>% 
         filter(lxv_legislatura_inicio > 0),
       "item", 
       hcaes(name = partido,
             color = colores,
             y = lxv_legislatura_inicio),
       name = "Curules",
       # marker = list(symbol = "square"),
       showInLegend = TRUE,
       size = "180%",
       center = list("50%", "100%"),
       startAngle = -90,
       endAngle  = 90) %>% 
  hc_title(text = "Distribución de la Cámara de Diputados",
           margin = 30,
           align = "center",
           style = list(color = "#333333", 
                        fontSize = 18,
                        fontWeight = "bold",
                        useHTML = TRUE)) %>% 
  # hc_caption(text = "Elaboración a partir de ",
  #          align = "center",
  #          style = list(color = "#7f7f7f", 
  #                       fontSize = 18,
  #                       # fontWeight = "bold",
  #                       useHTML = TRUE)) %>% 
  hc_subtitle( text = "Proyección a partir del PREP 2021",
               margin = 20,
               align = "center",
               style = list(color = "#7f7f7f",
                            fontSize = 16,
                            # fontWeight = "bold",
                            useHTML = TRUE)) %>%
  hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>') %>% 
  htmlwidgets::saveWidget("02_graficas/camara_distribucion.html")


# ## Evolución de resultados (ggplot) ----  
# install.packages("waffle", repos = "https://cinc.rud.is")
pacman::p_load(waffle)



cd %>% 
  select(-tipo_de_diputacion,-colores) %>% 
  pivot_longer(-partido,
               names_to = "legislatura",
               values_to = "curules") %>% 
  mutate(legislatura = recode(legislatura,
                              "lxiv_legislatura_inicio" = "Conformación inicial de acuerdo con los resultados electorales, 2018",
                              "lxiv_legislatura_final" = "Conformación vigente de las bancadas parlamentarias (previo a las elecciones 2021)",
                              "lxv_legislatura_inicio" = "Conformación inicial de acuerdo con los resultados electorales, 2021"),
         legislatura = factor(legislatura,
                              levels = c("Conformación inicial de acuerdo con los resultados electorales, 2021",
                                         "Conformación vigente de las bancadas parlamentarias (previo a las elecciones 2021)",
                                         "Conformación inicial de acuerdo con los resultados electorales, 2018")),
         partido = recode(partido,
                          "SIN PARTIDO" = "SP"),
         partido = factor(partido,
                          levels = c("MORENA", "PT", "PES", "PVEM", "MC",
                                     "PANAL", "SP", "PRD", "PRI", "PAN", "DIP. RETIRADAS"))) %>% 
  arrange(partido) %>% 
  guardar_bd() %>% 
  # group_by(legislatura) %>% 
  # summarise(suma = sum(curules))
  ggplot() +
  geom_waffle(aes(fill = fct_inorder(partido), values = curules),
              color = "white", size = .25, 
              # radius = unit(, "pt"),
              n_rows = 10, flip = F) +
  geom_segment(data = tibble(x = (250/10),
                             yinicio = 0.5,
                             yfin = c(11.5, 10, 10),
                             legislatura = factor(c("Conformación inicial de acuerdo con los resultados electorales, 2021",
                                                    "Conformación vigente de las bancadas parlamentarias (previo a las elecciones 2021)",
                                                    "Conformación inicial de acuerdo con los resultados electorales, 2018"))),
               mapping = aes(x = x, y = yinicio, 
                             xend = x, yend = yfin),
               size = 1,
               alpha = 0.7,
               color = "white") +
  geom_segment(data = tibble(x = 250/10,
                             yinicio = 0.5,
                             yfin = c(11.5, 10, 10),
                             legislatura = factor(c("Conformación inicial de acuerdo con los resultados electorales, 2021",
                                                    "Conformación vigente de las bancadas parlamentarias (previo a las elecciones 2021)",
                                                    "Conformación inicial de acuerdo con los resultados electorales, 2018"))),
               mapping = aes(x = x, y = yinicio, 
                             xend = x, yend = yfin),
               linetype = "dashed",
               color = "#787878") +
  geom_segment(data = tibble(x = ((500/3)/5),
                             yinicio = 0.5,
                             yfin = c(11.5, 10, 10),
                             legislatura = factor(c("Conformación inicial de acuerdo con los resultados electorales, 2021",
                                                    "Conformación vigente de las bancadas parlamentarias (previo a las elecciones 2021)",
                                                    "Conformación inicial de acuerdo con los resultados electorales, 2018"))),
               mapping = aes(x = x, y = yinicio, 
                             xend = x, yend = yfin),
               size = 1,
               alpha = 0.7,
               color = "white") +
  geom_segment(data = tibble(x = (500/3)/5,
                             yinicio = 0.5,
                             yfin = c(11.5, 10, 10),
                             legislatura = factor(c("Conformación inicial de acuerdo con los resultados electorales, 2021",
                                                    "Conformación vigente de las bancadas parlamentarias (previo a las elecciones 2021)",
                                                    "Conformación inicial de acuerdo con los resultados electorales, 2018"))),
               mapping = aes(x = x, y = yinicio, 
                             xend = x, yend = yfin),
               linetype = "dashed",
               color = "#787878") +
  geom_text(data = tibble(legislatura = factor("Conformación inicial de acuerdo con los resultados electorales, 2021")),
            aes(x = 250/10, y = 12.5,
                label = ""),
            size = 3) +
  geom_text(data = tibble(legislatura = factor("Conformación inicial de acuerdo con los resultados electorales, 2021")),
            aes(x = 250/10, y = 12,
                label = "Mayoría absoluta"),
            size = 2.6) +
  geom_text(data = tibble(legislatura = factor("Conformación inicial de acuerdo con los resultados electorales, 2021")),
            aes(x = (500/3)/5, y = 12,
                label = "Mayoría calificada"),
            size = 2.6) +
  facet_wrap(~legislatura, 
             strip.position = "top",
             # scales = "free_y",
             nrow = 3) +
  # geom_text_repel(data = bd_guardada %>% 
  #                   filter(curules > 0) %>% 
  #                   group_by(legislatura) %>% 
  #                   mutate(partido = paste0(partido, "\n",
  #                                           round(curules), " (",
  #                                           percent(curules/500, 
  #                                                   accuracy = 0.1),
  #                                           ")"),
  #                          curules_tot = ceiling(curules/10),
  #                          curules_cum = ceiling(cumsum(curules)/10),
  #                          curules_centro = (curules_tot/2)+ifelse(is.na(lag(curules_cum)),
  #                                                                  0,
  #                                                                  lag(curules_cum)),
  #                          num = rep(c(2, 1), 100)[1:max(row_number())],
  #                          
  #                          posicion  = case_when(legislatura == "Conformación inicial de acuerdo con los resultados electorales, 2021" ~ 0.5,
  #                                                legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
  #                                                  num == 1 &
  #                                                  str_detect(partido, "SP") ~ 4.5,
  #                                                legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
  #                                                  num == 1 &
  #                                                  str_detect(partido, "PANAL") ~ 1.5,
  #                                                legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
  #                                                  num == 1 ~ 0.5,
  #                                                T ~ 10.5)),
  #                 mapping = aes(x = curules_centro,
  #                               y = posicion,
  #                               color = partido,
  #                               label = ifelse(posicion <= 5,
  #                                              partido, NA)),
  #                 size = 2.5,
  #                 nudge_y = -2,
  #                 direction = "x",
  #                 linetype = "dashed",
  #                 min.segment.length = 0.01,
  #                 color = "grey30") +
  geom_text_repel(data = bd_guardada %>% 
                    filter(curules > 0) %>% 
                    group_by(legislatura) %>% 
                    mutate(partido = paste0(partido, "\n",
                                            round(curules), " (",
                                            percent(curules/500, 
                                                    accuracy = 0.1),
                                            ")"),
                           curules_tot = ceiling(curules/10),
                           curules_cum = ceiling(cumsum(curules)/10),
                           curules_centro = (curules_tot/2)+ifelse(is.na(lag(curules_cum)),
                                                                   0,
                                                                   lag(curules_cum)),
                           curules_centro = case_when(legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" &
                                                        str_detect(partido, "PANAL|SP") ~ curules_centro-0.5,
                                                      legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" &
                                                        str_detect(partido, "PES") ~ curules_centro-1,
                                                      legislatura == "Conformación inicial de acuerdo con los resultados electorales, 2018" &
                                                        str_detect(partido, "PRD") ~ curules_centro-1,
                                                      T ~ curules_centro),
                           num = rep(c(2,1), 100)[1:max(row_number())],
                           
                           posicion  = case_when(legislatura == "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                                   str_detect(partido, "RETIRADAS") ~ 10.5,
                                                 legislatura == "Conformación inicial de acuerdo con los resultados electorales, 2021" ~ 0.5,
                                                 legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                                   num == 1 &
                                                   str_detect(partido, "SP") ~ 4.5,
                                                 legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                                   num == 1 &
                                                   str_detect(partido, "PANAL") ~ 1.5,
                                                 legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                                   num == 1 ~ 0.5,
                                                 T ~ 10.5),
                           horizontal = case_when(posicion > 6 ~ 12.5,
                                                  posicion < 6 &
                                                    str_detect(partido, "PANAL") ~-3.5,
                                                  posicion < 6 &
                                                    str_detect(partido, "SP") ~-6.5,
                                                  T ~ -2)),
                  mapping = aes(x = curules_centro,
                                y = posicion,
                                color = partido,
                                nudge_y = horizontal,
                                label = partido),
                  size = 2.5,
                  # nudge_y = 2,
                  direction = "x",
                  linetype = "dashed",
                  min.segment.length = 0.01,
                  color = "grey30") +
  scale_fill_manual(values =  colores_partidos[c("MORENA","PT",
                                                 "PES","PVEM",
                                                 "MC", "PANAL",
                                                 "SP", "PRD",
                                                 "PRI", "PAN", "DIP. RETIRADAS")]) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.2, 0.05))) +
  # coord_equal() +
  labs(title = "Distribución de la Cámara de Diputados",
       fill = "Grupo \n parlamentario",
       caption = "Elaboración con datos del Instituto Nacional Electoral y la Cámara de Diputados | @MXvsCORRUPCION") +
  # mi_tema() +
  theme_enhance_waffle() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(plot.title = element_text(hjust=0.5, 
                                  size = 14, face = "bold", 
                                  color = "grey20"),
        plot.subtitle = element_text(hjust=0.5,
                                     size = 12,
                                     color = "gray50"),
        plot.caption =  element_text(color = "gray50",
                                     size=10, 
                                     hjust=0),
        legend.position = "top",
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent")
        ) 
  ggsave(paste0("02_graficas/",
                "distribución_cd",
                ".png"),
         bg = "transparent",
         width = 200,                  # Ancho de la gráfica
         height =230,
         units = "mm")

  

  
  
bd_guardada %>% 
  filter(curules > 0) %>% 
  group_by(legislatura) %>% 
  mutate(partido = paste0(partido, "\n",
                          round(curules), " (",
                          percent(curules/500, 
                                  accuracy = 0.1),
                          ")"),
         curules_tot = ceiling(curules/10),
         curules_cum = ceiling(cumsum(curules)/10),
         curules_centro = (curules_tot/2)+ifelse(is.na(lag(curules_cum)),
                                                 0,
                                                 lag(curules_cum)),
         num = rep(c(2,1), 100)[1:max(row_number())],
         
         posicion  = case_when(legislatura == "Conformación inicial de acuerdo con los resultados electorales, 2021" ~ 0.5,
                               legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                 num == 1 &
                                 str_detect(partido, "SP") ~ 4.5,
                               legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                 num == 1 &
                                 str_detect(partido, "PANAL") ~ 1.5,
                               legislatura != "Conformación inicial de acuerdo con los resultados electorales, 2021" & 
                                 num == 1 ~ 0.5,
                               T ~ 10.5),
         horizontal = case_when(posicion > 5 ~ 2,
                                posicion < 5 &
                                  str_detect(partido, "PANAL") ~-3.5,
                                posicion < 5 &
                                  str_detect(partido, "SP") ~-6.5,
                                T ~ -2)) %>% View


# Mapas por secciones por de las ZM del país ----
## Cargar datos de las zm ----

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

## Cruzar con datos ----



secciones_geo <- secciones_geo %>% 
  mutate(cve_ine = paste0(sprintf("%02d", entidad),
                            sprintf("%03d", municipio))) %>% 
  left_join(zm, 
            by = "cve_ine")


## prueba loop ----

zonas_met <- unique(zm$nom_zm)

mun_zm_gen <- secciones_geo %>% 
  group_by(cve_ine, municipio) %>% 
  summarise(seccion = sum(seccion))

for (zona_nom in zonas_met) {
  
  scciones_zm <- secciones_geo %>% 
    filter(nom_zm == zona_nom,
           !is.na(geometry)) %>% 
    mutate(partido = ifelse(is.na(partido) |
                              partido == "NO REGISTRADOS",
                            "DATOS NO REGISTRADOS",
                            partido)) %>% 
    fill(distrito_federal, nom_mun, estado) 
  
  if(!("DATOS NO REGISTRADOS" %in% scciones_zm$partido)) {
    break
  }
  
  
  labels_secc_zm_21 <- paste0("<strong>",
                              "Sec. ",
                              as.character(scciones_zm$seccion), ", ",
                              "<br>", 
                              "Dto. ",
                              as.character(scciones_zm$distrito_f),  "-",
                              as.character(scciones_zm$distrito_federal), ", ",
                              "<br>", 
                              as.character(scciones_zm$nom_mun), ", ", 
                              as.character(scciones_zm$estado), 
                              "<br>") %>% 
    lapply(htmltools::HTML)
  
  popup_secc_zm_21 <- paste0("<strong>",
                             "Sec. ",
                             as.character(scciones_zm$seccion), ", ",
                             "Dto. ",
                             as.character(scciones_zm$distrito_f), "-", 
                             as.character(scciones_zm$distrito_federal), ", ",
                             "<br>", 
                             as.character(scciones_zm$nom_mun), ", ", 
                             as.character(scciones_zm$estado), 
                             "</strong>", 
                             "<br>", "<b>", "Ganador: ", "</b>", 
                             as.character(scciones_zm$partido), 
                             " (",
                             as.character(percent(scciones_zm$votos_por,
                                                  accuracy = 0.1)),
                             ")",
                             # "<br>", "<b>", "Porcentaje de votos: ", "</b>",
                             # as.character(percent(scciones_zm$votos_por,
                             #                      accuracy = 0.1)), 
                             "<br>", "<b>", "Ventaja vs. 2do lugar: ",
                             "</b>", as.character(percent(scciones_zm$ventaja,
                                                          accuracy = 0.1)),
                             "<br>") %>% 
    lapply(htmltools::HTML)
  
  
  mun_zm <- mun_zm_gen %>% 
    filter(cve_ine %in% unique(scciones_zm$cve_ine)) 
  
  edo_zm <- entidad_dto %>% 
    filter(entidad %in% unique(scciones_zm$entidad)) 
  
  edo_zm_lines <- edo_zm %>% 
    nngeo::st_segments() %>% 
    st_filter(mun_zm, .predicate = st_intersects)
  
  
  mapa_secc_zm_21 <- leaflet(scciones_zm) %>%
    # Opcion para anadir imagenes o mapas de fondo (tiles)
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(
      fillColor = ~pal(partido),
      weight = 0.4,
      opacity = 1,
      color = "white",
      # dashArray = "3",
      dashArray = "",
      fillOpacity = 0.75,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.75,
        bringToFront = T,
        sendToBack = T),
      popup = popup_secc_zm_21,
      label = labels_secc_zm_21,
      popupOptions = labelOptions(
        interactive = F,
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "10px",
        direction = "auto"),
      labelOptions = labelOptions(
        interactive = F,
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "8px",
        opacity = 0.7,
        direction = "auto")) %>% 
    addLegend( # Legend options
      pal = pal, # Previously defined palette
      values = ~partido, # Values from data frame 
      opacity = 0.95,
      title = "Partido federal ganador", # Title
      position = "bottomleft") %>% 
    addPolylines(data = mun_zm,
                 weight = 1,
                 opacity = 1,
                 color = "#969696",
                 dashArray = "",
                 group = "Municipio") %>% 
    # addPolylines(data = edo_zm,
    #              weight = 1.5,
    #              opacity = 1,
    #              color = "#525252",
    #              dashArray = "",
    #              group = "Entidad") 
    addPolylines(data = edo_zm_lines,
                 weight = 1.5,
                 opacity = 1,
                 color = "#525252",
                 dashArray = "",
                 group = "Entidad") 
  
  
  htmlwidgets::saveWidget(mapa_secc_zm_21, paste0("02_graficas/Mapas ZM/mapa_secc_",
                                                  zona_nom,
                                                  ".html"))
  
}


