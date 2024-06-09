
##################################################################
##            Project: Análisis de diputaciones 2024            ##
##################################################################
##
## Description:    Este script analiza los resultados de la CD
##                 para las elecciones de 2024.
##
## Author:         Javier Mtz.-Rdz.  
##
## Creation date:  2024-06-06
##
## Email:          javier.mr@stat.ubc.ca
##
## ---------------------------
## Notes:          
## ---------------------------

# Setup ----
## Packages to use ----
pacman::p_load(tidyverse, janitor, writexl, 
              readxl, scales, mytidyfunctions, 
              presupuestoR)

## Set theme ------
theme_set(theme_jmr(text = element_text(family = "Lato")))

options(ggplot2.discrete.colour = paletas_jmr$general,
        ggplot2.discrete.fill = paletas_jmr$general)

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)

# Definir partidos y colores ----
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

# Cargar datos -----

## Cargar catálogos ------
cat_dip <- read_xlsx("01_datos_brutos/20240503_CORTE_CASILLAS_PEF24.xlsx") %>% 
  clean_names()

### Create match estado to id_estado
ent_to_id_db <- cat_dip %>% 
  count(estado, estado_descripcion) %>% 
  bind_rows(tibble(estado = 19, estado_descripcion = "Nuevo León")) %>% 
  transmute(id_entidad = estado, 
            entidad = entidad_to_abr2(estado_descripcion))

ent_to_id <- function(ent) {
  y <- entidad_to_abr2(ent)
  
  n <- length(y)
  
  ent_id <- vector(length = n)
  
  for (i in 1:n) {
    ent_id[i] <- ent_to_id_db$id_entidad[y[i]==ent_to_id_db$entidad][1]
  }
  
  return(sprintf("%02d", ent_id))
}

## Cargar PREP ------
prep_dip <- read_csv("01_datos_brutos/20240603_2005_PREP/20240603_2005_PREP_DIP_FED/DIP_FED_2024.csv", skip = 5) %>% 
  clean_names()

## Cargar candidatos ------
candi_dip <- read_csv("01_datos_brutos/20240603_2005_PREP/20240603_2005_PREP_DIP_FED/DIP_FED_CANDIDATURAS_2024.csv") %>% 
  clean_names() %>% 
  mutate(id_end_dist = paste0(id_entidad, id_distrito_federal)) %>% 
  group_by(id_end_dist, candidatura_propietaria) %>% 
  summarise(partido_ci = paste0(partido_ci, collapse = "_"),
            partido_ci = make_clean_names(partido_ci)) %>% 
  mutate(partido_ci = case_when(str_detect(partido_ci, "morena") &
                               str_detect(partido_ci, "pvem") &
                               str_detect(partido_ci, "pt")~ "pvem_pt_morena",
                             str_detect(partido_ci, "pan") &
                               str_detect(partido_ci, "prd") &
                               str_detect(partido_ci, "pri")~ "pan_prd_pri",
                             T ~ partido_ci)) 


## Cargar datos coaliciones ----

asig <- read_xlsx("04_datos_generados/Convenios de coalición - Diputados federales y senado - AYT.xlsx",
                     skip = 1) %>%
  clean_names() %>%
  pivot_longer(c(asignacion_coalicion_shh_morena_pt_pvem,
                 asignacion_fcm),
               names_to = "partido_asgn",
               values_to = "partido_coal") %>% 
  transmute(id_end_dist = paste0(ent_to_id(estado), sprintf("%03d", distrito)),
            partido_asgn = case_match(partido_asgn,
                                      "asignacion_coalicion_shh_morena_pt_pvem" ~ "pvem_pt_morena",
                                      "asignacion_fcm" ~ "pan_prd_pri"), 
            partido_coal) %>%
  filter(!is.na(partido_coal))


  

dto_shh <- candi_dip %>% 
  filter(partido_ci == "pvem_pt_morena") %>% 
  pull(id_end_dist)

dto_fcm <- candi_dip %>% 
  filter(partido_ci == "pan_prd_pri") %>% 
  pull(id_end_dist)

asig %>% 
  filter(partido_asgn == "pan_prd_pri") %>% 
  pull(id_end_dist) %>% 
  .[!. %in% dto_fcm]

dto_fcm[!dto_fcm %in% (asig %>% 
                         filter(partido_asgn == "pan_prd_pri") %>% 
                         pull(id_end_dist))]


# Procesamiento de datos por distrito ----

sum_dist <- prep_dip %>% 
  # Remover votos no contados para MR
  filter(tipo_acta != "4ERP",
         contabilizada == 1) %>% 
  # Seleccionar datos importantes
  select(clave_casilla,
         id_entidad, entidad, 
         id_distrito_federal, distrito_federal,
         pan:total_votos_asentado) %>% 
  # Crear claves de match
  mutate(id_entidad = sprintf("%02d", id_entidad),
         id_end_dist = paste0(id_entidad, id_distrito_federal)) %>% 
  # Hacer colúmna por partido
    pivot_longer(pan:nulos,
                 names_to = "partido",
                 values_to = "votos") %>% 
  # Aglutinar votos por partido
  mutate(partido_asgn = case_when(str_detect(partido,
                                     "morena|pt|pvem") &
                            id_end_dist %in% dto_shh ~ "pvem_pt_morena",
                            str_detect(partido,
                                       "pan|pri|prd") &
                              id_end_dist %in% dto_fcm ~ "pan_prd_pri",
                          T ~ partido),
         votos = as.numeric(votos),
         total_votos_asentado = as.numeric(total_votos_asentado)) %>% 
  # group_by(id_entidad, id_distrito_federal) %>% 
  left_join(asig,
            join_by(id_end_dist, partido_asgn)) %>% 
  mutate(partido_coal = ifelse(is.na(partido_coal),
                               str_to_upper(partido_asgn), str_to_upper(partido_coal))) %>% 
  group_by(id_end_dist, partido_asgn, partido_coal) %>%
  summarise(total_votos_asentado = first(total_votos_asentado, na_rm = T),
            votos = sum(as.numeric(votos), na.rm = T)) 

sum_dist %>% 
  group_by(id_end_dist) %>% 
  top_n(1, votos) %>% 
  ungroup() %>% 
  count(partido_coal,
        sort = T)
