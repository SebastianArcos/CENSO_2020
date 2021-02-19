
getwd()

# librerias ---------------------------------------------------------------
library(tidyverse)
library(extrafont)
extrafont::fonts()

# directorios -------------------------------------------------------------
censo_personas <- read_csv("Inputs/cpv2020_cb_personas_ej.CSV") %>% 
  janitor::clean_names() # convertir a minusculas

diccionario_censo <- readxl::read_excel("Documentos/Censo2020_CPV_CB_descriptor_bd_ejemplo.xlsx",
                                        sheet = "PERSONAS", 
                                        skip = 5) %>% 
  janitor::clean_names() # convertir a minusculas

# limpieza ----------------------------------------------------------------
diccionario_censo <-
diccionario_censo %>% 
  filter(!is.na(descripcion),
         !is.na(pregunta_y_categoria)) %>% 
  select(descripcion, variable = mnemonico) %>% 
  mutate(variable = str_to_lower(variable)) %>% 
  write_csv("Outputs processed/catalogo_variables.csv")

diccionario_censo

# valores faltantes -------------------------------------------------------
valores_faltantes <-
  censo_personas %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "missing") %>%
  left_join(diccionario_censo) %>%
  mutate(descripcion = fct_reorder(descripcion, missing),
         missing = missing / 1e6) %>%  # 1 exponente 6
  write_csv("Outputs processed/valores_faltantes.csv")
  
ggplot(valores_faltantes) +
  geom_col(aes(x = missing,
               y = descripcion),
           fill = "#cecece",
           color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "La variable con mayor cantidad de valores faltantes es la afiliación a la seguridad social",
       x = "Valores nulos\n(millones de observaciones)",
       y = NULL,
       caption = "Censo 2020 de INEGI") +
  ggthemes::theme_clean() +
  theme(text = element_text(".New York")) +
  ggsave("Gráficas/Valores_faltantes.png", height = 10, width = 20)

# eda ---------------------------------------------------------------------
censo_personas %>% 
  summarise(total = n_distinct(id_persona))