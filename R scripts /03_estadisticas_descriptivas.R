library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)
library(sf)
library(rnaturalearth)
library(dplyr)
library(geodata)

#Leer arhico excel desempeños_filtrado_matematicas
matematica_v1 <- read_excel("raw/ df_matematica_final.xlsx")

#Leer arhico excel desempeños_filtrado_lengua
lengua_v1 <- read_excel("raw/ df_lengua_final.xlsx")

#grafico de barras porcentual por desempeño (matematica)

#suma y agrupacion por jurisdiccion y desempeño
matematica_consolidado_jurisdiccion <- matematica_v1 %>%
  group_by(jurisdiccion) %>%
  summarise(
    Debajo = sum(Debajo, na.rm = TRUE),
    Basico = sum(Basico, na.rm = TRUE),
    Satisfactorio = sum(Satisfactorio, na.rm = TRUE),
    Avanzado = sum(Avanzado, na.rm = TRUE),
    .groups = "drop"
  )

#cambio de estrutura de filas y columnas
matematica_consolidado_jurisdiccion_v1 <- matematica_consolidado_jurisdiccion %>%
  pivot_longer(
    cols = c(Debajo, Basico, Satisfactorio, Avanzado),
    names_to = "Nivel",
    values_to = "Cantidad"
  )

#calculo de %
matematica_consolidado_jurisdiccion_v1 <- matematica_consolidado_jurisdiccion_v1 %>%
  group_by(jurisdiccion) %>%
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100, 2)) %>%
  ungroup()

#grafico por desempeño (matematica)
ggplot(matematica_consolidado_jurisdiccion_v1, aes(x = reorder(jurisdiccion, jurisdiccion), y = Porcentaje, fill = Nivel)) +
  geom_col(position = "stack", color = "white") +
  geom_text(aes(label = ifelse(Porcentaje > 5, paste0(Porcentaje, "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 2, color = "black") +
  labs(
    title = "Composición porcentual por nivel de desempeño en Matemática",
    subtitle = "Porcentaje de estudiantes por provincia",
    x = "Provincia",
    y = "Porcentaje de estudiantes",
    fill = "Nivel de desempeño"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "top"
  )

#grafico de barras porcentual por desempeño (lengua)

#suma y agrupacion por jurisdiccion y desempeño
lengua_consolidado_jurisdiccion <- lengua_v1 %>%
  group_by(jurisdiccion) %>%
  summarise(
    Debajo = sum(Debajo, na.rm = TRUE),
    Basico = sum(Basico, na.rm = TRUE),
    Satisfactorio = sum(Satisfactorio, na.rm = TRUE),
    Avanzado = sum(Avanzado, na.rm = TRUE),
    .groups = "drop"
  )

#cambio de estrutura de filas y columnas
lengua_consolidado_jurisdiccion_v1 <- lengua_consolidado_jurisdiccion %>%
  pivot_longer(
    cols = c(Debajo, Basico, Satisfactorio, Avanzado),
    names_to = "Nivel",
    values_to = "Cantidad"
  )

#calculo de %
lengua_consolidado_jurisdiccion_v1 <- lengua_consolidado_jurisdiccion_v1 %>%
  group_by(jurisdiccion) %>%
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100, 2)) %>%
  ungroup()

#grafico por desempeño (lengua)
ggplot(lengua_consolidado_jurisdiccion_v1, aes(x = reorder(jurisdiccion, jurisdiccion), y = Porcentaje, fill = Nivel)) +
  geom_col(position = "stack", color = "white") +
  geom_text(aes(label = ifelse(Porcentaje > 5, paste0(Porcentaje, "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 2, color = "black") +
  labs(
    title = "Composición porcentual por nivel de desempeño en Matemática",
    subtitle = "Porcentaje de estudiantes por provincia",
    x = "Provincia",
    y = "Porcentaje de estudiantes",
    fill = "Nivel de desempeño"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "top"
  )

#grafico de comparacion privado vs publico (matematica)

#suma y agrupacion por sector
matematica_consolidado_sector <- matematica_v1 %>%
  group_by(sector) %>%
  summarise(
    Debajo = sum(Debajo, na.rm = TRUE),
    Basico = sum(Basico, na.rm = TRUE),
    Satisfactorio = sum(Satisfactorio, na.rm = TRUE),
    Avanzado = sum(Avanzado, na.rm = TRUE),
    .groups = "drop"
  )

#pivot_longer para pasar de ancho a largo
matematica_consolidado_sector_v1 <- matematica_consolidado_sector %>%
  pivot_longer(cols = -sector, names_to = "nivel", values_to = "cantidad")

#calcular porcentaje dentro de cada sector
matematica_consolidado_sector_v1 <- matematica_consolidado_sector_v1 %>%
  group_by(sector) %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) %>%
  ungroup()

#grafico por sector
ggplot(matematica_consolidado_sector_v1, aes(x = sector, y = porcentaje, fill = nivel)) +
  geom_col(position = "stack", color = "white") +
  geom_text(
    aes(label = ifelse(porcentaje > 5, paste0(porcentaje, "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Distribución porcentual del desempeño por sector en Matematica",
    subtitle = "Comparación del desempeño entre escuelas estatales y privadas",
    x = "Sector escolar",
    y = "Porcentaje de estudiantes",
    fill = "Nivel de desempeño"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  )

#grafico de comparacion privado vs publico (lengua)

#suma y agrupacion por sector
lengua_consolidado_sector <- lengua_v1 %>%
  group_by(sector) %>%
  summarise(
    Debajo = sum(Debajo, na.rm = TRUE),
    Basico = sum(Basico, na.rm = TRUE),
    Satisfactorio = sum(Satisfactorio, na.rm = TRUE),
    Avanzado = sum(Avanzado, na.rm = TRUE),
    .groups = "drop"
  )

#pivot_longer para pasar de ancho a largo
lengua_consolidado_sector_v1 <- lengua_consolidado_sector %>%
  pivot_longer(cols = -sector, names_to = "nivel", values_to = "cantidad")

#calculo de porcentaje dentro de cada sector
lengua_consolidado_sector_v1 <- lengua_consolidado_sector_v1 %>%
  group_by(sector) %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) %>%
  ungroup()

#grafico por sector (lengua)
ggplot(lengua_consolidado_sector_v1, aes(x = sector, y = porcentaje, fill = nivel)) +
  geom_col(position = "stack", color = "white") +
  geom_text(
    aes(label = ifelse(porcentaje > 4, paste0(porcentaje, "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Distribución porcentual del desempeño por sector en Lengua",
    subtitle = "Comparación del desempeño entre escuelas estatales y privadas",
    x = "Sector escolar",
    y = "Porcentaje de estudiantes",
    fill = "Nivel de desempeño"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  )

#grafico de comparacion urbano vs rural (matematica) 

#suma y agrupacion por ambito
matematica_consolidado_ambito <- matematica_v1 %>%
  group_by(ambito) %>%
  summarise(
    Debajo = sum(Debajo, na.rm = TRUE),
    Basico = sum(Basico, na.rm = TRUE),
    Satisfactorio = sum(Satisfactorio, na.rm = TRUE),
    Avanzado = sum(Avanzado, na.rm = TRUE),
    .groups = "drop"
  )

#pivot_longer para pasar de ancho a largo
matematica_consolidado_ambito_v1 <- matematica_consolidado_ambito %>%
  pivot_longer(cols = -ambito, names_to = "nivel", values_to = "cantidad")

#calcular porcentaje dentro de cada ambito
matematica_consolidado_ambito_v1 <- matematica_consolidado_ambito_v1 %>%
  group_by(ambito) %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) %>%
  ungroup()

#grafico por ambito (matematica)
ggplot(matematica_consolidado_ambito_v1, aes(x = ambito, y = porcentaje, fill = nivel)) +
  geom_col(position = "stack", color = "white") +
  geom_text(
    aes(label = ifelse(porcentaje > 5, paste0(porcentaje, "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Distribución porcentual del desempeño por ambito en Matematica",
    subtitle = "Comparación entre escuelas urbanas y rurales",
    x = "Ambito escolar",
    y = "Porcentaje de estudiantes",
    fill = "Nivel de desempeño"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  )

#grafico de comparacion urbano vs rural (lengua) 

#suma y agrupacion por ambito
lengua_consolidado_ambito <- lengua_v1 %>%
  group_by(ambito) %>%
  summarise(
    Debajo = sum(Debajo, na.rm = TRUE),
    Basico = sum(Basico, na.rm = TRUE),
    Satisfactorio = sum(Satisfactorio, na.rm = TRUE),
    Avanzado = sum(Avanzado, na.rm = TRUE),
    .groups = "drop"
  )

#pivot_longer para pasar de ancho a largo
lengua_consolidado_ambito_v1 <- lengua_consolidado_ambito %>%
  pivot_longer(cols = -ambito, names_to = "nivel", values_to = "cantidad")

#calculo de porcentaje dentro de cada ambito
lengua_consolidado_ambito_v1 <- lengua_consolidado_ambito_v1 %>%
  group_by(ambito) %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) %>%
  ungroup()

#grafico por ambito (lengua)
ggplot(lengua_consolidado_ambito_v1, aes(x = ambito, y = porcentaje, fill = nivel)) +
  geom_col(position = "stack", color = "white") +
  geom_text(
    aes(label = ifelse(porcentaje > 5, paste0(porcentaje, "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Distribución porcentual del desempeño por ambito (Lengua)",
    subtitle = "Comparación entre escuelas urbanas y rurales",
    x = "Ambito escolar",
    y = "Porcentaje de estudiantes",
    fill = "Nivel de desempeño"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  )

#mapa de calor de provincias por desempeño (matematica)

#Obtenemos las provincias como sf (descargamos el mapa)
argentina_sf <- ne_states(country = "Argentina", returnclass = "sf")

#asiganmos valores a cada nivel de desempeño
matematica_mapa <- matematica_consolidado_jurisdiccion_v1 %>%
  mutate(puntaje = case_when(
    Nivel == "Debajo" ~ 1,
    Nivel == "Basico" ~ 2,
    Nivel == "Satisfactorio" ~ 3,
    Nivel == "Avanzado" ~ 4
  ))

#sacamos una columna que no usamos
matematica_mapa <- matematica_mapa %>% select(-Porcentaje)

#calculo de promedio
matematica_mapa_v2 <- matematica_mapa %>%
  group_by(jurisdiccion) %>%
  summarise(
    promedio = sum(Cantidad * puntaje) / sum(Cantidad)
  )

#cambios de nombre requerido en nuestro data frame para compatibilizar con el mapa
matematica_mapa_v2 <- matematica_mapa_v2 %>%
  mutate(jurisdiccion = case_when(
    jurisdiccion == "CABA" ~ "Ciudad de Buenos Aires",
    jurisdiccion == "Tierra del fuego" ~ "Tierra del Fuego",
    TRUE ~ jurisdiccion
  ))

#modificamos el nombre de la columna name para compatibilizar con nuestro data frame
argentina_sf <- argentina_sf %>%
  rename(jurisdiccion = name)

# Unimos mapa con el data frame
matematica_mapa_final <- left_join(argentina_sf, matematica_mapa_v2, by = "jurisdiccion")

#grafico
ggplot(matematica_mapa_final) +
  geom_sf(aes(fill = promedio), color = "white") +
  scale_fill_gradient(
    low = "#e5f5e0",  # verde muy claro
    high = "#006d2c", # verde oscuro
    name = "Promedio desempeño"
  ) +
  labs(
    title = "Promedio de desempeño en Matemática por provincia",
    subtitle = "Debajo = 1, Básico = 2, Satisfactorio = 3, Avanzado = 4",
    caption = "Fuente: Elaboración propia en base a pruebas aprender 2023"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

#mapa de calor de provincias por desempeño (matematica)

#Obtenemos las provincias como sf (descargamos el mapa)
argentina_sf <- ne_states(country = "Argentina", returnclass = "sf")

#asiganmos valores a cada nivel de desempeño
lengua_mapa <- lengua_consolidado_jurisdiccion_v1 %>%
  mutate(puntaje = case_when(
    Nivel == "Debajo" ~ 1,
    Nivel == "Basico" ~ 2,
    Nivel == "Satisfactorio" ~ 3,
    Nivel == "Avanzado" ~ 4
  ))

#sacamos una columna que no usamos
lengua_mapa <- lengua_mapa %>% select(-Porcentaje)

#calculo de promedio
lengua_mapa_v2 <- lengua_mapa %>%
  group_by(jurisdiccion) %>%
  summarise(
    promedio = sum(Cantidad * puntaje) / sum(Cantidad)
  )

#cambios de nombre requerido en nuestro data frame para compatibilizar con el mapa
lengua_mapa_v2 <- lengua_mapa_v2 %>%
  mutate(jurisdiccion = case_when(
    jurisdiccion == "CABA" ~ "Ciudad de Buenos Aires",
    jurisdiccion == "Tierra del fuego" ~ "Tierra del Fuego",
    TRUE ~ jurisdiccion
  ))

#modificamos el nombre de la columna name para compatibilizar con nuestro data frame
argentina_sf <- argentina_sf %>%
  rename(jurisdiccion = name)

# Unimos mapa con el data frame
lengua_mapa_final <- left_join(argentina_sf, lengua_mapa_v2, by = "jurisdiccion")

#grafico de mapa
ggplot(lengua_mapa_final) +
  geom_sf(aes(fill = promedio), color = "white") +
  scale_fill_gradient(
    low = "#e5f5e0",  # verde muy claro
    high = "#006d2c", # verde oscuro
    name = "Promedio desempeño"
  ) +
  labs(
    title = "Promedio de desempeño en Matemática por provincia",
    subtitle = "Debajo = 1, Básico = 2, Satisfactorio = 3, Avanzado = 4",
    caption = "Fuente: Elaboración propia en base a pruebas aprender 2023"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )
