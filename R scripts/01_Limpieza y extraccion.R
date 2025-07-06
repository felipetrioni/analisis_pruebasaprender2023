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

#extraccion de datos
desempeño_lengua <- read_excel("raw/2023 Desempeños de lengua.xlsx")

desempeño_filtrado_lengua <- desempeño_lengua[, c(1:12, 981:1022)]

desempeño_filtrado_lengua <- desempeño_filtrado_lengua[, colSums(!is.na(desempeño_filtrado_lengua)) > 0]

desempeño_matematicas <- read_excel("raw/2023 Desempeños de matematica.xlsx")

desempeño_filtrado_matematicas <- desempeño_matematicas[, c(1:12, 981:1022)]

desempeño_filtrado_matematicas <- desempeño_filtrado_matematicas [, colSums(!is.na(desempeño_filtrado_matematicas)) > 0]

#Limpieza de datos

#redondeo de todas las celdas
df <- desempeño_filtrado_lengua  
df_numerico <- df[, sapply(df, is.numeric)]
df_redondeado <- as.data.frame(t(apply(df_numerico, 1, function(fila) round(fila, 2))))
colnames(df_redondeado) <- colnames(df_numerico)
rownames(df_redondeado) <- rownames(df)
desempeño_filtrado_lengua[, colnames(df_numerico)] <- df_redondeado

#redondeo de todas las celdas
df <- desempeño_filtrado_matematicas
df_numerico <- df[, sapply(df, is.numeric)]
df_redondeado <- as.data.frame(t(apply(df_numerico, 1, function(fila) round(fila, 2))))
colnames(df_redondeado) <- colnames(df_numerico)
rownames(df_redondeado) <- rownames(df)
desempeño_filtrado_matematicas[, colnames(df_numerico)] <- df_redondeado

#Cambio de nombres de columnas de desempeño (matematica)
names(desempeño_filtrado_matematicas)[10:13] <- c("Debajo", "Basico", "Satisfactorio", "Avanzado")

#Cambio de nombres de columnas de desempeño (lengua)
names(desempeño_filtrado_lengua)[10:13] <- c("Debajo", "Basico", "Satisfactorio", "Avanzado")

#Cambio de nombres de jurisdicciones para facilidad
desempeño_filtrado_matematicas$jurisdiccion[
  desempeño_filtrado_matematicas$jurisdiccion == "Ciudad Autónoma de Buenos Aires"
] <- "CABA"

desempeño_filtrado_matematicas$jurisdiccion[
  desempeño_filtrado_matematicas$jurisdiccion == "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
] <- "Tierra del fuego"

desempeño_filtrado_lengua$jurisdiccion[
  desempeño_filtrado_lengua$jurisdiccion == "Ciudad Autónoma de Buenos Aires"
] <- "CABA"

desempeño_filtrado_lengua$jurisdiccion[
  desempeño_filtrado_lengua$jurisdiccion == "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
] <- "Tierra del fuego"

#Crear nuevos excels con datos filtrados
write_xlsx(desempeño_filtrado_lengua, "raw/ df_lengua_final.xlsx")
write_xlsx(desempeño_filtrado_matematicas, "raw/ df_matematica_final.xlsx")
