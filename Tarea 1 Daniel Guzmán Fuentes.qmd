---
title: "Tarea 1"
author: "Daniel Heriberto Guzmán Fuentes "
format: html
editor: visual
---

```{r}
#| code-fold: true
#| warning: false
#| message: false



library(rstudioapi)
library(tidyverse)
library(sf)          
library(viridis)     
library(janitor)    
library(scales)
library(ggplot2)

```

# Objetivo

El objetivo de este documento es realizar un análisis exploratorio de datos (EDA) utilizando técnicas de estadística descriptiva y visualización gráfica. Nos enfocaremos en variables cuantitativas, una a la vez, aplicando diversas representaciones gráficas para comprender mejor la distribución y características de los datos.

Uno de los primeros temas que exploraremos en la estadística inferencial será la estimación de parametros, por lo que es fundamental entender cómo se comportan las variables en una muestra.

# Datos espaciales de Baja California

```{r municipios}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

#municipios_Baja_California <- st_read("C:/Users/danie/Documents/DATA/02mun.shp", options = "ENCODING=LATIN1", quiet = TRUE)
municipios_Baja_California <- st_read("C:/Users/danie/Documents/DATA/02mun.shp", options = "ENCODING=LATIN1", quiet = TRUE)

# Crear mapa de municipios de Baja_California
mapa_municipios <- ggplot(municipios_Baja_California) +
  geom_sf(fill = "lightgreen", color = "white", size = 0.3) +
  labs(
    title = "Municipios de Baja California",
    subtitle = paste("Total de municipios:", nrow(municipios_Baja_California)),
    caption = "Fuente: INEGI - Marco Geoestadístico"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )

print(mapa_municipios)
```

# Lectura de datos

Carguemos los datos de población y vivienda para el año 2020, filtrando por algunos indicadores relevantes:

-Tasa de alfabetización de las personas de 15 a 24 años -Tamaño promedio de los hogares con jefe mujer -Porcentaje de hombres de 75 a 79 años -Porcentaje de viviendas con piso de tierra -Población no católica

```{r}
#| code-fold: true
#| warning: false
#| message: false


datos_total <- read_csv("C:/Users/danie/Documents/DATA/cpv_valor_02.csv", na = c("ND"))

datos_total <- datos_total |> filter(año == 2020)
#unique(datos_total$indicador)
datos_seleccion <- datos_total |> 
  filter(indicador %in% c("Población de 6 a 14 años que sabe leer y escribir", "Porcentaje de la población con discapacidad por accidente" ,"Tasa de alfabetización de las personas de 15 a 24 años", "Promedio de ocupantes por dormitorio", "Población no católica"  ), desc_municipio != "Estatal") |> 
  select(desc_municipio, indicador, valor)
```

Cambiamos el formato de los datos para que cada indicador sea una columna separada, facilitando el análisis posterior. Además, limpiamos los nombres de las columnas para que sean más manejables y creamos una columna `NOMGEO` para poder unir los datos espaciales con los datos de población y vivienda.

```{r}
#| code-fold: true
#| warning: false
#| message: false

datos <- datos_seleccion |>
    pivot_wider(names_from = indicador, values_from = valor) 

datos <- datos |> 
  clean_names() 

# Corrección de nombres de municipios

datos <- datos |> 
  mutate(desc_municipio = if_else(desc_municipio == "Mexicali", "Mexicali", desc_municipio))

datos <- datos |> 
  mutate(NOMGEO = desc_municipio)
```

# Estadísticas descriptivas

Ahora que tenemos nuestros datos preparados, podemos realizar un resumen numérico de las variables cuantitativas. Para ello, utilizaremos la función `summary()` de R, que nos proporcionará un resumen estadístico de cada variable, incluyendo la media, mediana, mínimo, máximo y cuartiles. Hay que agregar las demás medidas de dispersión analizadas en clase (desviación media, varianza, rango, etc.) para cada variable cuantitativa.

```{r}
#| code-fold: true

summary(datos)
```

Se unen los datos geograficos de los municipios de Baja California con los datos de población y vivienda. Esto nos permitirá crear mapas y realizar análisis espaciales. Para preservar la integridad de los datos, utilizamos `left_join()` de `dplyr`, el primer conjunto de datos debe ser el que contenga la columna `geometry`.

```{r}
#| code-fold: true

datos <- left_join(municipios_Baja_California, datos)
```

# Tasa de alfabetización de las personas de 15 a 24 años

```{r}
#| code-fold: true
#| fig-width: 8
#| fig-align: center


ggplot(datos) +
  geom_sf(aes(fill = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Tasa alfabetizacion 15 a 24 años"
  ) +
  labs(
    title = "Tasa de alfabetización de las personas de 15 a 24 años
a en los municipios de Baja California ",
    caption = "Fuente: INEGI - Marco Geoestadístico"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "right"
  )

```

## Histograma

```{r}
str(datos$Tasa_de_alfabetización_de_las_personas_de_15_a_24_anos)
```

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_histogram(
    aes(x = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos), 
    color = "black", fill = "yellow", 
    boundary = min(datos$tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos, na.rm = TRUE), 
    binwidth = 0.1
  ) +
  labs(
    title = "Distribución de la Tasa de Alfabetización (15 a 24 años)",
    x = "Tasa de alfabetización (%)", 
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )

```

## Gráfica de densidad

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_density(aes(tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos), color="black", fill = "blue", alpha=0.6) +
  labs(
    title = "Distribución de la Edad",
    x = "Edad (%)", y = "Densidad"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )


```

## Boxplot

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos)) +
    geom_boxplot(
    fill = "#4A90E2",           # Color de relleno
    color = "#2C5F7A",          # Color del borde más oscuro
    alpha = 0.8,                # Transparencia
    outlier.color = "#E74C3C",  # Color rojo para outliers
    outlier.size = 2.5,         # Tamaño de outliers
    outlier.alpha = 0.7,        # Transparencia de outliers
    width = 0.5,                # Ancho del boxplot
    notch = TRUE,               # Muescas para intervalo de confianza
    notchwidth = 0.7            # Ancho de las muescas
  ) +
  geom_jitter(alpha = 0.3, color = "#34495E", size = 1.5, width = 0.1) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la Tasa alfabetizacion 15 a 24 años", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```

## Gráfico de violín

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos)) +
    geom_violin(fill = "#4B90E2", color = "#2C5F7B", alpha = 0.8,
    trim = FALSE,               # No recortar las colas
    scale = "width", width = 0.8, linewidth = 0.8) +
    geom_boxplot( width = 0.15, fill = "white", color = "#2C5F9A", alpha = 0.9,
    outlier.color = "#D74C3C", outlier.size = 1.5, outlier.alpha = 0.8, notch = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3, fill = "white",
  color = "#2E5F7A", stroke = 1.5) +
  geom_jitter(alpha = 0.2, color = "#38495E", size = 0.8, width = 0.05) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la Tasa alfabetizacion 15 a 24 años", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

## Q-Q Plot (comparación de cuantiles)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center


# Q-Q Plot básico para normalidad
ggplot(data = datos, aes(sample = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos)) +
    stat_qq(color = "#4A90E2", size = 2, alpha = 0.7) +
    stat_qq_line(color = "#E74C3C", linewidth = 1, linetype = "dashed") +
    scale_x_continuous(name = "Cuantiles Teóricos (Distribución Normal)",
    breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(name = "Cuantiles Muestrales (Tasa alfabetizacion 15 a 24 años)",
    breaks = pretty_breaks(n = 6),
    labels = function(x) paste(x, "años")) +
    labs(title = "Gráfico Q-Q para Normalidad",
    subtitle = "Tasa alfabetizacion 15 a 24 años - Población Baja California") +
    theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  


```

## Dot Plot (Gráfico de puntos)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-height: 16
#| fig-align: center

ggplot(data = datos, aes(x = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos, y = reorder(NOMGEO, tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos))) +
  geom_point(color = "#4A90E2", size = 3, alpha = 0.8) +
  geom_segment(aes(x = 0, xend = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos, y = NOMGEO, yend = NOMGEO),
    color = "#BDC3C7", size = 0.5, alpha = 0.7) +
  scale_x_continuous(
    name = "Edad (años)",
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(
    name = "Municipios",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Dot Plot - Tasa alfabetizacion 15 a 24 años por Municipio",
    subtitle = "Estado de Baja California") +
  theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=9),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

# Poblacion_no_catolica

```{r}
#| code-fold: true
#| fig-width: 8
#| fig-align: center


ggplot(datos) +
  geom_sf(aes(fill = poblacion_no_catolica), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "poblacion_no_catolica"
  ) +
  labs(
    title = "poblacion_no_catolica de los Municipios de Baja California",
    caption = "Fuente: INEGI - Marco Geoestadístico"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "right"
  )

```

## Histograma

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_histogram(
    aes(x = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos), 
    color = "black", fill = "olivedrab", 
    boundary = min(datos$tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos, na.rm = TRUE), 
    binwidth = 0.1
  ) +
  labs(
    title = "Distribución de la Tasa de Alfabetización (15 a 24 años)",
    x = "Tasa de alfabetización (%)", 
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )

```

## Gráfica de densidad

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_density(aes(poblacion_no_catolica), color="black", fill = "firebrick", alpha=0.6) +
  labs(
    title = "Distribución de la Edad",
    x = "Edad (años)", y = "Densidad"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )


```

## Boxplot

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = poblacion_no_catolica)) +
    geom_boxplot(
    fill = "#4A90E2",           # Color de relleno
    color = "#2C5F7A",          # Color del borde más oscuro
    alpha = 0.8,                # Transparencia
    outlier.color = "#E74C3C",  # Color rojo para outliers
    outlier.size = 2.5,         # Tamaño de outliers
    outlier.alpha = 0.7,        # Transparencia de outliers
    width = 0.5,                # Ancho del boxplot
    notch = TRUE,               # Muescas para intervalo de confianza
    notchwidth = 0.7            # Ancho de las muescas
  ) +
  geom_jitter(alpha = 0.3, color = "#34495E", size = 1.5, width = 0.1) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la poblacion_no_catolica", 
  subtitle = "Población del Estado de Baja California ",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```

## Gráfico de violín

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = poblacion_no_catolica)) +
    geom_violin(fill = "#4A90E2", color = "#2C5F7A", alpha = 0.8,
    trim = FALSE,               # No recortar las colas
    scale = "width", width = 0.8, linewidth = 0.8) +
    geom_boxplot( width = 0.15, fill = "white", color = "#2C5F7A", alpha = 0.9,
    outlier.color = "#E74C3C", outlier.size = 1.5, outlier.alpha = 0.8, notch = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3, fill = "white",
  color = "#2C5F7A", stroke = 1.5) +
  geom_jitter(alpha = 0.2, color = "#34495E", size = 0.8, width = 0.05) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la poblacion_no_catolica", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

## Q-Q Plot (comparación de cuantiles)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center


# Q-Q Plot básico para normalidad
ggplot(data = datos, aes(sample = poblacion_no_catolica)) +
    stat_qq(color = "#4A90E2", size = 2, alpha = 0.7) +
    stat_qq_line(color = "#E74C3C", linewidth = 1, linetype = "dashed") +
    scale_x_continuous(name = "Cuantiles Teóricos (Distribución Normal)",
    breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(name = "Cuantiles Muestrales (Edad mediana)",
    breaks = pretty_breaks(n = 6),
    labels = function(x) paste(x, "años")) +
    labs(title = "Gráfico Q-Q para Normalidad",
    subtitle = "Edad Mediana - Baja California") +
    theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  


```

## Dot Plot (Gráfico de puntos)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-height: 16
#| fig-align: center

ggplot(data = datos, aes(x = poblacion_no_catolica, y = reorder(NOMGEO, poblacion_no_catolica))) +
  geom_point(color = "#4A90E2", size = 3, alpha = 0.8) +
  geom_segment(aes(x = 0, xend = poblacion_no_catolica, y = NOMGEO, yend = NOMGEO),
    color = "#BDC3C7", size = 0.5, alpha = 0.7) +
  scale_x_continuous(
    name = "Edad (años)",
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(
    name = "Municipios",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Dot Plot - poblacion_no_catolica por Municipio",
    subtitle = "Estado de Baja California") +
  theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=9),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

# Porcentaje de la población con discapacidad por accidente 

```{r}
#| code-fold: true
#| fig-width: 8
#| fig-align: center


ggplot(datos) +
  geom_sf(aes(fill = porcentaje_de_la_poblacion_con_discapacidad_por_accidente), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Edad (mediana)"
  ) +
  labs(
    title = "Edad (mediana) de los Municipios de Baja California",
    caption = "Fuente: INEGI - Marco Geoestadístico"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "right"
  )

```

## Histograma

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_histogram(aes(porcentaje_de_la_poblacion_con_discapacidad_por_accidente), color="black", fill = "olivedrab", boundary=min(datos$porcentaje_de_la_poblacion_con_discapacidad_por_accidente), binwidth = 2)
  labs(
    title = "Distribución del porcentaje de la población con discapacidad por accidente",
    x = "Edad (años)", y = "Frecuencia"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )

```

## Gráfica de densidad

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_density(aes(porcentaje_de_la_poblacion_con_discapacidad_por_accidente), color="black", fill = "firebrick", alpha=0.6) +
  labs(
    title = "Distribución de la población con discapacidad por accidente",
    x = "Edad (años)", y = "Densidad"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )


```

## Boxplot

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = porcentaje_de_la_poblacion_con_discapacidad_por_accidente)) +
    geom_boxplot(
    fill = "#4A90E2",           # Color de relleno
    color = "#2C5F7A",          # Color del borde más oscuro
    alpha = 0.8,                # Transparencia
    outlier.color = "#E74C3C",  # Color rojo para outliers
    outlier.size = 2.5,         # Tamaño de outliers
    outlier.alpha = 0.7,        # Transparencia de outliers
    width = 0.5,                # Ancho del boxplot
    notch = TRUE,               # Muescas para intervalo de confianza
    notchwidth = 0.7            # Ancho de las muescas
  ) +
  geom_jitter(alpha = 0.3, color = "#34495E", size = 1.5, width = 0.1) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la Edad (mediana)", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```

## Gráfico de violín

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = porcentaje_de_la_poblacion_con_discapacidad_por_accidente)) +
    geom_violin(fill = "#4A90E2", color = "#2C5F7A", alpha = 0.8,
    trim = FALSE,               # No recortar las colas
    scale = "width", width = 0.8, linewidth = 0.8) +
    geom_boxplot( width = 0.15, fill = "white", color = "#2C5F7A", alpha = 0.9,
    outlier.color = "#E74C3C", outlier.size = 1.5, outlier.alpha = 0.8, notch = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3, fill = "white",
  color = "#2C5F7A", stroke = 1.5) +
  geom_jitter(alpha = 0.2, color = "#34495E", size = 0.8, width = 0.05) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la Edad (mediana)", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

## Q-Q Plot (comparación de cuantiles)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center


# Q-Q Plot básico para normalidad
ggplot(data = datos, aes(sample = porcentaje_de_la_poblacion_con_discapacidad_por_accidente)) +
    stat_qq(color = "#4A90E2", size = 2, alpha = 0.7) +
    stat_qq_line(color = "#E74C3C", linewidth = 1, linetype = "dashed") +
    scale_x_continuous(name = "Cuantiles Teóricos (Distribución Normal)",
    breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(name = "Cuantiles Muestrales (Edad mediana)",
    breaks = pretty_breaks(n = 6),
    labels = function(x) paste(x, "años")) +
    labs(title = "Gráfico Q-Q para Normalidad",
    subtitle = "Edad Mediana - Poblacion Baja California") +
    theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  


```

## Dot Plot (Gráfico de puntos)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-height: 16
#| fig-align: center

ggplot(data = datos, aes(x = porcentaje_de_la_poblacion_con_discapacidad_por_accidente, y = reorder(NOMGEO, porcentaje_de_la_poblacion_con_discapacidad_por_accidente))) +
  geom_point(color = "#4A90E2", size = 3, alpha = 0.8) +
  geom_segment(aes(x = 0, xend = porcentaje_de_la_poblacion_con_discapacidad_por_accidente, y = NOMGEO, yend = NOMGEO),
    color = "#BDC3C7", size = 0.5, alpha = 0.7) +
  scale_x_continuous(
    name = "Edad (años)",
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(
    name = "Municipios",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Dot Plot - Edad (mediana) por Municipio",
    subtitle = "Estado de Baja California") +
  theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=9),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

# Promedio de ocupantes por dormitorio

```{r}
#| code-fold: true
#| fig-width: 8
#| fig-align: center


ggplot(datos) +
  geom_sf(aes(fill = promedio_de_ocupantes_por_dormitorio), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Promedio de ocupantes por dormitorio"
  ) +
  labs(
    title = "Promedio de ocupantes por dormitorio",
    caption = "Fuente: INEGI - Marco Geoestadístico"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "right"
  )

```

## Histograma

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_histogram(aes(promedio_de_ocupantes_por_dormitorio), color="black", fill = "olivedrab", boundary=min(datos$promedio_de_ocupantes_por_dormitorio), binwidth = 2) +
  labs(
    title = "Distribución de la Edad",
    x = "Edad (años)", y = "Frecuencia"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )

```

## Gráfica de densidad

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_density(aes(promedio_de_ocupantes_por_dormitorio), color="black", fill = "firebrick", alpha=0.6) +
  labs(
    title = "Distribución de la Edad",
    x = "Edad (años)", y = "Densidad"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )


```

## Boxplot

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = promedio_de_ocupantes_por_dormitorio)) +
    geom_boxplot(
    fill = "#4A90E2",           # Color de relleno
    color = "#2C5F7A",          # Color del borde más oscuro
    alpha = 0.8,                # Transparencia
    outlier.color = "#E74C3C",  # Color rojo para outliers
    outlier.size = 2.5,         # Tamaño de outliers
    outlier.alpha = 0.7,        # Transparencia de outliers
    width = 0.5,                # Ancho del boxplot
    notch = TRUE,               # Muescas para intervalo de confianza
    notchwidth = 0.7            # Ancho de las muescas
  ) +
  geom_jitter(alpha = 0.3, color = "#34495E", size = 1.5, width = 0.1) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de porcentaje de ocupantes por dormitorio", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```

## Gráfico de violín

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = promedio_de_ocupantes_por_dormitorio)) +
    geom_violin(fill = "#4A90E2", color = "#2C5F7A", alpha = 0.8,
    trim = FALSE,               # No recortar las colas
    scale = "width", width = 0.8, linewidth = 0.8) +
    geom_boxplot( width = 0.15, fill = "white", color = "#2C5F7A", alpha = 0.9,
    outlier.color = "#E74C3C", outlier.size = 1.5, outlier.alpha = 0.8, notch = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3, fill = "white",
  color = "#2C5F7A", stroke = 1.5) +
  geom_jitter(alpha = 0.2, color = "#34495E", size = 0.8, width = 0.05) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución del promedio de ocupantes por dormitorio", 
  subtitle = "Población del Estado de Sinaloa",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

## Q-Q Plot (comparación de cuantiles)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center


# Q-Q Plot básico para normalidad
ggplot(data = datos, aes(sample = promedio_de_ocupantes_por_dormitorio)) +
    stat_qq(color = "#4A90E2", size = 2, alpha = 0.7) +
    stat_qq_line(color = "#E74C3C", linewidth = 1, linetype = "dashed") +
    scale_x_continuous(name = "Cuantiles Teóricos (Distribución Normal)",
    breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(name = "Cuantiles Muestrales (Edad mediana)",
    breaks = pretty_breaks(n = 6),
    labels = function(x) paste(x, "años")) +
    labs(title = "Gráfico Q-Q para Normalidad",
    subtitle = "Edad Mediana - Porcentaje de ocupantes por dormitorio") +
    theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  


```

## Dot Plot (Gráfico de puntos)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-height: 16
#| fig-align: center

ggplot(data = datos, aes(x = promedio_de_ocupantes_por_dormitorio, y = reorder(NOMGEO, tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos))) +
  geom_point(color = "#4A90E2", size = 3, alpha = 0.8) +
  geom_segment(aes(x = 0, xend = tasa_de_alfabetizacion_de_las_personas_de_15_a_24_anos, y = NOMGEO, yend = NOMGEO),
    color = "#BDC3C7", size = 0.5, alpha = 0.7) +
  scale_x_continuous(
    name = "Edad (años)",
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(
    name = "Municipios",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Dot Plot - ocupantes por dormitorio",
    subtitle = "Estado de Baja California") +
  theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=9),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

# -Población de 6 a 14 años que no sabe leer y escribir

```{r}
#| code-fold: true
#| fig-width: 8
#| fig-align: center


ggplot(datos) +
  geom_sf(aes(fill = poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "-Población de 6 a 14 años que no sabe leer y escribir"
  ) +
  labs(
    title = "-Población de 6 a 14 años que no sabe leer y escribir ",
    caption = "Fuente: INEGI - Marco Geoestadístico"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "right"
  )

```

## Histograma

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_histogram(aes( poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir), color="black", fill = "olivedrab", boundary=min(datos$indice_de_envejecimiento), binwidth = 2) +
  labs(
    title = "Distribución de la Edad",
    x = "Edad (años)", y = "Frecuencia"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )

```

## Gráfica de densidad

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(datos) +
  geom_density(aes( poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir), color="black", fill = "firebrick", alpha=0.6) +
  labs(
    title = "Distribución de la Edad",
    x = "Edad (años)", y = "Densidad"
  ) +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13)
  )


```

## Boxplot

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y =  poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir)) +
    geom_boxplot(
    fill = "#4A90E2",           # Color de relleno
    color = "#2C5F7A",          # Color del borde más oscuro
    alpha = 0.8,                # Transparencia
    outlier.color = "#E74C3C",  # Color rojo para outliers
    outlier.size = 2.5,         # Tamaño de outliers
    outlier.alpha = 0.7,        # Transparencia de outliers
    width = 0.5,                # Ancho del boxplot
    notch = TRUE,               # Muescas para intervalo de confianza
    notchwidth = 0.7            # Ancho de las muescas
  ) +
  geom_jitter(alpha = 0.3, color = "#34495E", size = 1.5, width = 0.1) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Población de 6 a 14 años que no sabe leer y escribir", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```

## Gráfico de violín

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = datos, aes(x = "", y = poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir)) +
    geom_violin(fill = "#4A90E2", color = "#2C5F7A", alpha = 0.8,
    trim = FALSE,               # No recortar las colas
    scale = "width", width = 0.8, linewidth = 0.8) +
    geom_boxplot( width = 0.15, fill = "white", color = "#2C5F7A", alpha = 0.9,
    outlier.color = "#E74C3C", outlier.size = 1.5, outlier.alpha = 0.8, notch = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3, fill = "white",
  color = "#2C5F7A", stroke = 1.5) +
  geom_jitter(alpha = 0.2, color = "#34495E", size = 0.8, width = 0.05) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir", 
  subtitle = "Población del Estado de Baja California",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
```

## Q-Q Plot (comparación de cuantiles)

```{r}
#| code-fold: true
#| fig-width: 7
#| fig-align: center


# Q-Q Plot básico para normalidad
ggplot(data = datos, aes(sample = poblacion_de_6_a_14_anos_que_sabe_leer_y_escribir)) +
    stat_qq(color = "#4A90E2", size = 2, alpha = 0.7) +
    stat_qq_line(color = "#E74C3C", linewidth = 1, linetype = "dashed") +
    scale_x_continuous(name = "Cuantiles Teóricos (Distribución Normal)",
    breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(name = "Cuantiles Muestrales (Edad mediana)",
    breaks = pretty_breaks(n = 6),
    labels = function(x) paste(x, "años")) +
    labs(title = "Gráfico Q-Q para Normalidad",
    subtitle = "Edad Mediana - Población  Baja California") +
    theme_bw()+
    theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```
