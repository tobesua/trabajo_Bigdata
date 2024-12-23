#- código R usado en el trabajo

# 1: ANÁLISIS DE LA EVOLUCIÓN DE LOS SALARIOS EN ESPAÑA (2008-2024)-> SALARIO MEDIO Y SALARIO COMÚN, PARA PODER COMPARAR.

# paquetes necesarios:        
library(tidyverse)
library(knitr)
library(httr)
library(jsonlite)
library(dplyr)
library(devtools) #install.packages("devtools")
library(eurostat)
library(plotly) #install.packages("plotly")
library(gganimate) #install.packages("gganimate")
library(leaflet)
library(readxl)
library(gifski)
library(magick)
library(tmap) #install.packages("tmap")
library(sf) #install.packages("sf")
library(rnaturalearth)



# definimos la ruta para poder importar los datos
ruta <- here::here("assets", "salario_esp.xlsx")
datos_salarios <- rio::import(ruta)

# para confirmar que se ha cargado correctamente
head(datos_salarios)  #todo ok

# arreglamos los datos para que el programa los pueda procesar > formato largo
datos_largos <- datos_salarios %>%
  pivot_longer(cols = c("salario_medio", "salario_mediana"), 
               names_to = "tipo_salario", 
               values_to = "valor")

# burramos los datos que no se usan para el gráfico o el análisis
rm(datos_salarios)


# gráfico de barras

plot_ly(datos_largos, 
        x = ~años, 
        y = ~valor, 
        color = ~tipo_salario, 
        type = 'bar', 
        text = ~paste(tipo_salario, ": ", valor, "€"),
        hoverinfo = 'text') %>%
  layout(
    title = "Evolución de los Salarios en España (2008-2024)",
    xaxis = list(
      title = "Año", 
      tickmode = "linear", 
      tickangle = 45, 
      showgrid = TRUE, 
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Salario (€)", 
      tickprefix = "€", 
      showgrid = TRUE, 
      zeroline = FALSE
    ),
    barmode = "group", # Modo para mostrar las barras agrupadas
    legend = list(
      title = list(text = "Tipo de Salario"),
      orientation = "h", 
      x = 0.5, 
      xanchor = "center", 
      y = -0.2, 
      yanchor = "bottom"
    ),
    margin = list(l = 50, r = 50, t = 50, b = 100), # márgenes para ajustar el diseño
    plot_bgcolor = "#f4f4f4", # color de fondo del gráfico
    paper_bgcolor = "#ffffff", # color de fondo de la página
    font = list(family = "Arial", size = 12) # fuente y tamaño
  )



#hacemos lo mismo para el salario mensual

ruta <- here::here("assets", "salario_mensual.xlsx")
datos_salarios_mensual <- rio::import(ruta)

head(datos_salarios_mensual)

datos_largos_mensual <- datos_salarios_mensual %>%
  pivot_longer(cols = c("medio_mensual", "mediana_mensual"), 
               names_to = "tipo_salario", 
               values_to = "valor")

rm(datos_salarios_mensual)

#otro grafico 

plot_ly(datos_largos_mensual, 
        x = ~años, 
        y = ~valor, 
        color = ~tipo_salario, 
        type = 'bar', 
        text = ~paste(tipo_salario, ": ", valor, "€"),
        hoverinfo = 'text') %>%
  layout(
    title = "Evolución del Salario Mensual en España (2008-2024)",
    xaxis = list(
      title = "Año", 
      tickmode = "linear", 
      tickangle = 45, 
      showgrid = TRUE, 
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Salario (€)", 
      tickprefix = "€", 
      showgrid = TRUE, 
      zeroline = FALSE
    ),
    barmode = "group", # Modo para mostrar las barras agrupadas
    legend = list(
      title = list(text = "Tipo de Salario"),
      orientation = "h", 
      x = 0.5, 
      xanchor = "center", 
      y = -0.2, 
      yanchor = "bottom"
    ),
    margin = list(l = 50, r = 50, t = 50, b = 100), # márgenes para ajustar el diseño
    plot_bgcolor = "#f4f4f4", # color de fondo del gráfico
    paper_bgcolor = "#ffffff", # color de fondo de la página
    font = list(family = "Arial", size = 12) # fuente y tamaño
  )


#Ahora importamos la base de datos del IPV 
ipv <- read_excel("./assets/ipv.xlsx", sheet = "usar")

#reordenamos la base de datos para tenerla en formato panel 
datos_panel_ipv <- ipv %>%
  pivot_longer(
    cols = -year, 
    names_to = "region", 
    values_to = "valor" 
  )

# Ordenar por año y región para mantener la estructura panel
datos_panel_ipv <- datos_panel_ipv %>%
  arrange(year, region)

# seteamos la variable year en formato numerico para no tener problemas al momento de graficar
datos_panel_ipv <- datos_panel_ipv %>%
  mutate(year = as.numeric(year))


#boxplot por region
ggplot(datos_panel_ipv, aes(x = region, y = valor, fill = region)) +
  geom_boxplot() +
  labs(
    title = "Distribución de valores por región",
    x = "Región",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------#grafico animado----------------------------------------

# Seleccionar regiones y calcular el promedio general
regiones_elegidas <- c("Comunitat Valenciana", "Madrid", "Cataluña")
datos_filtrados <- datos_panel_ipv %>%
  filter(region %in% regiones_elegidas)

#limpiamos la base 
datos_filtrados %>%
  filter(is.na(year) | is.na(valor)) %>%
  print()



# Crear el gráfico animado
grafico_animado <- ggplot(datos_filtrados, aes(x = year, y = valor, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Cataluña" = "blue", "Comunitat Valenciana" = "green", 
                                "Madrid" = "red")) +
  labs(
    title = "Evolución de Precios de Viviendas",
    x = "Año", y = "Precio", color = "Región"
  ) +
  theme_minimal() +
  transition_reveal(year)







#----------------------------------------Análisis de IPV----------------------------------------
#Aumento de los precios de los pisos turisticos en las ciudades----------------------------------------

# Cargar datos de viviendas turísticas



datos_valores_viviendas <- read_excel("assets/datos_valores_viviendas.xlsx",sheet = "formateada")

#convertimos los datos a años para poder ilustrar de mejor forma 
df_annual <- datos_valores_viviendas %>%
  mutate(year = substr(year, 1, 4)) %>%  # Extraer el año de la columna 'year'
  group_by(year, region) %>%            # Agrupar por año y región
  summarise(value = mean(value, na.rm = TRUE)) %>%  # Calcular el promedio anual
  ungroup()

df_annual <- df_annual %>%
  mutate(year = as.numeric(year))

##ahora procedemos a crear un grafico de barras animado 
p <- ggplot(df_annual, aes(x = reorder(region, -value), y = value, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Valores por Región: {closest_state}",
       x = "Región",
       y = "Valor",
       fill = "Región") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

# Animar el gráfico
animate(p, fps = 10, duration = 10, width = 800, height = 600)

#ahora podemos hacer un grafico de lineas animado con el dataframe anual, en este caso se puede hacer un grafico de puntos simple

p <- ggplot(df_annual, aes(x = region, y = value, color = region)) +
  geom_point(size = 4) +
  labs(title = "Valores por Región: {frame_time}",
       x = "Región",
       y = "Valor",
       color = "Región") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_time(year) +
  ease_aes('linear')

# Animar el gráfico
animate(p, fps = 5, duration = 5, width = 800, height = 600)

#ahora intentarremos hacer un mapa de calor

p <- ggplot(df_annual, aes(x = region, y = year, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Value", option = "plasma") +
  labs(
    title = "Mapa de calor para los valores por Región y año: {frame_time}",
    x = "Region",
    y = "Year",
    fill = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  transition_time(as.numeric(year)) +
  ease_aes('linear')

# Animate the heatmap
animate(p, fps = 10, duration = 10, width = 800, height = 600)

#--------------------------------------EXTRA PARA VER GRAFICOS CHULOS-------------------------------------------

poblacion_espana <- read_excel("./assets/poblacion_espana.xlsx", sheet = "formateada")

# Asegurarse de que las columnas sean numéricas
poblacion_espana$year <- as.numeric(poblacion_espana$year)
poblacion_espana$value <- as.numeric(poblacion_espana$value)

# Crear el gráfico
poblacion_espana_plot <- ggplot(poblacion_espana, aes(x = year, y = value, group = 1)) +  # Grupo explícito
  geom_line(color = "#2C3E50", size = 1.5, alpha = 0.8, stat = "identity") +  # Línea principal con stat_identity
  geom_point(color = "#E74C3C", size = 3, alpha = 0.9, stat = "identity") +   # Puntos destacados con stat_identity
  labs(
    title = "Incremento de la Población en España",
    subtitle = "Evolución desde {round(frame_along, 0)}",  # Año sin decimales
    x = "Año",
    y = "Población",
    caption = "Fuente: Instituto Nacional de Estadísticas"
  ) +
  scale_x_continuous(breaks = seq(min(poblacion_espana$year), max(poblacion_espana$year), by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#34495E", hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "#7F8C8D", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", color = "#34495E"),
    axis.title.y = element_text(size = 12, face = "bold", color = "#34495E"),
    axis.text = element_text(size = 10, color = "#34495E"),
    panel.grid.major = element_line(color = "#BDC3C7", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "#7F8C8D", hjust = 0)
  )

# Añadir animación
anim <- poblacion_espana_plot + 
  transition_reveal(along = year)  # Configurar explícitamente el eje temporal

# Renderizar la animación
animate(anim, nframes = 150, fps = 15, width = 900, height = 600, renderer = gifski_renderer())







----------------------------------------
#Aumento del número de viviendas turisticas por CCAA ----------------------------------------
# definimos la ruta para poder importar los datos
ruta <- here::here("assets", "viv_tur_2020.xlsx")
viv_tur_2020 <- rio::import(ruta)

df_viv_tur_2020 <- viv_tur_2020



#- cargo geometrías de provincias
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(df_geo_prov, max.plot = 1)

#- podemos ver que la última columna de df_geo_prov tiene las "geometrías"
names(df_geo_prov)
head(df_geo_prov)

#- me quedo con las vv. q me interesan
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)
names(df_geo_prov)

#- podemos "agregar" geometrías
df_geo_ccaa <- df_geo_prov %>%
  group_by(ine_ccaa, ine_ccaa.n) %>% summarize() %>% ungroup()
plot(df_geo_ccaa, max.plot = 1)
names(df_geo_ccaa)


#- junto geometría (df_geo_ccaa) con datos INE (df_viv_tur_2020)
#- las geometrías a la izquierda
df_ok <- left_join(df_geo_ccaa, df_viv_tur_2020, by = c("ine_ccaa.n" = "ccaa"))
names(df_ok)

#- basic plot
p <- ggplot() +
  geom_sf(data = df_ok,
          aes(geometry = geometry, fill = viviendas_turisticas),
          color = "white", size = 0.09)

p + scale_fill_distiller(palette = 2)
p + pjpv.curso.R.2022::theme_pjp_maps()
p + scale_fill_viridis_c(option = "plasma") 


#- mejoramos un poco el plot ---------------------------------------------------
#- para ello calculo centroides
library(sf)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))
names(df_geo_ccaa)
#- vuelvo a juntar datos EPA con geometrías (que ahora incorporan los centroides)
df_ok <- left_join(df_geo_ccaa, df_viv_tur_2020, by = c("ine_ccaa.n" = "ccaa"))

p <- ggplot() +
  geom_sf(data = df_ok) +
  geom_text(data = df_ok, aes(x = X, y = Y, label = viviendas_turisticas), #- v. continua
            color = "black",
            check_overlap = TRUE, size = 3)  #- fontface = "bold"

p


p <- ggplot() +
geom_sf(data = df_ok,
        aes(geometry = geometry), fill = "#B0E0E6",
        color = "black", size = 0.09) +
  geom_text(data = df_ok, aes(x = X, y = Y, label = viviendas_turisticas), #- v. continua
            color = "black",
            check_overlap = TRUE, size = 3)  #- fontface = "bold"

p


#- luego ya hay que añadirle el título
p2020 <- p + pjpv.curso.R.2022::theme_pjp_maps() +
  labs(title = "Viviendas Turísticas 2020",
       caption = "Datos provenientes del INE")

p2020

#Ahora hacemos lo mismo para el año 2024 con el fin de ver el cambio en el número de viviendas turísticas

# definimos la ruta para poder importar los datos
ruta2 <- here::here("assets", "viv_tur_2024.xlsx")
viv_tur_2024 <- rio::import(ruta2)

df_viv_tur_2024 <- viv_tur_2024



#- cargo geometrías de provincias
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(df_geo_prov, max.plot = 1)

#- podemos ver que la última columna de df_geo_prov tiene las "geometrías"
names(df_geo_prov)
head(df_geo_prov)

#- me quedo con las vv. q me interesan
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)
names(df_geo_prov)

#- podemos "agregar" geometrías
df_geo_ccaa <- df_geo_prov %>%
  group_by(ine_ccaa, ine_ccaa.n) %>% summarize() %>% ungroup()
plot(df_geo_ccaa, max.plot = 1)
names(df_geo_ccaa)


#- junto geometría (df_geo_ccaa) con datos INE (df_viv_tur_2024)
#- las geometrías a la izquierda
df_ok1 <- left_join(df_geo_ccaa, df_viv_tur_2024, by = c("ine_ccaa.n" = "ccaa"))
names(df_ok1)

#- basic plot
p1 <- ggplot() +
  geom_sf(data = df_ok1,
          aes(geometry = geometry, fill = viviendas_turisticas),
          color = "white", size = 0.09)

p1 + scale_fill_distiller(palette = 2)
p1 + pjpv.curso.R.2022::theme_pjp_maps()
p1 + scale_fill_viridis_c(option = "plasma") 


#- mejoramos un poco el plot ---------------------------------------------------
#- para ello calculo centroides
library(sf)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))
names(df_geo_ccaa)
#- vuelvo a juntar datos EPA con geometrías (que ahora incorporan los centroides)
df_ok1 <- left_join(df_geo_ccaa, df_viv_tur_2024, by = c("ine_ccaa.n" = "ccaa"))

p1 <- ggplot() +
  geom_sf(data = df_ok1) +
  geom_text(data = df_ok1, aes(x = X, y = Y, label = viviendas_turisticas), #- v. continua
            color = "black",
            check_overlap = TRUE, size = 3)  #- fontface = "bold"

p1


p1 <- ggplot() +
  geom_sf(data = df_ok1,
          aes(geometry = geometry), fill = "#B0E0E6",
          color = "black", size = 0.09) +
  geom_text(data = df_ok1, aes(x = X, y = Y, label = viviendas_turisticas), #- v. continua
            color = "black",
            check_overlap = TRUE, size = 3)  #- fontface = "bold"

p1

#- luego ya hay que mejorarlo
p2024 <- p1 + pjpv.curso.R.2022::theme_pjp_maps() +
  labs(title = "Viviendas Turísticas 2024",
       caption = "Datos provenientes del INE")

p2024

#- utilizamos el pkg "patchwork" para mostrar los 2 gráficos (p2020 y p2024) side-by-side
library(patchwork)

p2020 + p2024

