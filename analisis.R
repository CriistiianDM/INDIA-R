if (!require(Amelia)) install.packages("Amelia")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(tidyr)) install.packages("tidyr")
if (!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(Amelia)
library(tidyr)

csv_directory <- "DATA_NEW"

csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

combined_data <- do.call(rbind, lapply(csv_files, function(file) {
  read.csv(file, sep = ",", stringsAsFactors = FALSE)
}))

# Nueva variable rendimiento
combined_data <- combined_data %>%
  mutate(Rendimiento = Production / Area)

# Filtrar datos para años mayores a 2020
#filtered_data <- combined_data %>%
#  filter(Crop_Year >= 2011)

#class(combined_data$Crop_Year)
#unique(combined_data$Crop_Year)

write.csv(combined_data, "C:/Users/USUARIO/Documents/GitHub/BRICS-R/DATA_NEW/nueva_variable.csv", row.names = FALSE)
#head(combined_data)
#nrow(filtered_data)
#nrow(combined_data)

#write.csv(filtered_data, "C:/Users/USUARIO/Documents/GitHub/BRICS-R/DATA_NEW/datos_investigacion_mayor_a_2011.csv", row.names = FALSE)

# AQUI LELSLI 
# Sobre ese archivo voy a trabjar para evitar calculos pesados y sobre estos datos voy a relaizar  la inevstigacion
csv_directory <- "Investigacion"

csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

combined_data <- do.call(rbind, lapply(csv_files, function(file) {
  read.csv(file, sep = ",", stringsAsFactors = FALSE)
}))

coconut_data <- combined_data %>%
  filter(Crop != "Coconut ")

unique_season <- unique(combined_data$Season)

head(unique_season, n=10)
# combined_data Aqui tienes la informacion para pintar los graficos
head(combined_data)
#nrow(combined_data)

# Saco los datos unicos por año y tipo de cultivo
unique_crops <- unique(combined_data$Crop)
unique_year <- unique(combined_data$Crop_Year)


# Saco el promedio del rendimiento por año de un tipo de cultivo 
average_yield <- aggregate(Rendimiento ~ Crop + Crop_Year, data = combined_data, FUN = mean)

top_crops_per_year <- average_yield %>%
  group_by(Crop_Year) %>%
  arrange(desc(Rendimiento)) %>%
  slice_head(n = 10) %>%
  ungroup()

# Ver los resultados
head(top_crops_per_year, n = 11)


top_crops <- unique(top_crops_per_year$Crop)

# Filtrar el promedio de rendimiento solo para los cultivos principales
top_crops_data <- average_yield %>%
  filter(Crop %in% top_crops)

# Crear el gráfico
ggplot(data = top_crops_data, aes(x = Crop_Year, y = Rendimiento, color = Crop, group = Crop)) +
  geom_line(size = 1) +  # Línea para cada cultivo
  geom_point(size = 2) + # Puntos en la línea
  labs(title = "Rendimiento Promedio de los 10 Principales Cultivos por Año",
       x = "Año",
       y = "Rendimiento Promedio",
       color = "Cultivo") +
  theme_minimal()


# Grafico auxiliar
coconut_data <- average_yield %>%
  filter(Crop == "Coconut ")

head(coconut_data)

# Crear el gráfico
ggplot(data = coconut_data, aes(x = Crop_Year, y = Rendimiento)) +
  geom_line(size = 1, color = "green") +  # Línea verde para el rendimiento
  geom_point(size = 3, color = "green") + # Puntos en la línea
  labs(title = "Rendimiento del Cultivo de Coconut por Año",
       x = "Año",
       y = "Rendimiento Promedio") +
  theme_minimal()

# Segundo Grafico

sugarcane_data <- combined_data %>%
  filter(Crop == "Sugarcane" & Crop_Year >= 2010 & Crop_Year <= 2015)

# Calcular el promedio de rendimiento por estado
average_yield_by_state <- sugarcane_data %>%
  group_by(State_Name) %>%
  summarise(Average_Rendimiento = mean(Rendimiento, na.rm = TRUE)) %>%
  arrange(desc(Average_Rendimiento))

# Seleccionar los 10 estados con el mayor rendimiento promedio
top_10_states <- average_yield_by_state %>%
  slice_head(n = 15)

ggplot(data = top_10_states, aes(x = reorder(State_Name, Average_Rendimiento), y = Average_Rendimiento)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 15 Estados con Mayor Rendimiento Promedio de Sugarcane (2010-2015)",
       x = "Estado",
       y = "Rendimiento Promedio") +
  coord_flip() +  # Voltear el gráfico para facilitar la lectura de los nombres
  theme_minimal()




####


sugarcane_data <- combined_data %>%
  filter(Crop == "Sugarcane" & Crop_Year >= 2010 & Crop_Year <= 2015)

# Calcular el promedio de producción por estado
average_production_by_state <- sugarcane_data %>%
  group_by(State_Name) %>%
  summarise(Average_Production = mean(Production, na.rm = TRUE)) %>%
  arrange(desc(Average_Production))

# Seleccionar los 15 estados con la mayor producción promedio
top_15_states_production <- average_production_by_state %>%
  slice_head(n = 15)

# Crear el gráfico de barras para la producción promedio
ggplot(data = top_15_states_production, aes(x = reorder(State_Name, Average_Production), y = Average_Production)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Top 15 Estados con Mayor Producción Promedio de Sugarcane (2010-2011)",
       x = "Estado",
       y = "Producción Promedio") +
  coord_flip() +  # Voltear el gráfico para facilitar la lectura de los nombres
  theme_minimal()



##########


# Filtrar los datos de "Sugarcane" entre los años 2010 y 2015
sugarcane_data <- combined_data %>%
  filter(Crop == "Sugarcane" & Crop_Year >= 2010 & Crop_Year <= 2015)

# Calcular el promedio de rendimiento y producción por estado
average_metrics_by_state <- sugarcane_data %>%
  group_by(State_Name) %>%
  summarise(Average_Rendimiento = mean(Rendimiento, na.rm = TRUE),
            Average_Production = mean(Production, na.rm = TRUE)) %>%
  arrange(desc(Average_Rendimiento))

# Seleccionar los 15 estados con el mayor rendimiento promedio
top_15_states_metrics <- average_metrics_by_state %>%
  slice_head(n = 15)

# Reestructurar los datos en formato largo para el gráfico combinado
top_15_states_long <- top_15_states_metrics %>%
  pivot_longer(cols = c("Average_Rendimiento", "Average_Production"),
               names_to = "Metric",
               values_to = "Value")

# Crear el gráfico combinado
ggplot(data = top_15_states_long, aes(x = reorder(State_Name, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Average_Rendimiento" = "skyblue", "Average_Production" = "lightgreen"),
                    labels = c("Rendimiento Promedio", "Producción Promedio")) +
  labs(title = "Top 15 Estados con Mayor Rendimiento y Producción Promedio de Sugarcane (2010-2015)",
       x = "Estado",
       y = "Promedio",
       fill = "Métrica") +
  coord_flip() +  # Voltear el gráfico para facilitar la lectura de los nombres
  theme_minimal()

####### 

coconut_data <- combined_data %>%
  filter(Crop == "Coconut ")

# Crear el gráfico de cajas
ggplot(data = coconut_data, aes(x = as.factor(Crop_Year), y = Rendimiento)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Variabilidad del Rendimiento de Coconut por Ano",
       x = "Ano",
       y = "Rendimiento",
       color = "Cultivo") +
  theme_minimal()



###################### GRAFICOS DE PRODUCION

average_production <- aggregate(Production ~ Crop + Crop_Year, data = combined_data, FUN = mean)

top_crops_per_year_production <- average_production %>%
  group_by(Crop_Year) %>%
  arrange(desc(Production)) %>%
  slice_head(n = 20) %>%
  ungroup()

# Ver los resultados
head(top_crops_per_year_production, n = 11)

top_crops_production <- unique(top_crops_per_year_production$Crop)

# Filtrar el promedio de producción solo para los cultivos principales
top_crops_data_production <- average_production %>%
  filter(Crop %in% top_crops_production)

head(top_crops_data_production,n=30)

# Crear el gráfico
ggplot(data = top_crops_data_production, aes(x = Crop_Year, y = Production, color = Crop, group = Crop)) +
  geom_line(size = 1) +  # Línea para cada cultivo
  geom_point(size = 2) + # Puntos en la línea
  labs(title = "Producción Promedio de los 10 Principales Cultivos por Ano",
       x = "Ano",
       y = "Producción Promedio",
       color = "Cultivo") +
  theme_minimal()



#################################################################

combined_data <- combined_data %>%
  filter(Crop != "Coconut ")
head(co)
average_production <- aggregate(Production ~ Crop + Crop_Year, data = combined_data, FUN = mean)

# Seleccionar los 10 cultivos principales por año basados en la producción
top_crops_per_year_production <- average_production %>%
  group_by(Crop_Year) %>%
  arrange(desc(Production)) %>%
  slice_head(n = 10) %>%
  ungroup()

# Obtener los cultivos principales únicos
top_crops <- unique(top_crops_per_year_production$Crop)

# Filtrar los datos para incluir solo los cultivos principales en el promedio de producción
top_crops_data_production <- average_production %>%
  filter(Crop %in% top_crops)

# Crear el gráfico para la producción
ggplot(data = top_crops_data_production, aes(x = Crop_Year, y = Production, color = Crop, group = Crop)) +
  geom_line(linewidth = 1) +  # Línea para cada cultivo
  geom_point(size = 2) +      # Puntos en la línea
  labs(title = "Producción Promedio de los 10 Principales Cultivos por Año",
       x = "Año",
       y = "Producción Promedio",
       color = "Cultivo") +
  theme_minimal() +
  theme(legend.position = "bottom")


############### 
coconut_data <- combined_data %>%
  filter(Crop == "Coconut " & !is.na(Production) & is.finite(Production))

# Verifica si quedan datos después del filtro
print(nrow(coconut_data))

# Crear el gráfico de cajas de producción
ggplot(data = coconut_data, aes(x = as.factor(Crop_Year), y = Production)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Variabilidad de la Producción del Cereal y Leche por Ano",
       x = "Ano",
       y = "Produccion",
       color = "Cultivo") +
  theme_minimal()


coconut_data <- combined_data %>%
  filter(Crop == "Coconut " & !is.na(Rendimiento) & is.finite(Rendimiento))

# Crear el gráfico de cajas
ggplot(data = coconut_data, aes(x = as.factor(Crop_Year), y = Rendimiento)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Variabilidad del Rendimiento del Cereal y Leche por Ano",
       x = "Ano",
       y = "Rendimiento",
       color = "Cultivo") +
  theme_minimal()


############  

combined_data__ <- combined_data %>%
  filter(Crop != "Coconut ")

combined_data__ <- combined_data__ %>%
  filter(!is.na(Rendimiento) & !is.na(Production))

set.seed(123)  # Para reproducibilidad
sample_data <- combined_data__ %>% sample_frac(0.1)

ggplot(sample_data, aes(x = Rendimiento, y = Production)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Relación entre Producción y Rendimiento",
    x = "Rendimiento",
    y = "Producción"
  ) +
  xlim(0, 200) +
  ylim(0, 200) +
  theme_minimal()

head(coconut_data)
coconut_2014 <- coconut_data %>%
  filter(Crop_Year == 2011)

# Calcular las estadísticas
rendimiento_stats <- coconut_2014 %>%
  summarize(
    mean_rendimiento = mean(Rendimiento, na.rm = TRUE),
    sd_rendimiento = sd(Rendimiento, na.rm = TRUE),
    coef_var = (sd_rendimiento / mean_rendimiento) * 100,
    q1 = quantile(Rendimiento, 0.25, na.rm = TRUE),
    median = median(Rendimiento, na.rm = TRUE),
    q3 = quantile(Rendimiento, 0.75, na.rm = TRUE)
  )

print(rendimiento_stats)