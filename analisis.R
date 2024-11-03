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

# combined_data Aqui tienes la informacion para pintar los graficos
head(combined_data)
#nrow(combined_data)

# Saco los datos unicos por año y tipo de cultivo
unique_crops <- unique(combined_data$Crop)
unique_year <- unique(combined_data$Crop_Year)

# Saco el promedio del rendimiento por año de un tipo de cultivo 
average_yield <- aggregate(Rendimiento ~ Crop + Crop_Year, data = combined_data, FUN = mean)


ggplot(data = average_yield, aes(x = Crop_Year, y = Rendimiento, color = Crop, group = Crop)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Promedio de Rendimiento por Cultivo y Ano",
       x = "Ano",
       y = "Rendimiento",
       color = "Cultivo") +
  theme_minimal() +
  theme(legend.position = "none")