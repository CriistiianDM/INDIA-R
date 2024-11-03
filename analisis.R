library(dplyr)
library(Amelia)
library(tidyr)
library(ggplot2)

csv_directory <- "DATA_NEW"

csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

combined_data <- do.call(rbind, lapply(csv_files, function(file) {
  read.csv(file, sep = ",", stringsAsFactors = FALSE)
}))

# convertir en multiples columnas
combined_data <- combined_data %>%
  separate(State_Name.District_Name.Crop_Year.Season.Crop.Area.Production,
           into = c("State_Name", "District_Name", "Crop_Year", "Season", "Crop", "Area", "Production"),
           sep = ",",
           convert = TRUE)

# Nueva variable rendimiento
combined_data <- combined_data %>%
  mutate(Rendimiento = Production / Area)

# Filtrar datos para años mayores a 2020
filtered_data <- combined_data %>%
  filter(Crop_Year >= 2011)

class(combined_data$Crop_Year)
unique(combined_data$Crop_Year)

#write.csv(combined_data, "C:/Users/USUARIO/Documents/GitHub/BRICS-R/DATA_NEW/nuevaVariable.csv", row.names = FALSE)
head(combined_data)
nrow(filtered_data)

#write.csv(filtered_data, "C:/Users/USUARIO/Documents/GitHub/BRICS-R/DATA_NEW/datos_investigacion_mayor_a_2011.csv", row.names = FALSE)

# Sobre ese archivo voy a trabjar para evitar calculos pesados y sobre estos datos voy a relaizar  la inevstigacion
csv_directory <- "Investigacion"

csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

combined_data <- do.call(rbind, lapply(csv_files, function(file) {
  read.csv(file, sep = ",", stringsAsFactors = FALSE)
}))

unique_crops <- unique(combined_data$Crop)

head(unique_crops)
nrow(combined_data)

# Iterar sobre cada cultivo único
for (crop in unique_crops) {
  # Filtrar los datos para el cultivo actual
  crop_data <- combined_data %>%
    filter(Crop == crop) %>%
    arrange(Crop_Year)
  
  # Crear el gráfico de línea para el rendimiento
  p <- ggplot(crop_data, aes(x = Crop_Year, y = Rendimiento)) +
    geom_line(color = "blue") +
    labs(title = paste("Rendimiento de", crop, "por Año"),
         x = "Año",
         y = "Rendimiento") +
    theme_minimal()
  
  # Mostrar el gráfico
  print(p)
}