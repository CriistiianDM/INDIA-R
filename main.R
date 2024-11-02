library(dplyr)
library(Amelia)
library(tidyr)
csv_directory <- "DATA/"

csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

combined_data <- do.call(rbind, lapply(csv_files, function(file) {
  read.csv(file, sep = ";", stringsAsFactors = FALSE)
}))

head(combined_data)

# CLEAN DATA

# Mirar la dimension inicial
dim(combined_data)

# al ver la estrutura de los datos
# Notamos que todo esta en una columna pero separado por comas ','
str(combined_data)

# Primero sera organizar el archivo a las 7 variables
combined_data_separated <- combined_data %>%
  separate(State_Name.District_Name.Crop_Year.Season.Crop.Area.Production,
           into = c("State_Name", "District_Name", "Crop_Year", "Season", "Crop", "Area", "Production"),
           sep = ",", extra = "merge", fill = "right")

# Verificamos que la salida tenga un date.frame de 7 variables
str(combined_data_separated)

# Guardar el data frame en un archivo CSV
# NOTA: Dejo comentado porque no quiero que guarde de nuevo
#write.csv(combined_data_separated, "C:/Users/USUARIO/Documents/GitHub/BRICS-R/DATA_NEW/2.csv", row.names = FALSE)

# Miramos si tenemos datos faltantes
# Podemos observar que nuestro conjunto de datos a analizar no tiene datos faltantes
missmap(combined_data_separated, main = "Mapa de datos faltantes", legend = TRUE)

# Como los datos estan completos pasamos a separarlos por columnas con el fin de poder 
# aplicar converciones de tipos
combined_data_separated <- combined_data %>%
  separate(State_Name.District_Name.Crop_Year.Season.Crop.Area.Production,
           into = c("State_Name", "District_Name", "Crop_Year", "Season", "Crop", "Area", "Production"),
           sep = ",", extra = "merge", fill = "right")

# Convertir columnas numéricas al tipo correcto
combined_data_separated <- combined_data_separated %>%
  mutate(
    Crop_Year = as.integer(Crop_Year),
    Area = as.numeric(Area),
    Production = as.numeric(Production)
)

# Revisamos la completirud de los datos pero esta vez no esta completos
missmap(combined_data_separated, main = "Mapa de datos faltantes", legend = TRUE)


# eliminamos las filas que tienen datos vacios

combined_data_clean <- na.omit(combined_data_separated)

# Revisamos la completirud de los datos pero esta vez no esta completos
missmap(combined_data_clean, main = "Mapa de datos faltantes", legend = TRUE)

# Vamos a eliminar datos repitidos que puedan alterar las conclusiones que queremos obener
combined_data_clean <- combined_data_clean %>% distinct()

# Revisamos casos atipicos para quitarlos
boxplot(combined_data_clean$Area, main = "Boxplot de Area")

# Podemos notar muchos valores atipicos. Vamos a tratar de eliminar los que estan muy por encima para tener un mejor analiis de los datos
# Para esto usaremos calcularemos los cuartiles
Q1 <- quantile(combined_data_clean$Area, 0.25)
Q3 <- quantile(combined_data_clean$Area, 0.75)
IQR <- Q3 - Q1
combined_data_clean <- combined_data_clean %>%
  filter(Area >= (Q1 - 1.5 * IQR) & Area <= (Q3 + 1.5 * IQR))

# Revisamos como quedaron los datos
boxplot(combined_data_clean$Area, main = "Boxplot de Area")

# Eliminamos espacios adcionales y convertimos todo a minusculas
combined_data_clean$Season <- trimws(combined_data_clean$Season)
combined_data_clean$Crop <- tolower(combined_data_clean$Crop)   

#Aplicamos un filtro para tener valores positivos
# El area debe ser mayor que 0 y la producion tambien
combined_data_clean <- combined_data_clean %>%
  filter(Crop_Year >= 1900 & Crop_Year <= as.integer(format(Sys.Date(), "%Y")) & Area > 0 & Production > 0)

# Revisamos un informe final de los datos para ver si tenemos que aplicar modificasiones
summary(combined_data_clean)

# Guardamos el archivo con los tipos de datos correctos y completos
# NOTA: Dejo comentado porque no quiero que guarde de nuevo y esta vez el pes
#write.csv(combined_data_separated, "C:/Users/USUARIO/Documents/GitHub/BRICS-R/DATA_NEW/3.csv", row.names = FALSE)

