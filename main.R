library(dplyr)
csv_directory <- "DATA/"

csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

combined_data <- do.call(rbind, lapply(csv_files, read.csv))

head(combined_data,n=10)

filtered_data <- combined_data %>%
  filter(CountryCode == "BRA") %>%
  arrange(Value)

# Mostrar las primeras 10 filas del resultado
head(filtered_data, 10)