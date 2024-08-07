#Código para unir y depurar la base de datos de defunciones del DANE

pacman::p_load(readxl, tidyverse, dplyr, here, purrr, readr)


# Ruta
dir_path <- here::here("04.depurar_fuentes_externas/input/dane_defunciones")
files_txt <- list.files(path = dir_path, pattern = "Defunciones_\\d{4}\\.txt$", full.names = TRUE)
files_csv <- list.files(path = dir_path, pattern = "Defunciones_\\d{4}\\.csv$", full.names = TRUE)

#######################
# Archivos TXT
# Define una función para procesar cada archivo
process_file_txt <- function(file) {

	data <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	names(data) <- tolower(names(data))
	
	data_summary <- data %>%
		group_by(cod_dpto, cod_munic, ano) %>%
		select(cod_dpto, cod_munic, ano) %>%
		summarise(defunciones_mpio = n(), .groups = 'drop')
	
	return(data_summary)
}

# Aplica la función a todos los archivos y combina los resultados en un solo data frame
all_data_txt <- map_df(files_txt, process_file_txt)


#######################
# Archivos csv
# Define una función para procesar cada archivo csv
process_file_csv <- function(file) {
	
	data <- read_delim(file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
	names(data) <- tolower(names(data))
	
	data_summary <- data %>%
		select(cod_dpto, cod_munic, ano) %>%
		group_by(cod_dpto, cod_munic, ano) %>%
		summarise(defunciones_mpio = n())
	
	return(data_summary)
}

# Aplica la función a todos los archivos y combina los resultados en un solo data frame
all_data_csv <- map_df(files_csv, process_file_csv)

#######################
# Unir todos los años y arreglar formato de las variables

defunciones_dane <- rbind(all_data_txt, all_data_csv) %>%
	mutate(cod_dpto = as.character(cod_dpto),
				 ano = as.character(ano),
				 cod_munic = sprintf("%03d", as.integer(cod_munic)),
				 codmpio = paste0(cod_dpto, cod_munic)) %>%
	select(codmpio, ano, defunciones_mpio)


saveRDS(defunciones_dane, (here::here("04.depurar_fuentes_externas/output/defunciones_dane.rds")))

