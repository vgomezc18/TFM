#Código para unir la base de datos de nacimientos del DANE

pacman::p_load(readxl, tidyverse, dplyr, here)


# Ruta

dir_path <- here::here("04.depurar_fuentes_externas/input/dane_nacimientos")

# Lista los archivos .xls y .XLS en el directorio
file_list_dane_nac <- list.files(path = dir_path, pattern = "\\.[xX][lL][sS]$", full.names = TRUE)

data_frames <- list()


# Iterar sobre cada archivo y procesar los datos
for (file in file_list_dane_nac) {
	# Leer la primera hoja del archivo
	sheet_data <- read_excel(file, sheet = 1)
	
	# Reemplazar '.' por NA
	sheet_data <- sheet_data %>% mutate_all(~ifelse(. == ".", NA, .))
	
	# Obtener el nombre base del archivo para usarlo como nombre del data frame
	df_name <- tools::file_path_sans_ext(basename(file))
	# Reemplazar caracteres no permitidos en nombres de variables
	df_name <- make.names(df_name)
	
	# Convertir la variable 'ano' a carácter
	if ("ano" %in% names(sheet_data)) {
		sheet_data <- sheet_data %>% mutate(ano = as.character(ano))
	}
	
	# Dejar únicamente los números en la variable 'Municipio'
	if ("Municipio" %in% names(sheet_data)) {
		sheet_data <- sheet_data %>% mutate(Municipio = str_extract(Municipio, "\\d+"))%>%
			mutate(Municipio = as.numeric(Municipio)) %>%
			mutate(Municipio = as.character(Municipio))
	}
	
	# Convertir 'Hombres' y 'Mujeres' a numérico
	if ("Hombres" %in% names(sheet_data)) {
		sheet_data <- sheet_data %>% mutate(Hombres = as.numeric(Hombres))
	}
	if ("Mujeres" %in% names(sheet_data)) {
		sheet_data <- sheet_data %>% mutate(Mujeres = as.numeric(Mujeres))
	}
	
	# Almacenar el data frame en la lista
	data_frames[[df_name]] <- sheet_data
}

# Asignar los data frames a variables en el entorno global
list2env(data_frames, envir = .GlobalEnv)


#Unir todos los años

nacimientos_dane <- rbind(X1998, X1999, X2000, X2001, X2002, X2003, X2004,
													X2005, X2006, X2007, X2008, X2009, X2010, X2011,
													X2012, X2013, X2014, X2015, X2016, X2017, X2018,
													X2019, X2020, X2021) %>%
	rename(tot_nacimientos = Total, 
				 nac_hombres = Hombres,
				 nac_mujeres = Mujeres,
				 codigo = Municipio)


rm(X1998, X1999, X2000, X2001, X2002, X2003, X2004,
	 X2005, X2006, X2007, X2008, X2009, X2010, X2011,
	 X2012, X2013, X2014, X2015, X2016, X2017, X2018,
	 X2019, X2020, X2021)

saveRDS(nacimientos_dane, (here::here("04.depurar_fuentes_externas/output/nacimientos_dane.rds")))

