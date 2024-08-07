#Código para depurar la base de datos de Departamentos de Colombia

pacman::p_load(readxl, tidyverse, dplyr, here)


# Ruta
# https://www.datos.gov.co/Mapas-Nacionales/Departamentos-y-municipios-de-Colombia/xdk5-pm3f/about_data


departamentos_colombia <- read_csv(here::here("04.depurar_fuentes_externas/input/departamentos_colombia/Departamentos_y_municipios_de_Colombia_20240723.csv")) %>%
	mutate(codmpio = sprintf("%04d", as.integer(`CÓDIGO DANE DEL MUNICIPIO` * 1000)),
				 coddepto = as.character(`CÓDIGO DANE DEL DEPARTAMENTO`),
				 depto = DEPARTAMENTO,
				 mpio = MUNICIPIO) %>%
	select(codmpio, coddepto, depto, mpio)

# Barranquilla y Baranoa tienen un código de municipio que no corresponde. 
departamentos_colombia <- departamentos_colombia %>%
	mutate(codmpio = case_when(
		mpio == "Barranquilla" ~ "8001",
		mpio == "Baranoa" ~ "8078",
		TRUE ~ codmpio))


saveRDS(departamentos_colombia, (here::here("04.depurar_fuentes_externas/output/departamentos_colombia.rds")))

