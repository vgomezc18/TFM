# Trabajo Final de Máster

Este repositorio contiene el detalle del código ejecutado para la elaboración del Trabajo Final de Máster titulado: **Contraste de métodos de imputación estadística en la documentación de víctimas del conflicto armado colombiano**

El flujo de trabajo consta de 10 pasos y se explican a continuación:

1. [01.tablas_documentadas](https://github.com/vgomezc18/TFM/tree/main/01.tablas_documentadas): En esta carpeta se encuentra el procesamiento de los datos para poder obtener las tablas documentadas de las violaciones de desaparición forzada, reclutamiento, homicidio y secuestro del proyecto JEP-CEV-HRDAG. Se utiliza el [paquete `verdata`](https://github.com/HRDAG/verdata) para importar los datos y se tiene en cuenta las funciones necesarias para obtener cada una de las tablas documentadas. Se tiene en cuenta las variables `match_group_id`, `dept_code_hecho`, `edad_categoria`, `edad_jep`, `etnia`, `muni_code_hecho`, `p_str`, `sexo`, `yy_hecho` para obtener los resultados, se procesa para cada violación y se unen en una misma base de datos.

2. [02.data_4_graphs](https://github.com/vgomezc18/TFM/tree/main/02.data_4_graphs): En esta carpeta se prepara la base de datos con las víctimas documentadas para la realización de gráficos necesarios para la elaboración del TFM. A la base de datos documentados del proyecto JEP-CEV-HRDAG se añaden variables como la macroregión, el periodo presidencial y las coordenadas de los departamentos para la elaboración de mapas.

3. [03.graphs](https://github.com/vgomezc18/TFM/tree/main/03.graphs): En esta carpeta se realizan gráficos de los datos faltantes existentes en la base de datos documentada y un gráfico del mapa de Colombia dividido por macroregiones. 

4. [04.depurar_fuentes_externas](https://github.com/vgomezc18/TFM/tree/main/04.depurar_fuentes_externas): En esta carpeta se depuran los datos de nacimientos, de defunciones y los datos de departamentos de Colombia, ya que 2 departamentos no tienen el código que corresponde.

5. [05.imputar_var_aux](https://github.com/vgomezc18/TFM/tree/main/05.imputar_var_aux): En esta carpeta se imputan las variables auxiliares que posteriormente se utilizaran para la imputación del modelo por donante, regresión y machine learning.

La carpeta de inputs no contiene las bases de datos que se usaron del Panel Municipal de Características Generales del [CEDE](https://datoscede.uniandes.edu.co/es/catalogo-de-microdata), para acceder a ellas es necesario ser estudiante o egresado de la Universidad de los Andes o solicitar permiso para acceder a ellas: 

- PANEL_BUEN_GOBIERNO(2023)
- PANEL_CARACTERISTICAS_GENERALES(2022)
- PANEL_CONFLICTO_Y_VIOLENCIA(2022)
- PANEL_DE_EDUCACION(2023)
- PANEL_SALUD_Y_SERVICIOS

6. [06.union](https://github.com/vgomezc18/TFM/tree/main/06.union): En esta carpeta se unen las variables auxiliares con la base de datos de datos documentados. Se filtran los datos de 1998 en adelante.

7. [07.descriptivos](https://github.com/vgomezc18/TFM/tree/main/07.descriptivos): En esta carpeta se realizan algunos conteos de los valores faltantes para cada una de las variables de interés (etnia, perpetrador, edad y sexo).

8. [08.modelos_imputacion](https://github.com/vgomezc18/TFM/tree/main/08.modelos_imputacion): Esta carpeta contiene los diferentes modelos de imputación (donante, machine_learning y regresion). 

9. [09.comparacion_modelos](https://github.com/vgomezc18/TFM/tree/main/09.comparacion_modelos): En esta carpeta se comparan los diferentes métodos de imputación (resultados JEP-CEV-HRDAG, imputación por donante, imputación por machine learning e imputación por regresión). Se realizan gráficos que permitan comparar los diferentes resultados. 

10. [10.validacion_modelos](https://github.com/vgomezc18/TFM/tree/main/10.validacion_modelos): En esta carpeta se validan los resultados obtenidos a través de los diferentes métodos de imputación (imputación por donante, imputación por machine learning e imputación por regresión). Se realiza una validación cruzada.


Autora: Valentina Gómez Calderón

Contacto: valegome@ucm.es - valengomez018@gmail.com