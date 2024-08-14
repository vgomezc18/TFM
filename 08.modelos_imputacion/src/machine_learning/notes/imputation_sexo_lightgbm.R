s# Load library ####
library(data.table)
library(Matrix)
library(vcd)
library(xgboost)
library(caret)
library(ggplot2)
library(pROC)

# Set path and filenames ####
#path_data <- 'N:/UDMTD08/00 - Descargas'
path_data <- 'C:/Users/U000000/Documents'
data_fn <- 'violencias_unidas_aux.rds'

# Read data ####
data.dt <- as.data.table(readRDS(file.path(path_data, data_fn)))

# Prepare data (encodings) ####
regressors_sexo <- c("macroregion", "dept_code_hecho", "muni_code_hecho", "muni_code_hecho_imp", "areaoficialkm2_mpio", "discapital_mpio", "frontera_mpio",
                     "yy_hecho",
                     "violencia", 
                     "etnia", "p_str", "edad_categoria",
                     "pobl_tot_mpio", "indrural_mpio", "predo_rural_mpio",
                     "y_transf_depto", "g_func_general_depto",  
                     "H_coca_depto",
                     "asistesc_depto", "per_alfa_depto", "alumn_total_depto"
)

data.dt <- data.dt[
  , c('sexo', regressors_sexo), with = FALSE][
  , yy_hecho := as.numeric(yy_hecho)][
  , dept_code_hecho := as.numeric(dept_code_hecho)][
  , muni_code_hecho := as.numeric(muni_code_hecho)][
  , muni_code_hecho_imp := as.numeric(muni_code_hecho_imp)][
  is.na(etnia), etnia := '*'][
  is.na(edad_categoria), edad_categoria := '*'][
  is.na(p_str), p_str := '*']

macroregion_levels <- sort(unique(data.dt$macroregion))
macroregion_enc <- seq_along(macroregion_levels) - 1
names(macroregion_enc) <- macroregion_levels

violencia_levels <- sort(unique(data.dt$violencia))
violencia_enc <- seq_along(violencia_levels) - 1
names(violencia_enc) <- violencia_levels

pstr_levels <- sort(unique(data.dt$p_str))
pstr_enc <- seq_along(pstr_levels) - 1
names(pstr_enc) <- pstr_levels

etnia_levels <- sort(unique(data.dt$etnia))
etnia_enc <- seq_along(etnia_levels) - 1
names(etnia_enc) <- etnia_levels

edadCat_levels <- sort(unique(data.dt$edad_categoria))
edadCat_enc <- seq_along(edadCat_levels) - 1
names(edadCat_enc) <- edadCat_levels

edadJep_levels <- sort(unique(data.dt$edad_jep))
edadJep_enc <- seq_along(edadJep_levels) - 1
names(edadJep_enc) <- edadJep_levels

sexo_levels <- sort(unique(data.dt$sexo))
sexo_enc <- seq_along(sexo_levels) - 1
names(sexo_enc) <- sexo_levels

data_enc.dt <- copy(data.dt)[
  , sexo := sexo_enc[sexo]][
  , macroregion := macroregion_enc[macroregion]][
  , violencia := violencia_enc[violencia]][
  , etnia := etnia_enc[etnia]][
  , p_str := pstr_enc[p_str]][
  , edad_categoria := edadCat_enc[edad_categoria]]


# Variable a imputar: sexo
# Mantenemos las variables etnia, violencia, p_str, edad_categoria como regresores
# convirtiendo sus NA en una nueva categoría * (está hecho más arriba)
data_sexo_enc.dt <- data_enc.dt[          # datos para contruir el modelo
  !is.na(sexo)]

data_sexo_enc_impute.dt <- data_enc.dt[   # datos a imputar
  is.na(sexo)]

# Build model with undersampling and without cross-validation ####
# Submuestreamos para tratar la falta de balance en la variable sexo
table(data_sexo_enc.dt$sexo, useNA = 'always')
n_mujeres <- data_sexo_enc.dt[sexo == 1, .N]
n_hombres <- data_sexo_enc.dt[sexo == 0, .N]
set.seed(412)
data_sexo_undersampl.dt <- rbindlist(list(
  data_sexo_enc.dt[sexo == 0][
    sample(c(TRUE, FALSE), n_hombres, prob = c(n_mujeres / n_hombres, 1 - n_mujeres / n_hombres) , replace = TRUE)],
  data_sexo_enc.dt[sexo == 1]))
table(data_sexo_undersampl.dt$sexo, useNA = 'always')

data_sexo_train <- lgb.Dataset(
  Matrix(as.matrix(data_sexo_undersampl.dt[, -1]), sparse = TRUE), 
  label = data_sexo_undersampl.dt$sexo,
  categorical_feature = c(1L, 2L, 3L, 4L, 7L, 9L, 10L, 11L, 12L)
)    
    
# categóricas: "macroregion", "dept_code_hecho", 
#              "muni_code_hecho", "muni_code_hecho_imp",
#              "frontera_mpio", "violencia", "etnia",
#              "p_str", "edad_categoria"))


lgb_grid <- list(
  objective = "binary",
  metric    = "auc",
  bagging_freq = 5,
  force_row_wise = TRUE
)
model_sexo_impute <- lightgbm(
  data = data_sexo_train, 
  params = lgb_grid, 
  nrounds = 5000L, 
  verbose = 2L
)

## AUC, ROC and confusion matrix for undersampled train data ####
prob_sexo_train    <- model_sexo_impute$predictions[, 'HOMBRE']
roc_sexo_train.roc <- roc(data_sexo_undersampl.dt$sexo, prob_sexo_train) # Comparando con pruebas sin submuestreo la AUC no parece cambiar mucho
thrsh <- coords(roc_sexo_train.roc, 'best', best.method="closest.topleft")[["threshold"]]
data_sexo_undersampl.dt[
  , sexo := factor(sexo, levels = c('HOMBRE', 'MUJER'))][
    , prob_hombre := prob_sexo_train][
      , sexo_pred := ifelse(prob_hombre >= thrsh, 'HOMBRE', 'MUJER')][
        , sexo_pred := factor(sexo_pred, levels = c('HOMBRE', 'MUJER'))]
confMat_sexo_train.lst <- caret::confusionMatrix(as.factor(data_sexo_undersampl.dt$sexo_pred), as.factor(data_sexo_undersampl.dt$sexo))
confMat_sexo_train.lst$byClass
confMat_sexo_train.lst$table


prob_sexo_train    <- predict(model_sexo_impute, Matrix(as.matrix(data_sexo_undersampl.dt[, -1]), sparse = TRUE))
roc_sexo_train.roc <- roc(data_sexo_undersampl.dt$sexo, prob_sexo_train) # Comparando con pruebas sin submuestreo la AUC no parece cambiar mucho
thrsh <- coords(roc_sexo_train.roc, 'best', best.method="closest.topleft")[["threshold"]]
data_sexo_undersampl.dt[
  , prob_mujer := prob_sexo_train][
  , sexo_pred := ifelse(prob_mujer >= thrsh, 1, 0)]
confMat_sexo_train.lst <- caret::confusionMatrix(as.factor(data_sexo_undersampl.dt$sexo_pred), as.factor(data_sexo_undersampl.dt$sexo))
confMat_sexo_train.lst$byClass
confMat_sexo_train.lst$table



## Impute sex with model (undersampled; no CV) ####
data_sexo_impute <- Matrix(as.matrix(data_sexo_enc_impute.dt[, -1]), sparse = TRUE)
prob_sexo_impute <- predict(model_sexo_impute, data_sexo_impute)
data_sexo_enc_impute.dt[
  , prob_mujer := prob_sexo_impute][
  , sexo := ifelse(prob_mujer >= thrsh_completo, 1, 0)]

sexo_imp_enc <- data_sexo_enc_impute.dt$sexo
sexo_imp <- ifelse(sexo_imp_enc == 0, 'HOMBRE', 'MUJER')
data_sexo_imputed.dt <- copy(data.dt)[
  , sexo_imp := ifelse(is.na(sexo), TRUE, FALSE)][
  is.na(sexo), sexo := sexo_imp]

#saveRDS(data_sexo_imputed.dt, file.path(path_data, 'data_sexo_imputed.dt.rds'))

## Visualise  ####
# Calculamos ratios de hombres y mujeres y visualizamos comparando antes y después de comparar
ratio_sexo_original.dt <- data_sexo_imputed.dt[
  sexo_imp == FALSE, .(ratio_mujeres = sum(sexo == 'MUJER' & sexo_imp == FALSE) / .N, 
                       ratio_hombres = sum(sexo == 'HOMBRE' & sexo_imp == FALSE) / .N), by = c("muni_code_hecho", "yy_hecho", "violencia")]
ratio_missing_original.dt <- data_sexo_imputed.dt[
  , .(ratio_missing = sum(sexo_imp) / .N), by = c("muni_code_hecho", "yy_hecho", "violencia")][
  , dataset := 'original']

ratio_sexo_original.dt <- merge(ratio_sexo_original.dt, ratio_missing_original.dt, by = c("muni_code_hecho", "yy_hecho", "violencia"), all = TRUE)[
  ratio_missing >= 1 & is.na(ratio_mujeres), ratio_mujeres := 0][
  ratio_missing >= 1 & is.na(ratio_hombres), ratio_hombres := 0]

ratio_sexo_imputado.dt <- data_sexo_imputed.dt[
  , .(ratio_mujeres = sum(sexo == 'MUJER') / .N, 
  ratio_hombres = sum(sexo == 'HOMBRE') / .N), by = c("muni_code_hecho", "yy_hecho", "violencia")][
  , ratio_missing := 0][
  , dataset := 'imputado']

ratio_sexo.dt <- rbindlist(list(ratio_sexo_imputado.dt, ratio_sexo_original.dt))

### 1. imputation rates ####
ggplot(ratio_sexo.dt, aes(x = dataset, y = ratio_missing, fill = dataset)) +
  geom_boxplot() +
  facet_grid(violencia ~ yy_hecho) +
  labs(x = '', y = 'Tasa de imputación\n') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### 2. sex distributions ####
ggplot(ratio_sexo.dt[ratio_missing < 1], aes(x = dataset, y = ratio_hombres, fill = dataset)) +
  geom_boxplot() +
  facet_grid(violencia ~ yy_hecho) +
  labs(x = '', y = 'Ratio de hombres\n') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## La imputación no parece perturbar los ratios con estos niveles de desagregación, salvo cuando el número de missing es muy alto
