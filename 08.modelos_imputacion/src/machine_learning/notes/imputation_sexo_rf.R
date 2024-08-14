# Load library ####
library(data.table)
library(ranger)
library(caret)
library(ggplot2)
library(pROC)

# Set path and filenames ####
#path_data <- 'N:/UDMTD08/00 - Descargas'
path_data <- 'C:/Users/U000000/Documents'
data_fn <- 'violencias_unidas_aux.rds'

# Read data ####
data.dt <- as.data.table(readRDS(file.path(path_data, data_fn)))

data.dt[
  , sexo := factor(sexo)][  
  is.na(etnia), etnia := '*'][
  is.na(edad_categoria), edad_categoria := '*'][
  is.na(edad_jep), edad_jep := '*'][
  is.na(p_str), p_str := '*']
          

# Prepare data ####
regressors_sexo <- c(
  "macroregion", "dept_code_hecho", "muni_code_hecho", "muni_code_hecho_imp", "areaoficialkm2_mpio", "discapital_mpio", "frontera_mpio",
  "yy_hecho",
  "violencia", 
  "etnia", "p_str", "edad_categoria",
  "pobl_tot_mpio", "indrural_mpio", "predo_rural_mpio",
  "y_transf_depto", "g_func_general_depto",  
  "H_coca_depto",
  "asistesc_depto", "per_alfa_depto", "alumn_total_depto"
  #"nac_hombres_mpio", "nac_mujeres_mpio", "tot_nacimientos_mpio", "tot_nacimientos_depto", "nac_hombres_depto", "nac_mujeres_depto",   
  #"defunciones_mpio", "tot_defunciones_depto"
)

data_sexo.dt <- data.dt[          # datos para contruir el modelo
  !is.na(sexo)][
  , c('sexo', regressors_sexo), with = FALSE]
 
data_sexo_impute.dt <- data.dt[   # datos a imputar
  is.na(sexo)][ # Unidades a imputar 
  , c('sexo', regressors_sexo), with = FALSE] 

# Build model with undersampling and without cross-validation ####
# Variable a imputar: sexo
# Mantenemos las variables etnia, violencia, p_str, edad_categoria como regresores
# convirtiendo sus NA en una nueva categoría * (está hecho más arriba)
# Submuestreamos para tratar la falta de balance en la variable sexo
table(data_sexo.dt$sexo, useNA = 'always')
n_mujeres <- data_sexo.dt[sexo == 'MUJER', .N]
n_hombres <- data_sexo.dt[sexo == 'HOMBRE', .N]
set.seed(412)
data_sexo_undersampl.dt <- rbindlist(list(
  data_sexo.dt[sexo == 'HOMBRE'][
    sample(c(TRUE, FALSE), n_hombres, prob = c(n_mujeres / n_hombres, 1 - n_mujeres / n_hombres) , replace = TRUE)],
  data_sexo.dt[sexo == 'MUJER']))
table(data_sexo_undersampl.dt$sexo, useNA = 'always')

# Predecimos con unos hiperparámetros en la dirección de los óptimos (idealmente deberían encontrarse con CV)
model_sexo_impute <- ranger(
  x = data_sexo_undersampl.dt[, ..regressors_sexo], 
  y = data_sexo_undersampl.dt$sexo,
  mtry = 4,
  min.bucket = 10, 
  num.trees = 5000,
  importance = 'permutation',
  probability = TRUE, # atención: guardamos la probabilidad para hacer manualmente la predicción
  verbose = TRUE)

## Variable importance (permutation) ####
importnce <- sort(model_sexo_impute$variable.importance, decreasing = TRUE)
data_sexo_importance.dt <- data.table(
  variable = names(importnce),
  importancia = importnce)
ggplot(data_sexo_importance.dt, aes(x = reorder(variable, importancia), y = importancia)) +
  geom_col() +
  coord_flip() +
  labs(x = '', y = 'Importancia (permutación)') +
  theme_bw()

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

## AUC, ROC and confusion matrix for all train data ####
pred_sexo_train <- predict(model_sexo_impute, data = data_sexo.dt)
prob_sexo_train_completo <- pred_sexo_train$predictions[, 'HOMBRE']
roc_sexo_train_completo.roc <- roc(data_sexo.dt$sexo, prob_sexo_train_completo) # Comparando el caso con submuestreo la AUC no parece cambiar mucho
thrsh_completo <- coords(roc_sexo_train_completo.roc, 'best', best.method="closest.topleft")[["threshold"]]
data_sexo.dt[
  , sexo := factor(sexo, levels = c('HOMBRE', 'MUJER'))][
  , prob_hombre := pred_sexo_train$predictions[, 'HOMBRE']][
  , sexo_pred := ifelse(prob_hombre >= thrsh_completo, 'HOMBRE', 'MUJER')][
  , sexo_pred := factor(sexo_pred, levels = c('HOMBRE', 'MUJER'))]
confMat_sexo_completo.lst <- caret::confusionMatrix(as.factor(data_sexo.dt$sexo_pred), as.factor(data_sexo.dt$sexo))
confMat_sexo_completo.lst$byClass
confMat_sexo_completo.lst$table

## Impute sex with model (undersampled; no CV) ####
pred_sexo_impute <- predict(model_sexo_impute, data = data_sexo_impute.dt)
prob_sexo_impute <- pred_sexo_impute$predictions[, 'HOMBRE']
data_sexo_impute.dt[
  , prob_hombre := prob_sexo_impute][
  , sexo := ifelse(prob_hombre >= thrsh_completo, 'HOMBRE', 'MUJER')][
  , sexo := factor(sexo, levels = c('HOMBRE', 'MUJER'))]
data_sexo_imputed.dt <- copy(data.dt)[
  , sexo_imp := ifelse(is.na(sexo), TRUE, FALSE)][
  is.na(sexo), sexo := data_sexo_impute.dt$sexo]
# saveRDS(data_sexo_imputed.dt, file.path(path_data, 'data_sexo_imputed.dt.rds'))

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
## No tenemos en cuenta los municipios que se imputan por completo (ratio_missing == 1)
ggplot(ratio_sexo.dt[ratio_missing < 1], aes(x = dataset, y = ratio_hombres, fill = dataset)) +
  geom_boxplot() +
  facet_grid(violencia ~ yy_hecho) +
  labs(x = '', y = 'Ratio de hombres\n') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## La imputación no parece perturbar los ratios 
## por año y tipo de violencia, salvo cuando el 
## número de missing es muy alto



# Build model without undersampling and without cross-validation ####
# Variable a imputar: sexo
# Mantenemos las variables etnia, violencia, p_str, edad_categoria como regresores
# convirtiendo sus NA en una nueva categoría * (está hecho más arriba)

# Predecimos con unos hiperparámetros en la dirección de los óptimos encontrados (idealmente deberían estar en los anteriores)
model_sexo_impute <- ranger(
  x = data_sexo.dt[, ..regressors_sexo], 
  y = data_sexo.dt$sexo,
  mtry = 4,
  min.bucket = 10, 
  num.trees = 5000,
  importance = 'permutation',
  probability = TRUE, # atención: guardamos la probabilidad para hacer manualmente la predicción
  verbose = TRUE)

## Variable importance (permutation) ####
importnce <- sort(model_sexo_impute$variable.importance, decreasing = TRUE)
data_sexo_importance.dt <- data.table(
  variable = names(importnce),
  importancia = importnce)
ggplot(data_sexo_importance.dt, aes(x = reorder(variable, importancia), y = importancia)) +
  geom_col() +
  coord_flip() +
  labs(x = '', y = 'Importancia (permutación)') +
  theme_bw()

## AUC, ROC and confusion matrix for all train data ####
pred_sexo_train <- predict(model_sexo_impute, data = data_sexo.dt)
prob_sexo_train_completo <- pred_sexo_train$predictions[, 'HOMBRE']
roc_sexo_train_completo.roc <- roc(data_sexo.dt$sexo, prob_sexo_train_completo) # Comparando el caso con submuestreo la AUC no parece cambiar mucho
thrsh_completo <- coords(roc_sexo_train_completo.roc, 'best')[["threshold"]]
data_sexo.dt[
  , sexo := factor(sexo, levels = c('HOMBRE', 'MUJER'))][
    , prob_hombre := pred_sexo_train$predictions[, 'HOMBRE']][
      , sexo_pred := ifelse(prob_hombre >= thrsh_completo, 'HOMBRE', 'MUJER')][
        , sexo_pred := factor(sexo_pred, levels = c('HOMBRE', 'MUJER'))]
confMat_sexo_completo.lst <- caret::confusionMatrix(as.factor(data_sexo.dt$sexo_pred), as.factor(data_sexo.dt$sexo))
confMat_sexo_completo.lst$byClass

## Impute sex with model (no undersampling; no CV) ####
pred_sexo_impute <- predict(model_sexo_impute, data = data_sexo_impute.dt)
prob_sexo_impute <- pred_sexo_impute$predictions[, 'HOMBRE']
data_sexo_impute.dt[
  , prob_hombre := prob_sexo_impute][
    , sexo := ifelse(prob_hombre >= thrsh_completo, 'HOMBRE', 'MUJER')][
      , sexo := factor(sexo, levels = c('HOMBRE', 'MUJER'))]
data_sexo_imputed.dt <- copy(data.dt)[
  , sexo_imp := ifelse(is.na(sexo), TRUE, FALSE)][
    is.na(sexo), sexo := data_sexo_impute.dt$sexo]

#saveRDS(data_sexo_imputed.dt, file.path(path_data, 'data_sexo_imputed.dt.rds'))


## Visualise ####
# Calculamos ratios de hombres y mujeres y visualizamos comparando antes y después de comparar
ratio_sexo_original.dt <- data_sexo_imputed_nosub.dt[
  sexo_imp == FALSE, .(ratio_mujeres = sum(sexo == 'MUJER' & sexo_imp == FALSE) / .N, 
                       ratio_hombres = sum(sexo == 'HOMBRE' & sexo_imp == FALSE) / .N), by = c("muni_code_hecho", "yy_hecho", "violencia")]
ratio_missing_original.dt <- data_sexo_imputed_nosub.dt[
  , .(ratio_missing = sum(sexo_imp) / .N), by = c("muni_code_hecho", "yy_hecho", "violencia")][
    , dataset := 'original']

ratio_sexo_original.dt <- merge(ratio_sexo_original.dt, ratio_missing_original.dt, by = c("muni_code_hecho", "yy_hecho", "violencia"), all = TRUE)[
  ratio_missing >= 1 & is.na(ratio_mujeres), ratio_mujeres := 0][
    ratio_missing >= 1 & is.na(ratio_hombres), ratio_hombres := 0]

ratio_sexo_imputado.dt <- data_sexo_imputed_nosub.dt[
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

# :===============================: ####
# AUC, ROC y matriz de confusión para el conjunto de entrenamiento completo sin submuestreo ####
model_sexo_impute_nosub <- ranger(
  x = data_sexo.dt[, ..regressors], 
  y = data_sexo.dt$sexo,
  mtry = 4,
  min.bucket = 10, 
  num.trees = 5000,
  importance = 'permutation',
  probability = TRUE, # atención: guardamos la probabilidad para hacer manualmente la predicción
  verbose = TRUE)
prob_sexo_train_nosub    <- model_sexo_impute_nosub$predictions[, 'HOMBRE']
roc_sexo_train_nosub.roc <- roc(data_sexo.dt$sexo, prob_sexo_train_nosub) # Comparando con pruebas sin submuestreo la AUC no parece cambiar mucho
thrsh_nosub <- coords(roc_sexo_train_nosub.roc, 'best')[["threshold"]]
data_sexo_nosub.dt <- copy(data_sexo.dt)[
  , sexo := factor(sexo, levels = c('HOMBRE', 'MUJER'))][
    , prob_hombre := prob_sexo_train_nosub][
      , sexo_pred := ifelse(prob_hombre >= thrsh_nosub, 'HOMBRE', 'MUJER')][
        , sexo_pred := factor(sexo_pred, levels = c('HOMBRE', 'MUJER'))]
confMat_sexo_train_nosub.lst <- caret::confusionMatrix(as.factor(data_sexo_nosub.dt$sexo_pred), as.factor(data_sexo_nosub.dt$sexo))
confMat_sexo_train_nosub.lst$byClass
# Imputamos sexo con la predicción del modelo sin submuestreo
pred_sexo_impute_nosub <- predict(model_sexo_impute_nosub, data = data_sexo_impute.dt)
prob_sexo_impute_nosub <- pred_sexo_impute_nosub$predictions[, 'HOMBRE']
data_sexo_impute.dt[
  , prob_hombre_nosub := prob_sexo_impute_nosub][
    , sexo_nosub := ifelse(prob_hombre_nosub >= thrsh_nosub, 'HOMBRE', 'MUJER')][
      , sexo_nosub := factor(sexo_nosub, levels = c('HOMBRE', 'MUJER'))]
data_sexo_imputed_nosub.dt <- copy(data.dt)[
  , sexo_imp := ifelse(is.na(sexo), TRUE, FALSE)][
    is.na(sexo), sexo := data_sexo_impute.dt$sexo_nosub]

saveRDS(data_sexo_imputed_nosub.dt, file.path(path_data, 'data_sexo_imputed_nosub.dt.rds'))


## Manual (non-repeated) k-fold cross-validation ####
## Grid search for hyperparameters
grid_hyperp.dt <- as.data.table(expand.grid(
  mtry = c(4, length(regressors) - 3),
  min.node.size = c(50, 500),
  num.trees = c(100, 500)))

# Highly unbalanced dataset. We undersample on sexo == 'HOMBRE'
table(data_sexo.dt$sexo)
n_mujeres <- data_sexo.dt[sexo == 'MUJER', .N]
n_hombres <- data_sexo.dt[sexo == 'HOMBRE', .N]
data_sexo_undersampl.dt <- rbindlist(list(
  data_sexo.dt[sexo == 'HOMBRE'][
    sample(c(TRUE, FALSE), n_hombres, prob = c(n_mujeres / n_hombres, 1 - n_mujeres / n_hombres) , replace = TRUE)],
  data_sexo.dt[sexo == 'MUJER']))

#Construimos los 5 folds
n_rf    <- nrow(data_sexo_undersampl.dt)
n_folds <- 5
set.seed(543) # For k-fold reproducibility
data_sexo_undersampl.dt[, fold_idx := sample(1:n_folds, n_rf, replace = TRUE)]

# Train-test para cada fold y cada celda de hiperparámetros
roc_sexo_train.lst <- vector(mode = 'list', length = nrow(grid_hyperp.dt))
roc_sexo_test.lst  <- vector(mode = 'list', length = nrow(grid_hyperp.dt))
prob_sexo_test.lst  <- vector(mode = 'list', length = nrow(grid_hyperp.dt))
n_cells <- nrow(grid_hyperp.dt)
for (grid_id in 1:n_cells){
  
  mtry          <- grid_hyperp.dt[grid_id][['mtry']]
  min.node.size <- grid_hyperp.dt[grid_id][['min.node.size']]
  num.trees     <- grid_hyperp.dt[grid_id][['num.trees']]
  
  cat(paste0('mtry= ', mtry, '; min.node.size= ', min.node.size, '; num.trees= ', num.trees, '\n'))
  
  roc_sexo_train.lst[[grid_id]] <- vector('list', n_folds)
  
  for (fold in 1:n_folds){
    
    
    data_train.dt <- data_sexo_undersampl.dt[fold_idx != fold]
    data_test.dt  <- data_sexo_undersampl.dt[fold_idx == fold]
    model_sexo <- ranger(
      sexo ~ ., 
      data = data_train.dt,
      splitrule = "gini",
      mtry = mtry,
      min.node.size = min.node.size,
      num.trees = num.trees,
      probability = TRUE)
    pred_sexo_train  <- predict(model_sexo, data = data_train.dt)
    roc_sexo_train.lst[[grid_id]][[fold]] <- roc(data_train.dt$sexo, pred_sexo_train$predictions[, 'HOMBRE'], levels = c('HOMBRE', 'MUJER'))
    
    pred_sexo_test  <- predict(model_sexo, data = data_test.dt)
    roc_sexo_test.lst[[grid_id]][[fold]] <- roc(data_test.dt$sexo, pred_sexo_test$predictions[, 'HOMBRE'], levels = c('HOMBRE', 'MUJER')) 
    prob_sexo_test.lst[[grid_id]][[fold]] <- pred_sexo_test$predictions
    cat(paste0('   fold= ', fold, '; auc_pred= ', auc(roc_sexo_test.lst[[grid_id]][[fold]]), '\n'))
  }    
  names(roc_sexo_train.lst[[grid_id]]) <- paste0('fold= ', 1:n_folds)
  names(roc_sexo_test.lst[[grid_id]])  <- paste0('fold= ', 1:n_folds)
  
}
names(roc_sexo_train.lst) <- paste0('hyper_cell= ', 1:n_cells)
names(roc_sexo_test.lst) <- paste0('hyper_cell= ', 1:n_cells)

#Calculamos el AUC en cada fold y cada celda
auc_sexo_train.lst <- lapply(roc_sexo_train.lst, function(lst){sapply(lst, function(roc){auc(roc)})})
auc_sexo_test.lst  <- lapply(roc_sexo_test.lst,  function(lst){sapply(lst, function(roc){auc(roc)})})

# Tomamos la media de las AUC en cada celda
auc_sexo_train_main <- sapply(auc_sexo_train.lst, mean)
auc_sexo_test_main <- sapply(auc_sexo_test.lst, mean)
auc.dt <- data.table(
  hyper_cell = seq_along(auc_sexo_test_main), 
  train = round(auc_sexo_train_main, 3), 
  test  = round(auc_sexo_test_main, 3))
auc.dt <- auc.dt[
  grid_hyperp.dt[, hyper_cell := 1:.N], on = 'hyper_cell']
auc.dt[which.max(auc.dt$test)]

# (el modelo óptimo parece darse cuando mtry es bajo, min.node.size es bajo y num.trees es alto)
# (el valor de auc no es muy bueno; quizá haya que probar un boosting)
# (el undersampling no parece mejorar el modelo)
# (la cantidad de overfitting no parece reducirse mucho con el undersampling)


