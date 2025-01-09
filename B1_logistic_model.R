library(tidyverse)
library(foreach)
library(doParallel)




rename_vars <- function(data){
  aux = data
  imuno = colnames(aux$df)[aux$fil_immuno]
  tam =length(imuno) 
  vari = rep("a",tam)
  vari = paste(vari,1:tam,sep = "")
  colnames(aux$df)[aux$fil_immuno] = vari
  return(list(data=aux,col_imuno = imuno))
}

regress1 <- function(data, njobs = 15){
  aux <- rename_vars(data)
  imuno <- aux$col_imuno
  tam <- length(imuno)
  df <- aux$data$df
  cl <- makeCluster(njobs)
  registerDoParallel(cl)
  result <- foreach(i = 1:tam, .combine = 'rbind', .packages = 'stats') %dopar% {
    suppressWarnings(
      tryCatch({
        formula_str <- paste0("disease ~ a", i, " + gender + age")
        model <- glm(formula_str, data = df, family = binomial())
        coef_name <- paste0("a", i)
        or <- exp(coef(model))[coef_name]
        ci <- exp(confint(model, parm = coef_name))
        ci_lower <- ci[1]
        ci_upper <- ci[2]
        pvalue <- coef(summary(model))[coef_name, "Pr(>|z|)"]
        data.frame(or = or, cl = ci_lower, ch = ci_upper, pvalue = pvalue)
      }, error = function(e) {
        data.frame(or = NA, cl = NA, ch = NA, pvalue = NA)
      })
    )
  }
  
  stopCluster(cl)
  df_result <- data.frame(vars = imuno, result)
  return(df_result)
}

regress2 <- function(data, njobs = 15){
  aux <- rename_vars(data)
  imuno <- aux$col_imuno
  tam <- length(imuno)
  df <- aux$data$df
  
  # Padronizar a idade
  df$age_std <- as.numeric(scale(df$age, center = TRUE, scale = TRUE))
  
  cl <- makeCluster(njobs)
  registerDoParallel(cl)
  
  result <- foreach(i = 1:tam, .combine = 'rbind', .packages = c('stats')) %dopar% {
    suppressWarnings(
      tryCatch({
        # Fórmulas dos modelos
        formula_full <- as.formula(paste("disease ~ a", i, " * age_std + gender", sep = ""))
        formula_null <- as.formula(paste("disease ~ a", i, " + age_std + gender", sep = ""))
        
        # Ajuste dos modelos
        model_full <- glm(formula_full, data = df, family = binomial())
        model_null <- glm(formula_null, data = df, family = binomial())
        
        # Teste da razão de verossimilhança
        lr_test <- anova(model_null, model_full, test = "LRT")
        pvalue_lrtest <- lr_test$"Pr(>Chi)"[2]
        
        # Nomes dos coeficientes
        beta2_name <- paste0("a", i)
        beta3_name <- paste0("a", i, ":age_std")
        
        # OR e intervalos de confiança para β2
        or_beta2 <- exp(coef(model_full)[beta2_name])
        ci_beta2 <- exp(confint(model_full, parm = beta2_name))
        ci_beta2_lower <- ci_beta2[1]
        ci_beta2_upper <- ci_beta2[2]
        pvalue_beta2 <- summary(model_full)$coefficients[beta2_name, "Pr(>|z|)"]
        
        # OR e intervalos de confiança para β3
        or_beta3 <- exp(coef(model_full)[beta3_name])
        ci_beta3 <- exp(confint(model_full, parm = beta3_name))
        ci_beta3_lower <- ci_beta3[1]
        ci_beta3_upper <- ci_beta3[2]
        pvalue_beta3 <- summary(model_full)$coefficients[beta3_name, "Pr(>|z|)"]
        
        data.frame(
          pvalue_lrtest = pvalue_lrtest,
          or_beta2 = or_beta2,
          ci_beta2_lower = ci_beta2_lower,
          ci_beta2_upper = ci_beta2_upper,
          pvalue_beta2 = pvalue_beta2,
          or_beta3 = or_beta3,
          ci_beta3_lower = ci_beta3_lower,
          ci_beta3_upper = ci_beta3_upper,
          pvalue_beta3 = pvalue_beta3
        )
      }, error = function(e) {
        data.frame(
          pvalue_lrtest = NA,
          or_beta2 = NA,
          ci_beta2_lower = NA,
          ci_beta2_upper = NA,
          pvalue_beta2 = NA,
          or_beta3 = NA,
          ci_beta3_lower = NA,
          ci_beta3_upper = NA,
          pvalue_beta3 = NA
        )
      })
    )
  }
  
  stopCluster(cl)
  df_result <- data.frame(vars = imuno, result)
  return(df_result)
}

regress3 <- function(data, njobs = 15){
  aux <- rename_vars(data)
  imuno <- aux$col_imuno
  tam <- length(imuno)
  df <- aux$data$df
  
  # Padronizar a idade (se ainda não foi padronizada)
  if (!"age_std" %in% colnames(df)) {
    df$age_std <- as.numeric(scale(df$age, center = TRUE, scale = TRUE))
  }
  
  cl <- makeCluster(njobs)
  registerDoParallel(cl)
  
  result <- foreach(i = 1:tam, .combine = 'rbind', .packages = c('stats')) %dopar% {
    suppressWarnings(
      tryCatch({
        # Fórmula do modelo linear
        formula_lm <- as.formula(paste("a", i, " ~ age_std + gender", sep = ""))
        
        # Ajuste do modelo
        model_lm <- lm(formula_lm, data = df)
        
        # Nome do coeficiente de idade padronizada
        beta_name <- "age_std"
        
        # Extração dos coeficientes
        beta <- coef(model_lm)[beta_name]
        confint_beta <- confint(model_lm, parm = beta_name)
        ci_lower <- confint_beta[1]
        ci_upper <- confint_beta[2]
        pvalue <- summary(model_lm)$coefficients[beta_name, "Pr(>|t|)"]
        
        data.frame(
          beta = beta,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          pvalue = pvalue
        )
      }, error = function(e) {
        data.frame(
          beta = NA,
          ci_lower = NA,
          ci_upper = NA,
          pvalue = NA
        )
      })
    )
  }
  
  stopCluster(cl)
  df_result <- data.frame(vars = imuno, result)
  return(df_result)
}
################################################################################

data = readRDS("./pos_data/dataset.rds")

res1_base_norm = regress1(data$base_norm)
res1_fil_norm = regress1(data$fil_norm)

res2_base_norm = regress2(data$base_norm)
res2_fil_norm = regress2(data$fil_norm)

res3_base_norm = regress3(data$base_norm)
res3_fil_norm = regress3(data$fil_norm)

colnames(res1_base_norm) = c("vars","or_mark_dis","cl_mark_dis","ch_mark_dis","pvalue_mark_dis")
colnames(res1_fil_norm) = c("vars","or_mark_dis","cl_mark_dis","ch_mark_dis","pvalue_mark_dis")

colnames(res3_base_norm) = c("vars","beta_age","cl_age","ch_age","pvalue_age")
colnames(res3_fil_norm) = c("vars","beta_age","cl_age","ch_age","pvalue_age")

colnames(res2_base_norm) = c("vars","p_iter_test","or2_mark_dis","cl2_mark_dis","ch2_mark_dis","pvalue2_mark_dis","or2_iter_dis","cl2_iter_dis","ch2_iter_dis","pvalue2_iter_dis")
colnames(res2_fil_norm) = c("vars","p_iter_test","or2_mark_dis","cl2_mark_dis","ch2_mark_dis","pvalue2_mark_dis","or2_iter_dis","cl2_iter_dis","ch2_iter_dis","pvalue2_iter_dis")

res_base = merge.data.frame(res3_base_norm,res1_base_norm,by="vars")
res_base = merge.data.frame(res_base,res2_base_norm,by="vars")

res_fil = merge.data.frame(res3_fil_norm,res1_fil_norm,by="vars")
res_fil = merge.data.frame(res_fil,res2_fil_norm,by="vars")

data = list(res_base = res_base,res_fil = res_fil)
#saveRDS(data,"./result/logistic.rds")
