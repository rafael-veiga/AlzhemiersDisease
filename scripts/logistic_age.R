library(tibble)
library(foreach)
library(doParallel)

args <- commandArgs(trailingOnly = TRUE)
args = c("data.rds","res_lr_age.csv", 15)

file_input = args[1]
file_outpu = args[2]
n_jobs = as.numeric(args[3])

################
rename_vars <- function(data){
  aux = data
  imuno = data$immun
  tam =length(imuno) 
  vari = rep("a",tam)
  vari = paste(vari,1:tam,sep = "")
  col = colnames(aux$df)
  col[col %in% imuno] = vari
  colnames(aux$df) = col
  return(aux)
}

regress <- function(data, n_jobs = 15){
  df = data$df
  df$age_std <- as.numeric(scale(df$age, center = TRUE, scale = TRUE))
  imuno <- data$immun
  tam <- length(imuno)
  cl <- makeCluster(n_jobs)
  registerDoParallel(cl)
  result <- foreach(i = 1:tam, .combine = 'rbind', .packages = 'stats') %dopar% {
    suppressWarnings(
      tryCatch({
        formula_lm <- as.formula(paste("a", i, " ~ age_std + gender", sep = ""))
        model_lm <- lm(formula_lm, data = df)
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
        data.frame(beta = NA, cl = NA, ch = NA, pvalue = NA)
      })
    )
  }
  stopCluster(cl)
  df_result <- data.frame(vars = imuno, result)
  return(df_result)
}

ordenar_marks <- function(df){
  df = df[order(df$pvalue, decreasing = FALSE),]
  return(df)
}

#############################
data = readRDS(file = file_input)

data = rename_vars(data)

res = regress(data,n_jobs)

res = ordenar_marks(res)
write.table(res,file_outpu,row.names = FALSE,sep=",",quote = FALSE)