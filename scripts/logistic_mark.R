library(tibble)
library(foreach)
library(doParallel)

args <- commandArgs(trailingOnly = TRUE)
#args = c("data.rds","res_lr.csv", 15)

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
  imuno <- data$immun
  tam <- length(imuno)
  cl <- makeCluster(n_jobs)
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

ordenar_marks <- function(df){
  df$mag = df$or
  df$mag[df$or<1] = 1/df$or[df$or<1]
  df1 = df[df$pvalue<=0.05,]
  df2 = df[!(df$pvalue<=0.05),]
  df1=df1[order(df1$mag, decreasing = TRUE), ]
  df2=df2[order(df2$pvalue, decreasing = FALSE), ]
  
  df = rbind(df1,df2)
  return(df)
}

#############################
data = readRDS(file = file_input)

data = rename_vars(data)

res = regress(data,n_jobs)

res = ordenar_marks(res)
write.table(res,file_outpu,row.names = FALSE,sep=",",quote = FALSE)