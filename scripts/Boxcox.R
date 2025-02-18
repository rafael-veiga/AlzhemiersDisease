library(tibble)
library(forecast)
args <- commandArgs(trailingOnly = TRUE)
#args = c("data_raw.rds","out1.rds")

file_input = args[1]
file_outpu = args[2]


################
box_cox_trans <- function(df1){
  df_min = rep(NA,length(colnames(df1)))
  lampda = rep(NA,length(colnames(df1)))
  for(i in 1:length(colnames(df1))){
    print(i)
    vari = as.numeric(df1[[i]])
    df_min[i] = min(vari,na.rm = TRUE)-1
    df1[,i] = vari - df_min[i]
    lampda[i] = BoxCox.lambda(df1[,i][!is.na(df1[,i])], method = c("loglik"), lower = -5, upper = 5)
    if(lampda[i]==0){
      df1[,i] = log(df1[,i])
    }else{
      if(!is.na(lampda[i])){
        df1[,i] = (sign(df1[,i])*((abs(df1[,i]))^lampda[i])-1)/lampda[i]
      }
    }
  }
  trans = tibble(lampda=lampda,df_min = df_min)
  return(list(df=df1,trans=trans))
}

#############################
data = readRDS(file = file_input)

df = data$df

df = df[,data$immun]

res = box_cox_trans(df)

data$df[,data$immun] = res$df[,data$immun]

saveRDS(data,file_outpu)