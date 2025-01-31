library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
#args = c("out.rds","out2.rds")

file_input = args[1]
file_outpu = args[2]

################
f_standart <- function(df1){
  med = rep(NA,length(colnames(df1)))
  std = rep(NA,length(colnames(df1)))
  for(i in 1:length(colnames(df1))){
    print(i)
    med[i] = colMeans(df1[,i],na.rm=TRUE)
    std[i] = sd(as.numeric(df1[[i]]),na.rm=TRUE)
    df1[,i] = (df1[,i] - med[i])/std[i]
    
  }
  trans = tibble(med=med,std = std)
  return(list(df=df1,trans=trans))
}

#############################
data = readRDS(file = file_input)

df = data$df

df = df[,data$immun]

res = f_standart(df)

data$df[,data$immun] = res$df[,data$immun]

saveRDS(data,file_outpu)