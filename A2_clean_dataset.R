library(tidyverse)
library(forecast)
library(VIM)
################################################################################
# functions

get_fil <- function(col,type="all",pop="all",mark="all"){
  fil_t = rep(FALSE,length(col))
  for(t in type){
    if(t=="all"){
      fil_t=rep(TRUE,length(col))
      break
    }
    if(t=="other"){
      fil_t = (fil_t) | (col=="Sample") | (col=="batch") | (col=="id") | (col=="type") | (col=="age") | (col=="sex") | (col=="disease") | (col=="fil_dis")
    }
    if(t=="pop_in_pop"){
      fil_t = (fil_t) | (grepl(" in ",col) & (!grepl(" median ",col)) )
    }
    if(t=="mark_in_pop"){
      fil_t = (fil_t) | grepl(" median in ",col)
    }
  }
  fil_p = rep(FALSE,length(col))
  for(p in pop){
    if(p=="all"){
      fil_p=rep(TRUE,length(col))
      break
    }
    f = as.logical(sapply(col,function(s){
      if(grepl(" in ",s)){
        return (unlist(str_split(s," in "))[2])
      }
      return(s)
    })==p)
    ini = as.logical(sapply(col,function(s){
      if(grepl(" in ",s) & (!grepl(" median ",s))){
        return (unlist(str_split(s," in "))[1])
      }
      return(s)
    })==p)
    f_ini = f | ini
    fil_p = fil_p | f_ini
  }
  fil_m = rep(FALSE,length(col))
  for(m in mark){
    if(m=="all"){
      fil_m=rep(TRUE,length(col))
      break
    }
    f = as.logical(sapply(col,function(s){return (unlist(str_split(s," median in "))[1])})==m)
    fil_m = fil_m | f
  }
  return(fil_m & fil_p & fil_t)
}

get_pop <- function(col){
  pop_secific = rep(NA,length(col))
  pop_reference = rep(NA,length(col))
  for(s in 1:length(col)){
    aux = strsplit(col[s]," in ")[[1]]
    pop_secific[s] = aux[1]
    pop_reference[s] = aux[2]
  }
  return(data.frame(pop=col,pop_secific=pop_secific,pop_reference=pop_reference))
}

remove_missing <- function(data,perc=0.2){
  percent_missing <- sapply(data$df[data$fil_sam,data$fil_immuno], function(x) sum(is.na(x)) / length(x))
  fil_immuno = data$fil_immuno
  for(i in 1:length(percent_missing)){
    if(percent_missing[i]>=perc){
      fil_immuno[data$fil_immuno][i]=FALSE
    }
    
  }
  data$fil_immuno=fil_immuno
  return(data)
}


box_cox_trans <- function(data){
  df1 = data$df[data$fil_sam,data$fil_immuno]
  df_min = rep(NA,length(colnames(df1)))
  lampda = rep(NA,length(colnames(df1)))
  std_mean =  rep(NA,length(colnames(df1)))
  std_sd =  rep(NA,length(colnames(df1)))
  for(i in 1:length(colnames(df1))){
    print(i)
    df_min[i] = min(df1[,i],na.rm = TRUE)-1
    df1[,i] = df1[,i] - df_min[i]
    lampda[i] = BoxCox.lambda(df1[,i][!is.na(df1[,i])], method = c("loglik"), lower = -5, upper = 5)
    if(lampda[i]==0){
      df1[,i] = log(df1[,i])
    }else{
      df1[,i] = (sign(df1[,i])*((abs(df1[,i]))^lampda[i])-1)/lampda[i]
    }
    #standat
    std_mean[i] = mean(df1[,i],na.rm = TRUE)[[1]]
    std_sd[i] = sd(unlist(df1[!is.na(df1[,i]),i]))
    df1[,i] = (df1[,i]-std_mean[i])/std_sd[i]
  }
  data$df[data$fil_sam,data$fil_immuno] = df1
  trans = data.frame(lampda=lampda,std_mean=std_mean,std_sd = std_sd,df_min = df_min)
  return(list(data=data,trans=trans))
}

imputation <- function(data,k=3){
  df1 = data$df[data$fil_sam,data$fil_immuno]
  df1 = kNN(df1,k = k,imp_var = FALSE,useImputedDist=TRUE)
  data$df[data$fil_sam,data$fil_immuno] = df1
  return(data)
}

################################################################################
# no filter data save
df = readRDS(file = "./pos_data/data_raw.rds")





fil_immuno = get_fil(colnames(df),type = c("pop_in_pop"))

res_pop = get_pop(colnames(df)[fil_immuno])

write.csv(res_pop, "./result/populations_not_filter.csv")

populations = sort(unique(c(res_pop$pop_secific,res_pop$pop_reference)))

write.csv(populations, "./result/unique_populations_not_filter.csv")
fil_sam = rep(TRUE,nrow(df))
aux = list(df = df,fil_sam=fil_sam,fil_immuno=fil_immuno)
data = list(base = aux)

# filtered
fil_sam = complete.cases(df$`ST1: batch`, df$`ST2: batch`, df$`ST3: batch`)
aux = data$base
aux$fil_sam = fil_sam
aux = remove_missing(data=aux,perc=0.2)

# boxcox transformation
res = box_cox_trans(data$base)
data$base_norm = res$data
data$fil_norm = aux
data$fil_norm$df = data$base_norm$df

#imputation
aux = data$fil_norm
aux = imputation(aux,k=5)

data$fil_imp = aux
saveRDS(data,"./pos_data/dataset.rds")