library(tibble)

args <- commandArgs(trailingOnly = TRUE)
#args = c("data_raw.rds","out1.rds")

file_input = args[1]
file_outpu = args[2]
a=0
################
remove_missing <- function(data, perc=0.2) {
  #ST1
  fil = data$painel=="ST1"
  fil[is.na(fil)] = FALSE
  immun = colnames(data$df)[fil]
  aux = data$df[!is.na(data$df$`ST1: batch`),]
  tot = round(nrow(aux)*perc)
  for(i in 1:length(immun)){
    dcol = aux[,immun[i]]
    n = length(dcol[is.na(dcol)])
    if(n>tot){
      data$painel = data$painel[colnames(data$df)!= immun[i]]
      data$df = data$df[,colnames(data$df)!= immun[i]]
      data$immun = data$immun[data$immun!=immun[i]]
    }
  }
  #ST2
  fil = data$painel=="ST2"
  fil[is.na(fil)] = FALSE
  immun = colnames(data$df)[fil]
  aux = data$df[!is.na(data$df$`ST2: batch`),]
  tot = round(nrow(aux)*perc)
  for(i in 1:length(immun)){
    dcol = aux[,immun[i]]
    n = length(dcol[is.na(dcol)])
    if(n>tot){
      data$painel = data$painel[colnames(data$df)!= immun[i]]
      data$df = data$df[,colnames(data$df)!= immun[i]]
      data$immun = data$immun[data$immun!=immun[i]]
    }
  }
  #ST3
  fil = data$painel=="ST3"
  fil[is.na(fil)] = FALSE
  immun = colnames(data$df)[fil]
  aux = data$df[!is.na(data$df$`ST3: batch`),]
  tot = round(nrow(aux)*perc)
  for(i in 1:length(immun)){
    dcol = aux[,immun[i]]
    n = length(dcol[is.na(dcol)])
    if(n>tot){
      data$painel = data$painel[colnames(data$df)!= immun[i]]
      data$df = data$df[,colnames(data$df)!= immun[i]]
      data$immun = data$immun[data$immun!=immun[i]]
    }
  }
  
  return(data)
}

#############################
data = readRDS(file = file_input)

data = remove_missing(data)

saveRDS(data,file_outpu)