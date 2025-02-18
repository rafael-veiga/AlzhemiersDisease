library(tibble)

args <- commandArgs(trailingOnly = TRUE)
#args = c("data_raw.rds","out1.rds")

file_input = args[1]
file_outpu = args[2]
a=0
################
remove_missing <- function(data, perc=0.2) {
  base= data$df
  base = base[!is.na(base$`ST1: batch`),]
  base = base[!is.na(base$`ST2: batch`),]
  base = base[!is.na(base$`ST3: batch`),]
  immun = data$immun
  aux = data$df
  tot = round(nrow(base)*perc)
  for(i in 1:length(immun)){
    dcol = base[,immun[i]]
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