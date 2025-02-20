library(tibble)

args <- commandArgs(trailingOnly = TRUE)
#args = c("out1.rds","out4.rds")

file_input = args[1]
file_outpu = args[2]

data = readRDS(file = file_input)

data$df = data$df[data$df$disease==0,] 


saveRDS(data,file_outpu)