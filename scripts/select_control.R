library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
#args = c("data_raw.rds","out.rds")

file_input = args[1]
file_outpu = args[2]

data = readRDS(file = file_input)

data$df = data$df[data$df$disease==0,]
saveRDS(data,file_outpu)