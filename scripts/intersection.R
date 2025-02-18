library(tibble)

args <- commandArgs(trailingOnly = TRUE)
#args = c("out1.rds","out2.rds")

file_input = args[1]
file_outpu = args[2]

data = readRDS(file = file_input)

data$df = data$df[!is.na(data$df$`ST1: batch`),]
data$df = data$df[!is.na(data$df$`ST2: batch`),]
data$df = data$df[!is.na(data$df$`ST3: batch`),]

saveRDS(data,file_outpu)