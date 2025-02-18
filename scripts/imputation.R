library(tibble)
library(VIM)

args <- commandArgs(trailingOnly = TRUE)
#args = c("out2.rds","out3.rds")

file_input = args[1]
file_outpu = args[2]

data = readRDS(file = file_input)
df = data$df[,data$immun]
data$df[,data$immun] = kNN(df,k = 3,imp_var = FALSE)

saveRDS(data,file_outpu)