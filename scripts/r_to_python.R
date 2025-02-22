library(tibble)

args <- commandArgs(trailingOnly = TRUE)
#args = c("imp_data.rds","data_py")

file_input = args[1]
file_outpu = args[2]

data = readRDS(file = file_input)
write.table(data$df,paste0(file_outpu,"_df.csv"),sep = ";",row.names = FALSE,quote = FALSE)
write.table(data$immun,paste0(file_outpu,"_immun.csv"),sep = "\n",row.names = FALSE,col.names = FALSE,quote = FALSE)