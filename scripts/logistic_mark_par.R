library(tibble)
library(foreach)
library(doParallel)

args <- commandArgs(trailingOnly = TRUE)
#args = c("data.rds","res_lr_par.csv", 15)

file_input = args[1]
file_outpu = args[2]
n_jobs = as.numeric(args[3])

cl <- makeCluster(n_jobs)
registerDoParallel(cl)

################
data <- readRDS(file = file_input)

id <- unique(data$df$Sample)
ref_df <- tibble(Sample = id)
imuno <- data$immun

df_pos <- data$df[data$df$disease == 1, ]
df_pos <- merge(ref_df, df_pos, by = "Sample", all.x = TRUE)

df_neg <- data$df[data$df$disease == 0, ]
df_neg <- merge(ref_df, df_neg, by = "Sample", all.x = TRUE)

# Paralelizando o loop
results <- foreach(i = 1:length(imuno), 
                   .combine = 'rbind', 
                   .packages = "stats") %dopar% {
                     
                     # Extraindo as colunas correspondentes a imuno[i]
                     pos <- df_pos[, imuno[i]]
                     neg <- df_neg[, imuno[i]]
                     
                     # Removendo os valores NA
                     fil <- !is.na(pos)
                     pos <- pos[fil]
                     neg <- neg[fil]
                     
                     fil <- !is.na(neg)
                     pos <- pos[fil]
                     neg <- neg[fil]
                     
                     # Estimador de Hodges-Lehmann
                     res <- wilcox.test(pos, neg, paired = TRUE, conf.int = TRUE)
                     
                     # Retorna os resultados como data.frame
                     data.frame(var     = imuno[i],
                                differ  = res$estimate,
                                diff_l  = res$conf.int[1],
                                diff_h  = res$conf.int[2],
                                pvalue  = res$p.value)
                   }

# Finaliza o cluster paralelo
stopCluster(cl)
results=results[order(results$pvalue), ]
write.table(results,file_outpu,row.names = FALSE,sep=",",quote = FALSE)