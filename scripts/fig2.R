library(tibble)
library(ggplot2)
library(tidyr)

args <- commandArgs(trailingOnly = TRUE)

#args = c("res_lr.csv","data_raw_f.rds")

file_res_lr = args[1]
file_data = args[2]

#####################################################################################################################
font ="serif"
color_eu = c("Healthy" ="#66CD00",
             "Alzheimer"="#FF4500")

new_labels = function(vet){
  aux = sapply(as.character(vet), function(x) {
    x = gsub("\\+", "<sup>+</sup>", x)
    x = gsub("\\-", "<sup>-</sup>", x)
    x = gsub("Non<sup>-</sup>Switched", "Non-Switched", x)
    x = gsub("low", "<sup>low</sup>", x)
    x = gsub("high", "<sup>high</sup>", x)
    x = gsub("dim", "<sup>dim</sup>", x)
    x = gsub("bright", "<sup>bright</sup>", x)
    return(x)
  })
  return(aux)
}

distan = 4
################################################################################
data = readRDS(file_data)
df = data$df
res = read.csv(file_res_lr)
res = res$vars[1:15]
aux = df[,c("disease",res)]

aux$disease = as.factor(aux$disease)
aux = gather(data = aux, key = marks,value = value,-disease)
aux = aux[!is.na(aux$value),]
levels(aux$disease) = names(color_eu)
aux$marks = factor(aux$marks,levels = res)
levels(aux$marks) = new_labels(levels(aux$marks))

##violin

fig2=aux %>% ggplot(aes(x=disease,y=value,fill = disease))+
  facet_wrap( ~ marks,scales = "free_y",ncol = 3 )+
  geom_violin(scale = "width",trim = FALSE,draw_quantiles = c(0.5),na.rm = TRUE)+
  geom_jitter(width = 0.2,height = 0,size=1,na.rm = TRUE)+
  scale_x_discrete(limits = names(color_eu))+
  scale_y_continuous(limits = c(0,NA))+
  scale_fill_manual(values=c(color_eu[[1]],color_eu[[2]]),limits = names(color_eu))+
  labs(x=element_blank(),y = "Frequency")+
  theme_bw(base_family =font)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=8,family = font),
        axis.title.y = element_text(size=10,family = font),
        legend.title=element_blank(),
        legend.text = element_text(size=11,family = font),
        strip.text = ggtext::element_markdown(size = 10,family =font),
        strip.background = element_rect(fill = "bisque"),
        legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal',
        plot.margin = unit(c(0,2,2,2),"mm"))
pdf("fig2.pdf", family = font, width = 14, height = 10)
print(fig2)
dev.off()