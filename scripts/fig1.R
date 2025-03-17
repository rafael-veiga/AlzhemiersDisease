library(tibble)
library(ggplot2)
library(dplyr)
library(cowplot)   
library(ggtext)
library(ggExtra)


args <- commandArgs(trailingOnly = TRUE)

#args = c("res_lr.csv","lr_n_auc.csv","auc_curv.csv","imp_data.rds")

file_res_lr = args[1]
file_n_auc = args[2]
file_auc_curv = args[3]
file_data = args[4]
#####################################################################################################################
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
#fig1a

data_log = read.csv(file_res_lr)
data_log = data_log[data_log$pvalue<=0.05,]

data_log <- data_log %>%
  arrange(mag)  # Reordena 'base_log' de forma crescente com base em 'mo'

desired_order <- data_log$vars  # Extraímos a ordem desejada das variáveis
data_log$out <- factor(data_log$vars, levels = desired_order)

print(levels(data_log$out))

# 3. Ajustar os limites e criar flags para valores fora dos limites
x_lower_limit <- 0.5
x_upper_limit <- 2.0  # Defina um limite superior específico se necessário

# Ajustar 'xmin' e 'xmax', e criar flags para valores fora dos limites
data_log <- data_log %>% mutate(
  xmin_adj = pmax(cl, x_lower_limit),
  xmax_adj = pmin(ch, x_upper_limit),
  cl_below = cl < x_lower_limit,
  ch_above = ch > x_upper_limit
)

line_size <- 0.4
cap_size <- 0.4

# 2. Função para criar os gráficos
create_plot <- function(data, x_var, xmin_var, xmax_var, cl_below_var, ch_above_var,
                        x_lower_limit, x_upper_limit, x_scale, x_breaks, x_label, color_segments) {
  f = ggplot(data, aes(y = out, x = .data[[x_var]])) +
    geom_point(shape = 15, size = 2, color = color_segments) +
    # Caso 2: cl abaixo do limite inferior
    geom_segment(
      data = data %>% filter(.data[[cl_below_var]]) %>% mutate(out = factor(out, levels = levels(data$out))),
      aes(x = .data[[xmin_var]], xend = .data[[xmax_var]], y = out, yend = out),
      size = line_size,
      color = color_segments,
      arrow = arrow(
        angle = 15, ends = "first", type = "closed",
        length = unit(0.3, "cm")
      )
    ) +
    # Caso 1: cl e ch dentro dos limites
    geom_errorbarh(
      data = data %>% filter(!.data[[cl_below_var]] & !.data[[ch_above_var]]) %>% mutate(out = factor(out, levels = levels(data$out))),
      aes(xmin = .data[[xmin_var]], xmax = .data[[xmax_var]], y = out),
      height = cap_size,
      size = line_size,
      color = color_segments
    ) +
    # Caso 3: ch acima do limite superior
    geom_segment(
      data = data %>% filter(.data[[ch_above_var]]) %>% mutate(out = factor(out, levels = levels(data$out))),
      aes(x = .data[[xmin_var]], xend = .data[[xmax_var]], y = out, yend = out),
      size = line_size,
      color = color_segments,
      arrow = arrow(
        angle = 15, ends = "last", type = "closed",
        length = unit(0.3, "cm")
      )
    ) +
    # Adicionar cap vertical no extremo superior para Caso 2
    geom_errorbarh(
      data = data %>% filter(.data[[cl_below_var]]) %>% mutate(out = factor(out, levels = levels(data$out))),
      aes(xmin = .data[[xmax_var]], xmax = .data[[xmax_var]], y = out),
      height = cap_size,
      size = line_size,
      color = color_segments
    ) +
    # Adicionar cap vertical no extremo inferior para Caso 3
    geom_errorbarh(
      data = data %>% filter(.data[[ch_above_var]]) %>% mutate(out = factor(out, levels = levels(data$out))),
      aes(xmin = .data[[xmin_var]], xmax = .data[[xmin_var]], y = out),
      height = cap_size,
      size = line_size,
      color = color_segments
    ) +
    # Adicionar estimativas pontuais
    geom_point(shape = 15, size = 2, color = color_segments) +
    # Linha vertical em x = referência (1 para OR, 0 para Beta)
    geom_vline(xintercept = ifelse(x_label == "OR", 1, 0), linetype = "dashed", linewidth = 1, color = "azure3") +
    # Ajustar escalas e rótulos
    scale_y_discrete(
      labels = new_labels(levels(data$out))
    ) +
    scale_x_continuous(
      trans = if (x_scale == "log") "log10" else "identity",
      breaks = x_breaks,
      limits = c(x_lower_limit, x_upper_limit)
    ) +
    labs(y = NULL, x = x_label) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      axis.text.y = ggtext::element_markdown(size = 10),
      plot.margin = unit(c(2, 2, 0, 2), "mm"),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    coord_cartesian(clip = "off")
  return(f)
}

# 3. Criar os subplots

# Primeiro subplot (Alzheimer's)
f1_a <- create_plot(
  data = data_log,
  x_var = "or",
  xmin_var = "xmin_adj",
  xmax_var = "xmax_adj",
  cl_below_var = "cl_below",
  ch_above_var = "ch_above",
  x_lower_limit = x_lower_limit,
  x_upper_limit = x_upper_limit,
  x_scale = "log",
  x_breaks = c(0.5, 0.7, 1.0, 1.2, 1.5, 1.7, 2.0),
  x_label = "OR",
  color_segments = "black"
)

################################################################################
# fig1_b

data = read.csv(file_n_auc,sep=";")
f1_b = data[1:30,] %>% 
  ggplot(aes(x=n,y=auc))+
  geom_point(size=1,color="gray")+
  geom_line()+
  #geom_ribbon(aes(ymax = auc_h,ymin = auc_l),fill = dis$color[d],alpha=0.4)+
  geom_smooth(method = "loess",span=0.3,fill="lightblue",color="lightgreen")+
  labs(x = "Markers number",y="AUC")+
  #scale_y_continuous(limits = c(0.7,NA)) +
  #scale_x_continuous(limits = c(1,NA)) +
  geom_vline(xintercept=distan,color = "darkred") +
  annotate("text",x=distan +1,y=0.65,label=paste0("n = ",distan),size=4,angle = 90,color = "darkred")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size=8),axis.text.y = element_text(size=8))


################################################################################
# fig1_c
aux = read.csv(file_auc_curv,sep=";")
label =paste0(sprintf(aux$auc[1], fmt = '%#.2f')," (",sprintf(aux$auc_l[1], fmt = '%#.2f')," : ",sprintf(aux$auc_h[1], fmt = '%#.2f'),")")
f1_c = aux %>%  ggplot(aes(x=fpr,y=tpr))+
  geom_line(color = "lightgreen",linewidth=1)+
  geom_ribbon(aes(ymax = tpr_h,ymin = tpr_l),fill = "lightblue",alpha=0.4)+
  geom_richtext(color="black",fill="gray95",x=0.7,y=0.1,label=label,size=4,fontface ="plain")+
  geom_abline(slope = 1,intercept = 0,linetype=2,linewidth=1,color="gray")+
  scale_x_continuous(limits = c(0, 1),expand = c(0,0))+
  scale_y_continuous(limits = c(0, 1),expand = c(0,0))+
  ylab("True positive rate")+
  xlab("False positive rate")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size=8),axis.text.y = element_text(size=8))

################################################################################
# fig1_d
data = readRDS(file_data)
x = data$df[,data$immun]
y = data$df[,"disease"]
y = as.factor(data$df$disease)
levels(y) = names(color_eu)
df_pca =  prcomp(x, scale. = TRUE)
df_pca <- data.frame(PC1 = df_pca$x[, 1], PC2 = df_pca$x[, 2], Disease = as.character(y))
p <- df_pca %>% ggplot(aes(x=PC1, y=PC2)) +
  geom_point(aes(color=Disease), size=2) +
  scale_color_manual(
    values=color_eu
  ) +
  geom_hline(yintercept=0, linetype=2, linewidth=1, color="darkgray") +
  geom_vline(xintercept=0, linetype=2, linewidth=1, color="darkgray") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none", # Posição relativa da legenda
    legend.background = element_rect(fill = "white", color = "black"), # Fundo branco com borda preta
    legend.box.margin = margin(0, 0, 0, 0), # Margem interna da caixa
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

f1_d <- ggExtra::ggMarginal(
  p,
  groupColour = TRUE,
  groupFill = TRUE
)
################################################################################
# fig1_e
data = readRDS(file_data)
res = read.csv(file_res_lr)
x = data$df[,res$vars[1:4]]
y = data$df[,"disease"]
y = as.factor(data$df$disease)
levels(y) = names(color_eu)
pc = prcomp(x,scale. = TRUE)
data = as.data.frame(pc$x[,c(1,2)])
data$Disease = y
aux =  as.data.frame(pc$rotation[,c(1,2)])[res$vars[1:4],]
#row.names(aux) = NULL
p <- data %>% ggplot(aes(x=PC1, y=PC2))+
  geom_point(aes(color=Disease),size=2)+
  scale_color_manual(values=color_eu)+
  geom_hline(yintercept=0, linetype=2,linewidth=1,color="darkgray")+geom_vline(xintercept=0, linetype=2,linewidth=1,color="darkgray")+
  theme_bw()+
  theme(legend.position = "none",axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size=8),axis.text.y = element_text(size=8))
fig1_pca <- p + 
  geom_richtext(data=aux, aes(x=PC1*2, y=PC2*2, label=new_labels(row.names(aux))), size = 2.5, color="black",fill=fill_alpha("bisque2",0.7))+
  geom_segment(data=aux,aes(x=0, y=0, xend=PC1*2, yend=PC2*2), arrow=arrow(length=unit(0.3,"cm")), color="black",linewidth=1)+
  theme_bw()+
  theme(legend.position = "none",axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size=8),axis.text.y = element_text(size=8),plot.caption = element_markdown(size = 10))

f1_e=ggExtra::ggMarginal(fig1_pca,groupColour = TRUE, groupFill = TRUE)

################################################################################
# painel fig1

painel_inferior <- plot_grid(f1_b, f1_c, f1_d, f1_e,labels = c("b", "c", "d", "e"),ncol = 2)
painel_final <- plot_grid(f1_a, painel_inferior,labels = c("a", ""),ncol = 1)

pdf("fig1.pdf", width = 10, height = 13)
print(painel_final)
dev.off()


