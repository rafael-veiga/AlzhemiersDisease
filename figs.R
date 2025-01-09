library(tidyverse)
library(ggplot2)
library(dplyr)
library(cowplot)   # Para plot_grid
library(ggtext)    # Para element_markdown
library(scales)
library(ggrepel)



#####################################################################################################################
color_eu = c("Healthy" ="#66CD00",
             "Alzheimer"="#FF4500")
#####################################################
dis = list(var = c("Healthy","Alzheimer"),
           color = color_eu,
           ref = c(9,7,8))
distan=4
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
get_peach_color <- function(value, min_val, max_val) {
  num_colors <- 9
  colors <- brewer.pal(num_colors, "YlOrRd")
  normalized_value <- (value - min_val) / (max_val - min_val)
  color_index <- round(normalized_value * (num_colors - 1)) + 1
  return(colors[color_index])
}

regression <- function(df,var,out){
  aux = df[,c(var,"gender",out)]
  colnames(aux) = c("mark","sex","out")
  model <- glm(out~mark+sex, data = aux, family = binomial())
  or <- exp(coef(model))["mark"]
  ci <- exp(confint(model, parm = "mark"))
  ci_lower <- ci[1]
  ci_upper <- ci[2]
  return(list(or=or[[1]],cl=ci_lower[[1]],ch=ci_upper[[1]]))
}

font ="serif"
################################################################################
#table1
data = readRDS("./pos_data/dataset.rds")
df = data$base$df
df = df[,c("age","gender","disease")]
df$age_dis = NA
df$age_dis[df$age<60] = 0 #<60
df$age_dis[df$age>=60 & df$age<75] = 1
df$age_dis[df$age>=75] = 2 
df$age_dis = as.factor(df$age_dis)
df$age1 = NA
df$age2 = NA
df$age1[df$age_dis==0] = 0
df$age1[df$age_dis==1] = 1
df$age2[df$age_dis==0] = 0
df$age2[df$age_dis==2] = 1

model = glm(disease~gender,data = df,family = binomial())
exp(coef(model))
exp(confint(model))

model = glm(disease~age1,data = df,family = binomial())
exp(coef(model))
exp(confint(model))

model = glm(disease~age2,data = df,family = binomial())
exp(coef(model))
exp(confint(model))
################################################################################

################################################################################
#fig 1
data_log = readRDS("./result/logistic.rds")
aux = data_log$res_base
base_log = aux[aux$pvalue_mark_dis<0.05,]
base_log$mo = NA
base_log$mo[base_log$or_mark_dis>=1] = base_log$or_mark_dis[base_log$or_mark_dis>=1]
base_log$mo[base_log$or_mark_dis<1] = 1/base_log$or_mark_dis[base_log$or_mark_dis<1]

base_log <- base_log %>%
  arrange(desc(mo))  # Ordena 'base_log' de forma decrescente com base em 'mo'

write_csv(base_log,file = "./result/base_log_best.csv")

base_log <- base_log[1:30,]  # Seleciona as primeiras 30 linhas

base_log <- base_log %>%
  arrange(mo)  # Reordena 'base_log' de forma crescente com base em 'mo'

# 2. Definir 'out' como um fator com níveis na ordem desejada
# Aqui, assumo que a ordem desejada é a ordem atual de 'base_log$vars' após as ordenações
desired_order <- base_log$vars  # Extraímos a ordem desejada das variáveis

# Definir 'out' como fator com os níveis na ordem desejada
base_log$out <- factor(base_log$vars, levels = desired_order)

# Verifique se 'out' possui os níveis corretos
print(levels(base_log$out))

# 3. Ajustar os limites e criar flags para valores fora dos limites
x_lower_limit <- 0.1
x_upper_limit <- 50  # Defina um limite superior específico se necessário

# Ajustar 'xmin' e 'xmax', e criar flags para valores fora dos limites
base_log <- base_log %>% mutate(
  xmin_adj = pmax(cl_mark_dis, x_lower_limit),
  xmax_adj = pmin(ch_mark_dis, x_upper_limit),
  cl_below = cl_mark_dis < x_lower_limit,
  ch_above = ch_mark_dis > x_upper_limit
)

line_size <- 0.4
cap_size <- 0.4

x_lower_limit_or <- 0.1
x_upper_limit_or <- 50

base_log <- base_log %>% mutate(
  xmin_adj_or = pmax(cl_mark_dis, x_lower_limit_or),
  xmax_adj_or = pmin(ch_mark_dis, x_upper_limit_or),
  cl_below_or = cl_mark_dis < x_lower_limit_or,
  ch_above_or = ch_mark_dis > x_upper_limit_or
)

# Para o segundo subplot (Age)
x_lower_limit_beta <- min(base_log$cl_age, na.rm = TRUE)
x_upper_limit_beta <- max(base_log$ch_age, na.rm = TRUE)

base_log <- base_log %>% mutate(
  xmin_adj_beta = pmax(cl_age, x_lower_limit_beta),
  xmax_adj_beta = pmin(ch_age, x_upper_limit_beta),
  cl_below_beta = cl_age < x_lower_limit_beta,
  ch_above_beta = ch_age > x_upper_limit_beta
)

line_size <- 0.4
cap_size <- 0.4

# 2. Função para criar os gráficos
create_plot <- function(data, x_var, xmin_var, xmax_var, cl_below_var, ch_above_var,
                        x_lower_limit, x_upper_limit, x_scale, x_breaks, x_label, title, color_segments) {
  ggplot(data, aes(y = out, x = .data[[x_var]])) +
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
    labs(y = NULL, x = x_label, title = title) +
    theme_classic(base_family = font) +
    theme(
      axis.title.x = element_text(size = 10, family = font),
      axis.text.x = element_text(size = 8, family = font),
      axis.text.y = ggtext::element_markdown(size = 10, family = font),
      plot.margin = unit(c(2, 2, 0, 2), "mm"),
      plot.title = element_text(hjust = 0.5, size = 12, family = font)
    ) +
    coord_cartesian(clip = "off")
}

# 3. Criar os subplots

# Primeiro subplot (Alzheimer's)
f_or <- create_plot(
  data = base_log,
  x_var = "or_mark_dis",
  xmin_var = "xmin_adj_or",
  xmax_var = "xmax_adj_or",
  cl_below_var = "cl_below_or",
  ch_above_var = "ch_above_or",
  x_lower_limit = x_lower_limit_or,
  x_upper_limit = x_upper_limit_or,
  x_scale = "log",
  x_breaks = c(0.1, 0.2, 0.5, 1, 1.5, 5, 10, 15, 30, 50),
  x_label = "OR",
  title = "Alzheimer's",
  color_segments = "black"
)

# Segundo subplot (Age)
f_beta <- create_plot(
  data = base_log,
  x_var = "beta_age",
  xmin_var = "xmin_adj_beta",
  xmax_var = "xmax_adj_beta",
  cl_below_var = "cl_below_beta",
  ch_above_var = "ch_above_beta",
  x_lower_limit = x_lower_limit_beta,
  x_upper_limit = x_upper_limit_beta,
  x_scale = "linear",
  x_breaks = pretty_breaks(n = 5)(c(x_lower_limit_beta, x_upper_limit_beta)),
  x_label = "Beta",
  title = "Age",
  color_segments = "orange"
)

# Remover os textos do eixo Y do segundo subplot para compartilhar com o primeiro
f_beta <- f_beta +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

# 4. Combinar os subplots em uma única figura
combined_plot <- plot_grid(
  f_or,
  f_beta,
  nrow = 1,
  rel_widths = c(2.5, 1),
  align = "h"
)

# 5. Salvar o gráfico combinado em PDF
pdf("base_log_or.pdf", family = font, width = 12, height = 7)
print(combined_plot)
dev.off()

# Exibir o gráfico combinado
print(combined_plot)
################################################################################
#fig 2
data_log = readRDS("./result/logistic.rds")
aux = data_log$res_fil
base_log = aux[aux$pvalue_mark_dis<0.05 & aux$p_iter_test>=0.05,]

base_log$mo = NA
base_log$mo[base_log$or_mark_dis>=1] = base_log$or_mark_dis[base_log$or_mark_dis>=1]
base_log$mo[base_log$or_mark_dis<1] = 1/base_log$or_mark_dis[base_log$or_mark_dis<1]

base_log <- base_log %>%
  arrange(desc(mo))  # Ordena 'base_log' de forma decrescente com base em 'mo'

write_csv(base_log,file = "./result/fil_log_best.csv")

base_log <- base_log[1:30,]  # Seleciona as primeiras 30 linhas

base_log <- base_log %>%
  arrange(mo)  # Reordena 'base_log' de forma crescente com base em 'mo'

# 2. Definir 'out' como um fator com níveis na ordem desejada
# Aqui, assumo que a ordem desejada é a ordem atual de 'base_log$vars' após as ordenações
desired_order <- base_log$vars  # Extraímos a ordem desejada das variáveis

# Definir 'out' como fator com os níveis na ordem desejada
base_log$out <- factor(base_log$vars, levels = desired_order)

# Verifique se 'out' possui os níveis corretos
print(levels(base_log$out))

# 3. Ajustar os limites e criar flags para valores fora dos limites
x_lower_limit <- 0.5
x_upper_limit <- 2  # Defina um limite superior específico se necessário

# Ajustar 'xmin' e 'xmax', e criar flags para valores fora dos limites
base_log <- base_log %>% mutate(
  xmin_adj = pmax(cl_mark_dis, x_lower_limit),
  xmax_adj = pmin(ch_mark_dis, x_upper_limit),
  cl_below = cl_mark_dis < x_lower_limit,
  ch_above = ch_mark_dis > x_upper_limit
)

line_size <- 0.4
cap_size <- 0.4

x_lower_limit_or <- 0.5
x_upper_limit_or <- 2

base_log <- base_log %>% mutate(
  xmin_adj_or = pmax(cl_mark_dis, x_lower_limit_or),
  xmax_adj_or = pmin(ch_mark_dis, x_upper_limit_or),
  cl_below_or = cl_mark_dis < x_lower_limit_or,
  ch_above_or = ch_mark_dis > x_upper_limit_or
)

# Para o segundo subplot (Age)
x_lower_limit_beta <- min(base_log$cl_age, na.rm = TRUE)
x_upper_limit_beta <- max(base_log$ch_age, na.rm = TRUE)

base_log <- base_log %>% mutate(
  xmin_adj_beta = pmax(cl_age, x_lower_limit_beta),
  xmax_adj_beta = pmin(ch_age, x_upper_limit_beta),
  cl_below_beta = cl_age < x_lower_limit_beta,
  ch_above_beta = ch_age > x_upper_limit_beta
)

line_size <- 0.4
cap_size <- 0.4

# 2. Função para criar os gráficos
create_plot <- function(data, x_var, xmin_var, xmax_var, cl_below_var, ch_above_var,
                        x_lower_limit, x_upper_limit, x_scale, x_breaks, x_label, title, color_segments) {
  ggplot(data, aes(y = out, x = .data[[x_var]])) +
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
    labs(y = NULL, x = x_label, title = title) +
    theme_classic(base_family = font) +
    theme(
      axis.title.x = element_text(size = 10, family = font),
      axis.text.x = element_text(size = 8, family = font),
      axis.text.y = ggtext::element_markdown(size = 10, family = font),
      plot.margin = unit(c(2, 2, 0, 2), "mm"),
      plot.title = element_text(hjust = 0.5, size = 12, family = font)
    ) +
    coord_cartesian(clip = "off")
}

# 3. Criar os subplots

# Primeiro subplot (Alzheimer's)
f_or <- create_plot(
  data = base_log,
  x_var = "or_mark_dis",
  xmin_var = "xmin_adj_or",
  xmax_var = "xmax_adj_or",
  cl_below_var = "cl_below_or",
  ch_above_var = "ch_above_or",
  x_lower_limit = x_lower_limit_or,
  x_upper_limit = x_upper_limit_or,
  x_scale = "log",
  x_breaks = c(0.5, 0.7,1,1.2, 1.5, 1.7,2),
  x_label = "OR",
  title = "Alzheimer's",
  color_segments = "black"
)

# Segundo subplot (Age)
f_beta <- create_plot(
  data = base_log,
  x_var = "beta_age",
  xmin_var = "xmin_adj_beta",
  xmax_var = "xmax_adj_beta",
  cl_below_var = "cl_below_beta",
  ch_above_var = "ch_above_beta",
  x_lower_limit = x_lower_limit_beta,
  x_upper_limit = x_upper_limit_beta,
  x_scale = "linear",
  x_breaks = pretty_breaks(n = 5)(c(x_lower_limit_beta, x_upper_limit_beta)),
  x_label = "Beta",
  title = "Age",
  color_segments = "orange"
)

# Remover os textos do eixo Y do segundo subplot para compartilhar com o primeiro
f_beta <- f_beta +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

# 4. Combinar os subplots em uma única figura
combined_plot <- plot_grid(
  f_or,
  f_beta,
  nrow = 1,
  rel_widths = c(2.5, 1),
  align = "h"
)

# 5. Salvar o gráfico combinado em PDF
pdf("fil_log_or.pdf", family = font, width = 12, height = 7)
print(combined_plot)
dev.off()

# Exibir o gráfico combinado
print(combined_plot)

################################################################################
#fig 3
data_log = readRDS("./result/logistic.rds")
aux = data_log$res_fil
base_log = aux[aux$p_iter_test<0.05,]


data = readRDS("./pos_data/dataset.rds")

df = data$fil_norm$df
df$age_dis = NA
df$age_dis[df$age<60] = 0 #<60
df$age_dis[df$age>=60 & df$age<75] = 1
df$age_dis[df$age>=75] = 2 
df$age_dis = as.factor(df$age_dis)

inter_var = base_log$vars
or1 = rep(NA, length(inter_var))
cl1 = rep(NA, length(inter_var))
ch1 = rep(NA, length(inter_var))

or2 = rep(NA, length(inter_var))
cl2 = rep(NA, length(inter_var))
ch2 = rep(NA, length(inter_var))

or3 = rep(NA, length(inter_var))
cl3 = rep(NA, length(inter_var))
ch3 = rep(NA, length(inter_var))


for(i in 1:length(inter_var)){
  print(i)
  aux = df[df$age_dis==0,]
  res = regression(df = aux,var = inter_var[i],out = "disease")
  or1[i] = res$or
  cl1[i] = res$cl
  ch1[i] = res$ch
  
  aux = df[df$age_dis==1,]
  res = regression(df = aux,var = inter_var[i],out = "disease")
  or2[i] = res$or
  cl2[i] = res$cl
  ch2[i] = res$ch
  
  aux = df[df$age_dis==2,]
  res = regression(df = aux,var = inter_var[i],out = "disease")
  or3[i] = res$or
  cl3[i] = res$cl
  ch3[i] = res$ch
}

aux = tibble(vars=inter_var,or1=or1,cl1=cl1,ch1=ch1,or2=or2,cl2=cl2,ch2=ch2,or3=or3,cl3=cl3,ch3=ch3)

aux$mo = NA
aux$mo[aux$or1>=1] = aux$or1[aux$or1>=1]
aux$mo[aux$or1<1] = 1/aux$or1[aux$or1<1]

base_log <- aux %>%
  arrange(desc(mo))  # Ordena 'base_log' de forma decrescente com base em 'mo'

write_csv(base_log,file = "./result/iter_best.csv")


base_log <- base_log %>%
  arrange(mo)  # Reordena 'base_log' de forma crescente com base em 'mo'

x_lower_limit <- 0.1
x_upper_limit <- 3

# Para o primeiro subplot (< 60)
base_log <- base_log %>% mutate(
  xmin_adj1 = pmax(cl1, x_lower_limit),
  xmax_adj1 = pmin(ch1, x_upper_limit),
  cl_below1 = cl1 < x_lower_limit,
  ch_above1 = ch1 > x_upper_limit
)

# Para o segundo subplot (60 to 74)
base_log <- base_log %>% mutate(
  xmin_adj2 = pmax(cl2, x_lower_limit),
  xmax_adj2 = pmin(ch2, x_upper_limit),
  cl_below2 = cl2 < x_lower_limit,
  ch_above2 = ch2 > x_upper_limit
)

# Para o terceiro subplot (75 or more)
base_log <- base_log %>% mutate(
  xmin_adj3 = pmax(cl3, x_lower_limit),
  xmax_adj3 = pmin(ch3, x_upper_limit),
  cl_below3 = cl3 < x_lower_limit,
  ch_above3 = ch3 > x_upper_limit
)

line_size <- 0.4
cap_size <- 0.4

# 2. Função para criar os gráficos (usando 'vars' em vez de 'out')
create_plot <- function(data, x_var, xmin_var, xmax_var, cl_below_var, ch_above_var,
                        x_lower_limit, x_upper_limit, x_scale, x_breaks, x_label, title, color_segments) {
  ggplot(data, aes(y = vars, x = .data[[x_var]])) +
    geom_point(shape = 15, size = 2, color = color_segments) +
    # Caso 2: cl abaixo do limite inferior
    geom_segment(
      data = data %>% filter(.data[[cl_below_var]]),
      aes(x = .data[[xmin_var]], xend = .data[[xmax_var]], y = vars, yend = vars),
      size = line_size,
      color = color_segments,
      arrow = arrow(
        angle = 15, ends = "first", type = "closed",
        length = unit(0.3, "cm")
      )
    ) +
    # Caso 1: cl e ch dentro dos limites
    geom_errorbarh(
      data = data %>% filter(!.data[[cl_below_var]] & !.data[[ch_above_var]]),
      aes(xmin = .data[[xmin_var]], xmax = .data[[xmax_var]], y = vars),
      height = cap_size,
      size = line_size,
      color = color_segments
    ) +
    # Caso 3: ch acima do limite superior
    geom_segment(
      data = data %>% filter(.data[[ch_above_var]]),
      aes(x = .data[[xmin_var]], xend = .data[[xmax_var]], y = vars, yend = vars),
      size = line_size,
      color = color_segments,
      arrow = arrow(
        angle = 15, ends = "last", type = "closed",
        length = unit(0.3, "cm")
      )
    ) +
    # Adicionar cap vertical no extremo superior para Caso 2
    geom_errorbarh(
      data = data %>% filter(.data[[cl_below_var]]),
      aes(xmin = .data[[xmax_var]], xmax = .data[[xmax_var]], y = vars),
      height = cap_size,
      size = line_size,
      color = color_segments
    ) +
    # Adicionar cap vertical no extremo inferior para Caso 3
    geom_errorbarh(
      data = data %>% filter(.data[[ch_above_var]]),
      aes(xmin = .data[[xmin_var]], xmax = .data[[xmin_var]], y = vars),
      height = cap_size,
      size = line_size,
      color = color_segments
    ) +
    # Adicionar estimativas pontuais
    geom_point(shape = 15, size = 2, color = color_segments) +
    # Linha vertical em x = 1
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1, color = "azure3") +
    # Ajustar escalas e rótulos
    scale_y_discrete(
      limits = levels(data$vars),  # Manter a ordem dos níveis de 'vars'
      labels = new_labels(levels(data$vars))  # Se tiver uma função para ajustar os rótulos
    ) +
    scale_x_continuous(
      trans = "log10",
      breaks = c(0.1, 0.2, 0.5, 1, 1.5, 2,3),
      limits = c(x_lower_limit, x_upper_limit)
    ) +
    labs(y = NULL, x = x_label, title = title) +
    theme_classic(base_family = font) +
    theme(
      axis.title.x = element_text(size = 10, family = font),
      axis.text.x = element_text(size = 8, family = font),
      axis.text.y = ggtext::element_markdown(size = 10, family = font),
      plot.margin = unit(c(2, 2, 0, 2), "mm"),
      plot.title = element_text(hjust = 0.5, size = 12, family = font)
    ) +
    coord_cartesian(clip = "off")
}

# 3. Criar os subplots com cores diferentes

# Definir cores para cada subplot
colors <- c("blue", "green", "red")  # Você pode escolher as cores que preferir

# Primeiro subplot (< 60)
f1 <- create_plot(
  data = base_log,
  x_var = "or1",
  xmin_var = "xmin_adj1",
  xmax_var = "xmax_adj1",
  cl_below_var = "cl_below1",
  ch_above_var = "ch_above1",
  x_lower_limit = x_lower_limit,
  x_upper_limit = x_upper_limit,
  x_scale = "log",
  x_breaks = c(0.1, 0.2, 0.5, 1, 1.5, 2,3),
  x_label = "OR",
  title = "< 60",
  color_segments = colors[1]
)

# Segundo subplot (60 to 74)
f2 <- create_plot(
  data = base_log,
  x_var = "or2",
  xmin_var = "xmin_adj2",
  xmax_var = "xmax_adj2",
  cl_below_var = "cl_below2",
  ch_above_var = "ch_above2",
  x_lower_limit = x_lower_limit,
  x_upper_limit = x_upper_limit,
  x_scale = "log",
  x_breaks = c(0.1, 0.2, 0.5, 1, 1.5, 2,3),
  x_label = "OR",
  title = "60 to 74",
  color_segments = colors[2]
)

# Terceiro subplot (75 or more)
f3 <- create_plot(
  data = base_log,
  x_var = "or3",
  xmin_var = "xmin_adj3",
  xmax_var = "xmax_adj3",
  cl_below_var = "cl_below3",
  ch_above_var = "ch_above3",
  x_lower_limit = x_lower_limit,
  x_upper_limit = x_upper_limit,
  x_scale = "log",
  x_breaks = c(0.1, 0.2, 0.5, 1, 1.5, 2,3),
  x_label = "OR",
  title = "75 or more",
  color_segments = colors[3]
)

# Remover os textos do eixo Y dos subplots 2 e 3 para compartilhar com o primeiro
f2 <- f2 +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

f3 <- f3 +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

# 4. Combinar os subplots em uma única figura
combined_plot <- plot_grid(
  f1,
  f2,
  f3,
  nrow = 1,
  rel_widths = c(2, 1, 1),
  align = "h"
)


# 5. Salvar o gráfico combinado em PDF
pdf("interac1_plots.pdf", family = font, width = 18, height = 7)
print(combined_plot)
dev.off()

# Exibir o gráfico combinado
print(combined_plot)

################################################################################
#fig4
data = readRDS("./pos_data/dataset.rds")
df = data$base$df
lab = read_csv("./result/base_log_best.csv")$vars
aux = df[,c("disease",lab[1:20])]
aux$disease = as.factor(aux$disease)
aux = gather(data = aux, key = marks,value = value,-disease)
aux = aux[!is.na(aux$value),]
levels(aux$disease) = dis$var
aux$marks = factor(aux$marks,levels = lab[1:20])
##violin

fig_c=aux %>% ggplot(aes(x=disease,y=value,fill = disease))+
  facet_wrap( ~ marks,scales = "free_y",ncol = 4 )+
  geom_violin(scale = "width",trim = FALSE,draw_quantiles = c(0.5),na.rm = TRUE)+
  geom_jitter(width = 0.2,height = 0,size=1,na.rm = TRUE)+
  scale_x_discrete(limits = dis$var)+
  scale_y_continuous(limits = c(0,NA))+
  scale_fill_manual(values=dis$color,limits = dis$var)+
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
pdf("fig_violin1.pdf", family = font, width = 17, height = 10)
print(fig_c)
dev.off()
fig_c
################################################################################
#fig5
data = readRDS("./pos_data/dataset.rds")
df = data$base$df
lab = read_csv("./result/fil_log_best.csv")$vars
aux = df[,c("disease",lab[1:20])]
aux$disease = as.factor(aux$disease)
aux = gather(data = aux, key = marks,value = value,-disease)
aux = aux[!is.na(aux$value),]
levels(aux$disease) = dis$var
aux$marks = factor(aux$marks,levels = lab[1:20])
##violin

fig_c=aux %>% ggplot(aes(x=disease,y=value,fill = disease))+
  facet_wrap( ~ marks,scales = "free_y",ncol = 4 )+
  geom_violin(scale = "width",trim = FALSE,draw_quantiles = c(0.5),na.rm = TRUE)+
  geom_jitter(width = 0.2,height = 0,size=1,na.rm = TRUE)+
  scale_x_discrete(limits = dis$var)+
  scale_y_continuous(limits = c(0,NA))+
  scale_fill_manual(values=dis$color,limits = dis$var)+
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
pdf("fig_violin2.pdf", family = font, width = 17, height = 10)
print(fig_c)
dev.off()
fig_c

################################################################################
#enviar para python
data = readRDS("./pos_data/dataset.rds")
df_fil = data$fil_imp$df[data$fil_imp$fil_sam,]

data_log = readRDS("./result/logistic.rds")
aux = data_log$res_fil
aux1 = aux[aux$pvalue_mark_dis<0.05 & aux$p_iter_test>=0.05,]
aux2 = aux[aux$pvalue_mark_dis>=0.05 & aux$p_iter_test>=0.05,]
aux1$mo = NA
aux1$mo = NA
aux1$mo[aux1$or_mark_dis>=1] = aux1$or_mark_dis[aux1$or_mark_dis>=1]
aux1$mo[aux1$or_mark_dis<1] = 1/aux1$or_mark_dis[aux1$or_mark_dis<1]
aux1 <- aux1 %>%
  arrange(desc(mo))
aux1$mo = NULL
aux2 <- aux2 %>%
  arrange(pvalue_mark_dis)
aux = rbind(aux1,aux2)
col = c(aux$vars,"age","gender","disease")
df = df_fil[,col]
saveRDS(df,"./pos_data/fil_data_ord.rds")

################################################################################
#fig6
data = read.csv("./result/auc_n.csv")
fig_b = data[1:30,] %>% 
  ggplot(aes(x=n_var,y=auc))+
  geom_point(size=1,color="gray")+
  geom_line()+
  #geom_ribbon(aes(ymax = auc_h,ymin = auc_l),fill = dis$color[d],alpha=0.4)+
  geom_smooth(method = "loess",span=0.3,fill="lightblue",color="lightgreen")+
  labs(x = "Markers number",y="AUC")+
  #scale_y_continuous(limits = c(0.7,NA)) +
  #scale_x_continuous(limits = c(1,NA)) +
  geom_vline(xintercept=distan,color = "darkred") +
  annotate("text",x=distan +1,y=0.65,label=paste0("n = ",distan),size=4,family=font,angle = 90,color = "darkred")+
  theme_bw(base_family  = font)+
  theme(axis.title.x = element_text(size = 10,family = font),axis.title.y = element_text(size = 10,family = font),
        axis.text.x = element_text(size=8,family = font),axis.text.y = element_text(size=8,family = font))
pdf("auc_n.pdf", family = font, width = 3, height = 3)
print(fig_b)
dev.off()
fig_b
################################################################################
#fig7
## AUC curve
aux = read.csv(file = "./result/auc_curv.csv")
label =paste0(sprintf(aux$auc[1], fmt = '%#.2f')," (",sprintf(aux$auc_l[1], fmt = '%#.2f')," : ",sprintf(aux$auc_h[1], fmt = '%#.2f'),")")
fig_d = aux %>%  ggplot(aes(x=fpr,y=tpr))+
  geom_line(color = dis$color[2],linewidth=1)+
  geom_ribbon(aes(ymax = tpr_h,ymin = tpr_l),fill = dis$color[2],alpha=0.4)+
  geom_richtext(color="black",fill="gray95",x=0.7,y=0.1,label=label,size=4,family = font,fontface ="plain")+
  geom_abline(slope = 1,intercept = 0,linetype=2,linewidth=1,color="gray")+
  scale_x_continuous(limits = c(0, 1),expand = c(0,0))+
  scale_y_continuous(limits = c(0, 1),expand = c(0,0))+
  ylab("True positive rate")+
  xlab("False positive rate")+
  theme_bw(base_family = font)+
  theme(axis.title.x = element_text(size = 10,family = font),axis.title.y = element_text(size = 10,family = font),
        axis.text.x = element_text(size=8,family = font),axis.text.y = element_text(size=8,family = font))
pdf("auc_curv.pdf", family = font, width = 3, height = 3)
print(fig_d)
dev.off()
fig_d
################################################################################
#fig8
##PCA all
data = readRDS("./pos_data/dataset.rds")
x = data$fil_imp$df[data$fil_imp$fil_sam,data$fil_imp$fil_immuno]
y = data$fil_imp$df[data$fil_imp$fil_sam,"disease"]
y = as.factor(y)
levels(y) = c(dis$var[1],dis$var[2])
df_pca =  prcomp(x, scale. = TRUE)
df_pca <- data.frame(PC1 = df_pca$x[, 1], PC2 = df_pca$x[, 2], Disease = as.character(y))
p <- df_pca %>% ggplot(aes(x=PC1, y=PC2)) +
  geom_point(aes(color=Disease), size=2) +
  scale_color_manual(
    values=color_eu,
    limits = c(dis$var[1], dis$var[2])
  ) +
  geom_hline(yintercept=0, linetype=2, linewidth=1, color="darkgray") +
  geom_vline(xintercept=0, linetype=2, linewidth=1, color="darkgray") +
  theme_bw(base_family = font) +
  ggtitle("PCA All marks") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.15, 0.15), # Posição relativa da legenda
    legend.background = element_rect(fill = "white", color = "black"), # Fundo branco com borda preta
    legend.box.margin = margin(0, 0, 0, 0), # Margem interna da caixa
    axis.title.x = element_text(size = 10, family = font),
    axis.title.y = element_text(size = 10, family = font),
    axis.text.x = element_text(size = 8, family = font),
    axis.text.y = element_text(size = 8, family = font)
  )

fig_e <- ggExtra::ggMarginal(
  p,
  groupColour = TRUE,
  groupFill = TRUE
)

pdf("pca_all.pdf", family = font, width = 5, height = 5)
print(fig_e)
dev.off()
fig_e
################################################################################
#fig9
##PCA best
data = readRDS("./pos_data/dataset.rds")
x = data$fil_imp$df[data$fil_imp$fil_sam,data$fil_imp$fil_immuno]
y = data$fil_imp$df[data$fil_imp$fil_sam,"disease"]
y = as.factor(y)
levels(y) = c(dis$var[1],dis$var[2])
lab = read_csv("./result/fil_log_best.csv")$vars[1:4]
x = x[,lab]
pc = prcomp(x,scale. = TRUE)
data = as.data.frame(pc$x[,c(1,2)])
data$Disease = y
aux =  as.data.frame(pc$rotation[,c(1,2)])[lab[1:4],]
#row.names(aux) = NULL
p <- data %>% ggplot(aes(x=PC1, y=PC2))+
  geom_point(aes(color=Disease),size=2)+
  scale_color_manual(values=color_eu,limits = dis$var)+
  geom_hline(yintercept=0, linetype=2,linewidth=1,color="darkgray")+geom_vline(xintercept=0, linetype=2,linewidth=1,color="darkgray")+
  theme_bw(base_family = font)+
  theme(legend.position = "none",axis.title.x = element_text(size = 10,family = font),axis.title.y = element_text(size = 10,family = font),
        axis.text.x = element_text(size=8,family = font),axis.text.y = element_text(size=8,family = font))
fig1_pca <- p + 
  geom_richtext(data=aux, aes(x=PC1*2, y=PC2*2, label=new_labels(row.names(aux))), size = 2.5, color="black",family=font,fill=fill_alpha("bisque2",0.7))+
  geom_segment(data=aux,aes(x=0, y=0, xend=PC1*2, yend=PC2*2), arrow=arrow(length=unit(0.3,"cm")), color="black",linewidth=1)+
  theme_bw(base_family = font)+
  theme(legend.position = "none",axis.title.x = element_text(size = 10,family = font),axis.title.y = element_text(size = 10,family = font),
        axis.text.x = element_text(size=8,family = font),axis.text.y = element_text(size=8,family = font),plot.caption = element_markdown(size = 10))

fig_f=ggExtra::ggMarginal(fig1_pca,groupColour = TRUE, groupFill = TRUE)
fig_f