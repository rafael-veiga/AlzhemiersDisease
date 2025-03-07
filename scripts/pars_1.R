library(tibble)
library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
#args = c("imp_data.rds","res_lr.csv","out.csv")

file_input = args[1]
file_input2 = args[2]
file_outpu = args[3]
set.seed(42)

################

#############################
data = readRDS(file = file_input)
 
pvalue_diff = rep(NA,length(data$immun))
stat_diff = pvalue_diff
pvalue_real = pvalue_diff
stat_real = pvalue_diff
pvalue_alt = pvalue_diff
stat_alt = pvalue_diff
for(i in 1:length(data$immun)){
  print(i)
imuno = data$immun[i]
if(data$painel[colnames(data$df)==imuno]=="ST1"){
  st = data$df$`ST1: batch`
}
if(data$painel[colnames(data$df)==imuno]=="ST2"){
  st = data$df$`ST2: batch`
}
if(data$painel[colnames(data$df)==imuno]=="ST3"){
  st = data$df$`ST3: batch`
}
aux = data$df[!is.na(st),]
st = st[!is.na(st)]
aux$st = st
id = unique(aux$Sample)
#remove id not par
for(a in id){
  if(length(aux$Sample[aux$Sample==a])!=2){
    id = id[id!=a]
  }
}
#remover != bach
id_aux = id
for(j in 1:length(id)){
  fil = aux$Sample==id[j]
  if(st[fil][1]!=st[fil][2]){
    id_aux[id_aux!=id[j]]
  }
}
id = id_aux
real_neg = rep(NA,length(id))
real_pos = rep(NA,length(id))
alt_neg = rep(NA,length(id))
alt_pos = rep(NA,length(id))
aux = aux[aux$Sample %in% id,]
aux_real = aux
aux_sim = aux[aux$disease==0,]
falta = list()
#criterio 1 sex e bach
for(j in 1:length(id)){
  fil1 = (aux$Sample ==id[j])&(aux$disease==0)
  fil2 = (aux$Sample ==id[j])&(aux$disease==1)
  sex = aux$gender[fil1]
  bach = aux$st[fil1]
  real_neg[j] = aux[fil1,imuno]
  real_pos[j] = aux[fil2,imuno]
  alt_pos[j] = real_pos[j]
  filt_1 = (aux_sim$st==bach) & (aux_sim$gender==sex) & (aux_sim$Sample != id[j])
  if(length(aux_sim$Sample[filt_1])>0){
    id_select =  aux_sim$Sample[sample(1:length(aux_sim$Sample[filt_1]), 1)]
    alt_neg[j] = aux_sim[aux_sim$Sample==id_select,imuno]
    aux_sim = aux_sim[!aux_sim$Sample==id_select,]
  }else{
    falta = append(falta,j)
  }
}
#criterio 2 bach
falta2 = list()
for(j in falta){
  fil1 = (aux$Sample ==id[j])&(aux$disease==0)
  fil2 = (aux$Sample ==id[j])&(aux$disease==1)
  bach = aux$st[fil1]
  filt_1 = (aux_sim$st==bach) & (aux_sim$Sample != id[j])
  if(length(aux_sim$Sample[filt_1])>0){
    id_select =  aux_sim$Sample[sample(1:length(aux_sim$Sample[filt_1]), 1)]
    alt_neg[j] = aux_sim[aux_sim$Sample==id_select,imuno]
    aux_sim = aux_sim[!aux_sim$Sample==id_select,]
  }else{
    falta2 = append(falta2,j)
  }
}
#criterio 3 sex
falta3 = list()
for(j in falta2){
  fil1 = (aux$Sample ==id[j])&(aux$disease==0)
  fil2 = (aux$Sample ==id[j])&(aux$disease==1)
  sex = aux$gender[fil1]
  filt_1 = (aux_sim$gender==sex) & (aux_sim$Sample != id[j])
  if(length(aux_sim$Sample[filt_1])>0){
    id_select =  aux_sim$Sample[sample(1:length(aux_sim$Sample[filt_1]), 1)]
    alt_neg[j] = aux_sim[aux_sim$Sample==id_select,imuno]
    aux_sim = aux_sim[!aux_sim$Sample==id_select,]
  }else{
    falta3 = append(falta3,j)
  }
}
#criterio 4 no criterium
for(j in falta3){
    id_select =  aux_sim$Sample[sample(1:length(aux_sim$Sample[aux_sim$Sample!=id[j]]), 1)]
    alt_neg[j] = aux_sim[aux_sim$Sample==id_select,imuno]
    aux_sim = aux_sim[!aux_sim$Sample==id_select,]
}
real_pos = unlist(real_pos)
real_neg = unlist(real_neg)
alt_pos = unlist(alt_pos)
alt_neg = unlist(alt_neg)
delta_real = real_pos - real_neg
delta_sim = alt_pos - alt_neg
res = wilcox.test(delta_real, delta_sim,paired = TRUE, exact = TRUE)
pvalue_diff[i] = res$p.value
stat_diff[i] = res$statistic/sqrt(length(delta_real))
res = wilcox.test(real_pos, real_neg,paired = TRUE, exact = TRUE)
pvalue_real[i] = res$p.value
stat_real[i] = res$statistic/sqrt(length(delta_real))
res = wilcox.test(alt_pos, alt_neg,paired = TRUE, exact = TRUE)
pvalue_alt[i] = res$p.value
stat_alt[i] = res$statistic/sqrt(length(delta_real))

}
res = data_frame(vars=data$immun,pvalue_diff,pvalue_real,pvalue_alt,stat_diff,stat_real,stat_alt)
write.table(res,file = file_outpu,sep=";",quote = FALSE,row.names = FALSE)

fig1 <- ggplot(res, aes(x = -log10(pvalue_real), y = -log10(pvalue_alt))) +
  geom_point(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE, color = "red") +
  labs(
    title = "Scatterplot pares",
    x = "p-value real pares (-log10)",
    y = "p-value simulated pares (-log10)"
  ) +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +
  theme_minimal()
pdf("fig1.pdf", width = 5, height = 5)
print(fig1)
dev.off()

##############################################################################
#st1
aux = data$df
imuno = data$immun
aux = aux[!is.na(aux$`ST1: batch`),]
id = unique(aux$Sample)
for(a in id){
  if(length(aux$Sample[aux$Sample==a])!=2){
    id = id[id!=a]
  }
}
aux = aux[aux$Sample %in% id,]
dist_par_real = rep(NA,length(id))
dist_par_sim = rep(NA,length(id))
aux$`ST2: batch` = NULL
aux$`ST3: batch` = NULL
aux$reg = NULL
aux$age = NULL
aux_pos = aux[aux$disease==1,]
aux_neg = aux[aux$disease==0,]
aux_neg$disease = NULL
aux_pos$disease = NULL
sex = aux_neg$gender
aux_neg$gender = NULL
aux_pos$gender= NULL
sample_neg = aux_neg$Sample
sample_pos = aux_pos$Sample
aux_neg$Sample = NULL
aux_pos$Sample = NULL
st_neg = aux_neg$`ST1: batch`
st_pos = aux_pos$`ST1: batch`
aux_neg$`ST1: batch` = NULL
aux_pos$`ST1: batch` = NULL
vaux = aux_neg
vsample = sample_neg
vsex = sex
vbatch = st_neg
falta = list()
for(i in 1:length(id)){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  value_neg_real = as.numeric(aux_neg[sample_neg==id[i],])
  value_pos_real = as.numeric(aux_pos[sample_pos==id[i],])
  dist_par_real[i] = sqrt(sum((value_pos_real - value_neg_real)^2))
  fil = (vsex==isex)&(vbatch==batch)&(vsample!=id[i])
  #criterio 1 sex batch
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta = append(falta,i)
  }
}
falta2 = list()
for(i in falta){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  fil = (vbatch==batch)&(vsample!=id[i])
  #criterio 2 batch
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta2 = append(falta2,i)
  }
}
falta3 = list()
for(i in falta2){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  fil = (vsex==isex)&(vsample!=id[i])
  #criterio 3 sex
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta3 = append(falta3,i)
  }
}
falta4 = list()
for(i in falta3){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  fil = (vsample!=id[i])
  #criterio 3
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta4 = append(falta4,i)
  }
}


df <- data.frame(
  distance = c(dist_par_sim, dist_par_real),
  group = factor(rep(c("Simulated", "Real"),
                     times = c(length(dist_par_sim), length(dist_par_real))))
)

fig2 = ggplot(df, aes(x = group, y = distance, fill = group)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers so they don't overlap with jitter
  geom_jitter(width = 0.2, color = "black", alpha = 0.7) +
  labs(
    title = "Euclidean Distances pars all variables",
    x = "pars",
    y = "Euclidean Distance ST1"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

pdf("fig2.pdf", width = 4, height = 4)
print(fig2)
dev.off()

################################################################################
res = read.csv(file_input2)
aux = data$df
imuno = data$immun
aux = aux[!is.na(aux$`ST1: batch`),]
id = unique(aux$Sample)
for(a in id){
  if(length(aux$Sample[aux$Sample==a])!=2){
    id = id[id!=a]
  }
}
aux = aux[aux$Sample %in% id,]
dist_par_real = rep(NA,length(id))
dist_par_sim = rep(NA,length(id))
aux$`ST2: batch` = NULL
aux$`ST3: batch` = NULL
aux$reg = NULL
aux$age = NULL
aux_pos = aux[aux$disease==1,]
aux_neg = aux[aux$disease==0,]
aux_neg$disease = NULL
aux_pos$disease = NULL
sex = aux_neg$gender
aux_neg$gender = NULL
aux_pos$gender= NULL
sample_neg = aux_neg$Sample
sample_pos = aux_pos$Sample
aux_neg$Sample = NULL
aux_pos$Sample = NULL
st_neg = aux_neg$`ST1: batch`
st_pos = aux_pos$`ST1: batch`
aux_neg$`ST1: batch` = NULL
aux_pos$`ST1: batch` = NULL
vaux = aux_neg
vsample = sample_neg
vsex = sex
vbatch = st_neg
falta = list()
imune_T = res$vars[1:10]
for(i in 1:length(id)){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  value_neg_real = as.numeric(aux_neg[sample_neg==id[i],imune_T])
  value_pos_real = as.numeric(aux_pos[sample_pos==id[i],imune_T])
  dist_par_real[i] = sqrt(sum((value_pos_real - value_neg_real)^2))
  fil = (vsex==isex)&(vbatch==batch)&(vsample!=id[i])
  #criterio 1 sex batch
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,imune_T])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta = append(falta,i)
  }
}
falta2 = list()
for(i in falta){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  fil = (vbatch==batch)&(vsample!=id[i])
  #criterio 2 batch
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,imune_T])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta2 = append(falta2,i)
  }
}
falta3 = list()
for(i in falta2){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  fil = (vsex==isex)&(vsample!=id[i])
  #criterio 3 sex
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,imune_T])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta3 = append(falta3,i)
  }
}
falta4 = list()
for(i in falta3){
  isex = sex[sample_neg==id[i]]
  batch = st_neg[sample_neg==id[i]]
  fil = (vsample!=id[i])
  #criterio 3
  
  if(length(fil[fil])>0){
    id_select =  vsample[sample(1:length(fil[fil]), 1)]
    value_neg_sim = as.numeric(vaux[vsample==id_select,imune_T])
    dist_par_sim[i] = sqrt(sum((value_pos_real - value_neg_sim)^2))
    fil2 = vsample!=id_select
    vaux = vaux[fil2,]
    vsample = vsample[fil2]
    vsex = vsex[fil2]
    vbatch = vbatch[fil2]
  }else{
    falta4 = append(falta4,i)
  }
}


df <- data.frame(
  distance = c(dist_par_sim, dist_par_real),
  group = factor(rep(c("Simulated", "Real"),
                     times = c(length(dist_par_sim), length(dist_par_real))))
)

fig3 = ggplot(df, aes(x = group, y = distance, fill = group)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers so they don't overlap with jitter
  geom_jitter(width = 0.2, color = "black", alpha = 0.7) +
  labs(
    title = "Euclidean Distances pars 10 best",
    x = "pars",
    y = "Euclidean Distance ST1"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

pdf("fig3.pdf", width = 4, height = 4)
print(fig3)
dev.off()