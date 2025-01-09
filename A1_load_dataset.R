library(tidyverse)

# read files
df = read_csv("./origin_data/AD_data_combined.csv")
df$disease = NA
fill1 = str_ends(df$Sample,"P")
fill0 = str_ends(df$Sample,"C")
df$disease[fill1] = 1
df$disease[fill0] = 0

df_age = read_csv("./origin_data/age_cohort.csv")
df = merge(df_age,df,by="Sample")
df$gender = as_factor(df$gender)

df$Sample = substr(df$Sample,1,5)

other = c("Sample","ST1: batch","ST2: batch","ST3: batch","age","gender","disease")
pos = rep(TRUE,ncol(df))
neg = rep(TRUE,ncol(df))
for(i in 1:ncol(df)){
  print(i)
  if(sum(is.na(df[df$disease==0,i])) >= nrow(df[df$disease==0,])-2){
    neg[i] = FALSE
  }
  if(sum(is.na(df[df$disease==1,i])) >= nrow(df[df$disease==1,])-2){
    pos[i] = FALSE
  }
}
df = df[,pos & neg]


st1_df = df[,other]
st2_df = df[,other]
st3_df = df[,other]

col = colnames(df)
for(var in col){
if(!(var %in% other)){
 aux = str_split(var,": ")
 if(aux[[1]][1]=="ST1"){
   st1_df[[aux[[1]][2]]] = df[[var]]
 }
 if(aux[[1]][1]=="ST2"){
   st2_df[[aux[[1]][2]]] = df[[var]]
 }
 if(aux[[1]][1]=="ST3"){
   st3_df[[aux[[1]][2]]] = df[[var]]
 }
}
}

tot = c(colnames(st1_df)[8:length(st1_df)],colnames(st2_df)[8:length(st2_df)],colnames(st3_df)[8:length(st3_df)])
tot = unique(tot)
df = df[,other]
for(var in tot){
  aux = data.frame(Coluna_NA = rep(NA, length(st1_df$Sample)))
  if(var %in% colnames(st1_df)){
    aux[["st1"]] = st1_df[[var]] 
  }
  if(var %in% colnames(st2_df)){
    aux[["st2"]] = st2_df[[var]] 
  }
  if(var %in% colnames(st3_df)){
    aux[["st3"]] = st3_df[[var]] 
  }
  df[[var]] = rowMeans(aux, na.rm = TRUE)
}

st2_df = st2_df[,colnames(st2_df)!= "ST1: batch"]
st3_df = st3_df[,colnames(st3_df)!= "ST1: batch"]

st1_df = st1_df[,colnames(st1_df)!= "ST2: batch"]
st3_df = st3_df[,colnames(st3_df)!= "ST2: batch"]

st1_df = st1_df[,colnames(st1_df)!= "ST3: batch"]
st2_df = st2_df[,colnames(st2_df)!= "ST3: batch"]


st1_df = st1_df[!is.na(st1_df$`ST1: batch`),]
st2_df = st2_df[!is.na(st2_df$`ST2: batch`),]
st3_df = st3_df[!is.na(st3_df$`ST3: batch`),]

saveRDS(df, file = "./pos_data/data_raw.rds")
saveRDS(st1_df, file = "./pos_data/ST1_raw.rds")
saveRDS(st2_df, file = "./pos_data/ST2_raw.rds")
saveRDS(st3_df, file = "./pos_data/ST3_raw.rds")