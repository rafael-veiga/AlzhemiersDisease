library(tibble)
library(readr)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
#args = c("AD_data_combined.csv","age_cohort.csv")

# functions
get_fil <- function(col,type="all",pop="all",mark="all"){
  fil_t = rep(FALSE,length(col))
  for(t in type){
    if(t=="all"){
      fil_t=rep(TRUE,length(col))
      break
    }
    if(t=="other"){
      fil_t = (fil_t) | (col=="Sample") | (col=="batch") | (col=="id") | (col=="type") | (col=="age") | (col=="sex") | (col=="disease") | (col=="fil_dis")
    }
    if(t=="pop_in_pop"){
      fil_t = (fil_t) | (grepl(" in ",col) & (!grepl(" median ",col)) )
    }
    if(t=="mark_in_pop"){
      fil_t = (fil_t) | grepl(" median in ",col)
    }
  }
  fil_p = rep(FALSE,length(col))
  for(p in pop){
    if(p=="all"){
      fil_p=rep(TRUE,length(col))
      break
    }
    f = as.logical(sapply(col,function(s){
      if(grepl(" in ",s)){
        return (unlist(str_split(s," in "))[2])
      }
      return(s)
    })==p)
    ini = as.logical(sapply(col,function(s){
      if(grepl(" in ",s) & (!grepl(" median ",s))){
        return (unlist(str_split(s," in "))[1])
      }
      return(s)
    })==p)
    f_ini = f | ini
    fil_p = fil_p | f_ini
  }
  fil_m = rep(FALSE,length(col))
  for(m in mark){
    if(m=="all"){
      fil_m=rep(TRUE,length(col))
      break
    }
    f = as.logical(sapply(col,function(s){return (unlist(str_split(s," median in "))[1])})==m)
    fil_m = fil_m | f
  }
  return(fil_m & fil_p & fil_t)
}
# read files
df = read_csv(args[1])
df$disease = NA
fill1 = str_ends(df$Sample,"P")
fill0 = str_ends(df$Sample,"C")
df$disease[fill1] = 1
df$disease[fill0] = 0

df_age = read_csv(args[2])
df = merge(df_age,df,by="Sample")
df$gender = as.factor(df$gender)

df$Sample = substr(df$Sample,1,5)
df <- tibble(reg = 1:nrow(df), df)

st1_list = list()
st2_list = list()
st3_list = list()
col = colnames(df)
for(i in 1:length(col)){
  aux = str_split(col[i],": ")[[1]]
  if(length(aux)==2){
    if(aux[2]!="batch"){
      if(aux[1]=="ST1"){
        st1_list = append(st1_list,aux[2])
        col[i] = aux[2]
      }
      if(aux[1]=="ST2"){
        st2_list = append(st2_list,aux[2])
        col[i] = aux[2]
      }
      if(aux[1]=="ST3"){
        st3_list = append(st3_list,aux[2])
        col[i] = aux[2]
      }
    }
  }
}
lista_duplicados = unique(col[duplicated(col)])
novocol = unique(col)
novodf = tibble(reg = df$reg)
for(var in novocol){
  if(var %in% lista_duplicados){
    novodf[[var]] = rowMeans(df[,col==var],na.rm = TRUE)
  }else{
    novodf[[var]] = df[[colnames(df)[col==var]]]
  }
  
}
df = novodf
st1_list = unlist(st1_list)
st2_list = unlist(st2_list)
st3_list = unlist(st3_list)

fil = get_fil(colnames(df),type = c("pop_in_pop","mark_in_pop"))
immun = colnames(df)[fil]
other = colnames(df)[!fil]
col = colnames(df)
painel = rep(NA,length(col))
for(i in 1:length(col)){
  if(col[i] %in% st3_list){
    painel[i] = "ST3"
  }
  if(col[i] %in% st1_list){
    painel[i] = "ST1"
  }
  if(col[i] %in% st2_list){
    painel[i] = "ST2"
  }
    
}

st1_df = df[,c(other,st1_list)]
st2_df = df[,c(other,st2_list)]
st3_df = df[,c(other,st3_list)]


st1_df = st1_df[!is.na(st1_df$`ST1: batch`),]
st2_df = st2_df[!is.na(st2_df$`ST2: batch`),]
st3_df = st3_df[!is.na(st3_df$`ST3: batch`),]

data = list(df=df,painel=painel,immun=immun)

saveRDS(data, file = "data_raw.rds")

fil = get_fil(colnames(st1_df),type = c("pop_in_pop","mark_in_pop"))
immun = colnames(st1_df)[fil]
data = list(df=st1_df,immun=immun)
saveRDS(st1_df, file = "ST1_raw.rds")

fil = get_fil(colnames(st2_df),type = c("pop_in_pop","mark_in_pop"))
immun = colnames(st2_df)[fil]
data = list(df=st2_df,immun=immun)
saveRDS(st2_df, file = "ST2_raw.rds")

fil = get_fil(colnames(st3_df),type = c("pop_in_pop","mark_in_pop"))
immun = colnames(st3_df)[fil]
data = list(df=st3_df,immun=immun)
saveRDS(st3_df, file = "ST3_raw.rds")