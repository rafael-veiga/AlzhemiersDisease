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

remove_missing <- function(df,per){
  df1 = df
  #df1$type = NULL
  vari = colnames(df1)[get_fil(colnames(df1),type=c("mark_in_pop","pop_in_pop"))]
  for(v in vari){
    tab = as.data.frame(table(is.na(df1[,colnames(df1)==v])))
    if("TRUE" %in% tab$Var1){
      perc = tab$Freq[tab$Var1=="TRUE"]/ sum(tab$Freq)
      
    }else{
      perc = 0
    }
    
    if(perc>per){
      #print(colnames(df1))
      df1 = df1[,colnames(df1)!=v]
    }
  }
  return(df1)
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

st1_df = df[,c(other,st1_list)]
st2_df = df[,c(other,st2_list)]
st3_df = df[,c(other,st3_list)]


st1_df = st1_df[!is.na(st1_df$`ST1: batch`),]
st2_df = st2_df[!is.na(st2_df$`ST2: batch`),]
st3_df = st3_df[!is.na(st3_df$`ST3: batch`),]

st1_df = remove_missing(st1_df,per=0.2)
st2_df = remove_missing(st2_df,per=0.2)
st3_df = remove_missing(st3_df,per=0.2)

remove_dub <-function(df_aux){
  col = colnames(df_aux)
  fil_x = str_ends(col,".x")
  a = col[fil_x]
  a = substr(a,start = 1,stop=nchar(a)-2)
  for(i in 1:length(a)){
    if(is.factor(df_aux[,paste0(a[i],".x")])){
      df_aux[a[i]] = factor(
        ifelse(is.na(df_aux[[paste0(a[i], ".x")]]), 
               as.character(df_aux[[paste0(a[i], ".y")]]), 
               as.character(df_aux[[paste0(a[i], ".x")]]))
      )
    }else{
      df_aux[a[i]] = rowMeans(df_aux[,c(paste0(a[i],".x"),paste0(a[i],".y"))],na.rm = TRUE)
    }
    df_aux[paste0(a[i],".x")] = NULL
    df_aux[paste0(a[i],".y")] = NULL
  }
  return(df_aux)
}



df <- merge(st1_df, st2_df, by = c("Sample", "disease"),all = TRUE)
df = remove_dub(df)
df <- merge(df, st3_df, by = c("Sample", "disease"),all = TRUE)
df = remove_dub(df)

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

data = list(df=df,painel=painel,immun=immun)

saveRDS(data, file = "data_raw.rds")

