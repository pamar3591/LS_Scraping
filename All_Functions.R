library('plyr')
library('dplyr')
library('stringr')

compare<-function(df1, df2, key1,key2,rvalue){
  #df1<-req_ls_data   df2<-constituency_electoral_info  key1<-ls_key    key2<-const_eci_key
  
  ncol_df1=(ncol(df1))
  ncol_df2=(ncol(df2))
  
  #EXACT MATCH
  exact_match<-merge(df1,df2,by=colnames(key1)[1])
  
  #IN DF1 but not in DF2
  all_unmatch<-merge(df1,df2,by.x=colnames(key1)[1],by.y=colnames(key2)[1],all=TRUE)
  ncol_df1+2
  c1<-colnames(all_unmatch)[ncol_df1+2]
  all_unmatch<-all_unmatch[is.na(all_unmatch[,ncol_df1+2]),]
  
  #in DF2 but not in DF1
  all_unmatch2<-merge(df2,df1,by.y = colnames(key1),by.x=colnames(key2),all=TRUE)
  all_unmatch2<-all_unmatch2[is.na(all_unmatch2[,ncol_df2+2]),]
  
  
  if(rvalue=="EXACT"){
    return(exact_match)
  }
  else if(rvalue=="UNMATCH A TO B"){
    
    return(all_unmatch)
  }
  else if(rvalue=="UNMATCH B TO A"){
    
    return(all_unmatch2)
  }
  #Match Outputs: 
  
}

#Canonicalize for Constituency Names only 

# indian name canonicalization


canonicalize <- function(s) {
  
  trim <- function (s) { return (gsub ("\\s+$", "", gsub("^\\s+", "", s))) }
  # concats with space
  concat <- function(a, b) {
    return (paste(a, b))
  }
  x <- toupper (s)
  
  # remove all chars except A-Z and space. convert special chars like , to space, to retain tokenization
  #x <- gsub ("[^A-Z]", " ", x)
  x<-gsub("[^[:alnum:]]"," ",x)
  
  
  tokens <- strsplit(x, " ")[[1]] # this syntax needed because strsplit returns a list (of length 1)
  tokens <- sort (tokens)
  
  tokenNumber <- 1
  result <- ""
  for (token in tokens) {
    
    # token is already in uppercase
    token <- trim (token)
    token <- gsub ("[^[:alnum:]]", "", token)
    #Removing SC from canon name 
    if(token=="SC" | token=="AND" | token=="ST"){
      token=""
    }
    
    if (nchar(token) == 0) {
      next
    }
    
    # some common phonetic variations
    token <- gsub ("TH", "T", token)
    token <- gsub ("V", "W", token)
    token <- gsub ("GH", "G", token)
    token <- gsub ("BH", "B", token)
    token <- gsub ("DH", "D", token)
    token <- gsub ("JH", "J", token)
    token <- gsub ("KH", "K", token)
    token <- gsub ("MH", "M", token)
    token <- gsub ("PH", "P", token)
    token <- gsub ("SH", "S", token)
    token <- gsub ("ZH", "S", token)
    token <- gsub ("Z", "S", token)
    token <- gsub ("Y", "I", token)
    token <- gsub ("AU", "OU", token)
    token <- gsub ("OO", "U", token)
    token <- gsub ("EE", "I", token)
    token <- gsub ("KSH", "X", token)
    
    # repeated letters
    token <- gsub ("AA", "A", token)
    token <- gsub ("BB", "B", token)
    token <- gsub ("CC", "C", token)
    token <- gsub ("DD", "D", token)
    token <- gsub ("FF", "F", token)
    token <- gsub ("GG", "G", token)
    token <- gsub ("JJ", "J", token)
    token <- gsub ("KK", "K", token)
    token <- gsub ("LL", "L", token)
    token <- gsub ("MM", "M", token)
    token <- gsub ("NN", "N", token)
    token <- gsub ("PP", "P", token)
    token <- gsub ("RR", "R", token)
    token <- gsub ("SS", "S", token)
    token <- gsub ("TT", "T", token)
    token <- gsub ("WW", "W", token)
    token <- gsub ("YY", "Y", token)
    token <- gsub ("ZZ", "Z", token)
    
    
    # now a bunch of rules that are mostly true -- change these per requirement
    
    if (nchar(token) > 0) {
      result <- concat (result, token)
    }
    
    tokenNumber = tokenNumber + 1
  }
  return (trim(result)) # remove the unneeded space in the beginning
}

compare_canon<-function(df1, df2, key1,key2,rvalue){
  
  #REMOVE NA COLUMNS FROM DF1
  
  remcols1=data.frame(df1)
  k=0
  for(i in 1:ncol(remcols1)){
    if(k==0){
      if(all(is.na(remcols1[,i]))){
        k=1
        num=i
      }
    }
  }
  new_df1<-remcols1[,c(1:num-1)]
  
  #REMOVE NA COLUMNS FROM DF2
  remcols2=data.frame(df2)
  k=0
  for(i in 1:ncol(remcols2)){
    if(k==0){
      if(all(is.na(remcols2[,i]))){
        k=1
        num=i
      }
    }
  }
  new_df2<-remcols2[,c(1:num-1)]
  
  
  #add canonicalized values
  numcol_df1=ncol(new_df1)
  for(i in 1:nrow(new_df1)){
    new_df1[i,numcol_df1+1]=canonicalize(new_df1[i,1])
  }
  colnames(new_df1)[numcol_df1+1]="CANON"
  
  numcol_df2=ncol(new_df2)
  for(i in 1:nrow(new_df2)){
    new_df2[i,numcol_df2+1]=canonicalize(new_df2[i,1])
  }
  
  colnames(new_df2)[numcol_df2+1]="CANON"
  
  #EXACT MERGE
  mer_canon=merge(new_df1,new_df2,by="CANON")
  
  #Remaining Unmerged from A to B
  unmatched_AB=data.frame()
  j=1
  for(i in 1:nrow(new_df1)){
    if(!(new_df1$CANON[i] %in% mer_canon$CANON)){
      unmatched_AB[j,1]=new_df1[i,1]
      j=j+1
    }
  }
  colnames(unmatched_AB)[1]=colnames(key1)[1]
  mer_un_AB=unique(merge(unmatched_AB,df1,by=colnames(key1)[1]))
  
  #Remaining Unmerged from B to A
  unmatched_BA=data.frame()
  j=1
  for(i in 1:nrow(new_df2)){
    if(!(new_df2$CANON[i] %in% mer_canon$CANON)){
      unmatched_BA[j,1]=new_df2[i,1]
      j=j+1
    }
  }
  colnames(unmatched_BA)[1]=colnames(key1)[1]
  mer_un_BA=unique(merge(unmatched_BA,df2,by=colnames(key1)[1]))
  
  
  if(rvalue=="MERGED"){
    return(mer_canon)
  }
  else if(rvalue=="UNMERGED A TO B"){
    return(mer_un_AB)
  }
  else if(rvalue=="UNMERGED B TO A"){
    return(mer_un_BA)  
  }
  
}

compare_edit_dist<-function(df1, df2, key1,key2,rvalue){
  
  #REMOVE NA COLUMNS FROM DF1
  
  
  remcols1=data.frame(df1)
  k=0
  for(i in 1:ncol(remcols1)){
    if(k==0){
      if(all(is.na(remcols1[,i]))){
        k=1
        num=i
      }
    }
  }
  new_df1<-remcols1[,c(1:num-1)]
  
  #REMOVE NA COLUMNS FROM DF2
  remcols2=data.frame(df2)
  k=0
  for(i in 1:ncol(remcols2)){
    if(k==0){
      if(all(is.na(remcols2[,i]))){
        k=1
        num=i
      }
    }
  }
  new_df2<-remcols2[,c(1:num-1)]
  
  #add canonicalized values
  numcol_df1=ncol(new_df1)
  for(i in 1:nrow(new_df1)){
    new_df1[i,numcol_df1+1]=canonicalize(new_df1[i,1])
  }
  colnames(new_df1)[numcol_df1+1]="CANON"
  
  numcol_df2=ncol(new_df2)
  for(i in 1:nrow(new_df2)){
    new_df2[i,numcol_df2+1]=canonicalize(new_df2[i,1])
  }
  
  colnames(new_df2)[numcol_df2+1]="CANON"
  
  for(i in 1:nrow(new_df1)){
    #Ignoring LS Year 
    new_df1[i,numcol_df1+1]
    sp_f=str_locate(new_df1[i,numcol_df1+1]," ")
    temp=substr(new_df1[i,numcol_df1+1],sp_f+1,nchar(new_df1[i,numcol_df1+1]))
    for(j in 1:nrow(new_df2)){
      sp_f2=str_locate(new_df2[j,numcol_df2+1]," ")
      temp2=substr(new_df2[j,numcol_df2+1],sp_f2+1,nchar(new_df2[j,numcol_df2+1]))
      edist=adist(temp,temp2)
      if(edist==1 | edist==2){
        n_ls=substr(new_df1[i,numcol_df1+1],1,sp_f-1)
        n_eci=substr(new_df2[j,numcol_df2+1],1,sp_f2-1)
        if(n_ls==n_eci){
          new_df1[i,numcol_df1+2]="Matched"
          new_df1[i,numcol_df1+3]=new_df2[j,numcol_df2+1]
        }
        
      }
      
    }
    if(is.na(new_df1[i,numcol_df1+2])){
      new_df1[i,numcol_df1+2]="Unmatched"
    }
  }
  
  colnames(new_df1)[numcol_df1+2]="MATCH_INDICATOR"
  
  colnames(new_df1)[numcol_df1+3]="KEY"
  
  
  df1_sub<-subset(new_df1, MATCH_INDICATOR=="Matched")
  
  mer_ed_dist=merge(df1_sub,new_df2,by.x="KEY",by.y="CANON")
  
  #to find unmerged from B To A
  unmerged_edist_ba=data.frame()
  j=1
  num=ncol(new_df2)
  for(i in 1:nrow(new_df2)){
    if(!(new_df2$CANON[i] %in% df1_sub$KEY)){
      unmerged_edist_ba[j,1]=new_df2$CANON[i]
      j=j+1
    }
  }
  
  colnames(unmerged_edist_ba)[1]=colnames(new_df2)[num]
  unm_ba=merge(unmerged_edist_ba,new_df2,by=colnames(new_df2)[num])
  
  if(rvalue=="MERGED"){
    return(mer_ed_dist)
  }
  else if(rvalue=="UNMERGED A TO B"){
    return(subset(new_df1, MATCH_INDICATOR=="Unmatched"))
  }
  else if(rvalue=="UNMERGED B TO A"){
    return(unm_ba)  
  }
  
}
canonicalize1 <- function(s) {
  trim <- function (s) { return (gsub ("\\s+$", "", gsub("^\\s+", "", s))) }
  # concats with space
  concat <- function(a, b) {
    return (paste(a, b))
  }
  
  x <- toupper (s)
  
  # remove all chars except A-Z and space. convert special chars like , to space, to retain tokenization
  #x <- gsub ("[^A-Z]", " ", x)
  x<-gsub("[^[:alnum:]]"," ",x)
  
  tokens <- strsplit(x, " ")[[1]] # this syntax needed because strsplit returns a list (of length 1)
  tokens <- sort (tokens)
  
  tokenNumber <- 1
  result <- ""
  for (token in tokens) {
    # token is already in uppercase
    token <- trim (token)
    token <- gsub ("[^A-Z]", "", token)
    if (nchar(token) == 0) {
      next
    }
    
    # these titles are simply dropped
    if (token == "DR" | token == "MR" | token == "PROF" | token == "MRS" | token == "ENG" | token == "SHRI" | token == "SMT" | token == "SHRI" | token == "SARDAR" | token == "PANDIT" || token == "PT" || token == "THIRU")
      next
    
    # some common phonetic variations
    token <- gsub ("TH", "T", token)
    token <- gsub ("V", "W", token)
    token <- gsub ("GH", "G", token)
    token <- gsub ("BH", "B", token)
    token <- gsub ("DH", "D", token)
    token <- gsub ("JH", "J", token)
    token <- gsub ("KH", "K", token)
    token <- gsub ("MH", "M", token)
    token <- gsub ("PH", "P", token)
    token <- gsub ("SH", "S", token)
    token <- gsub ("ZH", "S", token)
    token <- gsub ("Z", "S", token)
    token <- gsub ("Y", "I", token)
    token <- gsub ("AU", "OU", token)
    token <- gsub ("OO", "U", token)
    token <- gsub ("EE", "I", token)
    token <- gsub ("KSH", "X", token)
    
    # repeated letters
    token <- gsub ("AA", "A", token)
    token <- gsub ("BB", "B", token)
    token <- gsub ("CC", "C", token)
    token <- gsub ("DD", "D", token)
    token <- gsub ("FF", "F", token)
    token <- gsub ("GG", "G", token)
    token <- gsub ("JJ", "J", token)
    token <- gsub ("KK", "K", token)
    token <- gsub ("LL", "L", token)
    token <- gsub ("MM", "M", token)
    token <- gsub ("NN", "N", token)
    token <- gsub ("PP", "P", token)
    token <- gsub ("RR", "R", token)
    token <- gsub ("SS", "S", token)
    token <- gsub ("TT", "T", token)
    token <- gsub ("WW", "W", token)
    token <- gsub ("YY", "Y", token)
    token <- gsub ("ZZ", "Z", token)
    
    # now a bunch of rules that are mostly true -- change these per requirement
    
    token <- gsub ("PD", "PRASAD", token)
    
    token <- gsub ("MD", "MOHAMMAD", token)
    token <- gsub ("MOHAMAD", "MOHAMMAD", token)
    token <- gsub ("MOHMED", "MOHAMMAD", token)
    token <- gsub ("MOHAMED", "MOHAMED", token)
    
    # according to Gilles, Ku being the first token implies KUNWAR, otherwise KUMAR
    if (tokenNumber == 1) {
      token <- gsub("^KU$", "KUNWAR", token)
    } else {
      token <- gsub("^KU$", "KUMAR", token)
    }
    
    # replace suffixes at the end of the token (indicated by $)
    token <- gsub ("BHAI$", "", token)
    token <- gsub ("BEN$", "", token)
    token <- gsub ("BAI$", "", token)
    token <- gsub ("JI$", "", token)
    
    if (nchar(token) > 0) {
      result <- concat (result, token)
    }
    
    tokenNumber = tokenNumber + 1
  }
  return (trim(result)) # remove the unneeded space in the beginning
}


