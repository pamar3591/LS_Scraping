library(RCurl)
library(XML)
library(dplyr)
library(rvest)
library(stringr)
library(plyr)

#importing constituency & candidate level data

constituency_electoral_info<-read.csv('constituency_electoral_info.csv')
candidates_electoral_info<-read.csv('candidates_electoral_info.csv')



#Getting the Lok Sabha Data List Alphabetic Member's list from 'Members since 1952' on LS Site

ls_list<- data.frame()
curl = getCurlHandle()
alphalist<-list()
alphalist <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

#Retrieve A-Z List
for (i in 1:length(alphalist)){
  url = 'http://164.100.47.194/Loksabha/Members/lokprev.aspx?search='
  url = paste(url, alphalist[i], sep = "")
  html = getURL (url, curl = curl)
  curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
  viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
  viewstategenerator <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
  eventvalidation = as.character(sub('.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
  
  params <- list('__EVENTTARGET' = 'ctl00$ContentPlaceHolder1$drdpages',
                 '__VIEWSTATE' = viewstate, 
                 '__VIEWSTATEGENERATOR' = viewstategenerator, 
                 '__VIEWSTATEENCRYPTED' = '',
                 '__EVENTVALIDATION' = eventvalidation, 
                 'ctl00$ContentPlaceHolder1$drdpages' = '5000'
  )
  html = postForm (url, .params=params, curl = curl)
  doc = htmlParse(html, asText=TRUE)
  p1<-xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[2]', xmlValue)
  p2<-as.data.frame(p1)
  p2<-mutate_all(p2, funs(trimws))
  p3<-xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[3]', xmlValue)
  p4<-as.data.frame(p3)
  p4<-mutate_all(p4, funs(trimws))
  p5<-xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[4]', xmlValue)
  p6<-as.data.frame(p5)
  p6<-mutate_all(p6, funs(trimws))
  p7<-xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[5]', xmlValue)
  p8<-as.data.frame(p7)
  p8<-mutate_all(p8, funs(trimws))
  
  list_alpha<-cbind(p2,p4,p6,p8)
  colnames(list_alpha)<-c("Name_Of_Member", "Party_Name","Constituency_State","Lok_Sabha_Experience")
  list_alpha<-list_alpha[-1,]
  
  hrefs <-xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[2]/a', xmlGetAttr,'href')
  href_data_frame<-data.frame()
  for(j in 1:length(hrefs)){
    href_data_frame[j,1]<-hrefs[j]
  }
  
  list_alpha<-cbind(list_alpha, href_data_frame)
  ls_list<-rbind(ls_list,list_alpha)
  print(i)
}
colnames(ls_list)[5]="Links"


#retrieve scraped data for members extracted from LS alphabetic list of all members since 1952

scraped_data<-data.frame()
for(i in 563:nrow(ls_list)){
  l=ls_list$Links[i]
  link<-paste0("http://164.100.47.194/Loksabha/Members/",l)
  h=""
  tctch = tryCatch(
    {
    read_html(link,warn=FALSE)
 
    },
    error=function(cond){
      message("Error Occured")
      return(NA)
    },
    warning=function(cond){
      message("Warning Occured")
      return(NA)
    }
    )
  if(is.na(tctch)){
    scraped_data[i,1]="Page Error"
  }else{
    
    l1=read_html(link)
    
    nod1 = tryCatch(
      {
        html_nodes(l1,css='font , #Label4 b')
      },
      error=function(cond){
        message("Error within")
        return(NA)
      }
      )
    if(!length(nod1)==0){
      if(!is.na(nod1[1][1])){
        l3<-html_text(nod1)
        
        for(j in 1:length(l3)){
          h=paste(h,l3[j]) 
        }
        scraped_data[i,1]<-h
      }else{
        scraped_data[i,1]="Page Error"
      }
    }
    else{
      {scraped_data[i,1]="Page Error"}
    }
  }
  print(i)
}


colnames(scraped_data)[1]<-"Scraped Data"



#Binding the Scraped Data with the LS Alphabetic List


ls_list=cbind(ls_list,scraped_data)


#SCraping from LS Years - Lists for Each LS Number : LS01 to LS15 
#This List includes bye-elections as well 

ls_number_list=data.frame()

for(i in 1:15){
  link1<-paste0("http://164.100.47.194/Loksabha/Members/lokaralpha.aspx?lsno=",i)
  h = read_html(link1)
  h1 = html_nodes(h,css='#ContentPlaceHolder1_tblMember')
  h2 = data.frame(html_table(h1,fill=TRUE))
  n=nrow(h2)-1
  h2=h2[c(2:n),c(1,2,3,4)]
  h2$LSNum=i
  ls_number_list=rbind(ls_number_list,h2)
  print(i)
  
  
}

colnames(ls_number_list)=c("SI_NO","Member_Name","Party_Name","Const_State","LSNum")

#Cleaning ls_number_list 

final_ls_list=data.frame()
k=1
c4=0
for(i in 1:nrow(ls_number_list)){
  if(ls_number_list[i,1]=="Sl. No."){
    c4=c4+1
    next
  }
  for(j in 1:ncol(ls_number_list)){
    final_ls_list[k,j]=ls_number_list[i,j]
  }
  
  k=k+1
}
colnames(final_ls_list)=c("SI_NO","Member_Name","Party_Name","Const_State","LSNum")

#final_ls_list=final_ls_list[!(final_ls_list$LSNum==1 | final_ls_list$LSNum==2),]

#adding LS 16 Data 

ls_16=read_html('http://164.100.47.194/Loksabha/Members/AlphabeticalList.aspx')
ls_16_nodes=html_nodes(ls_16,css='#ContentPlaceHolder1_tblMember')
ls_16_table=data.frame(html_table(ls_16_nodes,fill=TRUE))
n1=nrow(ls_16_table)
colnames(ls_16_table)=ls_16_table[2,]
ls_16_table=ls_16_table[c(3:n1),c(1,2,3,4)]
ls_16_table$LSNum=16
colnames(ls_16_table)=colnames(final_ls_list)

#Combining LS01-03 Data with LS16 Data

final_ls_list=rbind(final_ls_list,ls_16_table)

#Get Dead & Resigned Member Data
ls_16_died_resigned=read_html('http://164.100.47.194/Loksabha/Members/MembersDied_Resigned.aspx')
ls_16_r=html_nodes(ls_16_died_resigned,css='#pnlDiv1 > table')
ls_16_d=html_nodes(ls_16_died_resigned,css='#pnlDiv2 > table')
ls_16_rtable=data.frame(html_table(ls_16_r))
ls_16_dtable=data.frame(html_table(ls_16_d))

#Extraction of all Nominated Members: 
n_link="http://164.100.47.194/Loksabha/Members/nominated1to12.aspx?"
nom_member_list=data.frame()
for(i in 1:15){
  lsno=i
  tab=15-i
  a=paste0(n_link,"lsno=",lsno,"&tab=",tab)
  a1=read_html(a)
  a2=html_nodes(a1,css='#ContentPlaceHolder1_tblMember')
  a3=data.frame(html_table(a2,fill=TRUE))
  n=nrow(a3)
  colnames(a3)=a3[2,]
  a3=a3[c(-1,-2,-n),c(1,2,3,4)]
  a3$LSNum=lsno
  nom_member_list=rbind(nom_member_list,a3)

}

#extract nominated member for current ls from this page: 
nom_ls16="http://164.100.47.194/Loksabha/Members/nominated.aspx"
a1_16=read_html(nom_ls16)
a2_16=html_nodes(a1_16,css='#prints > div > table')
a3_16=data.frame(html_table(a2_16,fill=TRUE))
n_16=nrow(a3_16)
colnames(a3_16)=a3_16[1,]
a3_16=a3_16[c(-1),c(1,2,3,4)]
a3_16$LSNum=16
colnames(a3_16)=colnames(nom_member_list)
nom_member_list=rbind(nom_member_list,a3_16)



#Creating Member_Name_Key by removing spaces
for(i in 1:nrow(final_ls_list)){
  nm=final_ls_list$Member_Name[i]
  nm=str_replace_all(nm," ","")
  final_ls_list$Member_Name_Key[i]=nm
}

for(i in 1:nrow(ls_list)){
  nm=ls_list$Name_Of_Member[i]
  nm=str_replace_all(nm," ","")
  ls_list$Member_Name_Key[i]=nm
}


c1=as.data.frame(table(ls_list$Member_Name_Key))
colnames(c1)=c("Member_Name_Key","freq")
merge_showcount=merge(x=final_ls_list,y=c1,by="Member_Name_Key",all.x=TRUE)
merge_showcount=merge(x=merge_showcount, y=ls_list,by="Member_Name_Key",all.x=TRUE)


#CODE TO GET MISSING MEMBERS DATA

missing_members=merge_showcount[is.na(merge_showcount$freq),]
missing_member_names=unique(data.frame((missing_members$Member_Name)))

require(gdata)
require(dplyr)
require(RCurl)
require(XML)

df = missing_member_names

cleannames <- function(x){
  x <- gsub("Shri |Dr |Kum |Ms |Mrs |Mr "," ",x)
  x <- gsub("[[:punct:]]"," ",x)
  x <- trimws(x)
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE)
  return(x)
}
df <- mutate_all(df, cleannames)
missing_list=data.frame()
for (i in 1:nrow(df)){
  y <- df[[1]][i]
  curl = getCurlHandle()
  url = 'http://164.100.47.194/Loksabha/Members/lokprev.aspx'
  html = getURL (url, curl = curl)
  curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
  viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
  viewstategenerator <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
  eventvalidation = as.character(sub('.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
  
  params <- list('__EVENTTARGET' = 'ctl00$ContentPlaceHolder1$drdpages',
                 '__EVENTARGUMENT' = '',
                 '__LASTFOCUS' = '',
                 '__VIEWSTATE' = viewstate, 
                 '__VIEWSTATEGENERATOR' = viewstategenerator, 
                 '__VIEWSTATEENCRYPTED' = '',
                 '__EVENTVALIDATION' = eventvalidation, 
                 'ctl00$ContentPlaceHolder1$txtSearch' = y,
                 'ctl00$txtSearchGlobal' = '',
                 'ctl00$ContentPlaceHolder1$btnSearch' = '',
                 'ctl00$ContentPlaceHolder1$member' = 'rdbtnName',
                 'ctl00$ContentPlaceHolder1$ddlfile' = '.pdf',
                 'ctl00$ContentPlaceHolder1$ddlstate' = '--Select State--',
                 'ctl00$ContentPlaceHolder1$ddlparty' = '--Select Party--',
                 'ctl00$ContentPlaceHolder1$txtPageNo' = '',
                 'ctl00$ContentPlaceHolder1$drdpages' = '10',
                 'ctl00$ContentPlaceHolder1$hidTableCount' = ''
  )
  html = postForm (url, .params=params, curl = curl)
  doc = htmlParse(html, asText=TRUE)
  
  a2=data.frame(xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[2]', xmlValue))
  if(nrow(a2)==0){
    t1=str_locate_all(y," ")
    l1=length(t1[[1]])
    l2=t1[[1]][l1]
    t2=str_sub(y,1,l2-1)
    y=t2
    curl = getCurlHandle()
    url = 'http://164.100.47.194/Loksabha/Members/lokprev.aspx'
    html = getURL (url, curl = curl)
    curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
    viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
    viewstategenerator <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
    eventvalidation = as.character(sub('.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
    
    params <- list('__EVENTTARGET' = 'ctl00$ContentPlaceHolder1$drdpages',
                   '__EVENTARGUMENT' = '',
                   '__LASTFOCUS' = '',
                   '__VIEWSTATE' = viewstate, 
                   '__VIEWSTATEGENERATOR' = viewstategenerator, 
                   '__VIEWSTATEENCRYPTED' = '',
                   '__EVENTVALIDATION' = eventvalidation, 
                   'ctl00$ContentPlaceHolder1$txtSearch' = y,
                   'ctl00$txtSearchGlobal' = '',
                   'ctl00$ContentPlaceHolder1$btnSearch' = '',
                   'ctl00$ContentPlaceHolder1$member' = 'rdbtnName',
                   'ctl00$ContentPlaceHolder1$ddlfile' = '.pdf',
                   'ctl00$ContentPlaceHolder1$ddlstate' = '--Select State--',
                   'ctl00$ContentPlaceHolder1$ddlparty' = '--Select Party--',
                   'ctl00$ContentPlaceHolder1$txtPageNo' = '',
                   'ctl00$ContentPlaceHolder1$drdpages' = '10',
                   'ctl00$ContentPlaceHolder1$hidTableCount' = ''
    )
    html = postForm (url, .params=params, curl = curl)
    doc = htmlParse(html, asText=TRUE)
    a2=data.frame(xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[2]', xmlValue))
    
  }
  
  if(!nrow(a2)==0){
  for(n in 1:nrow(a2)){
    name=a2[n,1]
    name=trimws(name)
    clean_name=cleannames(name)
    if(clean_name==y){
      break
    }
  }
  a2=trimws(a2[n,1])
  a3=data.frame(xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[3]', xmlValue))
  a3=trimws(a3[n,1])
  a4=data.frame(xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[4]', xmlValue))
  a4=trimws(a4[n,1])
  a5=data.frame(xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[5]', xmlValue))
  a5=trimws(a5[n,1])
  
  href1 <-data.frame(xpathSApply(doc, '//*[contains(@class, "member_list_table")]//td[2]/a', xmlGetAttr,'href'))
  if(n>2){
  href1=trimws(href1[n,1])
  }
  mvals=cbind(a2,a3,a4,a5,href1)
  colnames(mvals)=c("Member_Name","Party_Name","Constituency_State","Lok_Sabha_Exp","Link")
  missing_list=rbind(missing_list,mvals)
  }
  print(i)
}

#Scraping the data for the missing members 

scraped_data2<-data.frame()
for(i in 1:nrow(missing_list)){
  l=missing_list$Link[i]
  link<-paste0("http://164.100.47.194/Loksabha/Members/",l)
  h=""
  try(l1<-read_html(link))
  if(length(l1)>1){
    l2<-html_nodes(l1,css='font , #Label4 b')
    l3<-html_text(l2)
    
    for(j in 1:length(l3)){
      h=paste(h,l3[j]) 
    }
  }
  scraped_data2[i,1]<-h
  print(i)
}
colnames(scraped_data2)[1]<-"Scraped Data"
missing_list=cbind(missing_list,scraped_data2)
colnames(missing_list)=c("Member_Name","Party_Name","Constituency_State","Lok_Sabha_Exp","Link","Scraped_Data")
missing_list$Member_Name_Key=str_replace_all(missing_list$Member_Name," ","")

colnames(missing_list)=colnames(ls_list)
final_missing_list=rbind(ls_list,missing_list)

#CHECK FOR SAME SCRAPED DATA IN CONSEC ROWS

colnames(final_missing_list)[6]="Scraped_Data"
f2=final_missing_list
nr=nrow(f2)
j=1
cop=data.frame()
cop[1,1]="NOT COPIED"
for(i in 2:nr){
  if(f2$Scraped_Data[i]==final_missing_list$Scraped_Data[j]){
    cop[i,1]="COPIED"
  }else{
    cop[i,1]="NOT COPIED"
  }
  j=j+1
  print(i)
}
colnames(cop)="DUPLICATE_CHECK"

f2=cbind(f2,cop)

final_missing_list=f2

c1=plyr::count(final_missing_list,'Member_Name_Key')

merge_final=merge(x=final_ls_list,y=c1,by="Member_Name_Key",all.x=TRUE)

merge_final=merge_final[!is.na(merge_final$freq),]

merge_final=merge(x=merge_final, y=final_missing_list,by="Member_Name_Key",all.x=TRUE)
write.csv(merge_final,"merged_data.csv", row.names=FALSE)
#merge_final=merge_final[!(merge_final$LSNum==1 | merge_final$LSNum==2),]

#Removing incorrect merges for candidates with multiple pages: Check if the LSNum is a subset of the LS Experience

final_merge=data.frame()
k=1
delrow=0
deldata=data.frame()
for(i in 1:nrow(merge_final)){
  f=merge_final$freq[i]
  if(f==1){
    for(j in 1:ncol(merge_final)){
      final_merge[k,j]=merge_final[i,j]
    }
    k=k+1
  }else{
    s1=as.character(merge_final$LSNum[i])
    s2=as.character(merge_final$Lok_Sabha_Experience[i])
    ch=str_detect(s2,s1)
    
    if(ch==TRUE){ #& ch2==TRUE){  OR RAM SAGAR BARA BANKI Handle1){
      for(j in 1:ncol(merge_final)){
        final_merge[k,j]=merge_final[i,j]
      }
      k=k+1
    }else{
      delrow=delrow+1 
      for(j in 1:ncol(merge_final)){
        deldata[delrow,j]=merge_final[i,j]
      }
    }
  }
  print(i)
}
colnames(deldata)=colnames(merge_final)
colnames(final_merge)=colnames(merge_final)

un_names=data.frame(paste0(final_merge$Member_Name_Key,final_merge$LSNum))
colnames(un_names)[1]="Key"
un = unique(un_names)
c3=plyr::count(un_names,'Key')
#Clean this list using logic that = extract state names - if state name in both isnt the same then remove (eg. Dalbir Singh)
for(i in 1:nrow(final_merge)){
  c1=final_merge$Const_State[i]
  l=str_locate(c1,"\\(")
  c1=str_sub(c1,1,l[[1]]-1)
  c1=str_trim(c1)
  final_merge$C1[i]=c1
  
  c2=final_merge$Constituency_State[i]
  l2=str_locate(c2,'\\(')
  c2=str_sub(c2,1,l2[[1]]-1)
  c2=str_trim(c2)
  final_merge$C2[i]=c2
  
}
final=data.frame()
temp=data.frame()
for(i in 1:nrow(final_merge)){
  name=final_merge$Member_Name_Key[i]
  lsn=final_merge$LSNum[i]
  if( (name=="DalbirSingh,Shri" & lsn==7) | (name=="DalbirSingh,Shri" & lsn==8) | (name=="RamSagar,Shri" & lsn==9) | (name=="Gandhi,ShriManeklalMaganlal" & lsn==1) | (name=="Hedaoo,ShriRam" & lsn==5) | (name=="Shivananjappa,ShriM.K." & lsn==3) ){
    if(!(final_merge$C1[i]==final_merge$C2[i])){
      print("1")
      next
    }
  }
  temp=final_merge[i,]
  final=rbind(final,temp)
}


req_ls_data=final[,c(1,3,4,5,6,7,12,13)]


#Adding NA Indicator

na_ind=data.frame()
for(i in 1:nrow(req_ls_data)){
  caps=tryCatch(
    {
      toupper(req_ls_data$Scraped_Data[i])
    },
    error=function(cond){
      message=("Error")
      return(NA)
    }
  )
  if(!is.na(caps)){
  loc<-str_locate(caps,"NOT AVAIL")
  if(is.na(loc[1,1])){
    na_ind[i,1]="Available"
   }else{
    na_ind[i,1]="Not Available"
   }
  }else{
    na_ind[i,1]="Available"
  }
}
colnames(na_ind)="NA INDICATOR"
req_ls_data=cbind(req_ls_data,na_ind)


#Adding Column for MPS_Number
mpsno<-data.frame()
for(i in 1:nrow(req_ls_data)){
  addr<-req_ls_data$Links[i]
  sub_mpsno<-str_sub(addr,start=str_locate(addr,"=")[1,1]+1,end=str_locate(addr,"&")[1,1]-1)
  mpsno[i,1]<-sub_mpsno
}
colnames(mpsno)[1]<-"MPS_No"
req_ls_data=cbind(req_ls_data,mpsno)

#Separating State Name & Constituency Name

const_st=data.frame(req_ls_data$Const_State)
colnames(const_st)=c("CONST_ST")

constcol<-data.frame()
stcol<-data.frame()

for(i in 1:nrow(const_st)){
  #constituency names
  s<-const_st[i,1]
  s=trimws(s)
  if(s==""){
    next
  }
  len<-str_length(s)
  stc<-str_locate(s,"\\(")
  constname<-str_sub(s,start=1,end=stc[1,1]-2)
  constcol[i,1]<-trimws(constname)
  #state names
  lastc<-str_locate(s,"\\)")
  stname<-str_sub(s,start=stc[1,1]+1,end=lastc[1,1]-1)
  #check if district is SC or ST
  if(stname=="SC" | stname=="ST"){
    s1<-str_sub(s,start=lastc[1,1]+1)
    newstc<-str_locate(s1,"\\(")
    newlastc<-str_locate(s1,"\\)")
    nstname<-str_sub(s1,start=newstc[1,1]+1,end=newlastc[1,1]-1)
    stcol[i,1]<-toupper(trimws(nstname))
    next
  }
  stcol[i,1]<-toupper(trimws(stname))
  print(i)
}
for(i in 1:nrow(constcol)){
  constcol[i,1]=str_replace_all(constcol[i,1],"-SC","")
  constcol[i,1]=str_replace_all(constcol[i,1],"-ST","")
  }
colnames(constcol)[1]="CONSTITUENCY"
colnames(stcol)[1]="STATE_NAME"

req_ls_data=cbind(req_ls_data,constcol,stcol)

state_norm=data.frame()
comp_state=data.frame(req_ls_data$STATE_NAME)
colnames(comp_state)="NAME"
for(i in 1:nrow(comp_state)){
  if(is.na(comp_state$NAME[i])){
    state_norm[i,1]="NA"
  }else if(comp_state$NAME[i]=="NCT OF DELHI"){
    state_norm[i,1]="DELHI"
  }else{
    st=comp_state$NAME[i]
    st_name=comp_state$NAME[i]
    st_name=trimws(st_name)
    #getting space position
    spfind<-str_locate(st_name," ")
    if(!is.na(spfind[1,1])){
      state_norm[i,1]=str_replace_all(st_name," ","_")
    }
    else{
      state_norm[i,1]=as.character(comp_state$NAME[i])
    }
  }
}
colnames(state_norm)[1]="Correct_State"

#Remove AND 
for(i in 1:nrow(state_norm)){
  state_norm$Correct_State[i]=str_replace(state_norm$Correct_State[i],"_AND_","_&_")
}
req_ls_data=cbind(req_ls_data,state_norm)
req_ls_data$CONSTITUENCY=toupper(req_ls_data$CONSTITUENCY)
req_ls_data$ECI_KEY=paste0(req_ls_data$LSNum,"_",req_ls_data$Correct_State,"_",req_ls_data$CONSTITUENCY)

for(i in 1:nrow(req_ls_data)){
  stname<-req_ls_data$Correct_State[i]
  if(stname=="PONDICHERRY"){
    req_ls_data$Correct_State[i]="PUDUCHERRY"
    req_ls_data$ECI_KEY[i]=paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i])
  }
  if(stname=="DADRA_&_NAGAR_HAVELI"){
    req_ls_data$Correct_State[i]="DADRA_NAGAR_&_HAVELI"
    req_ls_data$ECI_KEY[i]=paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i])
  }
  if(stname=="LACCADIVES,_MINICOY_&_AMINDIVI_ISLANDS"){
    req_ls_data$Correct_State[i]="LAKSHADWEEP"
    req_ls_data$ECI_KEY[i]=paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i])
  }
  if(stname=="GOA" & req_ls_data$LSNum[i]==8){
    req_ls_data$Correct_State[i]="GOA,_DAMAN_&_DIU"
    req_ls_data$ECI_KEY[i]=paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i])
  }
}


st_ls_16=list("TELANGANA")
st_eci_16=list("ANDHRA_PRADESH")
st_ls_13=list("JHARKHAND","CHHATTISGARH","UTTARAKHAND")
st_eci_13=list("BIHAR","MADHYA_PRADESH","UTTAR_PRADESH")

for(i in 1:nrow(req_ls_data)){
  st_n<-req_ls_data$Correct_State[i]
  if(!is.na(st_n)){
    if(!(req_ls_data$LSNum[i]==14|req_ls_data$LSNum[i]==15|req_ls_data$LSNum[i]==16 )){
      for(k in 1:length(st_ls_13)){
      if(st_n==as.character(st_ls_13[k])){
        req_ls_data$Correct_State[i]=as.character(st_eci_13[k])
        req_ls_data$ECI_KEY[i]=as.character(paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i]))
      }
    }
    }
  if(req_ls_data$LSNum[i]==16){
    st_n<-req_ls_data$Correct_State[i]
    for(k in 1:length(st_ls_16)){
      if(st_n==as.character(st_ls_16[k])){
        req_ls_data$Correct_State[i]=as.character(st_eci_16[k])
        req_ls_data$ECI_KEY[i]=as.character(paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i]))
      }
    }
  }
  if(st_n=="ORISSA"){
    req_ls_data$Correct_State[i]="ODISHA"
    req_ls_data$ECI_KEY[i]=as.character(paste0(req_ls_data$LSNum[i],"_",req_ls_data$Correct_State[i],"_",req_ls_data$CONSTITUENCY[i]))
  }
    
  }
}


#PREPARING CONSTITUENCY INFO DATA (here)

constituency_electoral_info<-mutate_all(constituency_electoral_info,funs(toupper))
constituency_electoral_info<-mutate_all(constituency_electoral_info,funs(trimws))
cei_key<-data.frame()
for(i in 1:nrow(constituency_electoral_info)){
  cei_key[i,1]<-trimws(paste0(constituency_electoral_info$ga_no[i],"_",constituency_electoral_info$State_name[i],"_",constituency_electoral_info$PC_name[i]))
}

colnames(cei_key)[1]<-"ECI_KEY"
constituency_electoral_info<-cbind(constituency_electoral_info,cei_key)
req_ls_data$ECI_KEY=trimws(req_ls_data$ECI_KEY)

ls_key=data.frame(req_ls_data$ECI_KEY)
colnames(ls_key)[1]="ECI_KEY"
const_eci_key=data.frame(constituency_electoral_info$ECI_KEY)
colnames(const_eci_key)[1]="ECI_KEY"

#---start function processing from here-----

#Get Exact Values
ex=compare(req_ls_data,constituency_electoral_info,ls_key,const_eci_key,"EXACT")

final_ex=ex[,c("ECI_KEY","Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","NA INDICATOR","MPS_No","CONSTITUENCY","Correct_State","State_name","Year","PC_no","PC_name","PC_type","Electors","ga_no","poll_no")]

final_ex$MergedBy="Direct"

#unmerged after exact merge
unmerged_A_to_B=compare(req_ls_data,constituency_electoral_info,ls_key,const_eci_key,"UNMATCH A TO B")

unmerged_B_to_A=compare(req_ls_data,constituency_electoral_info,ls_key,const_eci_key,"UNMATCH B TO A")

unmerged_ls_key=data.frame(unmerged_A_to_B$ECI_KEY)
unmerged_eci_key=data.frame(unmerged_B_to_A$ECI_KEY)
colnames(unmerged_ls_key)[1]="ECI_KEY"
colnames(unmerged_eci_key)[1]="ECI_KEY"

#Calling Canon Function 

#MERGED
mer_canon=compare_canon(unmerged_A_to_B,unmerged_B_to_A,unmerged_ls_key,unmerged_eci_key,"MERGED")

final_can=mer_canon[,c("ECI_KEY.x","Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","NA.INDICATOR","MPS_No","CONSTITUENCY","Correct_State","State_name","Year","PC_no","PC_name","PC_type","Electors","ga_no","poll_no")]

final_can$MergedBy="Canonicalization"



unmer_canon_ab=compare_canon(unmerged_A_to_B,unmerged_B_to_A,unmerged_ls_key,unmerged_eci_key,"UNMERGED A TO B")


unmer_canon_ba=compare_canon(unmerged_A_to_B,unmerged_B_to_A,unmerged_ls_key,unmerged_eci_key,"UNMERGED B TO A")

#Calling Edit Distance

ed_ab_key<-data.frame(unmer_canon_ab$ECI_KEY)
ed_ba_key<-data.frame(unmer_canon_ba$ECI_KEY)

#MERGED
mer_ed=compare_edit_dist(unmer_canon_ab,unmer_canon_ba,ed_ab_key, ed_ba_key,"MERGED")

final_ed=mer_ed[,c("ECI_KEY.x","Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","NA.INDICATOR","MPS_No","CONSTITUENCY","Correct_State","State_name","Year","PC_no","PC_name","PC_type","Electors","ga_no","poll_no")]

final_ed$MergedBy="Edit Distance"

unmer_ed_ab=compare_edit_dist(unmer_canon_ab,unmer_canon_ba,ed_ab_key, ed_ba_key,"UNMERGED A TO B")

unmer_ed_ba=compare_edit_dist(unmer_canon_ab,unmer_canon_ba,ed_ab_key, ed_ba_key,"UNMERGED B TO A")

#Adding Compatibility
numls=ncol(unmer_ed_ab)
unmerls=unmer_ed_ab[,c(-numls)]
unmereci=unmer_ed_ba

new_mer=data.frame()
unmer_fin=data.frame()
for(i in 1:nrow(unmerls)){
  k=0
  lstemp=trimws(as.character(unmerls$CONSTITUENCY[i]))
  if(!is.na(lstemp)){
    for(j in 1:nrow(unmereci)){
    ecitemp=as.character(unmereci$PC_name[j])
    compat=compatibility(lstemp,ecitemp)
    #if(compat>0 & (unmerls$CONSTITUENCY[i]=="RAEBARELI" | unmerls$CONSTITUENCY[i]=="BARABANKI" | unmerls$CONSTITUENCY[i]=="COOCHBEHAR" | unmerls$ConsName[i]=="NAINITAL" | unmerls$ConsName[i]=="TELLICHERRY" | unmerls$ConsName[i]=="HAZARIBAGH")){
    if(compat>0 & (lstemp=="RAEBARELI" | lstemp=="BARABANKI" | lstemp=="COOCHBEHAR" | lstemp=="NAINITAL" | lstemp=="TELLICHERRY" | lstemp=="HAZARIBAGH")){
      new_mer[i,1]=ecitemp
      k=1
      }
    }
  if(k==0){
    unmer_fin=rbind(unmer_fin,unmerls[i,])
    }
  }
}


unmerls=cbind(unmerls,new_mer)

mod_eci_key=unmerls[!is.na(unmerls$V1),]

#REPLACE CONST NAMES FOR THESE 
mod_eci_key$newkey=paste0(mod_eci_key$LSNum,"_",mod_eci_key$Correct_State,"_",mod_eci_key$V1)

keyrep=mod_eci_key[,c("ECI_KEY","newkey")]
for(i in 1:nrow(unmer_ed_ab)){
  for(j in 1:nrow(keyrep))
    if(unmer_ed_ab$ECI_KEY[i]==keyrep$ECI_KEY[j]){
      unmer_ed_ab$ECI_KEY[i]=keyrep$newkey[j]
      unmer_ed_ab$KEY[i]="Changed"
    }
}

mer_compat=subset(unmer_ed_ab,KEY=="Changed")
mer_compat=join(mer_compat,unmer_ed_ba,by="ECI_KEY")
mer_compat$MATCH_INDICATOR="Matched after Compatibility"
final_compat=mer_compat[,c("ECI_KEY","Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","NA.INDICATOR","MPS_No","CONSTITUENCY","Correct_State","State_name","Year","PC_no","PC_name","PC_type","Electors","ga_no","poll_no")]
final_compat$MergedBy="Canonicalization"


unmereci$MOD="NA"
for(i in 1:nrow(unmereci)){
  if(unmereci$ECI_KEY[i] %in% keyrep$newkey){
    unmereci$MOD[i]="Matched"
  }
}


#GETTING ECI UNMATCHED
unmer_compat_ba=unmereci[unmereci$MOD=="NA",]
config_ls_key=c("10_ASSAM_GUWAHATI","10_KERALA_PALAKKAD","10_KERALA_THIRUVANANTHAPURAM","10_PUNJAB_JALANDHAR","11_ASSAM_GUWAHATI","11_PUNJAB_JALANDHAR","12_KERALA_THIRUVANANTHAPURAM","13_TAMIL_NADU_CHENNAI CENTRAL","13_TAMIL_NADU_CHENNAI NORTH","13_TAMIL_NADU_CHENNAI SOUTH","14_TAMIL_NADU_CHENNAI CENTRAL","14_TAMIL_NADU_CHENNAI NORTH","14_TAMIL_NADU_CHENNAI SOUTH","15_WEST_BENGAL_BARDHAMAN-DURGAPUR","15_WEST_BENGAL_SERAMPORE","16_WEST_BENGAL_BARDHAMAN-DURGAPUR","16_WEST_BENGAL_SERAMPORE","3_MAHARASHTRA_BOMBAY  CENTRAL NORTH","3_MAHARASHTRA_BOMBAY CENTRAL SOUTH","3_MAHARASHTRA_YAVATMAL","4_ANDAMAN_&_NICOBAR_ISLANDS_ANDAMAN AND NICOBAR ISLANDS","4_ASSAM_AUTONOMOUS DISTRICT","4_GOA,_DAMAN_&_DIU_MORMUGAO","4_LAKSHADWEEP_LACCADIVE, MINICOY AND AMINDIVI ISLANDS","4_MAHARASHTRA_YAVATMAL","4_MYSORE_BANGALORE CITY","4_MYSORE_MADHUGIRI","5_ANDAMAN_&_NICOBAR_ISLANDS_ANDAMAN AND NICOBAR ISLANDS","5_DADRA_NAGAR_&_HAVELI_DADRA AND NAGAR HAVELI","5_LAKSHADWEEP_LAKSHADWEEP","5_MAHARASHTRA_YAVATMAL","5_MYSORE_BANGALORE CITY","6_MAHARASHTRA_YAVATMAL","6_TAMIL_NADU_MAYURAM","7_TAMIL_NADU_MAYURAM","8_ASSAM_GUWAHATI","8_GUJARAT_VALSAD","8_TAMIL_NADU_MAYURAM","6_TAMIL_NADU_SALEM-TIRUCHENGODE")
config_eci_key=c("10_ASSAM_GAUHATI","10_KERALA_PALGHAT","10_KERALA_TRIVANDRUM","10_PUNJAB_JULLUNDUR","11_ASSAM_GAUHATI","11_PUNJAB_JULLUNDUR","12_KERALA_TRIVANDRUM","13_TAMIL_NADU_MADRAS CENTRAL","13_TAMIL_NADU_MADRAS NORTH","13_TAMIL_NADU_MADRAS SOUTH","14_TAMIL_NADU_MADRAS CENTRAL","14_TAMIL_NADU_MADRAS NORTH","14_TAMIL_NADU_MADRAS SOUTH","15_WEST_BENGAL_BURDWAN - DURGAPUR","15_WEST_BENGAL_SRERAMPUR","16_WEST_BENGAL_BURDWAN - DURGAPUR","16_WEST_BENGAL_SRERAMPUR","3_MAHARASHTRA_BOMBAY CITY CENTRAL NORTH","3_MAHARASHTRA_BOMBAY CITY CENTRAL SOUTH","3_MAHARASHTRA_YEOTMAL","4_ANDAMAN_&_NICOBAR_ISLANDS_ANDAMAN NICOBAR","4_ASSAM_AUTONOMOUS DISTS","4_GOA,_DAMAN_&_DIU_MARMAGOA","4_LAKSHADWEEP_LACCADIVE MINICOY AND AMINDIVI","4_MAHARASHTRA_YEOTMAL","4_MYSORE_BANGALORE","4_MYSORE_NADHUGIRI","5_ANDAMAN_&_NICOBAR_ISLANDS_A AND N ISLANDS","5_DADRA_NAGAR_&_HAVELI_D AND N HAVELI","5_LAKSHADWEEP_L M AND A ISLANDS","5_MAHARASHTRA_YEOTMAL","5_MYSORE_BANGALORE","6_MAHARASHTRA_YEOTMAL","6_TAMIL_NADU_MAYILADUTURAI","7_TAMIL_NADU_MAYILADUTURAI","8_ASSAM_GAUHATI","8_GUJARAT_BULSAR","8_TAMIL_NADU_MAYILADUTURAI","6_TAMIL_NADU_SALEM")

mer_config=subset(unmer_ed_ab,is.na(KEY))
for(i in 1:nrow(mer_config)){
  for(j in 1:length(config_ls_key)){
    if(mer_config$ECI_KEY[i]==config_ls_key[j]){
      mer_config$ECI_KEY[i]=config_eci_key[j]
      mer_config$KEY[i]="Changed"
    }
  }
}


mer_fin_config=subset(mer_config,KEY=="Changed")
mer_fin_config=join(mer_fin_config,unmer_ed_ba,by="ECI_KEY")
final_config=mer_fin_config[,c("ECI_KEY","Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","NA.INDICATOR","MPS_No","CONSTITUENCY","Correct_State","State_name","Year","PC_no","PC_name","PC_type","Electors","ga_no","poll_no")]
final_config$MergedBy="Via Config"

#FROM A TO B
unmerged_AB=mer_config[is.na(mer_config$KEY),]
unmerged_AB=unmerged_AB[!(unmerged_AB$LSNum==1 | unmerged_AB$LSNum==2),]



#COMBINE FULL 
colnames(final_can)=colnames(final_ex)
colnames(final_ed)=colnames(final_ex)
colnames(final_compat)=colnames(final_ex)
colnames(final_config)=colnames(final_ex)
final_merged_ls_list=rbind(final_ex,final_can,final_ed,final_compat,final_config)


#FROM B TO A 
for(i in 1:nrow(unmer_compat_ba)){
  if(unmer_compat_ba$ECI_KEY[i] %in% final_merged_ls_list$ECI_KEY){
    unmer_compat_ba$MOD[i]="Found"
  }
}

unmerged_BA=subset(unmer_compat_ba,MOD=="NA")

#Manually Handling Rajgarh Issue
raigarh_tuples=final_merged_ls_list[final_merged_ls_list$PC_name=="RAIGARH" ,]
final_merged_ls_list=final_merged_ls_list[!final_merged_ls_list$PC_name=="RAIGARH",]
raigarh_config=list("10_MADHYA_PRADESH_RAIGARH_Digvijaya Singh,Shri",
                    "10_MADHYA_PRADESH_RAIGARH_Singh,Shri Lakshman",
                    "11_MADHYA_PRADESH_RAIGARH_Singh,Shri Lakshman",
                    "12_MADHYA_PRADESH_RAIGARH_Singh,Shri Lakshman",
                    "3_MADHYA_PRADESH_RAIGARH_Bhanu Prakash Singh (Narsinghgarh),H.H.Maharaja",
                    "6_MADHYA_PRADESH_RAIGARH_Pandit,Dr. Vasant Kumar",
                    "7_MADHYA_PRADESH_RAIGARH_Pandit,Dr. Vasant Kumar",
                    "8_MADHYA_PRADESH_RAIGARH_Digvijaya Singh,Shri",
                    "9_MADHYA_PRADESH_RAIGARH_Khandelwal,Shri Pyarelal")
raigarh_tuples$ECI_NAMES=paste0(raigarh_tuples$ECI_KEY,"_",raigarh_tuples$Member_Name)
raigarh_merge=data.frame()
merge_ind=data.frame()
k=1
merge_ind=data.frame()
for(i in 1:nrow(raigarh_tuples)){
  ls_raigarh=raigarh_tuples$ECI_NAMES[i]
  for(j in 1:length(raigarh_config)){
    eci_raigarh=as.character(raigarh_config[j])
    if(ls_raigarh==eci_raigarh){
      for(l in 1:ncol(raigarh_tuples)){
        raigarh_merge[k,l]=raigarh_tuples[i,l]  
      }
      merge_ind[i,1]=1
      k=k+1
    }
    
  }
  if(is.na(merge_ind[i,1])){
    merge_ind[i,1]=0
  }
}
colnames(raigarh_merge)=colnames(raigarh_tuples)

ncol_rai=ncol(raigarh_merge)
raigarh_merge=raigarh_merge[,c(-ncol_rai)]
raigarh_merge=raigarh_merge[,c("ECI_KEY","Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","NA INDICATOR","MPS_No","CONSTITUENCY","Correct_State")]

raigarh_merge$CONSTITUENCY="RAJGARH"
raigarh_merge$ECI_KEY=paste0(raigarh_merge$LSNum,"_",raigarh_merge$Correct_State,"_",raigarh_merge$CONSTITUENCY)

raigarh_merge=merge(raigarh_merge,unmerged_BA,by="ECI_KEY")


raigarh_merge=raigarh_merge[,c(-11)]
raigarh_merge=raigarh_merge[,c(-19,-20,-21)]
raigarh_merge$MOD="Rajgarh Exception"
colnames(raigarh_merge)=colnames(final_merged_ls_list)
final_merged_ls_list=rbind(final_merged_ls_list,raigarh_merge)

#modify unmerged list
unmerged_BA=subset(unmerged_BA,!PC_name=="RAJGARH")


#retrieve unchanged Raigarh List & add to final_merged_ls_list
raigarh_tuples=cbind(raigarh_tuples,merge_ind)
raigarh_add=subset(raigarh_tuples,V1==0)
raigarh_add=raigarh_add[,c(-20,-21)]
raigarh_add$MergedBy="RAIGARH"
final_merged_ls_list=rbind(final_merged_ls_list,raigarh_add)

final_merged_ls_list$CAND_KEY=paste0(final_merged_ls_list$LSNum,"_",final_merged_ls_list$State_name,"_",final_merged_ls_list$PC_no)

#MERGING WITH CANDIDATE LEVEL DATA

candidates_electoral_info=candidates_electoral_info[candidates_electoral_info$Position==1,]
candidates_electoral_info<-mutate_all(candidates_electoral_info,funs(toupper))

ls_num<-data.frame()
cand_key=data.frame()
for(i in 1:nrow(candidates_electoral_info)){
  if(candidates_electoral_info$Year[i]==1962){
    ls_num[i,1]<-3
  }else if(candidates_electoral_info$Year[i]==1967){
    ls_num[i,1]<-4
  }else if(candidates_electoral_info$Year[i]==1971){
    ls_num[i,1]<-5
  }else if(candidates_electoral_info$Year[i]==1977){
    ls_num[i,1]<-6
  }else if(candidates_electoral_info$Year[i]==1980){
    ls_num[i,1]<-7
  }else if(candidates_electoral_info$Year[i]==1984){
    ls_num[i,1]<-8
  }else if(candidates_electoral_info$Year[i]==1985){
    ls_num[i,1]<-8
  }else if(candidates_electoral_info$Year[i]==1989){
    ls_num[i,1]<-9
  }else if(candidates_electoral_info$Year[i]==1991){
    ls_num[i,1]<-10
  }else if(candidates_electoral_info$Year[i]==1992){
    ls_num[i,1]<-10
  }else if(candidates_electoral_info$Year[i]==1996){
    ls_num[i,1]<-11
  }else if(candidates_electoral_info$Year[i]==1998){
    ls_num[i,1]<-12
  }else if(candidates_electoral_info$Year[i]==1999){
    ls_num[i,1]<-13
  }else if(candidates_electoral_info$Year[i]==2004){
    ls_num[i,1]<-14
  }else if(candidates_electoral_info$Year[i]==2009){
    ls_num[i,1]<-15
  }else if(candidates_electoral_info$Year[i]==2014){
    ls_num[i,1]<-16
  }
}
colnames(ls_num)[1]<-"LS Number"
candidates_electoral_info=cbind(candidates_electoral_info,ls_num)

for(i in 1:nrow(candidates_electoral_info)){
  p=paste0(candidates_electoral_info$`LS Number`[i],"_",candidates_electoral_info$State_name[i],"_",candidates_electoral_info$PC_no[i])
  cand_key[i,1]=p
  }
colnames(cand_key)[1]<-"CAND_KEY"
candidates_electoral_info<-cbind(candidates_electoral_info,cand_key)
cand_info_final=candidates_electoral_info

merge_cand_info=merge(final_merged_ls_list,cand_info_final,by="CAND_KEY")
merge_cand_info_ba=merge(cand_info_final,final_merged_ls_list,by="CAND_KEY")

#for(i in 1:nrow(merge_cand_info)){
 # if(merge_cand_info$DUPLICATE_CHECK[i]=="COPIED"){
  #  merge_cand_info$Scraped.Data[i]="Page Error"
  

write.csv(merge_cand_info,"MERGEDCANDIDATES25_09.csv",row.names=FALSE)

cand1<-merge_cand_info$Member_Name
cand2<-data.frame(sapply(cand1,canonicalize1))
colnames(cand2)[1]="INDIAN_NAMES LS NAMES"
cand3<-merge_cand_info$Cand
cand4<-data.frame(sapply(cand3,canonicalize1))

colnames(cand4)[1]="INDIAN_NAMES ECI NAMES"

name_check=cbind(cand2,cand4)

for(i in 1:nrow(name_check)){
  y=compatibility(as.character(name_check$`INDIAN_NAMES LS NAMES`[i]),as.character(name_check$`INDIAN_NAMES ECI NAMES`[i]))>0
  name_check$COMPATIBILITY[i]=y
}
for(i in 1:nrow(name_check)){
  y2=adist(as.character(name_check$`INDIAN_NAMES LS NAMES`[i]),as.character(name_check$`INDIAN_NAMES ECI NAMES`[i]))
  name_check$ADIST[i]=y2
}

cand_merge_final=cbind(merge_cand_info,name_check)

write.csv(cand_merge_final,"TOTALFINALCANDMERGE25_09.csv",row.names=FALSE)

#removing bye-elections
cand_check=cand_merge_final[cand_merge_final$poll_no==0,]
cand_check
write.csv(cand_check,"ThisFile.csv",row.names=FALSE)

#-----Executed till here ------- 

#cand_merge_final_woscr=cand_merge_final[,c(-7)]
#write.csv(cand_merge_final_woscr,"TOTALFINALCANDMERGE_WOSCRAP_01_09.csv",row.names=FALSE)

cand_merge_final$CAND_NAME_KEY=paste0(cand_merge_final$LSNum,"_",cand_merge_final$State_name.x,"_",cand_merge_final$PC_no.y,"_",cand_merge_final$Cand)

config_names=list("10_DELHI_1_LAL KRISHAN ADVANI","10_MADHYA_PRADESH_31_ATAL BIHARI VAJPEYEE","11_ANDHRA_PRADESH_27_P.V.NARASIMHA RAO","11_KERALA_13_REMESH CHENNITHALA","11_KERALA_16_P.J.KURIEN","11_ODISHA_6_BIJU PATNAIK","12_PUNJAB_3_PREM SINGH LALPURA","13_KARNATAKA_5_SONIA GANDHI","13_UTTAR_PRADESH_67_MULAYAM SINGH YADAV","14_BIHAR_21_LALU PRASAD","15_UTTAR_PRADESH_20_AKHILESH YADAV","16_ANDHRA_PRADESH_15_KADIYAM SRIHARI","16_ANDHRA_PRADESH_6_KALVAKUNTLA CHANDRASEKHAR RAO","16_ASSAM_14_SARBANANDA SONOWAL","16_GUJARAT_20_NARENDRA MODI","16_JAMMU_&_KASHMIR_2_TARIQ HAMEED KARRA","16_KERALA_6_E. AHAMED","16_MADHYA_PRADESH_12_DALPAT SINGH PARASTE","16_MADHYA_PRADESH_24_DILEEPSINGH BHURIA","16_MEGHALAYA_2_PURNO AGITOK SANGMA","16_ODISHA_13_HEMENDRA CHANDRA SINGH","16_PUNJAB_2_CAPTAIN AMARINDER SINGH","16_WEST_BENGAL_1_RENUKA SINHA","16_WEST_BENGAL_14_KAPIL KRISHNA THAKUR","16_WEST_BENGAL_30_ADHIKARI SUVENDU","4_MADHYA_PRADESH_35_S.C.K. BAJPAI","4_MADRAS_32_D.S. GOPALAR","4_MYSORE_2_M. YESHVANTAPPA","4_MYSORE_21_D. D. DATTATRAYA","6_GUJARAT_4_PATEL KESHUBHAI SAVDASBHAI","6_KARNATAKA_3_RAJSHEKHAR MALLAPPA","6_MAHARASHTRA_46_GOTKHINDE GANAPATRAO TUKARAM","6_PUNJAB_12_PARKASH SINGH","6_RAJASTHAN_17_IALIYA","7_UTTAR_PRADESH_23_INDIRA GANDHI","8_MADHYA_PRADESH_34_KALICHARAN RAMRATAN","8_MAHARASHTRA_43_PAWAR SHARADCHANDRA GOVINDRAO")
cand_merge_final$ELIM=0
for(i in 1:nrow(cand_merge_final)){
  candname=cand_merge_final$CAND_NAME_KEY[i]
  for(j in 1:length(config_names)){
    if(candname==config_names[j]){
      cand_merge_final$ELIM[i]=1
    }
  }
  
}

cand_merge_final$poll_no=as.integer(cand_merge_final$poll_no)

cand_merge_final$ELIM=cand_merge_final$ELIM+cand_merge_final$poll_no
cand_merge_final=cand_merge_final[cand_merge_final$ELIM==0,]
cand_merge_final1=cand_merge_final[,c("Member_Name","Party_Name.x","LSNum","Links","Scraped_Data","MPS_No","CONSTITUENCY","Correct_State","State_name.x","Year.x","PC_no.x","PC_name","PC_type","Electors","ga_no","Cand","Sex","Cand_Type","Party","Votes","COMPATIBILITY","ADIST")]


colnames(cand_merge_final1)=c("MEMBER_NAME_LS","PARTY_NAME_LS","LS_NUMBER","PROFILE_LINK","SCRAPED_DATA","MPS_NO","LS_CONSTITUENCY","LS_STATE","ECI_STATE","ECI_YEAR","PC_NO","PC_NAME","PC_TYPE","ELECTORS","GA_NO","ECI_CANDIDATE_NAME","SEX","CAND_TYPE","ECI_PARTY","VOTES","COMPATIBILITY","ADIST")
cand_merge_final1$LINK_KEY=paste0(cand_merge_final1$LS_NUMBER,"_",cand_merge_final1$ECI_STATE,"_",cand_merge_final1$PC_NO)
cand_merge_final1$PROFILE_LINK=paste0("http://164.100.47.194/Loksabha/Members/",cand_merge_final1$PROFILE_LINK)

#Final File is here

  
##Manually Combine: Boianapalli,ShriVinodKumar – 14_NA_NA to 14_ANDHRA_PRADESH_38 
# Rao,ShriKalvakuntlaChandrashekar – 14_NA_NA to 14_ANDHRA_PRADESH_37
# Sidhu,ShriNavjotSingh -14_NA_NA to 14_Punjab_2
# JB SINGH FROM JAN SANGH IN SHAHAJANPUR DIST to 4_UTTAR_PRADESH_15 (Shahabad) 

add1=c("Boianapalli,Shri Vinod Kumar","TRS","14","MemberBioprofile.aspx?mpsno=4084&lastls=16","<U+00A0> Father's Name Shri B. Muralidhar Rao Mother's Name Late Smt. B. Sugunadevi Date of Birth 22 Jul 1959 Place of Birth Karimnagar (Telangana) Marital Status Married Date of Marriage 11 Aug 1990 Spouse's Name Dr. (Smt.) B. Madhavi No. of Sons 2 Educational Qualifications B.Sc., LL.B. Educated at University College of Law, Kakatiya University, Hanamkonda, Warangal, Telangana Profession Advocate Permanent Address Flat No. 401, Poulomi Residency, Mukarampura, Karimnagar-505001 Telangana 09848037689, 09643013567 (M) Present Address Bungalow No. 3, Harish Chandra Mathur Lane, New Delhi-110 001 09013869903 (M) Positions Held 2004 Elected to 14th Lok Sabha Member, Standing Committee on Energy 3 March 2008 Resigned from Lok Sabha June, 2008 Re-elected to 14th Lok Sabha in a bye-election May, 2014 Re-elected to 16th Lok Sabha (2nd term) 1 Sep. 2014 onwards Member, Standing Committee on Water Resources Member, Consultative Committee, Ministry of Health and Family Welfare 13 May 2015 onwards Member, Joint Committee on the Right to Fair Compensation and Transparency in Land Acquisition, Rehabilitation and Resettlement (Second Amendment) Bill, 2015 11 May 2016 onwards Member, Joint Committee on the Enforcement of Security Interest and Recovery of Debts Laws and Miscellaneous Provision (Amendment) Bill, 2016 1 May 2017 onwards Member, Committee on Public Undertakings <U+00A0> Literary, Artistic & Scientific Accomplishments Compilation of Articles published in various Newspapers during the period 2004 - 2014 <U+00A0> <U+00A0> Special Interests Participating in Seminars and Conferences relevant to society; viewing and listening to debates <U+00A0> Favourite Pastime and Recreation Reading, listening to news and events of National and International importance <U+00A0> Sports and Clubs Member, Nizam Club, Hyderabad; Indian Habitat Centre, New Delhi; and Mahindra Club Holiday <U+00A0> Countries Visited Austria, France, Germany, Italy, Netherlands, Switzerland, U.K. and U.S.A. <U+00A0> Other Information Actively participated in students` movements; Office bearer, All India Students Federation (AISF); and Indian Association of Lawyers; Elected to Students` Union at Degree College; Member, Bar Association, Warangal and A.P. High Court; Participated in various National and International Seminars on World Peace and Social Justice","4084","HANAMKONDA","ANDHRA_PRADESH","ANDHRA_PRADESH","2004","38","HANAMKONDA","GEN","1207089","14","B.VINOD KUMAR","M","GEN","TRS","496048",NA,NA,"14_ANDHRA_PRADESH_38")
add2=c("Rao,Shri Kalvakuntla Chandrashekar","TRS","14","MemberBioprofile.aspx?mpsno=4083&lastls=16","<U+00A0> Father's Name Shri Kalvakuntla Raghava Rao Mother's Name Smt. Venkatamma Date of Birth 17 Feb 1954 Place of Birth Chintamadaka, Distt. Medak, Andhra Pradesh (now Telangana) Marital Status Married Date of Marriage 23 Apr 1969 Spouse's Name Smt. K. Shobha No. of Sons 1 No.of Daughters 1 Educational Qualifications M.A. (Literature) Educated at Osmania University, Hyderabad, Andhra Pradesh (now Telangana) Profession Social Worker Permanent Address 8-2-220-110/1/3, Road No. 14, Banjara Hills, Hyderabad Telangana Tels. (040) 23555798, 23555669 Present Address Camp Office, Green Lands, Begumpet, Hyderabad, Telangana Tel : (040) 23410333 Samatha Block, Secretariat,Hyderabad, Telangana Tels : (040) 23456698, 23455205, 23452933, 23234404 Positions Held 1982 Chairman, Raghavapur Primary Agriculture Cooperative Society (P.A.C.S.), Siddipet Vice-President, Andhra Pradesh Youth Congress 1985, 1989, 1994, 1999, 2001 and 2014* Member, Andhra Pradesh Legislative Assembly (six terms) [*w.e.f. 2 June 2014, Telangana Legislative Assembly] 1987-88 Minister of State, Govt. of Andhra Pradesh 1988 - 89 Minister of Drought, Govt. of Andhra Pradesh 1989 - 93 District Party President, Telugu Desam Party (TDP) 1992-93 Chairman, Committee on Public Undertakings 1993 - 94 State Secretary, Telugu Desam Party (T.D.P.) 1995 - 96 Member, Committee on Public Undertakings 1997-2000 Cabinet Minister, Transport, Govt. of Andhra Pradesh 1999-2001 Deputy Speaker, Andhra Pradesh Legislative Assembly 2003 Convenor, New States National Front 2004 Elected to 14th Lok Sabha 2004-06 Union Cabinet Minister, Labour and Employment 23 Sept. 2006 Resigned from Lok Sabha 7 Dec. 2006 Re-elected to 14th Lok Sabha in bye-election 3 March 2008 Resigned from Lok Sabha 2009 Re-elected to 15th Lok Sabha (2nd term) Leader, Telangana Rashtra Samithi Parliamentary Party, Lok Sabha 31 Aug. 2009 Member, Standing Committee on Energy 23 Sept. 2009 Member, Rules Committee 16 May 2014 Re-elected to 16th Lok Sabha (3rd term) 29 May 2014 Resigned from Lok Sabha <U+00A0> <U+00A0> Social And Cultural Activities Involved in social work and strived for the upliftment of downtrodden and poor; some of the works undertaken include, (i) Massive tree plantation scheme worth one crore rupees, undertaken in Karimnagar in 2004; (ii) Construction of several homes for muslim minorities; (iii) Supply of drinking water to 145 habitants affected by floods in Siddipet constituency <U+00A0> Special Interests Writing and service to the poor <U+00A0> Favourite Pastime and Recreation Reading and playing Badminton <U+00A0> <U+00A0> <U+00A0> Other Information Resigned from the Telugu Desam Party, Office of Deputy Speaker and as M.L.A. in 2001 and launched the <U+2018>Telangana Rashtra Samithi<U+2019> (T.R.S.). Subsequently, elected to the Andhra Pradesh Legislative Assembly in a bye-election held from Siddipet Constituency. In General election to the 14th Lok Sabha and Andhra Pradesh Legislative Assembly, the T.R.S. won 5 Lok Sabha Seats and 26 Assembly Seats in alliance with Congress-I. In August 2006, resigned from the Union Cabinet for the cause of Telangana and sat on Hunger Strike at Jantar Mantar in New Delhi. Went on a Hunger Strike again on 29 November 2009 which he withdrew on 9 December 2009 following the announcement by the Govt. of India that the process for formation of a separate State of Telangana would be initiated. In the General Elections held to the 16th Lok Sabha and to the Andhra Pradesh* Legislative Assembly, won from both Medak Parliamentary Constituency and Siddipet Assembly Constituency respectively. Resigned from the Medak Parliamentary Constituency on 29 May 2014 __________________ *with effect from 2 June 2014, the Telangana Legislative Assembly","4083","KARIMNAGAR","ANDHRA_PRADESH","ANDHRA_PRADESH","2004","37","KARIMNAGAR","GEN","1344786","14","K. CHANDRA SHAKHER RAO","M","GEN","TRS","451199",NA,NA,"14_ANDHRA_PRADESH_37")
add3=c("Sidhu,Shri Navjot Singh","BJP","14","MemberBioprofile.aspx?mpsno=4000&lastls=15","<U+00A0> Date of Birth 20 Oct 1963 Educational Qualifications B.A., LL.B. Profession Political and Social Worker Artist, Commentator Permanent Address 26, Yadavmdra Colony, Mall Road PatialaPunjab Tel. (0175) 2212126 Present Address C-1/20, Humayun Road, New Delhi-110 003 Tels.(011) 24636244, 09868180404 (M) Positions Held 2004 Elected to 14th Lok Sabha Member, Committee on Urban Development 4 Dec. 2006 Resigned from Lok Sabha 27 Feb. 2007 Re-elected in a bye-election to 14th Lok Sabha 2009 Re-elected to 15th Lok Sabha (2nd term) 31 Aug. 2009 Member, Committee on Rural Development Member, Committee on External Affairs <U+00A0> <U+00A0> <U+00A0> <U+00A0> <U+00A0> <U+00A0> <U+00A0>","4000","AMRITSAR","PUNJAB","PUNJAB","2004","2","AMRITSAR","GEN","451199","14","NAVJOT SINGH SIDHU","M","GEN","BJP","394223",NA,NA,"14_PUNJAB_2")

cand_merge_final1=rbind(cand_merge_final1,add1,add2,add3)

write.csv(cand_merge_final1,'FinalScrapedDatabase.csv')
