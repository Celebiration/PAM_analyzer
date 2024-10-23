setwd("D:\\projects\\杂项\\2024_10_8_ljy_PAM\\PAM_6n")

library(dplyr)
nnn<-8
target_nnn_start<-1
target_nnn_end<-6

files<-list.files(pattern = ".*_freq_sorted.txt$")

for (file in files) {
  input<-read.csv(file,sep = "\t",stringsAsFactors = F,header = F)
  names(input)<-c("input","input_n")
  input<-na.omit(input)
  input<-input[nchar(input$input)==nnn,]
  tmp<-unlist(lapply(input$input,function(x){return(substr(x,start = target_nnn_start,stop = target_nnn_end))}))
  tmp<-data.frame(input=tmp,input_n=input$input_n)
  new<-tmp %>% group_by(input) %>% summarise(input_n=sum(input_n))
  new<-new[order(new$input_n,decreasing = T),]
  write.table(new,paste0(file,".shortened.txt"),row.names = F,col.names = F,sep = "\t",quote = F)
}
