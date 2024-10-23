setwd("D:\\projects\\杂项\\2024_10_8_ljy_PAM\\PAM_8n")
library(ggplot2)
library(stringr)
library(reshape2)

mydark<-"black"

mapping<-read.csv("mapping.csv",header = T,stringsAsFactors = F)
inputs<-unique(mapping[,c("input","nnn_length")])

for (row in 1:nrow(inputs)) {
  file<-inputs$input[row]
  nnn<-inputs$nnn_length[row]
  sample<-unlist(strsplit(file,split = "_NNN"))[1]
  input<-read.csv(file,sep = "\t",stringsAsFactors = F,header = F)
  names(input)<-c("input","input_n")
  input<-na.omit(input)
  input<-input[nchar(input$input)==nnn,]
  
  all_nnn<-c("")
  for (i in 1:nnn) {
    all_nnn<-apply(expand.grid(all_nnn,c("A","T","C","G")),1,paste0,collapse="")
  }
  
  tmp<-merge(data.frame(all_nnn),input,by.x="all_nnn",by.y="input",all.x=T)
  tmp$input_n[is.na(tmp$input_n)]<-0
  tmp<-tmp[order(tmp$input_n,decreasing = F,na.last = T),]
  var<-var(tmp$input_n*length(tmp$input_n)/sum(tmp$input_n))
  tmp<-cbind(tmp,index=1:nrow(tmp),A=tmp$input_n*str_count(tmp$all_nnn,"A")/nnn,T=tmp$input_n*str_count(tmp$all_nnn,"T")/nnn,C=tmp$input_n*str_count(tmp$all_nnn,"C")/nnn,G=tmp$input_n*str_count(tmp$all_nnn,"G")/nnn)
  tmp<-melt(tmp[,-1],id.vars = c("input_n","index"))
  
  graphics.off()
  pdf(paste0("input_rank.",sample,"_nnn",nnn,".pdf"),width = 8,height = 6)
  print(
  ggplot(tmp)+
    geom_bar(aes(x=index,y=value,group=variable,fill=variable),position="stack",stat = "identity",width = 1)+
    geom_hline(yintercept = 0,linewidth=0.5)+
    scale_x_continuous(breaks = c(length(all_nnn)),labels = c(length(all_nnn)),expand = expansion(mult = c(0, 0.05)))+
    theme_minimal()+
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y.left = element_line(linewidth = 0.5),
          text = element_text(colour = mydark),line = element_line(linewidth = 0.5,colour = mydark),
          axis.text.y = element_text(colour = mydark),
          axis.ticks.x = element_line(linewidth = 0.5,colour = mydark),
          axis.ticks.y = element_line(linewidth = 0.5,colour = mydark))+
    labs(x="rank",y="input #",fill=NULL)+
    ggtitle(paste0("var=",round(var,5)))
  )
  dev.off()
}


