#!/usr/bin/Rscript

library(ggseqlogo)
library(ggplot2)
library(seqLogo)
library(gridExtra)
library(openxlsx)

Args<-commandArgs(trailingOnly=T)

wkdir<-paste0(Args[1],"/")
outputdir<-paste0(Args[2],"/")

#读取
mapping<-list.files(path = wkdir,pattern = "mapping.*")
mapping<-read.csv(paste0(wkdir,mapping),header = T,stringsAsFactors = F)
uniq_inputs<-unique(mapping$input)
nrow<-length(uniq_inputs)
ncol<-max(table(mapping$input))
tmp<-mapping[!duplicated(mapping[,1:2]),]
ncol0<-max(table(tmp$input))

mySeqLogo = seqLogo::seqLogo
bad = (sapply( body(mySeqLogo), "==", "grid.newpage()") |
         sapply( body(mySeqLogo), "==", "par(ask = FALSE)"))
body(mySeqLogo)[bad] = NULL
mydark<-"black"


ID<-c()
ID0<-c()
graphics.off()
pdf(paste0(outputdir,"seqlogo.pdf"),width = 1+ncol*4.5,height = 1+nrow*3)
grid.newpage()
layout_matrix0 = matrix(nrow = nrow,ncol=ncol0)
layout_matrix = matrix(nrow = nrow,ncol=ncol)
jj<-1
kk<-1
for (n in 1:length(uniq_inputs)) {
  input0<-uniq_inputs[n]
  data0<-mapping[mapping$input==input0,]
  ll<-1
  for (o in 1:nrow(data0)) {
    layout_matrix[n,o]<-jj
    jj<-jj+1
    
    output<-data0$output[o]
    nnn<-data0$nnn_length[o]
    z<-data0$zscore_cutoff[o]
    all_nnn<-c("")
    for (i in 1:nnn) {
      all_nnn<-apply(expand.grid(all_nnn,c("A","T","C","G")),1,paste0,collapse="")
    }
    
    id0<-paste0(unlist(strsplit(output,"_NNN"))[1],"_",unlist(strsplit(input0,"_NNN"))[1])
    if(!id0 %in% ID0){
      ID0<-c(ID0,id0)
      layout_matrix0[n,ll]<-kk
      ll<-ll+1
      kk<-kk+1
    }
    id<-paste0(unlist(strsplit(output,"_NNN"))[1],"_",unlist(strsplit(input0,"_NNN"))[1],"_",z)
    ID<-c(ID,id)
	print(paste0("正在处理",id,"..."))
    
    input<-read.csv(paste0(wkdir,input0),sep = "\t",stringsAsFactors = F,header = F)
    names(input)<-c("input","input_n")
    input<-na.omit(input)
    input<-input[nchar(input$input)==nnn,]
    
    output<-read.csv(paste0(wkdir,output),sep = "\t",header = F,stringsAsFactors = F)
    names(output)<-c("output","output_n")
    output<-na.omit(output)
    output<-output[nchar(output$output)==nnn,]
    data<-merge(input,output,by.x="input",by.y="output",all=T)
    data$output_n[is.na(data$output_n)]<-0
    data$input_n[is.na(data$input_n)]<-0
    data<-cbind(data,input_p=data$input_n/sum(data$input_n),output_p=data$output_n/sum(data$output_n))
    
    #ggplot(data)+geom_point(aes(x=input_p,y=output_p))+geom_abline()+scale_x_log10()+scale_y_log10()+theme_bw()
    
    mysum<-0
    k<-0
    for (j in 1:nrow(data)) {
      if (data$output_p[j]>data$input_p[j] & data$input_p[j]!=0) {
        k<-k+1
        sigmaS<-1/(data$input_n[j])+1/(data$output_n[j])
        mysum<-mysum+(log((data$output_p[j])/(data$input_p[j])))^2-sigmaS
      }
    }
    sigmaA<-mysum/k
    equation<-function(input_p,output_p,sigmaAA=sigmaA,suminput_n=sum(data$input_n),sumoutput_n=sum(data$output_n)){
      return(-log(output_p/input_p)/sqrt(sigmaAA+1/(suminput_n*input_p)+1/(sumoutput_n*output_p)))
    }
    zscore<-unlist(apply(data[,c("input_p","output_p")],1,function(x){return(equation(x[1],x[2]))}))
    index<-zscore > z
    index[is.na(index)]<-FALSE
    # contour_data<-expand.grid(input_p=exp(seq(from=log(max(min(data$input_p),1e-7)),to=log(max(data$input_p)),length.out=100)),
    #                           output_p=exp(seq(from=log(max(min(data$output_p),1e-7)),to=log(max(data$output_p)),length.out=100)))
	contour_data<-expand.grid(input_p=exp(seq(from=log(min(data$input_p[data$input_p != 0])),to=log(max(data$input_p)),length.out=100)),
                              output_p=exp(seq(from=log(min(data$output_p[data$output_p != 0])),to=log(max(data$output_p)),length.out=100)))
    contour_data<-cbind(contour_data,z=equation(contour_data$input_p,contour_data$output_p))
    plotdata<-cbind(data,group=index)
    assign(paste0("p_",id),
           ggplot()+
             geom_point(data = plotdata,mapping=aes(x=input_p,y=output_p,color=group),size=0.5)+
             geom_abline()+
             scale_x_log10()+scale_y_log10()+
             geom_contour(data = contour_data,aes(x=input_p,y=output_p,z = z), breaks = z,linewidth=1,color="#2093DA")+
			 geom_contour_filled(data = contour_data,aes(x=input_p,y=output_p,z = z), breaks = z,color="#2093DA", alpha = 0.5)+
             guides(color="none")+
             labs(x="%input",y="%output")+ 
             theme_bw()+
             theme(text = element_text(colour = mydark),line = element_line(linewidth = 0.2,colour = mydark),
                   axis.text.x = element_text(colour = mydark),
                   axis.text.y = element_text(colour = mydark),
                   axis.ticks = element_line(linewidth = 0.2,colour = mydark))+
             ggtitle(id)
    )
    
    data<-cbind(data,zscore=zscore)
    #log(input_p/output_p)累积分布图
    tmp<-merge(data.frame(all_nnn),data,by.x="all_nnn",by.y="input",all.x=T)
    tmp$output_n[is.na(tmp$output_n)]<-0
    tmp$input_n[is.na(tmp$input_n)]<-0
    tmp$output_p[is.na(tmp$output_p)]<-0
    tmp$input_p[is.na(tmp$input_p)]<-0
    value<-(-log2(tmp$output_p/tmp$input_p))
    tmp<-cbind(tmp,depletion_score=value)
    tmp<-tmp[order(tmp$zscore,tmp$depletion_score,decreasing = T,na.last = T),]
    names(tmp)[1]<-paste0("n",nnn)
    write.table(tmp,paste0(outputdir,id,".statistics.tsv"),sep = "\t",row.names = F)
    tmp<-tmp[order(tmp$depletion_score,decreasing = F,na.last = T),]
    tmp<-cbind(tmp,index=1:nrow(tmp))
    assign(paste0("pp_",id0),
           ggplot(tmp)+
             geom_bar(aes(x=index,y=depletion_score),stat = "identity",width = 1,fill="#2093DA")+
             geom_hline(yintercept = 0,linewidth=0.5)+
             scale_x_continuous(breaks = c(nrow(tmp)),labels = c(nrow(tmp)),expand = expansion(mult = c(0, 0.05)))+
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
             labs(x="rank",y=expression(paste("PAM depletion (-log"[2]," ratio)")))+
             ggtitle(id))
    #seqlogo
    row<-n
    col<-o
    vp <- viewport(x = (col-1)/ncol, y = 1-(row-1)/nrow, w = 1/ncol, h = 1/nrow, just = c("left", "top"))
    pushViewport(vp)
    
    if (sum(index)==0) {
	  upViewport()
      next
    }
    tmp<-data[index,]
    weights<-log2(tmp$input_p/tmp$output_p)
    weights[is.nan(weights)]<-0
    weights[is.infinite(weights)]<-max(weights[is.finite(weights)])
    pwd<-matrix(0,nrow = 4,ncol = nnn)
    for (j in 1:nnn) {
      for (k in 1:4) {
        m<-c('A','C','G','T')[k]
        value<-sum(weights*unlist(lapply(tmp$input,function(x){return(substr(x,j,j)==m)})))
        pwd[k,j]<-value
      }
    }
    pwd<-pwd/colSums(pwd)
    pwd<-makePWM(pwd)
    mySeqLogo(pwd,ic.scale = TRUE)
    grid.text(sprintf("%s, num=%d",id,sum(weights != 0)),
              x=0.5, y=0.95,
              just="top")
    upViewport()
  }
}
dev.off()

graphics.off()
pdf(paste0(outputdir,"visualization.zscore_cutoff_",z,".pdf"),width = 1+ncol*4.5,height = 1+nrow*3)
eval(parse(text = paste0("grid.arrange(",paste0("`p_",ID,"`",collapse = ","),",layout_matrix=layout_matrix)")))
dev.off()

graphics.off()
pdf(paste0(outputdir,"rank.pdf"),width = 1+ncol0*4.5,height = 1+nrow*3)
eval(parse(text = paste0("grid.arrange(",paste0("`pp_",ID0,"`",collapse = ","),",layout_matrix=layout_matrix0)")))
dev.off()


