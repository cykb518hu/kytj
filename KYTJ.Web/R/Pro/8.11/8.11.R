args <- commandArgs()

rlib<-args[6]	#libary path
if (is.na(rlib)) {output<-args[6]} else {output<-args[7]}	#param path
print(output)
#output<-c('D:\\Works\\产品\\科研\\代码\\ScienceStatistics.Web\\Output\\1\\1_1\\636525834691779319')
setwd(output)
d<-read.csv("./data.csv")
p<-read.csv('./Parameters.csv')
a<-p

#参数设定
idata<-d
R.Version4RUN<-343;
if (is.na(rlib)) {R.LibLocation<-"C:/R/R-3.4.3/library"} else {R.LibLocation=rlib}

Sys.setlocale("LC_TIME", "C")
library(doBy,lib.loc=R.LibLocation)
library(plotrix,lib.loc=R.LibLocation)
library(showtext,lib.loc=R.LibLocation)
showtext_auto(enable=TRUE)
pdfwd<-6; pdfht<-6
if (length(which(ls()=="ClinStats"))==0) ClinStats<-get(ls()[1])
names(ClinStats)<-toupper(names(ClinStats))

inumber<-as.numeric(a[1,1]);
iyn1<-as.character(a[1,2]);iyv1<-as.character(a[1,3]);iys1<-as.numeric(a[1,4])
ixn1<-as.character(a[1,5]);ixv1<-as.character(a[1,6]);ixs1<-as.numeric(a[1,7])
ixn2<-as.character(a[1,8]);ixv2<-as.character(a[1,9]);ixs2<-as.numeric(a[1,10])
ixn3<-as.character(a[1,11]);ixv3<-as.character(a[1,12]);ixs3<-as.numeric(a[1,13])
ixn4<-as.character(a[1,14]);ixv4<-as.character(a[1,15]);ixs4<-as.numeric(a[1,16])
ixn5<-as.character(a[1,17]);ixv5<-as.character(a[1,18]);ixs5<-as.numeric(a[1,19])
ixn6<-as.character(a[1,20]);ixv6<-as.character(a[1,21]);ixs6<-as.numeric(a[1,22])
ixn7<-as.character(a[1,23]);ixv7<-as.character(a[1,24]);ixs7<-as.numeric(a[1,25])
slt.vname<-c()

if(inumber==1)  {idata<-idata[,c(iyn1,ixn1)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1)
}else if(inumber==2)  {idata<-idata[,c(iyn1,ixn1,ixn2)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2)
}else if(inumber==3)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3)
}else if(inumber==4)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4)
}else if(inumber==5)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)
}else if(inumber==6)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
}else if(inumber==7)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
}

ofname<-"8_11"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"主成分分析(PCA)"; 
attach(WD) 
subjvname<-NA;

if(inumber==1)  {
  xv<-as.matrix(idata[,c(ixn1)]); 
  xvname<-c(ixv1); 
  xvar<-c(ixv1); 
  xlv<-c(NA,ixs1)[-1]; 
}else if(inumber==2)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  xvname<-c(ixv1,ixv2); 
  xvar<-c(ixv1,ixv2); 
  xlv<-c(NA,ixs1,ixs2)[-1]; 
}else if(inumber==3)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  xvname<-c(ixv1,ixv2,ixv3); 
  xvar<-c(ixv1,ixv2,ixv3); 
  xlv<-c(NA,ixs1,ixs2,ixs3)[-1]; 
}else if(inumber==4)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4); 
  xlv<-as.numeric(c(NA,ixs1,ixs2,ixs3,ixs4)[-1]); 
}else if(inumber==5)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5)[-1]; 
}else if(inumber==6)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6)[-1]; 
}else if(inumber==7)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7)[-1]; 
}else if(inumber==8)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7,ixs8)[-1]; 
}

sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-idata[,iyn1];colvname<-c(iyv1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 


##R package##  ##R package##;
printTxt<-function(txt,cname,rname) {
  tmp<-as.matrix(txt,ncol=1); rname1<-rname
  if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
  colnames(tmp)=cname;  rownames(tmp)=rname1;print(tmp,quote=F)
}
cmp<-(apply(is.na(xv),1,sum)==0)
tmp.xx<-xv[cmp,]
tmp.pca<-princomp(tmp.xx,scores=TRUE)
tmp.scores<-tmp.pca$scores
tmp.scores<-rbind(colnames(tmp.scores),tmp.scores)
rownames(tmp.scores)<-c(colvname,colv[cmp])
write.table(tmp.scores,file=paste(ofname,"_scores.xls",sep=""),quote=FALSE, col.names=FALSE,row.names=TRUE,sep="\t")
sink(paste(ofname,".txt",sep=""))
printTxt("主成分分析","","")
print(tmp.pca);print(summary(tmp.pca));print(loadings(tmp.pca))
sink()
png(paste(ofname,"_scree.png",sep=""),width=720,height=560)
plot(tmp.pca,type="lines",main="Scree Plot")
dev.off()
png(paste(ofname,"_biplot.png",sep=""),width=720,height=560)
biplot(tmp.pca,main="Biplot",cex = c(0.2,0.6))
dev.off()