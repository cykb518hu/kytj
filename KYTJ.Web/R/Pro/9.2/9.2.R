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
#--#

inumber<-as.numeric(a[1,1]);p1<-as.numeric(a[1,2])
ixn1<-as.character(a[1,3]);ixv1<-as.character(a[1,4]);ixs1<-as.numeric(a[1,5])
ixn2<-as.character(a[1,6]);ixv2<-as.character(a[1,7]);ixs2<-as.numeric(a[1,8])
ixn3<-as.character(a[1,9]);ixv3<-as.character(a[1,10]);ixs3<-as.numeric(a[1,11])
ixn4<-as.character(a[1,12]);ixv4<-as.character(a[1,13]);ixs4<-as.numeric(a[1,14])
ixn5<-as.character(a[1,15]);ixv5<-as.character(a[1,16]);ixs5<-as.numeric(a[1,17])
ixn6<-as.character(a[1,18]);ixv6<-as.character(a[1,19]);ixs6<-as.numeric(a[1,20])
ixn7<-as.character(a[1,21]);ixv7<-as.character(a[1,22]);ixs7<-as.numeric(a[1,23])
slt.vname<-c()

if(inumber==1)  {idata<-idata[,c(ixn1)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1)
vlabel<-c("样本量(%)","统计量","合计",ixn1)
}else if(inumber==2)  {idata<-idata[,c(ixn1,ixn2)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2)
}else if(inumber==3)  {idata<-idata[,c(ixn1,ixn2,ixn3)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3)
}else if(inumber==4)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3,ixn4)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4)
}else if(inumber==5)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3,ixn4,ixn5)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5)
}else if(inumber==6)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
}else if(inumber==7)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
}

library(haplo.stats,lib.loc=R.LibLocation)
 
ofname<-"9_2"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"单倍体型频率估计"; 
attach(WD) 
subjvname<-NA; 
if(inumber==1)  {
  xv<-as.matrix(idata[ixn1]); 
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
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1];
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
}
sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-p1;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 


##R package## haplo.stats ##R package##;
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
set.seed(c(11,13,15,17,19,21,24,27,31,35,39,45,50,56,62,69,76,84,92))
tmp.nna<-apply(is.na(xv),1,sum)
xx<-xv[tmp.nna==0,]
if (par1==1) {
  xx<-geno1to2(xx);loci.lbl<-colnames(xv)
} else {
  loci.lbl<-apply(matrix(colnames(xv),ncol=2,byrow=TRUE),1,function(x) paste(x,collapse="-"))
}
cname<-c("#", "单倍体型编码", loci.lbl, "频率")
if (is.na(bvar)) {
  hpo<-haplo.em(geno=xx, locus.label=loci.lbl, miss.val=c(0,NA));
  freq<-hpo$hap.prob
  hpo.ff<-cbind(hpo$haplotype,freq)
  hpo.ff<-hpo.ff[order(-freq),]
  hpo.x2<-c(hpo$lnlike,hpo$lr,hpo$df.lr,1-pchisq(hpo$lr,df=hpo$df.lr))
  hpo.x2<-rbind(c("对数似然值","似然比检验 (卡方值)","自由度","P 值"),hpo.x2) 
  hpo.ss<-summary(hpo,show.haplo=TRUE)
  hpo.ss<-rbind(c("单倍体型编码",colnames(hpo.ss)),format(cbind(rownames(hpo.ss),hpo.ss),digits=4,nsmall=4))
  write.table(hpo.ss,file=paste(ofname,"xls",sep="."),row.names=FALSE, col.names=FALSE,sep="\t",append=FALSE,quote=FALSE);
} else {
  bv<-bv[tmp.nna==0]
  hpo<-haplo.group(bv, xx, locus.label=loci.lbl, miss.val=c(0,NA));
  hpo.ff<-hpo[[1]]
  hpo.ff<-hpo.ff[order(-hpo.ff[,"Total"]),]
  cname1<-" ";
  for (i in (1:length(hpo$group.count))) {
    cname1<-c(cname1,vlabel[vname==paste(bvar,names(hpo$group.count[i]),sep=".")]);
  }
  hpo.x2<-rbind(cname1[-1],hpo$group.count)
  cname<-c(cname,cname1[-1])
}
tmp.ff<-cbind((1:nrow(hpo.ff)),rownames(hpo.ff),hpo.ff)
hpo.oo<-rbind(cname,format(tmp.ff,digits=4,nsmall=4))
w<-c(w,"单倍体型频率估计<table border=2 bgcolor=\"wheat\">")
w<-c(w,paste("<tr><td>",apply(hpo.oo,1,function(x) paste(x,collapse="</td><td>")),"</td></tr>"))
w<-c(w,"</table>")
w<-c(w,"</br><table border=2 bgcolor=\"wheat\">")
w<-c(w,paste("<tr><td>",apply(hpo.x2,1,function(x) paste(x,collapse="</td><td>")),"</td></tr>"))
w<-c(w,"</table>")
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)