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
}

library(psychometric,lib.loc=R.LibLocation)
library(multilevel,lib.loc=R.LibLocation)
library(nlme,lib.loc=R.LibLocation)
library(irr,lib.loc=R.LibLocation)
 
ofname<-"8_2"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"Mahalanobis 距离"; 
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
}

sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-idata[,iyn1];colvname<-c(iyn1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 


##R package## psychometric multilevel nlme irr ##R package##;
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
nobs<-nrow(xv)
ncmp=sum(apply(is.na(cbind(xv,colv)),1,sum)==0);
if (is.na(colvname)) {
  colvname="_TMPID"; colv<-(1:nobs);colvb="_TMPID";
} else {
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
}
tmp.mu<-colMeans(xv,na.rm=TRUE)
if (!is.na(par1)) {mu<-as.numeric(strsplit(par1,split=",")[[1]]);} else {mu<-tmp.mu;}
nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
tmp.vv<-var(xv,na.rm=TRUE)
oo1<-cbind(c("","比较均数","样本均数"),rbind(xb,mu,tmp.mu))
tmp.md2<-mahalanobis(xv,mu,tmp.vv)
tmp.pd2<-1-pchisq(tmp.md2,df=nx)
tmp.pd2slt<-tmp.pd2[tmp.pd2<0.05]
tmp.md2slt<-tmp.md2[tmp.pd2<0.05]
tmp.idslt<-colv[tmp.pd2<0.05]
tmp.tt<-cbind(ID=colv,D2=tmp.md2,PVALUE=tmp.pd2)
tmp.ttslt<-rbind(c(colvb,"D2(Mahalanobis距离)","P 值"),cbind(ID=tmp.idslt,D2=tmp.md2slt,Pvalue=tmp.pd2slt))
write.table(tmp.tt,file=paste(ofname,"_mahalanobit.xls",sep=""),col.names=TRUE,sep="\t",row.names=FALSE)
tt.sort<-rbind(c(colvb,"D2(Mahalanobis距离)","P 值"), tmp.tt[order(-tmp.md2),][1:10,])

y.min<-min(xv,na.rm=TRUE)
y.max<-max(xv,na.rm=TRUE)
y.min<-ifelse(y.min>0, y.min*0.9, y.min*1.1)
y.max<-ifelse(y.max>0, y.max*1.1, y.max*0.9)
x.tmp<-(1:ncol(xv))
n.lines=nrow(xv)
c.tmp<-rep("black",n.lines)
c.tmp[tmp.pd2<0.05]<-"red"
png(paste(ofname,".png",sep=""),width=720,height=560)
plot(xv[1,]~x.tmp,type="l",lwd=0.2,xlim=c(1,ncol(xv)),ylim=c(y.min,y.max),col=c.tmp[1],ylab="",xlab="",xaxt="n")
for (i in (2:n.lines)) {
  par(new=TRUE)
  plot(xv[i,]~x.tmp,type="l",lwd=0.2,xlim=c(1,ncol(xv)),ylim=c(y.min,y.max),col=c.tmp[i],ylab="",xlab="",xaxt="n")
}
text(x = x.tmp, par("usr")[3] - 6, labels = xvar, srt = 90, pos = 1, xpd = TRUE)
dev.off()
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,"<h2>Mahalanobis 距离（异常值确定）</h2>")
w<-c(w,paste("</br>总记录数:",nobs))
w<-c(w,paste("</br>排除含缺失值的记录:",ncmp))
w<-c(w,"</br></br><table border=3>", mat2htmltable(oo1), "</table>")
w<-c(w,"</br> Mahalanobis 距离(D2)最大的10条记录：<table border=3>",mat2htmltable(tt.sort),"</table>")
w<-c(w,"</br> 下列记录的 Mahalanobis 距离(D2)的p值<0.05：<table border=3>",mat2htmltable(tmp.ttslt),"</table>")
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)
