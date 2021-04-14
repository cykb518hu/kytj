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
ixn1<-as.character(a[1,2]);ixv1<-as.character(a[1,3]);ixs1<-as.numeric(a[1,4])
ixn2<-as.character(a[1,5]);ixv2<-as.character(a[1,6]);ixs2<-as.numeric(a[1,7])
ixn3<-as.character(a[1,8]);ixv3<-as.character(a[1,9]);ixs3<-as.numeric(a[1,10])
ixn4<-as.character(a[1,11]);ixv4<-as.character(a[1,12]);ixs4<-as.numeric(a[1,13])
ixn5<-as.character(a[1,14]);ixv5<-as.character(a[1,15]);ixs5<-as.numeric(a[1,16])
ixn6<-as.character(a[1,17]);ixv6<-as.character(a[1,18]);ixs6<-as.numeric(a[1,19])
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
}

library(nortest,lib.loc=R.LibLocation)
library(mvnormtest,lib.loc=R.LibLocation)
 
ofname<-"8_1"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"多元正态性检验"; 
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
}

sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## nortest mvnormtest ##R package##;
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  t3<- paste(t2,collapse="");
  return(t2)
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
tmp.xx<- t(xv[apply(is.na(xv),1,sum)==0,])
tmp.xb<-vlabelV[match(xvname,vnameV)];
tmp.xb[tmp.xb==xvname]<-" "; tmp.xb[is.na(tmp.xb)]<-" ";
tmp.xb<-paste(xvname,tmp.xb,sep=": ")
tb<-cbind(c(" ",tmp.xb),rbind(c("N", "Mean","Std"),cbind(ncol(tmp.xx),apply(tmp.xx,1,mean),apply(tmp.xx,1,sd))))
tt<-mshapiro.test(tmp.xx)
tt<-rbind(c("statistic","p.value","method"),c(tt$statistic,tt$p.value,tt$method))
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,"<h2>多元正态性检验</h2>")
w<-c(w,"Basic statistics</br><table border=3>",mat2htmltable(tb),"</table></br>")
w<-c(w,"Multivariate normality test</br><table border=3>",mat2htmltable(tt),"</table></br>")
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)
