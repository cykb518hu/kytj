args <- commandArgs()

rlib<-args[6]	#libary path
if (is.na(rlib)) {output<-args[6]} else {output<-args[7]}	#param path
print(output)
#output<-c('D:\\Works\\产品\\科研\\代码\\ScienceStatistics.Web\\Output\\1\\7_3\\636535276820969360')
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

if(inumber==1)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1)
}else if(inumber==2)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2)
}else if(inumber==3)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3)
}else if(inumber==4)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4)
}else if(inumber==5)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)
}

library(np,lib.loc=R.LibLocation)
 
ofname<-"7_3"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"非参数曲线回归"; 
attach(WD) 
subjvname<-NA; 
if(inumber==1)  {
  xv<-as.matrix(idata[ixn1]); 
  xvname<-c(ixn1); 
  xvar<-c(ixn1); 
  xlv<-c(NA,ixs1)[-1];
}else if(inumber==2)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  xvname<-c(ixn1,ixn2); 
  xvar<-c(ixn1,ixn2); 
  xlv<-c(NA,ixs1,ixs2)[-1];
}else if(inumber==3)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  xvname<-c(ixn1,ixn2,ixn3); 
  xvar<-c(ixn1,ixn2,ixn3); 
  xlv<-c(NA,ixs1,ixs2,ixs3)[-1];
}else if(inumber==4)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4); 
  xvar<-c(ixn1,ixn2,ixn3,ixn4); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1];
}else if(inumber==5)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4,ixn5); 
  xvar<-c(ixn1,ixn2,ixn3,ixn4,ixn5); 
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
##R package## np ##R package##;
printTxt<-function(txt,cname,rname) {
  tmp<-as.matrix(txt,ncol=1); rname1<-rname
  if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
  colnames(tmp)=cname;  rownames(tmp)=rname1; print(tmp,quote=F)
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
if (!is.na(bvar)) {
 bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
 bv.lv<-levels(factor(bv)); 
 bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
 bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
 nbg<-length(bv.lv)
} else {nbg<-1;}

ylv<-levels(factor(colv)); ylv<-ylv[!is.na(ylv)]; nylv<-length(ylv)
yb<-vlabelV[match(colvname,vnameV)]; if (is.na(yb)) yb<-colvname;
xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
xname<-xvname; nx<-length(xvname)
xname[xlv>1]<-paste("factor(",xname[xlv>1],")",sep="")
fml<-paste(colvname,"~",paste(xname,collapse="+"),sep="")

sink(paste(ofname,".htm",sep=""))
printTxt("<html><head><meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>","","")
printTxt("<h2>Multivariate nonparametric (Kernel) smoothing regression</h2>","","")
printTxt(paste("</br>Outcome:", yb, "</br>"),"","")
if (nylv>0 & nylv<7) {
  printTxt("Outcome should be continuous</br>","",""); sink();
} else {
 sink();
 for (i in (1:nbg)) {
  if (nbg>1) {
     tmpd<-WD[bv==bv.lv[i],];
     sink(paste(ofname,".htm",sep=""),append=TRUE)
     printTxt(paste(bvar,"=",bv.lv[i],"</br>"),"","")
     sink()
     tmp.pname<-paste(ofname,bvar,bv.lv[i],sep=".");
  } else {tmpd<-WD;tmp.pname=ofname;} 
  bw.all<-npregbw(formula(fml),regtype="ll", bwmethod="cv.aic",data=tmpd)
  np.mdl<-npreg(bws=bw.all)
  np.sig<-npsigtest(np.mdl)
  sink(paste(ofname,".htm",sep=""),append=TRUE)
  printTxt("<textarea rows=35 cols=100>","","")
  print(summary(np.mdl))
  print(np.sig)
  printTxt("</textarea></br>","","")
  sink()
  png(paste(tmp.pname,"png",sep="."),width=720,height=560)
  plot(np.mdl,view="fixed",plot.errors.method="asymptotic")
  dev.off()
  pdf(paste(tmp.pname,"pdf",sep="."),width=pdfwd,height=pdfht,family="Helvetica");
  plot(np.mdl,view="fixed",plot.errors.method="asymptotic")
  dev.off()
 }
}
sink(paste(ofname,".htm",sep=""),append=TRUE)
printTxt("</body></html>","","")
sink()
