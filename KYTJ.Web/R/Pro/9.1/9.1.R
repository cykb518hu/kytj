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

library(HardyWeinberg,lib.loc=R.LibLocation)
 
ofname<-"9_1"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"Hardy-Weinberg平衡检验"; 
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
##R package## HardyWeinberg ##R package##;
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
if (is.na(par1)) par1<-2
if (is.na(bvar)) {
  bv<-rep(1,nrow(xv));bgp<-1;
} else {
  bvlbl<-vlabelV[match(bvar,vnameV)]; if (is.na(bvlbl)) bvlbl<-bvar; 
  bgp<-levels(factor(bv));
  bglbl<-vlabelZ[match(paste(bvar,bgp,sep="."),vnameZ)];
  bglbl[is.na(bglbl)]<-bgp[is.na(bglbl)]
}
nbgp<-length(bgp)
for (g in (1:nbgp)) {
  xx<-xv[bv==bgp[g],]; if (!is.matrix(xx)) xx<-matrix(xx,ncol=1)
  if (par1==2) {
    nsnp<-ncol(xv)/2;
    for (i in (1:nsnp)) {
      snpi<-cbind(xx[,c(i*2-1,i*2)])
      snpi<-snpi[apply(is.na(snpi),1,sum)==0,]
      ffsnpi<-table(apply(snpi,1,function(x) {return(ifelse(x[1]>x[2],paste(x[2],x[1]),paste(x[1],x[2])));}))
      hwi<-HWChisq(as.vector(ffsnpi),cc=0.5)
      hwi.oo<-format(round(c(hwi$pval,hwi$chisq,hwi$D,hwi$p),5),nsmall=5)
      ooi<-c(paste(colnames(snpi)),as.vector(ffsnpi),hwi.oo,names(ffsnpi[2]))
      if (i==1) {ffmat<-ooi;} else {ffmat<-rbind(ffmat,ooi);}
    }
  } else {
    nsnp<-ncol(xv);
    for (i in (1:nsnp)) {
      ffsnpi<-table(xx[,i])
      hwi<-HWChisq(as.vector(ffsnpi),cc=0.5)
      hwi.oo<-format(round(c(hwi$pval,hwi$chisq,hwi$D,hwi$p),5),nsmall=5)
      allestr<-vlabelZ[match(paste(colnames(xv)[i],"1",sep="."),vnameZ)]
      if (is.na(allestr)) allestr="1"
      ooi<-c(colnames(xv)[i],as.vector(ffsnpi),hwi.oo,allestr)
      if (i==1) {ffmat<-ooi;} else {ffmat<-rbind(ffmat,ooi);}
    }
  }
  if (!is.matrix(ffmat)) ffmat<-matrix(ffmat,nrow=1)
  if (nbgp>1) ffmat=rbind(c(paste(bvlbl,bglbl[g],sep="="),rep(" ",(ncol(ffmat)-1))),ffmat)
  if (g==1) {oo<-ffmat;} else {oo<-rbind(oo,ffmat);}
}


if (par1==2) {cname<-c("等位基因变量1","等位基因变量2");} else {cname<-"基因型变量";}
cname<-c(cname,"#AA","#AB","#BB","P 值","卡方值","D","等位基因1频率","等位基因");
oo<-rbind(cname,oo);
w<-c(w,"Hardy-Weinberg 平衡检验<table border=2 bgcolor=\"wheat\">")
w<-c(w,paste("<tr><td>",apply(oo,1,function(x) paste(x,collapse="</td><td>")),"</td></tr>"))
w<-c(w,"</table>")
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)