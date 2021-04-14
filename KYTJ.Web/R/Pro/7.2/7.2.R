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

library(nparcomp,lib.loc=R.LibLocation)
library(gdata,lib.loc=R.LibLocation)
library(stringr,lib.loc=R.LibLocation)
 
ofname<-"7_2"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"非参数方差分析"; 
attach(WD) 
subjvname<-NA;

if(inumber==1)  {
  xv<-as.matrix(idata[ixn1]); 
  xvname<-c(ixn1); 
  xvar<-c(ixv1); 
  xlv<-c(NA,ixs1)[-1];
}else if(inumber==2)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  xvname<-c(ixn1,ixn2); 
  xvar<-c(ixv1,ixv2); 
  xlv<-c(NA,ixs1,ixs2)[-1];
}else if(inumber==3)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  xvname<-c(ixn1,ixn2,ixn3); 
  xvar<-c(ixv1,ixv2,ixv3); 
  xlv<-c(NA,ixs1,ixs2,ixs3)[-1];
}else if(inumber==4)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1];
}else if(inumber==5)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4,ixn5); 
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
##R package## nparcomp gdata stringr ##R package##;
pvformat<-function(p,dec) {
  p1<-round(as.numeric(p),dec); pp<-p1
  tmp<-(substr(p1,2,9)=="e-04" & !is.na(p1))
  pp[tmp]<-paste("0.000",substr(pp[tmp],1,1),sep="")
  tmp<-(p1==0 & !is.na(p1))
  pp[tmp]<-paste(c("<0.",rep("0",dec-1),"1"),collapse="")
  tmp<-(p1>0 & !is.na(p1))
  pp[tmp]<-paste(pp[tmp],substr(rep("0000000",length(pp[tmp])),1,dec+2-nchar(pp[tmp])),sep="")
  tmp<-(p1==1 & !is.na(p1))
  pp[tmp]<-substr("0.999999999",1,dec+2)
  return(pp)
}
numfmt<-function(p,dec) {
  if (is.list(p)) p<-as.matrix(p)
  if (is.matrix(p)) {nr<-nrow(p);} else {nr<-1;}
  p1<-round(as.numeric(p),dec); 
  p2<-p1-floor(p1);
  tmp<-(p2==0 & !is.na(p2))
  p1[tmp]<-paste(p1[tmp],".0",sep="")
  p2[tmp]<-"0.0"; 
  p1<-paste(p1,substr(rep("0000000",length(p1)),1,dec+2-nchar(p2)),sep="")
  p1[as.numeric(p)>10000000]<-"inf."
  if (nr>1) {
    p1<-matrix(p1,nrow=nr);colnames(p1)<-colnames(p);rownames(p1)<-rownames(p)
  }
  return(p1)
}
aovMeanSd <- function(v,g) {
  N<-tapply(v,g,function(x) sum(!is.na(x)))
  Mean<-tapply(v,g,function(x) numfmt(mean(x,na.rm=TRUE),dec))
  Sd<-tapply(v,g,function(x) numfmt(sd(x,na.rm=TRUE),dec))
  tmp.qq<-tapply(v,g,function(x) quantile(x, probs=c(0,0.05,0.10,0.25,0.50,0.75,0.90,0.95,1), na.rm=TRUE))
  QQ<-numfmt(t(matrix(unlist(tmp.qq),nrow=9)),dec)
  tmp.o<-cbind(N,Mean,Sd,QQ)
  colnames(tmp.o)<-c("N","Mean","Sd","Min","5%","10%","Q1","Median","Q3","90%","95%","Max")
  return (tmp.o)
}
aovMeanSd2 <- function(v,g,g2) {
  g2.level <- names(table(g2))
  for (i in (1:length(g2.level))) {
    tmp.i <- aovMeanSd(v[g2==g2.level[i]],g[g2==g2.level[i]])
    tmp.i <- cbind(rep(as.numeric(g2.level[i]),nrow(tmp.i)),tmp.i)
    if (i==1) tmp.oo<-tmp.i
    if (i>1) tmp.oo<-rbind(tmp.oo,tmp.i)
  }
  return (tmp.oo)
}
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
mat2htmltable0<-function(mat) {
  mat<-rbind(colnames(mat),mat);
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste(paste("<tr><td>",t1,"</td></tr>"),collapse=" ")
  return(paste("</br><table border=3>",t2,"</table></br>"))
}
cleantmpd<-function(tmpd) {
  tmptb<-table(tmpd[,colvname])
  v2use<-as.numeric(names(tmptb[tmptb>30]))
  tmpd<-tmpd[(tmpd[,colvname] %in% v2use),]
  return (tmpd);
}
nparmc<-function(i,g) {
  fml<-paste(xvname[i],"~",colvname);
  oox<-"";
  if (!is.na(g)) {
    pngf<-paste(ofname,xvname[i],bvar,g,sep="_")
    tmpwd<-WD[(WD[,bvar]==bv.lv[g] & !is.na(WD[,xvname[i]])),]
    oox<-c(oox,paste("</br>For subset:",bvb,"=",bv.lb[g],"</br>"))
  } else {
    pngf<-paste(ofname,xvname[i],sep="_")
    tmpwd<-WD[(!is.na(WD[,xvname[i]])),]
  }
  tmpwd<-cleantmpd(tmpwd);
  nlv <- length(table(tmpwd[,colvname]));
  if (nlv>2) {
    m<-mctp(formula(fml),data=tmpwd,type="Tukey",alternative="two.sided",asy.method="fisher",correlation=TRUE);
    o1<- m$Data.Info
    o1[,1]<-colv.lb[match(o1[,1],colv.lv)]
    o1[,(3:ncol(o1))]<-numfmt(o1[,(3:ncol(o1))],4)
    oox<-c(oox,"</br>Statistics",mat2htmltable0(o1));
    o2<- m$Analysis; o2[,(1:4)]<-numfmt(o2[,(1:4)],4); o2[,5]<-pvformat(o2[,5],4)
    o2rname<-rownames(o2)
    o2<-cbind(matrix(as.numeric(unlist(strsplit(o2rname,split="-"))),ncol=2,byrow=TRUE),o2)
    o2[,1]<-colv.lb[match(o2[,1],colv.lv)]
    o2[,2]<-colv.lb[match(o2[,2],colv.lv)]
    oox<-c(oox,"Nonparametric Multiple Comparisons",mat2htmltable0(o2));
    o3<- m$Overall
    oox<-c(oox,paste("Overall test: Quantile=",numfmt(o3$Quantile,4)," P-value=",pvformat(o3$p.Value,4),"</br>",sep=" "))
    o4<-numfmt(m$Correlation,4);
    o4<-cbind(rownames(o4),o4)
    colnames(o4)[1]<-"";
    oox<-c(oox,"</br>Correlation matrix:",mat2htmltable0(o4));
    png(pngf,width=560,height=720)
    plot(m)
    dev.off()
  }
  if (nlv==2) {
    m<-npar.t.test(formula(fml),data=tmpwd,method="t.app");
    o1<- m$Info 
    o1[,1]<-colv.lb[match(o1[,1],colv.lv)]
    oox<-c(oox,"</br>Sample size",mat2htmltable0(o1));
    o2<- matrix(m$Analysis,nrow=1);
    o2[,1]<-paste(colv.lb,collapse=" vs. ")
    o2[,c(2,3,4,5)]<-numfmt(o2[,c(2,3,4,5)],4);
    o2[,6]<-pvformat(o2[,6],4)
    colnames(o2)<-c("","Estimator","Lower","Upper","T","P.value")
    oox<-c(oox,"Nonparametric: Brunner-Munzel-T-Approx test",mat2htmltable0(o2));
  }
  return(oox);
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
if (!is.na(colvname)) {
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  colv.lv<-levels(factor(colv))
  colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
  colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
}
if (!is.na(bvar)) {
 bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
 bv.lv<-levels(factor(bv)); 
 bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
 bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
 nbg<-length(bv.lv)
}

if (is.na(colvname)) {
  if (!is.na(bvar)) {
    colvname <- bvar; colv.lv<-bv.lv; colv.lb<-bv.lb; colvb<-bvb; bvar<-NA; 
  } else {
    tmpy<-as.vector(xv); tmpx<-as.vector(t(matrix(rep(1:nx,nrow(xv)),nrow=ncol(xv))))
    detach(WD); rm(WD);
    WD<-data.frame(Value=tmpy, Variables=tmpx);
    attach(WD);
    colvname<-"Variables"; colv.lb<-xb; colv.lv<-(1:nx); colvb<-"Variables";
    xvname<-"Value"; xb<-"Variables"; nx<-1;
  }
}
WD<-WD[!is.na(WD[,colvname]),]

oo<-""; 
for (i in (1:nx)) {
  oo<-c(oo,paste("</br>Nonparametric multiple comparison for",xb[i],"</br>"))
  if (!is.na(bvar)) {
    qqi<-aovMeanSd2(WD[,xvname[i]],WD[,colvname],WD[,bvar]); 
    qqi<-cbind(qqi[,1],colv.lb,qqi[,-1])
    qqi<-rbind(c(bvb,colvb,"N","Mean","Sd","Min","5%","10%","Q1","Median", "Q3","90%","95%","Max"), qqi)
    if (nx>1) qqi<-rbind(c(xb[i],rep(" ",13)),qqi)
    if (i>1) {qq<-rbind(qq,qqi);} else {qq<-qqi;}
    for (j in (1:nbg)) oo<-c(oo,nparmc(i,j))          
  } else {
     qqi<-aovMeanSd(WD[,xvname[i]],WD[,colvname]); 
     qqi<-cbind(colv.lb,qqi);
     qqi<-rbind(c(colvb,"N","Mean","Sd","Min","5%","10%","Q1","Median","Q3","90%","95%","Max"), qqi)
     if (nx>1) qqi<-rbind(c(xb[i],rep(" ",12)),qqi)
     if (i>1) {qq<-rbind(qq,qqi);} else {qq<-qqi;}
     oo<-c(oo,nparmc(i,NA))
  }
}
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,"</br>Statistics: </br><table border=3>",mat2htmltable(qq),"</table></br>") 
w<-c(w,oo) 
w<-c(w,"</br>Method: Fisher (for multiple comparisons); Brunner-Munzel t approximation (for two sample comparison)")
w<-c(w,"</br>Reference: Frank Konietschke, Marius Placzek, Frank Schaarschmidt, Ludwig A. Hothorn. nparcomp: An R Software Package for Nonparametric Multiple Comparisons and Simultaneous Confidence Intervals. Journal of Statistical Software, March 2015, 64(9))")
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)