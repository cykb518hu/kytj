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

library(stats,lib.loc=R.LibLocation)
 
ofname<-"7_1"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"两样本比较的非参数U检验"; 
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
colv<-idata[,iyn1];colvname<-c(iyv1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-1;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## stats ##R package##;
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
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  t3<- paste(t2,collapse="");
  return(t2)
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
wtest2mat<-function(tx) {
  oo<-c(round(tx$statistic,dec),pvformat(tx$p.value,4),tx$alternative,tx$method)
  return(rbind(c("W","p.value","alternative","method"),oo))
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
nx<-ncol(xv); tmp.n<-nrow(xv)
x.lb<-vlabelV[match(xvname,vnameV)]; x.lb[is.na(x.lb)]<-xvname[is.na(x.lb)]
if (!is.na(bvar)) {
 bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
 bv.lv<-levels(factor(bv)); 
 bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
 bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
 nbg<-length(bv.lv)
}
if (!is.na(colvname)) {
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  colv.lv<-levels(factor(colv))
  colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
  colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
}
if (is.na(par1)) par1<-1;
ah1<-c("t","g","l")[par1];
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")


w<-c(w,"<h2>两样本比较的非参数 U 检验</h2>")
if (is.na(colvname)) {
  if (nx==2) {
    tmp.yvar=c(xv[,1],xv[,2]); tmp.group<-c(rep(1,tmp.n),rep(2,tmp.n))
    if (is.na(bvar)) {
      tmp.nms<-aovMeanSd(tmp.yvar,tmp.group); 
      tmp.nms<-cbind(c(" ",xvname),rbind(colnames(tmp.nms),tmp.nms));
      w<-c(w,"样本量、均数、标准差:</br><table border=3>",mat2htmltable(tmp.nms),"</table></br>")
      tmp.t1<-wilcox.test(xv[,1],xv[,2],alternative=ah1,paired=TRUE)
      tmp.t2<-wilcox.test(xv[,1],xv[,2],alternative=ah1,paired=FALSE)
      tmp.tt<-cbind(c(" ","两变量配对比较","两变量非配对比较"),rbind(wtest2mat(tmp.t1),wtest2mat(tmp.t2)[2,]))
      w<-c(w,paste(x.lb[1], "vs.", x.lb[2]))
      w<-c(w,"两变量比较的非参数 U 检验:</br><table border=3>",mat2htmltable(tmp.tt),"</table></br>")
    } else {
      tmp.nms<-aovMeanSd2(tmp.yvar,tmp.group,c(bv,bv)); 
      rownames(tmp.nms)<-rep(xvname,nbg);    colnames(tmp.nms)[1]<-bvb
      tmp.nms<-cbind(c(" ",rownames(tmp.nms)),rbind(colnames(tmp.nms),tmp.nms))
      w<-c(w,"样本量、均数、标准差:</br><table border=3>",mat2htmltable(tmp.nms),"</table></br>")
      tmp.m<-NA;tmp.names<-NA
      for (j in (1:nbg)) {
        tmp.t1<-t.test(xv[bv==bv.lv[j],1],xv[bv==bv.lv[j],2],alternative=ah1,paired=TRUE)
        tmp.t2<-t.test(xv[bv==bv.lv[j],1],xv[bv==bv.lv[j],2],alternative=ah1,paired=FALSE)
        tmp.tt<-cbind(c(" ","两变量配对比较","两变量非配对比较"),rbind(wtest2mat(tmp.t1),wtest2mat(tmp.t2)[2,]))
        w<-c(w,paste(x.lb[1], "vs.", x.lb[2]))
        w<-c(w,"</br>两变量比较的非参数 U 检验:",paste(bvb,bv.lb[j],sep="="),"</br><table border=3>",mat2htmltable(tmp.tt),"</table></br>")
        tmp.m<-cbind(tmp.m,tmp.t2$estimate);tmp.names<-c(tmp.names,paste(x.lb,bv.lb[j],sep="\n"))
      }
    }
  }
} else {
  if (length(colv.lv)==2) {
    for (i in (1:nx)) {
    if (is.na(bvar)) {
      tmp.nms<-aovMeanSd(xv[,i],colv); rownames(tmp.nms)<-colv.lb; 
      tmp.nms<-cbind(c(colvb,rownames(tmp.nms)),rbind(colnames(tmp.nms),tmp.nms))
      w<-c(w,"样本量、均数、标准差:", x.lb[i],"</br><table border=3>",mat2htmltable(tmp.nms),"</table></br>")
      tmp.t1<-t.test(xv[colv==colv.lv[1],i],xv[colv==colv.lv[2],i],alternative=ah1)
      w<-c(w, paste(colvb,"=",paste(colv.lb,collapse=" vs. ")))
      w<-c(w,"</br>两样本比较的非参数 U 检验:",x.lb[i],"</br><table border=3>",mat2htmltable(wtest2mat(tmp.t1)),"</table></br>")
    } else {
      tmp.nms<-aovMeanSd2(xv[,i],colv,bv); 
      rownames(tmp.nms)<-rep(colv.lb,nbg); colnames(tmp.nms)[1:2]<-bvb
      tmp.nms<-cbind(c(colvb,rownames(tmp.nms)),rbind(colnames(tmp.nms),tmp.nms))
      w<-c(w,"样本量、均数、标准差:", x.lb[i],"</br><table border=3>",mat2htmltable(tmp.nms),"</table></br>")
      for (j in (1:nbg)) {
        tmp.t1<-t.test(xv[bv==bv.lv[j] & colv==colv.lv[1],i],xv[bv==bv.lv[j] & colv==colv.lv[2],i],alternative=ah1)
        w<-c(w,paste(bvb,bv.lb[j],sep=" = "))
        w<-c(w,"</br>",paste(colvb,"=",paste(colv.lb,collapse=" vs. ")))
        w<-c(w,"</br>两样本比较的非参数 U 检验:",x.lb[i],"</br><table border=3>",mat2htmltable(wtest2mat(tmp.t1)),"</table></br>")
      }
    }
    }
  } else {w<-c(w,"只适用于两组的比较")}
}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)