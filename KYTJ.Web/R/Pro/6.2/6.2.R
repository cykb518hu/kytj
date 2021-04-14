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

inumber<-as.numeric(a[1,1]);p1<-as.numeric(a[1,2])
iyn1<-as.character(a[1,3]);iyv1<-as.character(a[1,4]);iys1<-as.numeric(a[1,5])
itn1<-as.character(a[1,6]);itv1<-as.character(a[1,7]);its1<-as.numeric(a[1,8])
ixn1<-as.character(a[1,9]);ixv1<-as.character(a[1,10]);ixs1<-as.numeric(a[1,11])
ixn2<-as.character(a[1,12]);ixv2<-as.character(a[1,13]);ixs2<-as.numeric(a[1,14])
ixn3<-as.character(a[1,15]);ixv3<-as.character(a[1,16]);ixs3<-as.numeric(a[1,17])
ixn4<-as.character(a[1,18]);ixv4<-as.character(a[1,19]);ixs4<-as.numeric(a[1,20])
ixn5<-as.character(a[1,21]);ixv5<-as.character(a[1,22]);ixs5<-as.numeric(a[1,23])

if(inumber==1)  {idata<-idata[,c(iyn1,itn1,ixn1)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,itn1,ixn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,itn1)
}else if(inumber==2)  {idata<-idata[,c(iyn1,itn1,ixn1,ixn2)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,itn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,itn1)
}else if(inumber==3)  {idata<-idata[,c(iyn1,itn1,ixn1,ixn2,ixn3)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,itn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,itn1)
}else if(inumber==4)  {idata<-idata[,c(iyn1,itn1,ixn1,ixn2,ixn3,ixn4)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,itn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,itn1)
}else if(inumber==5)  {idata<-idata[,c(iyn1,itn1,ixn1,ixn2,ixn3,ixn4,ixn5)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,itn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,itn1)
}

library(survival,lib.loc=R.LibLocation)
library(gdata,lib.loc=R.LibLocation)
library(Hmisc,lib.loc=R.LibLocation)
library(survey,lib.loc=R.LibLocation)
 
ofname<-"6_2"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"Cox模型"; 
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

sxf<-c(NA,0,0,0,0)[-1]; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-idata[,itn1];timevname<-c(itv1); 
bv<-NA; bvar<-NA; 
colv<-idata[,iyn1];colvname<-c(iyv1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-1;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## survival gdata Hmisc survey ##R package##;
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
  p1[is.na(p) | p=="" | p==" "]<-""
  if (is.matrix(p)) {
    p1<-matrix(p1,nrow=nr);colnames(p1)<-colnames(p);rownames(p1)<-rownames(p)
  }
  return(p1)
}
getLandmarkDF <- function(yvname, WD, timevname, tp) {
  ny<-length(yvname); tp<-c(0, tp, max(WD[,timevname],na.rm=TRUE)); ntp<-length(tp)-1;
  tmpWD<-WD[!is.na(WD[,timevname]),]
  for (i in (1:ntp)) {
    wd.i<-tmpWD[tmpWD[,timevname]>tp[i],]
    for (j in (1:ny)) {wd.i[wd.i[,yvname[j]]==1 & wd.i[,timevname]>tp[i+1],yvname[j]]<-0;}
    wd.i[wd.i[,timevname]>tp[i+1],timevname]<-tp[i+1]
    wd.i<-cbind(wd.i, TIME.PHASE.4LMCOX=i)
    if (i==1) {wd.p<-wd.i;} else {wd.p<-rbind(wd.p,wd.i);}
  }
  return(wd.p)
}
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
runcoxfml<-function(fml,fm12,ofname,xxstr) {
 if (svy.DSN.YN) {
   print(summary(svy.DSN));
   tmp.cox<-svycoxph(formula(fml),svy.DSN, method=mth,data=WD1); 
 } else {
   tmp.cox<-coxph(formula(fml),method=mth,data=WD1); 
 }
 tmp.zph <- cox.zph(tmp.cox)
 martingale<-resid(tmp.cox,type="martingale")
 deviance<-resid(tmp.cox,type="deviance")
 res.score<-resid(tmp.cox,type="score")
 if (!is.matrix(res.score)) {
   res.score<-matrix(res.score,ncol=1); colnames(res.score)<-"score";
 } else {
   colnames(res.score)<-paste("score",colnames(res.score),sep=".")
 }
 resid<-cbind(martingale,deviance,res.score)
 if (nrow(resid)==nrow(WD1)) resid<-cbind(WD1[,wd.tmp],resid)
 write.table(resid,file=paste(ofname,"_resid.xls",sep=""),col.names=TRUE,row.names=FALSE,sep="\t")
 schoenfeld<-resid(tmp.cox,type="schoenfeld")
 event.time<-as.numeric(rownames(schoenfeld))
 schoenfeld<-cbind(event.time,schoenfeld)
 write.table(schoenfeld,file=paste(ofname,"_schoenfeld.xls",sep=""),col.names=TRUE,row.names=FALSE,sep="\t")
 nx<-length(xvname)
 for (i in (1:nx)) {
  png(paste(ofname,xvname[i],"martingale.png",sep="_"),width=720,height=560)
  plot(WD1[,xvname[i]],martingale, xlab=xb[i], ylab="Martingale residual"); lines(lowess(WD1[,xvname[i]],martingale),col="red")
  dev.off()
 }
 if (fml2>"") {
   sfit<-survfit(formula(fml2),data=WD1)
   tmp.miny<-floor(min(sfit$lower,na.rm=TRUE)*10)/10
   png(paste(ofname,"_survfit.png",sep=""),width=720,height=560)
   if (!is.na(vname.stop)) {
     plot(sfit,lty=(1:nbg),col=(2:(nbg+1)),ylim=c(tmp.miny,1.05),ylab="Survival",xlab=timevb,main=colvb)
     legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
   } else {
     plot(sfit,ylim=c(tmp.miny,1),ylab="Survival",xlab=timevb,main=colvb)
   }
   dev.off()
 }
 ss<-summary(tmp.cox); print(fml); print(ss);
 coe<-ss$coefficients; if (!is.matrix(coe)) coe<-matrix(coe,nrow=1)
 xxname<-rownames(coe)
 coe0<-cbind(coe[,1],coe[,3],coe[,4],coe[,2],1/coe[,2], exp(coe[,1]-1.96*coe[,3]), exp(coe[,1]+1.96*coe[,3]))
 if (!is.matrix(coe0)) coe0<-matrix(coe0,nrow=1)
 coe<-cbind(numfmt(coe0,4),pvformat(coe[,5],4))
 if (!is.matrix(coe)) coe<-matrix(coe,nrow=1)
 zph<-numfmt(tmp.zph$table,4)
 oo1<-cbind(xxstr[[1]],c(ss$n,ss$nevent))
 oo2<-rbind(xxstr[[2]],cbind(xxname,coe))
 oo3<-paste(paste(names(ss$rsq),"=",numfmt(ss$rsq,4)),collapse=", ")
 oo4<-paste(xxstr[[4]],ss$loglik[2])
 oo5<-numfmt(rbind(ss$logtest,ss$waldtest,ss$sctest),5)
 oo5<-cbind(c(" ","Likelihood ratio test","Wald test","Score(log rank)test"),rbind(colnames(oo5),oo5))
 oo6<-cbind(c("",rownames(zph)),rbind(colnames(zph),zph))
 hc<-rcorrcens(formula(paste(survStr,"~I(-1*predict(tmp.cox))")))
 oo7<-rbind(c(" ",colnames(hc)),cbind("Model",round(hc,4)))

 ww<-"";
 if (svy.DSN.YN) ww<-c(ww, "</br></br>Survey design (check .log or .lst for details)");
 ww<-c(ww,"</br></br>model: ", fml);
 ww<-c(ww,"</br></br><table border=3>",mat2htmltable(oo1),"</table>")
 ww<-c(ww,"</br>",xxstr[[3]],"<table border=3>",mat2htmltable(oo2),"</table>")
 if (!svy.DSN.YN) ww<-c(ww,paste("</br>",oo3))
 if (!svy.DSN.YN) ww<-c(ww,paste("</br>",oo4))
 if (!svy.DSN.YN) ww<-c(ww,"</br><table border=3>",mat2htmltable(oo5),"</table>")
 ww<-c(ww,"</br>",xxstr[[5]],"<table border=3>",mat2htmltable(oo6),"</table></br>")
 ww<-c(ww,"</br>Somers' Rank Correlation for Censored Data</br><table border=3>",mat2htmltable(oo7),"</table></br>")
 ww<-c(ww,"C: Harrell\'s C index; Dxy = 2(C - 0.5), Dxy is a U-statistic.")
 ww<-c(ww,"</br>SD: Standard error of Dxy, so, SE. for C = SD/2 </br>")
 
 if (is.na(vname.stop) & (nx>1)) {
   tmp.stp<-step(tmp.cox)
   ww<-c(ww,"</br>Step wise model selection<table border=3>",mat2htmltable(as.matrix(tmp.stp$anova)), "</table></br>")
   cos<-summary(tmp.stp)$coefficients
   if (length(cos)>0) {
     if (!is.matrix(cos)) cos<-matrix(cos,nrow=1)
     xxname<-rownames(cos)
     cos<-cbind(cos[,1],cos[,3],cos[,4],cos[,2],1/cos[,2], exp(cos[,1]-1.96*cos[,3]), exp(cos[,1]+1.96*cos[,3]), cos[,5])
     cos<-numfmt(cos,4)
     oos<-rbind(xxstr[[2]],cbind(xxname,cos))
     ww<-c(ww,"</br>Final model:<table border=3>",mat2htmltable(oos),"</table></br>")
   }
 }
 return(ww)
}

WD<-WD[!is.na(WD[,colvname]) & !is.na(WD[,timevname]),]

if (!is.na(bvar)) {
 if (bvar=="TIME.PHASE.4LMCOX") {
  WD<-getLandmarkDF(colvname, WD, timevname, tp);
  tmptimeb <- vlabel[vname==timevname];
  vname<-c(vname,"TIME.PHASE.4LMCOX", "TIME.PHASE.4LMCOX.1");
  vlabel<-c(vlabel, paste(tmptimeb, "segment"), paste("  <=", tp[1]));
  if (length(tp)>1) {
    vname<-c(vname, "TIME.PHASE.4LMCOX.2", "TIME.PHASE.4LMCOX.3");
    vlabel<-c(vlabel,  paste("  >", tp[1], " & <=", tp[2],sep=""),paste("  >", tp[2]));
  } else {
    vname<-c(vname, "TIME.PHASE.4LMCOX.2");
    vlabel<-c(vlabel,  paste("  >", tp[1]));
  }
  bv <- WD[,"TIME.PHASE.4LMCOX"];
}}

vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
tmp.vv<-cbind(colv,xv);
if (!is.na(vname.stop)) {
 bvb<-vlabelV[match(vname.stop,vnameV)]; if (is.na(bvb)) bvb<-vname.stop; 
 bv.lv<-levels(factor(WD[,vname.stop])); 
 bv.lb<-vlabelZ[match(paste(vname.stop,bv.lv,sep="."),vnameZ)]
 bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
 nbg<-length(bv.lv)
 sxf<-sxf[xvname!=vname.stop];xlv<-xlv[xvname!=vname.stop];xvname<-xvname[xvname!=vname.stop]
}
if (!is.na(bvar)) {
  stvb<-vlabelV[match(bvar,vnameV)]; if (is.na(stvb)) stvb<-bvar; 
  stv.lv<-levels(factor(WD[,bvar]));
  stv.lb<-vlabelZ[match(paste(bvar,stv.lv,sep="."),vnameZ)]
  stv.lb[is.na(stv.lb)]<-stv.lv[is.na(stv.lb)]
  nstg<-length(stv.lv)
  sxf<-sxf[xvname!=bvar];xlv<-xlv[xvname!=bvar];xvname<-xvname[xvname!=bvar]
}
mth<-"efron"; if (!is.na(par1)) mth<-c("efron","breslow","exact")[par1]
colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
timevb<-timevname;
if (!is.na(timevname)) {
 timevb<-vlabelV[match(timevname,vnameV)]; if (is.na(timevb)) timevb<-timevname;
}
if (!is.na(vname.start)) {
 tvb1<-vlabelV[match(vname.start,vnameV)]; if (is.na(tvb1)) tvb1<-vname.start;
 timevb<-paste(tvb1,"-",timevb);
}
xv1<-xvname; xv1[xlv>2]<-paste("factor(",xvname[xlv>2],")",sep="")

fml<-""
if (!is.na(vname.start)) {
  fml<-paste("Surv(",vname.start,",",timevname,",",colvname,")~",paste(xv1,collapse="+"))
  fml2<-paste("Surv(",vname.start,",",timevname,",",colvname,")");
  wd.tmp<-c(xvname,colvname,vname.start,timevname)
  tmp.vv<-cbind(tmp.vv,v.start,timev)
} else {
  if (!is.na(timevname)) fml<-paste("Surv(",timevname,",",colvname,")~",paste(xv1,collapse="+"))
  fml2<-paste("Surv(",timevname,",",colvname,")");  wd.tmp<-c(xvname,colvname,timevname)
  tmp.vv<-cbind(tmp.vv,timev)
}
survStr<-fml2;
if (!is.na(vname.stop) & fml>"") {
  fml<-paste(fml,"+strata(",vname.stop,")",sep="");  fml2<-paste(fml2,"~factor(",vname.stop,")")
  wd.tmp<-cbind(wd.tmp,vname.stop)
} else {fml2<-paste(fml2,"~1");}
xinstrstr<-""; fmla1<-""
if (!is.na(bvar)) {
  fmla0<-paste(fml,"+factor(",bvar,")");
  if (sum(sxf=="S")>0) {
    xintrstr<-paste(paste(xv1[sxf=="S"],paste("factor(",bvar,")",sep=""),sep="*"),collapse="+")
    fmla1<-paste(fmla0,"+",xintrstr)
  }
}
allvname<-c(xvname,colvname,bvar,vname.start,vname.stop,timevname); 
allvname<-allvname[!is.na(allvname)]
WD<-WD[,allvname];
WD0<-WD[(apply(is.na(WD),1,sum)==0),]
detach(WD)
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,"<h2>Cox模型</h2>")




xx1<-c("观察数（人数）","事件发生数")
xx2<-c(" ","回归系数(coef)","回归系数标准误","z","危险比(HR=exp(coef))","1/危险比","95%区间下限","95%区间上限","P　值")
xx3<-"比例风险模型"
xx4<-"Log Likelihood （对数似然值）="
xx5<-"比例风险模型诊断"
xxname<-list(xx1,xx2,xx3,xx4,xx5)

w<-c(w,paste("</br>结果变量: ",colvb))
w<-c(w,paste("</br>时间变量: ",timevb,"</br>"))

if (fml>"") {
 if (!is.na(bvar)) {
  for (s in (1:nstg)) {
    WD1<-WD0[WD0[,bvar]==stv.lv[s],]
    attach(WD1)
    w<-c(w,paste("</br>",stvb,": ",stv.lb[s],"</br>"))
	w<-c(w,runcoxfml(fml,fml2,paste(ofname,bvar,stv.lv[s],sep="_"),xxname))
	detach(WD1)
  }
  w<-c(w,"</br>Total:</br>")
  WD1<-WD0;attach(WD1)  
  w<-c(w,runcoxfml(fmla0,fml2,ofname,xxname))
  if (fmla1>"") {
    w<-c(w,"</br>Total: 交互作用模型</br>")
    w<-c(w,runcoxfml(fmla1,"",ofname,xxname))
  }	
 } else {
  WD1<-WD0;attach(WD1)  
  w<-c(w,runcoxfml(fml,fml2,ofname,xxname))
 }
}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)