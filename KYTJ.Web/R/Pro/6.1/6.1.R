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

  ixn1<-as.character(a[1,1]);ixv1<-as.character(a[1,2]);ixs1<-as.numeric(a[1,3])
  ixn2<-as.character(a[1,4]);ixv2<-as.character(a[1,5]);ixs2<-as.numeric(a[1,6])
  ixn3<-as.character(a[1,7]);ixv3<-as.character(a[1,8]);ixs3<-as.numeric(a[1,9])
  slt.vname<-c()
  
  vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3)
  vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3)

library(survival,lib.loc=R.LibLocation)
library(gdata,lib.loc=R.LibLocation)
library(gridExtra,lib.loc=R.LibLocation)
library(ggplot2,lib.loc=R.LibLocation)
library(tidyr,lib.loc=R.LibLocation)
library(broom,lib.loc=R.LibLocation)
library(survMisc,lib.loc=R.LibLocation)
library(survminer,lib.loc=R.LibLocation)
 
ofname<-"6_1"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"KM生存曲线与累积发生率"; 
attach(WD) 
subjvname<-NA; 
xvname<-NA; xv<-NA; xlv<-NA; 
sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-idata[,ixn2];timevname<-c(ixn2); 
bv<-idata[,ixn3];bvar<-c(ixn3);bvname<-c(ixn3); 
colv<-idata[,ixn1];colvname<-c(ixn1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## survival gdata gridExtra ggplot2 tidyr broom survMisc survminer ##R package##;
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
  if (is.matrix(p)) {nr<-nrow(p);} else {nr<-1;}
  p1<-round(as.numeric(p),dec); 
  p2<-p1-floor(p1);
  tmp<-(p2==0 & !is.na(p2))
  p1[tmp]<-paste(p1[tmp],".0",sep="")
  p2[tmp]<-"0.0"; 
  p1<-paste(p1,substr(rep("0000000",length(p1)),1,dec+2-nchar(p2)),sep="")
  p1[as.numeric(p)>10000000]<-"inf."
  if (nr>1) {p1<-matrix(p1,nrow=nr);colnames(p1)<-colnames(p);}
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

WD<-WD[!is.na(WD[,colvname]) & !is.na(WD[,timevname]),]

if (!is.na(par1) & is.na(vname.start)) {
  if (is.numeric(par1)) {tp<-par1;
  } else {
    tmp<-as.numeric(strsplit(par1," ")[[1]]);  tp<-c(tmp[!is.na(tmp)],NA)
  }
  if (length(tp)>2) tp<-tp[1:2]
  tp0<-tp
  WD<-getLandmarkDF(colvname, WD, timevname, tp);
  tmptimeb <- vlabel[vname==timevname];
  tptimeb <- paste(tmptimeb, "<=", tp[1]);
  if (length(tp)>1) {
    tptimeb<-c(tptimeb,  paste(tmptimeb, ">", tp[1], "& <=", tp[2]),paste(tmptimeb, ">", tp[2]));
  } else {
    tptimeb<-c(tptimeb,  paste(tmptimeb, ">", tp[1]));
  }
  ntp<-length(tp)+1; pngfsuff<-paste("_seg",(1:ntp),sep=""); tp<-c(0,tp,max(WD[,timevname],na.rm=TRUE))
} else {
  ntp<-1; WD<-cbind(WD,TIME.PHASE.4LMCOX=1);pngfsuff<-""; tp<-c(0,max(WD[,timevname],na.rm=TRUE))
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
}
if (!is.na(colvname)) {
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  colv.lv<-levels(factor(colv))
  colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
  colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
}
fml0<-"";
if (!is.na(vname.start)) {
  fml0<-paste("Surv(,",vname.start,",",timevname,",",colvname,")~")
  timevb<-paste(vlabelV[match(vname.start,vnameV)],"--",vlabelV[match(timevname,vnameV)]);
  if (is.na(timevb)) timevb<-paste(vname.start,"--",timevname);
} else {
  if (!is.na(timevname)) fml0<-paste("Surv(", timevname, ",", colvname, ")~"); 
  timevb<-vlabelV[match(timevname,vnameV)];
  if (is.na(timevb)) timevb<-timevname;
}
if (fml0>"") {if (!is.na(bvar)) {fml<-paste(fml0,bvar);} else {fml<-paste(fml0,"1");}}
colvb<-vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;  

w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,"<h2>Kaplan Meier Survival Curve</h2>")
w<-c(w,paste("</br>Outcome:", colvb))
w<-c(w,paste("</br>Time:", timevb))

allvname<-c(colvname,bvar,timevname,vname.start,"TIME.PHASE.4LMCOX"); 
allvname<-allvname[!is.na(allvname)]
WD<-WD[,allvname];
WD<-WD[(apply(is.na(WD),1,sum)==0),]
WD[,colvname]<-(WD[,colvname]==1)*1

if (!is.na(colvname) & (fml>"")) { 
  for (k in (1:ntp)) {
   if (ntp>1) w<-c(w,paste("</br></br>Landmark analysis for time segment:", tptimeb[k]), "</br>")
   pngf<-paste(ofname,pngfsuff[k],"_",c(1:4,0,"b"),".png",sep="")
   pdff<-paste(ofname,pngfsuff[k],"_",c(1:4,0,"b"),".pdf",sep="")
   wdtmp<-WD[WD[,"TIME.PHASE.4LMCOX"]==k,]
   tlim<-c(tp[k], tp[k+1])
   tmp.sfit<-survfit(formula(fml), data=wdtmp)
   if (is.na(bvar)) {
    ss<-summary(tmp.sfit)
    rr<-numfmt(cbind(ss$surv,ss$std.err,ss$lower,ss$upper),4)
    oo<-cbind(ss$time,ss$n.risk,ss$n.event,rr)
    oo2<-rbind(names(ss$table),ss$table)
    tmp.miny<-floor(min(tmp.sfit$lower,na.rm=TRUE)*10)/10

    png(pngf[1],width=720,height=560)
    plot(tmp.sfit,ylim=c(tmp.miny,1),xlim=tlim, ylab="Survival",xlab=timevb,main=colvb,mark.time=FALSE)
    dev.off()
    pdf(pdff[1],width=pdfwd, height=pdfht, family="Helvetica");
    plot(tmp.sfit,ylim=c(tmp.miny,1),xlim=tlim, ylab="Survival",xlab=timevb,main=colvb,mark.time=FALSE)
    dev.off()

    if (length(ss$time)>20) {
     png(pngf[2],width=720,height=560)
     plot(tmp.sfit,mark.time=FALSE,ylim=c(tmp.miny,1),xlim=tlim,ylab="Survival",xlab=timevb,main=colvb)
     dev.off()
     pdf(pdff[2],width=pdfwd, height=pdfht, family="Helvetica");
     plot(tmp.sfit,mark.time=FALSE,ylim=c(tmp.miny,1),xlim=tlim,ylab="Survival",xlab=timevb,main=colvb)
     dev.off()
    }

    png(pngf[3],width=720,height=560)
    plot(tmp.sfit,mark.time=FALSE,fun="cumhaz",xlim=tlim,ylab="Cumulative Hazard",xlab=timevb,main=colvb)
    dev.off()
    pdf(pdff[3],width=pdfwd, height=pdfht, family="Helvetica");
    plot(tmp.sfit,mark.time=FALSE,fun="cumhaz",xlim=tlim,ylab="Cumulative Hazard",xlab=timevb,main=colvb)
    dev.off()

    png(pngf[4],width=720,height=560)
    plot(tmp.sfit,mark.time=FALSE,fun="event",xlim=tlim,ylab="Cumulative events (1-y)",xlab=timevb,main=colvb)
    dev.off()
    pdf(pdff[4],width=pdfwd, height=pdfht, family="Helvetica");
    plot(tmp.sfit,mark.time=FALSE,fun="event",xlim=tlim,ylab="Cumulative events (1-y)",xlab=timevb,main=colvb)
    dev.off()
    
    ggsurv<-ggsurvplot(tmp.sfit,data=wdtmp,conf.int=TRUE,xlim=tlim,xlab=timevb,risk.table=TRUE,ncensor.plot=TRUE)
    png(pngf[6],width=720,height=720); print(ggsurv);  dev.off()
    pdf(pdff[6],width=pdfwd, height=pdfht*1.5, family="Helvetica"); print(ggsurv); dev.off()

    ow<-""
   } else {
    sv<-summary(tmp.sfit)
    rr<-numfmt(cbind(sv$surv,sv$std.err,sv$lower,sv$upper),4)
    b<-cbind(sv$time,sv$n.risk,sv$n.event,rr)
    g<-sv$strata; st<-levels(factor(sv$strata)); 
    stb<-trim(vlabelZ[match(paste(bvar,st,sep="."),vnameZ)])
    stb[is.na(stb)]<-st[is.na(stb)]
    oo<-NA
    for (i in (1:length(st))) {
      ooi<-rbind(c(paste(bvb,stb[i],sep=":"),rep("",(ncol(b)-1))),b[g==st[i],])
      if (is.na(oo[1])) {oo<-ooi;} else {oo<-rbind(oo,ooi);}
    }
    oo2<-rbind(c(bvar,colnames(sv$table)),cbind(rownames(sv$table),sv$table))
    tmp.miny<-floor(min(tmp.sfit$lower,na.rm=TRUE)*10)/10

    png(pngf[1],width=720,height=560)
    plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)), ylim=c(tmp.miny,1.05),xlim=tlim, ylab="Survival", xlab=timevb, main=colvb, mark.time=FALSE)
    legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
    dev.off()
    pdf(pdff[1],width=pdfwd, height=pdfht, family="Helvetica");
    plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)), ylim=c(tmp.miny,1.05),xlim=tlim, ylab="Survival", xlab=timevb, main=colvb, mark.time=FALSE)
    legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
    dev.off()
       
    ggsurv0<-ggsurvplot(tmp.sfit,data=wdtmp,pval=TRUE,conf.int=FALSE,xlim=tlim,xlab=timevb,legend.title=bvb,legend.labs=bv.lb,
    risk.table=TRUE,risk.table.y.text.col=T,risk.table.height=0.25,risk.table.y.text=FALSE,ncensor.plot=TRUE,ncensor.plot.height = 0.25)
    png(pngf[5],width=720,height=720); print(ggsurv0);  dev.off()
    pdf(pdff[5],width=pdfwd, height=pdfht*1.5, family="Helvetica"); print(ggsurv0); dev.off()
    
    ggsurv<-ggsurvplot(tmp.sfit,data=wdtmp,pval=TRUE,conf.int=FALSE,xlim=tlim,xlab=timevb,legend.title=bvb,legend.labs=bv.lb,
    risk.table=TRUE,risk.table.height=0.25,linetype="strata")
    png(pngf[6],width=720,height=720); print(ggsurv);  dev.off()
    pdf(pdff[6],width=pdfwd, height=pdfht*1.5, family="Helvetica"); print(ggsurv); dev.off()

    if (length(unique(sv$time))>20) {
      png(pngf[2],width=720,height=560)
      plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)),mark.time=FALSE, ylim=c(tmp.miny,1.05),xlim=tlim,
          ylab="Survival", xlab=timevb, main=colvb)
      legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
      dev.off()
      pdf(pdff[2],width=pdfwd, height=pdfht, family="Helvetica");
      plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)),mark.time=FALSE, ylim=c(tmp.miny,1.05),xlim=tlim,
          ylab="Survival", xlab=timevb, main=colvb)
      legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
      dev.off()
    }

    png(pngf[3],width=720,height=560)
    plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)),mark.time=FALSE,fun="cumhaz",xlim=tlim,
     ylab="Cumulative Hazard",xlab=timevb,main=colvb)
    legend("topleft",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
    dev.off()
    pdf(pdff[3],width=pdfwd, height=pdfht, family="Helvetica");
    plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)),mark.time=FALSE,fun="cumhaz",xlim=tlim,
     ylab="Cumulative Hazard",xlab=timevb,main=colvb)
    legend("topleft",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
    dev.off()

    png(pngf[4],width=720,height=560)
    plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)),mark.time=FALSE,fun="event",xlim=tlim,
     ylab="Cumulative events (1-y)",xlab=timevb,main=colvb)
    legend("topleft",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
    dev.off()
    pdf(pdff[4],width=pdfwd, height=pdfht, family="Helvetica");
    plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)),mark.time=FALSE,fun="event",xlim=tlim,
     ylab="Cumulative events (1-y)",xlab=timevb,main=colvb)
    legend("topleft",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
    dev.off()

    svd<-survdiff(formula(fml), data=wdtmp)
    ow<-c("Log rank test: implements the G-rho family of Harrington and Fleming (1982), with weights on each death of S(t)^rho, where S is the Kaplan-Meier estimate of survival. With rho = 0 this is the log-rank or Mantel-Haenszel test.")
    tmp.svdmat<-rbind(c(" ","N","Observed","Expected"),cbind(substr(names(svd$n),4,99),svd$n,svd$obs,numfmt(svd$exp,4)))
    ow<-c(ow,"</br><table border=3>",mat2htmltable(tmp.svdmat),"</table></br>") 
    ow<-c(ow,"Chisq=",numfmt(svd$chisq,4), " on ", length(svd$n)-1, " degree of freedom, p=", pvformat(1-pchisq(svd$chisq,length(svd$n)-1),4))
  }

  cname<-c(timevb,"N.Risk","N.Event","Survival","Std.err","95%CI Low","95%CI Upp")
  oo<-rbind(cname,oo)
  w<-c(w,"</br><table border=3>",mat2htmltable(oo),"</table></br>") 
  w<-c(w,"</br><table border=3>",mat2htmltable(oo2),"</table></br>") 
  w<-c(w,ow)
 }
 if (!is.na(bvar) & ntp>1) {
  WD<-cbind(WD, tgrp= WD[,"TIME.PHASE.4LMCOX"]*10+ WD[,bvar])
  fml2<-paste(fml0,"tgrp")
  tmp.sfit<-survfit(formula(fml2), data=WD)
  png(paste(ofname,".png",sep=""),width=720,height=560)
  plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)), ylim=c(tmp.miny,1.05), ylab="Survival", xlab=timevb, main=colvb)
  legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
  abline(v=tp0, lty=1)
  dev.off()
  pdf(paste(ofname,".pdf",sep=""),width=pdfwd, height=pdfht, family="Helvetica");
  plot(tmp.sfit,lty=(1:nbg),col=(2:(nbg+1)), ylim=c(tmp.miny,1.05), ylab="Survival", xlab=timevb, main=colvb)
  legend("topright",bv.lb,lty=(1:nbg),col=(2:(nbg+1)),title=bvb)
  abline(v=tp0, lty=1)
  dev.off()

  fml3<-paste(fml0,bvar)
  tmp.sfit<-survfit(formula(fml3), data=WD)
  ggsurv<-ggsurvplot(tmp.sfit,data=WD,conf.int=TRUE,xlab=timevb,risk.table=TRUE,ncensor.plot=TRUE)
  png(paste(ofname,"_0.png",sep=""),width=720,height=720); print(ggsurv);  dev.off()
  pdf(paste(ofname,"_0.pdf",sep=""),width=pdfwd, height=pdfht*1.5, family="Helvetica"); print(ggsurv); dev.off()  
 }
}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)
#dev.off()
