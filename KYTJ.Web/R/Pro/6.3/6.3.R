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
inumber<-as.numeric(a[1,1]);
iyn1<-as.character(a[1,2]);iyv1<-as.character(a[1,3]);iys1<-as.numeric(a[1,4])
itn1<-as.character(a[1,5]);itv1<-as.character(a[1,6]);its1<-as.numeric(a[1,7])
ixn1<-as.character(a[1,8]);ixv1<-as.character(a[1,9]);ixs1<-as.numeric(a[1,10])
ixn2<-as.character(a[1,11]);ixv2<-as.character(a[1,12]);ixs2<-as.numeric(a[1,13])
ixn3<-as.character(a[1,14]);ixv3<-as.character(a[1,15]);ixs3<-as.numeric(a[1,16])
ixn4<-as.character(a[1,17]);ixv4<-as.character(a[1,18]);ixs4<-as.numeric(a[1,19])
ixn5<-as.character(a[1,20]);ixv5<-as.character(a[1,21]);ixs5<-as.numeric(a[1,22])


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


library(numDeriv,lib.loc=R.LibLocation)
library(lava,lib.loc=R.LibLocation)
library(timereg,lib.loc=R.LibLocation)
 
ofname<-"6_3"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"竞争风险模型"; 
attach(WD) 
subjvname<-NA;

if(inumber==1)  {
  xv<-as.matrix(idata[,c(ixn1)]); 
  xvname<-c(ixv1); 
  xvar<-c(ixv1); 
  xlv<-c(NA,ixs1)[-1];
  sxf<-c(NA,0)[-1]; 
}else if(inumber==2)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  xvname<-c(ixv1,ixv2); 
  xvar<-c(ixv1,ixv2); 
  xlv<-c(NA,ixs1,ixs2)[-1];
  sxf<-c(NA,0,0)[-1]; 
}else if(inumber==3)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  xvname<-c(ixv1,ixv2,ixv3); 
  xvar<-c(ixv1,ixv2,ixv3); 
  xlv<-c(NA,ixs1,ixs2,ixs3)[-1];
  sxf<-c(NA,0,0,0)[-1]; 
}else if(inumber==4)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1];
  sxf<-c(NA,0,0,0,0)[-1]; 
}else if(inumber==5)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]); 
  xvname<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5)[-1];
  sxf<-c(NA,0,0,0,0,0)[-1]; 
}

svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-idata[,itn1];timevname<-c(itv1); 
bv<-NA; bvar<-NA; 
colv<-idata[,iyn1];colvname<-c(iyv1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## numDeriv lava timereg ##R package##;

WD<-WD[!is.na(WD[,colvname]) & !is.na(WD[,timevname]),]

vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]

if (!is.na(bvar)) {
  bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
  b.lv<-levels(factor(WD[,bvar]));
  b.lb<-vlabelZ[match(paste(bvar,b.lv,sep="."),vnameZ)]
  b.lb[is.na(b.lb)]<-b.lv[is.na(b.lb)]
  nbg<-length(b.lv)
  sxf<-sxf[xvname!=bvar];xlv<-xlv[xvname!=bvar];xvname<-xvname[xvname!=bvar]
} else {nbg<-1;}

colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
colv.lv=sort(unique(WD[,colvname])); colv.lv<-colv.lv[colv.lv>0];
colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
nst<-length(colv.lv)

xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]

timevb<-vlabelV[match(timevname,vnameV)]; if (is.na(timevb)) timevb<-timevname;

xv1<-xvname; xv1[xlv>2]<-paste("factor(",xvname[xlv>2],")",sep="")


allvname<-c(xvname,colvname,bvar,timevname); 
allvname<-allvname[!is.na(allvname)]
WD<-WD[,allvname];
WD0<-WD[(apply(is.na(WD),1,sum)==0),]
detach(WD)

fml0<-paste("Event(",timevname,",",colvname,")~",sep="")

fmlx0<-paste(xv1,collapse="+")
xvs<-xv1; xvs[sxf==0]<-paste("const(",xvs[sxf==0],")",sep="")
fmlx1<-paste(paste("const(",xv1,")",sep=""),collapse="+")
fmlx2<-paste(xvs,collapse="+")

fm0<-paste(fml0,fmlx0)
fm1<-paste(fml0,fmlx1)
fm2<-paste(fml0,fmlx2)

sink(paste(ofname,".txt",sep=""))

cat("\nCompeting risk regression model (using timereg package: Fine & Gray model)");
cat(paste("\nOutcome: ",colvb))
cat(paste("\nTime: ",timevb,"\n\n"))

for (i in 1:nbg) {
  if (nbg>1) {
    WD1<-WD0[WD0[,bvar]==b.lv[i],];
    cat(paste("\nFor subset:", bvb, "==", b.lb[i], "\n\n"));
  } else {WD1<-WD0;}
  attach(WD1)

  cat(paste("\nModel I: ",fm0, "\n"))
  for (j in 1:nst) {
     cat(paste("\n  Model for ", colvb, "==", colv.lb[j], "\n"))
     m0<-comp.risk(formula(fm0),data=WD1,cause=colv.lv[j],resample.iid=1,n.sim=1000,model="fg")
     summary(m0)
  }


  cat(paste("\nModel II: ",fm1, "\n"))
  for (j in 1:nst) {
     cat(paste("\n  Model for ", colvb, "==", colv.lb[j], "\n"))
     m1<-comp.risk(formula(fm1),data=WD1,cause=colv.lv[j],resample.iid=1,n.sim=1000,model="fg")
     summary(m1)
  }

  if (sum(sxf=="S")>0) {
    cat(paste("\nModel III: ",fm2, "\n"))
    for (j in 1:nst) {
      cat(paste("\n  Model for ", colvb, "==", colv.lb[j], "\n"))
      m2<-comp.risk(formula(fm2),data=WD1,cause=colv.lv[j],resample.iid=1,n.sim=1000,model="fg")
      summary(m2)
    }
  }

  detach(WD1)
}
sink()