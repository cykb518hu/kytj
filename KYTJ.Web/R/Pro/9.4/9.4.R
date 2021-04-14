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
iynumber<-as.numeric(as.character(a[1,1]));
iyn1<-as.character(a[1,2]);iyv1<-as.character(a[1,3]);iys1<-as.numeric(a[1,4]);iydist1<-as.character(a[1,5]);iylink1<-as.character(a[1,6])
iyn2<-as.character(a[1,7]);iyv2<-as.character(a[1,8]);iys2<-as.numeric(a[1,9]);iydist2<-as.character(a[1,10]);iylink2<-as.character(a[1,11])

if(iynumber==1)  {
  vname<-c("_N_","_STAT_","_TOTAL_",iyn1)
  vlabel<-c("样本量(%)","统计量","合计",iyn1)
}else if(iynumber==2)  {
  vname<-c("_N_","_STAT_","_TOTAL_",iyn1,iyn2)
  vlabel<-c("样本量(%)","统计量","合计",iyn1,iyn2)
}
#--#
ixnumber<-as.numeric(as.character(a[2,1]));
ixn1<-as.character(a[2,2]);ixv1<-as.character(a[2,3]);ixs1<-as.numeric(as.character(a[2,4]))
ixn2<-as.character(a[2,5]);ixv2<-as.character(a[2,6]);ixs2<-as.numeric(as.character(a[2,7]))
ixn3<-as.character(a[2,8]);ixv3<-as.character(a[2,9]);ixs3<-as.numeric(as.character(a[2,10]))
ixn4<-as.character(a[2,11]);ixv4<-as.character(a[2,12]);ixs4<-as.numeric(as.character(a[2,13]))
ixn5<-as.character(a[2,14]);ixv5<-as.character(a[2,15]);ixs5<-as.numeric(as.character(a[2,16]))

if(ixnumber==1)  {
  vname<-c(vname,ixn1)
  vlabel<-c(vlabel,ixn1)
}else if(ixnumber==2)  {
  vname<-c(vname,ixn1,ixn2)
  vlabel<-c(vlabel,ixn1,ixn2)
}else if(ixnumber==3)  {
  vname<-c(vname,ixn1,ixn2,ixn3)
  vlabel<-c(vlabel,ixn1,ixn2,ixn3)
}else if(ixnumber==4)  {
  vname<-c(vname,ixn1,ixn2,ixn3,ixn4)
  vlabel<-c(vlabel,ixn1,ixn2,ixn3,ixn4)
}else if(ixnumber==5)  {
  vname<-c(vname,ixn1,ixn2,ixn3,ixn4,ixn5)
  vlabel<-c(vlabel,ixn1,ixn2,ixn3,ixn4,ixn5)
}
#--#
isnymber<-as.numeric(as.character(a[3,1]))
isnp<-as.numeric(as.character(a[3,2]))
isn1<-as.character(a[3,3]);isv1<-as.character(a[3,4]);iss1<-as.numeric(as.character(a[3,5]))
row1<-as.character(a[3,6]);col1<-as.character(a[3,7])
irow1<-unlist(strsplit(row1,"[|]"));icol1<-unlist(strsplit(col1,"[|]"))
isn2<-as.character(a[3,8]);isv2<-as.character(a[3,9]);iss2<-as.numeric(as.character(a[3,10]))
row2<-as.character(a[3,11]);col2<-as.character(a[3,12])
irow2<-unlist(strsplit(row2,"[|]"));icol2<-unlist(strsplit(col2,"[|]"))
if(isnymber==1)  {
  vname<-c(vname,irow1)
  vlabel<-c(vlabel,icol1)
}else if(isnymber==2)  {
  vname<-c(vname,irow1,irow2)
  vlabel<-c(vlabel,icol1,icol2)
}
#--#
izn1<-as.character(a[4,1]);izv1<-as.character(a[4,2]);izs1<-as.numeric(as.character(a[4,3]))
izn2<-as.character(a[4,4]);izv2<-as.character(a[4,5]);izs2<-as.numeric(as.character(a[4,6]))
izlink<-as.character(a[4,7])

library(mgcv,lib.loc=R.LibLocation)
library(geepack,lib.loc=R.LibLocation)
library(gdata,lib.loc=R.LibLocation)
 
ofname<-"9_4"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"基因型与表型关联分析"; 
#WD<-WD[order(WD$FMYID),];
attach(WD) 
gee.SUBJ<-idata[,izn2];subjvname<-c(izn2);gee.TYPE<-tolower(c(izlink));

if(iynumber==1)  {
  yv<-as.data.frame(idata[c(iyn1)]); 
  yvname<-c(iyn1); 
  yvar<-c(iyv1); 
  ydist<-c(iydist); 
  ylink<-c(iylink); 
  ylv<-c(NA,iys1)[-1];
}else if(iynumber==2)  {
  yv<-as.data.frame(idata[c(iyn1,iyn2)]); 
  yvname<-c(iyn1,iyn2); 
  yvar<-c(iyv1,iyv2); 
  ydist<-c(iydist1,iydist2); 
  ylink<-c(iylink1,iylink2); 
  ylv<-c(NA,iys1,iys2)[-1];
}

if(isnymber==1)  {
  xv<-as.matrix(idata[isn1]); 
  xvname<-c(isn1); 
  xvar<-c(isv1); 
  xlv<-c(NA,iss1)[-1];
}else if(isnymber==2)  {
  xv<-as.matrix(idata[,c(isn1,isn2)]); 
  xvname<-c(isn1,isn2); 
  xvar<-c(isv1,isv2); 
  xlv<-c(NA,iss1,iss2)[-1];
}
sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA;
if(ixnumber==1)  {
  av<-as.matrix(idata[,c(ixn1)])
  avname<-c(ixn1)
  if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]
  nadj<-length(avname);alv<-c(NA,ixs1)[-1]
  saf<-c(NA,0)[-1]
}else if(ixnumber==2)  {
  av<-as.matrix(idata[,c(ixn1,ixn2)])
  avname<-c(ixn1,ixn2)
  if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]
  nadj<-length(avname);alv<-c(NA,ixs1,ixs2)[-1] 
  saf<-c(NA,0,0,0)[-1]
}else if(ixnumber==3)  {
  av<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]) 
  avname<-c(ixn1,ixn2,ixn3)
  if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]
  nadj<-length(avname);alv<-c(NA,ixs1,ixs2,ixs3)[-1]
  saf<-c(NA,0,0,0)[-1]
}else if(ixnumber==4){
  av<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)])
  avname<-c(ixn1,ixn2,ixn3,ixn4)
  if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]
  nadj<-length(avname);alv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1];
  saf<-c(NA,0,0,0,0)[-1]
}
timev<-NA; timevname<-NA; 
bv<-idata[,izn1];bvar<-c(izn1);bvname<-c(izv1); 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;
if(isnp==1)  {
parm<-c(1,NA, NA, NA);
}else if(isnp==1)  {
  parm<-c(NA,NA, NA, NA);
}
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## mgcv geepack gdata ##R package##;
pvformat<-function(p,dec) {
  if (dec>8) dec<-8
  p1<-round(as.numeric(p),dec); pp<-p1
  tmp<-(substr(p1,2,9)=="e-04" & !is.na(p1))
  pp[tmp]<-paste("0.000",substr(pp[tmp],1,1),sep="")
  tmp<-(p1==0 & !is.na(p1))
  pp[tmp]<-paste(c("<0.",rep("0",dec-1),"1"),collapse="")
  tmp<-(p1>0 & !is.na(p1))
  pp[tmp]<-paste(pp[tmp],substr(rep("000000000",length(pp[tmp])),1,dec+2-nchar(pp[tmp])),sep="")
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
  p1[p=="-Inf"]<-"-Inf"
  p1[p=="Inf"]<-"Inf"
  if (is.matrix(p)) {
    p1<-matrix(p1,nrow=nr);colnames(p1)<-colnames(p);rownames(p1)<-rownames(p)
  }
  return(p1)
}
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
setgam<-function(fml,yi) {
  if (ydist[yi]=="") ydist[yi]<-"gaussian"
  if (ydist[yi]=="exact") ydist[yi]<-"binomial"
  if (ydist[yi]=="breslow") ydist[yi]<-"binomial"
  if (ydist[yi]=="gaussian") mdl<-try(gam(formula(fml),data=wdtmp, family=gaussian(link="identity")))
  if (ydist[yi]=="binomial") mdl<-try(gam(formula(fml),data=wdtmp, family=binomial(link="logit")))
  if (ydist[yi]=="poisson") mdl<-try(gam(formula(fml),data=wdtmp, family=poisson(link="log")))
  if (ydist[yi]=="gamma") mdl<-try(gam(formula(fml),data=wdtmp, family=Gamma(link="inverse")))
  if (ydist[yi]=="negbin") mdl<-try(gam(formula(fml),data=wdtmp, family=negbin(c(1,10), link="log")))
  return(mdl)
}
setgee<-function(fml,yi) {
  if (ydist[yi]=="") ydist[yi]<-"gaussian"
  if (ydist[yi]=="exact") ydist[yi]<-"binomial"
  if (ydist[yi]=="breslow") ydist[yi]<-"binomial"
  if (ydist[yi]=="gaussian") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="gaussian",data=wdtmp))
  if (ydist[yi]=="binomial") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="binomial",data=wdtmp))
  if (ydist[yi]=="poisson") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="poisson",data=wdtmp))
  if (ydist[yi]=="gamma") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="Gamma",data=wdtmp))
  if (ydist[yi]=="negbin") md<-try(geeglm.nb(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,data=wdtmp))
  return(md)
}
setglm<-function(fml,yi) {
  if (ydist[yi]=="") ydist[yi]<-"gaussian"
  if (ydist[yi]=="exact") ydist[yi]<-"binomial"
  if (ydist[yi]=="breslow") ydist[yi]<-"binomial"
  if (ydist[yi]=="gaussian") md<-try(glm(formula(fml),family="gaussian",data=wdtmp))
  if (ydist[yi]=="binomial") md<-try(glm(formula(fml),family="binomial",data=wdtmp))
  if (ydist[yi]=="poisson") md<-try(glm(formula(fml),family="poisson",data=wdtmp))
  if (ydist[yi]=="gamma") md<-try(glm(formula(fml),family="Gamma",data=wdtmp))
  if (ydist[yi]=="negbin") md<-try(glm.nb(formula(fml),data=wdtmp))
  return(md)
}
mdl2oo<-function(mdl,opt, xvnamei) {
  if (is.na(mdl[[1]][1])) return(c("","",""))
  if (substr(mdl[[1]][1],1,5)=="Error") return(c("","",""))
  gs<-summary(mdl); print(gs);
  if (opt=="gam") {gsp <- gs$p.table;} else {gsp <- gs$coefficients;}
  gsp<-gsp[match(xvnamei,rownames(gsp)),]
  oo<-c(numfmt(gsp[1],dec),numfmt(gsp[2],dec),pvformat(gsp[4],8))
  return(oo)
}
readgeno <- function(genofile) {
 genof <- read.table(genofile,sep=" ",header=TRUE)
 if (ncol(genof)==1) genof <- read.table(genofile,sep="\t",header=TRUE)
 if (ncol(genof)==1) genof <- read.table(genofile,sep=",",header=TRUE)
 return(genof)
}
sumxx<-function(x) {sum(x,na.rm=TRUE)}
maxw<-function(x) {ifelse(max(x)>0,which.max(x),NA)}
minw<-function(x) {ifelse(max(x)>0,(1:4)[-c(which.max(x),which(x==0))],NA)}
ped2snp <- function(pedfile,outfile) {
 pedf <- read.table(pedfile,sep=" ",skip=1,header=FALSE)
 snpname <- read.table(pedfile,sep=" ",skip=0,nrow=1,header=FALSE)
 if (ncol(pedf)==1) {
  pedf <- read.table(pedfile,sep="\t",skip=1,header=FALSE)
  snpname <- read.table(pedfile,sep="\t",skip=0,nrow=1,header=FALSE)
 }
 if (ncol(pedf)==1) {
  pedf <- read.table(pedfile,sep=",",skip=1,header=FALSE)
  snpname <- read.table(pedfile,sep=",",skip=0,nrow=1,header=FALSE)
 }
 nsnp<-(ncol(pedf)-6)/2
 snpname<-snpname[1:nsnp]
 if (sum(is.na(snpname))>0) snpname<-paste("SNP",(1:nsnp),sep="")
 if (length(unique(pedf[,2]))!=length(pedf[,2])) {print("Error, found duplicated ID in .ped file!"); quit();}
 pedf_ID<-pedf[,2]
 pedf <- pedf[,-c(1:6)]
 pedf[pedf==0]<-NA
 nsub <- nrow(pedf)
 col1 <- seq(1,ncol(pedf),2)
 col2 <- seq(2,ncol(pedf),2)
 a1 <- pedf[,col1]
 a2 <- pedf[,col2]
 colnames(a1)<-t(snpname)
 colnames(a2)<-t(snpname)
 aa <- rbind(a1,a2)
 alle <- rbind(apply(aa==1,2,sumxx),apply(aa==2,2,sumxx),apply(aa==3,2,sumxx),apply(aa==4,2,sumxx))
 rownames(alle)<-c("A","C","G","T")
 alle1<-apply(alle,2,maxw)
 alle2<-apply(alle,2,minw)
 allele=c("A","C","G","T")
 line1<-paste(allele[alle1],"/",allele[alle2],sep="")
 a1[a1==matrix(rep(alle1,nsub),nrow=nsub,byrow=TRUE)]<-0
 a1[a1==matrix(rep(alle2,nsub),nrow=nsub,byrow=TRUE)]<-1
 a2[a2==matrix(rep(alle1,nsub),nrow=nsub,byrow=TRUE)]<-0
 a2[a2==matrix(rep(alle2,nsub),nrow=nsub,byrow=TRUE)]<-1
 a1<-cbind(pedf_ID, a1 + a2)
 colnames(a1)<-c("ID",t(snpname))
 write.table(a1,file=outfile,col.names=TRUE,row.names=FALSE,append=FALSE,quote=FALSE,sep=" ")
 rm(aa,a2,pedf,alle,alle1,alle2,line1)
 return(a1)
}
fdrcut<-function(p,alpha) {
  p0<-cbind(1:length(p),p); n<-sum(!is.na(p));p1<-p0[!is.na(p),]
  p1<-cbind(p1[order(p1[,2]),],(1:n)/n*alpha)
  return(p1[match(p0[,1],p1[,1]),3])
}

if (!exists("dec")) dec<-2
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
if (!is.na(avname[1])) { 
  if (sum((saf=="s" | saf=="S") & alv>0)>0) w<-c(w,"</br>Spline smoothing only applies for continuous variables")
  if (!is.na(subjvname) & (sum((saf=="s" | saf=="S") & alv==0)>0)) w<-c(w,"</br>Generalized estimate equation could not be used with spline smoothing terms")
}
allvname<-c(yvname,xvname,colvname,bvar,avname,subjvname,vname.start,vname.stop,timevname); 
allvname<-allvname[!is.na(allvname)]
WD<-WD[,allvname]
tmpID<-rep(1,nrow(WD))
WD<-cbind(WD,tmpID)

allvname1<-c(colvname,bvar,avname,subjvname,vname.start,vname.stop,timevname,"tmpID"); 
allvname1<-allvname1[!is.na(allvname1)]
WD<-WD[apply(is.na(cbind(1,WD[,allvname1])),1,sum)==0,]

if (!is.na(subjvname)) {
  if (!is.na(avname[1])) saf<-rep(0,length(saf)); 
  WD<-WD[order(WD[,subjvname]),];
}
rm(xv,yv,bv,av,colv,v.start,v.stop)

fml0<-""; na=0; avb=""; smoothav<-0; 
if (!is.na(avname[1])) {
  na<-length(avname)
  avb<-vlabelV[match(avname,vnameV)]; avb[is.na(avb)]<-avname[is.na(avb)]
  avname_ <- avname 
  smoothavi<-((saf=="s" | saf=="S") & alv==0)
  smoothav<-sum(smoothavi)
  smoothavname<-avname[smoothavi]
  avname_[smoothavi]<-paste("s(",avname[smoothavi],")",sep="")
  avb1<-avb
  avb1[smoothavi]<-paste(avb[smoothavi],"(Smooth)",sep="")
  avname_[alv>0]<-paste("factor(",avname[alv>0],")",sep="")
  fml0<-paste("+",paste(avname_,collapse="+"))
}
ny=length(yvname); 
yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]

if (is.na(xvname[1])) {
  if (!exists("PED_FNAME")) {print("No genotype information!"); quit();}
  if (is.na(bvar)) {print("Must specify subject ID variable!"); quit();}
  tmp<-strsplit(PED_FNAME,'\\.')[[1]]
  tmp<-toupper(tmp[length(tmp)])
  if (tmp=="PED") {geno<-ped2snp(PED_FNAME, paste(ofname,"_snps.xls",sep="")); 
  } else {geno<-readgeno(PED_FNAME); }
  id_ord<-match(WD[,bvar],geno[,1])
  if (sum(!is.na(id_ord))<10) {print("<10 subjects ID match with genotype file!"); quit();}
  WD_GENO<-geno[id_ord,]
  nx<-ncol(WD_GENO)-1
  xvname<-colnames(WD_GENO)[-1]
  xb<-xvname
} else {
  nx=length(xvname); 
  xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  WD_GENO<-WD[,c("tmpID",xvname)]
}
 
if (is.na(colvname)) {
  nclv<-1; clvb<-"Total"; clvb_<-"Total"; cvb=""
} else {
  clv<-levels(factor(WD[,colvname])); nclv<-length(clv)+1
  clvb_<-vlabelZ[match(paste(colvname,".",clv,sep=""),vnameZ)]; clvb_[is.na(clvb_)]<-clv[is.na(clvb_)];
  clvb<-c(paste(vlabelV[vnameV==colvname],clvb_,sep="="),"Total");
  clvb_<-c(clvb_,"Total")
  cvb <- vlabelV[vnameV==colvname];
} 

opt<-ifelse(!is.na(subjvname), "gee", ifelse(smoothav>0, "gam", "glm")) ;

sink(paste(ofname,".lst",sep=""))
w<-c(w,paste("<h2>", title, "</h2>"))
oo<-rep(" ",times=18)
tmp.xyg<-c(NA,NA,NA)
for (i in (1:nx)) {
  geno.i<-WD_GENO[,i+1];
  geno.d<-geno.i; geno.d[geno.d==2]<-1
  geno.r<-geno.i; geno.r[geno.r==1]<-0; geno.r[geno.r==2]<-1
  geno.i<-cbind(geno.i,geno.d,geno.r)
  genoi.name<-paste(xvname[i],c("","_dom","_rec"),sep="")
  colnames(geno.i)<-genoi.name
  for (j in (1:ny)) {
    for (k in (1:nclv)) {
      ooi<-rep(" ",times=18);  
      ooi[1]<-clvb_[k];  
      ooi[2]<-xb[i];  
      ooi[3]<-yb[j];
      wdtmp<-cbind(geno.i,WD[,c(yvname[j],allvname1)]);
      wdtmp<-wdtmp[apply(is.na(wdtmp),1,sum)==0,]
      if (k<nclv) wdtmp<-wdtmp[wdtmp[,colvname]==clv[k],];
      tmp.nn<-table(wdtmp[,1])
      ooi[4]<-tmp.nn["0"]; 
      ooi[5]<-tmp.nn["1"]; 
      ooi[6]<-tmp.nn["2"];
      tmp.mm<-numfmt(tapply(wdtmp[,yvname[j]],wdtmp[,1],function(z) mean(z,na.rm=TRUE)),dec)
      ooi[7]<-tmp.mm[1]; 
      ooi[8]<-tmp.mm[2]; 
      ooi[9]<-tmp.mm[3];
      fmli<-paste(yvname[j],"~",genoi.name[1],fml0);
      fmld<-paste(yvname[j],"~",genoi.name[2],fml0);
      fmlr<-paste(yvname[j],"~",genoi.name[3],fml0);
      if (k==nclv & !is.na(colvname)) {
         fmli<-paste(fmli,"+factor(",colvname,")",sep="")
         fmld<-paste(fmld,"+factor(",colvname,")",sep="")
         fmlr<-paste(fmlr,"+factor(",colvname,")",sep="")
      }
      tmp.n1<-sum(c(as.numeric(ooi[5]),as.numeric(ooi[6])),na.rm=TRUE)
      tmp.n2<-sum(as.numeric(ooi[6]),na.rm=TRUE)
      if (tmp.n1>1) {
        if (opt=="gam") tmp.mdl<-setgam(fmli,j)
        if (opt=="gee") tmp.mdl<-setgee(fmli,j)
        if (opt=="glm") tmp.mdl<-setglm(fmli,j)
        mdla<-mdl2oo(tmp.mdl,opt,genoi.name[1]); rm(tmp.mdl)
        ooi[10]<-mdla[1]; 
        ooi[11]<-mdla[2]; 
        ooi[12]<-mdla[3];
        if (opt=="gam") tmp.mdl<-setgam(fmld,j)
        if (opt=="gee") tmp.mdl<-setgee(fmld,j)
        if (opt=="glm") tmp.mdl<-setglm(fmld,j)
        mdld<-mdl2oo(tmp.mdl,opt,genoi.name[2]); rm(tmp.mdl)
        ooi[13]<-mdld[1]; 
        ooi[14]<-mdld[2]; 
        ooi[15]<-mdld[3];
      }
      if (tmp.n2>1) {
        if (opt=="gam") tmp.mdl<-setgam(fmlr,j)
        if (opt=="gee") tmp.mdl<-setgee(fmlr,j)
        if (opt=="glm") tmp.mdl<-setglm(fmlr,j)
        mdlr<-mdl2oo(tmp.mdl,opt,genoi.name[3]); rm(tmp.mdl)
        ooi[16]<-mdlr[1]; 
        ooi[17]<-mdlr[2]; 
        ooi[18]<-mdlr[3];
      }
      oo<-rbind(oo,ooi)
      rm(wdtmp, ooi)
      tmp.xyg<-rbind(tmp.xyg,c(i, j, k))
    }
  }
  rm(geno.i, genoi.name)
}


tmp.oo<-c(cvb,"SNP","Outcome","N0","N1","N2","MEAN0","MEAN1","MEAN2");
tmp.oo<-c(tmp.oo,"beta-add","se-add","P-add","beta-dom","se-dom","P-dom","beta-rec","se-rec","P-rec")
if (nx > 1) {
  tmp.xyg<-tmp.xyg[-1,];oo<-oo[-1,]; tmp.oo<-c(tmp.oo,"FDRCUT")
  for (k in (1:nclv)) {  
    for (j in (1:ny)) {  
      tmp.gyrow<-((tmp.xyg[,3]==k) & (tmp.xyg[,2]==j))
      tmp.oo<-rbind(tmp.oo,cbind(oo[tmp.gyrow,],pvformat(fdrcut(oo[tmp.gyrow,12],0.05),8)))
    }
  }
} else {tmp.oo<-rbind(tmp.oo,oo[-1,]);}
if (is.na(colvname)) tmp.oo<-tmp.oo[,-1]
print(tmp.oo)
sink()

w<-c(w,"</br><table border=3>", mat2htmltable(tmp.oo), "</table>")

w<-c(w,"</br>N0,N1,N2: number of subjects with genotype=0,1,2")
w<-c(w,"</br>MEAN0,MEAN1,MEAN2: mean outcome for genotype=0,1,2")
w<-c(w,"</br>beta-add, se-add, P-add: regression coefficient, standard err, pvalue from additive model")
w<-c(w,"</br>beta-dom, se-dom, P-dom: regression coefficient, standard err, pvalue from dominant model")
w<-c(w,"</br>beta-rec, se-rec, P-rec: regression coefficient, standard err, pvalue from recessive model")
w<-c(w,"</br>fdrcut: FDR cut point for P-value from additive model based on p<=0.05*k/m (Benjamini and Hochberg 1995)")
if (smoothav>0) w<-c(w,"</br>Generalized additive models were applied")
if (opt=="gee") w<-c(w, paste("</br>Generalized estimate equation were used, subject ID=", subjvname, "(", gee.TYPE,")",sep=""))
w<-c(w,wd.subset)
if (is.na(avname[1])) avb1<-"None";



w<-c(w,paste(c("</br>调整变量:",paste(avb1,collapse="; ")),collapse=" "))
w<-c(w,paste("</br>生成日期：",Sys.Date()))
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)

