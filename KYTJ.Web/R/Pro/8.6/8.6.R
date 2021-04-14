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
iyn3<-as.character(a[1,12]);iyv3<-as.character(a[1,13]);iys3<-as.numeric(a[1,14]);iydist3<-as.character(a[1,15]);iylink3<-as.character(a[1,16])
iyn4<-as.character(a[1,17]);iyv4<-as.character(a[1,18]);iys4<-as.numeric(a[1,19]);iydist4<-as.character(a[1,20]);iylink4<-as.character(a[1,21])
iyn5<-as.character(a[1,22]);iyv5<-as.character(a[1,23]);iys5<-as.numeric(a[1,24]);iydist5<-as.character(a[1,25]);iylink5<-as.character(a[1,26])

if(iynumber==1)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1)
vlabel<-c("样本量(%)","统计量","合计",iyn1)
}else if(iynumber==2)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,iyn2)
vlabel<-c("样本量(%)","统计量","合计",iyn1,iyn2)
}else if(iynumber==3)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,iyn2,iyn3)
vlabel<-c("样本量(%)","统计量","合计",iyn1,iyn2,iyn3)
}else if(iynumber==4)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,iyn2,iyn3,iyn4)
vlabel<-c("样本量(%)","统计量","合计",iyn1,iyn2,iyn3,iyn4)
}else if(iynumber==5)  {
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,iyn2,iyn3,iyn4,iyn5)
vlabel<-c("样本量(%)","统计量","合计",iyn1,iyn2,iyn3,iyn4,iyn5)
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
p1<-as.numeric(as.character(a[3,1]))
izn1<-as.character(a[3,2]);izv1<-as.character(a[3,3]);izs1<-as.numeric(as.character(a[3,4]))
irow<-as.character(a[3,5]);icol<-as.character(a[3,6])
irow1<-unlist(strsplit(irow,"[|]"));icol1<-unlist(strsplit(icol,"[|]"))
vname<-c(vname,irow1)
vlabel<-c(vlabel,icol1)


library(geepack,lib.loc=R.LibLocation)
library(mgcv,lib.loc=R.LibLocation)
library(gdata,lib.loc=R.LibLocation)
 
ofname<-"8_6"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"广义估计方程多应变量回归"; 
attach(WD) 
subjvname<-NA; 
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
  yvar<-c(iyv1.iyv2); 
  ydist<-c(iydist1,iydist2); 
  ylink<-c(iylink1,iylink2); 
  ylv<-c(NA,iys1,iys2)[-1];
}else if(iynumber==3)  {
  yv<-as.data.frame(idata[c(iyn1,iyn2,iyn3)]); 
  yvname<-c(iyn1,iyn2,iyn3); 
  yvar<-c(iyv1,iyv2,iyv3); 
  ydist<-c(iydist1,iydist2,iydist3); 
  ylink<-c(iylink1,iylink2,iylink3); 
  ylv<-c(NA,iys1,iys2,iys3)[-1];
}else if(iynumber==4)  {
  yv<-as.data.frame(idata[c(iyn1,iyn2,iyn3,iyn4)]); 
  yvname<-c(iyn1,iyn2,iyn3,iyn4); 
  yvar<-c(iyv1,iyv2,iyv3,iyv4); 
  ydist<-c(iydist1,iydist2,iydist3,iydist4); 
  ylink<-c(iylink1,iylink2,iylink3,iylink4); 
  ylv<-c(NA,iys1,iys2,iys3,iys4)[-1];
}else if(iynumber==5)  {
  yv<-as.data.frame(idata[c(iyn1,iyn2,iyn3,iyn4,iyn5)]); 
  yvname<-c(iyn1,iyn2,iyn3,iyn4,iyn5); 
  yvar<-c(iyv1,iyv2,iyv3,iyv4,iyv5); 
  ydist<-c(iydist1,iydist2,iydist3,iydist4,iydist5); 
  ylink<-c(iylink1,iylink2,iylink3,iylink4,iylink5); 
  ylv<-c(NA,iys1,iys2,iys3,iys4,iys5)[-1];
} 

if(ixnumber==1)  {
  xv<-as.matrix(idata[ixn1]); 
  xvname<-c(ixn1); 
  xvar<-c(ixv1); 
  xlv<-c(NA,ixs1)[-1];
}else if(ixnumber==2)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  xvname<-c(ixn1,ixn2); 
  xvar<-c(ixv1,ixv2); 
  xlv<-c(NA,ixs1,ixs2)[-1];
}else if(ixnumber==3)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  xvname<-c(ixn1,ixn2,ixn3); 
  xvar<-c(ixv1,ixv2,ixv3); 
  xlv<-c(NA,ixs1,ixs2,ixs3)[-1];
}else if(ixnumber==4)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1];
}else if(ixnumber==5)  {
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
colv<-as.integer(idata[,izn1]);colvname<-c(izv1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-p1;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## geepack mgcv gdata ##R package##;
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
  if (nr>1) {p1<-matrix(p1,nrow=nr);colnames(p1)<-colnames(p);}
  return(p1)
}
getNumber<-function(str, n) {
  str<-substr(str,2,nchar(str)-1)
  for (i in (1:nchar(str))) {if (substr(str,i,i)==",") {p=i; break}; }
  ifelse(n==1,return(substr(str,1,p-1)),return(substr(str,p+1,nchar(str))))
}
legLocate<-function(x,y) {
  xmin<-min(x,na.rm=TRUE); xmax<-max(x,na.rm=TRUE)
  ymin<-min(y,na.rm=TRUE); ymax<-max(y,na.rm=TRUE)
  yoff<-(ymax-ymin); tmp<-table(cut(x,3),cut(y,4))
  tmp.r=which.min(tmp[,4]);tmp.c=4
  if (tmp[2,1]==0) {tmp.r=2;tmp.c=1}
  if (tmp[1,1]==0) {tmp.r=1;tmp.c=1}
  if (tmp[3,1]==0) {tmp.r=3;tmp.c=1}
  if (tmp[2,4]==0) {tmp.r=2;tmp.c=4}
  if (tmp[1,4]==0) {tmp.r=1;tmp.c=4}
  if (tmp[3,4]==0) {tmp.r=3;tmp.c=4}
  pos.y<-colnames(tmp)[tmp.c];  pos.x<-rownames(tmp)[tmp.r];  pct<-0.15
  if (tmp.c==4) {
     if (min(tmp[,4])>0) {pct<-0.3}
     ymax<-ymax+yoff*pct; legy<-ymax;ymin<-ymin-yoff*0.1
  } 
  if (tmp.c==1) {
     if (min(tmp[,1])>0) {pct<-0.3}
     legy<-as.numeric(getNumber(pos.y,2));ymin<-ymin-yoff*pct;ymax=ymax+yoff*0.1
  } 
  legx<-as.numeric(getNumber(pos.x,1))
  return(cbind(xmin,xmax,ymin,ymax,legx,legy))
}
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
setgee<-function(fml) {
  if (ydist[1]=="gaussian") mdl<-geeglm(formula(fml),data=wd, family=gaussian(link="identity"),id=tmp.id,corstr=corstr)
  if (ydist[1]=="binomial") mdl<-geeglm(formula(fml),data=wd, family=binomial(link="logit"),id=tmp.id,corstr=corstr)
  if (ydist[1]=="poisson") mdl<-geeglm(formula(fml),data=wd, family=poisson(link="log"),id=tmp.id,corstr=corstr)
  if (ydist[1]=="gamma") mdl<-geeglm(formula(fml),data=wd, family=Gamma(link="inverse"),id=tmp.id,corstr=corstr)
  if (ydist[1]=="negbin") mdl<-geeglm(formula(fml),data=wd, family=negbin(c(1,10), link="log"),id=tmp.id,corstr=corstr)
  return(mdl)
}
setglm<-function(fml) {
  if (ydist[1]=="gaussian") mdl<-glm(formula(fml),data=wd, family=gaussian(link="identity"))
  if (ydist[1]=="binomial") mdl<-glm(formula(fml),data=wd, family=binomial(link="logit"))
  if (ydist[1]=="poisson") mdl<-glm(formula(fml),data=wd, family=poisson(link="log"))
  if (ydist[1]=="gamma") mdl<-glm(formula(fml),data=wd, family=Gamma(link="inverse"))
  if (ydist[1]=="negbin") mdl<-glm(formula(fml),data=wd, family=negbin(c(1,10), link="log"))
  return(mdl)
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
if (is.na(par1)) par1<-1; if (par1>4) par1<-1;
corstr<-c("independence","unstructured","exchangeable","ar1")[par1]
if (is.na(dec)) dec<-4;
dd<-cbind(colv,yv); cname<-c(colvname,yvname)
if (!is.na(xvname[1])) {dd<-cbind(dd,xv); cname<-c(cname,xvname);}
if (!is.na(bvar)) {dd<-cbind(dd,bv); cname<-c(cname,bvar);}
dd<-dd[apply(is.na(dd),1,sum)==0,]; colnames(dd)<-cname;
detach(WD); rm(WD); WD<-as.data.frame(dd); attach(WD)
ny<-ncol(yv); yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
nobs<-nrow(WD)
colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
colv.lv=levels(factor(colv)); colv.nlv<-length(colv.lv) 
xxname<-colvname;
nx<-0; if (!is.na(xvname[1])) {
 nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
 xxname<-c(xxname,xvname);
}
if (!is.na(bvar)) {
 bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
 bv.lv<-levels(factor(bv)); 
 bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
 bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
 nbg<-length(bv.lv)
}
tmp.xy<-matrix(0,nrow=nobs,ncol=ny)
for (i in (1:ny)) {
 tmp.xyi<-tmp.xy; tmp.xyi[,i]<-WD[,colvname]
 WD1<-cbind(tmp.id=(1:nobs), tmp.y=WD[,yvname[i]], tmp.yidx=i, WD[,xxname], tmp.xyi)
 if (!is.na(bvar)) WD1<-cbind(WD1, WD[,bvar])
 if (i==1) {WDD<-WD1;} else {WDD<-rbind(WDD,WD1);}
 rm(WD1);
}
cname<-c("tmp.id","tmp.y","tmp.yidx",xxname, paste(colvname,yvname,sep="."))
if (!is.na(bvar)) cname<-c(cname,bvar)
colnames(WDD)<-cname
rm(WD);WD<-as.data.frame(WDD)
WD<-WD[with(WD, order(tmp.id)), ];attach(WD)
colvstr=colvname; 
if ((colv.nlv>1) & (colv.nlv<10)) colv.lb<-vlabel[match(paste(colvname,colv.lv,sep="."),vname)]
if ((colv.nlv>2) & (colv.nlv<10)) {
  xxarr1<-c(paste("factor(",colvname,".",yvname,")",sep=""),"factor(tmp.yidx)")
  xxarr2<-c(paste("factor(",colvname,")"),"factor(tmp.yidx)")
  if (!is.na(bvar)) xxarr3<-c(paste("factor(",colvname,")*factor(",bvname,")"),"factor(tmp.yidx)")
} else {
  xxarr1<-c(paste(colvname,yvname,sep="."),"factor(tmp.yidx)")
  xxarr2<-c(colvname,"factor(tmp.yidx)")
  if (!is.na(bvar)) xxarr3<-c(paste(colvname,"*factor(",bvname,")"),"factor(tmp.yidx)")
}
if (!is.na(xvname[1])) {
 xv1<-xvname; xv1[xlv>2]<-paste("factor(",xvname[xlv>2],")",sep="")
 xv1<-paste(xv1,collapse="+"); xxarr1<-c(xxarr1,xv1); xxarr2<-c(xxarr2,xv1);
 if (!is.na(bvar)) xxarr3<-c(xxarr3,xv1)
}
fml1<-paste("tmp.y ~",paste(xxarr1,collapse="+"))
fml2<-paste("tmp.y ~",paste(xxarr2,collapse="+"))
if (!is.na(bvar)) {
  fml3<-paste("tmp.y ~",paste(xxarr3,collapse="+"))
  fml4<-paste("tmp.y ~",paste(c(paste("factor(",bvname,")"),xxarr2),collapse="+"))
}


gee2htmltable<-function(mdl) {
  gs<-summary(mdl)
  coe<-gs$coefficients
  if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
    cnames<-c(colnames(coe),"exp(coef)","95%区间下限","95%区间上限")
    coe<- cbind(coe, exp(coe[,1]), exp(coe[,1]-1.96*coe[,2]), exp(coe[,1]+1.96*coe[,2]))
  }
  if (gs$family[[2]]=="identity") {
    cnames<-c(colnames(coe),"95%区间下限","95%区间上限")
    coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2])
  }
  oo1<-cbind(c("",rownames(coe)),rbind(cnames,numfmt(coe,dec)))
  p1<-c("组数:",length(gs$clusz))
  p2<-c("最大的组内观察数:", max(gs$clusz))
  p3<-c("组内相关性类型:",gs$corstr)
  p4<-c("使用的总观察数:", length(gs$deviance.resid))
  oo2<-rbind(p1,p2,p3,p4)
  oo<-c("</br><table border=3>", mat2htmltable(oo1), "</table>")
  oo<-c(oo,"</br><table border=3>", mat2htmltable(oo2), "</table>")
  return(oo)
}
lrt2models<-function(fml1,fml2,prnopt=1) {
  glm1<-setglm(fml1);         glm2<-setglm(fml2)
  loglik1<-round(logLik(glm1),3);      loglik2<-round(logLik(glm2),3)
  df1<-summary(glm1)$df[1];   df2<-summary(glm2)$df[1]; 
  xsq<-round(abs((loglik1-loglik2)*2),3); ddf=abs(df1-df2);
  p<- round(1-pchisq(xsq, df=ddf),4);
  rr<-rbind(c("Log likelihood","df.","X-square","P.value"), c(loglik1,df1,"",""),c(loglik2,df2,xsq,p))
  rr<-cbind(c("","Model 1","Model 2"),rr)
  oo<-c("</br>对数似然比检验比较两回归模型(未使用GEE，使用GLM):")
  if (prnopt==1) {
    oo<-c(oo,paste("</br>Model 1 用不同回归系数拟合危险因素对各应变量的作用：</br>", fml1))
    oo<-c(oo,paste("</br>Model 2 用同一回归系数拟合危险因素对各应变量的作用：</br>", fml2))
  } else {
    oo<-c(oo,paste("</br>Model 1 危险因素与分层因素交互作用模型：</br>", fml1))
    oo<-c(oo,paste("</br>Model 2 无危险因素与分层因素交互作用模型：</br>", fml2))
  }
  oo<-c(oo,"</br><table border=3>",mat2htmltable(rr),"</table>")
  if (!is.na(p)) {
   if (p<0.05) {
    if (prnopt==1) {
      oo<-c(oo,"</br></br>对数似然比检验结果显著，表示危险因素对各应变量的回归系数显著不同")
      oo<-c(oo,"</br>下面用广义估计方程计算的危险因素对各应变量总的回归系数有可能不适用")
    } else {
      oo<-c(oo,"</br></br>对数似然比检验结果显著，表示危险因素与分层因素交互作用显著")
    }
   } 
  }
  return (oo)
}
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,"<h2>广义估计方程多应变量回归分析</h2>")
wd<-WD;
if (is.na(bvar)) {
  mdl1<-setgee(fml1); mdl2<-setgee(fml2)
  w<-c(w,"</br>回归方程1：不同回归系数拟合危险因素对每个应变量的作用")
  w<-c(w,gee2htmltable(mdl1))
  w<-c(w,lrt2models(fml1,fml2))
  w<-c(w,"</br></br>广义估计方程多应变量回归 (同一回归系数拟合危险因素对每个应变量的作用)")
  w<-c(w,gee2htmltable(mdl2))
  if (colv.nlv>1 & colv.nlv<10) {
    tmp.gb<-colv.lb[match(wd[,4],colv.lv)]; tmp.xb<-yb[wd[,3]]
    png(paste(ofname,"_means.png",sep=""),width=720,height=560)
    if (ydist[1]=="gaussian" | ydist[1]=="binomial") {
      interaction.plot(factor(tmp.xb),factor(tmp.gb),wd$tmp.y,fun = mean,trace.label=colvb,xlab="",ylab="Mean")
    } else {
      interaction.plot(factor(tmp.xb),factor(tmp.gb),wd$tmp.y,fun = median,trace.label=colvb,xlab="",ylab="Median")
    }
    dev.off()
  } else {
    fml<-paste("tmp.y~s(",colvname,",fx=FALSE,by=factor(tmp.yidx))")
    if (ydist[1]=="gaussian") mdl<-gam(formula(fml),data=wd, family=gaussian(link="identity"))
    if (ydist[1]=="binomial") mdl<-gam(formula(fml),data=wd, family=binomial(link="logit"))
    if (ydist[1]=="poisson") mdl<-gam(formula(fml),data=wd, family=poisson(link="log"))
    if (ydist[1]=="gamma") mdl<-gam(formula(fml),data=wd, family=Gamma(link="inverse"))
    if (ydist[1]=="negbin") mdl<-gam(formula(fml),data=wd, family=negbin(c(1,10), link="log"))
    pred<-predict.gam(mdl,type="terms",se.fit=FALSE)
    tmp.pv<-apply(pred,1,sum)+rep(tapply(wd$tmp.y,factor(wd$tmp.yidx),mean),nrow(pred)/ny)
    tmp.xx<-wd[,4];
    tmp.rnk<-rank(tmp.xx,ties.method="first")
    tmp.xx[tmp.rnk]<-tmp.xx
    tmp.pv[tmp.rnk]<-tmp.pv
    xy<-legLocate(tmp.xx,tmp.pv)
    tmp.col<-rainbow(ny); px<-c(20,1:9)
    tmp.pvi<-tmp.pv[tmp.yidx==1]; tmp.xxi<-tmp.xx[tmp.yidx==1];
    png(paste(ofname,"_smooth.png",sep=""),width=720,height=560)
    plot(tmp.pvi~tmp.xxi,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[1], type="l",lty=1,lwd=2,ylab="Smoothing", xlab=colvb)
    for (i in (2:ny)) {
      tmp.pvi<-tmp.pv[tmp.yidx==i]; tmp.xxi<-tmp.xx[tmp.yidx==i];par(new=TRUE); 
      plot(tmp.pvi~tmp.xxi,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[i], type="l",lty=i,lwd=2,ylab="", xlab="")
    }
    legend(xy[5],xy[6],yb,lty=(1:ny),bty="n",col=tmp.col)
    dev.off()
  }
} else {
  for (b in (1:nbg)) {
    rm(wd); wd<-WD[(WD[,bvar]==bv.lv[b]),]; 
    w<-c(w,paste("</br>亚组:", bvb, "=", bv.lb[b], "</br>"))
    mdl1<-setgee(fml1); mdl2<-setgee(fml2)
    w<-c(w,"</br>回归方程1：不同回归系数拟合危险因素对每个应变量的作用")
    w<-c(w,gee2htmltable(mdl1))
    w<-c(w,lrt2models(fml1,fml2))
    w<-c(w,"</br></br>广义估计方程多应变量回归 (同一回归系数拟合危险因素对每个应变量的作用)")
    w<-c(w,gee2htmltable(mdl2))
    if (colv.nlv>1 & colv.nlv<10) {
       tmp.gb<-colv.lb[match(wd[,4],colv.lv)]; tmp.xb<-yb[wd[,3]]
       png(paste(ofname,"_",bvar,bv.lv[b],"_means.png",sep=""),width=720,height=560)
       if (ydist[1]=="gaussian" | ydist[1]=="binomial") {
         interaction.plot(factor(tmp.xb),factor(tmp.gb),wd$tmp.y,fun = mean,trace.label=colvb,xlab="",ylab="Mean")
       } else {
         interaction.plot(factor(tmp.xb),factor(tmp.gb),wd$tmp.y,fun = median,trace.label=colvb,xlab="",ylab="Median")
       }
       dev.off()
    }  else {
       fml<-paste("tmp.y~s(",colvname,",fx=FALSE,by=factor(tmp.yidx))")
       if (ydist[1]=="gaussian") mdl<-gam(formula(fml),data=wd, family=gaussian(link="identity"))
       if (ydist[1]=="binomial") mdl<-gam(formula(fml),data=wd, family=binomial(link="logit"))
       if (ydist[1]=="poisson") mdl<-gam(formula(fml),data=wd, family=poisson(link="log"))
       if (ydist[1]=="gamma") mdl<-gam(formula(fml),data=wd, family=Gamma(link="inverse"))
       if (ydist[1]=="negbin") mdl<-gam(formula(fml),data=wd, family=negbin(c(1,10), link="log"))
       pred<-predict.gam(mdl,type="terms",se.fit=FALSE)
       tmp.pv<-apply(pred,1,sum)+rep(tapply(wd$tmp.y,factor(wd$tmp.yidx),mean),nrow(pred)/ny)
       tmp.xx<-wd[,4];
       tmp.rnk<-rank(tmp.xx,ties.method="first")
       tmp.xx[tmp.rnk]<-tmp.xx
       tmp.pv[tmp.rnk]<-tmp.pv
       xy<-legLocate(tmp.xx,tmp.pv)
       tmp.col<-rainbow(ny); px<-c(20,1:9)
       tmp.pvi<-tmp.pv[tmp.yidx==1]; tmp.xxi<-tmp.xx[tmp.yidx==1];
       png(paste(ofname,"_",bvar,bv.lv[b],"_smooth.png",sep=""),width=720,height=560)
       plot(tmp.pvi~tmp.xxi,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[1], type="l",lty=1,lwd=2,ylab="Smoothing", xlab=colvb)
       for (i in (2:ny)) {
         tmp.pvi<-tmp.pv[tmp.yidx==i]; tmp.xxi<-tmp.xx[tmp.yidx==i];par(new=TRUE); 
         plot(tmp.pvi~tmp.xxi,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[i], type="l",lty=i,lwd=2,ylab="", xlab="")
       }
       legend(xy[5],xy[6],yb,lty=(1:ny),bty="n",col=tmp.col)
       dev.off()
     }
  }
  rm(wd);wd<-WD;
  mdl4<-setgee(fml4)
  mdl3<-setgee(fml3)
  w<-c(w,paste("</br></br>For total</br>"))
  w<-c(w,"</br>广义估计方程多应变量回归 (同一回归系数拟合危险因素对每个应变量的作用: 危险因素与分层因素交互作用)")
  w<-c(w,gee2htmltable(mdl3))
  w<-c(w,"</br>广义估计方程多应变量回归 (同一回归系数拟合危险因素对每个应变量的作用: 无危险因素与分层因素交互作用)")
  w<-c(w,gee2htmltable(mdl4))
  w<-c(w,lrt2models(fml3,fml4,prnopt=2))
}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)

