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
iyn1<-as.character(a[1,3]);iyv1<-as.character(a[1,4]);iys1<-as.numeric(a[1,5])
iydist1<-as.character(a[1,6]);iylink1<-as.character(a[1,7])
ixn1<-as.character(a[1,8]);ixv1<-as.character(a[1,9]);ixs1<-as.numeric(a[1,10])
ixn2<-as.character(a[1,11]);ixv2<-as.character(a[1,12]);ixs2<-as.numeric(a[1,13])
ixn3<-as.character(a[1,14]);ixv3<-as.character(a[1,15]);ixs3<-as.numeric(a[1,16])
ixn4<-as.character(a[1,17]);ixv4<-as.character(a[1,18]);ixs4<-as.numeric(a[1,19])
ixn5<-as.character(a[1,20]);ixv5<-as.character(a[1,21]);ixs5<-as.numeric(a[1,22])
ixn6<-as.character(a[1,23]);ixv6<-as.character(a[1,24]);ixs6<-as.numeric(a[1,25])
ixn7<-as.character(a[1,26]);ixv7<-as.character(a[1,27]);ixs7<-as.numeric(a[1,28])
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
}else if(inumber==6)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
}else if(inumber==7)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
}

library(haplo.stats,lib.loc=R.LibLocation)
library(rms,lib.loc=R.LibLocation)
 
ofname<-"9_3"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"单倍体型分数检验"; 
attach(WD) 
subjvname<-NA; 
yv<-as.data.frame(idata[,c(iyn1)])
yvname<-c(iyv1); 
yvar<-c(iyv1); 
ydist<-c(iydist1);
ylink<-c(''); 
ylv<-c(NA,iys1)[-1]; 
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
##R package## haplo.stats rms ##R package##;
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
set.seed(c(11,13,15,17,19,21,24,27,31,35,39,45,50,56,62,69,76,84,92))
tmp.nna<-apply(is.na(xv),1,sum)
xx<-xv[tmp.nna==0,]
yy<-cbind(yv[tmp.nna==0,],NA)
if (is.na(bvar)) {
  bv<-rep(1,nrow(xx)); bvlbl<-"All"; bgp<-1; nbgp=1;
} else {
  bv<-bv[tmp.nna==0];
  bgp<-levels(factor(bv));nbgp<-length(bgp);
  bvlbl<-vlabelV[match(bvar,vnameV)]; if (is.na(bvlbl)) bvlbl<-bvar;
  bglbl<-vlabelZ[match(paste(bvar,bgp,sep="."),vnameZ)]
  bglbl[is.na(bglbl)]<-bgp[is.na(bglbl)]
}
if (nadj>0) av<-av[tmp.nna==0,]
if (is.na(par1)) par1<-2;
if (par1==1) {
  xx<-geno1to2(xx);loci.lbl<-colnames(xv)
} else {
  loci.lbl<-apply(matrix(colnames(xv),ncol=2,byrow=TRUE),1,function(x) paste(x,collapse="-"))
}
ny<-ncol(yv);yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
#sink(paste(ofname,".txt",sep=""))
nx<-ncol(xx)/2
sim<-(!is.na(parm[1]))



cname1<-c("模型","Global score","自由度","P 值","Global Simulate P 值")
cname2<-c(loci.lbl,"单倍体型频率","单倍体型.分数","P 值","Simulate P 值")
for (y in (1:ny)) {
  w<-c(w,paste("表型:",yb[y],"</br>"))
  for (g in (1:nbgp)) {
    xxg<-xx[bv==bgp[g],]; yyg<-yy[bv==bgp[g],y]
    if (nadj>0) {if (nadj>1) {avg<-av[bv==bgp[g],];} else {avg<-av[bv==bgp[g]];}} else {avg=NA;}
    if (ylv[y]==0) {
     hps.a<-haplo.score(yyg,xxg,trait.type=ydist[y],x.adj=avg,min.count=5, locus.label=loci.lbl,simulate=sim,haplo.effect="additive")
     hps.d<-haplo.score(yyg,xxg,trait.type=ydist[y],x.adj=avg,min.count=5, locus.label=loci.lbl,simulate=sim,haplo.effect="dominant")
     hps.r<-haplo.score(yyg,xxg,trait.type=ydist[y],x.adj=avg,min.count=5, locus.label=loci.lbl,simulate=sim,haplo.effect="recessive")
    }
    print(hps.a);print(hps.d);print(hps.r)
    glba <-c(hps.a$score.global,hps.a$df,hps.a$score.global.p)
    glbd <-c(hps.d$score.global,hps.d$df,hps.d$score.global.p)
    glbr <-c(hps.r$score.global,hps.r$df,hps.r$score.global.p)
    hpoa<-cbind(hps.a$hap.prob,hps.a$score.haplo,hps.a$score.haplo.p)
    hpod<-cbind(hps.d$hap.prob,hps.d$score.haplo,hps.d$score.haplo.p)
    hpor<-cbind(hps.r$hap.prob,hps.r$score.haplo,hps.r$score.haplo.p)
    if (sim) {
      glba<-c(glba,hps.a$score.global.p.sim);
      glbd<-c(glbd,hps.d$score.global.p.sim);
      glbr<-c(glbr,hps.r$score.global.p.sim);
      hpoa<-cbind(hpoa,hps.a$score.haplo.p.sim);
      hpod<-cbind(hpod,hps.d$score.haplo.p.sim);
      hpor<-cbind(hpor,hps.r$score.haplo.p.sim);
    }
    hpoa <-cbind(hps.a$haplotype,format(round(hpoa,5),nsmall=5))
    hpod <-cbind(hps.d$haplotype,format(round(hpod,5),nsmall=5))
    hpor <-cbind(hps.r$haplotype,format(round(hpor,5),nsmall=5))
    
    oo1<-cbind(c("相加模型","显性模型","隐形模型"),format(round(rbind(glba,glbd,glbr),5),nsmall=5))
    tmp.nc<-rep(" ",ncol(hpoa)-1)
    oo2<-rbind(c("相加模型",tmp.nc),format(hpoa),c("显性模型",tmp.nc),format(hpod),c("隐形模型",tmp.nc),format(hpor))
    oo1<-rbind(cname1,format(oo1)); oo2<-rbind(cname2,format(oo2))
    if (!is.na(bvar)) w<-c(w,paste("</br>亚组:", bvlbl, "=", bglbl[g]))
    w<-c(w,"</br>整体分数检验<table border=2 bgcolor=\"wheat\">")
    w<-c(w,paste("<tr><td>",apply(oo1,1,function(x) paste(x,collapse="</td><td>")),"</td></tr>"))
    w<-c(w,"</table>")
    w<-c(w,"</br>特定单倍体型分数<table border=2 bgcolor=\"wheat\">")
    w<-c(w,paste("<tr><td>",apply(oo2,1,function(x) paste(x,collapse="</td><td>")),"</td></tr>"))
    w<-c(w,"</table></br>")
  }
  w<-c(w,"</br>")
}
#sink()
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)