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
irow<-as.character(a[1,5]);icol<-as.character(a[1,6])
irow1<-unlist(strsplit(irow,"[|]"));icol1<-unlist(strsplit(icol,"[|]"))
ixn1<-as.character(a[1,7]);ixv1<-as.character(a[1,8]);ixs1<-as.numeric(a[1,9])
ixn2<-as.character(a[1,10]);ixv2<-as.character(a[1,11]);ixs2<-as.numeric(a[1,12])
ixn3<-as.character(a[1,13]);ixv3<-as.character(a[1,14]);ixs3<-as.numeric(a[1,15])
ixn4<-as.character(a[1,16]);ixv4<-as.character(a[1,17]);ixs4<-as.numeric(a[1,18])
ixn5<-as.character(a[1,19]);ixv5<-as.character(a[1,20]);ixs5<-as.numeric(a[1,21])
ixn6<-as.character(a[1,22]);ixv6<-as.character(a[1,23]);ixs6<-as.numeric(a[1,24])
slt.vname<-c()

if(inumber==1)  {idata<-idata[,c(iyn1,ixn1)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,irow1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,icol1)
}else if(inumber==2)  {idata<-idata[,c(iyn1,ixn1,ixn2)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,irow1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,icol1)
}else if(inumber==3)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,irow1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,icol1)
}else if(inumber==4)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,irow1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,icol1)
}else if(inumber==5)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,irow1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,icol1)
}else if(inumber==6)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,irow1)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,icol1)
}

library(gdata,lib.loc=R.LibLocation)
 
ofname<-"8_4"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"多元方差分析"; 
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
##R package## gdata  ##R package##;
mat2htmltable<-function(mat) {
  mat[is.na(mat)]<-" "
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
plotprofiles <- function(xv, xvcol, xvlab, pngname) {
 y.min<-min(xv,na.rm=TRUE)
 y.max<-max(xv,na.rm=TRUE)
 y.min<-ifelse(y.min>0, y.min*0.9, y.min*1.1)
 y.max<-ifelse(y.max>0, y.max*1.1, y.max*0.9)
 x.tmp<-(1:ncol(xv))
 n.lines<-nrow(xv)
 n.levels<-length(levels(factor(xvcol)))
 if (n.levels>1) {x.max<-ncol(xv)*1.2;} else {x.max<-ncol(xv);}
 png(pngname,width=720,height=560)
 plot(xv[1,]~x.tmp,type="l",lwd=0.2,xlim=c(1,x.max),ylim=c(y.min,y.max), col=xvcol[1],ylab="",xlab="",xaxt="n")
 for (i in (2:n.lines)) {
  par(new=TRUE)
  plot(xv[i,]~x.tmp,type="l",lwd=0.2,xlim=c(1,x.max),ylim=c(y.min,y.max), col=xvcol[i],ylab="",xlab="",xaxt="n")
 }
 text(x = x.tmp, par("usr")[3]*2, labels = xvlab, srt = -90, pos = 1, xpd = TRUE)
 legend(ncol(xv)+0.3,y.max*0.9,colv.lb,lwd=0.5, col=rainbow(n.levels),title=colvb,bty="n")
 dev.off()
}
maov2mat<-function(mas,col1=colvb) {return(cbind(c(col1,"Residuals"),round(mas$stats,6)))}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
bvb<-NA
if (is.na(colvname)) {colv<-bv; colvname<-bvar; bv<-NA; bvar<-NA;}
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
xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)] 
oo<-NA; pp<-NA; pp2<-NA; pp3<-NA; pp4<-NA
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,paste("<h2>多元方差分析</h2>",sep=""))
if (!is.na(colvname)) {
  nlv<-length(colv.lv)
  tmp.col<-rainbow(nlv)[match(colv,colv.lv)]
  if (!is.na(bvar)) {
    for (i in (1:nbg)) {
      plotprofiles(xv[bv==bv.lv[i],],tmp.col[bv==bv.lv[i]],xvar, paste(ofname,"_",bvar,bv.lv[i],".png",sep=""))
      for (j in (1:nlv)) {
       tb<-apply(!is.na(xv[bv==bv.lv[i] & colv==colv.lv[j],]),2,sum)
       tb<-cbind(tb,apply(xv[bv==bv.lv[i] & colv==colv.lv[j],],2,function(x) mean(x,na.rm=TRUE)))
       tb<-cbind(tb,apply(xv[bv==bv.lv[i] & colv==colv.lv[j],],2,function(x)  sd(x,na.rm=TRUE)))
       tb<-cbind(tb,apply(xv[bv==bv.lv[i] & colv==colv.lv[j],],2,function(x) min(x,na.rm=TRUE)))
       tb<-cbind(tb,apply(xv[bv==bv.lv[i] & colv==colv.lv[j],],2,function(x) median(x,na.rm=TRUE)))
       tb<-cbind(tb,apply(xv[bv==bv.lv[i] & colv==colv.lv[j],],2,function(x) max(x,na.rm=TRUE)))
       tb<-round(tb,6)
       tb<-rbind(c(bvb,colvb,"","N","Mean","Sd","Min","Median","Max"), cbind(bv.lb[i],colv.lb[j],xb,tb))
       if (is.na(oo[1])) {oo<-tb;} else {oo<-rbind(oo,tb[-1,]);}
      }
      ma<-maov2mat(summary(manova(xv[bv==bv.lv[i],]~factor(colv[bv==bv.lv[i]]))))
      colnames(ma)[1]<-bvb
      ma<-rbind(colnames(ma),c(bv.lb[i],rep(" ",(ncol(ma)-1))),ma)
      if (is.na(pp[1])) {pp<-ma;} else {pp<-rbind(pp,ma[-1,]);}
    }
    for (j in (1:nlv)) {
      ma2<-maov2mat(summary(manova(xv[colv==colv.lv[j],]~factor(bv[colv==colv.lv[j]]))),col1=bvb)
      colnames(ma2)[1]<-colvb
      ma2<-rbind(colnames(ma2),c(colv.lb[j],rep(" ",(ncol(ma2)-1))),ma2)
      if (j==1) {pp2<-ma2;} else {pp2<-rbind(pp2,ma2[-1,]);}
    }
    pp3<-maov2mat(summary(manova(xv~factor(colv)+factor(bv))),col1=c(colvb,bvb))
    pp3<-rbind(colnames(pp3),pp3)
    pp4<-maov2mat(summary(manova(xv~factor(colv)*factor(bv))),col1=c(colvb,bvb,paste(colvb,bvb,sep="*")))
    pp4<-rbind(colnames(pp4),pp4)
  } else {
    plotprofiles(xv,tmp.col,xvar,paste(ofname,".png",sep=""))
    for (j in (1:nlv)) {
      tb<-apply(!is.na(xv[colv==colv.lv[j],]),2,sum)
      tb<-cbind(tb,apply(xv[colv==colv.lv[j],],2,function(x) mean(x,na.rm=TRUE)))
      tb<-cbind(tb,apply(xv[colv==colv.lv[j],],2,function(x)  sd(x,na.rm=TRUE)))
      tb<-cbind(tb,apply(xv[colv==colv.lv[j],],2,function(x) min(x,na.rm=TRUE)))
      tb<-cbind(tb,apply(xv[colv==colv.lv[j],],2,function(x) median(x,na.rm=TRUE)))
      tb<-cbind(tb,apply(xv[colv==colv.lv[j],],2,function(x) max(x,na.rm=TRUE)))
      tb<-round(tb,6)
      tb<-rbind(c(colvb,"","N","Mean","Sd","Min","Median","Max"),cbind(colv.lb[j],xb,tb))
      if (is.na(oo[1])) {oo<-tb;} else {oo<-rbind(oo,tb[-1,]);}
    }
    ma<-maov2mat(summary(manova(xv~factor(colv))))
    pp<-rbind(colnames(ma),ma)
  }
  w<-c(w,"</br>N,Mean,Sd: </br><table border=3>",mat2htmltable(oo),"</table></br>") 
  if (!is.na(bvb)) {
    w<-c(w,"</br>MANOVA (stratified by ", bvb , "): </br><table border=3>",mat2htmltable(pp),"</table></br>") 
  } else {
    w<-c(w,"</br>MANOVA: </br><table border=3>",mat2htmltable(pp),"</table></br>") 
  }
  if (!is.na(pp2[1])) w<-c(w,"</br>MANOVA (stratified by ", colvb, "):</br><table border=3>",mat2htmltable(pp2),"</table></br>") 
  if (!is.na(pp3[1])) w<-c(w,"</br>MANOVA (no interaction):</br><table border=3>",mat2htmltable(pp3),"</table></br>") 
  if (!is.na(pp4[1])) w<-c(w,"</br>MANOVA (with interaction):</br><table border=3>",mat2htmltable(pp4),"</table></br>") 
} else {w<-c(w,"No group variable specified!")}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)
