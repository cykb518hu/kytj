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
}

 
ofname<-"8_3"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"多元Hotelling T平方检验"; 
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
##R package##  ##R package##;
mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
if (is.na(colvname)) {colv<-bv; colvname<-bvar; bv<-NA; bvar<-NA;}
tb<-cbind(apply(!is.na(xv),2,sum),apply(xv,2,function(x) mean(x,na.rm=TRUE)),apply(xv,2,function(x) sd(x,na.rm=TRUE)))
tb<-rbind(c("N","Mean","Sd"),tb); tb<-cbind(c(" ",xb),tb)
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,paste("<h2>多元Hotelling T平方检验</h2>",sep=""))
w<-c(w,"</br><table border=3>",mat2htmltable(tb),"</table></br>")
ht2<-function(d1,d2) {
 n1<-nrow(d1);n2<-nrow(d2);xbar1<-apply(d1,2,mean);xbar2<-apply(d2,2,mean)
 dbar<-xbar2-xbar1;k=ncol(d1)
 v <- ((n1-1)*var(d1)+(n2-1)*var(d2))/(n1+n2-2)
 t2 <- n1*n2*dbar%*%solve(v)%*%dbar/(n1+n2)
 f <- (n1+n2-k-1)*t2/((n1+n2-2)*k)
 pvalue <- 1-pf(f,k,n1+n2-k-1);df=n1+n2-k-1
 return(c(n1,n2,round(t2,4),round(f,4),k,df,round(pvalue,6)))
}
oo<-NA;pp<-NA
if (!is.na(colvname)) {
  dd<-cbind(xv,colv);
  if (!is.na(bvar)) dd<-cbind(dd,bv)
  dd<-dd[apply(is.na(dd),1,sum)==0,]
  tmp.xx<-dd[,(1:nx)]; g1<-dd[,nx+1];
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  colv.lv<-levels(factor(colv))
  colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
  colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
  nlv<-length(colv.lv)
  if (!is.na(bvar)) {
    g2<-dd[,nx+2]
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
    for (i in (1:nbg)) {
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
      for (j in (1:(nlv-1))) {
        for (k in ((j+1):nlv)) {
          tmp.d1<-tmp.xx[g1==colv.lv[j] & g2==bv.lv[i],]; tmp.d2<-tmp.xx[g1==colv.lv[k] & g2==bv.lv[i],];
          tmp.oo<-c(bv.lb[i],colv.lb[j],colv.lb[k],ht2(tmp.d1,tmp.d2));
          if (is.na(pp[1])) {pp<-tmp.oo;} else {pp<-rbind(pp,tmp.oo);}
        }
      }
    }
    pp<-rbind(c(bvb,"Sample1","Sample2","N1","N2","T-square","F","df.1","df.2","P.value"),pp)
  } else {
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
      for (j in (1:(nlv-1))) {
        for (k in ((j+1):nlv)) {
          tmp.d1<-tmp.xx[g1==colv.lv[j],]; tmp.d2<-tmp.xx[g1==colv.lv[k],];
          tmp.oo<-c(colv.lb[j],colv.lb[k],ht2(tmp.d1,tmp.d2));
          if (is.na(pp[1])) {pp<-tmp.oo;} else {pp<-rbind(pp,tmp.oo);}
        }
      }
    pp<-rbind(c("Sample1","Sample2","N1","N2","T-square","F","df.1","df.2","P.value"),pp)
  }
  w<-c(w,"</br><table border=3>",mat2htmltable(oo),"</table></br>")
  w<-c(w,"</br>Hotelling T-square test<table border=3>",mat2htmltable(pp),"</table></br>")
}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)