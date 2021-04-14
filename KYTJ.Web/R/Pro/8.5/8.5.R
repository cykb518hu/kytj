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

 
ofname<-"8_5"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"两样本多元反应曲线图比较"; 
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
 plot(xv[1,]~x.tmp,type="l",lwd=0.2,xlim=c(1,x.max),ylim=c(y.min,y.max),col=xvcol[1],ylab="",xlab="",xaxt="n")
 for (i in (2:n.lines)) {
  par(new=TRUE)
  plot(xv[i,]~x.tmp,type="l",lwd=0.2,xlim=c(1,x.max),ylim=c(y.min,y.max), col=xvcol[i],ylab="",xlab="",xaxt="n")
 }
 text(x = x.tmp, par("usr")[3]*2, labels = xvlab, srt = -90, pos = 1, xpd = TRUE)
 legend(ncol(xv)+0.3,y.max*0.9,colv.lb,lwd=0.5,col=rainbow(n.levels),title=colvb,bty="n")
 dev.off()
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
x.lb<-vlabelV[match(xvname,vnameV)]; x.lb[is.na(x.lb)]<-xvname[is.na(x.lb)];
nx<-ncol(xv)
if (!is.na(colvname)) {
  dd<-cbind(xv,colv);  if (!is.na(bvar)) dd<-cbind(dd,bv)
  dd<-dd[apply(is.na(dd),1,sum)==0,]
  tmp.xx<-dd[,(1:nx)]; g1<-dd[,nx+1];
  colv.lv<-levels(factor(g1));
  colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
  colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  nlv<-length(colv.lv)
}
if (!is.na(bvar)) {
 g2<-dd[,nx+2]; 
 bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
 bv.lv<-levels(factor(bv)); 
 bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
 bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
 nbg<-length(bv.lv)
}


w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,paste("<h2>多变量反应曲线图分析:</h2>",sep=""))
w<-c(w,paste("分组变量:", colvname,"</br>"))
w<-c(w,paste("分层变量:", bvar,"</br>"))
profile2<-function(X1,X2,st) {
 dec<-6
 n1<-nrow(X1);n2<-nrow(X2);p<-ncol(X1);X1bar<-apply(X1,2,mean);X2bar<-apply(X2,2,mean)
 S1<-var(X1);S2=var(X2); ss1<-diag(S1);ss2<-diag(S2)
 sdp=sqrt(((n1-1)*ss1+(n2-1)*ss2)/(n1+n2-2))
 t1=(X1bar-X2bar)/(sdp*sqrt(1/n1+1/n2));df<-n1+n2-2
 t1pval<-2*(apply(rbind(pt(t1,df),1-pt(t1,df)),2,min))
 tmp.ttest<-round(cbind(n1,X1bar,n2,X2bar,t1,df,t1pval),dec)
 oo1<-cbind(c(" ",x.lb),rbind(c("N1","均数1","N2","均数2","t","自由度","P 值"),tmp.ttest))

 sp<-((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
 t2<-((n1*n2)/(n1+n2))*(X1bar-X2bar)%*%solve(sp)%*%(X1bar-X2bar)
 f1<-(n1+n2-p-1)/((n1+n2-2)*p)*t2; df<-n1+n2-p-1; t2pval<-1-pf(f1,p,df)
 tmp.tmean<-round(cbind(n1,n2,t2,f1,p,df,t2pval),dec)
 oo2<-rbind(c("N1","N2","T 平方","F","自由度1","自由度2","P 值"),tmp.tmean)

 z<-cbind(diag(p-1),matrix(0,nrow=p-1,ncol=1))-cbind(matrix(0,nrow=p-1,ncol=1),diag(p-1))
 zz<-z%*%sp%*%t(z)
 t2<-(n1*n2/(n1+n2))*((X1bar-X2bar)%*%t(z))%*%solve(zz)%*%t((X1bar-X2bar)%*%t(z))
 f2<-(n1+n2-p)/((n1+n2-2)*(p-1))*t2; df<-n1+n2-p; t2pval<-1-pf(f2,p-1,df)
 tmp.tpara<-round(cbind(n1,n2,t2,f2,p-1,df,t2pval),dec)
 oo3<-rbind(c("N1","N2","T 平方","F","自由度1","自由度2","P 值"),tmp.tpara)

 z<-matrix(1,nrow=1,ncol=p); zz<-z%*%sp%*%t(z)
 t2<-((n1*n2)/(n1+n2))*((X1bar-X2bar)%*%t(z))^2/zz
 df<-n1+n2-2; t2pval<-1-pf(t2,1,df)
 tmp.tcoin<-round(cbind(n1,n2,t2,t2,1,df,t2pval),dec)
 oo4<-rbind(c("N1","N2","T 平方","F","自由度1","自由度2","P 值"),tmp.tcoin)
 
 X<-rbind(X1,X2); Xbar<-mean(X,na.rm=TRUE)
 BSS<-sum((apply(X,2,function(x) mean(x,na.rm=TRUE))-Xbar)^2, na.rm=TRUE)*(n1+n2)
 TSS<-sum((X-Xbar)^2,na.rm=TRUE); WSS<-TSS-BSS; BMS<-BSS/(p-1); 
 WMS<-WSS/((n1+n2)*p-p); f<-BMS/WMS; df<-(n1+n2)*p-p; fpval<-1-pf(f,p-1,df)
 tmp.tflat<-round(cbind(TSS,BSS,p-1,BMS,WSS,df,WMS,f,fpval),dec)
 oo5<-rbind(c("总SS","组间SS","自由度1","组间MS","组内SS","自由度2","组内MS","F","P 值"),tmp.tflat)

 return(list(oo1,oo2,oo3,oo4,oo5))
}
if (!is.na(colvname)) {
  tmp.col<-rainbow(nlv)[match(g1,colv.lv)]
  if (!is.na(bvar)) {
    for (i in (1:nbg)) {
      plotprofiles(tmp.xx[g2==bv.lv[i],],tmp.col[g2==bv.lv[i]],xvar, paste(ofname,"_",bvar,bv.lv[i],".png",sep=""))
      for (j in (1:(nlv-1))) {
        for (k in ((j+1):nlv)) {
          tmp.d1<-tmp.xx[g1==colv.lv[j] & g2==bv.lv[i],]; 
          tmp.d2<-tmp.xx[g1==colv.lv[k] & g2==bv.lv[i],];
          tmp.oo<-profile2(tmp.d1,tmp.d2, paste("_",bvar,bv.lv,sep=""));
          tmp.oo[[1]]<-cbind(c(bvb,rep(bv.lb[i],nx)),tmp.oo[[1]])
          for (s in (2:5)) {tmp.oo[[s]]<-cbind(c(bvb,bv.lb[i]),tmp.oo[[s]]);}
          if (i==1) {oo<-tmp.oo;} else {for (s in (1:5)) {oo[[s]]<-rbind(oo[[s]],tmp.oo[[s]][-1,]);}}
          w<-c(w,paste(bvb,"=",bv.lb[i],"</br>"))
          w<-c(w,paste(colvb, ": ", colv.lb[j], " (1) vs. ", colv.lb[k]," (2)",sep=""))
          w<-c(w,"</br>每单个变量的 t 检验<table border=3>",mat2htmltable(oo[[1]]),"</table></br>")
          w<-c(w,"</br>Hotelling T 平方检验<table border=3>",mat2htmltable(oo[[2]]),"</table></br>")
          w<-c(w,"</br>检验平行性<table border=3>",mat2htmltable(oo[[3]]),"</table></br>")
          w<-c(w,"</br>检验一致性<table border=3>",mat2htmltable(oo[[4]]),"</table></br>")
          w<-c(w,"</br>检验反应曲线是否呈水平性<table border=3>",mat2htmltable(oo[[5]]),"</table></br>")
        }
      }
      y<-matrix(t(tmp.xx[g1==colv.lv[1] & g2==bv.lv[i],]),byrow=FALSE,nrow=1)
      g<-rep(colv.lb[1],length(y))
      x<-rep(x.lb,sum(g1==colv.lv[1] & g2==bv.lv[i]))
      for (j in (2:nlv)) {
        y.j<-matrix(t(tmp.xx[g1==colv.lv[j] & g2==bv.lv[i],]),byrow=FALSE,nrow=1)
        g.j<-rep(colv.lb[j],length(y.j))
        x.j<-rep(x.lb,sum(g1==colv.lv[j] & g2==bv.lv[i]))
        y<-c(y,y.j); g<-c(g,g.j); x<-c(x,x.j)
      }
      png(paste(ofname,"_",bvar,bv.lv[i],"_means.png",sep=""),width=720,height=560)
      interaction.plot(factor(x),factor(g),y,trace.label=colvb,xlab="",ylab="Mean")
      dev.off()
    }
  } else {
    plotprofiles(tmp.xx,tmp.col,xvar,paste(ofname,".png",sep=""))
    for (j in (1:(nlv-1))) {
      for (k in ((j+1):nlv)) {
        tmp.d1<-tmp.xx[g1==colv.lv[j],]; tmp.d2<-tmp.xx[g1==colv.lv[k],];
        oo<-profile2(tmp.d1,tmp.d2,"")
        w<-c(w,paste(colvb, ": ", colv.lb[j], " (1) vs. ", colv.lb[k], " (2)",sep=""))
        w<-c(w,"</br>每单个变量的 t 检验<table border=3>",mat2htmltable(oo[[1]]),"</table></br>")
        w<-c(w,"</br>Hotelling T 平方检验<table border=3>",mat2htmltable(oo[[2]]),"</table></br>")
        w<-c(w,"</br>检验平行性<table border=3>",mat2htmltable(oo[[3]]),"</table></br>")
        w<-c(w,"</br>检验一致性<table border=3>",mat2htmltable(oo[[4]]),"</table></br>")
        w<-c(w,"</br>检验反应曲线是否呈水平性<table border=3>",mat2htmltable(oo[[5]]),"</table></br>")
      }
    }
    y<-matrix(t(tmp.xx[g1==colv.lv[1],]),byrow=FALSE,nrow=1)
    g<-rep(colv.lb[1],length(y))
    x<-rep(x.lb,sum(g1==colv.lv[1]))
    for (j in (2:nlv)) {
       y.j<-matrix(t(tmp.xx[g1==colv.lv[j],]),byrow=FALSE,nrow=1)
       g.j<-rep(colv.lb[j],length(y.j))
       x.j<-rep(x.lb,sum(g1==colv.lv[j]))
       y<-c(y,y.j); g<-c(g,g.j); x<-c(x,x.j)
    }
    png(paste(ofname,"_means.png",sep=""),width=720,height=560)
    interaction.plot(factor(x),factor(g),y,trace.label=colvb,xlab="",ylab="Mean")
    dev.off()
  }
}
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)
