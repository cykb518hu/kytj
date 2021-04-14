  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\202005\\test\\0203"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  cparm1 <- as.character(pa[4,1])
  cparm2 <- as.character(pa[5,1])

  ixn <- unlist(strsplit(xparm1,"[,]")) 
  ixv <- unlist(strsplit(xparm2,"[,]")) 
  ixs <- unlist(strsplit(xparm3,"[,]")) 
  icn <- unlist(strsplit(cparm1,"[|]"))  
  icv <- unlist(strsplit(cparm2,"[|]")) 
  
  R.LibLocation <- rlib
  d <- read.csv("./data.csv")
  idata <- data.frame(d)
  colnames(idata)<-c(toupper(names(idata)))
  
  library(showtext,lib.loc=R.LibLocation)
  showtext_auto(enable=TRUE)
  library(doBy,lib.loc=R.LibLocation)
  library(plotrix,lib.loc=R.LibLocation)
  library(stringi,lib.loc=R.LibLocation)
  library(stringr,lib.loc=R.LibLocation)
  library(survival,lib.loc=R.LibLocation)
  library(rms,lib.loc=R.LibLocation)
  library(nnet,lib.loc=R.LibLocation)
  library(car,lib.loc=R.LibLocation)
  library(mgcv,lib.loc=R.LibLocation)
  pdfwd<-6; pdfht<-6
  if (length(which(ls()=="ClinStats"))==0) ClinStats<-get(ls()[1])
  names(ClinStats)<-toupper(names(ClinStats))
  
  vname<-c("N","STAT","TOTAL",ixn,icn)
  vlabel<-c("样本(%)","统计量","合计",ixv,icv)
  
  library(gdata,lib.loc=R.LibLocation)
  library(gplots,lib.loc=R.LibLocation)
  
  ofname<-"2_3";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"方差分析"
  attach(WD) 
  subjvname<-NA;
  
  library(stringr)
  sq <- str_count(xparm1,",")+1
  xv<-cbind(idata[,1])
  for (s1 in (1:sq)) {
    v1<-ixn[s1]
    xv<-cbind(xv,idata[,v1])
    s1=s1+1
  }
  colnames(xv)<-c(1,ixn)
  xv<-as.data.frame(xv[,-1])
  xvname<-c(ixn); 
  xvar<-c(ixn); 
  xlv<-c(NA,ixs)[-1]; 
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-as.matrix(idata[,icn[1]]);colvname<-c(icn[1]); 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## gdata gplots ##R package##;
  pvformat<-function(p,dec) {
    pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
    if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p)); colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
    lw <- paste("<",substr("0.00000000000",1,dec+1),"1",sep="");
    pp[as.numeric(p)<(1/10^dec)]<-lw
    return(pp)
  }
  numfmt<-function(p,dec) {
    if (is.list(p)) p<-as.matrix(p)
    pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
    if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p));colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
    pp[as.numeric(p)>10000000]<- "inf."
    pp[is.na(p) | gsub(" ","",p)==""]<- ""
    pp[p=="-Inf"]<-"-Inf"
    pp[p=="Inf"]<-"Inf"
    return(pp)
  }
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  aovMeanSd <- function(v,g) {
    N<-tapply(v,g,function(x) sum(!is.na(x)))
    Mean<-tapply(v,g,function(x) round(mean(x,na.rm=TRUE),dec))
    Sd<-tapply(v,g,function(x) round(sd(x,na.rm=TRUE),dec))
    Min<-tapply(v,g,function(x) round(min(x,na.rm=TRUE),dec))
    Max<-tapply(v,g,function(x) round(max(x,na.rm=TRUE),dec))
    Median<-tapply(v,g,function(x) round(median(x,na.rm=TRUE),dec))
    tmp.o<-cbind(N,Mean,Sd,Min,Median,Max)
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
  convert2label<-function(x,vv,bb) {
    newx<-x
    for (i in (1:length(vv))) newx[x==vv[i]]<-bb[i]
    return(newx)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
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
  nobs<-nrow(xv); oo<-NA; pp<-NA
  
  
  
  aov2mat<-function(aovobj,xname) {
    m<-matrix(unlist(summary(aovobj)),ncol=5)
    m11<-numfmt(m[,2:4],dec)
    if (!is.matrix(m11)) m11<-matrix(m11,nrow=1)
    m<-cbind(c(xname,"Residuals"),m[,1],m11,pvformat(m[,5],dec+2))
    return(rbind(c("模型","自由度","离均差平方和","均方","F 值","P 值"),m))
  }
  aovhsd2mat<-function(aovobj,xname,xlvv) {
    m<-TukeyHSD(aovobj)
    g1<-lv2gg(xlvv[[1]]); m1<-matrix(unlist(m[1]),ncol=4)
    m11<-numfmt(m1[,-ncol(m1)],dec)
    if (!is.matrix(m11)) m11<-matrix(m11,nrow=1)
    m1<-cbind(m11,pvformat(m1[,ncol(m1)],dec+2))
    m1<-cbind(g1[,1],g1[,2],m1)
    m1<-rbind(c(xname[1],rep(" ",5)),m1)
    if (length(xlvv)>1) {
      g2<-lv2gg(xlvv[[2]]);m2<-matrix(unlist(m[2]),ncol=4)
      m2<-cbind(g2[,1],g2[,2],format(round(m2,dec),nsmall=dec))
      m2<-rbind(c(xname[2],rep(" ",5)),m2)
      m1<-rbind(m1,m2)
    }
    colnames(m1)<-c("组 1","组 2","均数差","95%区间下限","95%区间上限","P 值")
    return(m1)
  }
  lv2gg<-function(xlvi) {
    t<-length(xlvi);gg<-c(NA,NA)
    for (i in (1:(t-1))) {for (j in ((i+1):t)) {gg<-rbind(gg,c(xlvi[j],xlvi[i]));}}
    return(matrix(gg[-1,],ncol=2))
  }
  aovHSD<-function(aovobj,xvname) {
    m<-TukeyHSD(aovobj)
    mx<-m[[1]]
    if (!is.matrix(mx)) mx<-matrix(mx,nrow=1)
    tmp1<-matrix(unlist(strsplit(rownames(mx),"-")),nrow=nrow(mx),byrow=TRUE)
    tmpvlb<-vlabel[match(paste(xvname[1],tmp1,sep="."),vname)]
    tmpvlb[is.na(tmpvlb)]<-tmp1[is.na(tmpvlb)]
    tmp1<-matrix(tmpvlb,nrow=nrow(mx))
    mx1<-numfmt(mx[,-ncol(mx)],dec)
    if (!is.matrix(mx1)) mx1<-matrix(mx1,nrow=1)
    mx<-cbind(mx1,pvformat(mx[,ncol(mx)],dec+2))
    mm<-cbind(cbind(tmp1[,1],".","&nbsp&nbsp-&nbsp&nbsp",tmp1[,2],"."),mx)
    if (length(m)>1) {
      mx<-m[[2]]
      if (!is.matrix(mx)) mx<-matrix(mx,nrow=1)
      tmp1<-matrix(unlist(strsplit(rownames(mx),"-")),nrow=nrow(mx),byrow=TRUE)
      tmp1<-matrix(vlabel[match(paste(xvname[2],tmp1,sep="."),vname)],nrow=nrow(mx))
      mx1<-numfmt(mx[,-ncol(mx)],dec)
      if (!is.matrix(mx1)) mx1<-matrix(mx1,nrow=1)
      mx<-cbind(mx1,pvformat(mx[,ncol(mx)],dec+2))
      m1<-cbind(cbind(".",tmp1[,1],"&nbsp&nbsp-&nbsp&nbsp",".",tmp1[,2]),mx)
      mm<-rbind(mm,m1)
    }
    if (length(m)>2) {
      mx<-m[[3]]
      if (!is.matrix(mx)) mx<-matrix(mx,nrow=1)
      tmp1<-matrix(unlist(strsplit(rownames(mx),"-")),nrow=nrow(mx),byrow=TRUE)
      tmp1<-matrix(unlist(strsplit(matrix(t(tmp1),nrow=1),":")),nrow=nrow(mx),byrow=TRUE)
      tmp1[,1]<-vlabel[match(paste(xvname[1],tmp1[,1],sep="."),vname)]
      tmp1[,2]<-vlabel[match(paste(xvname[2],tmp1[,2],sep="."),vname)]   
      tmp1[,3]<-vlabel[match(paste(xvname[1],tmp1[,3],sep="."),vname)]
      tmp1[,4]<-vlabel[match(paste(xvname[2],tmp1[,4],sep="."),vname)]
      mx1<-numfmt(mx[,-ncol(mx)],dec)
      if (!is.matrix(mx1)) mx1<-matrix(mx1,nrow=1)
      mx<-cbind(mx1,pvformat(mx[,ncol(mx)],dec+2))
      m1<-cbind(tmp1[,(1:2)],"&nbsp&nbsp-&nbsp&nbsp",tmp1[,(3:4)],mx)
      mm<-rbind(mm,m1)
    }
    xvlabel<-vlabel[match(xvname,vname)]
    if (length(xvname)==1) {
      mm<-mm[,-c(2,5)]; if (!is.matrix(mm)) mm<-matrix(mm,nrow=1)
      colnames(mm)<-c(xvlabel,"&nbsp&nbsp-&nbsp&nbsp",xvlabel,"均数差","95%区间下限","95%区间上限","P 值")
    }
    if (length(xvname)==2) {
      colnames(mm)<-c(xvlabel,"&nbsp&nbsp-&nbsp&nbsp",xvlabel,"均数差","95%区间下限","95%区间上限","P 值")
    }
    mm<-rbind(colnames(mm),mm); return(mm)
  }
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" />")
  w<-c(w,"<style type=\"text/css\">tr td {text-align:center;}</style></head><body>")
  w<-c(w,paste("<h2>方差分析</h2>",sep=""))
  if (is.na(colvname) ) {
    y<-as.vector(xv); x<-as.vector(t(matrix(rep(1:nx,nrow(xv)),nrow=nx)))
    colvname<-"Variables"; colv.lb<-xb; colv.lv<-(1:nx)
    tmp.obs<-rep(1:nobs,nx)
    if (!is.na(bvar)) {
      g<-rep(bv,nx)
      tmp.m<-NA; tmp.names<-NA
      tmp.nms<-aovMeanSd2(y,x,g); tmp.nms[,1]<-bv.lb[match(tmp.nms[,1],bv.lv)]; colnames(tmp.nms)[1]<-bvb
      rownames(tmp.nms)<-xb[as.numeric(rownames(tmp.nms))]
      tmp.nms<-rbind(c(" ",colnames(tmp.nms)),cbind(rownames(tmp.nms),tmp.nms))
      for (j in (1:nbg)) {
        tmp.aov<-aov(y[g==bv.lv[j]]~factor(x[g==bv.lv[j]])+factor(tmp.obs[g==bv.lv[j]]))
        oij<-aov2mat(tmp.aov,c("变量","观察对象"))     
        oij<-cbind(c(bvb,bv.lb[j],rep(" ",nrow(oij))),rbind(matrix(" ",ncol=ncol(oij),nrow=2),oij))
        if (is.na(oo[1])) {oo<-oij;} else {oo<-rbind(oo,oij[-1,]);}
        pij<-aovhsd2mat(tmp.aov,c("变量","观察对象"),list(xb))
        pij<-cbind(c(bvb,bv.lb[j],rep(" ",nrow(pij))),rbind(colnames(pij),rep(" ",ncol=ncol(pij)),pij))
        if (is.na(pp[1])) {pp<-pij;} else {pp<-rbind(pp,pij[-1,]);}
        tmp.m<-cbind(tmp.m,tapply(y[g==bv.lv[j]],factor(x[g==bv.lv[j]]),function(x) mean(x,na.rm=TRUE)))
        tmp.names<-c(tmp.names,paste(xb,bv.lb[j],sep="\n"))
      }
      tmp.names<-tmp.names[-1]; tmp.m<-tmp.m[,-1]
      w<-c(w,"</br>N、均数、标准差: </br><table border=3>",mat2htmltable(tmp.nms),"</table></br>") 
      w<-c(w,"</br>方差分析: </br><table border=3>",mat2htmltable(oo),"</table></br>") 
      w<-c(w,"</br>方差分析后多组间两两比较: </br><table border=3>",mat2htmltable(pp),"</table></br>") 
      
      png(paste(ofname,"boxplot.png",sep="_"),width=720,height=560)
      boxplot(y~factor(x)*factor(g),names=tmp.names,main="",ylab="",col=rainbow(nx))
      dev.off()
      tmp.maxy<-max(tmp.m,na.rm=TRUE);tmp.maxx<-(nx+1)*nbg+1
      png(paste(ofname,"barplot.png",sep="_"),width=720,height=560)
      barplot(tmp.m,beside=TRUE,space=c(0,1), names=bv.lb, legend=xb, ylim=c(0,tmp.maxy), xlim=c(0,tmp.maxx), ylab="均数", col=rainbow(nx), args.legend=list())
      dev.off()
      png(paste(ofname,"means.png",sep="_"),width=720,height=560)
      cmp<-!is.na(x) & !is.na(g) & !is.na(y)
      tmp.xfactor<-factor(xb[x[cmp]])
      tmp.tfactor<-factor(bv.lb[g[cmp]])
      interaction.plot(tmp.xfactor,tmp.tfactor, y[cmp], trace.label=bvb, xlab="", ylab="均数", main="", type="b", pch=c(19:25)[-2])
      dev.off()
    } else {
      tmp.nms<-aovMeanSd(y,x); tmp.nms<-rbind(c(" ",colnames(tmp.nms)),cbind(xb,tmp.nms))
      tmp.aov<-aov(y~factor(x)+factor(tmp.obs))
      oo<-aov2mat(tmp.aov,c("变量","观察对象"))     
      pp<-aovhsd2mat(tmp.aov,c("变量","观察对象"),list(xb))
      pp<-rbind(colnames(pp),pp)
      w<-c(w,"</br>N、均数、标准差: </br><table border=3>",mat2htmltable(tmp.nms),"</table></br>") 
      w<-c(w,"</br>方差分析: </br><table border=3>",mat2htmltable(oo),"</table></br>") 
      w<-c(w,"</br>方差分析后多组间两两比较: </br><table border=3>",mat2htmltable(pp),"</table></br>") 
      tmp.m<-tapply(y,factor(x),function(x) mean(x,na.rm=TRUE))
      png(paste(ofname,"_boxplot.png",sep=""),width=720,height=560)
      boxplot(y~factor(x),names=xb,main="",ylab="",col=rainbow(nx))
      dev.off()
      png(paste(ofname,"_barplot.png",sep=""),width=720,height=560)
      barplot(tmp.m,space=1,names=xb,main="均数",ylab="",col=rainbow(nx))
      dev.off()
      png(paste(ofname,"_means.png",sep=""),width=720,height=560)
      try(plotmeans(y~factor(x),legends=xb,main="均数",xlab="",ylab=" "))
      dev.off()
    }
  } else {
    if (!is.na(bvar)) {
      tfactor<-convert2label(bv,bv.lv,bv.lb)
      for (i in (1:nx)) {
        tmp.nms<-aovMeanSd2(xv[,i],colv,bv); tmp.nms[,1]<-bv.lb[match(tmp.nms[,1],bv.lv)]; colnames(tmp.nms)[1]<-bvb
        rownames(tmp.nms)<-colv.lb[match(as.numeric(rownames(tmp.nms)),colv.lv)]
        tmp.nms<-rbind(c(" ",colnames(tmp.nms)),cbind(rownames(tmp.nms),tmp.nms))
        tmp.aov<-aov(xv[,i]~factor(colv)+factor(bv))
        oo<-aov2mat(tmp.aov,c(colvb,bvb))     
        pp<-aovHSD(tmp.aov,c(colvname,bvar))
        w<-c(w,"</br>N、均数、标准差: ",xb[i],"</br><table border=3>",mat2htmltable(tmp.nms),"</table></br>") 
        w<-c(w,"</br>方差分析（无交互作用模型）: ",xb[i],"</br><table border=3>",mat2htmltable(oo),"</table></br>") 
        w<-c(w,"</br>方差分析后多组间两两比较: ",xb[i],"</br><table border=3>",mat2htmltable(pp),"</table></br>") 
        
        tmp.aov2<-aov(xv[,i]~factor(colv)*factor(bv))
        oo2<-aov2mat(tmp.aov2,c(colvb,bvb,paste(colvb,"*",bvb)))     
        pp2<-aovHSD(tmp.aov2,c(colvname,bvar))
        
        w<-c(w,"</br>方差分析（有交互作用模型）: ",xb[i],"</br><table border=3>",mat2htmltable(oo2),"</table></br>") 
        w<-c(w,"</br>方差分析后多组间两两比较: ",xb[i],"</br><table border=3>",mat2htmltable(pp2),"</table></br>") 
        
        tmp.m<-NA; tmp.names<-NA
        for (j in (1:nbg)) {
          tmp.m<-cbind(tmp.m,tapply(xv[bv==bv.lv[j],i],factor(colv[bv==bv.lv[j]]),function(x) mean(x,na.rm=TRUE)))
          tmp.names<-c(tmp.names,paste(colv.lb,bv.lb[j],sep="\n"))
        }
        tmp.m<-tmp.m[,-1]; tmp.names<-tmp.names[-1]
        tmp.col<-rainbow(length(colv.lv))
        tmp.mtxt<-paste("按",colvb,"和",bvb,"分层的",xvname[i],"的箱图")
        
        png(paste(ofname,xvar[i],"boxplot.png",sep="_"),width=720,height=560)
        boxplot(xv[,i]~factor(colv)*factor(bv),names=tmp.names,main=tmp.mtxt,ylab=xb[i],col=tmp.col)
        dev.off()
        
        tmp.mtxt<-paste("按",colvb,"和",bvb,"分层的",xvname[i],"的均数")
        tmp.maxy<-max(tmp.m,na.rm=TRUE);tmp.maxx<-(length(colv.lv)+1)*nbg+1
        png(paste(ofname,xvar[i],"barplot.png",sep="_"),width=720,height=560)
        barplot(tmp.m,beside=TRUE,space=c(0,1),names=bv.lb,legend=colv.lb, ylim=c(0,tmp.maxy), xlim=c(0,tmp.maxx), main=tmp.mtxt, ylab=xb[i], col=tmp.col,args.legend=list(title=colvb))
        dev.off()
        
        png(paste(ofname,xvar[i],"means.png",sep="_"),width=720,height=560)
        cmp<-!is.na(xv[,i]) & !is.na(bv) & !is.na(colv)
        tmp.xfactor<-factor(colv[cmp])
        tmp.tfactor<-tfactor[cmp]
        interaction.plot(tmp.xfactor,tmp.tfactor,xv[cmp,i], trace.label=bvb, xlab=colvb, ylab=xb[i], main=tmp.mtxt, type="b",pch=c(19:25)[-2])
        dev.off()
      }
    } else {
      for (i in (1:nx)) {
        tmp.nms<-aovMeanSd(xv[,i],colv); tmp.nms<-rbind(c(colvb,colnames(tmp.nms)),cbind(colv.lb,tmp.nms))
        tmp.aov<-aov(xv[,i]~factor(colv))
        tmp.m<-tapply(xv[,i],factor(colv),function(x) mean(x,na.rm=TRUE))
        oo<-aov2mat(tmp.aov,colvb)     
        pp<-aovHSD(tmp.aov,colvname)
        
        w<-c(w,"</br>N、均数、标准差: ",xb[i],"</br><table border=3>",mat2htmltable(tmp.nms),"</table></br>") 
        w<-c(w,"</br>方差分析: ",xb[i],"</br><table border=3>",mat2htmltable(oo),"</table></br>") 
        w<-c(w,"</br>方差分析后多组间两两比较: ",xb[i],"</br><table border=3>",mat2htmltable(pp),"</table></br>") 
        tmp.col<-rainbow(length(colv.lv))
        tmp.mtxt<-paste("按",colvb,"分层的",xvname[i],"的箱图")
        png(paste(ofname,xvar[i],"boxplot.png",sep="_"),width=720,height=560)
        boxplot(xv[,i]~factor(colv),names=colv.lb,main=tmp.mtxt,ylab=xb[i],col=tmp.col)
        dev.off()
        tmp.mtxt<-paste("按",colvb,"分层的",xvname[i],"的均数")
        png(paste(ofname,xvar[i],"barplot.png",sep="_"),width=720,height=560)
        barplot(tmp.m,space=1,names=colv.lb,main=tmp.mtxt,ylab=xb[i],col=tmp.col)
        dev.off()
        png(paste(ofname,xvar[i],"means.png",sep="_"),width=720,height=560)
        try(plotmeans(xv[,i]~factor(colv),legends=colv.lb,main=tmp.mtxt,xlab=colvb,ylab=xb[i]))
        dev.off()
      }
    }
  }
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)