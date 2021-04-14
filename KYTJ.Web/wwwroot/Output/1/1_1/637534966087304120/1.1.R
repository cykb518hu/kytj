  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "D:\\R\\R-3.4.3\\library"
  output <- args[7] # output <- "E:\\工作项目\\数据管理系统\\目前正在开发的版本\\华西sso版本\\kytj\\WebUI\\Output\\1\\1_1\\637248904392206445"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  
  ixn <- unlist(strsplit(xparm1,"[,]")) 
  ixv <- unlist(strsplit(xparm2,"[,]")) 
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  
  R.LibLocation <- rlib
  d <- read.csv("./data.csv",na= "NA")
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
  
  vname<-c("_N_","_STAT_","_TOTAL_",ixn)
  vlabel<-c("样本量(%)","统计量","合计",ixv)
  
  library(nortest,lib.loc=R.LibLocation)
  
  ofname<-"1_1";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"正态性检验"; 
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
  xv<-xv[,-1]
  xvname<-c(ixn); 
  xvar<-c(ixn); 
  xlv<-c(ixs);
  
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## nortest ##R package##;
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
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  nx<-length(xvname)
  sink(paste(ofname,".lst",sep=""))
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  tmpc<-c("method", "statistic", "p.value");
  
  w<-c(w,"<h2>正态性检验</h2>")
  for (i in (1:nx)) {
    xvi<-WD[,xvname[i]]
    nobs<-sum(!is.na(xvi));
    tmp.vb<-vlabel[vname==xvname[i]];
    tmp.t<-rbind(ad.test(xvi)[tmpc],cvm.test(xvi)[tmpc],lillie.test(xvi)[tmpc],pearson.test(xvi)[tmpc])
    print(tmp.t)
    if (nobs<5001 & nobs>4) tmp.t<-rbind(tmp.t,sf.test(xvi)[tmpc]);
    tmp.t<-rbind(c("方法","统计量","p 值"),cbind(tmp.t[,1],numfmt(tmp.t[,2],dec),pvformat(tmp.t[,3],dec+2)))
    w<-c(w,paste(tmp.vb,": 正态性检验</br>"),"<table border=3>",mat2htmltable(tmp.t),"</table></br>")
    tmp.h<-hist(xvi,plot=F)
    tmp.b=tmp.h$breaks;g=length(tmp.b);tmp.m=tmp.h$mids;tmp.n=tmp.h$counts;tmp.d=tmp.n/sum(tmp.n)
    tmp.ff<-cbind(low=tmp.b[1:(g-1)],upp=tmp.b[2:g],med.point=tmp.m,frequency=tmp.n,percent=numfmt(tmp.d*100,dec))
    tmp.qq<-quantile(xvi, probs=c(0,0.05,0.10,0.25,0.50,0.75,0.90,0.95,1), na.rm=TRUE)
    n<-sum(!is.na(xvi));mean<-mean(xvi,na.rm=TRUE); sd<-sd(xvi,na.rm=TRUE)
    tmp.qq<-rbind(n,numfmt(mean,dec),numfmt(sd,4),as.matrix(tmp.qq)); colnames(tmp.qq)<-xvname[i]; 
    tmp.ff<-rbind(c("分组区间下限","分组区间上限","分组区间中值","组内频数","百分数"),tmp.ff)
    print(tmp.qq)
    w<-c(w,paste(tmp.vb,": 频数分布</br>"),"<table border=3>",mat2htmltable(tmp.ff),"</table></br>")
    if (i==1) {xx.qq<-tmp.qq; xx.lb<-tmp.vb;} else {xx.qq<-cbind(xx.qq,tmp.qq); xx.lb<-c(xx.lb,tmp.vb);}
    rm(tmp.ff);rm(tmp.qq);rm(tmp.t);
    png(paste(ofname,"_",xvname[i],"_QQ.png",sep=""),width=720,height=560)
    qqnorm(xvi,ylab="样本位数（Z值）",xlab="理论位数（Z值）",main=paste(tmp.vb,"正态性QQ图"))
    qqline(xvi)
    dev.off()
    png(paste(ofname,"_",xvname[i],"_hist.png",sep=""),width=720,height=560)
    hist(xvi,plot=T,prob=T,xlab=tmp.vb,ylab="频数", main=paste(tmp.vb,"分布直方图"))
    lines(density(xvi,na.rm=TRUE))
    dev.off()
  }
  t1<-c("统计指标","N","均数","标准差","最小值","5%位数","10%位数","25%位数","50%位数","75%位数","90%位数","95%位数","最大值")
  xx.qq<-t(cbind(t1,rbind(xx.lb,xx.qq)))
  w<-c(w,"</br>基本统计</br><table border=3>",mat2htmltable(xx.qq),"</table></br>")
  w<-c(w,"</body></html>")
  sink()
  fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)