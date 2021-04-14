  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0201"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  mparm <- as.character(pa[4,1])
  
  ixn <- as.character(xparm1)
  ixv <- as.character(xparm2)
  ixs <- as.numeric(xparm3)
  imean <- as.numeric(mparm)
  
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
  
  vname<-c("_N_","_STAT_","_TOTAL_",ixn)
  vlabel<-c("样本量(%)","统计量","合计",ixv)
  
  library(nortest,lib.loc=R.LibLocation)

  ofname<-"2_1";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"样本均数与总体均数比较的T检验"; 
  attach(WD) 
  subjvname<-NA; 

  xv<-as.matrix(idata[,c(toupper(ixn))]);
  xv<-apply(xv,2,as.numeric)
  xvname<-c(toupper(ixn)); 
  xvar<-c(toupper(ixn)); 
  xlv<-c(ixs); 
  sxf<-imean;

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
  mu<-sxf; mu[is.na(mu)]<-0
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  sink(paste(ofname,".lst",sep=""))
  
  
  w<-c(w,paste("<h2>样本均数与总体均数比较的 t 检验</h2>",sep=""))
  for (i in (1:ncol(xv))) {
    tmp.vb<-xb[i]
    tt<-t.test(xv[,i],mu=mu[i],alternative="t")
    tg<-t.test(xv[,i],mu=mu[i],alternative="g")
    tl<-t.test(xv[,i],mu=mu[i],alternative="l")
    print(tt);print(tg); print(tl)
    oo<-cbind(tmp.vb,paste(c("双侧：样本均数<>","单侧：样本均数 >","单侧：样本均数 <"),mu[i]),numfmt(tt$estimate,dec))
    oo<-cbind(oo,numfmt(c(tt$conf.int[1],tg$conf.int[1],tl$conf.int[1]),dec))
    oo<-cbind(oo,numfmt(c(tt$conf.int[2],tg$conf.int[2],tl$conf.int[2]),dec))
    oo<-cbind(oo,numfmt(c(tt$statistic,tg$statistic,tl$statistic),2),tt$parameter)
    oo<-cbind(oo,pvformat(c(tt$p.value,tg$p.value,tl$p.value), dec+2))
    tmp.dst<-quantile(xv[,i],probs<-seq(0,1,by=0.05),na.rm=TRUE)
    print(tmp.dst)
    if (i==1) {xx.lb<-tmp.vb; xx.oo<-oo; ss<-tmp.dst} else {xx.lb<-c(xx.lb,tmp.vb); xx.oo<-rbind(xx.oo,oo);ss<-rbind(ss,tmp.dst);}
    png(paste(ofname,"_",xvname[i],"_boxplot.png",sep=""),width=720,height=560)
    boxplot(xv[,i],ylab=tmp.vb,main=paste(tmp.vb,"分布的箱图"))
    dev.off()
  }
  xx.oo<-rbind(c("变量名","备选假设","样本均数","95%可信区间下限","95%可信区间上限","t 值","自由度","p 值"),xx.oo)
  w<-c(w,"</br><table border=3>",mat2htmltable(xx.oo),"</table></br>")
  ss<-rbind(paste(seq(0,100,by=5),"%",sep=""),numfmt(ss,dec))
  ss<-cbind(c(" ",xx.lb),ss)
  w<-c(w,"变量分布：百分位数")
  w<-c(w,"</br><table border=3>",mat2htmltable(t(ss)),"</table></br>")
  w<-c(w,"</body></html>")
  sink()
  fileConn<-file(paste(ofname,".htm",sep=""))
  writeLines(w, fileConn)