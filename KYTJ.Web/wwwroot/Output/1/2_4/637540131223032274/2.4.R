  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "C:\\Users\\ThinkPad\\Desktop\\20200520_方法0204补丁"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  method1 <- as.character(pa[4,1])
  method2 <- as.character(pa[5,1])
  bparm1 <- as.character(pa[6,1])
  bparm2 <- as.character(pa[7,1])
  bparm3 <- as.character(pa[8,1])

  ixn <- unlist(strsplit(xparm1,"[,]")) 
  ixv <- unlist(strsplit(xparm2,"[,]")) 
  ixs <- unlist(strsplit(xparm3,"[,]")) 
  m1 <- as.numeric(method1)
  m2 <- as.numeric(method2)
  ibs <- as.numeric(bparm1) 
  ibn <- unlist(strsplit(bparm2,"[|]"))
  ibv <- unlist(strsplit(bparm3,"[|]"))
  
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
  
  if(ibs==0)  {
    vname<-c("_N_","_STAT_","_TOTAL_",ixn)
    vlabel<-c("样本量(%)","统计量","合计",ixv)
  }else {
    vname<-c("_N_","_STAT_","_TOTAL_",ixn,ibn)
    vlabel<-c("样本量(%)","统计量","合计",ixv,ibv)
  }
  
  library(mvtnorm,lib.loc=R.LibLocation)
  library(VGAM,lib.loc=R.LibLocation)
  library(splines,lib.loc=R.LibLocation)
  library(stats4,lib.loc=R.LibLocation)
  library(lawstat,lib.loc=R.LibLocation)
  
  ofname<-"2_4";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"多样本方差齐性检验"
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
  if(ibs==0)  {
    bv<-NA; bvar<-NA;  
  }else {
    bv<-idata[,ibn[1]];bvar<-c(ibn[1]);bvname<-c(ibn[1]); 
  }
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-m1;dec<-4;parm<-c(NA, m2,NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## mvtnorm VGAM splines stats4 lawstat ##R package##;
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
  par2<-parm[2];
  if (is.na(par1) | par1==0) par1<-1;
  if (is.na(par2) | par2==0) par2<-1;
  p.loc<-c("median","mean","trim.mean")[par1];
  p.cor<-c("none","zero.removal","zero.correction")[par2];
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td style=\"border:1px dotted;\">"))
    t2<- paste("<tr><td style=\"border:1px dotted;\">",t1,"</td></tr>")
    t3<- paste(t2,collapse="");
    return(t2)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  sink(paste(ofname,".lst",sep=""))
  xx <- WD[,xvname]; if (is.vector(xx)) xx<-matrix(xx,ncol=1)
  tt1<-c("var","statistic","p.value"); tt2<-tt1
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
    vv<-numfmt(apply(xx,2,function(z) tapply(z,factor(WD[,bvar]),function(x) var(x,na.rm=TRUE))),dec)
    nn<-apply(xx,2,function(z) tapply(z,factor(WD[,bvar]),function(x) sum(!is.na(x))))
    mm<-numfmt(apply(xx,2,function(z) tapply(z,factor(WD[,bvar]),function(x) mean(x,na.rm=TRUE))),dec)
    tb<-matrix(paste(nn," (",mm,") (",vv,")",sep=""),nrow=nbg)
    tb<-cbind(c(bvb,bv.lb),rbind(xb,tb))
    for (i in (1:length(xvname))) {
      cmp<-!is.na(xx[,i]) & !is.na(bv); x<-xx[cmp,i]; g<-bv[cmp];
      t1<-levene.test(x,g,location=p.loc,correction.method=p.cor,trim.alpha=0.25)
      tt1<-rbind(tt1,c(xvname[i],numfmt(t1$statistic,dec),pvformat(t1$p.value,dec+2)))
      mth1<-t1["method"][[1]]
      print(t1)
      if (nbg>2) {
        t2<-ltrend.test(x,g,location=p.loc,correction.method=p.cor,trim.alpha=0.25,tail="both")
        tt2<-rbind(tt2,c(xvname[i],numfmt(t2$statistic,dec),pvformat(t2$p.value,dec+2)))
        mth2<-t2["method"][[1]]
        print(t2)
      } else {tt2 <- NULL}
    }
  } else if (nx>1) {
    bvb <- NA
    vv<-numfmt(apply(xx,2,function(x) var(x,na.rm=TRUE)),dec)
    nn<-apply(xx,2,function(x) sum(!is.na(x)))
    mm<-numfmt(apply(xx,2,function(x) mean(x,na.rm=TRUE)),dec)  
    tb<-cbind(xb,paste(nn," (",mm,") (",vv,")",sep=""))
    xx <- xx[apply(is.na(xx),1,sum)==0,]
    x<- as.vector(as.matrix(xx))
    g<- as.vector(matrix(rep(1:nx,nrow(xx)),nrow=nrow(xx),byrow=TRUE))
    t1<-levene.test(x,g,location=p.loc,correction.method=p.cor,trim.alpha=0.25)
    tt1<-rbind(tt1[-1],c(numfmt(t1$statistic,dec),pvformat(t1$p.value,dec+2)))
    mth1<-t1["method"][[1]]
    print(t1)
    if (nx>2) {
      t2<-ltrend.test(x,g,location=p.loc,correction.method=p.cor,trim.alpha=0.25,tail="both")
      tt2<-rbind(tt2[-1],c(numfmt(t2$statistic,dec),pvformat(t2$p.value,dec+2))) 
      mth2<-t2["method"][[1]]  
      print(t2)  
    } else {tt2 <- NULL}
  } else {
    tt1 <- tt2 <- NULL
  }
  sink()
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  
  
  w<-c(w,"<h2>多样本方差齐性检验</h2>")
  w<-c(w,"</br>N (均数) 方差</br><table border=3>",mat2htmltable(tb),"</table></br>")
  if (!is.null(tt1)) {
    w<-c(w,"方差齐性检验:</br><table border=3>",mat2htmltable(tt1),"</table></br>")
    w<-c(w,mth1,"</br></br>")
  }
  if (!is.null(tt2)) {
    w<-c(w,"方差趋势检验:</br><table border=3>",mat2htmltable(tt2),"</table></br>")
    w<-c(w,mth2,"</br></br>")
  }
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep=""))
  writeLines(w, fileConn)