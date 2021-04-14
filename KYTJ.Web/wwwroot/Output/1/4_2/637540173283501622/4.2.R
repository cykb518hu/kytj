  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0402"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  xparm4 <- as.character(pa[4,1])

  ixn <- unlist(strsplit(xparm1,"[,]"))  
  ixv <- unlist(strsplit(xparm2,"[,]"))
  ixs <- unlist(strsplit(xparm3,"[,]"))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  
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

  library(mvtnorm,lib.loc=R.LibLocation)
  library(sfsmisc,lib.loc=R.LibLocation)
  library(polycor,lib.loc=R.LibLocation)

  ofname<-"4_2"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"序变量相关"; 
  attach(WD) 
  subjvname<-NA;

  library(stringr)
  sq <- str_count(xparm3,",")+1
  xv<-cbind(idata[,1])
  for (s1 in (1:sq)) {
    v1<-ixc[s1]
    xv<-cbind(xv,idata[,v1])
    s1=s1+1
  }
  colnames(xv)<-c(1,ixc)
  xv<-as.data.frame(xv[,-1])
  xvname<-c(ixc); 
  xvar<-c(ixc); 
  xlv<-c(NA,ixs)[-1];  
  
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA;
  av<-NA; avname<-NA;avlbl<-NA;nadj<-0; alv<-NA;
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## mvtnorm sfsmisc polycor ##R package##;
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    t3<- paste(t2,collapse="");
    return(t2)
  }
  xxcor2vec<-function(x,y,xname,yname) {
    xlb<-vlabel[vname==xname];   ylb<-vlabel[vname==yname]
    if (xname!=xlb) {xlb1<-paste(xname,xlb,sep=": ");} else {xlb1<-xname;}
    if (yname!=ylb) {ylb1<-paste(yname,ylb,sep=": ");} else {ylb1<-yname;}
    if (sum(!is.na(x) & !is.na(y))>1) {
  	tc<-try(polychor(x,y,ML = FALSE, std.err = TRUE, maxcor=.9999))
  	if (substr(tc[1],1,5)=="Error") {
  	  r.se<-NA; rho<-NA; rr<-c(NA,NA,NA,NA,NA,NA,NA);
  	} else {
  	  r.se<-sqrt(tc$var[[1]]);rho<-tc$rho
  	  rr<-round(c(rho, r.se, rho-1.96*r.se, rho+1.96*r.se, tc$chisq, tc$df, 1-pchisq(tc$chisq,tc$df)),4)
  	}
  	rr<-c(xlb1, ylb1, rr)
    } else {rr<-c(xlb1, ylb1,rep(NA,7));}
    if (!is.na(bvar)) {
  	rr<-c("Total",rr)
  	for (i in (1:length(blv))) {
  	  xi<-x[bv==blv[i]]; yi<-y[bv==blv[i]]; 
  	  if (sum(!is.na(xi) & !is.na(yi))>1) {
  		tc<-try(polychor(xi,yi,ML = FALSE, std.err = TRUE, maxcor=.9999))
  		if (substr(tc[1],1,5)=="Error") {
  		  r.se<-NA;rho<-NA;ooi<-c(NA,NA,NA,NA,NA,NA,NA);
  		} else {
  		  r.se<-sqrt(tc$var[[1]]);rho<-tc$rho
  		  ooi<-round(c(rho, r.se, rho-1.96*r.se, rho+1.96*r.se, tc$chisq, tc$df, 1-pchisq(tc$chisq,tc$df)),4)
  		}
  		rr<-rbind(rr,c(bglbl[i],xlb1, ylb1, ooi))
  	  } else {rr<-rbind(rr,c(bglbl[i],xlb1, ylb1, rep(NA,7)));}
  	}
    }
    return(rr)
  }
  nx<-length(xvname);
  xb<-vlabel[match(xvname,vname)];
  if (is.na(parm[1])) {parm1<-0;} else {parm1<-parm[1];}
  if (!is.na(bvar)) {
    blv<-levels(factor(bv));nrr<-length(blv)+1;
    bvlbl<-vlabel[vname==bvar];bgp<-levels(factor(bv));
    bglbl<-vlabel[match(paste(bvar,bgp,sep="."),vname)];
    bglbl[is.na(bglbl)]<-bgp[is.na(bglbl)]
  } else {nrr<-1;}
  oo<-c("Var 1","Var 2","Polychoric Correlation:"," "," "," ","Test of bivariate normality"," "," ")
  oo<-rbind(oo,c(" "," ","rho","se","95%CI low","95%CI upp","Chisq","df","P.value"))
  oo2<-NA
  if (!is.na(bvar)) oo<-cbind(bvlbl,oo)
  if (parm1==1 & nx==nadj) {
    for (i in (1:nx)) {oo<-rbind(oo,xxcor2vec(xv[,i],av[,i],xvname[i],avname[i]));}
  } else {
    if (nadj==0) {oo2<-matrix(" ",nrow=nrr*nx,ncol=nx);}
    for (i in (1:nx)) {
  	if (nadj>0) {
  	  for (j in (1:nadj)) {oo<-rbind(oo,xxcor2vec(xv[,i],av[,j],xvname[i],avname[j]));}  
  	} else { 
  	  oo2[(((i-1)*nrr+1):(i*nrr)),i]<-"1"
  	  for (j in ((i+1):nx)) {
  		if (i<nx) {
  		  tmp.ooi<-xxcor2vec(xv[,i],xv[,j],xvname[i],xvname[j])
  		  oo<-rbind(oo,tmp.ooi);
  		  if (!is.na(bvar)) {oo2[(((i-1)*nrr+1):(i*nrr)),j]<-tmp.ooi[,4]} else {oo2[i,j]<-tmp.ooi[3]}
  		}
  	  }  
  	}
    }
  }
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")

  oo[1,]<-c("变量 1","变量 2","序变量相关系数:"," "," "," ","二元正态分布检验"," "," ")
  oo[2,]<-c(" "," ","相关系数","标准误","95%区间下限","95%区间上限","卡方值","自由度","P　值")
  w<-c(w,"<h2>序变量相关系数（Polychoric correlation）</h2>")
  w<-c(w,"序变量相关系数</br><table border=3>",mat2htmltable(oo),"</table></br>")
  if (!is.na(oo2[1])) {
    if (!is.na(bvar)) {
  	oo2<-cbind(c(bvlbl,rep(c("Total",bglbl),times=nx)),rbind(c(" ",xb),cbind(matrix(t(matrix(rep(xb,nrr),nrow=nx,byrow=FALSE)),ncol=1),oo2)))
    } else {
  	oo2<-cbind(c(" ",xb),rbind(xb,oo2));
    }
    w<-c(w,"序变量相关系数</br><table border=3>",mat2htmltable(oo2),"</table></br>")
  }
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)