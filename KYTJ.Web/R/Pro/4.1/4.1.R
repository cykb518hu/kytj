  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0401"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  cparm1 <- as.character(pa[4,1])
  cparm2 <- as.character(pa[5,1])
  cparm3 <- as.character(pa[6,1])

  ixn <- c(xparm1)
  ixv <- c(xparm2)
  ixs <- as.numeric(xparm3)
  ian <- c(cparm1)
  iav <- c(cparm2)
  ias <- as.numeric(cparm3)
  
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

  vname<-c("N","STAT","TOTAL",ixn,ian)
  vlabel<-c("样本(%)","统计量","合计",ixv,iav)

  library(car,lib.loc=R.LibLocation)

  ofname<-"4_1";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"一般线性相关系数"
  attach(WD) 
  subjvname<-NA;
  xv<-as.matrix(idata[,c(toupper(ixn))]); 
  xvname<-c(toupper(ixn)); 
  xvar<-c(toupper(ixn)); 
  xlv<-c(NA,ixs)[-1]; 
  svname<-NA; sv<-NA; slv<-NA;

  av<-as.matrix(idata[,c(toupper(ian))]); 
  avname<-c(toupper(ian)); 
  if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]; 
  nadj<-length(avname);alv<-as.numeric(c(NA,ias)[-1])
  saf<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-1;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## car ##R package##;
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
    t3<- paste(t2,collapse="");
    return(t2)
  }
  xxcor2vec<-function(x,y,xname,yname) {
    xlb<-vlabelV[match(xname,vnameV)]; if (is.na(xlb)) xlb<-xname;    
    ylb<-vlabelV[match(yname,vnameV)]; if (is.na(ylb)) ylb<-yname;
    if (xname!=xlb) {xlb1<-paste(xname,xlb,sep=": ");} else {xlb1<-xname;}
    if (yname!=ylb) {ylb1<-paste(yname,ylb,sep=": ");} else {ylb1<-yname;}
    tmp.col<-c("black",rainbow(2))
    if (sum(!is.na(x) & !is.na(y))>1) {
  	tc<-cor.test(x,y,method=meth,use="pairwise.complete.obs")
  	if (par1==1) {
  	  rr<-numfmt(c(tc$estimate,tc$conf.int[1],tc$conf.int[2],tc$p.value,tc$statistic,tc$parameter),4)
  	} else {rr<-numfmt(c(tc$estimate,tc$p.value,tc$statistic),4)}
  	rr<-c(xlb1, ylb1, rr, meth)
  	png(paste(ofname,"_",xname,"_",yname,".png",sep=""),width=720,height=560)
  	TMPWD<-data.frame(y,x,bv)
  	if (!is.na(bvar)) {
  	  tmp.col<-c("black",rainbow(length(blv)))
  	  scatterplot(y~x|bv,data=TMPWD,col=tmp.col,boxplots="",smooth=F,grid=F,ylab=ylb,xlab=xlb)
  	  leg.xpos<-ifelse(lm(y~x)$coefficients[2]>0,"topleft","topright")
  	  legend(leg.xpos,bglbl,col=tmp.col,pch=(1:length(blv)),title=bvlbl)
  	} else {
  	  scatterplot(y~x,data=TMPWD,col=tmp.col,boxplots="",smooth=F,grid=F,ylab=ylb,xlab=xlb)
  	}
  	rm(TMPWD)
  	dev.off()
    } else {rr<-c(xlb1, ylb1,rep(NA,ncoo),meth);}
    if (!is.na(bvar)) {
  	rr<-c("Total",rr)
  	for (i in (1:length(blv))) {
  	  xi<-x[bv==blv[i]]; yi<-y[bv==blv[i]]; 
  	  if (sum(!is.na(xi) & !is.na(yi))>1) {
  		tc<-cor.test(xi,yi,method=meth,use="pairwise.complete.obs")
  		if (par1==1) {
  		  ooi<-numfmt(c(tc$estimate,tc$conf.int[1],tc$conf.int[2],tc$p.value,tc$statistic,tc$parameter),4)
  		} else {ooi<-numfmt(c(tc$estimate,tc$p.value,tc$statistic),4)}
  		rr<-rbind(rr,c(bglbl[i],xlb1, ylb1, ooi, meth))
  	  } else {rr<-rbind(rr,c(bglbl[i],xlb1, ylb1, rep(NA,ncoo),meth));}
  	}
    }
    return(rr)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-length(xvname);xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  if (is.na(par1)) par1<-1
  meth<-c("pearson","spearman","kendall")[par1];
  if (!is.na(bvar)) blv<-levels(factor(bv));
  if (is.na(bvar)) {bvlbl<-NA; bglbl<-NA; 
  } else {
    bvlbl<-vlabelV[match(bvar,vnameV)]; if (is.na(bvlbl)) bvlbl<-bvar; 
    bgp<-levels(factor(bv));
    bglbl<-vlabelZ[match(paste(bvar,bgp,sep="."),vnameZ)];
    bglbl[is.na(bglbl)]<-bgp[is.na(bglbl)]
  }
  if (is.na(parm[1])) {parm1<-0;} else {parm1<-parm[1];}
  if (par1==1) {
    ncoo=6; oo<-c("Var 1","Var 2","Correlation","95%CI low","95%CI upp","P.value","t","df","Method")
  } else {
    ncoo=3; 
    if (par1==2) oo<-c("Var 1","Var 2","Correlation","P.value","rho","Method")
    if (par1==3) oo<-c("Var 1","Var 2","Correlation","P.value","tau","Method")
  }
  if (!is.na(bvar)) oo<-c(bvlbl,oo)
  if (parm1==1 & nx==nadj) {
    for (i in (1:nx)) {oo<-rbind(oo,xxcor2vec(xv[,i],av[,i],xvname[i],avname[i]));}
    tmp.cc<-numfmt(cor(xv,av,method=meth,use="pairwise.complete.obs"),4)
    cc<-cbind(c(" ",xvname),rbind(avname,tmp.cc))
    if (!is.na(bvar)) {
  	for (g in (1:length(blv))) {
  	  tmp.cg<-numfmt(cor(xv[bv==blv[g],],av[bv==blv[g],],method=meth,use="pairwise.complete.obs"),4)
  	  cg<-cbind(c(paste(bvar,blv[g],sep="="),xvname),rbind(avname,tmp.cg))
  	  cc<-rbind(cc,cg)
  	}
    }   
  } else {
    for (i in (1:nx)) {
  	if (nadj>0) {
  	  for (j in (1:nadj)) {oo<-rbind(oo,xxcor2vec(xv[,i],av[,j],xvname[i],avname[j]));}  
  	  tmp.cc<-numfmt(cor(xv,av,method=meth,use="pairwise.complete.obs"),4)
  	  cc<-cbind(c(" ",xvname),rbind(avname,tmp.cc))
  	  if (!is.na(bvar)) {
  		for (g in (1:length(blv))) {
  		  tmp.cg<-numfmt(cor(xv[bv==blv[g],],av[bv==blv[g],],method=meth,use="pairwise.complete.obs"),4)
  		  cg<-cbind(c(paste(bvar,blv[g],sep="="),xvname),rbind(avname,tmp.cg))
  		  cc<-rbind(cc,cg)
  		}
  	  }   
  	} else {
  	  for (j in ((i+1):nx)) {
  		if (i<nx) oo<-rbind(oo,xxcor2vec(xv[,i],xv[,j],xvname[i],xvname[j]));
  	  }  
  	  tmp.cc<-numfmt(cor(xv,method=meth,use="pairwise.complete.obs"),4)
  	  cc<-cbind(c(" ",xvname),rbind(xvname,tmp.cc))
  	  if (!is.na(bvar)) {
  		for (g in (1:length(blv))) {
  		  tmp.cg<-numfmt(cor(xv[bv==blv[g],],method=meth,use="pairwise.complete.obs"),4)
  		  cg<-cbind(c(paste(bvar,blv[g],sep="="),xvname),rbind(xvname,tmp.cg))
  		  cc<-rbind(cc,cg)
  		}
  	  }   
  	  png(paste(ofname,"_pairs.png",sep=""),width=720,height=560);pairs(xv);dev.off()
  	  if (!is.na(bvar)) {
  		for (g in (1:length(blv))) {
  		  png(paste(ofname,"_",bvar,blv[g],"_pairs.png",sep=""),width=720,height=560);       
  		  pairs(xv[bv==blv[g],]);
  		  dev.off()
  		}
  	  }
  	}
    }
  }
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")


  w<-c(w,"<h2>一般相关系数</h2>")
  w<-c(w,"相关性检验</br><table border=3>",mat2htmltable(oo),"</table></br>")
  w<-c(w,"相关系数</br><table border=3>",mat2htmltable(cc),"</table></br>")
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep=""));writeLines(w, fileConn)