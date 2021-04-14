  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "D:\\R\\R-3.4.3\\library"
  output <- args[7] # output <- "E:\\工作项目\\数据管理系统\\目前正在开发的版本\\华西sso版本\\kytj\\WebUI\\Output\\1\\3_2\\637280081122311373"
  
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  yparm1 <- as.character(pa[1,1])
  yparm2 <- as.character(pa[2,1])
  yparm3 <- as.character(pa[3,1])
  xparm1 <- as.character(pa[4,1])
  xparm2 <- as.character(pa[5,1])
  xparm3 <- as.character(pa[6,1])  
  method1 <- as.character(pa[7,1])

  iyn <- as.character(yparm1)
  iyv <- as.character(yparm2)
  iys <-as.numeric((yparm3))
  ixn <- as.character(xparm1)
  ixv <- as.character(xparm2)
  ixs <-as.numeric((xparm3))
  m1 <- as.numeric(method1)
  
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

  vname<-c("N","STAT","TOTAL",iyn,ixn)
  vlabel<-c("样本(%)","统计量","合计",iyv,ixv)

  library(gdata,lib.loc=R.LibLocation)
  library(gmodels,lib.loc=R.LibLocation)
  library(exact2x2,lib.loc=R.LibLocation)
  library(vcd,lib.loc=R.LibLocation)

  ofname<-"3_2"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"卡方检验"; 
  attach(WD) 
  subjvname<-NA; 
  
  xv<-as.matrix(idata[,iyn]); 
  xvname<-c(iyn); 
  xvar<-c(iyn); 
  xlv<-c(NA,iys)[-1]; 
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA;

  av<-as.matrix(idata[,ixn]); 
  avname<-c(ixn); 
  if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]; 
  nadj<-length(avname);alv<-c(NA,ixs)[-1]; 
  saf<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-m1;dec<-4;parm<-c(NA, NA, NA, NA); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  
  ##R package## gdata gmodels exact2x2 vcd ##R package##;
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
  cb2list<-function(cb) {
    fs<-NA;
    if (!is.null(cb$fisher.ts)) {
  	fcont<-0
  	f1<-c(pvformat(cb$fisher.ts$p.value,4),cb$fisher.ts$alternative)
  	ft<-c("P-value","Method")
  	if (!is.null(cb$fisher.ts$estimate)) {
  	  fcont<-1
  	  f1<-c(numfmt(c(cb$fisher.ts$estimate,cb$fisher.ts$conf.int[1],cb$fisher.ts$conf.int[2]),4),f1)
  	  ft<-c("Odds ratio","95%CI low","95%CI high",ft)
  	}
  	fs<-rbind(ft,f1)
  	if (!is.null(cb$fisher.tl)) {
  	  f1<-c(pvformat(cb$fisher.tl$p.value,4),cb$fisher.tl$alternative)
  	  if (fcont==1) f1<-c(numfmt(c(cb$fisher.tl$estimate,cb$fisher.tl$conf.int[1],cb$fisher.tl$conf.int[2]),4),f1)
  	  fs<-rbind(fs,f1)      
  	}
  	if (!is.null(cb$fisher.gt)) {
  	  f1<-c(pvformat(cb$fisher.gt$p.value,4),cb$fisher.gt$alternative)
  	  if (fcont==1) f1<-c(numfmt(c(cb$fisher.gt$estimate,cb$fisher.gt$conf.int[1],cb$fisher.gt$conf.int[2]),4),f1)
  	  fs<-rbind(fs,f1)      
  	}
    }
    xq<-NA;
    if (!is.null(cb$chisq)) {
  	f1<-c(numfmt(cb$chisq$statistic,4),cb$chisq$parameter, pvformat(cb$chisq$p.value,4),cb$chisq$method)
  	xq<-rbind(c("Chi-square","df","P.value","Method"),f1)
  	if (!is.null(cb$chisq.corr)) {
  	  f1<-c(numfmt(cb$chisq.corr$statistic,4),cb$chisq.corr$parameter, pvformat(cb$chisq.corr$p.value,4),cb$chisq.corr$method)
  	  xq<-rbind(xq,f1)
  	}
    }
    tb<-NA; pp<-NA
    if (is.matrix(cb$t)) {
  	n<-cbind(cb$t,apply(cb$t,1,sum)); 
  	pp<-t(cb$prop.row)
  	p<-cbind(numfmt(cb$prop.row*100,2),100)
  	tb<-matrix(paste(n," (",p,"%)",sep=""), nrow=nrow(n))
    }
    return(list(tb,pp,xq,fs))
  }
  matchx2<-function(a,b) {
    mt<-NA;
    if (length(levels(factor(a))) == length(levels(factor(b)))) {
  	if (sum(!(levels(factor(a))==levels(factor(b))))==0) {
  	  try(m1<-mcnemar.test(a,b,correct=FALSE))
  	  try(m2<-mcnemar.test(a,b,correct=TRUE))
  	  if (!is.null(m1)) {
  		t1<-c(numfmt(m1$statistic,4),m1$parameter,pvformat(m1$p.value,4),"No")
  		t2<-c(numfmt(m2$statistic,4),m2$parameter,pvformat(m2$p.value,4),"Yes")
  		mt<- rbind(c("McNemar's chi-squared","df","p.value","Continuity correction"),t1,t2)
  	  }
  	}
    }
    return (mt);
  }
  kappax2<-function(a,b) {
    mt<-NA;
    if (length(levels(factor(a))) == length(levels(factor(b)))) {
  	if (sum(!(levels(factor(a))==levels(factor(b))))==0) {
  	  tmp.cb<-CrossTable(a,b,chisq=T,fisher=F)
  	  dd<-matrix(as.integer(tmp.cb$t),nrow=nrow(tmp.cb$t))
  	  ka1<-Kappa(dd, weights = "Equal-Spacing")
  	  ka2<-Kappa(dd, weights = "Fleiss-Cohen")
  	  k.pv<-pnorm(-abs(ka1$Unweight[1]/ka1$Unweight[2]))*2;
  	  k.pv<-c(k.pv,pnorm(-abs(ka1$Weighted[1]/ka1$Weighted[2]))*2);
  	  k.pv<-c(k.pv,pnorm(-abs(ka2$Weighted[1]/ka2$Weighted[2]))*2);
  	  k.ci<-rbind(confint(ka1),confint(ka2)[-1,]);
  	  mt<-format(round(cbind(c(ka1$Unweight[1],ka1$Weighted[1],ka2$Weighted[1]),k.ci,k.pv),4),nsmall=4)
  	  mt<-rbind(c(" ","Kappa","95%CI low","95%CI upp","p-value"),cbind(c("Unweighted","Equal-Spacing weighted","Fleiss-Cohen weighted"),mt)) 
  	  rownames(dd)<-x.lb
  	  colnames(dd)<-y.lb
  	  png(paste(ofname,"_agreement",ntest,".png",sep=""));
  	  agreementplot(dd,main="Agreement plot",xlab=yb,ylab=xb)
  	  dev.off()
  	}
    }
    return (mt);
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  parm1<-0; if (!is.na(parm[1])) parm1<-parm[1]
  if (is.na(par1)) par1<-1
  nbg<-1; 
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  ny<-ncol(xv); nx<-ncol(av);
  oot<-NA;
  w<-c("</br>")
  ntest<-0;



  for (k in (1:ny)) {
    if (parm1==1) {bgn<-k; edn=k;} else {bgn<-1; edn<-nx;}
    for (z in (bgn:edn)) {
  	yname<-xvname[k]; xname<-avname[z];y<-WD[,yname]; x<-WD[,xname]
  	xb<-vlabelV[match(xname,vnameV)]; if (is.na(xb)) xb<-xname
  	x.lv<-levels(factor(x)); 
  	x.lb<-trim(vlabelZ[match(paste(xname,x.lv,sep="."),vnameZ)])
  	x.lb[is.na(x.lb)]<-xname[is.na(x.lb)]
  	yb<-vlabelV[match(yname,vnameV)]; if (is.na(yb)) yb<-yname
  	y.lv<-levels(factor(y)); 
  	y.lb<-trim(vlabelZ[match(paste(yname,y.lv,sep="."),vnameZ)])
  	y.lb[is.na(y.lb)]<-yname[is.na(y.lb)]
  	nxlv<-length(x.lv); fs<-NA; tmp.pp<-NA;
  	oo<-NA;xq<-NA;opp<-NA; oxq<-NA; ofs<-NA; tname<-NA; omt<-NA; oka<-NA;
  	ntest<-ntest+1
  	
  	if (!is.na(bvar)) {
  	  for (i in (1:nbg)) {
  		tmp.cb<-NULL
  		tmp.try<-try(tmp.cb<-CrossTable(x[WD[,bvar]==bv.lv[i]],y[WD[,bvar]==bv.lv[i]],chisq=T,fisher=T))
  		if (substr(tmp.try[1],1,3)=="Err") tmp.cb<-CrossTable(x[WD[,bvar]==bv.lv[i]],y[WD[,bvar]==bv.lv[i]],chisq=T,fisher=F)
  		if (!is.null(tmp.cb)) {
  		  ooi<-cb2list(tmp.cb)
  		  oo1<-cbind(c(xb,x.lb),rbind(c(paste(yb,y.lb,sep=": "),"合计"),ooi[[1]]))
  		  oo1<-cbind(c(bvb,rep(bv.lb[i],(nrow(oo1)-1))),oo1)
  		  if (!is.na(oo[1])) {oo<-rbind(oo,oo1[-1,]);} else {oo<-oo1;}
  		  if (!is.na(opp[1])) {
  			opp<-cbind(opp,ooi[[2]]); tname<-c(tname,paste(x.lb,bv.lb[i],sep="\n"))
  		  } else { 
  			opp<-ooi[[2]]; tname<-paste(x.lb,bv.lb[i],sep="\n");
  		  }
  		  if (!is.na(ooi[[3]][1])) {
  			xq<-cbind(c(bvb,rep(bv.lb[i],(nrow(ooi[[3]])-1))),ooi[[3]])
  			if (is.na(oxq[1])) {oxq<-xq;} else {oxq<-rbind(oxq,xq[-1,]);}
  		  }
  		  if (!is.na(ooi[[4]][1])) {
  			fs<-cbind(c(bvb,rep(bv.lb[i],(nrow(ooi[[4]])-1))),ooi[[4]])
  			if (is.na(ofs[1])) {ofs<-fs;} else {ofs<-rbind(ofs,fs[-1,]);}
  		  }
  		  if (par1==2) {
  			omti<-matchx2(x[WD[,bvar]==bv.lv[i]],y[WD[,bvar]==bv.lv[i]]);
  			if (!is.na(omti[1])) {
  			  omti<-cbind(c(bvb,rep(bv.lb[i],(nrow(omti)-1))),omti)
  			  if (is.na(omt)) {omt<-omti;} else {omt<-rbind(omt,omti[-1,]);}
  			}
  		  }
  		  if (par1==3) {
  			okai<-kappax2(x[WD[,bvar]==bv.lv[i]],y[WD[,bvar]==bv.lv[i]]);
  			if (!is.na(okai[1])) {
  			  okai<-cbind(c(bvb,rep(bv.lb[i],(nrow(okai)-1))),okai)
  			  if (is.na(oka)) {oka<-okai;} else {oka<-rbind(oka,okai[-1,]);}
  			}
  		  }
  		}
  	  }
  	} else {
  	  tmp.cb<-NULL
  	  tmp.try<-try(tmp.cb<-CrossTable(x,y,chisq=T,fisher=T))
  	  if (substr(tmp.try[1],1,3)=="Err") tmp.cb<-CrossTable(x,y,chisq=T,fisher=F)
  	  if (!is.null(tmp.cb)) {
  		ooi<-cb2list(tmp.cb)
  		oo<-cbind(c(xb,x.lb),rbind(c(paste(yb,y.lb,sep=": "),"合计"),ooi[[1]]))
  		opp<-ooi[[2]]; tname<-x.lb
  		if (!is.na(ooi[[3]][1])) oxq<-ooi[[3]];
  		if (!is.na(ooi[[4]][1])) ofs<-ooi[[4]];
  		if (par1==2) omt<-matchx2(x,y);
  		if (par1==3) oka<-kappax2(x,y);
  	  }
  	}
  	
  	if (!is.na(opp[1])) {
  	  png(paste(ofname,"_barplot",ntest,".png",sep=""),width=720,height=560)
  	  nb<-nxlv*nbg*2+1
  	  tmp.col<-rainbow(length(y.lv))
  	  barplot(opp,beside=FALSE,space=1,names=tname,legend=y.lb,main=yb,ylim=c(0,1), xlim=c(0,nb), ylab="%", col=tmp.col, args.legend=list(title=yb))
  	  dev.off()
  	}
  	
  	w<-c(w,paste(paste("</br><a name=\"",ntest,"\">",sep=""),ntest,":</a> ",xb,"*",yb,"</br>"))
  	if (!is.na(oo[1])) w<-c(w,"</br>N (行百分数)<table border=3>",mat2htmltable(oo),"</table></br>")
  	if (!is.na(oxq[1])) w<-c(w,"</br>卡方检验<table border=3>",mat2htmltable(oxq),"</table></br>")
  	if (!is.na(omt[1])) w<-c(w,"</br>配对资料的卡方检验<table border=3>",mat2htmltable(omt),"</table></br>")
  	if (!is.na(ofs[1])) w<-c(w,"</br>Fisher's 精确检验<table border=3>",mat2htmltable(ofs),"</table></br>")
  	if (!is.na(oka[1])) w<-c(w,"</br>Kappa 一致性检验<table border=3>",mat2htmltable(oka),"</table></br>")
  	if (par1==1) ootmp<-oxq;
  	if (par1==2) ootmp<-omt;
  	if (par1==3) ootmp<-oka;
  	if (ntest==1) oot<-c("#","Row var.","Column var.",ootmp[1,]);
  	tmpi<-ootmp[-1,]
  	if (!is.matrix(tmpi)) {
  	  oot<-rbind(oot,c(paste("<a href=\"#",ntest,"\">",ntest,"</a>",sep=""),xb,yb,tmpi));
  	} else {
  	  tmp0<-matrix("",nrow=nrow(tmpi),ncol=3);
  	  tmp0[1,]<-c(paste("<a href=\"#",ntest,"\">",ntest,"</a>",sep=""),xb,yb)
  	  oot<-rbind(oot,cbind(tmp0,tmpi))
  	}
    }  
  }

  w0<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w0<-c(w0,"<h2>卡方检验</h2>")
  if (ntest > 1) {
    if (par1==1) w0<-c(w0,"Summry (Pearson Chi-squared test )");
    if (par1==2) w0<-c(w0,"Summry (McNemar Chi-squared test)");
    if (par1==3) w0<-c(w0,"Summry (Kappa agreement test)");
    w<-c(w0,"</br><table border=3>",mat2htmltable(oot),"</table></br></br>",w)
  } else {
    w<-c(w0,w)
  }
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)