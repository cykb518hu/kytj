  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0301"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  cparm1 <- as.character(pa[1,1])
  cparm2 <- as.character(pa[2,1])
  cparm3 <- as.character(pa[3,1])
  tagparm <- as.character(pa[4,1])
  
  icn <- unlist(strsplit(cparm1,"[|]"))
  icv <- unlist(strsplit(cparm2,"[|]"))
  ics <- as.numeric(cparm3)
  tp <- tagparm
  
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
  
  vname<-c("N","STAT","TOTAL",icn)
  vlabel<-c("样本(%)","统计量","合计",icv)

  library(gdata,lib.loc=R.LibLocation)
  library(stringr,lib.loc=R.LibLocation)

  ofname<-"3_1";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"单向频数表"
  attach(WD) 
  subjvname<-NA;

  ixn<-icn[1]
  xv<-as.matrix(idata[,c(toupper(ixn))]); 
  xvname<-c(toupper(ixn)); 
  xvar<-c(toupper(ixn)); 
  xlv<-c(NA,ics)[-1]; 
  sxf<-NA;
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, tp,NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## gdata stringr ##R package##;
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
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  h0 <- NA
  if (!is.na(parm[2]) & nx==1) {
    h0 <- as.numeric(unlist(strsplit(as.character(parm[2])," ")))
    if (round(sum(h0),1)!=1) h0 <- NA
  }  
  sepChar <- " "
  if (!is.na(par1)) {
    if (par1==1) {sepChar <- ",";
    } else if (par1==2) {sepChar <- ";";
    } else if (par1==3) {sepChar <- "/";
    } else if (par1==4) {sepChar <- "\\";
    } else {sepChar <- par1; }
  }  
  minCount <- 0
  if (!is.na(parm[4])) minCount <- parm[4]
  nn.obs <- nrow(WD)
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" />")
  w<-c(w,"</head><body>")
  oo2 <- NA
  sink(paste(ofname,".lst",sep=""))



  if (parm[5]==1) {
    WD.NEW<-WD
    tmp.001.999 <- paste(substr(rep("000",999),1,3-nchar(001:999)),(1:999),sep="")
    for (i in (1:nx)) {
  	xlst <- trim(strsplit(paste(WD[,xvname[i]],"",sep=""), sepChar))
  	tmp.tb<-table(unlist(xlst)); tmp.tp<-numfmt(prop.table(tmp.tb)*100,2)
  	tmp.tt <- cbind(names(tmp.tb),tmp.tb,tmp.tp)[order(tmp.tb,decreasing=TRUE),]
  	nsubstr <- length(tmp.tb)
  	xsubStr <- tmp.tt[,1]
  	mxx <- matrix(0,nrow=nn.obs, ncol=nsubstr)
  	for (j in 1:nn.obs) {
  	  tmp.xlstj <- unique(xlst[[j]])
  	  tmp.xlstj <- tmp.xlstj[!is.na(tmp.xlstj)]
  	  if (length(tmp.xlstj)>0) mxx[j,match(tmp.xlstj, xsubStr)]<-1
  	}
  	if (nsubstr>999) {
  	  if (tmp.tt[1000,2] > minCount) minCount<-tmp.tt[1000,2]
  	}  
  	if (minCount>0) {
  	  tmpnlbl <- sum(as.numeric(tmp.tt[,2]) > minCount)
  	  newlblnames <- paste(xvname[i],c(tmp.001.999[1:tmpnlbl],rep("000",nsubstr-tmpnlbl)),sep=".")
  	  mxx<-cbind(mxx[,1:tmpnlbl],(apply(mxx[,-c(1:tmpnlbl)],1,sum)>0)*1)      
  	} else {
  	  newlblnames <- paste(xvname[i],tmp.001.999[1:nsubstr],sep=".")
  	}    
  	tmp.oo <- rbind(c("Var.NAME","Substring","Frequency","%"),cbind(newlblnames,tmp.tt))
  	nlblused <- ncol(mxx)
  	colnames(mxx) <- newlblnames[1:nlblused]
  	WD.NEW<-cbind(WD.NEW,mxx)
  	w<-c(w,"</br>", xb[i], "</br>")
  	w<-c(w, paste("</br>字段分解成:", nsubstr, "不同的子字段</br>"))
  	if (minCount>0) w<-c(w,"合并频数<=",minCount,"的字段，")
  	w<-c(w, paste("生成:", nlblused, "个 0/1 新变量</br>"))
  	w<-c(w,"</br>子字段频数分布: </br><table border=3 class=\"sortable\">",mat2htmltable(tmp.oo),"</table></br>")     
  	tmp.vv<-cbind(newlblnames,"",tmp.tt[,1])[1:nlblused,]
  	if (minCount>0) tmp.vv[nlblused,3]<-paste(tmp.vv[nlblused,3],"or others")
  	if (i==1) {out.vv<-tmp.vv;} else {out.vv<-rbind(out.vv,tmp.vv);}
    }  
    nr<-nrow(out.vv)
    out.vv <-cbind(out.vv,rep("",nr),rep("0",nr),rep("No",nr),rep("",nr),rep("1",nr),rep("Yes",nr))
    out.vv<-matrix(t(out.vv),ncol=3,byrow=TRUE)
    out.vv<-rbind(c("变量名","取值编码","意义"), out.vv)
    vxlsfname <- paste(ofname,"_variables.xls",sep="")
    write.table(out.vv,file=vxlsfname,col.names=FALSE,row.names=FALSE,sep="\t",quote=FALSE);
    xlsfname <- paste(ofname,".xls",sep="")
    write.table(WD.NEW,file=xlsfname,col.names=TRUE,row.names=FALSE,sep="\t",quote=FALSE);
    w<-c(w, paste("</br>新生成的变量存放在数据文件:", xlsfname), "</br>")
  } else {
    oo2<-c("","N","频率(prop)","95%CI low","95%CI upp","P.value(H0: prop=0.5)")
    n2x <- 0
    for (i in (1:nx)) {
  	tmp.lv<-levels(factor(WD[,xvname[i]]))
  	tmp.lb<-vlabelZ[match(paste(xvname[i],tmp.lv,sep="."),vnameZ)]
  	tmp.lb[is.na(tmp.lb)]<-tmp.lv[is.na(tmp.lb)]
  	tmp.tb<-table(WD[,xvname[i]]); tmp.tp<-round(prop.table(tmp.tb),4)
  	tmp.lb<-paste(tmp.lb,"\n ",numfmt(tmp.tp*100,1),"%",sep="")    
  	if (!is.na(h0[1])) {
  	  nlvi <- length(tmp.lv)
  	  if (length(h0)!=nlvi) h0 <- rep(1/nlvi,nlvi)
  	  xq<-chisq.test(tmp.tb,p=h0)
  	  print(xq)
  	  oo<-cbind(tmp.lb,xq$observed,tmp.tp,h0,numfmt(xq$expected,2))
  	  oo<-rbind(c(xb[1],"观察数","观察频率","理论频率","期望数"),oo)
  	  xx<-rbind(c("卡方值","自由度","p 值"),c(numfmt(xq$statistic,dec),xq$parameter,pvformat(xq$p.value,dec+2)))
  	  w<-c(w,"</br>", xb[i], "</br>")
  	  w<-c(w,"</br>单向频数表: </br><table border=3>",mat2htmltable(oo),"</table></br>") 
  	  w<-c(w,"</br>卡方检验: </br><table border=3>",mat2htmltable(xx),"</table></br>") 
  	} else {
  	  oo2<-rbind(oo2,c(xb[i],"","","","",""))
  	  ooi<-cbind(tmp.lb,tmp.tb,tmp.tp)
  	  if (length(tmp.tb)==2) {
  		n2x <- n2x+1
  		tmp.tt<-prop.test(tmp.tb)
  		print(tmp.tt)
  		ooi<-cbind(ooi,numfmt(rbind(tmp.tt$conf.int,sort(1-tmp.tt$conf.int)),dec),pvformat(tmp.tt$p.value,dec+2))
  	  } else {
  		ooi<-cbind(ooi,"","","")
  	  }
  	  oo2<-rbind(oo2,ooi)
  	}
  	png(paste(ofname,xvname[i],"pie.png",sep="_"),width=720,height=560)
  	pie(tmp.tp,col=rainbow(length(tmp.lv)),labels=tmp.lb,main=paste("频数分布:",xb[i]))
  	dev.off()
    }
    if (is.matrix(oo2)) {
  	if (n2x==0) ooi<-oo2[,-c(4,5,6)]
  	w<-c(w,"</br>单向频数表: </br><table border=3>",mat2htmltable(oo2),"</table></br>") 
  	if (n2x>0) w<-c(w, "1 sample proportions test with continuity correction</br>")
    }
    if (nx>1 & !is.na(parm[1])) {
  	ntmp<-rep(1,times=nrow(xv))
  	ddu<-apply(xv,1,function(z) paste(z,collapse=""))
  	tb3<-tapply(ntmp,factor(ddu),sum);tb3.m<-names(tb3);tbb<-xv[match(tb3.m,ddu),]
  	tb1<-cbind(tbb,tb3,numfmt(tb3/sum(tb3),dec+2))
  	tb1<-rbind(c(xb,"N","频率"),tb1)
  	w<-c(w,"</br>多变量联合频数分布: </br><table border=3>",mat2htmltable(tb1),"</table></br>") 
  	tbb1<-apply(is.na(tbb),1,sum)
  	if (sum(tbb1)>0) {
  	  tb4<-tb3[tbb1==0];
  	  tb2<-cbind(tbb[tbb1==0,],tb4,numfmt(tb4/sum(tb4),dec+2)); tb2<-rbind(c(xb,"N","频率"),tb2)
  	  w<-c(w,"</br>多变量联合频数分布(除去缺失值): </br><table border=3>",mat2htmltable(tb2),"</table></br>") 
  	}
    }
  }
  sink()
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)