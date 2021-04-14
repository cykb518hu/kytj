  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0505"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  cparm1 <- as.character(pa[1,1])
  cparm2 <- as.character(pa[2,1])
  xparm1 <- as.character(pa[3,1])
  xparm2 <- as.character(pa[4,1])
  xparm3 <- as.character(pa[5,1])
  xparm4 <- as.character(pa[6,1])
  xparm5 <- as.character(pa[7,1])
  method1 <- as.character(pa[8,1]) 
  tparm1 <- as.character(pa[9,1])
  tparm2 <- as.character(pa[10,1])  

  icn <- unlist(strsplit(cparm1,"[|]"))  
  icv <- unlist(strsplit(cparm2,"[|]")) 
  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
  m1 <- as.numeric(method1)
  itimen <- tparm1
  itimev <- tparm2
  
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
  
  vname<-c("_N_","_STAT_","_TOTAL_",icn,ixn,itimen)
  vlabel<-c("样本量(%)","统计量","合计",icv,ixv,ixv,itimev)

  library(survival,lib.loc=R.LibLocation)
  ofname<-"5_5"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"条件Logistic回归"; 
  attach(WD) 
  subjvname<-NA;

  library(stringr)
  sq <- str_count(xparm4,",")+1
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
  xlv<-c(ixs); 
  
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  
  timev<-idata[,itimen];timevname<-c(itimen); 
  
  bv<-NA; bvar<-NA; 
  colv<-idata[,icn[1]];colvname<-c(icn[1]); 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-m1;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## survival ##R package##;
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  clog2htmltable<-function(mdl) {
    gs<-summary(mdl)
    coe<-gs$coefficients; rname.coe<-rownames(coe)
    cnames<-c(colnames(coe)[-2],"exp(coef)","95%CI low","95%CI upp")
    coe<- cbind(matrix(coe[,c(1,3,4,5)],ncol=4), exp(coe[,1]), exp(coe[,1]-1.96*coe[,3]), exp(coe[,1]+1.96*coe[,3]))
    oo1<-cbind(c("",rname.coe),rbind(cnames,round(coe,dec)))
    p1<-c("N:", gs$n)
    p2<-c("Number of events:",gs$nevent)
    p3<-c("Log Likelihood:", format(gs$loglik[2],digits=4,nsmall=4))
    p4<-c("R-square",paste(round(gs$rsq[1],4), ", max r-square", round(gs$rsq[2],4)))
    oo2<-rbind(p1,p2,p3,p4)
    oo3<-rbind(gs$logtest,gs$waldtest,gs$sctest)
    c1<-c("Likelihood ratio test","Wald test","Score (log rank) test")
    oo3<-cbind(c1,round(oo3[,1],4),oo3[,2],format(oo3[,3],digits=4,nsmall=4))
    oo3<-rbind(c("","test","df","p.value"),oo3)
    oo<-c("</br><table border=3>", mat2htmltable(oo1), "</table>")
    oo<-c(oo,"</br><table border=3>", mat2htmltable(oo2), "</table>")
    oo<-c(oo,"</br><table border=3>", mat2htmltable(oo3), "</table>")
    return(oo)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  dd<-cbind(weights,timev,colv,xv); cname<-c("weights",timevname,colvname,xvname)
  if (!is.na(bvar)) {dd<-cbind(dd,bv); cname<-c(cname,bvar);}
  colnames(dd)<-cname;
  cmp<-(apply(is.na(dd),1,sum)==0); dd<-dd[cmp,]; dd<-dd[order(dd[,1]),]
  detach(WD); rm(WD); WD<-as.data.frame(dd); attach(WD)
  if (is.na(par1)) par1<-2
  mth<-"exact"; if (par1==1) mth<-"approximate"
  if (is.na(dec)) dec<-4
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  xv1<-xvname; xv1[xlv>2]<-paste("factor(",xvname[xlv>2],")",sep="")
  xv1<-paste(xv1,collapse="+"); fml<-paste(colvname,"~",xv1,"+strata(",timevname,")")
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  timevb<-vlabelV[match(timevname,vnameV)]; if (is.na(timevb)) timevb<-timevname;
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,"<h2>Conditional Logistic Regression</h2>")
  w<-c(w,"</br>Outcome variable:", colvb)
  w<-c(w,"</br>Matched set ID variable:", timevb)
  w<-c(w,"</br>Model:", fml)
  if (!is.na(bvar)) w<-c(w,"</br>Total:")
  tmp.mdl<-clogit(formula(fml),method=mth,weights=WD$weights,data=WD)
  w<-c(w,clog2htmltable(tmp.mdl))    
  if (!is.na(bvar)) {
    bv<-WD[,ncol(WD)]
    for (b in (1:nbg)) {
      tmp.mdl<-clogit(formula(fml),method=mth,weights=WD$weights,data=WD,subset=(bv==bv.lv[b]))
      w<-c(w,paste("</br></br>For sub-group:", bvar, "=", bv.lb[b]))
      w<-c(w,clog2htmltable(tmp.mdl))    
    }
  }    
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)