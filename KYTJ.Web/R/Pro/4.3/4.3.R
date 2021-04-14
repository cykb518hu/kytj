  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0403"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  cparm1 <- as.character(pa[4,1])
  cparm2 <- as.character(pa[5,1])
  cparm3 <- as.character(pa[6,1])

  ixn <- unlist(strsplit(xparm1,"[,]"))
  ixv <- unlist(strsplit(xparm2,"[,]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  icn <- cparm1
  icv <- cparm2
  
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

  library(psychometric,lib.loc=R.LibLocation)
  library(multilevel,lib.loc=R.LibLocation)
  library(nlme,lib.loc=R.LibLocation)
  library(irr,lib.loc=R.LibLocation)

  ofname<-"4_3";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"组内相关系数"; 
  attach(WD) 
  subjvname<-NA;

  library(stringr)
  sq <- str_count(xparm3,",")+1
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
  xlv<-c(ixs); 
  
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  
  colv<-as.matrix(idata[,c(icn)]);colvname<-c(icn); 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, 1, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 

  ##R package## psychometric multilevel nlme ##R package##;
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
  mltobs<-1; if (!is.na(parm[4])) mltobs<-parm[4]
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  if (!is.na(colvname)) {
    colvb <- vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname; 
  }
  oo <- c("Method","ICC","ICC 95%CI lower","ICC 95%CI upper")
  if (mltobs==2) {
    x <- as.vector(as.matrix(WD[,xvname]))
    g <- as.vector(t(matrix(rep(1:nx,nrow(WD)),nrow=nx)))
    wtmp <- data.frame(x,g)
    if (!is.na(bvar)) {
      z <- rep(WD[,bvar],nx)
      for (b in (1:nbg)) {
        oo <- rbind(oo,c(paste(bvb,"=",bv.lb[b]),"","",""))
        WD1<-wtmp[z==bv.lv[b],]
        ic1.lme  <- numfmt(ICC1.lme(WD1[,1],WD1[,2],WD1),dec)
        ic21.lme <- numfmt(ICC2.lme(WD1[,1],WD1[,2],WD1, weighted = FALSE),dec)
        ic22.lme <- numfmt(ICC2.lme(WD1[,1],WD1[,2],WD1, weighted = TRUE),dec)
        ic1.aov  <- numfmt(ICC1.CI(WD1[,1],WD1[,2],WD1)[c(2,1,3)],dec)
        ic2.aov  <- numfmt(ICC2.CI(WD1[,1],WD1[,2],WD1)[c(2,1,3)],dec)
        icctb <- rbind(c(ic1.lme,"",""),c(ic21.lme,"",""),c(ic22.lme,"",""),ic1.aov,ic2.aov)
        iccname <- c("ICC1 from random effect model","ICC2 from random effect model, not weighted","ICC2 from random effect model, weighted")
        iccname <- c(iccname,"ICC1 from ANOVA","ICC2 from ANOVA")
        oo <- rbind(oo,cbind(iccname,icctb))
      }
    } else {
      WD1<-wtmp
      ic1.lme  <- numfmt(ICC1.lme(WD1[,1],WD1[,2],WD1),dec)
      ic21.lme <- numfmt(ICC2.lme(WD1[,1],WD1[,2],WD1, weighted = FALSE),dec)
      ic22.lme <- numfmt(ICC2.lme(WD1[,1],WD1[,2],WD1, weighted = TRUE),dec)
      ic1.aov  <- numfmt(ICC1.CI(WD1[,1],WD1[,2],WD1)[c(2,1,3)],dec)
      ic2.aov  <- numfmt(ICC2.CI(WD1[,1],WD1[,2],WD1)[c(2,1,3)],dec)
      icctb <- rbind(c(ic1.lme,"",""),c(ic21.lme,"",""),c(ic22.lme,"",""),ic1.aov,ic2.aov)
      iccname <- c("ICC1 from random effect model","ICC2 from random effect model, not weighted","ICC2 from random effect model, weighted")
      iccname <- c(iccname,"ICC1 from ANOVA","ICC2 from ANOVA")
      oo <- rbind(oo,cbind(iccname,icctb))
    }
  } else {
    if (!is.na(bvar)) {
      for (b in (1:nbg)) {
        WD1 <- WD[bv==bv.lv[b],]
        oo <- rbind(oo,c(paste(bvb,"=",bv.lb[b]),"","",""))
        for (i in (1:nx)) {
          if (nx>1) oo<-rbind(oo,c(xb[i],"","",""))
          ic1.lme  <- numfmt(ICC1.lme(WD1[,xvname[i]],WD1[,colvname],WD1),dec)
          ic21.lme <- numfmt(ICC2.lme(WD1[,xvname[i]],WD1[,colvname],WD1, weighted = FALSE),dec)
          ic22.lme <- numfmt(ICC2.lme(WD1[,xvname[i]],WD1[,colvname],WD1, weighted = TRUE),dec)
          ic1.aov  <- numfmt(ICC1.CI(WD1[,xvname[i]],WD1[,colvname],WD1)[c(2,1,3)],dec)
          ic2.aov  <- numfmt(ICC2.CI(WD1[,xvname[i]],WD1[,colvname],WD1)[c(2,1,3)],dec)
          icctb <- rbind(c(ic1.lme,"",""),c(ic21.lme,"",""),c(ic22.lme,"",""),ic1.aov,ic2.aov)
          iccname <- c("ICC1 from random effect model","ICC2 from random effect model, not weighted","ICC2 from random effect model, weighted")
          iccname <- c(iccname,"ICC1 from ANOVA","ICC2 from ANOVA")
          oo <- rbind(oo,cbind(iccname,icctb))
        }
        icc1<-icc(xv[bv==bv.lv[b],],model=way,type=type,unit=unit)
        if (b==1) {oo<-icc1;} else {oo<-rbind(oo,icc1);}
        rm(icc1);
      }
      oo<-cbind(c(bvb,colnames(oo)),t(cbind(bv.lb,oo)))
    } else {
      for (i in (1:nx)) {
        if (nx>1) oo<-rbind(oo,c(xb[i],"","",""))
        ic1.lme  <- numfmt(ICC1.lme(WD[,xvname[i]],WD[,colvname],WD),dec)
        ic21.lme <- numfmt(ICC2.lme(WD[,xvname[i]],WD[,colvname],WD, weighted = FALSE),dec)
        ic22.lme <- numfmt(ICC2.lme(WD[,xvname[i]],WD[,colvname],WD, weighted = TRUE),dec)
        ic1.aov  <- numfmt(ICC1.CI(WD[,xvname[i]],WD[,colvname],WD)[c(2,1,3)],dec)
        ic2.aov  <- numfmt(ICC2.CI(WD[,xvname[i]],WD[,colvname],WD)[c(2,1,3)],dec)
        icctb <- rbind(c(ic1.lme,"",""),c(ic21.lme,"",""),c(ic22.lme,"",""),ic1.aov,ic2.aov)
        iccname <- c("ICC1 by random effect model","ICC2 by random effect model, not weighted","ICC2 by random effect model, weighted")
        iccname <- c(iccname,"ICC1 by one-way ANOVA","ICC2 by one-way ANOVA")
        oo <- rbind(oo,cbind(iccname,icctb))
      }
    }
  }
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  
  
  w<-c(w,"<h2>组内相关系数</h2>")
  w<-c(w,"</br><table border=3>",mat2htmltable(oo),"</table></br>") 
  w<-c(w,"注解:</br>")
  w<-c(w,"</br>ICC by random effect model (从随机效应模型计算的组内相关系数): ")
  w<-c(w,"</br>let t00 = variance in intercept of the model, sigma2 = residual variance for the model. ")
  w<-c(w,"</br>ICC1 = t00/(t00 + siqma2) ")
  w<-c(w,"</br>ICC2: first computing ICC2 for each group t00/(t00 + sigma2/nj), nj = size of group j. ")
  w<-c(w,"</br>ICC2 not weighted: the mean across all groups. ")
  w<-c(w,"</br>ICC2 weighted: the mean across all groups weighted by group size. ")
  w<-c(w,"</br></br>ICC by one-way ANOVA:")
  w<-c(w,"</br>The CI is computed using formula provided by McGraw & Wong (1996).")
  w<-c(w,"</br></br>从随机效应模型计算的组内相关系数与采用单向方差分析法相比, 当每组样本量相同且没有缺失数据时是相同的; 否则，从随机效应模型计算的组内相关系数相对更确切。")
  w<-c(w,"</br></br>ICC2: The group mean reliability (Intraclass correlation 2)</br>")
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)