  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0501"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  yparm1 <- as.character(pa[1,1])
  yparm2 <- as.character(pa[2,1])
  yparm3 <- as.character(pa[3,1])
  ydist <- as.character(pa[4,1]) 
  ylink <- as.character(pa[5,1])
  xparm1 <- as.character(pa[6,1])
  xparm2 <- as.character(pa[7,1])
  xparm3 <- as.character(pa[8,1])
  xparm4 <- as.character(pa[9,1])
  xparm5 <- as.character(pa[10,1])
  method1 <- as.character(pa[11,1])
  
  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
  iyn <- yparm1
  iyv <- yparm2
  iys <- as.numeric(yparm3)
  iydist <- ydist
  iylink <- ylink
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
  library(gplots,lib.loc=R.LibLocation)
  library(car,lib.loc=R.LibLocation)
  library(MASS,lib.loc=R.LibLocation)
  library(relaimpo,lib.loc=R.LibLocation)
  library(leaps,lib.loc=R.LibLocation)
  library(vegan,lib.loc=R.LibLocation)
  library(methods,lib.loc=R.LibLocation)
  library(nortest,lib.loc=R.LibLocation)
  library(survey,lib.loc=R.LibLocation)

  ofname<-"5_1"; 
  svy.DSN.YN <- FALSE; 
  WD<-idata; wd.subset=""; 
  title<-"广义线性模型"; 
  attach(WD) 
  subjvname<-NA;
  
  yv<-as.data.frame(idata[c(iyn)]); 
  yvname<-c(iyn); 
  yvar<-c(iyn); 
  ydist<-c(iydist); 
  ylink<-c(iylink); 
  ylv<-c(iys); 

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
  sxf<-c(ifs); 
  
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, m1, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## Matrix gdata gplots car MASS relaimpo leaps vegan methods nortest survey ##R package##;
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
  coefnames2formula<-function(coefnamearray) {
    tmp2<-""; ff=""
    for (i in (2:length(coefnamearray))) {
      tmp<-coefnamearray[i]; tmp1<-tmp
      if (substr(tmp,1,7)=="factor(") {
        for (k in (9:nchar(tmp))) {if (substr(tmp,k,k)==")") tmp1<-substr(tmp,1,k);}
      }
      if (tmp1!=tmp2) {ff<-ifelse(ff=="",tmp1,paste(ff,tmp1,sep="+")); }
      tmp2=tmp1
    }
    return(ff)
  }
  glmtermplot<-function(mdl,grp,yvar,yi) {
    for (k in (1:nx)) {
      if (grp==0) {
        png(paste(ofname,"_",yvar,"_termplot",k,".png",sep=""),width=720,height=560); 
        mtxt=paste("Outcome:",yb[yi]);
      } else {
        png(paste(ofname,"_",yvar,"_",bvname,grp,"_termplot",k,".png",sep=""),width=720,height=560);
        mtxt=paste("Outcome:",yb[yi],", subgroup:",bvb,"=",bv.lb[grp])
      } 
      termplot(mdl,partial.resid=T,terms=k,main=mtxt)
      dev.off()
    }
  }
  stpanova2htmltable<-function(mdl,w) {
    tmp<-matrix(unlist(mdl$anova),ncol=length(mdl$anova))
    tmp<-rbind(names(mdl$anova),tmp)
    w<-c(w,"</br><table border=3>", mat2htmltable(tmp), "</table>")
    return(w)
  }
  glm2htmltable<-function(mdl,w) {
    gs<-summary(mdl);print(gs)
    coe<-gs$coefficients
    colp<-ncol(coe)
    coep<-coe[,colp]
    if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
      coe<-coe[,-colp]
      cnames<-c(colnames(coe),"exp(coef)","95%CI low","95%CI upp","P.value")
      coe<- cbind(coe, exp(coe[,1]), exp(coe[,1]-1.96*coe[,2]), exp(coe[,1]+1.96*coe[,2]))
    }
    if (gs$family[[2]]=="identity") {
      coe<-coe[,-colp]
      cnames<-c(colnames(coe),"95%CI low","95%CI upp","P.value")
      coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2])
    }
    coe<-cbind(numfmt(coe,5),pvformat(coep,6))
    oo1<-cbind(c("",rownames(coe)),rbind(cnames,coe))
    p1<-c("AIC:",numfmt(gs$aic,4))
    p2<-c("Log Likelihood:", paste(numfmt(logLik(mdl),4), ", df=",gs$df[1]+1))
    p3<-c("Null.deviance",paste(numfmt(gs$null.deviance,4), "on", gs$df.null, "degrees of freedom"))
    p4<-c("deviance",paste(numfmt(gs$deviance,4), "on", gs$df.residual, "degrees of freedom"))
    norp<-round(pearson.test(mdl$residuals)$p.value,4)
    if (norp==0) norp<-"<0.0001"
    p41<-c("residuals SD",paste(round(sd(mdl$residuals),4),"(pearson chi-square normality test P=", norp,")"))
    p5<-cbind(c("R-squared","Adj R-squared"), numfmt(unlist(RsquareAdj(mdl)),4))
    p6<-c("Number of observations used:", length(gs$deviance.resid))
    oo2<-rbind(p1,p2,p3,p4,p41,p5,p6)
    w<-c(w,"</br><table border=3>", mat2htmltable(oo1), "</table>")
    w<-c(w,"</br><table border=3>", mat2htmltable(oo2), "</table>")
    return(w)
  }
  bsglm2htmltable <- function(fml, yd, yl) {
    wdt <- wd;
    nnwd<- nrow(wdt);
    tmp.mdl0 <- setglm(fml,yd,yl)
    gs <- summary(tmp.mdl0)
    betas <-gs$coefficients[,1];
    rnames <- rownames(gs$coefficients);
    rrrd <- FALSE
    if (gs$family[[2]] == "logit") {
      dm0 <- or2rrrd(tmp.mdl0, data=wd, rrrd.only=TRUE)
      ref.pop <- dm0[,1]
      exp.pop <- dm0[,2]
      r0s <- dm0[,3]
      r1s <- dm0[,4]
      rrs <- dm0[,5]
      rds <- dm0[,6]
      rrrd <- TRUE
    }
    set.seed(123456);
    for (s in (1:1000)) {
      assign("wd", wdt[sample(1:nnwd,nnwd,replace=T),], envir = .GlobalEnv) 
      tmp.mdls <- setglm(fml,yd,yl)
      betai <-summary(tmp.mdls)$coefficients[,1];
      betas <- cbind(betas,betai);
      if (rrrd) {
        dmi <- or2rrrd(tmp.mdls, data=wd, ref.pop=ref.pop, exp.pop=exp.pop, rrrd.only=TRUE)
        r0s <- cbind(r0s, dmi[,3])
        r1s <- cbind(r1s, dmi[,4])
        rrs <- cbind(rrs, dmi[,5])
        rds <- cbind(rds, dmi[,6])
      }
    }
    coe <- cbind(betas[,1],t(apply(betas,1,function(z) c(quantile(z,probs=c(0.025,0.975)),ifelse(z[1]>0,1-sum(z>0)/length(z),sum(z>0)/length(z))))))
    colnames(coe) <- c("coef.","95%CI low","95%CI upp","P.value")
    if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
      coe[,1:3] <- exp(coe[,1:3])
      colnames(coe) <- c("exp(coef)","95%CI low","95%CI upp","P.value")
    }
    coe<- numfmt(coe,5)
    oo1<-cbind(c("",rnames),rbind(colnames(coe),coe))
    oo <- "</br>Bootstrap results (N resampling: 1000)" 
    oo <- c(oo,"</br><table border=3>", mat2htmltable(oo1), "</table>")
    if (rrrd) {
      r0 <- cbind(r0s[,1],t(apply(r0s,1,function(z) quantile(z,probs=c(0.025,0.975)))))
      r1 <- cbind(r1s[,1],t(apply(r1s,1,function(z) quantile(z,probs=c(0.025,0.975)))))
      rr <- cbind(rrs[,1],t(apply(rrs,1,function(z) c(quantile(z,probs=c(0.025,0.975)),ifelse(z[1]>1,1-sum(z>1)/length(z),sum(z>1)/length(z))))))
      rd <- cbind(rds[,1],t(apply(rds,1,function(z) c(quantile(z,probs=c(0.025,0.975)),ifelse(z[1]>0,1-sum(z>0)/length(z),sum(z>0)/length(z))))))
      oo2<-cbind(r0,r1,rr,rd)
      oo2 <- cbind(ref.pop,exp.pop,numfmt(oo2,4))
      cname <- c("","R0.pop","R1.pop","R0","R0.low","R0.upp","R1","R1.low","R1.upp","RR","RR.low","RR.upp","RR.p","RD","RD.low","RD.upp","RD.p")
      oo2 <- rbind(cname, cbind(rnames[-1],oo2))
      oo <- c(oo, "</br></br>Calculate RR (risk ratio) and RD (risk difference) from logistic model using bootstrap (N resampling: 1000)")
      oo <- c(oo,"</br><table border=3>", mat2htmltable(oo2), "</table>")
      oo <- c(oo, "</br>Note: R1 were calculated by change the line of R0.pop with R1.pop value (only change one value each a time). ")
      if (grepl("\\*",fml)) {
        oo <- c(oo,"For interactions(IA): R0 were based on R0.pop but with the 1st IA variable changed with R1.pop, R1 were with both IA variables changed.")
      }
      oo <- c(oo, "</br>")
    }
    assign("wd",wdt,envir = .GlobalEnv)
    return(oo)
  }
  stpanova2htmltable<-function(mdl,w) {
    tmp<-matrix(unlist(mdl$anova),ncol=length(mdl$anova))
    tmp<-rbind(names(mdl$anova),tmp)
    w<-c(w,"</br><table border=3>", mat2htmltable(tmp), "</table>")
    return(w)
  }
  setglm<-function(fml,yd,yl="") {
    if (yd=="gaussian" | yd=="normal") {if (yl=="") fam<-gaussian() else fam<- gaussian(link=yl);}
    if (yd=="bin" | yd=="binomial")    {if (yl=="") fam<-binomial() else fam<- binomial(link=yl);}
    if (yd=="poisson")          {if (yl=="") fam<-poisson() else fam<-poisson(link=yl);}
    if (yd=="gamma")            {if (yl=="") fam<-Gamma() else fam<-Gamma(link=yl);}
    if (yd=="inverse.gaussian") {if (yl=="") fam<-inverse.gaussian() else fam<-inverse.gaussian(link=yl);}
    if (yd=="quasi")            {if (yl=="") fam<-quasi() else fam<-quasi(link=yl);}
    if (yd=="quasibinomial")    {if (yl=="") fam<-quasibinomial() else fam<-quasibinomial(link=yl);}
    if (yd=="quasipoisson")     {if (yl=="") fam<-quasipoisson() else fam<-quasipoisson(link=yl);}
    if (yd=="negbin")           fam<-negbin(c(1,10),link="log") 
    assign("fam", fam, envir = .GlobalEnv)
    if (svy.DSN.YN) {
      md<-try(svyglm(formula(fml),svy.DSN, family=fam, data=wd, na.action=na.omit))
    } else {
      md<-try(glm(formula(fml), family=fam, weights=wd$weights, data=wd, na.action=na.omit))
    }
    return(md)
  }
  or2rrrd <- function(mdl, data=wd, ref.pop=c(), exp.pop=c(), rrrd.only=FALSE) {
    coef <- mdl$coefficients
    vmat <- vcov(mdl)
    ftmp <- names(coef)[-1]
    fvar <- sapply(ftmp, function(z) strsplit(z,":")[[1]][1])
    fia <- (fvar != ftmp)
    nxt <- length(ftmp)
    if (length(ref.pop) == nxt) {
      fval <- ref.pop
    } else {
      fval <- rep(0,nxt)
      fcon <- (substr(ftmp,1,7)!="factor(")
      if (sum(fcon)>0) fval[fcon] <- apply(cbind(1,data[,fvar[fcon]]),2,function(z) round(mean(z, na.rm=TRUE),2))[-1]
    } 
    if (length(exp.pop) == nxt) {
      fexp <- exp.pop
    } else {
      fexp <- fval + 1
    }  
    fia1 <- rep("", nxt); fia1[fia] <- sapply(ftmp[fia], function(z) strsplit(z,":")[[1]][1])
    fia2 <- rep("", nxt); fia2[fia] <- sapply(ftmp[fia], function(z) strsplit(z,":")[[1]][2])
    fref <- fval
    fref[fia] <- 0
    v0 <- c(-1, fref)
    p0s <- paste("1/(1+exp(",paste(paste(v0,"*x",1:length(v0),sep=""),collapse=" - "),"))",sep="")
    p0f <- formula(paste("~",p0s))
    out <- NULL
    for (t in 1:nxt) {
      v1 <- fref;  v1[t] <- fexp[t]
      if (fia[t]) {
        v0x <- fref
        v0x[ftmp == fia1[t]] <- fexp[ftmp == fia1[t]]
        v0x <- c(-1, v0x)
        p0sx <- paste("1/(1+exp(",paste(paste(v0x,"*x",1:length(v0x),sep=""),collapse=" - "),"))",sep="")
        p0fx <- formula(paste("~",p0sx))
        v1[ftmp == fia1[t]] <- fexp[ftmp == fia1[t]]
        v1[ftmp == fia2[t]] <- fexp[ftmp == fia2[t]]      
      } else {
        p0sx <- p0s
        p0fx <- p0f
      }
      v1 <- c(-1, v1)
      p1s <- paste("1/(1+exp(",paste(paste(v1,"*x",1:length(v1),sep=""),collapse=" - "),"))",sep="")
      p1f <- formula(paste("~",p1s))
      prf <- formula(paste("~ (", p1s, ") / (", p0sx, ")", sep=""))
      pdf <- formula(paste("~ (", p1s, ") - (", p0sx, ")", sep=""))
      sei <- deltaMethod(list(p0fx,p1f,prf,pdf), coef, vmat, rrrd.only=rrrd.only)
      out <- rbind(out, sei)
    }
    if (rrrd.only) {
      out <- cbind(round(fref,2),round(fexp,2),out)
      colnames(out) <- c("R0.pop","R1.pop","R0","R1","RR","RD")
      rownames(out) <- ftmp
    } else {
      out <- cbind(ftmp,round(fref,2),round(fexp,2),numfmt(out[,-c(3,6)],4))
      out <- rbind(c("", "R0.pop","R1.pop","R0","R0.se","R1","R1.se","RR","RR.se","RR.p","RD","RD.se","RD.p"), out)
      out <- c("</br>RR (risk ratio) and RD (risk difference) from OR (odds ratio) by delta method</br><table border=3>", mat2htmltable(out), "</table>")
      out <- c(out, "</br>Note: R1 were calculated by change the line of R0.pop with R1.pop value (only change one value each a time). ")
      if (sum(fia)>0) {
        out <- c(out,"For interactions(IA): R0 were based on R0.pop but with the 1st IA variable changed with R1.pop, R1 were with both IA variables changed.")
      }
      out <- c(out, "</br>")
    }
    return(out)
  }
  deltaMethod <- function (g, mean, cov, rrrd.only=FALSE) {
    cov <- as.matrix(cov)
    n <- length(mean)
    if (!is.list(g)) g <- list(g)
    if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) stop(paste("Covariances should be a ", n, " by ", n, " matrix"))
    syms <- paste("x", 1:n, sep = "")
    for (i in 1:n) assign(syms[i], mean[i])
    gdashmu <- t(sapply(g, function(form) {as.numeric(attr(eval(deriv(form, syms)), "gradient"))}))
    gmu <- sapply(g, function(form) {as.numeric(eval(deriv(form, syms)))})
    if (rrrd.only) {
      return(gmu)
    } else {
      new.covar <- gdashmu %*% cov %*% t(gdashmu)
      se <- sqrt(diag(new.covar))
      z <- rep(NA, length(gmu))
      z[se!=0] <- gmu[se!=0]/se[se!=0]
      p <- 2*pnorm(-abs(z))
      return(matrix(rbind(gmu, se, p), nrow=1))
    }
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  xv1<-xvname; xv1[xlv>1]<-paste("factor(",xvname[xlv>1],")",sep="")
  xvstr<-paste(xv1,collapse="+")
  xintrstr<-""
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(WD[,bvar])); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
    xvstr0<-paste(xvstr,"+factor(",bvar,")",sep="")
    if (sum(sxf=="S")>0) xintrstr<-paste(paste(xv1[sxf=="S"], paste("factor(",bvar,")",sep=""),sep="*"), collapse="+")
  }
  nx<-length(xvname); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  ny<-length(yvname); yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
  allname <- c(yvname,xvname,bvar); allname <- allname[!is.na(allname)]
  WD <- WD[,allname]
  dd.tmp <- cbind(WD[,-(1:ny)],1)
  cmp<-(apply(is.na(dd.tmp),1,sum)==0); rm(dd.tmp)
  WD <- WD[cmp, ]
  parmdl <- parm[4];
  if (is.na(parmdl)) parmdl=1;  if (!is.numeric(parmdl)) parmdl=1;
  if (nx==1) parmdl=1;
  nvmax=8; if (!is.na(parm[2])) {if (is.numeric(parm[2])) nvmax=parm[2];}
  nbest=3; if (!is.na(par1)) {if (is.numeric(par1)) nbest=par1;}
  if (nvmax>nx) nvmax=nx;
  if (nbest>nx) nbest=nx;
  bt.mdl <- parm[1]; 
  if (is.na(bt.mdl)) bt.mdl <- 0
  if (bt.mdl == 1) parmdl <- 1
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  
  
  glmdiagplot<-function(mdl,yvar,yi) {
    png(paste(ofname,yvar,"residual.png",sep="_"),width=720,height=560)
    plot(mdl$fitted.values,mdl$residuals, main=paste(yb[yi],"残差与预测值关系"), xlab="模型的预测值",ylab="残差")
    dev.off()
    png(paste(ofname,yvar,"QQ.png",sep="_"),width=720,height=560)
    qqnorm(mdl$residuals, main=paste(yb[yi],"残差的QQ图"));  qqline(mdl$residuals)
    dev.off()
  }
  regss2htmltable<-function(rs,yname,yi,grp,w) {
    rss<-summary(rs)
    rs1<-rss$which
    rs2<-matrix("",nrow=nrow(rs1),ncol=ncol(rs1)); rs2[rs1]<-"*"
    nmd<-length(rss$adjr2)
    beta<-coef(rs,(1:nmd))
    for (z in (1:nrow(rs2))) {
      rs2[z,match(names(beta[[z]]),colnames(rs1))]<-numfmt(beta[[z]],4)
    } 
    oo1<-cbind(c("#Variables",rownames(rs1)), rbind(colnames(rs1),rs2))
    rsq<-rbind(c("R-squared","Adj-Rsquared"),numfmt(cbind(rss$rsq,rss$adjr2),4))
    oo1<-cbind(oo1, rsq)
    tmp.midx<-which.max(rss$adjr2)
    tmp.xnm<-names(coef(rs,tmp.midx))
    tmp.xfml<-paste(yvname[yi],"~",coefnames2formula(tmp.xnm))
    tmp.gxm<-glm(formula(tmp.xfml),family=gaussian,weights=wd$weights,data=wd); print(tmp.gxm);
    if (grp==0 & length(tmp.xnm)>2) {
      tmp.relimp<-calc.relimp(tmp.gxm,type=c("lmg","last","first"),rela=TRUE);
      png(paste(ofname,"_",yname,"_relimp.png",sep=""),width=720,height=560)
      plot(tmp.relimp, main=paste("自变量相对重要性(Y=",yb[yi],")",sep=""))
      dev.off()
      print(tmp.relimp);
    }
    w<-c(w,"</br><table border=3>", mat2htmltable(oo1), "</table>")
    w<-c(w,"</br>调整的R平方值为最大的模型:")
    w<-glm2htmltable(tmp.gxm,w)
    return(w)
  }
  w<-c(w,"<h2>广义线性模型</h2>")
  sink(paste(ofname,".lst",sep=""))
  if (svy.DSN.YN) summary(svy.DSN)
  for (i in (1:ny)) {
    if (ylink[i]=="" && ydist[i]=="binomial") ylink[i] <- "logit"
    fml<-paste(yvname[i],"~",xvstr)
    w<-c(w,"</br>结局变量:", yb[i])
    w<-c(w,"</br>变量分布与联系函数:", paste(ydist[i]), " (", ylink[i], ")", sep="")
    if (!is.na(bvar)) {
      w<-c(w,"</br>模型:", fml)
      for (b in (1:nbg)) {
        wd<-WD[WD[,bvar]==bv.lv[b],]
        w<-c(w,paste("</br></br>亚组:", bvar, "=", bv.lb[b]))
        tmp.glm<-setglm(fml,ydist[i],ylink[i])
        if (substr(tmp.glm[[1]][1],1,5)!="Error") {
          if (parmdl==3 & ydist[i]=="gaussian") {
            tmp.rs<-regsubsets(formula(fml),weights=wd$weights,data=wd,nbest=nbest,nvmax=nvmax, intercept=TRUE, method=c("exhaustive"))
            w<-regss2htmltable(tmp.rs,yvar[i],i,b,w)
          } else {
            if (parmdl==2) {
              tmp.stp<-stepAIC(tmp.glm,direction="both");
              w<-glm2htmltable(tmp.stp,w); w<-stpanova2htmltable(tmp.stp,w);
            } else {
              glmtermplot(tmp.glm,b,yvar[i],i); w<-glm2htmltable(tmp.glm,w);
            }
          }
          if (ylink[i]=="logit") w<-c(w, or2rrrd(tmp.glm, data=wd))
          if (bt.mdl==1) w<-c(w, bsglm2htmltable(fml,ydist[i],ylink[i]))
        }
        rm(wd)
      }
      fml0<-paste(yvname[i],"~",xvstr0,sep="")
      w<-c(w,"</br>合计:")
    } else {fml0<-fml;} 
    w<-c(w,"</br>模型:", fml0)
    wd<-WD[cmp,];
    tmp.mdl<-setglm(fml0,ydist[i],ylink[i])  
    if (substr(tmp.mdl[[1]][1],1,5)!="Error") {
      if (parmdl==3 & ydist[i]=="gaussian") {
        tmp.rs<-try(regsubsets(formula(fml0),weights=wd$weights,data=wd,nbest=nbest,nvmax=nvmax, intercept=TRUE, method=c("exhaustive")))
        if (substr(tmp.rs[[1]][1],1,5)!="Error") w<-regss2htmltable(tmp.rs,yvar[i],i,0,w)
      } else {
        if (parmdl==2) {
          tmp.stp<-stepAIC(tmp.mdl,direction="both");
          if (substr(tmp.stp[[1]][1],1,5)!="Error") {
            glmdiagplot(tmp.stp, yvar[i],i); 
            w<-glm2htmltable(tmp.stp,w); w<-stpanova2htmltable(tmp.stp,w);
          }
        } else {
          glmdiagplot(tmp.mdl,yvar[i],i); w<-glm2htmltable(tmp.mdl,w);
        }
      }
      if (is.na(bvar)) glmtermplot(tmp.mdl,0,yvar[i],i)
      if (ylink[i]=="logit") w<-c(w, or2rrrd(tmp.mdl, data=wd))
      if (bt.mdl==1) w<-c(w, bsglm2htmltable(fml0,ydist[i],ylink[i]))
    }
    w<-c(w,"</br>")
    if (xintrstr > "") {
      wd <- WD[cmp,];
      fmlx<-paste(fml0,xintrstr,sep="+")
      w<-c(w,"</br></br>交互作用模型:", fmlx)
      tmp.mdl<-setglm(fmlx,ydist[i],ylink[i])  
      if (substr(tmp.mdl[[1]][1],1,5)!="Error") {
        if (parmdl==2) {
          tmp.stp<-stepAIC(tmp.mdl,direction="both");
          glmdiagplot(tmp.stp, yvar[i],i); 
          w<-glm2htmltable(tmp.stp,w); w<-stpanova2htmltable(tmp.stp,w);
        } else {
          glmdiagplot(tmp.mdl,yvar[i],i); w<-glm2htmltable(tmp.mdl,w);
        }
        if (ylink[i]=="logit") w<-c(w, or2rrrd(tmp.mdl, data=wd))
        if (bt.mdl==1) w<-c(w, bsglm2htmltable(fmlx,ydist[i],ylink[i]))
      }
    }
  }    
  sink()
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)