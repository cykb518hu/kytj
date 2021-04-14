  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0507"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  cparm1 <- as.character(pa[1,1])
  cparm2 <- as.character(pa[2,1])
  xparm1 <- as.character(pa[3,1])
  xparm2 <- as.character(pa[4,1])
  xparm3 <- as.character(pa[5,1])
  xparm4 <- as.character(pa[6,1])
  xparm5 <- as.character(pa[7,1])
  bparm1 <- as.character(pa[8,1]) 
  bparm2 <- as.character(pa[9,1])
  bparm3 <- as.character(pa[10,1])  

  icoln <- unlist(strsplit(cparm1,"[|]"))
  icolv <- unlist(strsplit(cparm2,"[|]"))
  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
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

  if(ibs==1)  {
    vname<-c("_N_","_STAT_","_TOTAL_",icoln,ixn,ibn)
    vlabel<-c("样本量(%)","统计量","合计",icolv,ixv,ibv)
  }else {
    vname<-c("_N_","_STAT_","_TOTAL_",icoln,ixn)
    vlabel<-c("样本量(%)","统计量","合计",icolv,ixv)
  }
  
  library(nnet,lib.loc=R.LibLocation)
  library(reshape,lib.loc=R.LibLocation)
  library(proto,lib.loc=R.LibLocation)
  library(ggplot2,lib.loc=R.LibLocation)
  library(MASS,lib.loc=R.LibLocation)
   
  ofname<-"5_7"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"多分类Logistic回归分析"; 
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
  sxf<-c(ifs);   
  
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  
  if(ibs==1)  {
    bv<-idata[,ibn[1]]; bvar<-c(ibn[1]);bvname<-c(ibn[1]); 
  }else {
    bv<-NA; bvar<-NA; 
  }

  icol<-icoln[1]
  colv<-idata[,icol];colvname<-c(icol)
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## nnet reshape proto ggplot2 MASS ##R package##;
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
  mlogistic <- function(d) {
    mdl <- NA;
    if (mgam == 1) {    
      mdl <- try(gam(fmls,data=d, weights=weights, family=multinom(K=k)))
    }  
    if (length(mdl)>2) {
      return(gamx2table(d,mdl));
    } else {
      mdl<-try(nnet::multinom(formula(fml),weights=d$weights,data=d))
      return(nnet2table(d,mdl));
    }
  }
  nnet2table<-function(d, mdl) {
    t1<-NA; outd<-NA;outd2<-NA
    if (length(mdl)>2) {
      ms<-summary(mdl); coe<-ms$coefficients; se<-ms$standard.errors;
      z<-coe/se;  p<-(1-pnorm(abs(z),0,1))*2
      print("Coefficients");print(ms$coefficients);
      print("Std. err");print(ms$standard.errors);print("P.value");print(p);
      oddratio<-numfmt(exp(coe),4);
      odd.low<-numfmt(exp(coe-1.96*se),4); 
      odd.upp<-numfmt(exp(coe+1.96*se),4);
      ww<-t(matrix(paste(oddratio," (",odd.low,", ",odd.upp,") ", pvformat(p,4), sep=""),nrow=nrow(coe)))
      rownames(ww)<-colnames(coe);       
      ww<-cbind("1.0 (ref.)",ww);
      colnames(ww)<-paste(colvname,"=",colv.lv)
      print("OR (95%CI) P.value")
      print(ww)
      w1<-cbind(c(" ",rownames(ww)),rbind(colnames(ww),ww))
      t1<-paste("</br>Multinomial Logistic Regression:  Odd.ratio (95% CI) P.value</br><table border=3>")
      t1<-c(t1,mat2htmltable(w1),"</table></br>")
      fit<-numfmt(fitted(mdl),5); fit1<-matrix(NA,nrow=nrow(d),ncol=ncol(fit))
      colnames(fit1)<-paste("P(",colvname,"=",colv.lv,")",sep="")
      outd<-cbind(d,fit1)
      outd[rownames(fit),((ncol(d)+1):ncol(outd))]<-fit
      tmpp<-nnetpred(mdl,as.matrix(d[,xvname],ncol=nx))
      if (!is.na(tmpp[[1]][1])) {
        t1<-c(t1,"</br>Predicted probability for each sub-group (with other Xs = mean or the first level)</br><table border=3>")
        t1<-c(t1,mat2htmltable(tmpp[[1]]),"</table></tr>")
      }
      outd2<-tmpp[[2]]
    } 
    return(list(t1,outd,outd2))
  }
  gamx2table<-function(d, mdl) {
    t1<-NA; outd<-NA;outd2<-NA
    if (length(mdl)>2) {
      print(summary(mdl))
      if (sum(sxf==0)>0) {
        mp <- summary(mdl)$p.table
        coe <- matrix(mp[,1], nrow=k, byrow=TRUE); colnames(coe) <- rownames(mp)[1:ncol(coe)]
        se <- matrix(mp[,2], nrow=k, byrow=TRUE);  colnames(se) <- rownames(mp)[1:ncol(se)]
        p <- matrix(mp[,4], nrow=k, byrow=TRUE);   colnames(p) <- rownames(mp)[1:ncol(p)]
        print("Coefficients");print(coe);
        print("Std. err");print(se);print("P.value");print(p);
        oddratio<-numfmt(exp(coe),4);
        odd.low<-numfmt(exp(coe-1.96*se),4); 
        odd.upp<-numfmt(exp(coe+1.96*se),4);
        ww<-t(matrix(paste(oddratio," (",odd.low,", ",odd.upp,") ", pvformat(p,4), sep=""),nrow=nrow(coe)))
        rownames(ww)<-colnames(coe);       
        ww<-cbind("1.0 (ref.)",ww);
        colnames(ww)<-paste(colvname,"=",colv.lv)
        print("OR (95%CI) P.value")
        print(ww)
        w1<-cbind(c(" ",rownames(ww)),rbind(colnames(ww),ww))
        t1<-paste("</br>Multinomial Logistic Regression:  Odd.ratio (95% CI) P.value</br><table border=3>")
        t1<-c(t1,mat2htmltable(w1),"</table></br>")
      }  
      fit<-numfmt(predict(mdl, type="response"),5)
      fit1<-matrix(NA,nrow=nrow(d),ncol=ncol(fit))
      colnames(fit1)<-paste("P(",colvname,"=",colv.lv,")",sep="")
      outd<-cbind(d,fit1)
      outd[rownames(fit),((ncol(d)+1):ncol(outd))]<-fit
      c0<-as.vector(sapply(paste(colvname, "=", colv.lv[-1]), function(z) paste(z, ": ", xname[sxf=="S"])))
      ms <- summary(mdl)$s.table
      w2 <- cbind(c(" ", c0),rbind(colnames(ms),numfmt(ms,4)))
      t1<-c(t1,"</br><table border=3>",mat2htmltable(w2),"</table></br>")
    }
    return(list(t1,outd,outd2))
  }
  npolr2table<-function(tmpd) {
    m2<-try(polr(formula(fml),weights=tmpd$weights,data=tmpd))
    if (length(m2)>2) {
      ms<-summary(m2); coe<-ms$coefficients; print(ms)
      p<-pvformat((1-pnorm(abs(coe[,3]),0,1))*2,4);
      coe1<- coe[,1]-1.96*coe[,2]; coe2<- coe[,1]+1.96*coe[,2]
      coe<-cbind(numfmt(cbind(exp(coe[,1]),exp(coe1),exp(coe2)),4),p);
      coe<-rbind(c("OR","95%CI Low","95%CI Upp","P-value"),coe)
      coe<-cbind(rownames(coe),coe)
      t1<-c("</br>Ordinary logistic regression</br><table border=3>")
      t1<-c(t1,mat2htmltable(coe),"</table></br>")
    } else {t1<-NA}
    return(t1)
  }
  nnetpred<-function(mdl,d) {
    m1<-apply(d,2,function(z) mean(z,na.rm=TRUE))
    m2<-apply(d,2,function(z) levels(factor(z))[1])
    m1[xlv>0]<-m2[xlv>0]
    nx<-ncol(d); nn<-nrow(d)
    tmpd0<-t(matrix(rep(as.numeric(unlist(m1)),nn),nrow=nx))
    p.xcatg<-NA
    for (i in 1:nx) {
      if (xlv[i]==0) {
        tmpd<-cbind(d[,i],tmpd0[,-i]); 
        colnames(tmpd)<-c(xvname[i],xvname[-i]);
        pp<-predict(mdl,newdata=tmpd,"probs");       
        lpp<-cbind(i,xb[i],rep(d[,i],ncol(pp)),melt(pp)[,-1])
      } else {
        tmpd1<-as.numeric(levels(factor(d[,i])));tmpd<-cbind(tmpd1,tmpd0[1:length(tmpd1),-i]);
        colnames(tmpd)<-c(xvname[i],xvname[-i]);
        pp<-predict(mdl,newdata=tmpd,"probs"); 
        rownames(pp)<-tmpd1
        lpp<-cbind(i,xb[i],melt(pp))
        tmp.xcatg<-cbind(paste(xvname[i],"=",rownames(pp)),format(round(pp,5),nsmall=5))
        if (is.na(p.xcatg[1])) {p.xcatg<-tmp.xcatg;} else {p.xcatg<-rbind(p.xcatg,tmp.xcatg);} 
      }
      colnames(lpp)<-c("x.numb","x.var","x.value","y.level","probability")
      if (i==1) {wpp<-lpp;} else {wpp<-rbind(wpp,lpp);}
    }
    if (!is.na(p.xcatg[1])) p.xcatg<-rbind(c("Sub-group",paste("P(",colvname,"=",colv.lv,")",sep="")),p.xcatg)
    return(list(p.xcatg,wpp))
  }
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  allname<-c(colvname,xvname,bvar,"weights");
  allname<-allname[!is.na(allname)];
  WD<-WD[,allname]
  rm(weights)
  ofname1<-ofname
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  } else {nbg<-1;}
  colv.lv<-levels(factor(colv));
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
  colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
  xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  xname<-xvname; nx<-length(xvname)
  xname[xlv>1]<-paste("factor(",xname[xlv>1],")",sep="")
  fml<-paste("factor(",colvname,")~",paste(xname,collapse="+"),sep="")
  mgam <- 0
  if (sum(sxf == "S")>0) {
    mgam <- 1
    k <- length(colv.lv) - 1
    WD$tmp.y <- NA
    for (s in (1:length(colv.lv))) {
      s.row <- WD[,colvname] == as.numeric(colv.lv[s])
      s.row[is.na(s.row)] <- FALSE
      WD[s.row, "tmp.y"] <- s-1
    }
    xname[sxf=="S"]<-paste("s(",xname[sxf=="S"],")",sep="")
    fml1 <- formula(paste("tmp.y ~", paste(xname,collapse="+")))
    fml2 <- formula(paste("~", paste(xname,collapse="+")))
    fmls <- list()
    fmls[[1]] <- fml1
    for (s in 2:k) fmls[[s]] <- fml2 
  }
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,"<h2>Multinomial Logistic Regression</h2>")
  w<-c(w,"</br>Outcome:", colvb, "</br>")
  sink(paste(ofname1,".lst",sep=""))
  for (i in (1:nbg)) {
    if (nbg>1) {
      tmpd<-WD[bv==bv.lv[i],];
      w<-c(w,paste("</br>",bvb,":",bv.lb[i],"</br>"));
      print(paste(bvar,"=",bv.lv[i]))
    } else {tmpd<-WD;} 
    a1<-mlogistic(tmpd)
    w<-c(w,a1[[1]])
    if (mgam==0) w<-c(w,npolr2table(tmpd))
    if (i==1) {wdd<-a1[[2]];} else {wdd<-rbind(wdd,a1[[2]]);}
    if (mgam == 0) {
      if (nbg>1) {
        tmp2<-cbind(a1[[3]],bv.lv[i],paste(bvb,":",bv.lb[i]));
        colnames(tmp2)<-c(colnames(a1[[3]]), bvar,"strata");
        if (i==1) {wdd2<-tmp2;} else {wdd2<-rbind(wdd2,tmp2);}
      } else {wdd2<-a1[[3]];}
    }
  }
  sink()
  write.table(wdd,file=paste(ofname1,"_pred.xls",sep=""),row.names=FALSE, col.names=TRUE, sep="\t",append=FALSE,quote=FALSE)
  if (mgam==0) {
    write.table(wdd2,file=paste(ofname1,"_xprob.xls",sep=""),row.names=FALSE, col.names=TRUE, sep="\t",append=FALSE,quote=FALSE)
    for (i in (1:nx)) {
      tmpd<-wdd2[wdd2[,1]==i,]
      for (j in (1:length(colv.lv))) {tmpd[tmpd[,4]==colv.lv[j],4]<-paste(colvb,":",colv.lb[j],sep="");}
      if (xlv[i]>0) {
        xi.lv<-levels(factor(xv[,i]));
        xi.lb<-vlabelZ[match(paste(xvname[i],xi.lv,sep="."),vnameZ)]
        xi.lb[is.na(xi.lb)]<-xi.lv[is.na(xi.lb)]
        #for (j in (1:length(xi.lv))) {tmpd[tmpd[,3]==xi.lv[j],3]<-paste(xb[i],":",xi.lb[j],sep="");}
      }
      if (nbg>1) {
        if (xlv[i]>0) {
          tmpdf<-data.frame(factor(tmpd[,3]),tmpd[,5],factor(tmpd[,4]),factor(tmpd[,7]))
          names(tmpdf)<-c("xcatg","Probability","Outcome","g")
          p<-ggplot(tmpdf, aes(x=xcatg,y=Probability,fill=Outcome))+ xlab(xb[i])+ geom_bar(stat="identity")+ geom_line()+ facet_grid(g~., scales="free")
        } else {
          tmpdf<-cbind(tmpd[,c(3,5,4,7)]); colnames(tmpdf)<-c("xval","Probability","g","strata")
          p<-ggplot(tmpdf, aes(x=xval,y=Probability,colour=strata))+ xlab(xb[i])+ ylab("Probability")+ geom_line()+ facet_grid(g~., scales="free")
        }
      } else {
        if (xlv[i]>0) {
          tmpdf<-data.frame(xcatg<-factor(tmpd[,3]),Probability<-tmpd[,5],ylevel<-factor(tmpd[,4]))
          names(tmpdf)<-c("xcatg","Probability","Outcome")
          p<-ggplot(tmpdf,aes(x=xcatg,y=Probability,fill=Outcome))+xlab(xb[i])+geom_bar(stat="identity")
        } else {
          tmpdf<-cbind(tmpd[,c(3,5,4)]); colnames(tmpdf)<-c("xval","Probability","g")
          p<-ggplot(tmpdf, aes(x=xval,y=Probability))+ xlab(xb[i])+ ylab("Probability")+ geom_line()+ facet_grid(g~., scales="free")
        }
      }
      try(ggsave(p, width=4,height=4, filename = paste(ofname1,"_",xvname[i],".pdf",sep="")))
      try(ggsave(p, width=4,height=4, filename = paste(ofname1,"_",xvname[i],".png",sep=""))) 
    }
  }
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)