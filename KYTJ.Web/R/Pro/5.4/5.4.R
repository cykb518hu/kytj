  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0504"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  yparm1 <- as.character(pa[1,1])
  yparm2 <- as.character(pa[2,1])
  yparm3 <- as.character(pa[3,1])
  ydist <- as.character(pa[4,1]) 
  ylink <- as.character(pa[5,1])  
  sparm1 <- as.character(pa[6,1])
  sparm2 <- as.character(pa[7,1])
  sparm3 <- as.character(pa[8,1])
  xparm1 <- as.character(pa[9,1])
  xparm2 <- as.character(pa[10,1])
  xparm3 <- as.character(pa[11,1])
  xparm4 <- as.character(pa[12,1])
  xparm5 <- as.character(pa[13,1])
  sjparm1 <- as.character(pa[14,1])
  sjparm2<- as.character(pa[15,1])  
  
  iyn <- yparm1
  iyv <- yparm2
  iys <- as.numeric(yparm3)
  iydist <- ydist
  iylink <- ylink
  isn <- unlist(strsplit(sparm1,"[,]"))
  isv <- unlist(strsplit(sparm2,"[,]"))
  isdf <- as.numeric(unlist(strsplit(sparm3,"[,]")))
  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
  isjn <- as.character(sjparm1)
  isjv <- as.character(sjparm2)
  
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
  
  vname<-c("_N_","_STAT_","_TOTAL_",iyn,isn,ixn,isjn)
  vlabel<-c("样本量(%)","统计量","合计",iyv,isv,ixv,isjv)


  library(mgcv,lib.loc=R.LibLocation)
  library(nlme,lib.loc=R.LibLocation)
  library(lme4,lib.loc=R.LibLocation)
  library(gdata,lib.loc=R.LibLocation)
  library(nortest,lib.loc=R.LibLocation)

  ofname<-"5_4"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"广义相加混合模型"; 
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
  
  sp <- str_count(sparm1,",")+1
  sv<-cbind(idata[,1])
  for (s2 in (1:sp)) {
    u1<-isn[s2]
    sv<-cbind(sv,idata[,u1])
    s2=s2+1
  }
  colnames(sv)<-c(0,isn)
  sv<-sv[,-1]
  svname<-c(isn); 
  svar<-c(isn); 
  sdf<-c(isdf); 
  slv<-c(isdf); 
  
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-idata[,isjn]; vname.start<-c(isjn); 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(1,NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## mgcv nlme lme4 gdata nortest ##R package##;
  ##R package## mgcv nlme lme4 gdata nortest ##R package##;
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
  printTxt<-function(txt,cname,rname) {
    tmp<-as.matrix(txt,ncol=1); rname1<-rname
    if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
    colnames(tmp)=cname;  rownames(tmp)=rname1; print(tmp,quote=F)
  }
  getNumber<-function(str, n) {
    str<-substr(str,2,nchar(str)-1)
    for (i in (1:nchar(str))) {if (substr(str,i,i)==",") {p=i; break}; }
    ifelse(n==1,return(substr(str,1,p-1)),return(substr(str,p+1,nchar(str))))
  }
  
  legLocate<-function(x,y) {
    xmin<-min(x,na.rm=TRUE); xmax<-max(x,na.rm=TRUE)
    ymin<-min(y,na.rm=TRUE); ymax<-max(y,na.rm=TRUE)
    yoff<-(ymax-ymin); tmp<-table(cut(x,3),cut(y,4))
    tmp.r=which.min(tmp[,4]);tmp.c=4
    if (tmp[2,1]==0) {tmp.r=2;tmp.c=1}
    if (tmp[1,1]==0) {tmp.r=1;tmp.c=1}
    if (tmp[3,1]==0) {tmp.r=3;tmp.c=1}
    if (tmp[2,4]==0) {tmp.r=2;tmp.c=4}
    if (tmp[1,4]==0) {tmp.r=1;tmp.c=4}
    if (tmp[3,4]==0) {tmp.r=3;tmp.c=4}
    pos.y<-colnames(tmp)[tmp.c];  pos.x<-rownames(tmp)[tmp.r];  pct<-0.15
    if (tmp.c==4) {
      if (min(tmp[,4])>0) {pct<-0.3}
      ymax<-ymax+yoff*pct; legy<-ymax;ymin<-ymin-yoff*0.1
    } 
    if (tmp.c==1) {
      if (min(tmp[,1])>0) {pct<-0.3}
      legy<-as.numeric(getNumber(pos.y,2));ymin<-ymin-yoff*pct;ymax=ymax+yoff*0.1
    } 
    legx<-as.numeric(getNumber(pos.x,1))
    return(cbind(xmin,xmax,ymin,ymax,legx,legy))
  }
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  
  lme2htmltable<-function(mdl) {
    gm<-summary(mdl)
    if (is.list(gm)) {
      coe<-gm$tTable
      cnames<-c(colnames(coe),"95%CI.low","95%CI.upp")
      coe<-cbind(coe,coe[,1]-1.96*coe[,2],coe[,1]+1.96*coe[,2])
      coe[,c(1,2,4,6,7)]<-numfmt(coe[,c(1,2,4,6,7)],dec)
      coe[,5]<-pvformat(coe[,5],dec)
      oo1<-cbind(c("",rownames(coe)),rbind(cnames,coe))
      cor<-gm$corFixed
      oo2<-cbind(c("",rownames(cor)),rbind(colnames(cor),numfmt(cor,dec)))
      oo3<-rbind(c("Method:", gm$method),cbind(c("AIC:","BIC:","Log likelihood:"),numfmt(c(gm$AIC,gm$BIC,gm$logLik),dec)))
      tmp<-numfmt(rbind(apply(mdl$residuals,2,mean),apply(mdl$residuals,2,sd)),4)
      tmp1<-pvformat(apply(mdl$residuals,2,function(x) pearson.test(x)$p.value),4);
      tmp.int<-mdl$coefficients$random$subj.ID
      tmp2<-c("Random intercept", pvformat(c(mean(tmp.int),sd(tmp.int),pearson.test(tmp.int)$p.value),4))
      oo4<-cbind(c("","Mean","SD","Pearson chi-square normality test"),rbind(colnames(mdl$residuals),tmp,tmp1),tmp2)
      oo<-c("</br>Fixed effects<table border=3>",mat2htmltable(oo1),"</table>")
      oo<-c(oo,"</br>Correlation<table border=3>",mat2htmltable(oo2),"</table>")
      oo<-c(oo,"</br>Model info.<table border=3>",mat2htmltable(oo3),"</table>")
      oo<-c(oo,"</br>Residuals<table border=3>",mat2htmltable(oo4),"</table>")
    } else {
      oo<-""
    }
    return(oo)
  }
  
  setgamm<-function(fml,ydist) {
    if (gamm==1) {
      if (rdmv==0) {
        if (ydist=="gaussian") mdl<-try(gamm(formula(fml),random=list(subj.ID=~1),weights=wdd$weights,data=wdd,family=gaussian))
        if (ydist=="binomial") {
          if (smooth==0) {
            tmp.fml<-paste(fml,"+(1|subj.ID)")
            mdl<-try(glmer(formula(tmp.fml),weights=wdd$weights,data=wdd,family=binomial))
          } else {
            mdl<-try(gamm(formula(fml),random=list(subj.ID=~1),weights=wdd$weights,data=wdd,family=binomial))
          }
        }
        if (ydist=="poisson") mdl<-try(gamm(formula(fml),random=list(subj.ID=~1),weights=wdd$weights,data=wdd,family=poisson))
        if (ydist=="gamma") mdl<-try(gamm(formula(fml),random=list(subj.ID=~1),weights=wdd$weights,data=wdd,family=Gamma))
      } else {
        if (rdmint==0) {
          if (ydist=="gaussian") mdl<-try(gamm(formula(fml),random=list(subj.ID=~rdm.var),weights=wdd$weights,data=wdd,family=gaussian))
          if (ydist=="binomial") {
            tmp.fml<-paste(fml,"+(rdm.var|subj.ID)")
            mdl<-try(glmer(formula(tmp.fml),weights=wdd$weights,data=wdd,family=binomial))
          }
          if (ydist=="poisson") mdl<-try(gamm(formula(fml),random=list(subj.ID=~rdm.var),weights=wdd$weights,data=wdd,family=poisson))
          if (ydist=="gamma") mdl<-try(gamm(formula(fml),random=list(subj.ID=~rdm.var),weights=wdd$weights,data=wdd,family=Gamma))
        } else {
          if (ydist=="gaussian") mdl<-try(gamm(formula(fml),random=list(subj.ID=~1+rdm.var),weights=wdd$weights,data=wdd,family=gaussian))
          if (ydist=="binomial") {
            tmp.fml<-paste(fml,"+(1+rdm.var|subj.ID)")
            mdl<-try(glmer(formula(tmp.fml),weights=wdd$weights,data=wdd,family=binomial))
          }
          if (ydist=="poisson") mdl<-try(gamm(formula(fml),random=list(subj.ID=~1+rdm.var),weights=wdd$weights,data=wdd,family=poisson))
          if (ydist=="gamma") mdl<-try(gamm(formula(fml),random=list(subj.ID=~1+rdm.var),weights=wdd$weights,data=wdd,family=Gamma))
        }
      }
    } else {
      wdd$v <- 1/wdd$weights^2;
      if (rdmv==0) {
        mdl<-try(lme(formula(fml),random=list(subj.ID=~1),weights=varFixed(~v),data=wdd))
      } else {
        if (rdmint==0) {
          mdl<-try(lme(formula(fml),random=list(subj.ID=~rdm.var),weights=varFixed(~v),data=wdd))
        } else {
          mdl<-try(lme(formula(fml),random=list(subj.ID=~1+rdm.var),weights=varFixed(~v),data=wdd))
        }
      }
    }
    return (mdl)
  }
  gamm2pngs<-function(mdl,yi,bvvstr) {
    pred<-predict.gam(mdl,type="terms",se.fit=TRUE)
    mfit<-NA; sfit<-NA;
    cnamefit<-colnames(pred$fit)
    for (k in (1:ns)) {
      svnamek1<-paste("s(",svname[k],")",sep="")
      svk.colmns<-which(substr(cnamefit,1,nchar(svnamek1))==svnamek1)
      mfit<-cbind(mfit,apply(cbind(0,pred$fit[,svk.colmns]),1,sum));
      sfit<-cbind(sfit,apply(cbind(0,pred$se.fit[,svk.colmns]),1,sum));
    }
    tmp.cname<-svname;
    if (nx>0) {
      mfit<-cbind(mfit,pred$fit[,xv1]); sfit<-cbind(sfit,pred$se.fit[,xv1]); 
      tmp.cname<-c(tmp.cname,xvname);
    }
    mfit<-matrix(mfit[,-1],ncol=length(tmp.cname)); colnames(mfit)<-paste(tmp.cname,".fit",sep="");
    sfit<-matrix(sfit[,-1],ncol=length(tmp.cname)); colnames(sfit)<-paste(tmp.cname,".se",sep="");
    if (mdl$family[2]=="logit") {
      tmp.sh<-mean(mdl$fitted.values); tmp.sh<-log(tmp.sh/(1-tmp.sh))-apply(mfit,2,mean)
      mfit<-mfit+matrix(rep(tmp.sh,nrow(mfit)),nrow=nrow(mfit),byrow=TRUE)
      mfit.low<-mfit-1.96*sfit; mfit.low<-matrix(exp(mfit.low)/(1+exp(mfit.low)),ncol=length(tmp.cname))
      mfit.upp<-mfit+1.96*sfit; mfit.upp<-matrix(exp(mfit.upp)/(1+exp(mfit.upp)),ncol=length(tmp.cname))
      mfit<-matrix(exp(mfit)/(1+exp(mfit)),ncol=length(tmp.cname))
      colnames(mfit.low)<-paste(tmp.cname,".low",sep="");
      colnames(mfit.upp)<-paste(tmp.cname,".upp",sep="");
      colnames(mfit)<-paste(tmp.cname,".fit",sep="");
      ww<-cbind(wdd,mfit,mfit.low,mfit.upp)
    } else if (mdl$family[2]=="log") {
      tmp.sh<-mean(mdl$fitted.values); tmp.sh<-log(tmp.sh)-apply(mfit,2,mean)
      mfit<-mfit+matrix(rep(tmp.sh,nrow(mfit)),nrow=nrow(mfit),byrow=TRUE)
      mfit.low<-mfit-1.96*sfit; mfit.low<-matrix(exp(mfit.low),ncol=length(tmp.cname))
      mfit.upp<-mfit+1.96*sfit; mfit.upp<-matrix(exp(mfit.upp),ncol=length(tmp.cname))
      mfit<-matrix(exp(mfit),ncol=length(tmp.cname))
      colnames(mfit.low)<-paste(tmp.cname,".low",sep="");
      colnames(mfit.upp)<-paste(tmp.cname,".upp",sep="");
      colnames(mfit)<-paste(tmp.cname,".fit",sep="");
      ww<-cbind(wdd,mfit,mfit.low,mfit.upp)
    } else if (mdl$family[2]=="identity") {
      tmp.sh<-mean(mdl$fitted.values)-apply(mfit,2,mean)
      mfit<-mfit+matrix(rep(tmp.sh,nrow(mfit)),nrow=nrow(mfit),byrow=TRUE)
      ww<-cbind(wdd,mfit,sfit)
    } else {
      ww<-cbind(wdd,mfit,sfit)
    }
    xf<-paste(ofname,yvname[yi],"gamm.xls", sep="_")
    if (bvvstr>"") xf<-paste(ofname,yvname[yi],bvvstr,"gamm.xls", sep="_")
    write.table(ww,file=xf,row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE,quote=FALSE)
    px<-c(20,1:9); gg<-"<table><tr>";
    for (k in (1:ns)) {
      cname1<-paste(svname[k],".fit",sep="");  y.tmp<-mfit[,cname1]; y.org<-ww[,yvname[yi]]
      if (mdl$family[2]=="logit" | mdl$family[2]=="log") {
        cname2<-paste(svname[k],".low",sep=""); y.low<-mfit.low[,cname2]
        cname3<-paste(svname[k],".upp",sep=""); y.upp<-mfit.upp[,cname3]
      } else {
        cname2<-paste(svname[k],".se",sep=""); se.tmp<-sfit[,cname2]; 
        y.low<-y.tmp-1.96*se.tmp; y.upp<-y.tmp+1.96*se.tmp
      }
      x.tmp<-ww[,svname[k]]; 
      pngf<-paste(ofname,yvar[yi],svar[k],"smooth.png",sep="_")
      pngf0<-paste(ofname,yvar[yi],svar[k],"smooth1.png",sep="_")
      if (bvvstr>"") pngf<-paste(ofname,yvar[yi],svar[k],bvvstr,"smooth.png",sep="_")
      if (is.na(colvname)) {
        if (is.na(parm[1])) {tmp.col<-rainbow(2);} else {tmp.col<-rep("black",2);}
        xy<-legLocate(c(x.tmp,x.tmp),c(y.low,y.upp))
        png(pngf,width=720,height=560)
        plot(y.tmp~x.tmp,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[1],type="p", pch=20, ylab="", xlab="")
        par(new=TRUE); 
        plot(y.low~x.tmp,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[2], type="p", pch=".", ylab="", xlab="")
        par(new=TRUE); 
        plot(y.upp~x.tmp,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[2], type="p", pch=".", ylab=yb[yi], xlab=sb[k])
        dev.off()
        png(pngf0,width=720,height=560)
        tmp.ord<-order(x.tmp);  x.tmp0<-x.tmp[tmp.ord]; 
        y.tmp0<-y.tmp[tmp.ord];y.low0<-y.low[tmp.ord];y.upp0<-y.upp[tmp.ord]
        plot(y.tmp0~x.tmp0,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[1],type="l", lty=1, lwd=2, ylab="", xlab="")
        par(new=TRUE); 
        plot(y.low0~x.tmp0,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[2], type="l", lty=3, lwd=1, ylab="", xlab="")
        par(new=TRUE); 
        plot(y.upp0~x.tmp0,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[2], type="l", lty=3, lwd=1, ylab=yb[yi], xlab=sb[k])
        dev.off()
        rm(tmp.ord,y.tmp0,x.tmp0,y.low0,y.upp0)
        xy<-legLocate(x.tmp,y.org)
        pngf1<-paste(ofname,yvar[yi],svar[k],"scatter.png",sep="_")
        if (bvvstr>"") pngf1<-paste(ofname,yvar[yi],svar[k],bvvstr,"scatter.png",sep="_")
        png(pngf1,width=720,height=560)
        plot(y.tmp~x.tmp,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[1], type="p", pch=20, ylab="", xlab="")
        par(new=TRUE); 
        plot(y.org~x.tmp,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),type="p",pch=1,cex=0.5, ylab=yb[yi], xlab=sb[k])    
        dev.off()
      } else {
        if (is.na(parm[1])) {tmp.col<-rainbow(ncg);} else {tmp.col<-rep("black",ncg);}
        xy<-legLocate(x.tmp,y.tmp)
        png(pngf,width=720,height=560)
        for (b in (1:ncg)) {
          y0<-y.tmp[ww[,colvname]==colv.lv[b]]; x0<-x.tmp[ww[,colvname]==colv.lv[b]]
          if (b>1) par(new=TRUE)
          plot(y0~x0,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[b], type="p", pch=px[b], ylab=yb[yi], xlab=sb[k])
        }
        legend(xy[5],xy[6],colv.lb,pch=px[1:ncg],bty="n",col=tmp.col)
        dev.off()
        png(pngf0,width=720,height=560)
        for (b in (1:ncg)) {
          y0<-y.tmp[ww[,colvname]==colv.lv[b]]; x0<-x.tmp[ww[,colvname]==colv.lv[b]]
          tmp.ord<-order(x0); x00<-x0[tmp.ord]; y00<-y0[tmp.ord];
          if (b>1) par(new=TRUE)
          plot(y00~x00,ylim=c(xy[3],xy[4]),xlim=c(xy[1],xy[2]),col=tmp.col[b], type="l", lty=b, lwd=2, ylab=yb[yi], xlab=sb[k])
          rm(tmp.ord,x00,y00)
        }
        legend(xy[5],xy[6],colv.lb,lty=(1:ncg),bty="n",col=tmp.col)
        dev.off()
      }
      gg<-c(gg,"<td>",yb[yi]," vs. ",sb[k],"</br><a href=\"",pngf,"\" target=_BLANK><img src=\"",pngf,"\" width=320,height=320></a></td>")
    }
    gg<-c(gg,"</tr></table>")
    return(gg)
  }
  TMP.ID=(1:nrow(WD)); WD.0<-cbind(TMP.ID,WD)
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  rv<-v.stop; rvname<-vname.stop; 
  subj.ID<-v.start; 
  rdmint<-parm[1]; nx<-0
  if (!is.na(rvname)) {
    rlv<-length(levels(factor(rv)));
    if (is.na(xvname[1])) {
      xv<-matrix(rv,ncol=1); xvname<-rvname; xlv<-rlv;
    } else if (is.na(match(rvname, xvname))) {
      xv<-cbind(xv,rv); xvname<-c(xvname,rvname); xlv<-c(xlv,rlv);
    }
    nx<-length(xvname)
  }
  ny<-ncol(yv);
  yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
  xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  gamm0<-0; smooth<-0;
  svstr<-""; nsterms<-0; 
  if (!is.na(svname[1])) {
    sb<-vlabelV[match(svname,vnameV)]; sb[is.na(sb)]<-svname[is.na(sb)]
    ns<-length(svname); nsterms<-ns;
    sv1<-paste("s(",svname,",bs='cr'",sep="") 
    sdf1<-paste(",k=",sdf); sdf1[sdf==0]<-"";
    sv1<-paste(sv1,sdf1)
    if (!is.na(colvname)) {
      sv1<-paste(sv1,",by=factor(",colvname,")",sep="")
      colv.lv<-levels(factor(colv)); ncg<-length(colv.lv); 
      colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
      colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
      colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
      nsterms<-nsterms*ncg;
    }
    svstr<-paste(paste(sv1,")",sep=""),collapse="+")
    gamm0<-1; smooth<-1
  }
  xvstr<-""
  if (!is.na(xvname[1])) {
    xv1<-xvname; xv1[xlv>2]<-paste("factor(",xv1[xlv>2],")",sep="")
    xvstr<-paste(xv1,collapse="+")
  }
  if (sum(sxf=="S")>0 & !is.na(bvar)) xintrstr<-paste(paste(xv1[sxf=="S"],paste("factor(",bvar,")",sep=""),sep="*"),collapse="+")
  if (svstr>"" & xvstr>"") sxstr<-paste(svstr,"+",xvstr,sep="");
  if (svstr>"" & xvstr=="") sxstr<-svstr;
  if (svstr=="" & xvstr>"") sxstr<-xvstr;
  ofname1<-ofname;
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  
  
  gam2htmltable<-function(mdl) {
    gs<-summary(mdl)
    np<-length(gs$p.coeff)
    coe<-rbind(NA,gs$p.table)
    colp<-ncol(coe)
    coep<-pvformat(coe[,colp],4)
    cnames<-colnames(coe)
    if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
      coe<-coe[,-colp]
      cnames<-c(colnames(coe),"exp(est)","95%区间下限","95%区间上限","P.value")
      coe<- cbind(coe, exp(coe[,1]), exp(coe[,1]-1.96*coe[,2]), exp(coe[,1]+1.96*coe[,2]),coep)
    }
    if (gs$family[[2]]=="identity") {
      coe<-coe[,-colp]
      cnames<-c(colnames(coe),"95%区间下限","95%区间上限","P.value")
      coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2],coep)
    }
    coe[,-ncol(coe)]<-numfmt(coe[,-ncol(coe)],dec)
    oo1<-cbind(c("",rownames(coe[-1,])),rbind(cnames,coe[-1,]))
    oo<-c("</br>线性回归项的作用<table border=3>",mat2htmltable(oo1),"</table>")
    if (!is.null(gs$pTerms.table)) {
      xsq<-gs$pTerms.table
      xsq[,2]<-numfmt(xsq[,2],dec)
      xsq[,3]<-pvformat(xsq[,3],4)
      oo2<-cbind(c("",rownames(xsq)),rbind(colnames(xsq),xsq))
      oo<-c(oo, "</br>线性回归项卡方检验<table border=3>",mat2htmltable(oo2),"</table>")
    }
    stb<-gs$s.table
    stb[,c(1,2,3)]<-numfmt(stb[,c(1,2,3)],dec)
    stb[,4]<-pvformat(stb[,4],4)
    oo3<-cbind(c("",rownames(stb)),rbind(colnames(stb),stb))
    oo<-c(oo, "</br>曲线拟合项的显著性检验<table border=3>",mat2htmltable(oo3),"</table>")
    p0<-c("N:", gs$n)
    p1<-c("Adj. r-square:", numfmt(gs$r.sq,4))
    p2<-c("Deviance explained:", numfmt(gs$dev.expl,4))
    p3<-c("Scale estimate:", numfmt(gs$scale,dec))
    p4<-c("family:", gs$family[[1]])
    p5<-c("link function:", gs$family[[2]])
    oo4<-rbind(p0,p1,p2,p3,p4,p5)
    oo<-c(oo, "</br>Model statistics<table border=3>",mat2htmltable(oo4),"</table>")
    return(oo)
  }
  gamm2html<-function(mdl, xfname, yb1, ydist) {
    oo<-paste("</br></br>结局变量:", yb1, "</br>")
    if (smooth==1) {
      oo<-c(oo,"</br>相加模型结果</br>")
      oo<-c(oo,gam2htmltable(mdl$gam));
      oo<-c(oo,"</br></br>线性混合效应</br>")
      oo<-c(oo,lme2htmltable(mdl$lme));
      for (g in (1:nsterms)) {
        png(paste(xfname,"page",g,"smooth.png",sep="_"),width=720,height=560);
        plot(mdl$gam, select=g, main=yb1)
        dev.off()
      }
      png(paste(xfname,"gamm.png",sep="_"),width=720,height=560);
      plot(mdl$lme$fitted[,"subj.ID"],mdl$lme$residuals[,"subj.ID"],xlab="Fitted value",ylab="Standardized residuals",main=yb1); 
      dev.off()
      png(paste(xfname,"residual.png",sep="_"),width=720,height=560);
      hist(mdl$lme$residuals[,"subj.ID"], plot=T, prob=T, main="残差直方图",xlab="残差")
      lines(density(mdl$lme$residuals[,"subj.ID"],na.rm=TRUE))
      dev.off()    
    } else {
      oo<-paste("</br></br>线性混合效应: Y=", yb1, " distribution:", ydist, "</br>")
      oo<-c(oo,lme2htmltable(mdl));
      if (y.gaussian==1) {
        png(paste(xfname,"lme.png",sep="_"),width=720,height=560);
        plot(mdl$fitted[,"subj.ID"],mdl$residuals[,"subj.ID"],xlab="Fitted value",ylab="Standardized residuals",main=yb1); 
        dev.off()
        png(paste(xfname,"residual.png",sep="_"),width=720,height=560);
        hist(mdl$residuals[,"subj.ID"], plot=T, prob=T, main="残差直方图",xlab="残差")
        lines(density(mdl$residuals[,"subj.ID"],na.rm=TRUE))
        dev.off()    
      }
    }
    return(oo)
  }
  w<-c(w,"<h2>相加混合模型</h2>")
  for (k in (1:ny)) {
    yv1<-yv[,k]; yvname1<-yvname[k]; yb1<-yb[k];
    rdmv<-0
    yres.NAME<-paste(yvname1,c("fixed","subjID"),sep=".")
    tmp.YRES<-matrix(NA,nrow=nrow(WD.0),ncol=2); colnames(tmp.YRES)<-yres.NAME
    WD.0<-cbind(WD.0,tmp.YRES)
    dd<-cbind(weights,TMP.ID,subj.ID,yv1); cname<-c("weights","TMP.ID","subj.ID",yvname1)
    if (!is.na(xvname[1])) {dd<-cbind(dd,xv); cname<-c(cname,xvname);}
    if (!is.na(svname[1])) {dd<-cbind(dd,sv); cname<-c(cname,svname);}
    if (!is.na(bvar)) {dd<-cbind(dd,bv); cname<-c(cname,bvar);}
    if (!is.na(colvname)) {dd<-cbind(dd,colv); cname<-c(cname,colvname);}
    if (!is.na(rvname)) {cname<-c(cname,"rdm.var"); dd<-cbind(dd,rv); rdmv<-1;}
    dd<-dd[apply(is.na(dd),1,sum)==0,]; colnames(dd)<-cname;
    detach(WD); rm(WD);
    WD<-as.data.frame(dd); attach(WD);
    fml<-paste(yvname1,"~",sxstr,sep="")
    if (!is.na(bvar)) fml0<-paste(fml,"+factor(",bvar,")",sep="")
    if (sum(sxf=="S")>0 & !is.na(bvar)) fmlx<-paste(fml0,xintrstr,sep="+")
    if ((ydist[k]!="gaussian") & is.na(svname[1])) {gamm<-1;} else {gamm<-gamm0;}
    y.gaussian<-0; if (ydist[k]=="gaussian") y.gaussian<-1;
    wdd<-WD; 
    xf<-ofname1; if (ny>1) xf<-paste(xf,yvname1,sep="_")
    if (is.na(bvar)) {
      tmp.mdl<-setgamm(fml,ydist[k])
      if (is.list(tmp.mdl)) {
        errStr = substr(tmp.mdl[[1]][1],1,5);
      } else {
        if (is.array(tmp.mdl)) {errStr=substr(tmp.mdl[1],1,5);} else {errStr="";}
      }
      if (is.list(tmp.mdl)) {
        if (errStr!="Error") {
          w<-c(w,gamm2html(tmp.mdl, xf, yb1, ydist[k])); 
          if (smooth==1) w<-c(w,"</br>",gamm2pngs(tmp.mdl$gam,k,""))
          if (smooth==1) {yk.resi<-tmp.mdl$lme$residuals;} else {yk.resi<-tmp.mdl$residuals;}
          WD.0[match(wdd[,"TMP.ID"],WD.0[,"TMP.ID"]),yres.NAME]<-yk.resi[,c("fixed","subj.ID")]
        }
      } else {if (errStr!="Error") w<-c(w,gamm2html(tmp.mdl, xf, yb1, ydist[k]));}
    } else {
      for (b in (1:nbg)) {
        rm(wdd); wdd<-WD[(WD[,bvar]==bv.lv[b]),];
        xf1<-paste(xf,bvar,bv.lv[b],sep="_")
        w<-c(w,paste("</br></br>亚组分析:", bvb, "=", bv.lb[b]))
        tmp.mdl<-setgamm(fml,ydist[k])
        if (is.list(tmp.mdl)) {
          errStr = substr(tmp.mdl[[1]][1],1,5);
        } else {
          if (is.array(tmp.mdl)) {errStr=substr(tmp.mdl[1],1,5);} else {errStr="";}
        }
        if (is.list(tmp.mdl)) {
          if (errStr!="Error") {
            w<-c(w,gamm2html(tmp.mdl, xf1, yb1, ydist[k]));
            if (smooth==1) w<-c(w,"</br>",gamm2pngs(tmp.mdl$gam,k,paste(bvar,bv.lv[b],sep="")))
            if (smooth==1) {yk.resi<-tmp.mdl$lme$residuals;} else {yk.resi<-tmp.mdl$residuals;}
            WD.0[match(wdd[,"TMP.ID"],WD.0[,"TMP.ID"]),yres.NAME]<-yk.resi[,c("fixed","subj.ID")]
          }
        } else {if (errStr!="Error") w<-c(w,gamm2html(tmp.mdl, xf1, yb1, ydist[k]));}
      }
      w<-c(w,paste("</br></br>合计: "))
      rm(wdd); wdd<-WD; tmp.mdl<-setgamm(fml0,ydist[k]); 
      if (is.list(tmp.mdl)) {
        errStr = substr(tmp.mdl[[1]][1],1,5);
      } else {
        if (is.array(tmp.mdl)) {errStr=substr(tmp.mdl[1],1,5);} else {errStr="";}
      }
      if (is.list(tmp.mdl)) {
        if (errStr!="Error") w<-c(w,gamm2html(tmp.mdl, xf, yb1,ydist[k]))     
      } else {if (errStr!="Error") w<-c(w,gamm2html(tmp.mdl, xf, yb1,ydist[k]));}
      
      if (sum(sxf=="S")>0) {
        w<-c(w,paste("</br></br>分组因素与选择的自变量交互作用分析：", bvb, "with", paste(xb[sxf=="S"], collapse=" ")))
        rm(wdd); wdd<-WD; tmp.mdl<-setgamm(fmlx,ydist[k]); 
        if (is.list(tmp.mdl)) {
          errStr = substr(tmp.mdl[[1]][1],1,5);
        } else {
          if (is.array(tmp.mdl)) {errStr=substr(tmp.mdl[1],1,5);} else {errStr="";}
        }
        if (is.list(tmp.mdl)) {
          if (errStr!="Error") w<-c(w,gamm2html(tmp.mdl, xf, yb1, ydist[k]))     
        } else {if (errStr!="Error") w<-c(w,gamm2html(tmp.mdl, xf, yb1, ydist[k]));}
      }
    }
  }
  w<-c(w,"</body></html>")
  if (!is.list(w)) {
    fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)
  } else {
    sink(paste(ofname,".htm",sep=""));
    for (i in (1:length(w))) ifelse(is.vector(w[[i]]), printTxt(w[[i]],"",""), print(w[i]))
    sink()
  }
  write.table(WD.0,file=paste(ofname,"_residuals.xls",sep=""),row.names=FALSE,col.names=TRUE,sep="\t")