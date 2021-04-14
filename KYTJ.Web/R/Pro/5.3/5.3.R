  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0503"
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
  bparm1 <- as.character(pa[11,1])
  bparm2 <- as.character(pa[12,1])
  bparm3 <- as.character(pa[13,1])
  sjparm1 <- as.character(pa[14,1])
  sjparm2<- as.character(pa[15,1])  
  
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
  ibs <- as.numeric(bparm1)
  ibn <- unlist(strsplit(bparm2,"[|]"))
  ibv <- unlist(strsplit(bparm3,"[|]"))
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
  
  if(ibs==1)  {
  vname<-c("_N_","_STAT_","_TOTAL_",iyn,ixn,ibn,isjn)
  vlabel<-c("样本量(%)","统计量","合计",iyv,ixv,ibv,isjv)
  }else {
  vname<-c("_N_","_STAT_","_TOTAL_",iyn,ixn,isjn)
  vlabel<-c("样本量(%)","统计量","合计",iyv,ixv,isjv)
  }

  library(geepack,lib.loc=R.LibLocation)
  library(nortest,lib.loc=R.LibLocation)

  ofname<-"5_3"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"广义估计方程";
  WD<-WD[order(WD[,isjn]),]
  attach(WD)

  gee.SUBJ<-idata[,isjn];
  subjvname<-c(isjn);
  gee.TYPE<-"independence"

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
  
  if(ibs==1)  {
    bv<-idata[,ibn[1]]; bvar<-c(ibn[1]);bvname<-c(ibn[1]); 
  }else {
    bv<-NA; bvar<-NA; 
  }

  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## geepack nortest ##R package##;
  chkformula <- function(y,xv,xvf,subset,wd) {
    wdtmp<-wd[subset,c(y,xv)]
    wdtmp<-wdtmp[apply(is.na(wdtmp),1,sum)==0,]
    wdtmpS<-apply(wdtmp,2,isSingular)
    if (wdtmpS[1]) {print(paste("Only had one level: ", y)); return ("")}
    if (sum(wdtmpS[-1])>= (length(wdtmpS)-1)) {print(paste("Only had one level: ", paste(xv,collapse=" ")));return ("")}
    if (nrow(wdtmp)<5) {print("Less than 5 records availabel"); return ("")}   
    if (!wdtmpS[1] & sum(wdtmpS[-1])<length(xv) ) {
      xvx<-xv[!wdtmpS[-1]]
      if (length(xvf)==length(xv)) {
        xvfx<-xvf[!wdtmpS[-1]][1:length(xvx)]
        xvx[xvfx==1]<-paste("factor(",xvx[xvfx==1],")",sep="")
      }
      return (paste(y,"~",paste(xvx,collapse="+")))
    }
  }
  isSingular<-function(mx) return (min(mx,na.rm=TRUE) == max(mx,na.rm=TRUE))
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  gee2htmltable<-function(mdl,w) {
    if (!(substr(mdl[[1]][1],1,5)=="Error")) {
      gs<-summary(mdl)
      coe<-gs$coefficients
      colp<-ncol(coe)
      coep<-coe[,colp]
      if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
        coe<-coe[,-colp]
        cnames<-c(colnames(coe),"exp(coef)","95%CI low","95%CI upp","P.value")
        coe<- cbind(coe, exp(coe[,1]), exp(coe[,1]-1.96*coe[,2]), exp(coe[,1]+1.96*coe[,2]),coep)
      }
      if (gs$family[[2]]=="identity") {
        coe<-coe[,-colp]
        cnames<-c(colnames(coe),"95%CI low","95%CI upp","P.value")
        coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2],coep)
      }
      oo1<-cbind(c("",rownames(coe)),rbind(cnames,round(coe,dec)))
      p1<-c("Number of cluster:",length(gs$clusz))
      p2<-c("Maximum cluster size:", max(gs$clusz))
      p3<-c("Correlation structure:",gs$corstr)
      p4<-c("Number of observations used:", length(gs$deviance.resid))
      norp<-round(pearson.test(mdl$residuals)$p.value,4)
      if (norp==0) norp<-"<0.0001"
      p41<-c("residuals SD",paste(round(sd(mdl$residuals),4),"(pearson chi-square normality test P=", norp,")"))
      oo2<-rbind(p1,p2,p3,p4,p41)
      w<-c(w,"</br><table border=3>", mat2htmltable(oo1), "</table>")
      w<-c(w,"</br><table border=3>", mat2htmltable(oo2), "</table>")
    } else {
      w<-c(w,"</br></br>Error")
    }
    return(w)
  }
  setgee<-function(fml,yd) {
    if (yd=="gaussian" | yd=="normal") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="gaussian",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="bin" | yd=="binomial") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="binomial",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="poisson") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="poisson",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="gamma") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="Gamma",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="inverse.gaussian") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="inverse.gaussian",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="quasi") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="quasi",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="quasibinomial") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="quasibinomial",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="quasipoisson") md<-try(geeglm(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, family="quasipoisson",weights=wd$weights,data=wd,na.action=na.omit))
    if (yd=="nb") md<-try(geeglm.nb(formula(fml),id=gee.SUBJ,corstr=gee.TYPE, weights=wd$weights,data=wd,na.action=na.omit))
    return(md)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  ny<-ncol(yv); yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
  dd<-cbind(yv,xv,weights);  colnames(dd)<-c(yvname,xvname,"weights");
  if (!is.na(bvar)) {dd<-cbind(dd,bv); colnames(dd)<-c(yvname,xvname,"weights",bvar);}
  dd<-cbind(dd,gee.SUBJ)
  dd.tmp<-cbind(dd[,-(1:ny)],1)
  cmp<-(apply(is.na(dd.tmp),1,sum)==0); dd<-dd[cmp,]; rm(dd.tmp)
  ord<-order(dd[,ncol(dd)]); dd<-dd[ord,]
  detach(WD); rm(WD); WD<-as.data.frame(dd); attach(WD)
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  xvstr<-""
  if (!is.na(xvname[1])) {
    xv1<-xvname; xv1[xlv>2]<-paste("factor(",xv1[xlv>2],")",sep="")
    xvstr<-paste(xv1,collapse="+")
  }
  if (sum(sxf=="S")>0 & !is.na(bvar)) xintrstr<-paste(paste(xv1[sxf=="S"],paste("factor(",bvar,")",sep=""),sep="*"),collapse="+")
  
  
  
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,"<h2>广义估计方程</h2>")
  for (i in (1:ny)) {
    wd<-WD
    fml<-paste(yvname[i],"~",xvstr,sep="")
    fml0<-fml
    if (!is.na(bvar)) fml0<-paste(fml,"+factor(", bvar, ")",sep="")
    if (sum(sxf=="S")>0 & !is.na(bvar)) fmlx<-paste(fml0,xintrstr,sep="+")
    w<-c(w,"</br>结局变量:", yb[i])
    w<-c(w,"</br>变量分布与联系函数:", ydist[i])
    w<-c(w,"</br>模型:", fml)
    if (!is.na(bvar)) {
      for (b in (1:nbg)) {
        rm(wd); wd<-WD[(bv==bv.lv[b]),]
        w<-c(w,paste("</br></br>亚组分析:", bvar, "=", bv.lb[b]))
        tmp.glm<-setgee(fml,ydist[i]); w<-gee2htmltable(tmp.glm,w)
        png(paste(ofname,"_",yvar[i],"_",bvname,bv.lv[b],"_resid.png",sep=""),width=720,height=560)
        plot(tmp.glm$fitted.values,tmp.glm$residuals); dev.off()
      }
      w<-c(w,"</br></br>合计:")
    }  
    rm(wd); wd<-WD;
    tmp.mdl<-setgee(fml0,ydist[i])  
    w<-gee2htmltable(tmp.mdl,w);
    png(paste(ofname,yvar[i],"resid.png",sep="_"),width=720,height=560)
    plot(tmp.mdl$fitted.values,tmp.mdl$residuals); dev.off()
    if (!is.na(bvar) & sum(sxf=="S")>0) {
      w<-c(w,paste("</br></br>分组因素与选择的自变量交互作用分析：", bvb, "with", paste(xb[sxf=="S"], collapse=" ")))
      tmp.mdl<-setgee(fmlx,ydist[i]); w<-gee2htmltable(tmp.mdl,w);
    }
  }    
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)