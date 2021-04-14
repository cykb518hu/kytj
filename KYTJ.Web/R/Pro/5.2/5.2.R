  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0502"
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

  vname<-c("_N_","_STAT_","_TOTAL_",iyn,isn,ixn)
  vlabel<-c("样本量(%)","统计量","合计",iyv,isv,ixv)
  
  library(mgcv,lib.loc=R.LibLocation)
  library(gdata,lib.loc=R.LibLocation)

  ofname<-"5_2"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"广义相加模型"; 
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
  sv<-sv[,-1]
  colnames(sv)<-isn
  svname<-c(isn); 
  svar<-c(isn); 
  sdf<-c(isdf); 
  slv<-c(isdf); 
  
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## mgcv gdata ##R package##;
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  setgam<-function(fml,yi) {
    if (ydist[yi]=="") ydist[yi]<-"gaussian"
    if (ydist[yi]=="gaussian") mdl<-gam(formula(fml),weights=wd$weights,data=wd, family=gaussian(link="identity"))
    if (ydist[yi]=="binomial") mdl<-gam(formula(fml),weights=wd$weights,data=wd, family=binomial(link="logit"))
    if (ydist[yi]=="poisson") mdl<-gam(formula(fml),weights=wd$weights,data=wd, family=poisson(link="log"))
    if (ydist[yi]=="gamma") mdl<-gam(formula(fml),weights=wd$weights,data=wd, family=Gamma(link="inverse"))
    if (ydist[yi]=="negbin") mdl<-gam(formula(fml),weights=wd$weights,data=wd, family=negbin(c(1,10), link="log"))
    return(mdl)
  }
  savePredict<-function(mdl,wd,save2fname) {
    tmp.pred<-predict.gam(mdl,wd,se.fit=TRUE)
    tmp.xls<-cbind(wd,tmp.pred$fit,tmp.pred$se.fit)
    colnames(tmp.xls)<-c(colnames(wd),"PRED_","PRED.SE_")
    write.table(tmp.xls,file=save2fname,col.names=TRUE,row.names=FALSE,sep="\t")
  }
  gam2htmltable<-function(mdl) {
    gs<-summary(mdl)
    np<-length(gs$p.coeff)
    coe<-rbind(NA,gs$p.table)
    colp<-ncol(coe)
    coep<-coe[,colp]
    cnames<-colnames(coe)
    if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
      coe<-coe[,-colp]
      cnames<-c(colnames(coe),"exp(est)","95%CI low","95%CI upp","P.value")
      coe<- cbind(coe, exp(coe[,1]), exp(coe[,1]-1.96*coe[,2]), exp(coe[,1]+1.96*coe[,2]),coep)
    }
    if (gs$family[[2]]=="identity") {
      coe<-coe[,-colp]
      cnames<-c(colnames(coe),"95%CI low","95%CI upp","P.value")
      coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2],coep)
    }
    oo1<-cbind(c("",rownames(coe[-1,])),rbind(cnames,round(coe[-1,],dec)))
    oo<-c("</br>Linear terms effect<table border=3>",mat2htmltable(oo1),"</table>")
    if (!is.null(gs$pTerms.table)) {
      xsq<-gs$pTerms.table
      oo2<-cbind(c("",rownames(xsq)),rbind(colnames(xsq),round(xsq,dec)))
      oo<-c(oo, "</br>Chi-square tests for linear terms<table border=3>",mat2htmltable(oo2),"</table>")
    }
    stb<-gs$s.table
    oo3<-cbind(c("",rownames(stb)),rbind(colnames(stb),round(stb,dec)))
    oo<-c(oo, "</br>Approximate significance of smooth terms<table border=3>",mat2htmltable(oo3),"</table>")
    p0<-c("N:", gs$n)
    p1<-c("Adj. r-square:", round(gs$r.sq,4))
    p2<-c("Deviance explained:", round(gs$dev.expl,4))
    p3<-c("UBRE score (sp.criterion):", round(gs$sp.criterion,4))
    p4<-c("Scale estimate:", gs$scale)
    p5<-c("family:", gs$family[[1]])
    p6<-c("link function:", gs$family[[2]])
    oo4<-rbind(p0,p1,p2,p3,p4,p5,p6)
    oo<-c(oo, "</br>Model statistics<table border=3>",mat2htmltable(oo4),"</table>")
    return(oo)
  }
  
  
  if (!is.na(svname[1])) {
    vlabelN<-(substr(vlabel,1,1)==" ");
    vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
    vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
    ny=ncol(yv); ns=ncol(sv)
    cname<-c(timevname, svname, colvname, bvar, xvname,"weights")
    cname<-cname[!is.na(cname)]
    dd.tmp<-WD[,cname]
    cmp<-(apply(is.na(dd.tmp),1,sum)==0)
    WD0<-WD[cmp,c(yvname,cname)]
    detach(WD); rm(WD); WD<-WD0; rm(WD0); attach(WD)
    sb<-vlabelV[match(svname,vnameV)]; sb[is.na(sb)]<-svname[is.na(sb)]
    yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
    ssf<-rep(",fx=FALSE", length(svname)); ssf[sdf>0]<-paste(",k=",sdf[sdf>0],sep="")
    sxStr<-paste("s(",svname,ssf,sep="")
    if (!is.na(colvname)) {
      sxStr<-paste(sxStr,",by=factor(", colvname, ")",sep="")
      colv.lv<-levels(factor(colv)); ncg<-length(colv.lv);
      colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
      sxplots<-NA
      for (i in (1:ns)) {sxplots<-c(sxplots,paste(svar[i],"_",colvname,colv.lv,sep=""));}
      sxplots<-sxplots[-1]
    } else {ncg<-1;sxplots<-svar;}
    sxStr<-paste(paste(sxStr,")",sep=""),collapse="+")
    if (!is.na(colvname)) sxStr<-paste(sxStr,"+factor(",colvname,")",sep="")
    
    nx<-0
    if (!is.na(xvname[1])) {
      nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
      xv1<-xvname; xv1[xlv>2]<-paste("factor(",xvname[xlv>2],")",sep="")
      sxStr<-paste(sxStr,"+",paste(xv1,collapse="+"),sep="")
    }
    
    if (!is.na(bvar)) {
      bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
      bv.lv<-levels(factor(bv)); 
      bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
      bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
      nbg<-length(bv.lv)
      sxStr2<-paste(sxStr,"+factor(",bvar,")")
      if (sum(sxf=="S")>0) {
        xintrstr<-paste(paste(xv1[sxf=="S"],paste("factor(",bvar,")",sep=""),sep="*"),collapse="+")
        sxStria<-paste(sxStr2,"+",xintrstr)
      } else {sxStria<-""}
    }
    nterms=ns*ncg*15+nx
    nplot=ns*ncg
  }
  
  
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,"<h2>广义相加模型</h2>")
  if (is.na(svname[1])) {w<-c(w,"未定义曲线拟合变量!");} else {
    for (i in (1:ny)) {
      fml<-paste(yvname[i],"~",sxStr)
      w<-c(w,"</br></br>结局变量:", yb[i])
      w<-c(w,"</br></br>变量分布:", ydist[i])
      w<-c(w,"</br>模型:", fml,"</br>")
      if (is.na(bvar)) {
        wd<-WD; tmp.obs<-nrow(wd)
        if (tmp.obs>nterms) {
          tmp.gam<-setgam(fml,i); w<-c(w,gam2htmltable(tmp.gam))
          for (z in (1:nplot)) {
            png(paste(ofname,yvar[i],sxplots[z],"gamplot.png",sep="_"),width=720,height=560)
            plot(tmp.gam, select=z); dev.off()
          }
          savePredict(tmp.gam,wd,paste(ofname,yvar[i],"pred.xls",sep="_"))
        } else {w<-c(w,paste("</br>N=", tmp.obs, "样本量不够大，可减少曲线拟合变量数重试"));}
      } else {
        for (b in (1:nbg)) {
          rm(wd); wd<-WD[(WD[,bvar]==bv.lv[b]),]; tmp.obs<-nrow(wd)
          w<-c(w,paste("</br>亚组:", bvb, "=", bv.lb[b]))
          if (tmp.obs>nterms) {
            tmp.gam<-setgam(fml,i); w<-c(w,gam2htmltable(tmp.gam))
            for (z in (1:nplot)) {
              png(paste(ofname,yvar[i],bvname,bv.lv[b],sxplots[z],"gamplot.png",sep="_"),width=720,height=560)
              plot(tmp.gam, select=z); dev.off()
            }
            savePredict(tmp.gam,wd,paste(ofname,yvar[i],bv.lv[b],"pred.xls",sep="_"))
          } else {w<-c(w,paste("</br>N=", tmp.obs, "样本量不够大，可减少曲线拟合变量数重试"));}
        }
        fml2<-paste(yvname[i],"~",sxStr2)
        wd<-WD; tmp.obs<-nrow(wd)
        w<-c(w,paste("</br>合计:"))
        if (tmp.obs>nterms) {
          tmp.gam<-setgam(fml2,i); w<-c(w,gam2htmltable(tmp.gam))
          for (z in (1:ns)) {
            png(paste(ofname,yvar[i],sxplots[z],"gamplot.png",sep="_"),width=720,height=560)
            plot(tmp.gam, select=z); dev.off()
          }
          savePredict(tmp.gam,wd,paste(ofname,yvar[i],"pred.xls",sep="_"))
          if (sxStria>"") {
            w<-c(w,paste("</br>合计: 含交互作用模型"))
            fmlia<-paste(yvname[i],"~",sxStria)
            tmp.gam<-setgam(fmlia,i); w<-c(w,gam2htmltable(tmp.gam))
          }
        }
      }
    }
  }
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)