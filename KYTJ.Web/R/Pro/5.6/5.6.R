  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0506"
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

  iyn <- yparm1
  iyv <- yparm2
  iys <- as.numeric(yparm3)
  iydist <- ydist
  iylink <- ylink
  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
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

  library(MASS,lib.loc=R.LibLocation)

  ofname<-"5_6"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"稳健(Robust)线性回归"; 
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
  sxf<-NA; 
  
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA; 
  
  if(ibs==1)  {
    bv<-idata[,ibn[1]]; bvar<-c(ibn[1]);bvname<-c(ibn[1]); 
  }else {
    bv<-NA; bvar<-NA; 
  }
  
  colv<-idata[,isjn];colvname<-c(isjn)
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6;  
  ##R package## MASS ##R package##;
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  rlm2htmltable<-function(mdl,w) {
    gs<-summary(mdl)
    coe<-gs$coefficients
    cnames<-c(colnames(coe),"95%CI low","95%CI upp")
    coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2])
    oo1<-cbind(c("",rownames(coe)),rbind(cnames,round(coe,dec)))
    p1<-c("Log Likelihood:", paste(format(logLik(mdl),digits=4,nsmall=4), ", df=",gs$df[1]+1))
    p2<-c("sigma:",format(gs$sigma,digits=4,nsmall=4))
    p3<-c("Number of observations used:", length(gs$residuals))
    oo2<-rbind(p1,p2,p3)
    w<-c(w,"</br><table border=3>", mat2htmltable(oo1), "</table>")
    w<-c(w,"</br><table border=3>", mat2htmltable(oo2), "</table>")
    return(w)
  }
  glm2htmltable<-function(mdl,w) {
    gs<-summary(mdl)
    coe<-gs$coefficients
    cnames<-c(colnames(coe),"95%CI low","95%CI upp")
    coe<- cbind(coe, coe[,1]-1.96*coe[,2], coe[,1]+1.96*coe[,2])
    oo1<-cbind(c("",rownames(coe)),rbind(cnames,round(coe,dec)))
    p1<-c("sigma:",format(gs$sigma,digits=4,nsmall=4))
    p2<-c("Log Likelihood:", paste(format(logLik(mdl),digits=4,nsmall=4), ", df=",gs$df[1]+1))
    p3<-c("R-squared:",format(gs$r.squared,digits=4,nsmall=4))
    p4<-c("Adj. r-squared:",format(gs$adj.r.squared,digits=4,nsmall=4))
    p5<-c("Number of observations used:", length(gs$residuals))
    oo2<-rbind(p1,p2,p3,p4,p5)
    w<-c(w,"</br><table border=3>", mat2htmltable(oo1), "</table>")
    w<-c(w,"</br><table border=3>", mat2htmltable(oo2), "</table>")
    return(w)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  ny<-ncol(yv); yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
  dd<-cbind(yv,xv); cname<-c(yvname,xvname)
  if (!is.na(bvar)) {dd<-cbind(dd,bv); cname<-c(cname,bvar);}
  if (!is.na(colvname)) {dd<-cbind(dd,colv); cname<-c(cname,colvname);}
  colnames(dd)<-cname;
  dd.tmp<-cbind(dd[,-(1:ny)],1)
  cmp<-(apply(is.na(dd.tmp),1,sum)==0); dd<-dd[cmp,]; rm(dd.tmp)
  detach(WD); rm(WD); WD<-as.data.frame(dd); attach(WD)
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  xv1<-xvname; xv1[xlv>2]<-paste("factor(",xvname[xlv>2],")",sep="")
  xv1<-paste(xv1,collapse="+")


  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,"<h2>稳健回归 (迭代重加权最小二乘法)</h2>")
  for (i in (1:ny)) {
    wd<-WD[!is.na(WD[,i]),]
    fml<-paste(yvname[i],"~",xv1)
    w<-c(w,"</br>结果变量:", yb[i])
    w<-c(w,"</br>变量分布与联系函数:", ydist[i])
    w<-c(w,"</br>模型:", fml)
    if (!is.na(bvar)) w<-c(w,"</br>合计:")
    tmp.mdl<-rlm(formula(fml),data=WD)
    w<-c(w,"</br>稳健回归 (迭代重加权最小二乘法)")
    w<-rlm2htmltable(tmp.mdl,w);
    tmp.mdl0<-lm(formula(fml),data=wd)
    w<-c(w,"</br>普通最小二乘回归")
    w<-glm2htmltable(tmp.mdl0,w);
    Cooks.distance<-cooks.distance(tmp.mdl0)
    std.residual<-stdres(tmp.mdl0)
    weight<-tmp.mdl$w
    tmpoo<-cbind(Cooks.distance,std.residual,weight,wd)
    stdres.abs<-abs(std.residual)
    oos<-tmpoo[order(-stdres.abs),]
    oos1<-rbind(colnames(oos),oos[1:10,])
    w<-c(w,"</br>被分配权重最小的 10 条记录")
    w<-c(w,"</br><table border=3>",mat2htmltable(oos1),"</table>")
    png(paste(ofname,yvar[i],"resid.png",sep="_"),width=720,height=560)
    plot(tmp.mdl0,which=1,las=1); dev.off()
    png(paste(ofname,yvar[i],"QQ.png",sep="_"),width=720,height=560)
    plot(tmp.mdl0,which=2,las=1); dev.off()
    png(paste(ofname,yvar[i],"ScaleLoc.png",sep="_"),width=720,height=560)
    plot(tmp.mdl0,which=3,las=1); dev.off()
    png(paste(ofname,yvar[i],"CooksD.png",sep="_"),width=720,height=560)
    plot(tmp.mdl0,which=4,las=1); dev.off()
    if (!is.na(bvar)) {
  	for (b in (1:nbg)) {
  	  rm(wd); wd<-WD[(WD[,bvar]==bv.lv[b] & !is.na(WD[,i])),]
  	  w<-c(w,paste("</br></br>亚组:", bvar, "=", bv.lb[b]))
  	  tmp.mdl<-rlm(formula(fml),data=wd)
  	  w<-c(w,"</br>稳健回归 (迭代重加权最小二乘法)")
  	  w<-rlm2htmltable(tmp.mdl,w);
  	  tmp.mdl0<-lm(formula(fml),data=wd)
  	  w<-c(w,"</br>普通最小二乘回归")
  	  w<-glm2htmltable(tmp.mdl0,w);
  	  Cooks.distance<-cooks.distance(tmp.mdl0)
  	  std.residual<-stdres(tmp.mdl0)
  	  weight<-tmp.mdl$w
  	  tmpoo<-cbind(Cooks.distance,std.residual,weight,wd)
  	  stdres.abs<-abs(std.residual)
  	  oos<-tmpoo[order(-stdres.abs),]
  	  oos1<-rbind(colnames(oos),oos[1:10,])
  	  w<-c(w,"</br>被分配权重最小的 10 条记录")
  	  w<-c(w,"</br><table border=3>",mat2htmltable(oos1),"</table>")
  	  png(paste(ofname,yvar[i],bvname,bv.lv[b],"resid.png",sep="_"),width=720,height=560)
  	  plot(tmp.mdl0,which=1,las=1); dev.off()
  	  png(paste(ofname,yvar[i],bvname,bv.lv[b],"QQ.png",sep="_"),width=720,height=560)
  	  plot(tmp.mdl0,which=2,las=1); dev.off()
  	  png(paste(ofname,yvar[i],bvname,bv.lv[b],"ScaleLoc.png",sep="_"),width=720,height=560)
  	  plot(tmp.mdl0,which=3,las=1); dev.off()
  	  png(paste(ofname,yvar[i],bvname,bv.lv[b],"CooksD.png",sep="_"),width=720,height=560)
  	  plot(tmp.mdl0,which=4,las=1); dev.off()
  	}     
    }
  }    
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)