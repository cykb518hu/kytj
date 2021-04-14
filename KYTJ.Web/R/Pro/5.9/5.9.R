  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0509"
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

  icoln <- unlist(strsplit(cparm1,"[|]"))
  icolv <- unlist(strsplit(cparm2,"[|]"))
  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
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
  
  vname<-c("_N_","_STAT_","_TOTAL_",icoln,ixn)
  vlabel<-c("样本量(%)","统计量","合计",icolv,ixv)
  
  library(rpart,lib.loc=R.LibLocation)
  library(rpart.plot,lib.loc=R.LibLocation)
  library(survival,lib.loc=R.LibLocation)
   
  ofname<-"5_9"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"分类决策树"; 
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
  timev<-NA; timevname<-NA; 
  bv<-NA; bvar<-NA; 
  colv<-idata[,icoln[1]];colvname<-c(icoln[1])
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-m1;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## rpart rpart.plot survival ##R package##;
  printTxt<-function(txt,cname,rname) { 
    tmp<-as.matrix(txt,ncol=1); rname1<-rname
    if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
    colnames(tmp)=cname;  rownames(tmp)=rname1; print(tmp,quote=F)
  }
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  if (par1<4) {
    wdtmp<-data.frame(cbind(colv,xv));names(wdtmp)<-c(colvname,xvname)
  } else {
    timevb<-vlabelV[match(timevname,vnameV)]; 
    if (is.na(timevb)) timevb<-timevname;
    wdtmp<-data.frame(cbind(colv,timev,xv));names(wdtmp)<-c(colvname,timevname,xvname)
  }
  for (i in 1:length(xvname)) {
   if (xlv[i]>0 && xlv[i]<=5) {
     wdtmp[,xvname[i]]<-as.factor(wdtmp[,xvname[i]]);
   } else {wdtmp[,xvname[i]]<-as.numeric(wdtmp[,xvname[i]]);}
  }
  xv1<-paste(xvname,collapse="+")
  colvb<-vlabelV[match(colvname,vnameV)]; 
  colvb=vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
  colv.lv<-levels(factor(colv))
  if (length(colv.lv)<10 & par1==1) {
    colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
    colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
    wdtmp[,colvname]<-factor(wdtmp[,colvname],levels=colv.lv,labels=colv.lb)
  }
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  sink(paste(ofname,".htm",sep=""))
  printTxt("<!DOCTYPE html><html><head><meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /</head><body>","","")
  mthi<-c("class","anova","poisson","exp")[par1]
  if (par1<4) {fml<-paste(colvname,"~",xv1,sep="");
  } else {fml<-paste("Surv(",timevname,",",colvname,")~",xv1,sep="");}
  rfiti<-try(rpart(formula(fml),data=wdtmp,method=mthi))
  printTxt(fml,"","")
  vimp<-round(rfiti$variable.importance/sum(rfiti$variable.importance)*100,1)
  vimp<-rbind(c("Variable","Importance"), cbind(names(vimp),vimp))
  printTxt(paste("</br><table border=3>",mat2htmltable(vimp),'</table></br>'),"","")
  cpi= rfiti$cptable[(rfiti$cptable[,"xerror"]==sort(rfiti$cptable[,"xerror"])[2]),"CP"][1]
  rfit2<-prune(rfiti, cp= cpi)
  fname<-paste(ofname,"tree.png",sep="_")
  fname2<-paste(ofname,"tree2.png",sep="_")
  printTxt(paste("Outcome=",colvb),"","")
  printTxt("</br></br>","","")
  pngfs<-paste("<img src=\"",fname,"\" width=500 height=500>",sep="")
  pngfs<-paste("<a href=\"",fname,"\" width=500 height=500>",pngfs,"</a>",sep="")
  pngfs2<-paste("<img src=\"",fname2,"\" width=500 height=500>",sep="")
  pngfs2<-paste("<a href=\"",fname2,"\" width=500 height=500>",pngfs2,"</a>",sep="")
  printTxt(pngfs,"","")
  printTxt("</br>After prune</br>","","")
  printTxt(pngfs2,"","")
  printTxt("</br></br>Recursive partitioning analysis</br>","","")
  printTxt("<textarea rows=30 cols=100>","","")
  print(rfiti)
  printcp(rfiti)
  printTxt(paste("cp=",cpi),"","")
  printTxt("Prune tree analysis","","")
  print(rfit2)
  printTxt("</textarea></body></html>","","")
  sink()
   
  png(fname,width=720,height=560)
  if (mthi=="class") {
       rpart.plot(rfiti, branch=1, type=4, extra=102, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  } else {
       rpart.plot(rfiti, branch=1, type=4, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  }
  dev.off()
  png(fname2,width=720,height=560)
  if (mthi=="class") {
       rpart.plot(rfit2, branch=1, type=4, extra=102, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  } else {
       rpart.plot(rfit2, branch=1, type=4, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  }
  dev.off()
  pdf(paste(ofname,"tree.pdf",sep="_"),width=pdfwd,height=pdfht,family="Helvetica");
  if (mthi=="class") {
       rpart.plot(rfiti, branch=1, type=4, extra=102, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  } else {
       rpart.plot(rfiti, branch=1, type=4, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  }
  dev.off()
  pdf(paste(ofname,"tree2.pdf",sep="_"),width=pdfwd,height=pdfht,family="Helvetica");
  if (mthi=="class") {
       rpart.plot(rfit2, branch=1, type=4, extra=102, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  } else {
       rpart.plot(rfit2, branch=1, type=4, shadow.col="gray", box.col="green",  
       border.col="blue", split.col="red", split.cex=1.2, main=paste(colvb,"决策树"))
  }
  dev.off()
  if (par1==4) {
    fname3<-paste(ofname,"km.png",sep="_")
    xleg<-max(timev,na.rm=TRUE)*0.8
    km<-survfit(formula(paste("Surv(",timevname,",",colvname,")~rfit2$where")),data=wdtmp)
    nodes<-levels(factor(rfit2$where)); nnode<-length(nodes)
    png(fname3,width=720,height=560)
    plot(km,lty=1:nnode, mark.time=FALSE,xlab=timevb,ylab=colvb)
    legend(xleg,0.95,paste("node",nodes),lty=1:nnode)
    dev.off()
  }
  yprob<-round(predict(rfiti,newdata=wdtmp,method=mthi),5)
  yprob2<-round(predict(rfit2,newdata=wdtmp,method=mthi),5)
  if (is.matrix(yprob)) {
      colnames(yprob)<-paste(colvname,colnames(yprob),"PRED",sep=".")
      colnames(yprob2)<-paste(colvname,colnames(yprob2),"PRED2",sep=".")
  } else {
      yprob<-matrix(yprob,ncol=1); colnames(yprob)<-paste(colvname,"PRED",sep=".");
      yprob2<-matrix(yprob2,ncol=1); colnames(yprob2)<-paste(colvname,"PRED2",sep=".");
  }
  wdtmp<-cbind(yprob,yprob2,wdtmp)
  write.table(wdtmp,file=paste(ofname,".PRED.xls",sep=""),row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE,quote=FALSE)