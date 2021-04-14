  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\202005\\test\\0202"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  cparm1 <- as.character(pa[4,1])
  cparm2 <- as.character(pa[5,1])
  method1 <- as.character(pa[6,1])
  method2 <- as.character(pa[7,1])
  
  ixn <- unlist(strsplit(xparm1,"[,]")) 
  ixv <- unlist(strsplit(xparm2,"[,]")) 
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]"))) 
  icn <- unlist(strsplit(cparm1,"[|]"))  
  icv <- unlist(strsplit(cparm2,"[|]")) 
  m1 <- as.numeric(method1)
  m2 <- as.numeric(method2)

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
  
  ofname<-"2_2";
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  title<-"两样本比较的T检验"
  attach(WD) 
  subjvname<-NA;
  
  library(stringr)
  sq <- str_count(xparm1,",")+1
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
  xlv<-c(NA,ixs)[-1]; 
  sxf<-NA; 
  svname<-NA; sv<-NA; slv<-NA; 
  av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
  timev<-NA; timevname<-NA;
  bv<-NA; bvar<-NA; 
  colv<-as.matrix(idata[,icn[1]]);colvname<-c(icn[1]); 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-m1;dec<-4;parm<-c(NA, m2,NA, NA); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## gdata gplots ##R package##;
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
    t3<- paste(t2,collapse="");
    return(t2)
  }
  aovMeanSd <- function(v,g) {
    v <- v[order(g)]
    N<-tapply(v,g,function(x) sum(!is.na(x)))
    Mean<-tapply(v,g,function(x) numfmt(mean(x,na.rm=TRUE),dec))
    Sd<-tapply(v,g,function(x) numfmt(sd(x,na.rm=TRUE),dec))
    Min<-tapply(v,g,function(x) numfmt(min(x,na.rm=TRUE),dec))
    Median<-tapply(v,g,function(x) numfmt(median(x,na.rm=TRUE),dec))
    Max<-tapply(v,g,function(x) numfmt(max(x,na.rm=TRUE),dec))
    tmp.o<-cbind(levels(factor(g)),N,Mean,Sd,Min,Median,Max)
    return (tmp.o)
  }
  aovMeanSd2 <- function(v,g,g2) {
    g2.level <- names(table(g2))
    for (i in (1:length(g2.level))) {
      tmp.i <- aovMeanSd(v[g2==g2.level[i]],g[g2==g2.level[i]])
      tmp.i <- cbind(rep(as.numeric(g2.level[i]),nrow(tmp.i)),tmp.i)
      if (i==1) tmp.oo<-tmp.i
      if (i>1) tmp.oo<-rbind(tmp.oo,tmp.i)
    }
    return (tmp.oo)
  }
  ttest2mat<-function(tx) {
    if (length(tx$estimate)==2) {oo<-c(tx$estimate[1],tx$estimate[2],tx$estimate[1]-tx$estimate[2])}
    else {oo<-c(NA,NA,tx$estimate[1])}
    oo<-numfmt(c(oo,tx$conf.int[1],tx$conf.int[2],tx$statistic),dec)
    oo<-c(oo,round(tx$parameter), pvformat(tx$p.value,dec+2),tx$alternative,tx$method)
    return(oo)
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-ncol(xv); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  tmp.n<-nrow(xv)
  if (!is.na(bvar)) {
    bvb<-vlabelV[match(bvar,vnameV)]; if (is.na(bvb)) bvb<-bvar; 
    bv.lv<-levels(factor(bv)); 
    bv.lb<-vlabelZ[match(paste(bvar,bv.lv,sep="."),vnameZ)]
    bv.lb[is.na(bv.lb)]<-bv.lv[is.na(bv.lb)]
    nbg<-length(bv.lv)
  }
  if (!is.na(colvname)) {
    colvb<-vlabelV[match(colvname,vnameV)]; if (is.na(colvb)) colvb<-colvname;
    colv.lv<-levels(factor(colv))
    colv.lb<-vlabelZ[match(paste(colvname,colv.lv,sep="."),vnameZ)]
    colv.lb[is.na(colv.lb)]<-colv.lv[is.na(colv.lb)]
  }
  if (is.na(par1)) par1<-1;
  ah1<-c("t","g","l")[par1];
  if (is.na(parm[2])) parm[2]<-1; 
  eqvar<-c(TRUE,FALSE)[parm[2]];
  o.tt <- o.ttalt <- o.ttmethod <- NULL
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  sink(paste(ofname,".lst",sep=""))
  
  
  
  tname<-c("样本1均数","样本2均数","均数差","95%区间下限","95%区间上限","t　值","自由度","p　值")
  w<-c(w,"<h2>两样本比较的 t 检验</h2>")
  if (is.na(colvname)) {
    if (nx==2) {
      o.nms <- c("", "N", "Mean", "Sd", "Min", "Median", "Max")
      if (!is.na(bvar)) o.nms <- c(bvb, o.nms)
      tmp.yvar=c(xv[,1],xv[,2]); tmp.group<-c(rep(1,tmp.n),rep(2,tmp.n))    
      if (is.na(bvar)) {
        tmp.nms<-aovMeanSd(tmp.yvar,tmp.group); 
        o.nms<-rbind(o.nms, cbind(xb,tmp.nms[,-1]));
        tmp.t1<-t.test(xv[,1]-xv[,2],alternative=ah1,var.equal=eqvar)
        tmp.t2<-t.test(xv[,1],xv[,2],alternative=ah1,var.equal=eqvar)
        print(tmp.t1)
        print(tmp.t2)
        o.tt <- rbind(c("比较",tname,"Method"), cbind(c("配对","非配对"),rbind(ttest2mat(tmp.t1)[-9], ttest2mat(tmp.t2)[-9])))
        tmp.m<-tmp.t2$estimate
        tmp.mtxt<-paste(xvname,": 分布的箱图",collapse=" ")
        png(paste(ofname,"_boxplot.png",sep=""),width=720,height=560)
        boxplot(tmp.yvar~tmp.group,names=xb,main=tmp.mtxt,ylab=" ",col=rainbow(2))
        dev.off()
        png(paste(ofname,"_barplot.png",sep=""),width=720,height=560)
        tmp.mtxt<-paste(xvname,": 的均数",collapse=" ")
        barplot(tmp.m,space=1,names=xb,main=tmp.mtxt,ylab="",col=rainbow(2))
        dev.off()
      } else {
        tmp.nms<-aovMeanSd2(tmp.yvar,tmp.group,c(bv,bv)); 
        tmp.nms[,1] <- bv.lb[match(tmp.nms[,1],bv.lv)]
        tmp.nms[,2] <- xb[match(tmp.nms[,2],(1:2))]
        o.nms<-rbind(o.nms, tmp.nms);
        tmp.m<-NA;tmp.names<-NA
        if (is.null(o.tt)) o.tt <- c(bvb, "比较", tname, "Method")
        for (j in (1:nbg)) {
          tmp.t1<-t.test(xv[bv==bv.lv[j],1]-xv[bv==bv.lv[j],2],alternative=ah1,var.equal=eqvar)
          tmp.t2<-t.test(xv[bv==bv.lv[j],1],xv[bv==bv.lv[j],2],alternative=ah1,var.equal=eqvar)
          print(tmp.t1)
          print(tmp.t2)
          o.tt <- rbind(o.tt, cbind(bv.lb[j],c("配对","非配对"),rbind(ttest2mat(tmp.t1)[-9], ttest2mat(tmp.t2)[-9])))
          tmp.m<-cbind(tmp.m,tmp.t2$estimate);tmp.names<-c(tmp.names,paste(xb,bv.lb[j],sep="\n"))
        }
        tmp.m<-tmp.m[,-1];tmp.names<-tmp.names[-1]
        tmp.mtxt<-paste("按", bvb, "分层的分布箱图")
        png(paste(ofname,"_boxplot.png",sep=""),width=720,height=560)
        boxplot(tmp.yvar~tmp.group*c(bv,bv),names=tmp.names,main=tmp.mtxt,ylab="",col=rainbow(2))
        dev.off()
        tmp.mtxt<-paste("按", bvb, "分层的均数")
        tmp.maxy<-max(tmp.m,na.rm=TRUE);tmp.maxx<-8
        png(paste(ofname,"_barplot.png",sep=""),width=720,height=560)
        barplot(tmp.m, beside=TRUE, space=c(0,1), names=bv.lb, legend=xb, ylim=c(0,tmp.maxy), xlim=c(0,tmp.maxx), main=tmp.mtxt, ylab="", col=rainbow(2),args.legend=list(title=""))
        dev.off()
      }
      o.ttalt <- tmp.t1$alternative
    } else {w<-c(w,"</br>请设置分组变量"); }
  } else {
    if (length(colv.lv)==2) {
      o.nms <- c(colvb, "N", "Mean", "Sd", "Min", "Median", "Max")
      if (!is.na(bvar)) o.nms <- c(bvb, o.nms)
      for (i in (1:nx)) {
        ofname1 <- ofname;  if (nx>1) ofname1 <- paste(ofname,xvname[i],sep="_")
        if (is.na(bvar)) {
          tmp.nms <- aovMeanSd(xv[,i],colv)
          tmp.nms[,1] <- colv.lb[match(tmp.nms[,1],colv.lv)]
          o.nms<-rbind(o.nms,tmp.nms);
          tmp.t1<-t.test(xv[colv==colv.lv[1],i],xv[colv==colv.lv[2],i],alternative=ah1,var.equal=eqvar)
          print(tmp.t1)        
          if (is.null(o.tt)) o.tt <- c("变量", tname)
          o.tt <- rbind(o.tt,c(xb[i],ttest2mat(tmp.t1)[1:8]))
          tmp.m<-tmp.t1$estimate
          tmp.mtxt<-paste(xb[i],": 分布的箱图",collapse=" ")
          png(paste(ofname1,"boxplot.png",sep="_"),width=720,height=560)
          boxplot(xv[,i]~colv,names=colv.lb,main=tmp.mtxt,ylab=xb[i],col=rainbow(2))
          dev.off()
          png(paste(ofname1,"barplot.png",sep="_"),width=720,height=560)
          tmp.mtxt<-paste(xb[i],": 的均数",collapse=" ")
          barplot(tmp.m,space=1,names=colv.lb,main=tmp.mtxt,ylab=xb[i],col=rainbow(2))
          dev.off()
        } else {
          tmp.nms <- aovMeanSd2(xv[,i], colv, bv)
          tmp.nms[,1] <- bv.lb[match(tmp.nms[,1],bv.lv)]
          tmp.nms[,2] <- colv.lb[match(tmp.nms[,2],colv.lv)]
          o.nms<-rbind(o.nms, tmp.nms);
          tmp.m<-NA;tmp.names<-NA
          for (j in (1:nbg)) {
            tmp.t1<-t.test(xv[bv==bv.lv[j] & colv==colv.lv[1],i],xv[bv==bv.lv[j] & colv==colv.lv[2],i],alternative=ah1,var.equal=eqvar)
            print(tmp.t1)        
            if (is.null(o.tt)) o.tt <- c("变量", bvb, tname)
            o.tt <- rbind(o.tt, c(xb[i],bv.lb[j],ttest2mat(tmp.t1)[1:8]))
            tmp.m<-cbind(tmp.m,tmp.t1$estimate); tmp.names<-c(tmp.names,paste(colv.lb,bv.lb[j],sep="\n"))
          }
          tmp.m<-tmp.m[,-1];tmp.names<-tmp.names[-1]
          tmp.mtxt<-paste("按", bvb, "分层的",xb[i],"分布箱图")
          png(paste(ofname1,"_boxplot.png",sep=""),width=720,height=560)
          boxplot(xv[,i]~colv*bv,names=tmp.names,main=tmp.mtxt,ylab=xb[i],col=rainbow(2))
          dev.off()
          tmp.mtxt<-paste("按", bvb, "分层的",xb[i],"均数")
          tmp.maxy<-max(tmp.m,na.rm=TRUE);tmp.maxx<-nbg*3+2
          png(paste(ofname1,"_barplot.png",sep=""),width=720,height=560)
          barplot(tmp.m,beside=TRUE,space=c(0,1),names=bv.lb, legend=colv.lb, ylim=c(0,tmp.maxy), xlim=c(0,tmp.maxx),
                  main=tmp.mtxt,ylab=xb[i],col=rainbow(2),args.legend=list(title=colvb))
          dev.off()
        }
        if (is.null(o.ttalt)) o.ttalt <- tmp.t1$alternative
        if (is.null(o.ttmethod)) o.ttmethod <- tmp.t1$method
      }
    } else {w<-c(w,"</br>超过2组样本，请用多组比较的方差分析模块。")}
  }
  w<-c(w,"样本量、均数、标准差:</br><table border=3>", mat2htmltable(o.nms),"</table></br>")
  if (is.na(colvname)) {
    w<-c(w,"</br>两变量比较的 t 检验")
    w<-c(w,paste(xb[1], "vs.", xb[2], ", ", o.ttalt))
  } else {
    w<-c(w,"</br>两样本比较的 t 检验")
    w<-c(w,"</br>",paste(paste(colvb,"=",paste(colv.lb,collapse=" vs. ")), ", ", o.ttalt))
    w<-c(w,"</br>方法：", o.ttmethod)
  }
  w<-c(w,"</br><table border=3>", mat2htmltable(o.tt),"</table></br>")
  w<-c(w,"</body></html>")
  sink()
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)
