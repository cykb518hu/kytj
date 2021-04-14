  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0510"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  xparm1 <- as.character(pa[1,1])
  xparm2 <- as.character(pa[2,1])
  xparm3 <- as.character(pa[3,1])
  xparm4 <- as.character(pa[4,1])
  xparm5 <- as.character(pa[5,1])
  std <- as.character(pa[6,1]) 

  ixn <- unlist(strsplit(xparm1,"[|]"))
  ixv <- unlist(strsplit(xparm2,"[|]"))
  ixs <- as.numeric(unlist(strsplit(xparm3,"[,]")))
  ixc <- unlist(strsplit(xparm4,"[,]"))
  ifs <- as.numeric(unlist(strsplit(xparm5,"[,]")))
  istd <- as.numeric(std)
  
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

  vname<-c("_N_","_STAT_","_TOTAL_",ixn)
  vlabel<-c("样本量(%)","统计量","合计",ixv)
  
  library(fmsb,lib.loc=R.LibLocation)
   
  ofname<-"5_10"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
  title<-"自变量共线性筛查"; 
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
  colv<-NA; colvname<-NA; 
  v.start<-NA; vname.start<-NA; 
  v.stop<-NA; vname.stop<-NA; 
  par1<-istd;dec<-4;parm<-c(NA, NA, NA, NA); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## fmsb ##R package##;
  pvformat<-function(p,dec) {
    p1<-round(as.numeric(p),dec); pp<-p1
    tmp<-(substr(p1,2,9)=="e-04" & !is.na(p1))
    pp[tmp]<-paste("0.000",substr(pp[tmp],1,1),sep="")
    tmp<-(p1==0 & !is.na(p1))
    pp[tmp]<-paste(c("<0.",rep("0",dec-1),"1"),collapse="")
    tmp<-(p1>0 & !is.na(p1))
    pp[tmp]<-paste(pp[tmp],substr(rep("0000000",length(pp[tmp])),1,dec+2-nchar(pp[tmp])),sep="")
    tmp<-(p1==1 & !is.na(p1))
    pp[tmp]<-substr("0.999999999",1,dec+2)
    return(pp)
  }
  numfmt<-function(p,dec) {
    if (is.list(p)) p<-as.matrix(p)
    if (is.matrix(p)) {nr<-nrow(p);} else {nr<-1;}
    p1<-round(as.numeric(p),dec); 
    p2<-p1-floor(p1);
    tmp<-(p2==0 & !is.na(p2))
    p1[tmp]<-paste(p1[tmp],".0",sep="")
    p2[tmp]<-"0.0"; 
    p1<-paste(p1,substr(rep("0000000",length(p1)),1,dec+2-nchar(p2)),sep="")
    p1[as.numeric(p)>10000000]<-"inf."
    p1[is.na(p) | p=="" | p==" "]<-""
    p1[p=="-Inf"]<-"-Inf"
    p1[p=="Inf"]<-"Inf"
    if (is.matrix(p)) {
      p1<-matrix(p1,nrow=nr);colnames(p1)<-colnames(p);rownames(p1)<-rownames(p)
    }
    return(p1)
  }
  vifSelect<-function(dfr, t=5, fix=NA){
    if(!class(dfr)=="data.frame") dfr<-data.frame(dfr)
    vnames<-names(dfr); nvar<-length(vnames); 
    if (!is.na(fix)) {v4slt<-(1:nvar)[-fix];} else {v4slt<-(1:nvar);} 
    vifmat<-matrix(NA,nrow=nvar,ncol=1)
    vifmax<-100; v.rm<-0; k<-1
    while (vifmax>=t) {
      vnum <-(0:nvar)[-match(v.rm,(0:nvar))]
      for (v in vnum) {
        fml <- paste(vnames[v],"~",paste(vnames[vnum[vnum!=v]],collapse="+"))
        lm.mdl<-summary(lm(formula(fml),data=dfr));
        print(fml);print(lm.mdl)
        vifmat[v,k]<-round(1/(1-lm.mdl$r.squared),1)
      } 
      vifmax<-max(as.numeric(vifmat[v4slt,k]),na.rm=TRUE)
      if (vifmax<t) break
      v.rm<-c(v.rm,v4slt[which(as.numeric(vifmat[v4slt,k])==vifmax)[1]])
      k<-k+1; vifmat<-cbind(vifmat,NA); 
    }
    slt.vv<-!is.na(vifmat[v4slt,k])
    vif.Rm<-vnames[is.na(vifmat[,k])]
    vifmat2 <- rbind(c(" ",paste("Step",(1:k))),cbind(vnames,vifmat))
    dfcmpt<-dfr[apply(is.na(dfr),1,sum)==0,]
    dfvmax<-apply(dfcmpt,2,max);  dfvmin<-apply(dfcmpt,2,min);  singlar<-(dfvmax==dfvmin);
    if (sum(singlar)>0) {
      slt.vv<-(!is.na(vifmat[,k]) & (!singlar))[v4slt]
      vif.Rm<-vnames[is.na(vifmat[,k]) | singlar]
      vifmat2 <-cbind(vifmat2, c("Singlarity check", singlar))
    }
    return(list(slt.vv,vif.Rm,vifmat2))
  }
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  nx<-length(xvname); xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
  if (is.na(par1)) par1<-5
  if (!is.numeric(par1)) par1<-5
  sink(paste(ofname,".lst",sep=""))
  vifChk<-vifSelect(xv,t=par1,fix=NA)
  sink()
  
  
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,paste("<h2>", title, "</h2>"))
  w<-c(w,"</br>自变量共线性逐步筛查:")
  w<-c(w,"</br></br><table border=3>", mat2htmltable(vifChk[[3]]), "</table>")
  if (length(vifChk[[2]])>0) w<-c(w,"</br>剔除的变量: ", paste(vifChk[[2]],collapse=" "))
  w<-c(w,"</br>选出的变量: ", paste(xvname[vifChk[[1]]], collapse=" "));
  w<-c(w,"</br></br>1. VIF: variance inflation factors. 某变量(eg. X1)的VIF计算方法为 VIF = 1/(1-R2), 其中R2即来自一个线性回归方程的R平方值，该方程因变量为该变量(eg. X1), 自变量为所有其它变量(eg. X1=X2+X3+...)")
  w<-c(w,"</br></br>2. VIF 逐步筛查方法:")
  w<-c(w, "</br>. 计算每个变量的 VIF, 如果最大的VIF值>=", par1, ", 剔除最大VIF值的变量")
  w<-c(w, "</br>. 重复上步，直到剩下的所有变量的VIF小于", par1)
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)