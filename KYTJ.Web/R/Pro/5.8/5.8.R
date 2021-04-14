  #环境参数
  args <- commandArgs()
  rlib <- args[6] # rlib <- "E:\\R\\R-3.6.3\\library"
  output <- args[7] # output <- "E:\\202005\\test\\0508"
  setwd(output)
  pa <- read.csv('./Parameters.csv')
  cparm1 <- as.character(pa[1,1])
  cparm2 <- as.character(pa[2,1])
  xparm1 <- as.character(pa[3,1])
  xparm2 <- as.character(pa[4,1])
  xparm3 <- as.character(pa[5,1])
  xparm4 <- as.character(pa[6,1])
  xparm5 <- as.character(pa[7,1])

  icoln <- unlist(strsplit(cparm1,"[|]"))
  icolv <- unlist(strsplit(cparm2,"[|]"))
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
  
  vname<-c("_N_","_STAT_","_TOTAL_",icoln,ixn)
  vlabel<-c("样本量(%)","统计量","合计",icolv,ixv)
  
  library(MASS,lib.loc=R.LibLocation)
  
  ofname<-"5_8"; 
  WD<-idata; wd.subset=""; 
  svy.DSN.YN <- FALSE; 
  weights<-1;weights.var <- NA; 
  WD<-cbind(WD,weights); WD<-WD[!is.na(weights),];  
  title<-"判别分析"; 
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
  par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA, 0); 
  if (!exists("pdfwd")) pdfwd<-6; 
  if (!exists("pdfht")) pdfht<-6; 
  ##R package## MASS ##R package##;
  lda2table<-function(x,y,yvname,pngfname) {
    y.lv<-levels(factor(y)); nly<-length(y.lv); cmp<-(apply(is.na(cbind(y,x)),1,sum)==0)
    pngf3<-paste(pngfname,"pairs.png",sep="_")
    png(pngf3,width=720,height=560); pairs(x, pch=22, bg=rainbow(nly)[y]); dev.off()
    m1<-try(lda(x[cmp,],y[cmp],cv=FALSE));  m2<-try(qda(x[cmp,],y[cmp],cv=FALSE))
    fml<-paste(yvname,"=",paste(colnames(x),collapse="+"))
    t1<- paste("</br>Call:</br>",fml,"</br>")
    if (length(m1)>2) {
      t1<-c(t1,"</br>Prior probabilities of groups:</br><table border=3>")
      t1<-c(t1,mat2htmltable(rbind(names(m1$prior),round(m1$prior,5))),"</table></br>")
      t1<-c(t1,"</br>Group means:</br><table border=3>")
      gmeans<-cbind(c(" ",rownames(m1$means)),rbind(colnames(m1$means),round(m1$means,5)))
      t1<-c(t1,mat2htmltable(gmeans),"</table></br>")
      t1<-c(t1,"</br></br>Linear discriminants analysis:</br>")
      t1<-c(t1,"</br>Coefficients of linear discriminants:</br><table border=3>")
      coeff<-cbind(c(" ",rownames(m1$scaling)),rbind(colnames(m1$scaling),round(m1$scaling,5)))
      t1<-c(t1,mat2htmltable(coeff),"</table></br>")
      svd.trace<-rbind(c("LD1.svd","LD2.svd","LD1.prop.trace","LD2.prop.trace"),round(c(m1$svd, m1$svd^2/sum(m1$svd^2)),5))
      t1<-c(t1,"</br>SVD and Proportion of trace:</br><table border=3>")
      t1<-c(t1,mat2htmltable(svd.trace),"</table></br>")
      t1<-c(t1,"<ul>")
      t1<-c(t1,"<li>svd: the singular values, the ratio of the between- and within-group standard deviations on the linear discriminant variables.</li>")
      t1<-c(t1,"<li>LD1 proportion of trace indicates the percentage of between-group variance that the first discriminant function is able to explain from the total amount of between-group variance.</li>")
      t1<-c(t1,"<li>High trace number = discriminant function plays an important role!</li>")
      t1<-c(t1,"</ul>")
      p1<-predict(m1,newdata=x)
      cstable1<-table(original=y,classify=p1$class, exclude=NULL)
      csprop1<- matrix(paste(cstable1, " (", round(cstable1/rowSums(cstable1),4)*100, "%)",sep=""),ncol=ncol(cstable1))
      pp1<-cbind(c("original",rownames(cstable1)),rbind(colnames(cstable1),csprop1))
      t1<-c(t1,"</br>LDA: Original(row) vs Classified(column)</br><table border=3>")
      t1<-c(t1,mat2htmltable(pp1),"</table></br>")
      t1<-c(t1,paste("% of matching:", sum(diag(cstable1))/sum(!is.na(y))),"</br>")
      oo<-cbind(x,y,p1$class,round(p1$posterior,5))
      cnames<-c(colnames(x),yvname,"LDA.class",paste("LDA.post",y.lv))
      pngf1<-paste(pngfname,"LDA.png",sep="_")
      if (ncol(m1$scaling)>1) {
        png(pngf1,width=720,height=560); plot(m1,main=paste("LDA:",fml)); dev.off()
      }
    }
    if (length(m2)>2) {
      p2<-predict(m2, newdata=x)
      cstable2<-table(original=y,classify=p2$class, exclude=NULL)
      csprop2<- matrix(paste(cstable2, " (", round(cstable2/rowSums(cstable2),4)*100, "%)",sep=""),ncol=ncol(cstable2))
      pp2<-cbind(c("original",rownames(cstable2)),rbind(colnames(cstable2),csprop2))
      t1<-c(t1,"</br>QDA: Original(row) vs Classified(column)</br><table border=3>")
      t1<-c(t1,mat2htmltable(pp2),"</table></br>")
      t1<-c(t1,paste("% of matching:", sum(diag(cstable2))/sum(!is.na(y))),"</br>")
      oo<-cbind(oo,p2$class,round(p2$posterior,5))
      cnames<-c(cnames,"QDA.class",paste("QDA.post",y.lv))
    }
    if (length(m1)>2) { 
      colnames(oo)<-cnames; xlsfname<-paste(pngfname,".xls",sep="")
      write.table(oo,file=xlsfname,row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE,quote=FALSE)
    }
    t1<-c(t1,"</br><table><tr>")
    t1<-c(t1,paste("<td><a href=\"",pngf3,"\",target=_BLANK><img src=\"",pngf3,"\" width=320,height=320></a></td>",sep=""))
    if (length(m1)>2) {
      if (ncol(m1$scaling)>1) {
        t1<-c(t1,paste("<td><a href=\"",pngf1,"\",target=_BLANK><img src=\"",pngf1,"\" width=320,height=320></a></td>",sep=""))
      } 
    }
    t1<-c(t1,"</tr></table></br>")
    return(t1)
  }
  mat2htmltable<-function(mat) {
    t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
    t2<- paste("<tr><td>",t1,"</td></tr>")
    return(paste(t2,collapse=" "))
  }
  vlabelN<-(substr(vlabel,1,1)==" ");
  vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
  vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
  ofname1<-ofname
  dd<-cbind(factor(colv),xv); colnames(dd)<-c(colvname,xvname);
  cmp<-(apply(is.na(cbind(1,xv)),1,sum)==0) 
  x<-dd[cmp,-1]; y<-dd[cmp,1]; 
  yb<-vlabelV[match(colvname,vnameV)]; if (is.na(yb)) yb<-colvname
  w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
  w<-c(w,"<h2>Discriminant Analysis</h2>")
  w<-c(w,"</br>Outcome:", yb, "</br>")
  w<-c(w,lda2table(x,y,colvname,ofname1))
  w<-c(w,"</body></html>")
  fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)