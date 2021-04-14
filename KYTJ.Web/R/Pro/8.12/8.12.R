args <- commandArgs()

rlib<-args[6]	#libary path
if (is.na(rlib)) {output<-args[6]} else {output<-args[7]}	#param path
print(output)
#output<-c('D:\\Works\\产品\\科研\\代码\\ScienceStatistics.Web\\Output\\1\\1_1\\636525834691779319')
setwd(output)
d<-read.csv("./data.csv")
p<-read.csv('./Parameters.csv')
a<-p

#参数设定
idata<-d
R.Version4RUN<-343;
if (is.na(rlib)) {R.LibLocation<-"C:/R/R-3.4.3/library"} else {R.LibLocation=rlib}

Sys.setlocale("LC_TIME", "C")
library(doBy,lib.loc=R.LibLocation)
library(plotrix,lib.loc=R.LibLocation)
library(showtext,lib.loc=R.LibLocation)
showtext_auto(enable=TRUE)
pdfwd<-6; pdfht<-6
if (length(which(ls()=="ClinStats"))==0) ClinStats<-get(ls()[1])
names(ClinStats)<-toupper(names(ClinStats))

inumber<-as.numeric(a[1,1]);
iyn1<-as.character(a[1,2]);iyv1<-as.character(a[1,3]);iys1<-as.numeric(a[1,4])
ixn1<-as.character(a[1,5]);ixv1<-as.character(a[1,6]);ixs1<-as.numeric(a[1,7]);
irow1<-unlist(strsplit(as.character(a[1,8]),"[|]"));icol1<-unlist(strsplit(as.character(a[1,9]),"[|]"))
ixn2<-as.character(a[1,10]);ixv2<-as.character(a[1,11]);ixs2<-as.numeric(a[1,12]);
irow2<-unlist(strsplit(as.character(a[1,13]),"[|]"));icol2<-unlist(strsplit(as.character(a[1,14]),"[|]"))
ixn3<-as.character(a[1,15]);ixv3<-as.character(a[1,16]);ixs3<-as.numeric(a[1,17]);
irow3<-unlist(strsplit(as.character(a[1,18]),"[|]"));icol3<-unlist(strsplit(as.character(a[1,19]),"[|]"))
ixn4<-as.character(a[1,20]);ixv4<-as.character(a[1,21]);ixs4<-as.numeric(a[1,22]);
irow4<-unlist(strsplit(as.character(a[1,23]),"[|]"));icol4<-unlist(strsplit(as.character(a[1,24]),"[|]"))
ixn5<-as.character(a[1,25]);ixv5<-as.character(a[1,26]);ixs5<-as.numeric(a[1,27]);
irow5<-unlist(strsplit(as.character(a[1,28]),"[|]"));icol5<-unlist(strsplit(as.character(a[1,29]),"[|]"))
ixn6<-as.character(a[1,30]);ixv6<-as.character(a[1,31]);ixs6<-as.numeric(a[1,32]);
irow6<-unlist(strsplit(as.character(a[1,33]),"[|]"));icol6<-unlist(strsplit(as.character(a[1,34]),"[|]"))
ixn7<-as.character(a[1,35]);ixv7<-as.character(a[1,36]);ixs7<-as.numeric(a[1,37]);
irow7<-unlist(strsplit(as.character(a[1,38]),"[|]"));icol7<-unlist(strsplit(as.character(a[1,39]),"[|]"))
slt.vname<-c()

if(inumber==1)  {idata<-idata[,c(iyn1,ixn1)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,irow1)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,icol1)
}else if(inumber==2)  {idata<-idata[,c(iyn1,ixn1,ixn2)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,irow1,irow2)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,icol1,icol2)
}else if(inumber==3)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,irow1,irow2,irow3)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,icol1,icol2,icol3)
}else if(inumber==4)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,irow1,irow2,irow3,irow4=)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,icol1,icol2,icol3,icol4)
}else if(inumber==5)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,irow1,irow2,irow3,irow4,irow5)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,icol1,icol2,icol3,icol4,icol5)
}else if(inumber==6)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,irow1,irow2,irow3,irow4,irow5,irow6)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,icol1,icol2,icol3,icol4,icol5,icol6)
}else if(inumber==7)  {idata<-idata[,c(iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]
vname<-c("_N_","_STAT_","_TOTAL_",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,irow1,irow2,irow3,irow4,irow5,irow6,irow7)
vlabel<-c("样本量(%)","统计量","合计",iyn1,ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,icol1,icol2,icol3,icol4,icol5,icol6,icol7)
}

library(lattice,lib.loc=R.LibLocation)
library(cluster,lib.loc=R.LibLocation)
library(ellipse,lib.loc=R.LibLocation)
library(scatterplot3d,lib.loc=R.LibLocation)
library(FactoMineR,lib.loc=R.LibLocation)
library(ggplot2,lib.loc=R.LibLocation)
 
ofname<-"8_12"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"多重对应分析 (MCA)"; 
attach(WD) 
subjvname<-NA; 

if(inumber==1)  {
  xv<-as.matrix(idata[,c(ixn1)]); 
  xvname<-c(ixn1); 
  xvar<-c(ixv1); 
  xlv<-c(NA,ixs1)[-1]; 
}else if(inumber==2)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  xvname<-c(ixn1,ixn2); 
  xvar<-c(ixv1,ixv2); 
  xlv<-c(NA,ixs1,ixs2)[-1]; 
}else if(inumber==3)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  xvname<-c(ixn1,ixn2,ixn3); 
  xvar<-c(ixv1,ixv2,ixv3); 
  xlv<-c(NA,ixs1,ixs2,ixs3)[-1]; 
}else if(inumber==4)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4); 
  xlv<-as.numeric(c(NA,ixs1,ixs2,ixs3,ixs4)[-1]); 
}else if(inumber==5)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4,ixn5); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5)[-1]; 
}else if(inumber==6)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6)[-1]; 
}else if(inumber==7)  {
  xv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]); 
  xvname<-c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7); 
  xvar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7); 
  xlv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7)[-1]; 
}

sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-idata[,iyn1];colvname<-c(iyv1); 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 


##R package##  lattice cluster ellipse scatterplot3d FactoMineR ggplot2 ##R package##;
printTxt<-function(txt,cname,rname) {
  tmp<-as.matrix(txt,ncol=1); rname1<-rname
  if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
  colnames(tmp)=cname;  rownames(tmp)=rname1;print(tmp,quote=F)
}
xv<-xv[,(xlv>=2)]
xvname<-xvname[xlv>=2]
cmp<-(apply(is.na(xv),1,sum)==0)
tmp.xx<-xv[cmp,]
tmp.xlv<-apply(tmp.xx,2,function(x) length(levels(factor(x))))
tmp.xx<-tmp.xx[,tmp.xlv>=2]
tmp.xname<-xvname[tmp.xlv>=2]
tmp.nx<-ncol(tmp.xx)
for (i in 1:tmp.nx) {
  tmp.xlvi<-levels(factor(tmp.xx[,i]))
  tmp.label<-gsub(" ","",vlabel[match(paste(tmp.xname[i],tmp.xlvi,sep="."),vname)])
  #tmp.label[tmp.label=="0"]<-"N"
  for (j in (1:length(tmp.xlvi))) {tmp.xx[(tmp.xx[,i]==tmp.xlvi[j]),i]<-tmp.label[j]}
}
tmp.mca<-MCA(tmp.xx)
sink(paste(ofname,".txt",sep=""))
printTxt("多重对应分析","","")
print(tmp.mca$eig)
print(dimdesc(tmp.mca))
sink()
mca_vars_df = data.frame(tmp.mca$var$coord, Variable = rep(names(tmp.xlv), tmp.xlv))
mca_obs_df = data.frame(tmp.mca$ind$coord)
if (!is.na(colvname)) {
 tmp.dim<-cbind(colv[cmp],mca_obs_df)
 colnames(tmp.dim)<-c(colvname,names(mca_obs_df))
 write.table(tmp.dim,file=paste(ofname,"_obs.xls",sep=""),quote=FALSE, col.names=TRUE,row.names=FALSE,sep="\t")
}
tmp.vars<-cbind(var.level=rownames(mca_vars_df),mca_vars_df)
write.table(tmp.vars,file=paste(ofname,"_vars.xls",sep=""),quote=FALSE, col.names=TRUE,row.names=FALSE,sep="\t")
png(paste(ofname,"_3.png",sep=""),width=720,height=560)
plot(tmp.mca,choix=c("var"),cex=1.5,new.plot=FALSE)
dev.off()
png(paste(ofname,"_1.png",sep=""),width=720,height=560)
ggplot(data=mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) +
 geom_hline(yintercept = 0, colour = "gray70") +
 geom_vline(xintercept = 0, colour = "gray70") +
 geom_text(aes(colour=Variable))
dev.off()
png(paste(ofname,"_2.png",sep=""),width=720,height=560)
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data=mca_vars_df,aes(x=Dim.1,y=Dim.2,label=rownames(mca_vars_df),colour=Variable)) +
  scale_colour_discrete(name = "Variable")
dev.off()