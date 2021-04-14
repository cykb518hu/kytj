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
#--#

inumber<-as.numeric(a[1,1])
ixn1<-as.character(a[1,2]);ixv1<-as.character(a[1,3]);ixs1<-as.numeric(a[1,4]);id1<-as.character(a[1,5]);
ixn2<-as.character(a[1,6]);ixv2<-as.character(a[1,7]);ixs2<-as.numeric(a[1,8]);id2<-as.character(a[1,9]);
ixn3<-as.character(a[1,10]);ixv3<-as.character(a[1,11]);ixs3<-as.numeric(a[1,12]);id3<-as.character(a[1,13]);
ixn4<-as.character(a[1,14]);ixv4<-as.character(a[1,15]);ixs4<-as.numeric(a[1,16]);id4<-as.character(a[1,17]);
ixn5<-as.character(a[1,18]);ixv5<-as.character(a[1,19]);ixs5<-as.numeric(a[1,20]);id5<-as.character(a[1,21]);
ixn6<-as.character(a[1,22]);ixv6<-as.character(a[1,23]);ixs6<-as.numeric(a[1,24]);id6<-as.character(a[1,25]);
ixn7<-as.character(a[1,26]);ixv7<-as.character(a[1,27]);ixs7<-as.numeric(a[1,28]);id7<-as.character(a[1,29]);
ixn8<-as.character(a[1,30]);ixv8<-as.character(a[1,31]);ixs8<-as.numeric(a[1,32]);id8<-as.character(a[1,33]);
ixn9<-as.character(a[1,34]);ixv9<-as.character(a[1,35]);ixs9<-as.numeric(a[1,36]);id9<-as.character(a[1,37]);
ixn10<-as.character(a[1,38]);ixv10<-as.character(a[1,39]);ixs10<-as.numeric(a[1,40]);id10<-as.character(a[1,41]);
ixn11<-as.character(a[1,42]);ixv11<-as.character(a[1,43]);ixs11<-as.numeric(a[1,44]);id11<-as.character(a[1,45]);
ixn12<-as.character(a[1,46]);ixv12<-as.character(a[1,47]);ixs12<-as.numeric(a[1,48]);id12<-as.character(a[1,49]);
slt.vname<-c()

if(inumber==1)  {idata<-idata[,c(ixn1)]
vname<-c("_N_","_STAT_","_TOTAL_",ixv1)
vlabel<-c("样本量(%)","统计量","合计",ixn1)
}else if(inumber==2)  {idata<-idata[,c(ixn1,ixn2)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2)
}else if(inumber==3)  {idata<-idata[,c(ixn1,ixn2,ixn3)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3)
}else if(inumber==4)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4)
}else if(inumber==5)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5)
}else if(inumber==6)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)
}else if(inumber==7)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)
}else if(inumber==8)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8)
}else if(inumber==9)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9)
}else if(inumber==10)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10)
}else if(inumber==11)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10,ixn11)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10,ixv11)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10,ixn11)
}else if(inumber==12)  {idata<-idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10,ixn11,ixn12)]
vname<-c("_N_","_STAT_","_TOTAL_",ixn1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10,ixv11,ixv12)
vlabel<-c("样本量(%)","统计量","合计",ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10,ixn11,ixn12)
}

library(lavaan,lib.loc=R.LibLocation)
library(mnormt,lib.loc=R.LibLocation)
library(pbivnorm,lib.loc=R.LibLocation)
library(quadprog,lib.loc=R.LibLocation)

ofname<-"8_10"; 
svy.DSN.YN <- FALSE; 
WD<-idata; wd.subset=""; 
title<-"验证性因子分析(潜变量分析)"; 
attach(WD) 
subjvname<-NA; 
xvname<-NA; xv<-NA; xlv<-NA; 
sxf<-NA; 

if(inumber==1)  {
  sv<-as.matrix(idata[,c(ixn1)]); 
  svname<-c(ixv1); 
  svar<-c(ixv1);
  sdf<-c(NA,id1)[-1];
  slv<-c(NA,ixs1)[-1]; 
}else if(inumber==2)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2)]); 
  svname<-c(ixv1,ixv2); 
  svar<-c(ixv1,ixv2); 
  sdf<-c(NA,id1,id2)[-1];
  slv<-c(NA,ixs1,ixs2)[-1]; 
}else if(inumber==3)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3)]); 
  svname<-c(ixv1,ixv2,ixv3); 
  svar<-c(ixv1,ixv2,ixv3);
  sdf<-c(NA,id1,id2,id3)[-1];
  slv<-c(NA,ixs1,ixs2,ixs3)[-1]; 
}else if(inumber==4)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4); 
  svar<-c(ixv1,ixv2,ixv3,ixv4); 
  sdf<-c(NA,id1,id2,id3,id4)[-1];
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4)[-1]; 
}else if(inumber==5)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5); 
  sdf<-c(NA,id1,id2,id3,id4,id5)[-1];
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5)[-1]; 
}else if(inumber==6)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6);
  sdf<-c(NA,id1,id2,id3,id4,id5,id6)[-1];
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6)[-1]; 
}else if(inumber==7)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7); 
  sdf<-c(NA,id1,id2,id3,id4,id5,id6,id7)[-1];  
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7)[-1]; 
}else if(inumber==8)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8);
  sdf<-c(NA,id1,id2,id3,id4,id5,id6,id7,id8)[-1];    
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7,ixs8)[-1]; 
}else if(inumber==9)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9);
  sdf<-c(NA,id1,id2,id3,id4,id5,id6,id7,id8,id9)[-1];    
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7,ixs8,ixs9)[-1]; 
}else if(inumber==10)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10);
  sdf<-c(NA,id1,id2,id3,id4,id5,id6,id7,id8,id9,id10)[-1];    
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7,ixs8,ixs9,ixs10)[-1]; 
}else if(inumber==11)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10,ixn11)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10,ixv11); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10,ixv11);
  sdf<-c(NA,id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11)[-1];    
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7,ixs8,ixs9,ixs10,ixs11)[-1]; 
}else if(inumber==12)  {
  sv<-as.matrix(idata[,c(ixn1,ixn2,ixn3,ixn4,ixn5,ixn6,ixn7,ixn8,ixn9,ixn10,ixn11,ixn12)]); 
  svname<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10,ixv11,ixv12); 
  svar<-c(ixv1,ixv2,ixv3,ixv4,ixv5,ixv6,ixv7,ixv8,ixv9,ixv10,ixv11,ixv12);
  sdf<-c(NA,id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11,id12)[-1];    
  slv<-c(NA,ixs1,ixs2,ixs3,ixs4,ixs5,ixs6,ixs7,ixs8,ixs9,ixs10,ixs11,ixs12)[-1]; 
}

av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-4;parm<-c(NA, NA, NA, NA); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## lavaan mnormt pbivnorm quadprog ##R package##;
printTxt<-function(txt,cname,rname) {
  tmp<-as.matrix(txt,ncol=1); rname1<-rname
  if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
  colnames(tmp)=cname;  rownames(tmp)=rname1; print(tmp,quote=F)
}
indicators<-unique(sdf)
n.indicators<-length(indicators)
mdl.xx<-NA
for (i in (1:n.indicators)) {mdl.xx<- c(mdl.xx, paste(indicators[i],"=~",paste(svname[sdf==indicators[i]],collapse="+")))}
mdl.xx<-mdl.xx[-1]
mdl.str<-paste(mdl.xx,collapse=" \n ")
cfa.fit<-cfa(mdl.str,data=WD)

sink(paste(ofname,".htm",sep=""))
printTxt("<html><head><meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>","","")
printTxt("<h2>Latent variable analysis (CFA)</h2>","","")
printTxt("<textarea rows=40 cols=100>","","")
printTxt(c(mdl.xx," "," "),"","")

print(summary(cfa.fit, fit.measures = TRUE))
printTxt("</textarea>","","")
printTxt("</body></html>","","")
sink()