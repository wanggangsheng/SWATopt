
# setwd("/Users/wg4/Dropbox (ORNL)/Rcode")
# source("TRB_VAR_CORR.R")
library(corrplot)
# library(sensitivity)
# parfile<-"TRB_HUC8_RunoffPAR.dat"
cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}


varfile<-"./data/TRB_SUB_WQ.dat"

# varfile<-"TRB_SUB_PROP_ALL.dat"

# varfile<-"NP_SWAT_SPARROW.dat"

# parfile<-"TRB_SUB_WQ.dat"
swat_var<-read.table(varfile,header=TRUE)
ncol_var = ncol(swat_var)
#par_mean<-aggregate(mendpar[,2:ncol_par],by=list(mendpar$Soil),FUN=mean)
#par_sd<-aggregate(mendpar[,2:ncol_par],by=list(mendpar$Soil),FUN=sd)
#par_median<-aggregate(mendpar[,2:ncol_par],by=list(mendpar$Soil),FUN=median)
var<-swat_var[,2:ncol_var]
corr<-cor(var)
res1 <- cor.mtest(var, 0.95)

col_usr <- c("darkgreen","green","greenyellow","yellow","gray","antiquewhite","skyblue","cornflowerblue","blue","darkblue")
# col_usr <- c("green4","greenyellow","gray","skyblue","blue")

pdf("./fig/TRB_VAR_CORR.pdf")
corrplot.mixed(corr,p.mat = res1[[1]],insig="blank",col=col_usr)
## add all p-values
# corrplot(corr, p.mat = res1[[1]], insig = "p-value", sig.level = -1)
dev.off()
corrplot.mixed(corr,p.mat = res1[[1]],insig="blank",col=col_usr)
## add all p-values
# corrplot(corr, p.mat = res1[[1]], insig = "p-value", sig.level = -1)

# HUC8file<-"TRB_HUC8_PROP.dat"
# HUC8file<-"TRB_SUB_PROP.dat"
# # HUC8file<-"TRB_SUB_PROP4WQ.dat"
# HUC8_prop<-read.table(HUC8file,header=TRUE)
# # mendsoil<-soil[,-6]
# ncol_prop<-ncol(HUC8_prop)

# #fac: factors; par: parameters
# npar = ncol_par - 1
# nfac = ncol_prop - 1
# fac = HUC8_prop[,2:ncol_prop]
# par = swat_par[,2:ncol_par]
# #par<-par_mean
# #par<-par_median
# cor1<-cor(fac,par)
# pvalue=matrix(nrow=nfac,ncol=npar)
# for(i in 1:(nfac)) {
# 	for(j in 1:(npar)){
# 		test<-cor.test(fac[,i],par[,j])
# 		pvalue[i,j]=test$p.value
# 	}
# }

# colnames(pvalue)<-colnames(cor1)
# rownames(pvalue)<-rownames(cor1)


# sig=matrix(nrow=nfac,ncol=npar)
# colnames(sig)<-colnames(cor1)
# rownames(sig)<-rownames(cor1)

# for(i in 1:(nfac)) {
# 	for(j in 1:(npar)){
# 		if(pvalue[i,j]<=0.05){
# 			sig[i,j]='**'
# 		} 	else if(pvalue[i,j]<=0.1){
# 				sig[i,j]='*'
# 			}	else {
# 				sig[i,j]='-'
# 			}
# 	}
# }

# print(sig,quote=FALSE)
# print(round(cor1,2))
# print(round(pvalue,2))



# srrc<-matrix(,nrow=nfac,ncol=npar)
# for(j in 1:npar){
# 	par0 <- par[,j]
# 	srrc0<-src(X=fac,y=par0, nboot=0,rank=TRUE)  ##SRCC: Standardized Rank Correlation Coefficient
# 	srrc[,j]<-srrc0[7][[1]][,1]
# }
# rownames(srrc)<-colnames(fac)
# colnames(srrc)<-colnames(par)

# prcc<-matrix(,nrow=nfac,ncol=npar)  
# for(j in 1:npar){
# 	par0 <- par[,j]
# 	prcc0<-pcc(X=fac,y=par0, rank=TRUE,nboot=0, conf=0.95)  ##PRCC: Partial Rank Correlation Coefficients
# 	prcc[,j]<-prcc0[7][[1]][,1]
# }
# rownames(prcc)<-colnames(fac)
# colnames(prcc)<-colnames(par)


# # col_usr <- c("darkgreen","green","greenyellow","antiquewhite","gray","gray50","skyblue","cornflowerblue","blue","darkblue")
# # col_usr <- c("green","antiquewhite","gray","skyblue","blue")
# # corrplot(cor1,is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=11,col=col_usr)
# # corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=11,col=col_usr)
# # corrplot(cor1,is.corr=TRUE,method="number", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=6,col=col_usr,add=TRUE)

# col_usr <- c("green4","greenyellow","gray","skyblue","blue")
# pdf("TRB_PAR_corr.pdf")
# # tiff("TRB_PAR_corr.tiff",width = 6, height = 6, units = 'in', res = 300, compression = 'lzw')
# # bitmap("TRB_PAR_analysis.tiff", height = 6, width = 6, units = 'in', type="tifflzw", res=300)
# corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=6,col=col_usr)
# dev.off()
# corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=6,col=col_usr)
