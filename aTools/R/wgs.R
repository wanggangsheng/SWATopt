trim <-
function(x) {
sub(" *([^ ]+) *", "\\1", x) 
}
#===================================================================================================
trimLR <-
function(x) {
sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
}
#===================================================================================================
NSE <-
function(yobs,ysim) {
n <- length(yobs)
ym_obs <- mean(yobs)
sum1 <- sum((yobs-ym_obs)^2)
sum2 <- sum((ysim-yobs)^2)
RSO<-mean(ysim)/mean(yobs)
NSEC <- 1.0-sum2/sum1
dret<-data.frame(n,RSO,NSEC)
return(dret) 
}
#===================================================================================================
cluster.no <-
function(mydata){
# Determine number of clusters
nRow<-nrow(mydata)
wss <- (nRow-1)*sum(apply(mydata,2,var))
for (i in 2:nRow-1) wss[i] <- sum(kmeans(mydata,
   centers=i)$withinss)
plot(1:(nRow-1), wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares") 
}
#===================================================================================================
histNorm <-
function(x,nbreaks,xlab,...){
h1<-hist(x,breaks=nbreaks,type="count",col="red",xlab=xlab,ylab="Frequency",main = NULL, border=TRUE)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h1$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 
}
#===================================================================================================
bar.compare <-
function(x,xylab,model1,std=TRUE, group=TRUE, horiz=FALSE, LSD=TRUE,diffLetter,fontSize,...) {
y<-x[,2]

#----------------------
    A1<-model1$model
    y1<-A1[,1]
    ipch<-pmatch(xylab[1],names(A1))
    if( is.na(ipch)) return(cat("Name: ",treatment,"\n",names(A1)[-1],"\n"))
    #name.t <-names(A)[ipch]
    trt<-A1[,ipch]
    #name.y <- names(A)[1]
    
    junto <- subset(data.frame(y1, trt), is.na(y1) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat="mean") #change
    sds <- tapply.stat(junto[, 1], junto[, 2], stat="sd")     #change
    nn <- tapply.stat(junto[, 1], junto[, 2], stat="length")  #change

	o1=order(means[,2],decreasing = TRUE)
	nn1 = rbind(nn[o1,2])
	nn1[1]<-paste("n=",nn1[1])
#----------------------

names(y)<-paste(x[,1],"\n",nn1)
if( std ) {
nivel0<-x[,2]-x[,5]*sqrt(x[,4])
nivel1<-x[,2]+x[,5]*sqrt(x[,4])
}
else {
nivel0<-x[,2]-x[,5]
nivel1<-x[,2]+x[,5]
}
n<-length(y)
tope<-max(nivel1)/20
Ytop = max(nivel1)+tope
Ybottom = min(nivel1)-tope
if(Ybottom>0) {
indice<-barplot(y,horiz=horiz, xlab=xylab[1], ylab=xylab[2],ylim = c(0,Ytop),cex.names=fontSize,cex.axis=fontSize,cex.lab=fontSize,...)
}
else {
indice<-barplot(y,horiz=horiz, xlab=xylab[1], ylab=xylab[2],ylim = c(Ybottom,Ytop),cex.names=fontSize, cex.axis=fontSize,cex.lab=fontSize,...)
}

for ( i in 1:n) {
if (horiz)  {
lines(rbind(c(nivel0[i],indice[i]),c(nivel1[i],indice[i])),col="red")
text( cex=1,nivel0[i],indice[i],"[")
text( cex=1,nivel1[i],indice[i],"]")
}
else {
lines(rbind(c(indice[i],nivel0[i]),c(indice[i],nivel1[i])),col="red")
text( cex=1,indice[i],nivel0[i],"---")
text( cex=1,indice[i],nivel1[i],"---")
}
}
if(group) {
if(!LSD){
x[,3]<-diffLetter
}
for ( i in 1:n) 
text(indice[i],nivel1[i]+tope*0.5,trimLR(x[i,3]),cex=fontSize)
}
}

#===================================================================================================
LSD1.test <-
function (y, trt, DFerror, MSerror, alpha = 0.05, p.adj = c("none",
    "holm", "hochberg", "bonferroni", "BH", "BY", "fdr"), group = TRUE,
    main = NULL)
{
    p.adj <- match.arg(p.adj)
    clase<-c("aov","lm")
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    y<-A[,1]
    ipch<-pmatch(trt,names(A))
    if( is.na(ipch)) return(cat("Name: ",trt,"\n",names(A)[-1],"\n"))
    name.t <-names(A)[ipch]
    trt<-A[,ipch]
    name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat="mean") #change
    sds <- tapply.stat(junto[, 1], junto[, 2], stat="sd")     #change
    nn <- tapply.stat(junto[, 1], junto[, 2], stat="length")  #change
    std.err <- sds[, 2]/sqrt(nn[, 2])
    Tprob <- qt(1 - alpha/2, DFerror)
    LCL <- means[,2]-Tprob*std.err
    UCL <- means[,2]+Tprob*std.err
    means <- data.frame(means, std.err, replication = nn[, 2],
    LCL, UCL)
    names(means)[1:2] <- c(name.t, name.y)
    #row.names(means) <- means[, 1]
    ntr <- nrow(means)
    nk <- choose(ntr, 2)
    if (p.adj != "none")
        {
        a <- 1e-06
        b <- 1
        for (i in 1:100) {
            x <- (b + a)/2
            d <- p.adjust(x, n = nk, p.adj) - alpha
            fa <- p.adjust(a, n = nk, p.adj) - alpha
            if (d * fa < 0)
                b <- x
            if (d * fa > 0)
                a <- x
        }
        Tprob <- qt(1 - x/2, DFerror)
    }
    nr <- unique(nn[, 2])
    cat("\nwgsStudy:", main)
    cat("\n\nLSD t Test for", name.y, "\n")
    if (p.adj != "none")
        cat("P value adjustment method:", p.adj, "\n")
    cat("\nMean Square Error: ",MSerror,"\n\n")
    cat(paste(name.t,",",sep="")," means and individual (",(1-alpha)*100,"%) CI\n\n")
    print(data.frame(row.names = means[,1], means[,-1]))
    cat("\nalpha:",alpha,"; Df Error:",DFerror)
    cat("\nCritical Value of t:", Tprob,"\n")
    if (group) {
        if (length(nr) == 1) {
            LSD <- Tprob * sqrt(2 * MSerror/nr)
            cat("\nLeast Significant Difference", LSD)
        }
        else {
            nr1 <- 1/mean(1/nn[, 2])
            LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
            cat("\nLeast Significant Difference", LSD1)
            cat("\nHarmonic Mean of Cell Sizes ", nr1)
            }
        cat("\nMeans with the same letter are not significantly different.")
        cat("\n\nGroups, Treatments and means\n")
        output <- order1.group(means[, 1], means[, 2], means[,
            4], MSerror, Tprob, means[, 3])
        w<-order(means[,2],decreasing = TRUE)
        output <- data.frame(output,LCI=means[w,5],UCI=means[w,6])
    }
    if (!group) {
        comb <- combn(ntr, 2)
        nn <- ncol(comb)
        dif <- rep(0, nn)
        LCL1<-dif
		UCL1<-dif
        sig<-NULL
        pvalue <- rep(0, nn)
        for (k in 1:nn) {
            i <- comb[1, k]
            j <- comb[2, k]
            if (means[i, 2] < means[j, 2]){
            comb[1, k]<-j
            comb[2, k]<-i
            }
            dif[k] <- abs(means[i, 2] - means[j, 2])
            sdtdif <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,
                4]))
            pvalue[k] <- 2 * (1 - pt(dif[k]/sdtdif, DFerror))
            if (p.adj != "none")
                pvalue[k] <- p.adjust(pvalue[k], n = nk, p.adj)
                pvalue[k] <- round(pvalue[k],6)
        LCL1[k] <- dif[k] - Tprob*sdtdif
		UCL1[k] <- dif[k] + Tprob*sdtdif
        sig[k]<-" "
        if (pvalue[k] <= 0.001) sig[k]<-"***"
        else  if (pvalue[k] <= 0.01) sig[k]<-"**"
        else  if (pvalue[k] <= 0.05) sig[k]<-"*"
        else  if (pvalue[k] <= 0.1) sig[k]<-"."
        }
        tr.i <- means[comb[1, ],1]
        tr.j <- means[comb[2, ],1]
        output<-data.frame("Difference" = dif, pvalue = pvalue,sig,LCL=LCL1,UCL=UCL1)
        rownames(output)<-paste(tr.i,tr.j,sep=" - ")
        cat("\nComparison between treatments means\n\n")
        print(output)
        output <- data.frame(trt = means[, 1], means = means[,
            2], M = "", N = means[, 4], std.err ,LCL,UCL)
    }
    invisible(output)
    }
	
#=============================================================================
order1.group <-
function(trt,means,N,MSerror,Tprob,std.err,parameter=1, snk=0, DFerror=NULL,alpha=NULL,sdtdif=NULL) {
N<-rep(1/mean(1/N),length(N))
n<-length(means)
z<-data.frame(trt,means,N,std.err)
# ordena tratamientos
w<-z[order(z[,2],decreasing = TRUE), ]
M<-rep("",n)
k<-1
j<-1
k<-1
cambio<-n
cambio1<-0
chequeo=0
M[1]<-letters[k]
while(j<n) {
chequeo<-chequeo+1
if (chequeo > n) break
for(i in j:n) {
nx<-abs(i-j)+1
if (nx==1) nx=2
if(snk ==1 ) Tprob <- qtukey(p=1-alpha,nmeans=nx, df=DFerror)
if(snk ==2 ) Tprob <- qtukey(p=(1-alpha)^(nx-1),nmeans=nx, df=DFerror)
if(is.null(sdtdif))  minimo<-Tprob*sqrt(parameter*MSerror*(1/N[i]+1/N[j]))
if(!is.null(sdtdif)) minimo<-Tprob*sdtdif
s<-abs(w[i,2]-w[j,2])<=minimo
if(s) {
if(lastC(M[i]) != letters[k])M[i]<-paste(M[i],letters[k],sep="")
}
else {
k<-k+1
cambio<-i
cambio1<-0
ja<-j
for(jj in cambio:n) M[jj]<-paste(M[jj]," ",sep="")
M[cambio]<-paste(M[cambio],letters[k],sep="")
for( v in ja:cambio) {
nx<-abs(v-cambio)+1
if(nx == 1)  nx=2
if(snk ==1 ) Tprob <- qtukey(p=1-alpha,nmeans=nx, df=DFerror)
if(snk ==2 ) Tprob <- qtukey(p=(1-alpha)^(nx-1),nmeans=nx, df=DFerror)
if(is.null(sdtdif))  minimo<-Tprob*sqrt(parameter*MSerror*(1/N[i]+1/N[j]))
if(!is.null(sdtdif)) minimo<-Tprob*sdtdif
if(abs(w[v,2]-w[cambio,2])>minimo) {j<-j+1

cambio1<-1
}
else break
}
break
}
}
if (cambio1 ==0 )j<-j+1
}
#-----
w<-data.frame(w,stat=M)
trt<-as.character(w$trt)
means<-as.numeric(w$means)
N<-as.numeric(w$N)
std.err<- as.numeric(w$std.err)
cmax<-max(nchar(trt))
trt<-paste(trt,"                                      ")
trt<-substr(trt,1,cmax)
for(i in 1:n){
cat(M[i],"\t",trt[i],"\t",means[i],"\n")
}
output<-data.frame(trt,means,M,N,std.err)
return(output)
}

#=============================================================================
KW.test <- function (resp, categ, probs = 0.05, cont = NULL,...) 
{
		db<-na.omit(data.frame(resp,categ))
		if(nrow(db)!=length(resp)) warning(paste(length(resp)-nrow(db),"lines including NA have been omitted"))
		resp<-db[,1]
		categ<-db[,2]
    lst <- split(rank(resp), categ)
	avg <- eval(bquote(tapply(.(as.name(colnames(db)[1])),.(as.name(colnames(db)[2])),mean)))
    name <- names(lst)
    R <- sapply(lst, mean)
    n <- sapply(lst, length)
	n<-n[order(avg,decreasing = TRUE)]
	R<-R[order(avg,decreasing = TRUE)]
	avg<-avg[order(avg,decreasing = TRUE)]
	name<-names(avg)
    N = length(resp)
    dif <- abs(outer(R, R, "-"))
    if (is.null(cont)) {
        difv <- NULL
        vname <- NULL
        indices <- NULL
        for (i in 1:(length(name) - 1)) {
            for (j in (i + 1):length(name)) {
                vname <- c(vname, paste(name[i], "-", name[j], sep = ""))
                indices <- rbind(indices, c(i, j))
                difv<-c(difv,dif[i,j])
            }
        }
        names(difv) <- vname
        z <- qnorm(probs/(length(lst) * (length(lst) - 1)), lower.tail = FALSE)
        lims <- z * sqrt(N * (N + 1)/12 * (1/n[indices[1:length(vname),1]] + 1/n[indices[1:length(vname), 2]]))
        names(lims) <- vname
        stat <- "Multiple comparison test after Kruskal-Wallis"
    }
    else {
        vname = NULL
        indices = NULL
        for (j in 2:length(dif[1, ])) {
            vname <- c(vname, paste(name[1], "-", name[j], sep = ""))
            indices <- rbind(indices, c(1, j))
        }
        dif <- dif[1, 2:length(dif[1, ])]
        names(dif) <- vname
        difv<-dif
        choice <- pmatch(cont, c("one-tailed", "two-tailed"), 
            nomatch = 3)
        if (choice == 1) {
            z <- qnorm(probs/2 * (length(lst) - 1), lower.tail = FALSE)
            lims <- z * sqrt(N * (N + 1)/12 * (1/n[indices[1:length(vname), 
                1]] + 1/n[indices[1:length(vname), 2]]))
            names(lims) <- vname
            stat <- "Multiple comparison test after Kruskal-Wallis, treatments vs control (one-tailed)"
        }
        if (choice == 2) {
            z <- qnorm(probs/(length(lst) - 1), lower.tail = FALSE)
            lims <- z * sqrt(N * (N + 1)/12 * (1/n[indices[1:length(vname), 
                1]] + 1/n[indices[1:length(vname), 2]]))
            names(lims) <- vname
            stat <- "Multiple comparison test after Kruskal-Wallis, treatment vs control (two-tailed)"
        }
        if (choice == 3) 
            stop("Values must be 'one-tailed' or 'two-tailed', partial matching accepted")
    }
    # output <- list(statistic = stat, p.value = probs, dif.com = data.frame(obs.dif = difv, 
       # critical.dif = lims, difference = ifelse((difv - lims) > 0, TRUE, FALSE)))
	sig<-ifelse((difv - lims) > 0, TRUE, FALSE)
	output <- data.frame(vname,difv, lims, sig)
	row.names(output)<-NULL
    #class(output) <- c("mc", "list")
    output
}
#=============================================================================
KW.group <-
function(LSD,KWtest)#(trt,means,N,MSerror,Tprob,std.err,parameter=1, snk=0, DFerror=NULL,alpha=NULL,sdtdif=NULL) {
{
n<-nrow(LSD)    #LSD already sorted
nKW<-nrow(KWtest)
M<-rep("",n)
k<-1
j<-1
cambio<-n
cambio1<-0
chequeo=0
M[1]<-letters[k]
trt<-trimLR(LSD[,1])
while(j<n) {
chequeo<-chequeo+1
if (chequeo > n) break
for(i in (j+1):n) {
vname <- paste(trt[j], "-", trt[i], sep = "")#c(paste(trt[j], "-", trt[i], sep = ""),paste(trt[i], "-", trt[j], sep = ""))
for(iKW in (0.5*(j-1)*(2*n-j)+1):(0.5*j*(2*n-j-1))){  #1:nKW
if(vname==trimLR(KWtest[iKW,1])){
s<-KWtest[iKW,4]
# cat(paste(iKW,"\t",s,"\n"))
break}  #if vname
} #end for iKW

if(s) #s=TRUE, difference 
{
if(M[i]!="") break  #&!is.null(match(lastC(M[i]),letters))
else{
k<-k+1
if(lastC(M[i]) != letters[k]) M[i]<-paste(M[i],letters[k],sep="")#M[cambio]<-paste(M[cambio],letters[k],sep="")
# cat(paste("\t",k,"\t",j,"\t",i,"\t",M[j],"\t",M[i],"\n"))
break
}
}
else {  #s=FALSE, no difference
bSame<-NsameLetters(M[j],M[i])
# cat("\t",bSame,"\t")
if(!bSame) {
if(M[i]!=""){
 M[j]<-paste(M[j],lastC(M[i]),sep="")}
else{
if(lastC(M[j]) != letters[k]) M[j]<-paste(M[j],letters[k],sep="")
if(lastC(M[i]) != letters[k]) M[i]<-paste(M[i],letters[k],sep="")
# cat(paste("\t",k,"\t",j,"\t",i,"\t",M[j],"\t",M[i],"\n"))
} #else
} #for i
}
}
j<-j+1
} #while j

return(M)
}
#=============================================================================
NsameLetters<-
function(str1,str2)
{
bSame<-FALSE
n1<-nchar(as.character(str1))
n2<-nchar(as.character(str2))
char1<-strsplit(str1,NULL)
char2<-strsplit(str2,NULL)
if(n1<1||n2<1) return(bSame)
for(i in 1:n1){
for(j in 1:n2){
if(char1[[1]][i]==char2[[1]][j]){
bSame<-TRUE
break
}
}
}
return(bSame)
}#function
#=============================================================================
matrixplot<-
function (corr, method = c("circle", "square", "ellipse", "number", 
    "shade", "color", "pie"), type = c("full", "lower", "upper"), 
    add = FALSE, col = NULL, bg = "white", title = "", is.corr = TRUE, 
    diag = TRUE, outline = FALSE, mar = c(0, 0, 0, 0), addgrid.col = NULL, 
    addCoef.col = NULL, addCoefasPercent = FALSE, order = c("original", 
        "AOE", "FPC", "hclust", "alphabet"), hclust.method = c("complete", 
        "ward", "single", "average", "mcquitty", "median", "centroid"), 
    addrect = NULL, rect.col = "black", rect.lwd = 2, tl.pos = NULL, 
    tl.cex = 1, tl.col = "red", tl.offset = 0.4, tl.srt = 90, 
    cl.pos = NULL, cl.lim = NULL, cl.length = NULL, cl.cex = 0.8, 
    cl.ratio = 0.15, cl.align.text = "c", cl.offset = 0.5, addshade = c("negative", 
        "positive", "all"), shade.lwd = 1, shade.col = "white", 
    p.mat = NULL, sig.level = 0.05, insig = c("pch", "p-value", 
        "blank", "n"), pch = 4, pch.col = "black", pch.cex = 3, 
    plotCI = c("n", "square", "circle", "rect"), lowCI.mat = NULL, 
    uppCI.mat = NULL, ...) 
{
    method <- match.arg(method)
    type <- match.arg(type)
    order <- match.arg(order)
    hclust.method <- match.arg(hclust.method)
    plotCI <- match.arg(plotCI)
    insig <- match.arg(insig)
    if (!is.matrix(corr) & !is.data.frame(corr)) 
        stop("Need a matrix or data frame!")
    if (is.null(addgrid.col)) {
        addgrid.col <- ifelse(method == "color" | method == "shade", 
            "white", "grey")
    }
    if (any(corr < cl.lim[1]) | any(corr > cl.lim[2])) 
        stop("color limits should cover matrix")
    if (is.null(cl.lim)) {
        if (is.corr) 
            cl.lim <- c(-1, 1)
        if (!is.corr) 
            cl.lim <- c(min(corr), max(corr))
    }
    intercept <- 0
    zoom <- 1
    if (!is.corr) {
        if (max(corr) * min(corr) < 0) {
            intercept <- 0
            zoom <- 1/max(abs(cl.lim))
        }
        if (min(corr) >= 0) {
            intercept <- -cl.lim[1]
            zoom <- 1/(diff(cl.lim))
        }
        if (max(corr) <= 0) {
            intercept <- -cl.lim[2]
            zoom <- 1/(diff(cl.lim))
        }
        corr <- (intercept + corr) * zoom
    }
    cl.lim2 <- (intercept + cl.lim) * zoom
    int <- intercept * zoom
    if (min(corr) < -1 - .Machine$double.eps || max(corr) > 1 + 
        .Machine$double.eps) {
        stop("The matrix is not in [-1, 1]!")
    }
    if (is.null(col)) {
        col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
            "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
            "#4393C3", "#2166AC", "#053061"))(200)
    }
    n <- nrow(corr)
    m <- ncol(corr)
    min.nm <- min(n, m)
    ord <- 1:min.nm
    if (!order == "original") {
        ord <- corrMatOrder(corr, order = order, hclust.method = hclust.method)
        corr <- corr[ord, ord]
    }
    if (is.null(rownames(corr))) 
        rownames(corr) <- 1:n
    if (is.null(colnames(corr))) 
        colnames(corr) <- 1:m
    getPos.Dat <- function(mat) {
        x <- matrix(1:n * m, n, m)
        tmp <- mat
        if (type == "upper") 
            tmp[row(x) > col(x)] <- Inf
        if (type == "lower") 
            tmp[row(x) < col(x)] <- Inf
        if (type == "full") 
            tmp <- tmp
        if (!diag) 
            diag(tmp) <- Inf
        Dat <- tmp[is.finite(tmp)]
        ind <- which(is.finite(tmp), arr.ind = TRUE)
        Pos <- ind
        Pos[, 1] <- ind[, 2]
        Pos[, 2] <- -ind[, 1] + 1 + n
        return(list(Pos, Dat))
    }
    Pos <- getPos.Dat(corr)[[1]]
    n2 <- max(Pos[, 2])
    n1 <- min(Pos[, 2])
    nn <- n2 - n1
    newrownames <- as.character(rownames(corr)[(n + 1 - n2):(n + 
        1 - n1)])
    m2 <- max(Pos[, 1])
    m1 <- min(Pos[, 1])
    mm <- m2 - m1
    newcolnames <- as.character(colnames(corr)[m1:m2])

    # cat("n=",n,"\n")
    # cat("m=",m,"\n")
    # mat1<-getPos.Dat(corr)
    # print(corr)
    # print(mat1)
    # cat("newrownames=",newrownames,"\n")
    # cat("newcolnames=",newcolnames,"\n")

    DAT <- getPos.Dat(corr)[[2]]
    len.DAT <- length(DAT)
    assign.color <- function(DAT) {
        newcorr <- (DAT + 1)/2
        newcorr[newcorr == 1] <- 1 - 1e-10
        col.fill <- col[floor(newcorr * length(col)) + 1]
    }
    col.fill <- assign.color(DAT)
    isFALSE = function(x) identical(x, FALSE)
    isTRUE = function(x) identical(x, TRUE)
    if (isFALSE(tl.pos)) {
        tl.pos <- "n"
    }
    if (is.null(tl.pos) | isTRUE(tl.pos)) {
        if (type == "full") 
            tl.pos <- "lt"
        if (type == "lower") 
            tl.pos <- "ld"
        if (type == "upper") 
            tl.pos <- "td"
    }
    if (isFALSE(cl.pos)) {
        cl.pos <- "n"
    }
    if (is.null(cl.pos) | isTRUE(cl.pos)) {
        if (type == "full") 
            cl.pos <- "r"
        if (type == "lower") 
            cl.pos <- "b"
        if (type == "upper") 
            cl.pos <- "r"
    }
    if (outline) 
        col.border <- "black"
    if (!outline) 
        col.border <- col.fill
    if (!add) {
        par(mar = mar, bg = "white")
        plot.new()
        xlabwidth <- ylabwidth <- 0
        for (i in 1:50) {
            xlim <- c(m1 - 0.5 - xlabwidth, m2 + 0.5 + mm * cl.ratio * 
                (cl.pos == "r"))
            ylim <- c(n1 - 0.5 - nn * cl.ratio * (cl.pos == "b"), 
                n2 + 0.5 + ylabwidth)
            plot.window(xlim + c(-0.2, 0.2), ylim + c(-0.2, 0.2), 
                asp = 1, xaxs = "i", yaxs = "i")
            x.tmp <- max(strwidth(newrownames, cex = tl.cex))
            y.tmp <- max(strwidth(newcolnames, cex = tl.cex))
            if (min(x.tmp - xlabwidth, y.tmp - ylabwidth) < 1e-04) 
                break
            xlabwidth <- x.tmp
            ylabwidth <- y.tmp
        }
        if (tl.pos == "n" | tl.pos == "d") 
            xlabwidth <- ylabwidth <- 0
        if (tl.pos == "td") 
            ylabwidth <- 0
        if (tl.pos == "ld") 
            xlabwidth <- 0
        laboffset <- strwidth("W", cex = tl.cex) * tl.offset
        xlim <- c(m1 - 0.5 - xlabwidth - laboffset, m2 + 0.5 + 
            mm * cl.ratio * (cl.pos == "r")) + c(-0.35, 0.15)
        ylim <- c(n1 - 0.5 - nn * cl.ratio * (cl.pos == "b"), 
            n2 + 0.5 + ylabwidth * abs(sin(tl.srt * pi/180)) + 
                laboffset) + c(-0.15, 0.35)
        if (.Platform$OS.type == "windows") {
            windows.options(width = 7, height = 7 * diff(ylim)/diff(xlim))
        }
        plot.window(xlim = xlim, ylim = ylim, asp = 1, xlab = "", 
            ylab = "", xaxs = "i", yaxs = "i")
    }
    laboffset <- strwidth("W", cex = tl.cex) * tl.offset
    symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1, 
        len.DAT), bg = bg, fg = bg)
    if (method == "circle" & plotCI == "n") {
        symbols(Pos, add = TRUE, inches = FALSE, bg = col.fill, 
            circles = 0.9 * abs(DAT)^0.5/2, fg = col.border)
    }
    if (method == "ellipse" & plotCI == "n") {
        ell.dat <- function(rho, length = 99) {
            k <- seq(0, 2 * pi, length = length)
            x <- cos(k + acos(rho)/2)/2
            y <- cos(k - acos(rho)/2)/2
            return(cbind(rbind(x, y), c(NA, NA)))
        }
        ELL.dat <- lapply(DAT, ell.dat)
        ELL.dat2 <- 0.85 * matrix(unlist(ELL.dat), ncol = 2, 
            byrow = TRUE)
        ELL.dat2 <- ELL.dat2 + Pos[rep(1:length(DAT), each = 100), 
            ]
        polygon(ELL.dat2, border = col.border, col = col.fill)
    }
    if (method == "number" & plotCI == "n") {
        text(Pos[, 1], Pos[, 2], font = 2, col = col.fill, labels = round((DAT - 
            int) * ifelse(addCoefasPercent, 100, 1)/zoom, ifelse(addCoefasPercent, 
            0, 2)))
    }
    if (method == "pie" & plotCI == "n") {
        symbols(Pos, add = TRUE, inches = FALSE, circles = rep(0.5, 
            len.DAT) * 0.85)
        pie.dat <- function(theta, length = 100) {
            k <- seq(pi/2, pi/2 - theta, length = 0.5 * length * 
                abs(theta)/pi)
            x <- c(0, cos(k)/2, 0)
            y <- c(0, sin(k)/2, 0)
            return(cbind(rbind(x, y), c(NA, NA)))
        }
        PIE.dat <- lapply(DAT * 2 * pi, pie.dat)
        len.pie <- unlist(lapply(PIE.dat, length))/2
        PIE.dat2 <- 0.85 * matrix(unlist(PIE.dat), ncol = 2, 
            byrow = TRUE)
        PIE.dat2 <- PIE.dat2 + Pos[rep(1:length(DAT), len.pie), 
            ]
        polygon(PIE.dat2, border = "black", col = col.fill)
    }
    if (method == "shade" & plotCI == "n") {
        addshade <- match.arg(addshade)
        symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1, 
            len.DAT), bg = col.fill, fg = addgrid.col)
        shade.dat <- function(w) {
            x <- w[1]
            y <- w[2]
            rho <- w[3]
            x1 <- x - 0.5
            x2 <- x + 0.5
            y1 <- y - 0.5
            y2 <- y + 0.5
            dat <- NA
            if ((addshade == "positive" || addshade == "all") & 
                rho > 0) {
                dat <- cbind(c(x1, x1, x), c(y, y1, y1), c(x, 
                  x2, x2), c(y2, y2, y))
            }
            if ((addshade == "negative" || addshade == "all") & 
                rho < 0) {
                dat <- cbind(c(x1, x1, x), c(y, y2, y2), c(x, 
                  x2, x2), c(y1, y1, y))
            }
            return(t(dat))
        }
        pos_corr <- rbind(cbind(Pos, DAT))
        pos_corr2 <- split(pos_corr, 1:nrow(pos_corr))
        SHADE.dat <- matrix(na.omit(unlist(lapply(pos_corr2, 
            shade.dat))), byrow = TRUE, ncol = 4)
        segments(SHADE.dat[, 1], SHADE.dat[, 2], SHADE.dat[, 
            3], SHADE.dat[, 4], col = shade.col, lwd = shade.lwd)
    }
    if (method == "square" & plotCI == "n") {
        symbols(Pos, add = TRUE, inches = FALSE, squares = abs(DAT)^0.5, 
            bg = col.fill, fg = col.border)
    }
    if (method == "color" & plotCI == "n") {
        symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1, 
            len.DAT), bg = col.fill, fg = col.border)
    }
    symbols(Pos, add = TRUE, inches = FALSE, bg = NA, squares = rep(1, 
        len.DAT), fg = addgrid.col)
    if (plotCI != "n") {
        if (is.null(lowCI.mat) || is.null(uppCI.mat)) 
            stop("Need lowCI.mat and uppCI.mat!")
        if (!order == "original") {
            lowCI.mat <- lowCI.mat[ord, ord]
            uppCI.mat <- uppCI.mat[ord, ord]
        }
        pos.lowNew <- getPos.Dat(lowCI.mat)[[1]]
        lowNew <- getPos.Dat(lowCI.mat)[[2]]
        pos.uppNew <- getPos.Dat(uppCI.mat)[[1]]
        uppNew <- getPos.Dat(uppCI.mat)[[2]]
        if (!(method == "circle" || method == "square")) 
            stop("method shoud be circle or square if draw confidence interval!")
        k1 <- (abs(uppNew) > abs(lowNew))
        bigabs <- uppNew
        bigabs[which(!k1)] <- lowNew[!k1]
        smallabs <- lowNew
        smallabs[which(!k1)] <- uppNew[!k1]
        sig <- sign(uppNew * lowNew)
        if (plotCI == "circle") {
            symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE, 
                inches = FALSE, circles = 0.95 * abs(bigabs)^0.5/2, 
                bg = ifelse(sig > 0, col.fill, col[ceiling((bigabs + 
                  1) * length(col)/2)]), fg = ifelse(sig > 0, 
                  col.fill, col[ceiling((bigabs + 1) * length(col)/2)]))
            symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE, 
                inches = FALSE, circles = 0.95 * abs(smallabs)^0.5/2, 
                bg = ifelse(sig > 0, bg, col[ceiling((smallabs + 
                  1) * length(col)/2)]), fg = ifelse(sig > 0, 
                  col.fill, col[ceiling((smallabs + 1) * length(col)/2)]))
        }
        if (plotCI == "square") {
            symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE, 
                inches = FALSE, squares = abs(bigabs)^0.5, bg = ifelse(sig > 
                  0, col.fill, col[ceiling((bigabs + 1) * length(col)/2)]), 
                fg = ifelse(sig > 0, col.fill, col[ceiling((bigabs + 
                  1) * length(col)/2)]))
            symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE, 
                inches = FALSE, squares = abs(smallabs)^0.5, 
                bg = ifelse(sig > 0, bg, col[ceiling((smallabs + 
                  1) * length(col)/2)]), fg = ifelse(sig > 0, 
                  col.fill, col[ceiling((smallabs + 1) * length(col)/2)]))
        }
        if (plotCI == "rect") {
            rect.width <- 0.25
            rect(pos.uppNew[, 1] - rect.width, pos.uppNew[, 2] + 
                smallabs/2, pos.uppNew[, 1] + rect.width, pos.uppNew[, 
                2] + bigabs/2, col = col.fill, border = col.fill)
            segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 
                2] + DAT/2, pos.lowNew[, 1] + rect.width, pos.lowNew[, 
                2] + DAT/2, col = "black", lwd = 1)
            segments(pos.uppNew[, 1] - rect.width, pos.uppNew[, 
                2] + uppNew/2, pos.uppNew[, 1] + rect.width, 
                pos.uppNew[, 2] + uppNew/2, col = "black", lwd = 1)
            segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 
                2] + lowNew/2, pos.lowNew[, 1] + rect.width, 
                pos.lowNew[, 2] + lowNew/2, col = "black", lwd = 1)
            segments(pos.lowNew[, 1] - 0.5, pos.lowNew[, 2], 
                pos.lowNew[, 1] + 0.5, pos.lowNew[, 2], col = "grey70", 
                lty = 3)
        }
    }
    if (!is.null(p.mat) & !insig == "n") {
        if (!order == "original") 
            p.mat <- p.mat[ord, ord]
        pos.pNew <- getPos.Dat(p.mat)[[1]]
        pNew <- getPos.Dat(p.mat)[[2]]
        ind.p <- which(pNew > (sig.level))
        if (insig == "pch") {
            points(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
                pch = pch, col = pch.col, cex = pch.cex, lwd = 2)
        }
        if (insig == "p-value") {
            text(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
                round(pNew[ind.p], 2), col = pch.col)
        }
        if (insig == "blank") {
            symbols(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
                inches = FALSE, squares = rep(1, length(pos.pNew[, 
                  1][ind.p])), fg = addgrid.col, bg = bg, add = TRUE)
        }
    }
    if (cl.pos != "n") {
        colRange <- assign.color(cl.lim2)
        ind1 <- which(col == colRange[1])
        ind2 <- which(col == colRange[2])
        colbar <- col[ind1:ind2]
        if (is.null(cl.length)) 
            cl.length <- ifelse(length(colbar) > 20, 11, length(colbar) + 
                1)
        labels <- seq(cl.lim[1], cl.lim[2], length = cl.length)
        at <- seq(0, 1, length = length(labels))
        if (cl.pos == "r") {
            vertical <- TRUE
            xlim <- c(m2 + 0.5 + mm * 0.02, m2 + 0.5 + mm * cl.ratio)
            ylim <- c(n1 - 0.5, n2 + 0.5)
        }
        if (cl.pos == "b") {
            vertical <- FALSE
            xlim <- c(m1 - 0.5, m2 + 0.5)
            ylim <- c(n1 - 0.5 - nn * cl.ratio, n1 - 0.5 - nn * 
                0.02)
        }
        print(ylim)
        colorlegend(colbar = colbar, labels = round(labels, 2), 
            offset = cl.offset, ratio.colbar = 0.3, cex = cl.cex, 
            xlim = xlim, ylim = ylim, vertical = vertical, align = cl.align.text)
    }
    if (tl.pos != "n") {
        ylabwidth2 <- strwidth(newrownames, cex = tl.cex)
        xlabwidth2 <- strwidth(newcolnames, cex = tl.cex)
        pos.xlabel <- cbind(m1:m2, n2 + 0.5 + laboffset)
        pos.ylabel <- cbind(m1 - 0.5, n2:n1)
        if (tl.pos == "td") {
            if (type != "upper") 
                stop("type should be \"upper\" if tl.pos is \"dt\".")
            pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
        }
        if (tl.pos == "ld") {
            if (type != "lower") 
                stop("type should be \"lower\" if tl.pos is \"ld\".")
            pos.xlabel <- cbind(m1:m2, n2:(n2 - mm) + 0.5 + laboffset)
        }
        if (tl.pos == "d") {
            pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
            pos.ylabel <- pos.ylabel[1:min(n, m), ]
            symbols(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], add = TRUE, 
                bg = bg, fg = addgrid.col, inches = FALSE, squares = rep(1, 
                  length(pos.ylabel[, 1])))
            text(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], newcolnames[1:min(n, 
                m)], col = tl.col, cex = tl.cex, ...)
        }
        else {
            text(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, 
                srt = tl.srt, adj = ifelse(tl.srt == 0, c(0.5, 
                  0), c(0, 0)), col = tl.col, cex = tl.cex, offset = tl.offset, 
                ...)
            text(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, 
                col = tl.col, cex = tl.cex, pos = 2, offset = tl.offset, 
                ...)
        }
    }
    title(title, ...)
    if (!is.null(addCoef.col) & (!method == "number")) {
        text(Pos[, 1], Pos[, 2], col = addCoef.col, labels = round((DAT - 
            int) * ifelse(addCoefasPercent, 100, 1)/zoom, ifelse(addCoefasPercent, 
            0, 2)))
    }
    if (type == "full" & plotCI == "n" & !is.null(addgrid.col)) 
        rect(m1 - 0.5, n1 - 0.5, m2 + 0.5, n2 + 0.5, border = addgrid.col)
    if (!is.null(addrect) & order == "hclust" & type == "full") {
        corrRect.hclust(corr, k = addrect, method = hclust.method, 
            col = rect.col, lwd = rect.lwd)
    }
    invisible(corr)
} #matrixplot
#=============================================================================
getPos.Dat <- 
function(mat,type = c("full", "lower", "upper"),diag = TRUE,...) {
    n <- nrow(mat)
    m <- ncol(mat)
        x <- matrix(1:n * m, n, m)
        tmp <- mat
        if (type == "upper") 
            tmp[row(x) > col(x)] <- Inf
        if (type == "lower") 
            tmp[row(x) < col(x)] <- Inf
        if (type == "full") 
            tmp <- tmp
        if (!diag) 
            diag(tmp) <- Inf
        Dat <- tmp[is.finite(tmp)]
        ind <- which(is.finite(tmp), arr.ind = TRUE)
        Pos <- ind
        Pos[, 1] <- ind[, 2]
        Pos[, 2] <- -ind[, 1] + 1 + n
        return(list(Pos, Dat))
    }
#=============================================================================