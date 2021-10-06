curve.smoothing <- function(dataset,x,y,method="spline",sp=4,object=NULL,plot=TRUE,col="black",linecol="red",lty=1,lwd=1,title="",subtitle="",xlab="",ylab="")
# smooth a scatter plot
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(x))
	{
	if(is.character(x))
		{
		xname <- x
		x <- eval(parse(text=x),envir=dataset)
		}
	else
		xname <- deparse(substitute(x))
	}
if(!is.null(y))
	{
	if(is.character(y))
		{
		yname <- y
		y <- eval(parse(text=y),envir=dataset)
		}
	else
		yname <- deparse(substitute(y))
	}
if(col == "auto")
	col <- "red"
if(lty == "auto")
	lty <- "solid"
x <- x[order(y,na.last=NA)]
y <- sort(y)
y <- y[order(x,na.last=NA)]
x <- sort(x)
if(length(unique(x)) < 4 | length(unique(y)) < 4)
	error(fill.levels)	
if(method=="spline") # compute a natural cubic spline
	smoothed <- as.data.frame(cbind(x,predict(lm(y~ns(x,df=sp)))))
else if(method=="lowess")
	smoothed <- as.data.frame(cbind(x,predict(loess(y~x,span=sp))))
else if(method=="moving_averages")    # compute moving averages of variable y corresponding to X-values
	smoothed <- moving.averages(y=y,x=x,k=sp)	
else if(method=="polynomial")   # compute a polynomial smoothing of variable y 
	smoothed <- as.data.frame(cbind(x,predict(lm(y~poly(x,degree=sp,raw=T)))))   
else
	smoothed <- NULL
colnames(smoothed) <- c(xname,yname)
if(!is.null(object))
    assign(object,smoothed,envir=.GlobalEnv)
names <- list(xname=xname,yname=yname,title=title,subtitle=subtitle,xlab=xlab,ylab=ylab)
options <- list(col=col,linecol=linecol,lty=lty,lwd=lwd)
retval <- list(plot=plot,x=x,y=y,names=names,smoothed=smoothed,options=options)
class(retval) <- c("curve.smoothing",class(smoothed))
plot(retval)
}	
		
plot.curve.smoothing <- function(z,...)	
{
if(z$plot)
	{
	if(EPIR_GUI)
		setPlotWindow() # start plot window if using the gui
	if(z$names$xlab == "") 	# to put an optional name
		xlab <- z$names$xname
	else
		xlab <- z$names$xlab
	if(z$names$ylab == "")
		ylab <- z$names$yname
	else
		ylab <- z$names$ylab
plot(z$x,z$y,col=z$options$col,sub="",xlab=xlab,ylab=ylab,main ="")
lines(z$smoothed[[1]],z$smoothed[[2]],col=z$options$linecol,lwd=z$options$lwd,lty=z$options$lty)
par(oma=c(5,2,2,2))
title(main=z$names$title,sub=z$names$subtitle,xlab="",ylab="",outer=TRUE)
    }
}
linear.regression <- function(dataset,formula,offset=NULL,subset=NULL,object=NULL,weights=NULL,summary=TRUE,anova=FALSE,durbin.watson=FALSE,goldfeld.quandt=FALSE,plot=TRUE,intercept=TRUE)
# perform a linear regression model
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
wname <- weights
attach(dataset, warn.conflicts = FALSE)
if(!intercept)
    formula <- paste(formula, "- 1", sep = "")
if(!is.null(offset))
	formula <- paste(formula, " + offset(", offset,")", sep = "")
formula <- as.formula(formula)
if(!is.null(weights))
	{
	weights <- eval(parse(text=paste("dataset$",weights,sep="")))
	weights <- abs(weights) #the weights can not to be less than zero	
	}
if(!is.null(subset))
	{
	if(is.character(subset))
		{
		subsetname <- subset
		subset <- eval(parse(text=subset),envir=dataset)
		}
	else
		subsetname <- deparse(substitute(subset))
	model <- lm(formula=formula,data=dataset,subset=subset,weights=weights)  
	}
	else
	   model <- lm(formula=formula,data=dataset,weights=weights)
if(!is.null(object))
	{
	data <- cbind.data.frame(model$residuals,model$fitted.values)
	names(data) <- c("Residuos", "Valores_Ajustados")
	assign(object,data,envir=.GlobalEnv)
	 }
class(model) <- c("linear.regression",class(model)) # to keep class in R and new class in EpiR
summary.rstudent <- NULL
r.squared <- NULL
adj.r.squared <- NULL
aic <- NULL
bic <- NULL
if(summary)
	{
	summary <- data.frame(summary.lm(model)[4]) #calculte summary
	IC.coef <- confint(model)
	rownames(IC.coef) = NULL
	summary[,5] <- as.data.frame(IC.coef[,1])
	summary[,6] <- as.data.frame(IC.coef[,2])
	r.squared <- summary.lm(model)$r.squared
	adj.r.squared <- summary.lm(model)$adj.r.squared
	aic <- AIC(model)
	n <- length(eval(formula[[2]]))
	bic <- AIC(model,k=log(n))
	summary.rstudent <- summary(rstandard(model))
	}
if(anova)
	{
	anova <- anova(model)
	attr(anova, "heading") <- NULL
	anova <- signif(anova,4)
}
if(durbin.watson==TRUE)
	durbin.watson <- dwtest(model)
if(goldfeld.quandt==TRUE)
	goldfeld.quandt <- as.list(gqtest(model))
retval <- list(model=model,formula=formula,wname=wname,
						  summary=summary,summary.rstudent=summary.rstudent,plot=plot,
							 r.squared=r.squared,adj.r.squared=adj.r.squared,aic=aic,bic=bic,
								anova=anova,durbin.watson=durbin.watson,goldfeld.quandt=goldfeld.quandt)
class(retval) <- "linear.regression"
plot(retval)
return(retval)
}

print.linear.regression <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
options(scipen=1,envir=.GlobalEnv) # to print numeric values in fixed and not in exponential notation
cat("","\n")   
if(is.null(x$wname))
{
cat("\t","\t","Modelo de Regressão Linear","\n")
cat("","\n")
cat("Fórmula:","\n")
print(x$formula)
cat("","\n")
}
else
{
cat("\t","\t","Modelo de Regressão Linear Ponderada","\n")
cat("","\n")
cat("Fórmula:","\n")
print(x$formula)
cat("Pesos:","\n")
cat(x$wname[[1]],"\n")
cat("","\n")
}
if(!class(x$summary[1])=="logical") # to check if summary of model is an output    
	{	
	if(rownames(x$summary)[[1]]=="(Intercept)")
		rownames(x$summary)[[1]] <- "Constante"
	colnames(x$summary) = c("coef", "ep", "t", "p","  [IC","95%]")
	cat("Sumário do Modelo:","\n") 
	cat("\n")
	summary <- round(x$summary[2:nrow(x$summary),2:ncol(x$summary)],digits)
	print(round(x$summary,digits=digits))
	cat("\n")
	names(x$summary.rstudent) = c("Min", "Q1", "Mediana", "Média", "Q3", "Max")
	cat("Resíduos Padronizados:","\n")
	cat("\n")
	print(round(x$summary.rstudent,digits=digits))
	cat("\n")
	names(x$r.squared) <- "R2"
	names(x$adj.r.squared) <- "R2-Aj"
	names(x$aic) <- "AIC" 
	names(x$bic) <- "BIC" 
	measures <- c(x$r.squared,x$adj.r.squared,x$aic,x$bic)
	cat("Medidas de Ajuste:","\n")
	cat("\n")
	print(round(measures,digits=digits))
	cat("\n","\n")
	}
else
	{
	cat("Coeficientes:","\n")
	if(names(x$model$coefficients)[1]=="(Intercept)")
	names(x$model$coefficients)[1] <- "Intercepto"
	print(x$model[[1]])
	cat("\n")
	}	
if(!class(x$anova[[1]])=="logical") # to check if anova of model is an output 
	{    
	colnames(x$anova) <- c("gl.","SQ","QM","F","p")
	rownames(x$anova)[nrow(x$anova)] <- "Resíduos"    
	cat("\n")
	cat("\t","\t","Tabela ANOVA","\n")
	cat("\n")
	print(x$anova)
	cat("\n")
    } 
if(!class(x$durbin.watson[1])=="logical") # to check if Durbin-Watson test is an output 
    {    
	names(x$durbin.watson[[1]]) <- "t"
	names(x$durbin.watson[[4]]) <- "p"
	cat("\n")
	cat("Teste para autocorrelação: Durbin-Watson","\n")
	cat("","\n")
	print(as.table(c(x$durbin.watson[[1]],x$durbin.watson[[4]])),digits=digits) 
	cat("\n")
    }
if(!class(x$goldfeld.quandt[1])=="logical") # to check if Durbin-Watson test is an output 
    {
	names(x$goldfeld.quandt[[1]]) <- "t"
	names(x$goldfeld.quandt[[2]]) <- c("gl(1)","gl(2)") 
	names(x$goldfeld.quandt[[4]]) <- "p"
    cat("\n")
	cat("Teste para heterocedasticidade: Goldfeld-Quandt","\n")
	cat("\n")
	print.factor(c(round(x$goldfeld.quandt[[1]],digits),x$goldfeld.quandt[[2]],round(x$goldfeld.quandt[[4]],digits=digits)),max.levels=0)
    }
cat("\n")
}

plot.linear.regression <- function(x,...)
{
if(x$plot==TRUE)
	{
	if(EPIR_GUI)
		setPlotWindow() # start plot windows if using the gui
	
	layout(matrix(1:4,nrow=2,byrow=TRUE))
	fit.values <- fitted.values(x$model)
	if(!is.null(x$weights))
		fit.values <- fit.values[x$weights!=0]
	
	plot(fit.values,rstandard(x$model),xlab ="Valores Ajustados",ylab ="Resíduos Padronizados", main ="Resíduos vs Valores Ajustados",cex.main=0.75, xlim = c(),ylim = c(-3,3),cex=0.43, pch=16, cex.axis=0.9, family="") 
        abline(-2,0, lty = 3, col = "red")
        abline(2,0, lty = 3, col = "red") 
	abline(0,0, lty = 1, col = "black")
	
        plot(rstandard(x$model), xlab = "Índice das Observações", ylab = "Resíduos Padronizado",main="Resíduos vs Valores Observados",cex.main=0.75, xlim = c(),ylim = c(-3,3),cex=0.43, pch=16, cex.axis=0.9, family="")        
        abline(-2,0, lty = 3, col = "red")
        abline(2,0, lty = 3, col = "red")
        abline(0,0, lty = 1, col = "black") 

	plot(cooks.distance(x$model), xlab = "Índice das Observações", ylab = "Distância", main ="Distância de Cook",cex.main=0.75, xlim = c(),ylim = c(0,max(0.20,cooks.distance(x$model))),cex=0.43, pch=16, cex.axis=0.9, family="") 
        abline(0.10,0, lty = 3, col = "red")
    
	qqnorm(rstandard(x$model), xlab="Percentis da Distribuições Normal", ylab="Resíduos Padronizados", main ="Gráfico de Normalidade",cex.main=0.75,cex=0.45, pch=16, cex.axis=0.9, family="",ylim=c(-3,3),xlim=c(-3,3))
    qqline(rstandard(x$model),col="red")
	}
}


moving.averages <- function(y,x,k) 
# compute moving averages of variable y corresponding to X-values
{
yhat <- double(length(y))
for(i in 1:length(x))
    # This function does not check for or accept NA
	yhat[i] <- mean(y[max(i-k,1):min(i+k,length(x))])
smoothed <- as.data.frame(cbind(x,yhat))
return(smoothed)
}

logistic.regression <- function(dataset,formula,offset=NULL,subset=NULL,object=NULL,weights=NULL,odds.ratio=FALSE,roc.curve=FALSE,HL.test=FALSE,basic.plot=TRUE,X.yhat=FALSE,D.yhat=FALSE,beta.yhat=FALSE,X.h=FALSE,D.h=FALSE,beta.h=FALSE)
# perform a logistic regression model
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
wname <- weights
attach(dataset, warn.conflicts = FALSE)
if(!is.null(offset))
	formula <- paste(formula, " + offset(", offset,")", sep = "")
formula <- as.formula(formula)
if(!is.null(weights))
	{
	weights <- eval(parse(text=paste("dataset$",weights,sep="")))
	weights <- abs(weights) #the weights can not to be less than zero	
	}
if(!is.null(subset))
	{
	if(is.character(subset))
		{
		subsetname <- subset
		subset <- eval(parse(text=subset),envir=dataset)
		}
	else
		subsetname <- deparse(substitute(subset))
	fit.model <- glm(formula=formula,data=dataset,subset=subset,weights=weights,family=binomial("logit"))
	}
	else
		fit.model <- glm(formula=formula,data=dataset,weights=weights,family=binomial("logit"))
stand.deviance.resid <- rstandard(fit.model,type=deviance)
retval <- list(fit.model=fit.model,formula=formula,object=object,stand.deviance.resid=stand.deviance.resid,odds.ratio=odds.ratio,roc.curve=roc.curve,HL.test=HL.test)
class(retval) <- c("logistic.regression",class(fit.model))
if(basic.plot && !any(X.yhat=FALSE,D.yhat=FALSE,beta.yhat=FALSE,X.h=FALSE,D.h=FALSE,beta.h=FALSE))
    plot(retval)
else
    diagnostics.logistic(fit.model=fit.model,X.yhat=X.yhat,D.yhat=D.yhat,beta.yhat=beta.yhat,X.h=X.h,D.h=D.h,beta.h=beta.h,salve=TRUE,object=object)
return(retval)
}


print.logistic.regression <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
options(scipen=1,envir=.GlobalEnv) # to print numeric values in fixed and not in exponential notation
if(is.null(digits))
	digits <- 4
model <- x$fit.model
cat("\n")
cat("\t","\t","Regressão Logística","\n","\n")	
cat("Família = Binomial, ","Função de ligação = logito","\n")
cat("Fórmula:","\n")
print(x$formula)
cat("\n")
summary.model <- summary.glm(model)
resid <- x$stand.deviance.resid
summary.Dresiduals <- summary(resid)
names(summary.Dresiduals) = c("Min", "Q1", "Mediana", "Média", "Q3", "Max")	
cat("Resíduos de Desvio Padronizado","\n")
print(round(summary.Dresiduals,digits))
cat("\n")
summary.coef <- summary.model$coefficients 
if(rownames(summary.coef)[[1]]=="(Intercept)")
	rownames(summary.coef)[[1]] <- "Constante"
cat("Sumário do Modelo:","\n") 
IC.coef <- confint.default(model)
rownames(IC.coef) = NULL
summary.coef <- as.data.frame(summary.coef)
summary.coef[,5] <- as.data.frame(IC.coef[,1])
summary.coef[,6] <- as.data.frame(IC.coef[,2])
colnames(summary.coef) <- c("coef", "ep", "t", "p","  [IC","95%]")
#print(summary.coef,digits=digits)
print(round(summary.coef,digits))
cat("\n")
df.null <- length(summary.model$deviance.resid) - 1
df.residual <- summary.model$df[2]
cat("Parâmetro de dispersão para a família Binomial:",round(summary.model[[4]]/df.residual,digits),"\n")
cat("\n    Desvio Nulo: ",round(summary.model[[8]],digits)," com ",df.null," graus de liberdade\n",sep="")
cat("Desvio Residual: ",round(summary.model[[4]],digits)," com ",df.residual," graus de liberdade\n",sep="")
cat("\n")
cat("Número de iterações: ",summary.model[[10]],"\n") 
AIC <- summary.model[[9]]
names(AIC) = "AIC"
R2 <- r2.glm(model)
names(R2) <- "R2"
measures <- c(r2.glm(model),AIC)
cat("\n")
cat("Medidas de Ajuste:","\n")
cat("\n")
print(round(measures,digits=digits))
cat("\n")
if(x$HL.test)
    {
cat("Estatística Hosmer-Lemeshow:",round(HL(model)[[1]],digits),"\n")
cat("valor-p = ",round(HL(model)[[2]],digits),"\n")
    }
cat("\n")
if(x$odds.ratio)
    {
    cat("Estimativa para a Razão de Chances","\n")
    print(odds.ratio.glm(model, conf.level=0.95,plot=F),digits)
    }
cat("\n")
}


plot.logistic.regression <- function(x,...)
{
model <- x$fit.model
if(!is.null(x$object))
    {
    output <- data.frame(model$fitted.values)
    names(output) <- "Valores_Ajustados"
    assign(x$object,output,envir=.GlobalEnv)
    }
if(EPIR_GUI)
    setPlotWindow() # start plot windows if using the gui
if(x$odds.ratio)
    layout(matrix(1:4,nrow=2,byrow=TRUE))
else
    layout(matrix(1:3,nrow=1,byrow=TRUE))
resid <- x$stand.deviance.resid
ymin <- min(min(resid,na.rm=TRUE),-4)*1.2
ymax <- max(max(resid,na.rm=TRUE),4)*1.2

plot(fitted.values(model),resid,xlab = "Valores Ajustados", ylab = "Resíduo de Desvio Padronizado",main="Resíduos vs Valores Ajustados",cex.main=0.75,ylim = c(ymin,ymax),cex=0.43, pch=16, cex.axis=0.9, family="") 

cook <- cooks.distance(model)
plot(cook, xlab = "Índice das Observações", ylab = "Distância", main ="Distância de Cook",cex.main=0.75, xlim = c(),ylim = c(),cex=0.43, pch=16, cex.axis=0.9, family="")
abline(0.10,0, lty = 3, col = "red")
if(x$odds.ratio)
    odds.ratio.glm(fit.model=model,conf.level=0.95,plot=T)
ROC.curve(fit.model=model,m=100,listdata=F,plot=T)
}




r2.glm <- function(fit.model)
{
n <- length(fit.model$residuals) # number of observations 
R2 <- (1 - exp((fit.model$deviance - fit.model$null.deviance)/n ))/(1 - exp(-fit.model$null.deviance/n))
names(R2) <- "Pseudo-R2"
return(R2)
} 


odds.ratio.glm <- function(fit.model,conf.level=0.95,plot=F,...)
{
options(scipen=1)
x<-summary.glm(fit.model)
output <- or(model=fit.model,conf.level=conf.level)
p <- 100*conf.level
if(dimnames(output)[[1]][1]=="(Intercept)")
    dimnames(output)[[1]][1] <- "Intercepto"
dimnames(output)[[2]] <- c("RC","[IC",paste(p,"%]"))
ylim <- range(output[,2],output[,3])
if(plot)
    {
    plot(output[,1],ylim=ylim,cex.main=0.75,cex=0.43, pch=16, cex.axis=0.9,xlab ="",ylab="Estimativa/ IC 95%",main ="Razão de Chances",axes=F)
    box()
    axis(2)
    n <- length(output[,1])
    axis(1, at=1:n, labels =  dimnames(output)[[1]], las = 3, cex.axis = .8)
    points(output[,2], pch = 24)
    points(output[,3], pch = 25)
    segments(1:n,output[,2],1:n,output[,3])
    abline(h=1)
    }
else
    return(output)
}

or <- function(model,conf.level)
{
lreg.coeffs <- coef(summary(model))
lci <- exp(lreg.coeffs[ ,1] - conf.level * lreg.coeffs[ ,2])
or <- exp(lreg.coeffs[ ,1])
uci <- exp(lreg.coeffs[ ,1] + conf.level* lreg.coeffs[ ,2])
lreg.or <- cbind(or,lci, uci)        
lreg.or
}


# Hosmer & Lemeshow,1985

HL<- function(fit.model,...)
{
p <- 1/(1+exp(-predict(fit.model)))
ord <- order(p)
resp <-fit.model$y[ord]
#po <- sort(p)
dec <- quantile(sort(p),seq(0,1,0.1))
o <-NULL
e <-NULL
n <- NULL
dec[1]<-0
for (i in 1:10)
    {
    indice<-(dec[i]<sort(p) & sort(p)<=dec[i+1])
    o[i] <- sum(resp[indice])
    e[i] <- sum(sort(p)[indice])
    n[i]<-sum(indice)
    }
hat.p <- e/n
x <- sum((o-e)^2/(n*hat.p*(1-hat.p)))
p.value <- 1-pchisq(x,8)
retval <- list(x,p.value)
names(retval) <- c("Hosmer-Lemeshow","p-value")
retval
}


              

ROC.curve<-function(fit.model,m=100,listdata=F,plot=T,...)
{
y.hat <- fitted.values(fit.model)
event <- fit.model$y
start <- min(y.hat)
end <- max(y.hat)
sens <- onemspec<-1
for (i in seq(start+0.000001,end-0.000001,by=(end-start)/m))
    {
    tab <- table(event,y.hat>i)
    if( dim(tab)[2]==1) 
        tab <- cbind(c(0,0),tab)
    sens <- c(sens,tab[2,2]/(tab[2,2]+tab[2,1]))
    onemspec <- c(onemspec,tab[1,2]/(tab[1,2]+tab[1,1]))
     }
sens <- c(sens,0)
onemspec <- c(onemspec,0)
if(plot)
    {
    plot(onemspec,sens,type="n",xlab="1-Especificidade",ylab="Sensitividade",main="Curva ROC",cex.main=0.75,cex=0.43,cex.axis=0.9,ylim=c(0,1),xlim=c(0,1))
    lines(lowess(onemspec,sens,f=0.2))
    }
if(listdata)
    list(sens=sens,onemspec=onemspec)
}

# Logistic diagnostics by Hommer and Lemeshow (chapter 5)

diagnostics.logistic <- function(fit.model,X.yhat=F,D.yhat=F,beta.yhat=F,X.h=F,D.h=F,beta.h=F,salve=TRUE,object=NULL,...)
{
#r.pearson  <- rstandard(fit.model,type="peasorn")
rp <- residuals(fit.model,type="pearson")
d <- residuals(fit.model,type="deviance")
stand.rp <- glm.diag(fit.model)$rp
h <- glm.diag(fit.model)$h
delta.beta <- ((stand.rp^2)*h)/(1-h)
delta.X <- stand.rp^2
delta.D <- d^2 + ((rp^2)*h)/(1-h) 
yhat <- fitted.values(fit.model)
ngraphs <-sum(c(X.yhat,D.yhat,beta.yhat,X.h,D.h,beta.h))
nscreen <- 1
if(EPIR_GUI)
    setPlotWindow() # start plot windows if using the gui 55
if(ngraphs==6)
    layout(matrix(1:6,nrow=2,byrow=TRUE))
else
    {
    if(ngraphs==5)
        {
        nscreen <- 3
        split.screen(c(2,1))
        screen(1)
        split.screen(c(1,3))
        screen(2)
        split.screen(c(1,2))
        }
    else if(ngraphs==4)
        split.screen(c(2,2))
    else if(ngraphs==3)
        split.screen(c(1,3))
    else if(ngraphs==2)
        split.screen(c(1,2))
    else
        split.screen(c(1,1))
    }
if(X.yhat)
    {
    screen(nscreen)
    nscreen <- nscreen + 1 # Hosmer and Lemeshow Delta chi-squared influence statistic
    plot(yhat,delta.X,cex.main=0.75,xlab="Valores Ajustados",ylab="Delta Qui-Quadrada de Hosmer-Lemeshow", xlim = c(),ylim = c(),cex=0.43, pch=16, cex.axis=0.9,family="",cex.main=1.10)
    }
if(D.yhat)
    {
    screen(nscreen)
    nscreen <- nscreen + 1 # Hosmer and Lemeshow Delta-D influence statistic
    plot(yhat,delta.D,cex.main=0.75,xlab="Valores Ajustados",ylab="Delta-D de Hosmer-Lemeshow",xlim = c(),ylim = c(),cex=0.43, pch=16, cex.axis=0.9, family="",cex.main=1.10)
    }
if(beta.yhat)
    {
    screen(nscreen)
    nscreen <- nscreen + 1 #Pregibon Delta-Beta influence statistic
    plot(yhat,delta.beta,cex.main=0.75,xlab="Valores Ajustados",ylab="Delta-Beta de Pregibon",xlim = c(),ylim = c(),cex=0.43, pch=16, cex.axis=0.9,family="",cex.main=1.10)
    }
if(X.h)
    {
    screen(nscreen)
    nscreen <- nscreen + 1
    plot(h,delta.X,cex.main=0.75,xlab="Pontos de Alavanca",ylab="Delta Qui-Quadrada de Hosmer-Lemeshow",xlim = c(),ylim = c(),cex=0.43, pch=16, cex.axis=0.9,family="",cex.main=1.10)
    }
if(D.h)
    {
    screen(nscreen)
    nscreen <- nscreen + 1
    plot(h,delta.D,cex.main=0.75,xlab="Pontos de Alavanca",ylab="Delta-D de Hosmer-Lemeshow",xlim = c(),ylim = c(),cex=0.43, pch=16, cex.axis=0.9,family="",cex.main=1.10)
    }
if(beta.h)
    {
    screen(nscreen)
    nscreen <- nscreen + 1
    plot(h,delta.beta,cex.main=0.75,xlab="Pontos de Alavanca",ylab="Delta-Beta de Pregibon",xlim = c(),ylim = c(),cex=0.43, pch=16,cex.axis=0.9,family="",cex.main=1.10)
    }
par(oma=c(5,2,2,2))
title(main="Diagnósticos Avançados (Hosmer-Lemeshow)",sub="",xlab="",ylab="",outer=TRUE) 
close.screen(all=TRUE)
output <- cbind.data.frame(fit.model$fitted.values,delta.X,delta.D,delta.beta)
names(output) <- c("Valores_Ajustados","Delta.X","Delta.D","Delta.beta")
if(salve)
    if(is.null(object))
        assign("Diag_Logistica",output,env=.GlobalEnv)
    else
        assign(object,output,env=.GlobalEnv)
}