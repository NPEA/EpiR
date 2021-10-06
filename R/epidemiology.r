# epidemiology functions


case.control <- function(dataset=NULL,x=NULL,x.non_dis=NULL,y=NULL,y.non_exp=NULL,cross.table=NULL,calc=FALSE,fisher.test=FALSE,chisq.test=FALSE,decimal=2,conf.level=0.95)
{               #cross.table should be outcome (desfecho=x) vs exposure (exposição =y) 
xname <- NULL
yname <- NULL
if(!is.null(cross.table))
	{
        ct.print <- cross.table
	dim(ct.print) <- c(2, 2)
	ct.print <- as.table(ct.print)
	rownames(ct.print) <- c("Non-diseased", "Diseased")
	colnames(ct.print) <- c("Non-exposed", "Exposed")
        cross.table <- matrix(data=NA,2,2)
        cross.table[1,1] <- ct.print[2,2]; cross.table[1,2] <- ct.print[2,1] # to keep the printing in traditional table in Brazil
        cross.table[2,1] <- ct.print[1,2]; cross.table[2,2] <- ct.print[1,1]
        cross.table <- as.table(cross.table)
	}
if(!calc)
	{
	if(is.character(dataset))
		dataset <- eval(parse(text=dataset),envir=.GlobalEnv)		
	if(!is.null(x))
		{
		if(is.character(x))
			{
			xname <- x
			x <- as.factor(eval(parse(text=x),envir=dataset))
			}
	else
			xname <- deparse(substitute(x))
		}
	if(!is.null(y))
		{
		if(is.character(y))
			{
			yname <- y
			y <- as.factor(eval(parse(text=y),envir=dataset))
			}
		else
			yname <- deparse(substitute(y))
		}
	x <- relevel(x,levels(x)[which(levels(x)==x.non_dis)])
	y <- relevel(y,levels(y)[which(levels(y)==y.non_exp)])
	ct.print <- table(x,y,dnn=list("",""))
        a <- relevel(x,ref=x.non_dis)
	b <- relevel(y,ref=y.non_exp)
        cross.table <- table(a,b,dnn=list("",""))
	}
if(class(conf.level)!="numeric")
	conf.level <- 0.95	
else
	if(conf.level>1)
		conf.level <- conf.level/100
od <- round(fisher.test(cross.table)$estimate,digits=decimal)
lower <- round(fisher.test(cross.table)$conf.int[1],decimal)
upper <- round(fisher.test(cross.table)$conf.int[2],decimal)	
if(fisher.test)
	fisher.test <- fisher.test(cross.table,conf.level=conf.level)
if(chisq.test)
	chisq.test <- suppressWarnings(chisq.test(cross.table))
else
	chisq.test <- NULL
retval <- list(ct.print=ct.print,calc=calc,decimal=decimal,od=od,lower=lower,upper=upper,
			xname=xname,yname=yname,fisher.test=fisher.test,chisq.test=chisq.test,conf.level=conf.level)
class(retval) <- "case.control"
return(retval)
}

print.case.control <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
ct.print <- x$ct.print # this will be to print of usual form in Brasil
if(!is.null(digits))
	decimal <- digits
else 
	decimal <- x$decimal
col.names <- colnames(ct.print)
row.names <- rownames(ct.print)

ct.print <- addmargins(ct.print)
if(x$calc)
    {
    colnames(ct.print) <- c("Expostos", "Não Expostos","Total")
    rownames(ct.print) <- c("Casos", "Controles","Total")
    }
else
    {
    rownames(ct.print) <- c(row.names[1:2],"Total")
    colnames(ct.print) <- c(col.names[1:2],"Total")
    }
cat("\t","Medidas Epidemiológicas","\n")
cat("\n")
if(!is.null(x$xname) || !is.null(x$yname))
	cat(paste("Dados: " ,x$xname," e ",x$yname, sep = ""),"\n","\n") 
print.noquote(ct.print)
cat("\n")
cat(paste("Razão de Chance = ",x$od,", IC ",(x$conf.level)*100,"% = [",x$lower,", ",x$upper,"]",sep = ""),"\n")
cat("\n")
if(!is.null(x$chisq.test))
	cat(paste("Teste Qui-Quidrado: Est. = ",round(x$chisq.test$statistic,3),
		", gl = ",x$chisq.test$parameter,", valor-p = ",round(x$chisq.test$p.value,EPIR_OPTION_DIGITS_PVALUE),sep = ""),"\n","\n")
	if(class(x$fisher.test)!="logical")        
	{
	cat(paste("Teste Exato de Fisher:", ", valor-p = ", round(x$fisher.test$p.value,EPIR_OPTION_DIGITS_PVALUE), sep = ""),"\n")
	cat("Hipótese alternativa: a razão de chances não é igual a 1","\n")
	}
cat("\n")
}

cohort <- function(dataset=NULL,x=NULL,x.non_dis=NULL,y=NULL,y.non_exp=NULL,cross.table=NULL,calc=FALSE,risk.print=TRUE,fisher.test=FALSE,chisq.test=FALSE,decimal=EPIR_OPTION_DIGITS,conf.level=0.95)
{               #cross.table should be outcome (desfecho=x) vs exposure (exposição =y) 
xname <- NULL
yname <- NULL
if(!is.null(cross.table))
	{
        ct.print <- cross.table
	dim(ct.print) <- c(2, 2)
	ct.print <- as.table(ct.print)
	rownames(ct.print) <- c("Non-diseased", "Diseased")
	colnames(ct.print) <- c("Non-exposed", "Exposed")
        cross.table <- matrix(data=NA,2,2)
        cross.table[1,1] <- ct.print[2,2]; cross.table[1,2] <- ct.print[2,1] # to keep the printing in traditional table in Brazil
        cross.table[2,1] <- ct.print[1,2]; cross.table[2,2] <- ct.print[1,1]
        cross.table <- as.table(cross.table)
	}
if(!calc)
	{
	if(is.character(dataset))
		dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
	if(!is.null(x))
		{
		if(is.character(x))
			{
			xname <- x
			x <- as.factor(eval(parse(text=x),envir=dataset))
			}
		else
			xname <- deparse(substitute(x))
		}
	if(!is.null(y))
		{
		if(is.character(y))
			{
			yname <- y
			y <- as.factor(eval(parse(text=y),envir=dataset))
			}
		else
			yname <- deparse(substitute(y))
		}
	x <- relevel(x,levels(x)[which(levels(x)==x.non_dis)])
	y <- relevel(y,levels(y)[which(levels(y)==y.non_exp)])
	ct.print <- table(x,y,dnn=list("",""))
        a <- relevel(x,ref=x.non_dis)
	b <- relevel(y,ref=y.non_exp)
        cross.table <- table(a,b,dnn=list("",""))
	}
if(class(conf.level)!="numeric")
	conf.level <- 0.95	
else
	if(conf.level>1)
		conf.level <- conf.level/100
r <- NULL
rr <- NULL
lowci <- NULL
hici <- NULL
a <- cross.table[1,1]
A <- sum(cross.table[,1])*sum(cross.table[1,])/sum(cross.table[,])
d <- sum(cross.table[, 1]) / (sum(cross.table[,]) - 1)*sum(cross.table[1, ])*sum(cross.table[, 2])*sum(cross.table[2, ])/sum(cross.table[, ])^2
chi2 <-abs(a-A)^2/d
for (i in 1:ncol(cross.table)) 
	r[i] <- cross.table[2, i]/colSums(cross.table)[i]
rr[1] <- 1
for (i in 2:ncol(cross.table)) 
	rr[i] <- (cross.table[2, i]/colSums(cross.table)[i])/(cross.table[2,1]/colSums(cross.table)[1])
for(i in 2:ncol(cross.table))
	lowci[i] <- rr[i]^(1-qnorm(1-.05)/sqrt(suppressWarnings(chisq.test(cbind(cross.table[,1],cross.table[,i])))$statistic))
for(i in 2:ncol(cross.table))
	hici[i] <- rr[i]^(1+qnorm(1-.05)/sqrt(suppressWarnings(chisq.test(cbind(cross.table[,1],cross.table[,i])))$statistic))

if(fisher.test)
	fisher.test <- fisher.test(cross.table,conf.level=conf.level)
else
	fisher.test <- NULL
if(chisq.test)
	chisq.test <- suppressWarnings(chisq.test(cross.table))
else
	chisq.test <- NULL
cross.table <- addmargins(cross.table)
risk <- cross.table[2,]/cross.table[3,]
risk.diff  <- round(risk[2]-risk[1],decimal)
risk.diff.lower <- round(risk.diff*(1-sign(risk.diff)*(qnorm(1-.05/2)/sqrt(chi2))),decimal)
risk.diff.upper <- round(risk.diff*(1+sign(risk.diff)*(qnorm(1-.05/2)/sqrt(chi2))),decimal)
risk.ratio <- round(risk[2]/risk[1], decimal)
risk.ratio.lower <- round(risk.ratio^(1-sign(risk.diff)*(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(cross.table)$statistic)))),decimal)
risk.ratio.upper <- round(risk.ratio^(1+sign(risk.diff)*(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(cross.table)$statistic)))),decimal)

retval <- list(ct.print=ct.print,calc=calc,decimal=decimal,risk=risk,xname=xname,yname=yname,risk.print=risk.print,
				risk.diff=risk.diff,risk.diff.lower=risk.diff.lower,risk.diff.upper=risk.diff.upper,risk.ratio=risk.ratio,risk.ratio.lower=risk.ratio.lower,risk.ratio.upper=risk.ratio.upper, 
					fisher.test=fisher.test,chisq.test=chisq.test,conf.level=conf.level)
class(retval) <- "cohort"
return(retval)
}

print.cohort <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
ct.print <- x$ct.print # this will be to print of usual form in Brasil
if(!is.null(digits))
	decimal <- digits
else 
	decimal <- x$decimal
col.names <- colnames(ct.print)
row.names <- rownames(ct.print)

ct.print <- addmargins(ct.print)
if(x$calc)
    {
    colnames(ct.print) <- c("Expostos", "Não Expostos","Total")
    rownames(ct.print) <- c("Casos", "Controles","Total")
    }
else
    {
    rownames(ct.print) <- c(row.names[1:2],"Total")
    colnames(ct.print) <- c(col.names[1:2],"Total")
    }
if(x$risk.print)
	{
	ct.print <- rbind(ct.print,c("","",""), c("RNE","RE","RT"), round(x$risk,decimal), deparse.level=1)
	rownames(ct.print)[c(4:6)] <- c("","","Risco")
	}
ct.print <- rbind(ct.print,c("","",""), c("Estimativa","[IC "," 95%]"),
	round(c(x$risk.diff,x$risk.diff.lower,x$risk.diff.upper),decimal),
		round(c(x$risk.ratio,x$risk.ratio.lower,x$risk.ratio.upper),decimal), deparse.level=1)
if(x$risk.print)
	rownames(ct.print)[c(9:10)] <- c("Dif. (RE - RNE)","Risco Relativo")
else
	rownames(ct.print)[c(6:7)] <- c("Dif. (RE - RNE)","Risco Relativo")
cat("\t","Medidas Epidemiológicas","\n")
cat("\n")
if(!is.null(x$xname) || !is.null(x$yname))
	cat(paste("Dados: " ,x$xname," e ",x$yname, sep = ""),"\n","\n") 
print.noquote(ct.print)		
cat("\n")
cat("RNE = Risco Não Expostos, RE = Risco Expostos, RT = Risco Total","\n")
cat("\n")
if(!is.null(x$chisq.test))
	cat(paste("Teste Qui-Quidrado: Est. = ",round(x$chisq.test$statistic,decimal),", gl = ",x$chisq.test$parameter,", valor-p = ",round(x$chisq.test$p.value,EPIR_OPTION_DIGITS_PVALUE),sep = ""),"\n","\n")
if(!is.null(x$fisher.test))        
	{
	cat(paste("Teste Exato de Fisher:", ", valor-p = ", round(x$fisher.test$p.value,EPIR_OPTION_DIGITS_PVALUE), sep = ""),"\n")
	cat("Hipótese alternativa: a razão de chances não é igual a 1","\n")
	
	lower <- round(x$fisher.test$conf.int[1],decimal)
	upper <- round(x$fisher.test$conf.int[2],decimal)
	cat(paste("Intervalo de ",(x$conf.level)*100,"% de  Confiança:"," [",lower,", ",upper,"]",sep = ""),"\n")
	cat("Estimativa Pontual:","\n")
	cat(paste("Razão de Chances = ",round(x$fisher.test$estimate,digits=decimal),sep = ""),"\n")
	}
cat("\n")
}
