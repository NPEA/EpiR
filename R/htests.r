# parametric tests functions 
association.test <-function(dataset,x,y,by=NULL,test="chisq.test",alternative="two.sided",conf.level=0.95,simulate.p.value = FALSE,na.rm=FALSE,show.table=TRUE,filter=NULL) {
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(filter))
	dataset <- subset(dataset,subset=eval(parse(text=filter)))
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
		xname[2] <- y
		y <- eval(parse(text=y),envir=dataset)
		}
	else
		xname[2] <- deparse(substitute(y))
	}
if(!is.null(by))
	{
	if(is.character(by))
		{
		byname <- by
		by <- as.factor(eval(parse(text=by),envir=dataset))
		}
	else
		byname <- deparse(substitute(by))
	}
else
	{
	byname <- by
	by <- as.factor(rep("",dim(dataset)[1]))
	}
if(!alternative %in% c("two.sided","less","greater") || is.null(alternative))
alternative <- "two.side"
if(class(par)!="numeric")
	if(test=="var.test")
		par <- 1
if(class(conf.level)!="numeric")
	conf.level <- 0.95
else
	if(conf.level>1)
		conf.level <- conf.level/100
if(na.rm)
	{
	data <- na.omit(data.frame(x,y,by))
	x <- data[,1]
	y <- data[,2]
	by <- data[,3]
	}
x <- as.factor(x)
y <- as.factor(y)
if(test=="chisq.test")
	chisq.htest(x=x,y=y,xname=xname,simulate.p.value=simulate.p.value,show.table=show.table)
else
	mantelhaen.htest(x=x,y=y,by=by,xname=xname,byname=byname,conf.level=conf.level,alternative=alternative,show.table=show.table)
}

chisq.htest <- function(x=x,y=y,xname=xname,show.table=show.table,simulate.p.value=simulate.p.value)
#used in association.test
{
htest <-NULL
if(is.null(y))
	{
	xname <- xname[1]
	table <- table(x,dnn=xname)
	htest <-  suppressWarnings(chisq.test(table,simulate.p.value=simulate.p.value))
	}
else
	{
	table <- table(x,y,dnn=xname)
	htest<-  suppressWarnings(chisq.test(table,simulate.p.value=simulate.p.value))  #if there are two vectors, par can´t be used. if there are twor vectors, the test will be all proportions are the same against at least one proportion is different
	}
if(!show.table)
	table <- NULL
retval <- list(htest=htest,xname=xname,table=table)
class(retval) <- c("chisq.htest",class(htest))
return(retval)
}

print.chisq.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
	digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste Qui-Quadrado para associação entre grupos","\n","\n")
if(length(x$xname)==1)
	cat("Dados: ",x$xname,"\n")
else
	cat("Dados: ",x$xname[[1]]," e ",x$xname[[2]],"\n")
cat(paste("Est. Qui-Quadrado = ", round(as.list(x$htest$statistic)[[1]],digits), ", gl = ", round(as.list(x$htest$parameter)$df,digits),", valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep = ""),"\n")
cat("\n")
if(!is.null(x$table))
	{
	cat("Tabela de Contingência:","\n")
	cat("\n")
	print(x$table)
	}
cat("\n","\n")
}


mantelhaen.htest <- function(x=x,y=y,by=by,xname=xname,byname=byname,conf.level=conf.level,alternative=alternative,show.table=show.table)
#used in association.test
{
htest <- NULL
table <- table(x,y,by)
htest <- mantelhaen.test(table,conf.level=conf.level,alternative=alternative)
if(!show.table)
	table <-NULL
else
    table <- table(x,y,by,dnn=c(xname,byname))
	#table <- ftable(x,y,by,dnn=c(xname,byname))
retval <- list(htest=htest,xname=xname,byname=byname,table=table,conf.level=conf.level,alternative=alternative)
class(retval) <- c("mantelhaen.htest",class(htest))
return(retval)
}

print.mantelhaen.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
	digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste Qui-Quadrado (Cochran-Mantel-Haenszel) para variáveis","\n")
cat("\t","\t", "condicionalmente independentes entre cada grupo","\n","\n")

cat(paste("Dados: ",x$xname[[1]]," e ",x$xname[[2]]," estratificado por ",x$byname, sep = ""),"\n")
cat(paste("Est. Mantel-Haenszel = ", round(as.list(x$htest$statistic)[[1]],digits), ", gl = ", round(as.list(x$htest$parameter)$df,digits),", valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep = ""),"\n")
if(x$alternative %in% names(x$htest))
	{
	if(x$alternative=="two.sided")
		cat(paste("Hipótese alternativa: a razão de chances comum não é igual a 1", sep = ""),"\n")
	else if(x$alternative=="less")
		cat(paste("Hipótese alternativa: a razão de chances comum é menor que 1", sep = ""),"\n")
	else
		cat(paste("Hipótese alternativa: a razão de chances comum é maior que 1", sep = ""),"\n")
	upper <- round(x$htest$conf.int[2],digits)
    lower <- round(x$htest$conf.int[1],digits)
	cat(paste("Intervalo de ",(x$conf.level)*100,"%"," de confiança", sep = ""),"\n")
	cat(paste("[",lower,", " ,upper,"]",sep = ""),"\n")
	cat("Estimativa Pontual:","\n")
	names(x$htest[[i]]$estimate) <- "Razão de chances comum"
	print(x$htest[[i]]$estimate,digits=digits)
	}
cat("\n")
if(!is.null(x$table))
	{
	cat("Tabela de Contingência:","\n")
	cat("\n")
	print(x$table)
	}
cat("\n","\n")
}

mean.test <- function(dataset,x,y=NULL,by=FALSE,group=NULL,filter=NULL,test="t.test",one.sample=TRUE,alternative="two.sided",par=0,paired=FALSE,var.equal=FALSE,conf.level=0.95,na.rm=FALSE)
# mean test
{
if(is.character(dataset))
    dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(filter))
    dataset <- subset(dataset,subset=eval(parse(text=filter)))
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
        xname[2] <- y
        y <- eval(parse(text=y),envir=dataset)
        }
    else
        xname[2] <- deparse(substitute(y))
    }
if(!alternative %in% c("two.sided","less","greater") || is.null(alternative))
    alternative <- "two.sided"
if(class(par)!="numeric")
    if(test=="t.test")
        par <- 0
    else
        par <- 0.5
if(class(paired)!="logical")
    paired <- FALSE
if(class(conf.level)!="numeric")
    conf.level <- 0.95
else
    if(conf.level>1)
        conf.level <- conf.level/100
if(na.rm)
    {
    if(is.null(y))
        x <- x[!is.na(x)]
    else
        {
        data <- na.omit(data.frame(x,y))
        x <- data[,1]
        y <- data[,2]
        }
    }
if(by)
    y <- as.factor(y)
if(test=="t.test")
    {
    if(one.sample)
        {
        if(is.null(y))
            t.htest(x=x,y=NULL,group=NULL,xname=xname,conf.level=conf.level,paired=paired,var.equal=FALSE,par=par,alternative=alternative)
        else
            t.htest(x=x,y=y,group=group,xname=xname,conf.level=conf.level,paired=paired,var.equal=FALSE,par=par,alternative=alternative)
        }
    else
        {
        if(class(y)=="factor")
            t.htest(x=x,y=y,group=group,xname=xname,conf.level=conf.level,paired=paired,par=par,var.equal=var.equal,alternative=alternative)
        else
           {
            t.htest(x=x,y=y,group=group,xname=xname,conf.level=conf.level,paired=paired,par=par,var.equal=var.equal,alternative=alternative)
           }
        }
    }
else
    {
    if(par >= 1 || par <= 0)
        par <- 0.5
    if(one.sample || is.null(y))
        prop.htest(x=x,y=NULL,group=group,xname=xname,conf.level=conf.level,par=par,alternative=alternative)
    else
        prop.htest(x=x,y=y,group=group,xname=xname,conf.level=conf.level,par=par,alternative=alternative)
    }
}


t.htest <- function(x=x,y=y,group=group,xname=xname,conf.level=conf.level,paired=paired,par=par,var.equal=var.equal,alternative=alternative)
#used in mean.test
{
by.names <- NULL
group <- NULL # this option is not avalible in Epi-R.
nvars <- ifelse(is.null(y),1,2)
levels <- NULL
htest <- NULL
if(nvars==1)
    {
    htest <- t.test(x=x,conf.level=conf.level,mu=par,alternative=alternative)
    #n = length(!is.na(x))
    }
else
    {
    if(!is.null(group))
        {
        x <- x[y==group]
        y <- group
        htest <- t.test(x=x,conf.level=conf.level,mu=par,alternative=alternative)
        #n = length(!is.na(x[y==group]))
        } 
    else
        {
        if(class(y)!="factor")
            {
            htest <- t.test(x=x,y=y,conf.level=conf.level,mu=par,var.equal=var.equal,alternative=alternative,paired=paired)
            #n = length(!is.na(x))
            }
        else
            {
            levels <- levels(y)
            z <- x[y==levels[1]]
            y <- x[y==levels[2]]
            x <- z
            htest <- t.test(x=x,y=y,conf.level=conf.level,mu=par,var.equal=var.equal,alternative=alternative,paired=paired)
            #n = c(length(x[y==levels[1]]),length(x[y==levels[2]]))
            by <- TRUE
            by.names <- c(levels[1],levels[2])
            }
        }
    }
retval <- list(htest=htest,x=x,y=y,xname=xname,nvars=nvars,group=group,levels=levels,alternative=alternative,par=par,paired=paired,conf.level=conf.level,var.equal=var.equal,by=by,by.names=by.names)
class(retval) <- c("t.htest",class(htest))
return(retval)
}

print.t.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
    digits <- 4
options(scipen=1,envir=.GlobalEnv)
if(x$var.equal)
    var.text <- "Variâncias iguais"
else
    var.text <- "Variâncias diferentes" 
if(x$nvars==1 || !is.null(x$group))
    {
    cat("\t","Teste t de Student para uma amostra","\n","\n")
    if(is.null(x$group))
        cat("Dados: ",x$xname[[1]],"\n")
    else
        cat("Dados: ",x$xname[[1]],"  Categoria de referência: ",x$group,"\n")
    s <- matrix(,1,3)
    rownames(s) <- x$xname[[1]]
    colnames(s) <- c("estimativa","ep","n")
    s[1,1] <- mean(x$x); s[1,2] <- sqrt(var(x$x)); s[1,3] <- length(x$x[!is.na(x$x)]);
    s[1,1:2] <- round(s[1,1:2],digits=digits)
    if(x$alternative=="two.sided")
        {
        hp0.text <- paste("Hipótese nula: a média é igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a média não é igual a ",x$par, sep = "")
        lower <- round(x$htest$conf.int[1],digits)
        upper <- round(x$htest$conf.int[2],digits)
        }
    else if(x$alternative=="less")
        {
        hp0.text <- paste("Hipótese nula: a média é maior ou igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a média é menor que ",x$par, sep = "")
        lower <- x$htest$conf.int[1]
        upper <- round(x$htest$conf.int[2],digits)
        }
    else
        {
        hp0.text <- paste("Hipótese nula: a média é menor ou igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a média é maior que ",x$par, sep = "")
        lower <- round(x$htest$conf.int[1],digits)
        upper <- x$htest$conf.int[2]
        }
    if(x$htest$p.value<0.001)
        p.value <- "valor-p < 0.001"
    else
        p.value <- paste("valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
    cat("\n")
    print(s);
    cat("\n")
    cat(hp0.text,"\n")
    cat(hp1.text,"\n")
    cat("\n")  
    cat("IC", paste((x$conf.level)*100,"%",sep="")," =",paste("[",lower,", " ,upper,"]",sep = ""),"\n")
    cat("t =",paste(round(as.list(x$htest$statistic)$t,digits),",",sep=""), "gl =", paste(round(as.list(x$htest$parameter)$df,digits),",",sep=""),p.value,"\n")
    }
else
    {
    if(x$paired)
        {
        cat("\t","Teste t de Student para amostras pareadas","\n")
        cat("\t","\t","\t",var.text,"\n")
        }
    else
        {
        cat("\t","Teste t de Student para amostras independentes","\n")
        cat("\t","\t","\t",var.text,"\n")
        }
    cat("","\n")
    if(is.null(x$levels))
        cat(paste("Dados: ",x$xname[[1]]," e ", x$xname[[2]],sep = ""),"\n")
    s <- matrix(,3,3)
    if(!x$by)
        rownames(s) <- c(x$xname[[1]],x$xname[[2]],"Dif.")
    else
        rownames(s) <- c(x$by.names,"Dif.")
    colnames(s) <- c("estimativa","ep","n")
    z <- x$x; w <- x$y; dif <- z - w;
    
    s[1,] <- c(mean(z),sqrt(var(z)),length(z[!is.na(z)]))
    s[2,] <- c(mean(w),sqrt(var(w)),length(w[!is.na(w)]))
    s[3,] <- c(mean(dif),sqrt(var(dif)),length(dif[!is.na(dif)]))
    s[1:3,1:2] <- round(s[1:3,1:2],digits=digits)
    if(x$alternative=="two.sided")
        {
        hp0.text <- paste("Hipótese nula: a diferença entre as médias é igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a diferença entre as médias não é igual a ",x$par, sep = "")
        lower <- round(x$htest$conf.int[1],digits)
        upper <- round(x$htest$conf.int[2],digits)
        }
    else if(x$alternative=="less")
        {
        hp0.text <- paste("Hipótese nula: a diferença entre as médias é maior ou igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a diferença entre as médias é menor que ",x$par, sep = "")
        lower <- x$htest$conf.int[1]
        upper <- round(x$htest$conf.int[2],digits)
        }
    else
        {
        hp0.text <- paste("Hipótese nula: a diferença entra as médias é menor ou igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a diferença entra as médias é maior que ",x$par, sep = "")
        lower <- round(x$htest$conf.int[1],digits)
        upper <- x$htest$conf.int[2]
        }
    if(x$htest$p.value<0.001)
        p.value <- "valor-p < 0.001"
    else
        p.value <- paste("valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
    cat("\n")
    print(s);
    cat("\n")
    cat(hp0.text,"\n")
    cat(hp1.text,"\n")
    cat("\n")
    cat("IC", paste((x$conf.level)*100,"%",sep="")," =",paste("[",lower,", " ,upper,"]",sep = ""),"\n")
    cat("t =",paste(round(as.list(x$htest$statistic)$t,digits),",",sep=""), "gl =", paste(round(as.list(x$htest$parameter)$df,digits),",",sep=""),p.value,"\n")
    }
cat("\n","\n")
}

prop.htest <- function(x=x,y=y,group=group,xname=xname,conf.level=conf.level,par=par,alternative=alternative) # x, y should be as factor
#used in mean.test
{
htest <-NULL
nvars <- ifelse(is.null(y),1,2)
if(nvars==1)
    {
    htest <- prop.test(x=length(x[x==group]),n=length(x),conf.level=conf.level,p=par[1],alternative=alternative)
    n = length(x[x==group]) 
    }
else
    {
    htest<- prop.test(x=c(length(x[x==group]),length(y[y==group])),n=c(length(x),length(y)),conf.level=conf.level,alternative=alternative)
    n = c(length(x[x==group]),length(y[y==group]))
    }
retval <- list(htest=htest,x=x,y=y,group=group,n=n,nvars=nvars,xname=xname,alternative=alternative,par=par,conf.level=conf.level)
class(retval) <- c("prop.htest",class(htest))
return(retval)
}



print.prop.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
    digits <- 4
options(scipen=1,envir=.GlobalEnv)
if(x$nvars==1)
    {
    cat("\t","Teste para a proporção","\n","\n")
    cat("Dados:",x$xname[[1]],"\n")
    cat(paste("Grupo de referência: ",x$group,sep=""),"\n")
    s <- matrix(,1,3)
    rownames(s) <- x$xname[[1]]
    colnames(s) <- c("estimativa","ep","n")
    s[1,1] <- x$htest$estimate; s[1,2] <- sqrt(s[1,1]*(1-s[1,1])); s[1,3] <- length(x$x[!is.na(x$x)]);
    s[1,1:2] <- round(s[1,1:2],digits=digits)
    if(x$alternative=="two.sided")
        {
        hp0.text <- paste("Hipótese nula: a proporção é igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a proporção não é igual a ",x$par, sep = "")
        }
    else if(x$alternative=="less")
        {
        hp0.text <- paste("Hipótese nula: a proporção é maior ou igual que ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a média é menor que ",x$par, sep = "")
        }
    else
        {
        hp0.text <- paste("Hipótese nula: a média é menor ou igual a ",x$par, sep = "")
        hp1.text <- paste("Hipótese alternativa: a média é maior que ",x$par, sep = "")
        }
    if(x$htest$p.value<0.001)
        p.value <- "valor-p < 0.001"
    else
        p.value <- paste("valor-p =",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
	lower <- round(x$htest$conf.int[1],digits)
    upper <- round(x$htest$conf.int[2],digits)
    cat("\n")
    print(s);
    cat("\n")
    cat(hp0.text,"\n")
    cat(hp1.text,"\n")
    cat("\n")  
    cat("IC", paste((x$conf.level)*100,"%",sep="")," =",paste("[",lower,", " ,upper,"]",sep = ""),"\n")
    cat("Est. de Pearson =",paste(round(as.list(x$htest$statistic)[[1]],digits),",",sep=""), "gl =", paste(round(as.list(x$htest$parameter)$df,digits),",",sep=""),p.value,"\n")
	}
else
    {
    cat("\t","Teste para a proporção entre duas amostras","\n","\n")
    if(x$xname[[1]]==x$xname[[2]])
        cat("Dados: ",x$xname[[1]],"\n")
    else
        cat("Dados: ",x$xname[[1]]," e ",x$xname[[2]],"\n")
    cat("Estrato de referência: ",x$group,"\n")
    s <- matrix(,3,3)
    rownames(s) <- c(x$xname[[1]],x$xname[[2]],"Dif.")
    colnames(s) <- c("estimativa","ep","n")
    z <- x$x; y <- x[[3]]
    s[1,1] <- x$htest$estimate[[1]]; s[2,1] <- x$htest$estimate[[2]]; s[3,1] <- abs(s[1,1] - s[2,1])
    s[1,2] <- sqrt(s[1,1]*(1-s[1,1]));  s[2,2] <- sqrt(s[2,1]*(1-s[2,1])) ;  s[3,2] <- sqrt(s[3,1]*(1-s[3,1]))
    s[1,3] <- length(z[!is.na(z)]); s[2,3] <- length(y[!is.na(y)]); s[3,3] <- min(s[1,3],s[2,3]) 
    s[1:2,1:2] <- round(s[1:2,1:2],digits=digits)
    if(x$alternative=="two.sided")
        {
        hp0.text <- paste("Hipótese nula: a proporção do estrato nas amostras são iguais", sep = "")
        hp1.text <- paste("Hipótese alternativa: a proporção do estrato nas amostras não são iguais", sep = "")
        }
    else if(x$alternative=="less")
        {
        hp0.text <- paste("Hipótese nula: a proporção do estrato na primeira amostra é maior ou igual que a proporção na segunda",sep = "")
        hp1.text <- paste("Hipótese alternativa: a proporção do estrato na primeira amostra é menor que a proporção na segunda",sep = "")
        }
    else
        {
        hp0.text <- paste("Hipótese nula: a proporção do estrato na primeira amostra é menor ou igual que a proporção na segunda",sep = "")
        hp1.text <- paste("Hipótese alternativa: a proporção do estrato na primeira amostra é maior que a proporção na segunda",sep = "")
        }
    if(x$htest$p.value<0.001)
        p.value <- "valor-p < 0.001"
    else
        p.value <- paste("valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
	lower <- round(x$htest$conf.int[1],digits)
    upper <- round(x$htest$conf.int[2],digits)
    cat("\n")
    print(s);
    cat("\n")
    cat(hp0.text,"\n")
    cat(hp1.text,"\n")
    cat("\n")
    cat("IC", paste((x$conf.level)*100,"%",sep="")," =",paste("[",lower,", " ,upper,"]",sep = ""),"\n")
    cat("Est. de Pearson =",paste(round(as.list(x$htest$statistic)[[1]],digits),",",sep=""), "gl =", paste(round(as.list(x$htest$parameter)$df,digits),",",sep=""),p.value,"\n")
    }
cat("\n","\n")
}

variance.test <- function(dataset,x,y=NULL,filter=NULL,test="var.test",alternative="two.sided",par=1,conf.level=0.95,na.rm=FALSE,group=FALSE)
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(filter))
	dataset <- subset(dataset,subset=eval(parse(text=filter)))
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
		xname[2] <- y
		y <- eval(parse(text=y),envir=dataset)
		}
	else
		xname[2] <- deparse(substitute(y))
	}
if(!alternative %in% c("two.sided","less","greater") || is.null(alternative))
alternative <- "two.sided"
if(class(par)!="numeric")
	par <- 1
if(class(conf.level)!="numeric")
	conf.level <- 0.95
else
	if(conf.level>1)
		conf.level <- conf.level/100
if(na.rm)
	{
	date <- na.omit(data.frame(x,y))
	x <- date[,1]
	y <- date[,2]
	}
if(test=="var.test")
	var.htest(x=x,y=y,xname=xname,conf.level=conf.level,par=par,alternative=alternative,group=group)
else
	{
	y <- as.factor(y)
	if(test=="bartlett.test")
		bartlett.htest(x=x,y=y,xname=xname)
	else
		fligner.htest(x=x,y=y,xname=xname)
	}
}

var.htest <- function(x=x,y=y,xname=xname,conf.level=conf.level, par=par,alternative=alternative,group=group)
{
htest <- NULL
levels <- NULL
n = length(x)
if(!group)
    htest <- var.test(x,y,conf.level=conf.level,ratio=par,paired=paired,alternative=alternative)
else
    {
	y <- as.factor(y)
    levels <- levels(y)
    z <- x[y==levels[1]]
    y <- x[y==levels[2]]
    x <- z
    htest <- var.test(x,y,conf.level=conf.level,ratio=par,paired=paired,alternative=alternative)
    }
retval <- list(htest=htest,x=x,y=y,xname=xname,n=n,alternative=alternative,par=par,conf.level=conf.level,group=group,levels=levels)
class(retval) <- c("var.htest",class(htest))
return(retval)
}

print.var.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
    digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste F para comparação entre duas variâncias","\n")
cat("","\n")
if(!x$group)
    cat(paste("Dados: ",x$xname[[1]]," e ", x$xname[[2]], sep = ""),"\n")
else
   cat(paste("Dados: ",x$xname[[1]]," estratificado por ", x$xname[[2]], sep = ""),"\n")
s <- matrix(,3,2)
if(!x$group)
    rownames(s) <- c(x$xname[[1]],x$xname[[2]],"razão")
else
    rownames(s) <- c(x$levels[[1]],x$levels[[2]],"razão")

colnames(s) <- c("estimativa","n")
z <- x$x; y <-x$y
s[1,1] <- sd(z); s[2,1] <- sd(y); s[3,1] <- s[1,1]/s[2,1]
s[1,2] <- length(!is.na(z)); s[2,2] <- length(!is.na(y)); s[3,2] <- min(s[2,2],s[1,2])
s[1:3,1:1] <- round(s[1:3,1:1],digits=digits)
if(x$alternative=="two.sided")
    {
    hp0.text <- paste("Hipótese nula: a razão das variâncias é igual a ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a razão das variâncias não é igual a ",x$par, sep = "")
    lower <- round(x$htest$conf.int[1],digits)
    upper <- round(x$htest$conf.int[2],digits)
    }
else if(x$alternative=="less")
    {
    hp0.text <- paste("Hipótese nula: a razão das variâncias é maior ou igual que ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a razão das variâncias é menor que ",x$par, sep = "")
    lower <- x$htest$conf.int[1]
    upper <- round(x$htest$conf.int[2],digits)
    }
else
    {
    hp0.text <- paste("Hipótese nula: a razão das variâncias é menor ou igual que ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a razão das variâncias é maior que ",x$par, sep = "")
    lower <- round(x$htest$conf.int[1],digits)
    upper <- x$htest$conf.int[2]
    }
if(x$htest$p.value<0.001)
        p.value <- "valor-p < 0.001"
else
    p.value <- paste("valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
cat("\n")
print(s);
cat("\n")
cat(hp0.text,"\n")
cat(hp1.text,"\n")
cat("\n")
gl.num <- round(as.list(x$htest$parameter)[[1]],digits)
gl.den <- round(as.list(x$htest$parameter)[[2]],digits)
gl <- paste(gl.num,"/",gl.den,sep="")
cat("IC", paste((x$conf.level)*100,"%",sep="")," =",paste("[",lower,", " ,upper,"]",sep = ""),"\n")
cat("F =",paste(round(as.list(x$htest$statistic)$F,digits),",",sep=""), paste("gl = ",gl,",",sep=""),p.value,"\n")
cat("\n","\n")
}

bartlett.htest <- function(x=x,y=y,xname=xname)
{
htest <- bartlett.test(x,g=y)
n = length(x)
retval <- list(htest=htest,n=n,xname=xname)
class(retval) <- c("bartlett.htest",class(htest))
return(retval)
}

print.bartlett.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
	digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste Bartlett para a homogenidade da variância","\n","\n")
cat(paste("Dados: ",x$xname[[1]]," e ",x$xname[[2]], sep = ""),"\n")
cat(paste("Est. Bartlett = ", round(as.list(x$htest$statistic)[[1]],digits), ", gl = ", round(as.list(x$htest$parameter)$df,digits),", valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep = ""),"\n")
cat("\n","\n")
}

fligner.htest <- function(x=x,y=y,xname=xname)
{
htest <- fligner.test(x=x,g=y)
retval <- list(htest=htest,xname=xname)
class(retval) <- c("fligner.htest",class(htest))
return(retval)
}

print.fligner.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
	digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste Fligner para a homogenidade da variância","\n","\n")
cat(paste("Dados: ",x$xname[[1]]," por ",x$xname[[2]], sep = ""),"\n")
cat(paste("Est. Fligner = ", round(as.list(x$htest$statistic)[[1]],digits), ", gl = ", round(as.list(x$htest$parameter)$df,digits),", valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep = ""),"\n")
cat("\n","\n")
}


# non parametric tests


 ## non-parametric statistical test

# teste do sinais
rank.htest <- 
function(dataset,x,y,alternative="two-sided",filter=NULL)
{
if(is.character(dataset))
    dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(filter))
    dataset <- subset(dataset,subset=eval(parse(text=filter)))
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
        xname[2] <- y
        y <- eval(parse(text=y),envir=dataset)
        }
    else
        xname[2] <- deparse(substitute(y))
    }
if(!alternative %in% c("two.sided","less","greater") || is.null(alternative))
    alternative <- "two.sided"
dif <- x - y
c <- ifelse(dif<0.00001,1,0)
n <- length(x)
p.value <- pbinom(sum(c),n,0.5,lower.tail=TRUE,log.p=FALSE)
if(alternative=="two-sided")
    p.value <- 2*p.value
par <- 0
retval <- list(p.value=p.value,x=x,y=y,xname=xname,alternative=alternative,par=par)
class(retval) <- c("rank.htest")
return(retval)
}


print.rank.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
    digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste dos Sinais","\n","\n")
cat(paste("Dados: ",x$xname[[1]]," e ", x$xname[[2]],sep = ""),"\n")
s <- matrix(,3,3)
rownames(s) <- c(x$xname[[1]],x$xname[[2]],"Dif.")
colnames(s) <- c("estimativa*","ep","n")
z <- x$x; y <- x[[3]]; dif <- z - y;
s[1,1] <- median(z); s[2,1] <- median(y); s[3,1] <- median(dif)
s[1,2] <- sqrt(var(z));  s[2,2] <- sqrt(var(y));  s[3,2] <- sqrt(var(dif))
s[1,3] <- length(z[!is.na(z)]); s[2,3] <- length(y[!is.na(y)]); s[3,3] <-length(dif[!is.na(dif)]);
s[1:3,1:2] <- round(s[1:3,1:2],digits=digits)
if(x$alternative=="two.sided")
    {
    hp0.text <- paste("Hipótese nula: a diferença entre as medianas é igual a ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a diferença entre as medianas não é igual a ",x$par, sep = "")
    }
else if(x$alternative=="less")
    {
    hp0.text <- paste("Hipótese nula: a diferença entre as medianas é maior ou igual a ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a diferença entre as medianas é menor que ",x$par, sep = "")
     }
else
     {
     hp0.text <- paste("Hipótese nula: a diferença entra as medianas é menor ou igual a ",x$par, sep = "")
     hp1.text <- paste("Hipótese alternativa: a diferença entra as medianas é maior que ",x$par, sep = "")
      }
if(x$p.value<0.001)
    p.value <- "Valor-p < 0.001"
else
    p.value <- paste("Valor-p = ",round(x$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
cat("\n")
print(s);
cat("(*)estimativa amostral para a mediana", "\n")
cat("\n")
cat(hp0.text,"\n")
cat(hp1.text,"\n")
cat("\n")
cat(p.value,"\n")
cat("\n","\n")
}

# Mann-Whitney ou Wilcoxon rank

wilcoxon.htest <- function(dataset,x,y,alternative="two-sided",mu=0,paired=FALSE,conf.int=TRUE,conf.level=0.95,filter=NULL)
{
if(is.character(dataset))
    dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(filter))
    dataset <- subset(dataset,subset=eval(parse(text=filter)))
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
        xname[2] <- y
        y <- eval(parse(text=y),envir=dataset)
        }
    else
        xname[2] <- deparse(substitute(y))
    }
if(!alternative %in% c("two.sided","less","greater") || is.null(alternative))
    alternative <- "two.sided"
if(class(conf.level)!="numeric")
    conf.level <- 0.95
else
    if(conf.level>1)
        conf.level <- conf.level/100

htest <- wilcox.test(x=x,y=y,alternative=alternative,mu=mu,paired=paired,conf.int=conf.int,conf.level=conf.level)
retval <- list(htest=htest,x=x,y=y,xname=xname,alternative=alternative,paired=paired,conf.level=conf.level,par=mu)
class(retval) <- c("wilcoxon.htest",class(htest))
return(retval)
}

print.wilcoxon.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
    digits <- 4
options(scipen=1,envir=.GlobalEnv)
if(x$paired)
    cat("\t","Teste de Mann-Whitney para amostras pareadas","\n")
else
    cat("\t","Teste de Mann-Whitney para amostras independentes","\n")
cat("","\n")
cat(paste("Dados: ",x$xname[[1]]," e ", x$xname[[2]],sep = ""),"\n")
s <- matrix(,3,3)
rownames(s) <- c(x$xname[[1]],x$xname[[2]],"Dif.")
colnames(s) <- c("estimativa*","ep","n")
z <- x$x; y <- x[[3]]; dif <- z - y;
s[1,1] <- mean(z); s[2,1] <- mean(y); s[3,1] <- mean(dif)
s[1,2] <- sqrt(var(z));  s[2,2] <- sqrt(var(y));  s[3,2] <- sqrt(var(dif))
s[1,3] <- length(z[!is.na(z)]); s[2,3] <- length(y[!is.na(y)]); s[3,3] <-length(dif[!is.na(dif)]);
s[1:3,1:2] <- round(s[1:3,1:2],digits=digits)
if(x$alternative=="two.sided")
    {
    hp0.text <- paste("Hipótese nula: a diferença entre as medianas é igual a ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a diferença entre as medianas não é igual a ",x$par, sep = "")
    lower <- round(x$htest$conf.int[1],digits)
    upper <- round(x$htest$conf.int[2],digits)
    }
else if(x$alternative=="less")
    {
    hp0.text <- paste("Hipótese nula: a diferença entre as medianas é maior ou igual a ",x$par, sep = "")
    hp1.text <- paste("Hipótese alternativa: a diferença entre as medianas é menor que ",x$par, sep = "")
    lower <- x$htest$conf.int[1]
    upper <- round(x$htest$conf.int[2],digits)
    }
else
    {
     hp0.text <- paste("Hipótese nula: a diferença entra as medianas é menor ou igual a ",x$par, sep = "")
     hp1.text <- paste("Hipótese alternativa: a diferença entra as medianas é maior que ",x$par, sep = "")
     lower <- round(x$htest$conf.int[1],digits)
     upper <- x$htest$conf.int[2]
     }
if(x$htest$p.value<0.001)
    p.value <- "valor-p < 0.001"
else
    p.value <- paste("valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
cat("\n")
print(s);
cat("(*)estimativa amostral para a mediana", "\n")
cat("\n")
cat(hp0.text,"\n")
cat(hp1.text,"\n")
cat("\n")
cat("IC", paste(round(attr(x$htest$conf.int,"conf.level")*100,2)
,"%",sep="")," =",paste("[",lower,", " ,upper,"]",sep = ""),"\n")
    cat("Est. Wilcoxon =",paste(round(as.list(x$htest$statistic)[[1]],digits),",",sep=""),p.value,"\n")
cat("\n","\n")
}

# Friedman test 

#http://www.gardenersown.co.uk/Education/Lectures/R/nonparam.htm#friedman

friedman.htest <- function(dataset,x,y,z=NULL,na.action="na.omit",filter=NULL,as.factor=FALSE)
{  							# x is always numeric but y,z can be factor   options: if y numeric then will not be used z
if(is.character(dataset))
    dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(filter))
    dataset <- subset(dataset,subset=eval(parse(text=filter)))
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
        xname[2] <- y
        y <- eval(parse(text=y),envir=dataset)
        }
    else
        xname[2] <- deparse(substitute(y))
    }
if(!is.null(z))
    {
    if(is.character(z))
        {
        xname[3] <- z
        z <- eval(parse(text=z),envir=dataset)
        }
    else
        xname[3] <- deparse(substitute(z))
    }
if(as.factor)
    {
    y <- as.factor(y)
    if(!is.null(z))
        z <- as.factor(z)
    else
        z <- as.factor(rep(1,length(x)))
    }
else
    {
    y <- as.numeric(y)
        if(!is.null(z))
            z <- as.numeric(z)
    }
if(is.numeric(y))
    {
    if(is.numeric(z))
	   {
	   nmax=max(c(length(x),length(y),length(z)))
	   m <- matrix(NA,nrow=nmax,ncol=3,dimnames=list(1:nmax,xname))
	   m[,] <- c(x,y,z) 
	   htest <- friedman.test(m,na.action=na.action)
	   }
    else
	{
	nmax=max(c(length(x),length(y)))
	m <- matrix(NA,nrow=nmax,ncol=2,dimnames=list(1:nmax,xname[1:2]))
	m[,] <- c(x,y)
	htest <- friedman.test(m,na.action=na.action)
	}
    retval <- list(htest=htest,x=x,y=y,z=z,xname=xname)
    }
else
    {
    if(is.factor(z))
	{
	a <- aggregate(x,by=list(y=y,z=z),FUN="mean")
        names(a) <- c(xname[2:3],xname[1])
        htest <- friedman.test(x ~ y|z,data=a,na.action=na.action)
	}
    retval <- list(htest=htest,x=x,y=y,z=z,xname=xname,a=a)
    }
class(retval) <- c("friedman.htest",class(htest))
return(retval)
}



print.friedman.htest <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
cat("\n")
if(is.null(digits))
    digits <- 4
options(scipen=1,envir=.GlobalEnv)
cat("\t","Teste de Friedman","\n")
cat("\n")
if(class(x$y)!="factor")
    {
    if(is.null(x$z))
        {
        cat(paste("Dados: ",x$xname[[1]]," e ",x$xname[[2]],sep = ""),"\n")
        s <- matrix(,2,3)
        rownames(s) <- c(x$xname[[1]],x$xname[[2]])
        colnames(s) <- c("estimativa","ep","n")
        w <- x$x; y <- x$y; 
        s[1,1] <- mean(w); s[2,1] <- mean(y); 
        s[1,2] <- sqrt(var(w));  s[2,2] <- sqrt(var(y));  
        s[1,3] <- length(z[!is.na(w)]); s[2,3] <- length(y[!is.na(y)]);
        s[1:2,1:2] <- round(s[1:2,1:2],digits=digits)    
        }
    else
        {
        cat(paste("Dados: ",x$xname[[1]],", ",x$xname[[2]]," e ", x$xname[[3]],sep = ""),"\n")
        s <- matrix(,3,3)
        rownames(s) <- c(x$xname[[1]],x$xname[[2]],x$xname[[3]])
        colnames(s) <- c("estimativa","ep","n")
        w <- x$x; y <- x$y; z <- x$z
        s[1,1] <- mean(w); s[2,1] <- mean(y); s[3,1] <- mean(z)
        s[1,2] <- sqrt(var(w));  s[2,2] <- sqrt(var(y));  s[3,2] <- sqrt(var(z))
        s[1,3] <- length(z[!is.na(w)]); s[2,3] <- length(y[!is.na(y)]); s[3,3] <-length(z[!is.na(z)]);
        s[1:3,1:2] <- round(s[1:3,1:2],digits=digits)
        }
    }
else
    {
    cat(paste("Dados: ",x$xname[[1]]," agrupado por ", x$xname[[2]]," e ", x$xname[[3]],sep = ""),"\n")
    s <- x$a
    names(s)[3] <- paste(names(s)[3]," (média)",sep="")
    }
hp0.text <- paste("Hipótese nula: as médias nos grupos são iguais ",sep = "")
hp1.text <- paste("Hipótese alternativa: pelo menos um média é diferente",sep = "")
if(x$htest$p.value<0.001)
    p.value <- "valor-p < 0.001"
else
    p.value <- paste("valor-p = ",round(x$htest$p.value,EPIR_OPTION_DIGITS_PVALUE),sep="")
cat("\n")
print(s);
cat("\n")
cat(hp0.text,"\n")
cat(hp1.text,"\n")
cat("\n")
cat("Est. Qui-Quadrada de Friedman =",paste(round(as.list(x$htest$statistic)[[1]],digits),",",sep=""),p.value,"\n")
cat("\n","\n")
}