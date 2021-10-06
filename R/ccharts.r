control.charts <- function(dataset,x,date=NULL,type="daily",date.month=NULL,date.year=NULL,year,until.year=F,simple.method=T,median.method=F,limit=1.96,col="red",lty=2,size=2,title="",subtitle="",xlab=NULL,ylab=NULL,year.plot=T)
{  
if(is.character(dataset))
		dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(x))
	{
	if(is.character(x))
		{
		xname <- x
		x <- as.numeric(eval(parse(text=x),envir=dataset))
		}
	else
		xname <- deparse(substitute(x))
	}
if(!is.null(date))
	{
	if(is.character(date))
		{
		date.name <- date
		date <- as.Date(as.character(eval(parse(text=date),envir=dataset)))
		}
	else
		date.name <- deparse(substitute(date))
	}
if(!is.null(date.month))
	if(is.character(date.month))
                {
                date.month.name <- date.month
		date.month <- as.character(eval(parse(text=date.month),envir=dataset))
                }
if(!is.null(date.year))
	if(is.character(date.year))
                {
                date.year.name <- date.year
		date.year <- as.character(eval(parse(text=date.year),envir=dataset))
                }
year <- as.character(year)
n <- length(x)
if(!is.null(date.month))
    dataset[names(dataset)==date.month.name][,1] <- as.character(dataset[names(dataset)==date.month.name][,1])
if(!is.null(date.year))
    dataset[names(dataset)==date.year.name][,1] <- as.character(dataset[names(dataset)==date.year.name][,1])
dataset[names(dataset)==xname][,1] <- as.numeric(dataset[names(dataset)==xname][,1])
if(until.year)
    {
    if(is.null(date.year))
        data <- subset(dataset,years(date) < year)
    else
        data <- subset(dataset,date.year < year)
    }
else
    {
    if(is.null(date.year))
        data <- subset(dataset,years(date) != year)
    else
        data <- subset(dataset,date.year != year)
    }
if(is.null(date.year))
    control.data <- subset(dataset,years(date) == year)
else
    control.data <- subset(dataset,as.character(date.year) == year)
dataset <- NULL
meses <- c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")
months.number <- c("01","02","03","04","05","06","07","08","09","10","11","12") 


if(is.null(date.month))    
    {
    if(sum((unique(months(as.Date(as.character(date)))) %in% meses)) != 0)
        months.data <- meses
    else
        months.data <- c("january","february","march","april","may","june","july","august","september","october","november","december")
    }
else
    {
    if(sum((unique(as.character(date.month)) %in% meses)) != 0)
        months.data <- meses
    else
        {
        if(sum((unique(as.character(date.month)) %in% months.number)) != 0)
            {
            for(l in 1:12)
                   {
                    for(k in 1:length(date.month))
                        {
                        if(date.month[k]==months.number[l])
                            date.month[k] <- meses[l]
                        }
                     for(k in 1:dim(data)[1])
                        {
                        if(data[names(data)==date.month.name][k,1]== months.number[l])
                            data[names(data)==date.month.name][k,1] <- meses[l]
                        }
                        for(k in 1:dim(control.data)[1])
                        {
                        if(control.data[names(control.data)==date.month.name][k,1]==months.number[l])
                            control.data[names(control.data)==date.month.name][k,1] <- meses[l]
                        }
                    }
                }
            months.data <- meses
            }
        }
mean.x <- sd.x <- median.x <- y <- NULL
upper <- lower <- NULL
if(is.null(date))
    {
    if(!is.null(date.year) && !is.null(date.month))
        {
        data <- data.frame(data[names(data)==xname],data[names(data)==date.month.name],data[names(data)==date.year.name])
        control.data <- data.frame(control.data[names(control.data)==xname],control.data[names(control.data)==date.month.name],control.data[names(control.data)==date.year.name])
        #names(data) <- c(xname,date.year.name,date.month.name)
        }
    else if(is.null(date.year))
        {
        data <- data.frame(data[names(data)==xname],data[names(data)==date.month.name])
        control.data <- data.frame(control.data[names(control.data)==xname],control.data[names(control.data)==date.month.name])
        names(data) <- c(xname,date.month.name)
        }
    }
else
    {
    data <- data.frame(data[names(data)==xname],data[names(data)==date.name])
    control.data <- data.frame(control.data[names(control.data)==xname],control.data[names(control.data)==date.name])
    names(data) <- c(xname,date.name)
     }   
if(type=="weekly")
    {
    sy <- as.character(sort(unique(data[names(data)==date.year.name][,1])))
    aux <- matrix(nrow=length(sy),ncol=12)    
    a <- as.numeric(data[,1])
    b <- as.character(data[,2])
    c <- as.character(data[,3])
   for(j in 1:length(sy))
        {
        for(i in 1:12)
            {
            if(class(subset(a,b == months.data[i] & c == sy[j]))!="numeric")
               aux[j,i] <- NA
            else
                {
                if(simple.method)
                    aux[j,i] <- mean(subset(a,b == months.data[i] & c == sy[j]))
                else
                    aux[j,i] <- median(subset(a,b == months.data[i] & c == sy[j]))
                }
            }
        }
        d <- as.numeric(control.data[,1])
        e <- as.character(control.data[,2])
        f <- as.character(control.data[,3])
        for(i in 1:12)
            {
            if(sum(is.na(aux[,i]))==length(aux[,i]))
                mean.x[i] <- sd.x[i] <- median.x[i] <- NA
            if(simple.method)
                {
                mean.x[i] <- mean(aux[,i],na.rm=T)
                sd.x[i] <- sd(aux[,i],na.rm=T)
                }
            else
                {
                median.x[i] <- median(aux[,i],na.rm=T)
                upper[i] <- quantile(aux[,i],probs=0.75,na.rm=T) 
                lower[i] <- quantile(aux[,i],probs=0.25,na.rm=T)
                }
        if(class(subset(d,e == months.data[i]))!="numeric") 
            y[i] <- NA
        else
            {
            if(simple.method)
                y[i] <- mean(subset(d,e == months.data[i]))
            else
                y[i] <- median(subset(d,e == months.data[i]))
            }
         }
    a <- NULL
    b <- NULL
    c <- NULL
    d <- NULL
    e <- NULL
    f <- NULL
    }
else if(type=="daily")
    {
    a <- as.numeric(data[,1])
    b <- as.character(data[,2])
    c <- as.numeric(control.data[,1])
    d <- as.character(control.data[,2])
    for(i in 1:12)
        {
        if(class(subset(a,months(as.Date(as.character(b))) == months.data[i]))!="numeric")
            mean.x[i] <- sd.x[i] <- median.x[i] <- NA
        else
            {
            if(simple.method)
                {
                mean.x[i] <- mean(subset(a,months(as.Date(as.character(b))) == months.data[i]))
                sd.x[i] <- sd(subset(a,months(as.Date(as.character(b))) == months.data[i]))
                }
            else
                {
                median.x[i] <- median(subset(a,months(as.Date(as.character(b))) == months.data[i]))
                upper[i] <- quantile(subset(a,months(as.Date(as.character(b))) ==  months.data[i]),probs=0.75,na.rm=T) 
                lower[i] <- quantile(subset(a,months(as.Date(as.character(b))) == months.data[i]),probs=0.25,na.rm=T)
                }
            }
        if(class(subset(a,months(as.Date(as.character(d))) == months.data[i]))!="numeric") 
            y[i] <- NA
        else
            {
            if(simple.method)
                y[i] <- mean(subset(c,months(as.Date(as.character(d))) == months.data[i]))
            else
                y[i] <- median(subset(c,months(as.Date(as.character(d))) == months.data[i])) 
            }
        }
    a <- NULL
    b <- NULL
    c <- NULL
    d <- NULL
    }
else if(type=="monthly")
    {
    a <- as.numeric(data[,1])
    b <- as.character(data[,2])
    d <- as.numeric(control.data[,1])
    e <- as.character(control.data[,2])
    for(i in 1:12)
        {
        if(class(subset(a,b == months.data[i]))!="numeric")
            mean.x[i] <- sd.x[i] <- median.x[i] <- NA
        else
            {
            if(simple.method)
                {
                mean.x[i] <- mean(subset(a,b == months.data[i]))
                sd.x[i] <- sd(subset(a,b == months.data[i]))
                }
            else
                {
                median.x[i] <- median(subset(a,b == months.data[i]))
                upper[i] <- quantile(subset(a,b == months.data[i]),probs=0.75,na.rm=T) 
                lower[i] <- quantile(subset(a,b == months.data[i]),probs=0.25,na.rm=T)
                }
            }
        if(class(subset(d,e == months.data[i]))!="numeric") 
            y[i] <- NA
        else
            {
            if(simple.method)
                y[i] <- mean(subset(d,e == months.data[i]))
            else
                y[i] <- median(subset(d,e == months.data[i])) 
            }
        }
    a <- NULL
    b <- NULL
    c <- NULL
    d <- NULL
    e <- NULL
    }
data <- control.data <- NULL       
if(simple.method)
    {
    upper <- mean.x + sd.x*limit
    lower <- mean.x - sd.x*limit
    }

method <- list(mean.x=mean.x,median.x=median.x,y=y,upper=upper,lower=lower)
retval <- list(method=method,col=col,lty=lty,xname=xname,size=size,title=title,subtitle=subtitle,xlab=xlab,ylab=ylab,year.plot=year.plot)
class(retval) <- "control.charts"
plot(retval)
}






years <- function(date,type="yyyy-mm-dd")
  {
  if(type=="yyyy-mm-dd")
    x <- substring(date,first=1,last=4) 
return(x)
  }
    



plot.control.charts <- function(x,...)
{
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
layout(matrix(1:1,1,1,byrow=TRUE))
ylab <- ifelse(is.null(x$ylab),x$xname,x$ylab)
xlab <- ifelse(is.null(x$ylab),"Mês",x$xlab)
if(!is.null(x$method$mean))
    {
    mean.x <- x$method$mean.x
    y <- x$method$y
    u <- x$method$upper
    l <- x$method$lower
    n <- length(l)
    plot(y,type="l",pch=8,col="blue",xlab="",ylab=ylab,lwd=x$size,main="",ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T)),axes=F) 
    axis(2, at=axTicks(2))
    par(new=T)
    plot(u,col=x$col,xlab="",ylab="",type="l",lty=x$lty,lwd=x$size,axes=F,ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T)))
    par(new=T)
    plot(l,col=x$col,xlab="",ylab="",type="l",lty=x$lty,lwd=x$size,axes=F,ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T)))
    if(x$year.plot)
        {
        par(new=T)
        plot(mean.x,col="black",xlab="",ylab="",type="l",lwd=x$size,pch=8,axes=F,ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T))) 
        }
    axis(1,at=1:12,as.character(seq(1:12)))
    if(x$year.plot)
        {
        legend("topright",lty=c(1,2), pch=c(-1,-1),col=c("blue",x$col,"black"),c("Média no período","Limites de controle","Média histórica"),bty="n",cex=0.7)   
        }
    else
        legend("topright",lty=c(1,2), pch=c(-1,-1),col=c("blue",x$col),c("Média no período","Limites de controle"),bty="n",cex=0.7)
    box()
    }
else
    {
    median.x <- x$method$median.x
    y <- x$method$y
    u <- x$method$upper
    l <- x$method$lower
    n <- length(l)
    plot(y,type="l",pch=8,col="blue",xlab="",ylab=ylab,lwd=x$size,main="",ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T)),axes=F) 
    axis(2, at=axTicks(2))
    par(new=T)
    plot(u,col=x$col,xlab="",ylab="",type="l",lty=x$lty,lwd=x$size,axes=F,ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T)))
    par(new=T)
    plot(l,col=x$col,xlab="",ylab="",type="l",lty=x$lty,lwd=x$size,axes=F,ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T)))
    if(x$year.plot)
        {
        par(new=T)
        plot(median.x,col="black",xlab="",ylab="",type="l",lwd=x$size,pch=8,axes=F,ylim=c(0.95*min(l,na.rm=T),1.15*max(u,na.rm=T))) 
        }
    axis(1,at=1:12,as.character(seq(1:12)))
    if(x$year.plot)
        {
        legend("topright",lty=c(1,2), pch=c(-1,-1),col=c("blue",x$col,"black"),c("Mediana no período","Limites de controle","Mediana histórica"),bty="n",cex=0.7)   
        }
    else
        legend("topright",lty=c(1,2), pch=c(-1,-1),col=c("blue",x$col),c("Mediana no período","Limites de controle"),bty="n",cex=0.7)
    box()
    }
par(oma=c(5,2,2,2))
title(main=x$title,sub=x$subtitle,xlab=xlab,outer=TRUE)
}