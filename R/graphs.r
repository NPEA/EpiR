# graphs functions
# numeric plots
simple.plot <- function(dataset,x,...,by=NULL,type="p",col="auto",lty="auto",pch="auto",size=1,overlay="none",hline=NULL,col.hline="auto",title="",subtitle="",xlab="",ylab="",legend=TRUE,legend.pos="topright",legend.horiz=FALSE)
# generate a simple plot
{
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(is.character(x))
	{
	xname <- x
	x <- eval(parse(text=x),envir=dataset)
	}
else
	xname <- deparse(substitute(x))
vars <- paste(match.call(expand.dots = FALSE)$...)
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
	by <- as.factor(rep("",dim(dataset)[1])	)
	}
levels <- levels(by)
nlevels <- length(levels)
nvars <- length(vars)
col.hline <- ifelse(col.hline=="auto","black",col.hline)
if(overlay=="levels")
	{
	if(col[1]=="auto")
		col <- rainbow(nlevels)
	else if(length(col)<nlevels)
		col <- rep(col[1],nlevels)
	if(lty[1]=="auto")
		lty <- 1:6
	else if(length(lty)<nlevels)
		lty <- rep(lty[1],nlevels)
	if(pch[1]=="auto")
		pch <- 18:(18+nlevels)
	else if(length(pch)<nlevels)
		pch <- rep(pch[1],nlevels)
	#layout
	if(length(hline)<nvars)
		hline <- rep(hline[1],nvars)
	if(nvars>5)
		nvars <- 5
	layout(matrix(1:(nvars*1),nvars,1,byrow=TRUE))
	for(v in 1:nvars)
		{
		y <- eval(parse(text=vars[v]),envir=dataset)
		ylim <- c(min(y,na.rm=TRUE),max(y,na.rm=TRUE))
		data <- subset(cbind.data.frame(x=x,y=y),by==levels[1])
		plot(data$x,data$y,type=type,lwd=size,pch=pch[1],cex=size,ylim=ylim,col=col[1],lty=lty[1],xlab="",ylab="",main="",sub="")
		if(!is.null(hline))
			abline(h=hline[v],col=col.hline,lwd=1)
		title(ylab=vars[v])
		if(nlevels>1)
			for(l in 2:nlevels)
				{
				data <- subset(cbind.data.frame(x=x,y=eval(parse(text=vars[v]),envir=dataset)),by==levels[l])
				lines(data$x,data$y,type=type,lwd=size,pch=pch[l],cex=size,col=col[l],lty=lty[l])
				}
		if(v==nvars)
			if(xlab == "")
				title(xlab=xname)
			else
				title(xlab=paste(xname,"\n", xlab))
		if((legend==TRUE) & (overlay!="none"))
			{
			if(type=="p")
				legend(legend.pos,legend=levels,col=col,pch=pch,horiz=legend.horiz,box.lty=0)
			else if(type=="b")
				legend(legend.pos,legend=levels,col=col,lty=lty,lwd=size,pch=pch,horiz=legend.horiz,box.lty=0)
			else
				legend(legend.pos,legend=levels,col=col,lty=lty,lwd=size,horiz=legend.horiz,box.lty=0)
			}
		} 
	}
else if(overlay=="variables")
	{
	if(col[1]=="auto")
		col <- rainbow(nvars)
	else if(length(col)<nvars)
		col <- rep(col[1],nvars)
	if(lty[1]=="auto")
		lty <- 1:6
	else if(length(lty)<nvars)
		lty <- rep(lty[1],nvars)
	if(pch[1]=="auto")
		pch <- 18:(18+nvars)
	else if(length(pch)<nvars)
		pch <- rep(pch[1],nvars)
	#layout
	if(length(hline)<nlevels)
		hline <- rep(hline[1],nlevels)
	if(nlevels>5)
		nlevels <- 5
	data <- NULL
	for(v in vars)
		data <- cbind(data,eval(parse(text=v),envir=dataset))
	ylim <- c(min(data,na.rm=TRUE),max(data,na.rm=TRUE))
	layout(matrix(1:(1*nlevels),nlevels,1,byrow=TRUE))
	for(l in 1:nlevels)
		{
		data <- subset(cbind.data.frame(x=x,y=eval(parse(text=vars[1]),envir=dataset)),by==levels[l])
		plot(data$x,data$y,type=type,lwd=size,pch=pch[1],cex=size,ylim=ylim,col=col[1],lty=lty[1],xlab="",ylab="",main="",sub="")
		if(!is.null(hline))
			abline(h=hline[l],col=col.hline,lwd=1)
		if(nlevels > 1)
			title(ylab=paste(byname,": ",levels[l]))
		else
			title(ylab="")
		if(nvars>1)
			for(v in 2:nvars)
				{
				data <- subset(cbind.data.frame(x=x,y=eval(parse(text=vars[v]),envir=dataset)),by==levels[l])
				y <- eval(parse(text=vars[v]),envir=dataset)
				lines(data$x,data$y,type=type,lwd=size,pch=pch[v],cex=size,col=col[v],lty=lty[v])
				}		
		if((legend==TRUE) & (overlay!="none"))
			{
			if(type=="p")
				legend(legend.pos,legend=vars,col=col,pch=pch,horiz=legend.horiz,box.lty=0)
			else if(type=="b")
				legend(legend.pos,legend=vars,col=col,lty=lty,lwd=size,pch=pch,horiz=legend.horiz,box.lty=0)
			else
				legend(legend.pos,legend=vars,col=col,lty=lty,lwd=size,horiz=legend.horiz,box.lty=0)
			}
		} 
	}
else
	{
	if(col[1]=="auto")
		col <- rainbow(nvars)
	else if(length(col)<nvars)
		col <- rep(col[1],nvars)
	lty <- ifelse(lty[1]=="auto","solid",lty) 
	pch <- ifelse(pch[1]=="auto",21,pch) 
	if(length(hline)<nvars)
		hline <- rep(hline[1],nvars)
	#layout
	if(nvars>5)
		nvars <- 5
	if(nlevels>5)
		nlevels <- 5
	layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
	for(v in 1:nvars)
		{
		for(l in 1:nlevels)
			{
			y <- eval(parse(text=vars[v]),envir=dataset)
			ylim <- c(min(y,na.rm=TRUE),max(y,na.rm=TRUE))
			data <- subset(cbind.data.frame(x=x,y=y),by==levels[l])
			plot(data$x,data$y,type=type,lwd=size,pch=pch,cex=size,ylim=ylim,col=col[v],lty=lty,xlab="",ylab="",main="",sub="")
			if(!is.null(hline))
				abline(h=hline[v],col=col.hline,lwd=1)
			if(l==1)
				title(ylab=vars[v])
			if(v==nvars)
				if(xlab == "")
					{
					if(nlevels > 1)
						title(xlab=xname,sub=paste(byname,": ",levels[l]))
					else
						title(xlab=xname)
					}
				else
					{
					if(nlevels > 1)
						title(xlab=xlab,sub=paste(byname," ",levels[l]))
					else
						title(xlab=xlab,sub="")
					}
				}
			}
		}
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab="",ylab=ylab,outer=TRUE)       
}


dispersion.plot <- function(dataset,...,by=NULL,col="auto",pch="auto",size=1,overlay="none",hline=NULL,col.hline="auto",title="",subtitle="",xlab="",ylab="",legend=TRUE,legend.pos="topright",legend.horiz=FALSE)
# generate a dispersion plot
{
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
pairs <- paste(match.call(expand.dots = FALSE)$...)
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
	by <- as.factor(rep("",dim(dataset)[1])	)
	}
levels <- levels(by)
nlevels <- length(levels)
npairs <- length(pairs) # should be found a better way to deal with it
col.hline <- ifelse(col.hline=="auto","black",col.hline)
if(overlay=="levels")
	{
	if(col[1]=="auto")
		col <- rainbow(nlevels)
	else if(length(col)<nlevels)
		col <- rep(col[1],nlevels)
	if(pch[1]=="auto")
		pch <- 18:(18+nlevels)
	else if(length(pch)<nlevels)
		pch <- rep(pch[1],nlevels)
	#layout
	if(length(hline)<npairs)
		hline <- rep(hline[1],npairs)
	if(npairs>5)
		npairs <- 5
	layout(matrix(1:(npairs*1),npairs,1,byrow=TRUE))
	for(v in 1:npairs)
		{
		frame <- model.frame(as.formula(pairs[v]),data=dataset) # it's not very efficient but necessary in order to get the y's limits
		xname <- colnames(frame)[2]
		yname <- colnames(frame)[1]
		y <- eval(parse(text=paste("frame$",yname,sep="")),envir=dataset)
		colnames(frame) <- c("y","x")
		ylim <- c(min(y,na.rm=TRUE),max(y,na.rm=TRUE))
		data <- subset(frame,by==levels[1])
		plot(data$x,data$y,type="p",pch=pch[1],cex=size,ylim=ylim,col=col[1],xlab="",ylab="",main="",sub="")
		if(!is.null(hline))
			abline(h=hline[v],col=col.hline,lwd=1)
		title(ylab=yname)
		if(nlevels>1)
			for(l in 2:nlevels)
				{
				data <- subset(frame,by==levels[l])
				points(data$x,data$y,pch=pch[l],cex=size,col=col[l])
				}
		if(v==npairs)
			title(xlab=xname)
		if((legend==TRUE) & (overlay!="none"))
			legend(legend.pos,legend=levels,col=col,pch=pch,horiz=legend.horiz,box.lty=0)
		} 
	}
else
	{
	if(col[1]=="auto")
		col <- rainbow(npairs)
	else if(length(col)<npairs)
		col <- rep(col[1],npairs)
	pch <- ifelse(pch[1]=="auto",21,pch) 
	if(length(hline)<npairs)
		hline <- rep(hline[1],npairs)
	#layout
	if(npairs>5)
		npairs <- 5
	if(nlevels>5)
		nlevels <- 5
	layout(matrix(1:(npairs*nlevels),npairs,nlevels,byrow=TRUE))
	for(v in 1:npairs)
		{
		for(l in 1:nlevels)
			{
			frame <- model.frame(as.formula(pairs[v]),data=dataset) # it's not very efficient but necessary in order to get the y's limits
			xname <- colnames(frame)[2]
			yname <- colnames(frame)[1]
			y <- eval(parse(text=paste("frame$",yname,sep="")),envir=dataset)
			colnames(frame) <- c("y","x")
			ylim <- c(min(y,na.rm=TRUE),max(y,na.rm=TRUE))
			data <- subset(frame,by==levels[l])
			plot(data$x,data$y,type="p",pch=pch,cex=size,ylim=ylim,col=col[v],xlab="",ylab="",main="",sub="")
			if(!is.null(hline))
				abline(h=hline[v],col=col.hline,lwd=1)
			if(l==1)
				title(ylab=yname)
			#if(v==npairs)
			#	{
				if(nlevels > 1)
					title(xlab=xname,sub=paste(byname,": ",levels[l]))
				else
					title(xlab=xname,sub="")
			#	}
			}
		} 
	}
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab=xlab,ylab=ylab,outer=TRUE)       
}

histogram.plot <- function(dataset,...,by=NULL,nbreaks=0,frequency=TRUE,col="auto",title="",subtitle="",xlab="",ylab="")
# generate a histogram plot
{
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
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
	by <- as.factor(rep("",dim(dataset)[1])	)
	}
levels <- levels(by)
nlevels <- length(levels)
nvars <- length(vars)
if(col[1]=="auto")
	col <- rainbow(nvars)
else if(length(col)<nvars)
	col <- rep(col[1],nvars)
if(nbreaks==0)
	nbreaks <- "Sturges"
else
	nbreaks <- nbreaks
#layout
if(nvars>4)
	nvars <- 4
if(nlevels>4)
	nlevels <- 4
layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
for(v in 1:nvars)
	{	
	range_hist = tapply(eval(parse(text=vars[v]),envir=dataset),by,hist,plot=F)	
	range_y <- NULL	
	for(i in 1:nlevels)
		range_y <- c(range_y,range(range_hist[[i]]$counts))		
	range_y <- range(range_y)
	for(l in 1:nlevels)
		{
		y <- eval(parse(text=vars[v]),envir=dataset)
		data <- subset(y,by==levels[l])
		hist(data,breaks=nbreaks,freq=frequency,col=col[v],xlab="",ylab="",main="",sub="",ylim=range_y)
		if(l==1)
			title(ylab=vars[v])
		if(v==nvars)
			{
			if(nlevels > 1)
				title(sub=paste(byname,": ",levels[l]))
			else
				title(sub="")
			}
		}
	} 
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab=xlab,ylab=ylab,outer=TRUE)       
}

normal.plot <- function(dataset,...,by=NULL,col="auto",line=TRUE,col.line="auto",title="",subtitle="",xlab="",ylab="")
# generate a normal plot
{
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
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
	by <- as.factor(rep("",dim(dataset)[1])	)
	}
levels <- levels(by)
nlevels <- length(levels)
nvars <- length(vars) 
col.line <- ifelse(col.line=="auto","black",col.line)
if(col[1]=="auto")
	col <- rainbow(nvars)
else if(length(col)<nvars)
	col <- rep(col[1],nvars)
#layout
if(nvars>3)
	nvars <- 3
if(nlevels>5)
	nlevels <- 5

layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
for(v in 1:nvars)
	{
	for(l in 1:nlevels)
		{
		y <- eval(parse(text=vars[v]),envir=dataset)
		data <- subset(y,by==levels[l])
		qqnorm(data,col=col[v],xlab="",ylab="",main="",sub="")
		if(line)
			qqline(data,col=col.line,xlab="",ylab="",main="",sub="")
		if(l==1)
			title(ylab=vars[v])
		if(xlab == "" && v==nvars)
			{
			if(nlevels > 1)
				title(xlab=paste(byname,": ",levels[l]))
			else
				title(xlab="")
			}
		}
	} 
par(oma=c(5,2,2,2))
if(!xlab == "")
title(main=title,sub=subtitle,xlab=xlab,ylab=ylab,outer=TRUE)
else
title(main=title,sub=subtitle,ylab=ylab,outer=TRUE)
}

# category plots

bar.plot <- function(dataset,...,by=NULL, horiz = FALSE,col="auto",lty="auto",lwd=1,hline=NULL,col.hline="auto",title="",subtitle="",xlab="",ylab="",legend=TRUE)
{
if(EPIR_GUI)
    setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
  dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
nvars <- length(vars)
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
levels <- levels(by)
nvars <- length(vars)
nlevels <- length(levels)
if(nvars>4)
	nvars <- 4
if(nlevels > 5)
	nlevels <- 5
layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
for(i in 1:nvars)
	{
	x <- as.factor(eval(parse(text = vars[i]), envir = dataset))
	xlevels <- levels(x)
	xnlevels <- nlevels(x)
	if(col[1]=="auto")
		col <- terrain.colors(nvars)
	else if(length(col)<nvars)
		col <- terrain.colors(nvars)
	lty <- ifelse(lty[1]=="auto","solid",lty)
	if(length(hline)<nvars)
		hline <- rep(hline[1],nvars)
	if(legend)
		leg <- xlevels
	else
		leg <- NULL
	heigths <- NULL
	range_y <- range(table(x,by))
	range_y <- c(0,1.1*range_y[2])
	for(k in 1:nlevels)
		{
		heigth <- NULL
		names_argument <- NULL
		for(j in xlevels)
			{
			subset <- subset(x, by == levels[k], na.rm = TRUE)
			heigth <- c(heigth, length(subset(subset, subset == j, na.rm = TRUE)))
			names_argument <- c(names_argument,j)
			}
		if(!horiz)
			if(i == nvars)
				{
				if(!is.null(byname))
					barplot(heigth,col=col[i],xlab=paste(byname, ": ",levels[k], sep = ""),names.arg=names_argument,ylab="", horiz = horiz,ylim=range_y)
				else
					barplot(heigth,col=col[i],names.arg=names_argument,xlab=levels[k],ylab="", horiz = horiz,ylim=range_y)
				}
			else
				barplot(heigth,col=col[i],names.arg=names_argument,xlab="",ylab="", horiz = horiz,ylim=range_y)
		if(horiz)
			if(!is.null(byname))
				barplot(heigth,col=col[i],xlab="",ylab=paste(byname, ": ", levels[k],sep = ""),names.arg=names_argument, horiz = horiz,xlim=range_y)
			else
				barplot(heigth,col=col[i],xlab="",ylab=levels[k],names.arg=names_argument, horiz = horiz,xlim=range_y)
		if(col.hline == "auto")
			col.hline = "black"
		if(!is.null(hline))
			abline(h=hline[i],col=col.hline,lwd=lwd, lty = lty)
		if(!horiz & k == 1)
			title(ylab = vars[i])
		else
			if(horiz & k == nlevels)        
				title(xlab = vars[i])      
		}
if(legend)
	legend("topright", leg, fill = col[i])
}
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab=xlab,ylab=ylab,outer=TRUE)
}


sector.plot <- function(dataset,...,by=NULL,col="auto",palette="terrain",title="",subtitle="",ylab="",xlab="",labels=NULL)
{
if(EPIR_GUI)
  setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
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
levels <- levels(by)
nlevels <- length(levels)
nvars <- length(vars)

if ((is.list(col)) && (length(col)!=nvars))
	col <- "auto"
#layout
if(nvars>5)
	nvars <- 5
if(nlevels>5)
	nlevels <- 5
layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
for(v in 1:nvars)
	{
	y <- as.factor(eval(parse(text=vars[v]),envir=dataset))
	if(nvars==1 && !is.null(labels))
		if(nlevels(y)==length(labels))
			y <- factor(y,labels=labels)
	ncolors <- nlevels(y)
	if (is.list(col))
		run.col <- col[[v]]
	else
		run.col <- col
	if(run.col == "auto" || length(run.col) < length(y))
		run.col <- genColors(ncolors, palette)		
	for(l in 1:nlevels)
		{	
		data <- subset(y,by==levels[l])
		if(xlab == "" && v == nvars)
			{
			labs <- ifelse(nlevels>1, paste(byname,":",levels[l]),"")
			pie(table(data),col=run.col,xlab=labs)
			}
		else		
			pie(table(data),col=run.col)
		
	if(l==1)
		title(ylab=vars[v])
		}
	} 
par(oma=c(5,2,2,2))
if(!xlab == "")
	title(main=title,sub=subtitle,outer=TRUE, ylab = ylab, xlab = xlab)
else
	title(main=title,sub=subtitle,outer=TRUE, ylab = ylab)
}

dot.plot <- function(dataset,...,by=NULL,col="auto",pch="auto",title="",subtitle="",xlab="",ylab="")
{
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
nvars <- length(vars)
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
levels <- levels(by)
nlevels <- length(levels)
if(col[1] == "auto")
	col <- rep("black",nvars) else
if(length(col) < nvars)
	col <- rep(col[1],nvars)
pch <- ifelse(pch[1]=="auto",21,pch)

#layout

if(nvars>4)
	nvars <- 4
if(nlevels>4)
	nlevels <- 4
layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
for(v in 1:nvars)
	{
	for(l in 1:nlevels)
		{
		if(l==1) 
			{
			x <- subset(eval(parse(text=vars[v]),envir=dataset),by==levels[l])
			data <- as.data.frame(x)
			dotchart(data$x,pch=pch,col=col[v],xlab="",ylab="",main="",sub="")
			if(xlab == "" && v==nvars && nlevels > 1)
				title(xlab=paste(byname,": ",levels[l]))
			title(ylab=vars[v])
			}
		else 
			{
			x <- subset(eval(parse(text=vars[v]),envir=dataset),by==levels[l])
			data <- as.data.frame(x)
			dotchart(data$x,pch=pch,col=col[v],xlab="",ylab="",main="",sub="")
			if(xlab == "" && v==nvars)
				if(byname != "" && nlevels > 1)
					title(xlab=paste(byname,": ",levels[l]))
				else
					title(xlab="")
			} 
		}
	}
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab=xlab,outer=TRUE, ylab = ylab)
}

box.plot <- function(dataset,...,by=NULL, col = "auto", box.width = 1,title=NULL,subtitle=NULL, xlab = "", ylab = "" ,na.rm = NULL)
{
# genrete a boxplot
if(EPIR_GUI)
	setPlotWindow() # start plot windows if using the gui
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
nvars <- length(vars)
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
levels <- levels(by)
nlevels <- length(levels)
if(col[1]=="auto")
	col <- rep("white",nvars)
	else
		{
		if(length(col)<nvars)
		col <- rep(col[1],nvars)
		}
if(nvars>4)
	nvars <- 4
	if(nlevels>4)
		nlevels <- 4
layout(matrix(1:(nvars*nlevels),nvars,nlevels,byrow=TRUE))
for(v in 1:nvars)
	{
	range_y <- range(eval(parse(text=vars[v]),envir=dataset))
	for(l in 1:nlevels)
		{
		x <- subset(eval(parse(text=vars[v]),envir=dataset),by==levels[l])
		data <- as.data.frame(x)
		if(v == nvars)
			if(!is.null(byname))
				boxplot(data$x,xlab=paste(byname,": ",levels[l],sep=""),main="",sub="",outline=TRUE,na.rm=na.rm,col=col[v],ylim=range_y)
			else
				boxplot(data$x,xlab="",main="",sub="",outline=TRUE,na.rm=na.rm,col=col[v],ylim=range_y)
		else
			boxplot(data$x,xlab="",main="",sub="",outline=TRUE,na.rm=na.rm,col=col[v],ylim=range_y)
		if(l==1)
			title(ylab=vars[v])
		}
	}
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab=xlab,ylab=ylab,outer=TRUE)
}

mosaic.plot <- function(dataset,...,by = NULL, col="auto",title="",subtitle="",xlab="",ylab="")
# generate a mosaic plot
{
if(EPIR_GUI)
	setPlotWindow()
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
pairs <- paste(match.call(expand.dots = FALSE)$...)
npairs <- length(pairs) 
aux <- 1
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
levels <- levels(by)
nlevels <- length(levels)
if(nlevels > 3)
	nlevels <- 3
if(npairs > 3)
	npairs <-  3
if(npairs == 1 && is.null(byname)) #if there is just one variable, there isn´t a table
	{
	frame <- model.frame(as.formula(pairs),data=dataset)
	yname <- colnames(frame)[1]
	xname <- colnames(frame)[2]
	y <- eval(parse(text=paste("frame$",yname,sep="")),envir=dataset)
	colnames(frame) <- c("y","x")
	data <- subset(frame,by==levels[1])
	names.col = levels(data$y)
	names.row = levels(data$x)
	if(col[1]=="auto")
		col <- rainbow(nlevels(dataset$x)) 
	else
		if(length(col)<nlevels(dataset$x))
			col <- rainbow(nlevels(dataset$x))
	table <- table(data$x,data$y,by)
	colnames(table) <- names.col
	rownames(table) <- names.row
	mosaicplot(table, xlab = xname, ylab = yname, main = "",col = col)
	}
else
	{
	layout(matrix(1:((npairs)*nlevels),(npairs),nlevels,byrow=TRUE))
	for(v in 1:npairs)
		{
		frame <- model.frame(as.formula(pairs[v]),data=dataset)
		pairs1 <- as.factor(t(frame[1]))
		pairs2 <- as.factor(t(frame[2]))
		xname <- colnames(frame)[1]
		yname <- colnames(frame)[2]
		for(cont in 1:nlevels)
			{
			pairs1 <- as.factor(subset(pairs1, by == levels[cont]))
			pairs2 <- as.factor(subset(pairs2, by == levels[cont]))
			if(length(pairs1) == 1 && is.na(pairs1) || length(pairs2)==1 && is.na(pairs2))
				next
			if(col[1]=="auto")
				col <- rainbow(nlevels(pairs1)) 
			else
				if(length(col)<nlevels(pairs1))
					col <- rainbow(nlevels(pairs1))
			table <- table(pairs1,pairs2)
			colnames(table) <- levels(pairs2)
			rownames(table) <- levels(pairs1)	
			if(v == 1)
				{
				if(cont == 1)
					mosaicplot(table,xlab=xname,ylab=yname,col=col,main=paste(byname,": ",levels[cont],sep="")) 
				else
					mosaicplot(table,xlab=xname,ylab="",col=col,main=paste(byname,": ",levels[cont],sep="")) 
				}
			else
				{
				if(cont == 1)
					mosaicplot(table,xlab=xname,ylab=yname,col=col,main="") 
				else
					mosaicplot(table,xlab=xname,ylab="",col=col,main="")     
				}
			}
		}
	}
par(oma=c(5,2,2,2))
title(main=title,sub=subtitle,xlab=xlab,ylab=ylab,outer=TRUE)
}