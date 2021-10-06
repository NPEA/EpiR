recodeVar <- function(dataset,x,numeric=TRUE,options="values",breaks=NULL,probs=NULL,rigth=FALSE
							,ordered_result=FALSE,ref=NULL,labels=NULL,object=NULL,return=FALSE,as.numeric=FALSE,na.level,...)
{
if(is.character(dataset))
    {
    dataname <- dataset 
    dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
    }
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
if(as.numeric)
    x <- as.numeric(as.character(x))
else
    {
    if(!is.null(labels))
        labels <- as.character(labels)
    if(numeric)
        {
        if(options=="values" && !is.null(breaks))
	   {
	   if(min(x,na.rm=TRUE) < min(breaks,na.rm=TRUE))
	       breaks <- unique(c(min(x,na.rm=TRUE),breaks, max(x,na.rm=TRUE)))
	   else
	       breaks <- unique(c(breaks, max(x,na.rm=TRUE)))
	   }
	   if(is.null(breaks))
	       {
	       if(options=="values")
	           breaks <- unique(sort(c(min(x,na.rm=TRUE):max(x,na.rm=TRUE),max(x,na.rm=TRUE))))
	       else if(options=="centiles") #quintil
	           breaks <- unique(sort(c(data.frame(quantile(x, probs = c(0.20, 0.40, 0.60, 0.80), na.rm=TRUE))[1:4,],min(x,na.rm=TRUE),max(x,na.rm=TRUE))))
	       else if(options=="quantile")
	           breaks <- unique(sort(c(data.frame(quantile(x, probs = c(0.25, 0.5, 0.75), na.rm=TRUE))[1:3,],min(x,na.rm=TRUE),max(x,na.rm=TRUE))))
	       else if(options=="median")
	           breaks <- unique(sort(c(data.frame(quantile(x, probs = 0.5, na.rm=TRUE))[1,],min(x,na.rm=TRUE),max(x,na.rm=TRUE))))
	       else
	           if(!is.null(probs))
		      breaks <- quantile(x, probs = probs, na.rm=TRUE)
	           else
		      breaks <- unique(sort(c(data.frame(quantile(x, probs = 0.5, na.rm=TRUE))[1,],min(x,na.rm=TRUE),max(x,na.rm=TRUE))))
	           }
        if(options == "percentiles")
	   if(min(x) < min(breaks))
	       breaks <- unique(c(min(x,na.rm=TRUE),breaks, max(x,na.rm=TRUE)))
	    else
	       breaks <- unique(c(breaks, max(x,na.rm=TRUE)))
        if(!is.null(ref))
	   var <- relevel(cut(x, breaks=breaks,rigth=rigth,include.lowest=TRUE,labels=labels),ref=ref)
        else
	   var <- cut(x, breaks=breaks,rigth=rigth,ordered_result=ordered_result,labels=labels,include.lowest=TRUE) 
    }
    else
        {
        if(!numeric)
	   x <- as.factor(x)
        var <- recordFactor(x,levels=NULL,new.levels=labels,ref=ref)
        }
    }
if(na.level)
    {
    var <- as.character(var)
    for(i in 1:length(var))
        if(is.na(var[i]))
	       var[i] <- "NA"
    var <- as.factor(as.character(var))
    }
if(!return) 
    {
    if(as.numeric)
        {
        if(is.null(object))
            dataset[which(names(dataset)==xname)] <- x  
        else
            {
            dataset <- data.frame(dataset,x)
            names(dataset)[dim(dataset)[2]] <- object
            } 
        }        
    else
       { 
        dataset <- data.frame(dataset,as.factor(var))
        if(!is.null(object))
	   names(dataset)[dim(dataset)[2]] <- object
        else
            {
            #dataset[xname] <- var
	    names(dataset)[dim(dataset)[2]] <- paste(xname,"_CAT")
            }
        }
    assign(dataname,dataset,envir=.GlobalEnv)
    }
else
    return(var)
}

	# uso quando x é um factor

recordFactor <- function(x,levels=NULL,new.levels,ref=NULL)
{
if(!is.null(levels) && nlevels(x)==length(levels))
	nlevels <- length(levels)
else 
	{
	nlevels <- nlevels(x)
	levels <- levels(x)
	}
x <- as.character(x)
for(i in 1:nlevels)
	x[x==levels[i]] <- new.levels[i]
if(!is.null(ref) && any(new.levels==ref))
	x <- relevel(as.factor(x),ref=ref)
else
	x <- as.factor(x)
class(var)
return(x)
}