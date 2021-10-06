# stats functions
brief.summary <- function(dataset,...,by=NULL)
# compute some basic statistics
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
types <- NULL
if (!is.null(by))
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
	byname <- ""
	by <- as.factor(rep("",dim(dataset)[1])	)
	}
levels <- levels(by)
results <- new.env()
	for(v in vars)
		{
		stats <- NULL
		x <- eval(parse(text=v),envir=dataset)
		x <- x[!is.na(x)]
		types <- c(types,class(x))
		for(l in levels)
			{
			by_no_na <- by[order(x)]
			by_no_na <- by_no_na[!is.na(x)]
			stats <- rbind(stats,summary(x[by_no_na==l]))
			}
		rownames(stats) <- levels
		assign(v,stats,envir=results)
		}
retval <- as.list(results)
attr(retval,"vars") <- vars
attr(retval,"types") <- types
attr(retval,"levels") <- levels
attr(retval,"by") <- byname
class(retval) <- "brief.summary"
print(retval)
}

print.brief.summary <- function(x,...)
# print method for brief.summary
{
vars <- attr(x,"vars")
types <- attr(x,"types")
levels <- attr(x,"levels")
by <- attr(x,"by")
statsnames <- c("Mín","Q1","Mediana","Média","Q3","Máx")
for(v in 1:length(vars))
	{
	if(levels!="")
		cat("Variável",toupper(vars[v]),"por",toupper(by),"com níveis:",levels,"\n")
	else
		cat("Variável",toupper(vars[v]),"\n")
	stats <- eval(parse(text=paste("x$",vars[v],sep="")),envir=x)
	if(types[v]!="factor")
		colnames(stats) <- statsnames
	print(stats)
	cat("\n")
	}
}

stem.leaf <- function(dataset,...,by=NULL,scale=1,width=80)
# implement a stem and leaf diagram
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
types <- NULL
if (!is.null(by))
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
	byname <- ""
	by <- as.factor(rep("",dim(dataset)[1])	)
	}
nvars <- length(vars)
levels <- levels(by)
nlevels <- length(levels)
for(v in 1:nvars)
	{
	if(levels!="")
		cat("Variável",toupper(vars[v]),"por",toupper(byname),"com níveis:",levels,"\n")
	else
		cat("Variável",toupper(vars[v]),"\n")
	for(l in 1:nlevels)
		{
		if(levels!="")
			cat("Nível:",levels[l],"\n")
		y <- eval(parse(text=vars[v]),envir=dataset)
		data <- subset(y,by==levels[l])
		stem(data,scale=scale,width=width)
		}
	}
}


desc.vars <- function(dataset,...,by=NULL,stats="ALL",probs=NULL,trim = 0, dec=2)
{
subset_mean <- NULL
subset_median <- NULL
subset_sdev <- NULL
subset_n <- NULL
subset_min <- NULL
subset_max <- NULL
subset_quantile <- NULL
subset_vc <- NULL
subset_na <- NULL
subset_range <- NULL
subset_var <- NULL
subset_s <- NULL
subset_k<- NULL
subset_centiles <- NULL
subset_bs <- NULL
subset_as <- NULL
subset_probs <- NULL
if(stats=="ALL")
	stats <- c("n","na","mean","sd","median","min","max","quantile","centiles","variation coefficent","var","skewness","kurtosis","range")		
if(!is.null(probs))
	if(any(probs > 1))
		probs <- probs/100
dataset <- eval(parse(text= dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
nvars <- length(vars)
stats <- tolower(stats)

if(trim > 1)
	trim <- trim/100

if (!is.null(by))
	{ 	  
		if(is.character(by))
		{
			byname <- by
			by <- as.factor(eval(parse(text= by),envir=dataset))
		}
	else
		byname <- deparse(substitute(by))
	}
else
	{
	byname <- ""
	by <- as.factor(rep("",dim(dataset)[1]))
	}	
levels <- levels(by)

for (i in 1:nvars)
{
  retval <- NULL

  for(v in levels)
  {
    subset_<- subset(eval(parse(text = paste("dataset$", vars[i], sep = ""))), by == v)
	if("n" %in% stats)
		subset_n <- length(!is.na(subset_))

    if("mean" %in% stats)
		subset_mean <- mean(subset_,trim = trim, na.rm=TRUE)

    if("sd" %in% stats)
		subset_sdev <- sd(subset_, na.rm=TRUE)

    if("median" %in% stats)
		subset_median <- median(subset_, na.rm=TRUE)

    if("min" %in% stats)
		subset_min <- min(subset_, na.rm=TRUE)
    
    if("max" %in% stats)
		subset_max <- max(subset_, na.rm=TRUE)
    
    if("quantile" %in% stats)
		subset_quantile <- quantile(subset_, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)

    if("variation coefficient" %in% stats)
		subset_vc <- sd(subset_[!is.na(subset_)]/mean(subset_,na.rm=TRUE))
    
    if("na" %in% stats)
		subset_na <- sum(is.na(subset_))
    
    if("range" %in% stats)
		subset_range <- max(subset_, na.rm=TRUE) - min(subset_, na.rm=TRUE)
    
    if("var" %in% stats)
		subset_var <- var(subset_, na.rm=TRUE)
   
    if("skewness" %in% stats)        
		subset_s <- mean((subset_[!is.na(subset_)]-mean(subset_,na.rm=TRUE))^3)/(sd(subset_, na.rm=TRUE)^3) 
          
    if("kurtosis" %in% stats)
		subset_k <- mean((subset_[!is.na(subset_)]-mean(subset_, na.rm=TRUE))^4)/(sd(subset_, na.rm=TRUE)^4)-3 
    
    if("centiles" %in% stats)
		subset_centiles <- quantile(subset_, probs = c(0.20, 0.40, 0.60, 0.80), na.rm=TRUE)
    if(!is.null(probs))
		subset_probs <- quantile(subset_, probs = probs, na.rm=TRUE)
	

	
    statistic <- c(subset_mean, subset_median, subset_sdev, subset_n, subset_min, subset_max, subset_quantile,subset_centiles, subset_vc, subset_na, subset_range, subset_var, subset_s, subset_k,subset_probs)
	retval <- rbind(retval, statistic)
	retval <- round(retval,digits=dec)
   }
   attr(retval, "stats") <- stats
   attr(retval, "namesrow") <- levels(by)
   attr(retval, "variables") <- vars[i]
   attr(retval, "byname") <- byname
   attr(retval, "probs") <- probs
   attr(retval, "trim") <- trim
   class(retval) <- "desc.vars"
   print(retval)
}
}

print.desc.vars <- function(x,...)
{
stats <- attr(x, "stats")
variables <- attr(x, "variables")
byname <- attr(x, "byname")
namerow <- attr(x, "namesrow")
names <- NULL
trim <- attr(x, "trim")
attr(x, "nrow") <- NULL
attr(x, "variables") <- NULL
attr(x, "byname") <- NULL
attr(x, "namesrow") <- NULL
attr(x,"trim") <- NULL

if("mean" %in% stats)
	names <- "Media"
if("median" %in% stats)
	names <- c(names, "Mediana")
if("sd" %in% stats)
	names <- c(names, "D-Padrao")
if("n" %in% stats)
	names <- c(names,"n")
if("min" %in% stats)
	names <- c(names,"Min")
if("max" %in% stats)
	names <- c(names,"Max")
if("quantile" %in% stats)
	names <- c(names, c("Q1", "Q2", "Q3"))
if("centiles" %in% stats)
	names <- c(names, c("P20", "P40", "P60","P80"))
if("variation coefficient" %in% stats)
	names <- c(names, "Coef. de Var.")
if("na" %in% stats)
	names <- c(names, "NA")
if("range" %in% stats)
	names <- c(names, "Amplitude")    
if("var" %in% stats)
	names <- c(names, "Var")
if("skewness" %in% stats)
	names <- c(names, "Assimetria")
if("kurtosis" %in% stats)
	names <- c(names, "Curtose")
if(!is.null(attr(x,"probs")))
{
	probs <- attr(x, "probs")
	attr(x, "probs") <- NULL
	names <- c(names, paste("P",100*probs, sep=""))
}

attr(x, "stats") <- NULL
ncol <- dim(x)[2]
result <- NULL
result <- cbind(result,x)

colnames(result) <- names
if(byname == "")
rownames(result) <- ""
else
rownames(result) <- namerow
cat("Variável:", "", variables, "\n")
if(!byname == "")
		cat("Estratos:", "", byname,"\n","\n")

print(result)
cat("\n","\n")
if(trim != 0)
	if(trim >= 0.5)
		cat("O valor encontrado para a média é igual ao valor da mediana uma vez que todas as observações foram retiradas")
	else
		cat(paste(round(2*100*trim,digits=4),"% dos valores foram excluídos para o cálculo da média", sep = "")) 
cat("\n")
}


frequency.tables <- function(dataset,...,by = NULL, na.rm = FALSE)
{
dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
nvars <- length(vars)
if (!is.null(by))
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
	byname <- ""
	by <- as.factor(rep("",dim(dataset)[1]))
	}
levels <- levels(by)
nlevels <- nlevels(by)
for(l in 1:nvars)
	{
	for(j in 1:nlevels)
		{
		subset_vars <- subset(eval(parse(text = paste("dataset$", vars[l], sep = ""))), by == levels[j])	
		na = sum(is.na(subset_vars))
		na.percent = 100*na/(length(subset_vars))
		if(na.rm)
			subset_vars <- subset_vars[!is.na(subset_vars)]		
		n <- as.vector(table(subset_vars))
		f <- n/length(subset_vars)
		F_dist <- cumsum(f)
		N <- cumsum(n)							
		F_dist <- round(F_dist, 4)
		f <- round(f, 4)		
		retval <- as.data.frame(cbind(n, N, f, F_dist))
		retval[,3] <- retval[,3]*100
		retval[,4] <- retval[,4]*100
		attr(retval, "rnames") <- rownames(table(subset_vars))
		attr(retval, "vars") <- vars[l]
		if(byname!="")
			{
			attr(retval, "bynames") <- byname
			attr(retval,"levels") <- levels[j]
			}
		else
			attr(retval, "bynames") <- FALSE
		attr(retval,"nlevels") <- j
		attr(retval,"NA") <- c(na,na.percent)
    		class(retval) <- "frequency.tables"
		print(retval)
		}
	}
}

print.frequency.tables <- function(x,...)
{
vars <- attr(x, "vars")
byname <- attr(x, "bynames")
levels <- attr(x, "rnames")
attr(x, "rnames") = NULL
attr(x, "vars") = NULL
attr(x, "bynames") = NULL
retval <- NULL
for (i in 1:length(x))
	retval <- as.data.frame(cbind(retval, x[[i]]))
if(attr(x,"nlevels")==1)
	{
	cat("\n","\n","\n")
	cat("\t","\t","Tabela de Freqüências","\n")
	}
colnames(retval) <- c("Freq", "Freq Acumulada", "Percent", "Perc Acumulado")
rownames(retval) <- levels
cat("\n")
if(class(byname)!="logical")
	{
	cat("Variável:", vars,"estratificado por", toupper(byname))
	cat("\n")
	cat("Estrato: ", attr(x,"levels"), "\n")
	}
else
	cat("Variável: ", vars)
cat("\n")
print(retval, digits=EPIR_OPTION_DIGITS)
cat("\n","Número de observações faltantes:",attr(x,"NA")[1],"(",round(attr(x,"NA")[2],EPIR_OPTION_DIGITS),"%)","\n")
}


cross.frequency <- function(dataset,...,by=NULL,frequency=TRUE,paste_margin=FALSE,calc_margin="total",print_margin="none")
{
datasetname <- dataset
dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
vars <- paste(match.call(expand.dots = FALSE)$...)
nvars<- length(vars)
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
	byname <- ""
	by <- as.factor(rep("",dim(dataset)[1]))
}
levels <- levels(by)
nlevels <- nlevels(by)
for(j in 1:nlevels)
	{
	x <- eval(parse(text = vars[1]), envir = dataset)
	x <- as.factor(subset(x, by == levels[j]))
	xlevels <- levels(x)
	y <- eval(parse(text = vars[2]), envir = dataset)
	y <- as.factor(subset(y, by == levels[j]))
	ylevels <- levels(y)
	tables <- table(x,y,dnn=c(vars[1],vars[2]))				
	
	if(!frequency)
		{
			if(calc_margin == "row")
				tables <- prop.table(tables,1)
			else if(calc_margin == "col")
				tables <- prop.table(tables,2)
			else
				tables <- tables/sum(tables)				
		}	
		
	if(paste_margin && print_margin != "none")
		if(print_margin == "total")
			{
			line <- margin.table(tables,2)
			line <- c(line, sum(line))
			col <- margin.table(tables,1)
			tables <- cbind(tables, col)
			tables <- rbind(tables, line) #table with the sum by line and column						
			}
		else
			if(print_margin == "row")
					{
					col <- margin.table(tables,1)
					tables <- cbind(tables, col)
					}
				else
					{
					line <- margin.table(tables,2)
					tables <- rbind(tables, line)
					}
	attr(tables, "colnames") <-  ylevels
	attr(tables, "rownames") <- xlevels
	attr(tables, "vars") <- c(vars[1], vars[2])
	if(paste_margin)
		attr(tables, "print_margin") <- print_margin
	attr(tables, "by") <- c(byname,levels[j])
	class(tables) <- "cross.frequency"
	print(tables)	
	}
}


print.cross.frequency <- function(x, ...)
{
vars <- attr(x, "vars")
xlevels <- attr(x, "rownames")
levels <- attr(x, "colnames")
print_margin <- attr(x, "print_margin")
byname <- attr(x, "by")
attr(x, "vars") <- NULL
attr(x, "rownames") <- NULL
attr(x, "colnames") <- NULL
attr(x, "print_margin") <- NULL
attr(x, "by") <- NULL
retval <- as.table(x)

if(!is.null(print_margin)&& print_margin != "none")
	{
	if(print_margin == "total")
		{		
		levels <- c(levels, "Total")
		xlevels <- c(xlevels, "Total")
		colnames(retval) <- levels
		rownames(retval) <- xlevels
		}
	else
		if(print_margin == "row")
			{		
			colnames(retval) <- c(levels, "Total")	
			}
		else
			{			
			rownames(retval) <- c(xlevels, "Total")
			}
	}
cat("\t \t Tabela de Contingência")
cat("\n", "\n","\n")
if(byname[1] != "")
	{	
	cat("Estratificação por", byname[1],"\n")
	cat("Estrato: ", byname[2], "\n","\n")
	}

print(retval, digits=EPIR_OPTION_DIGITS)
cat("\n","\n")	
}

correl <- function(dataset,...,by=NULL,correlation=TRUE,method="pearson",use ="everything")
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)	
vars <- paste(match.call(expand.dots = FALSE)$...)
if (!is.null(by))
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
	byname <- NULL
	by <- as.factor(rep("",dim(dataset)[1]))
	}
output <- NULL
nvars <- length(vars)
levels <- levels(by)
for(l in 1:nlevels(by))
    {
    x <- NULL
    data <-NULL
    y <- subset(dataset,by==levels(by)[l])
    for(i in 1:nvars)
        {
        x[i] <- data.frame(y[names(y)==vars[i]])
        data <- data.frame(c(data,x[i])) 
        }
    names(data) <- vars
    if(correlation)
            {
            y <- round(cor(data,method=method,use=use),EPIR_OPTION_DIGITS)
            if(l==1)
                {
                cat("\n")
                if(length(levels)!=1)
                    cat("\t","Matriz correlação pelo método de",method," e estratificado por",byname,"\n","\n")
                 else
                    cat("\t","Matriz correlação pelo método de",method,"\n","\n")
                 }
             if(length(levels)!=1)
                    cat("Estrato =",levels[l],"\n") 
             print(y)
             cat("\n")
             }
    else
        {
        y <- round(cov(data,use=use),EPIR_OPTION_DIGITS)
        if(l==1)
            {
            cat("\n")
            if(length(levels)!=1)
                cat("\t","Matriz de covariância pelo método de",method," e estratificado por",byname,"\n","\n")
            else
                 cat("\t","Matriz de covariância pelo método de",method,"\n","\n")
             }
          if(length(levels)!=1)
              cat("Estrato =",levels[l],"\n") 
          print(y)
          cat("\n")
         }
    }
}

correl <- function(dataset,...,by=NULL,correlation=TRUE,method="pearson",use ="everything")
{
if(is.character(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)	
vars <- paste(match.call(expand.dots = FALSE)$...)


if (!is.null(by))
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
	byname <- NULL
	by <- as.factor(rep("",dim(dataset)[1]))
	}
output <- NULL
nvars <- length(vars)
levels <- levels(by)
for(l in 1:nlevels(by))
    {
    x <- NULL
    data <-NULL
    y <- subset(dataset,by==levels(by)[l])
    for(i in 1:nvars)
        {
        x[i] <- data.frame(y[names(y)==vars[i]])
        data <- data.frame(c(data,x[i])) 
        }
    names(data) <- vars
    if(correlation)
            {
            y <- round(cor(data,method=method,use=use),EPIR_OPTION_DIGITS)
            if(l==1)
                {
                cat("\n")
                if(length(levels)!=1)
                    cat("\t","Matriz correlação pelo método de",method," e estratificado por",byname,"\n","\n")
                 else
                    cat("\t","Matriz correlação pelo método de",method,"\n","\n")
                 }
             if(length(levels)!=1)
                    cat("Estrato =",levels[l],"\n") 
             print(y)
             cat("\n")
             }
    else
        {
        y <- round(cov(data,use=use),EPIR_OPTION_DIGITS)
        if(l==1)
            {
            cat("\n")
            if(length(levels)!=1)
                cat("\t","Matriz de covariância pelo método de",method," e estratificado por",byname,"\n","\n")
            else
                 cat("\t","Matriz de covariância pelo método de",method,"\n","\n")
             }
          if(length(levels)!=1)
              cat("Estrato =",levels[l],"\n") 
          print(y)
          cat("\n")
         }
    }
}