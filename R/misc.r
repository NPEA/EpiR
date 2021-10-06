#some operators

"%notin%" <- function(x,y) !(x %in% y)

logit <- function(x)
# compute the logit transformation
return(log(x)/(1-log(x)))

alogit <- function(x)
# compute the anti-logit transformation
return(exp(x)/(1+exp(x)))

identity <- function(x) x

inverse <- function(x) 1/x

sqr <- function(x) x^2


save.output <- function(filename)
# save the output content to file
{
conn <- file(filename,"w")
txtOutput <- getWidget("Main","txtOutput")
text <- getTv(txtOutput)
writeLines(text,conn)
close(conn)
}


load.output <- function(filename)
# save the output content to file
{
conn <- file(filename,"r")
text <- readLines(conn)
close(conn)
txtOutput <- getWidget("Main","txtOutput")
clearTv(txtOutput)
appendTv(txtOutput,text)
}


format.date.core <- function(datevar,format,origin=NULL)
# format dates
{
datevar <- as.character(datevar)
formated.date <- as.Date(datevar,format=format,origin=origin)
return(formated.date)
}

fixFormat <- function(x)
{
if(tolower(x) == "aa")
	x <- "%y"
else if(tolower(x) == "aaaa")
	x <- "%Y"
else if(tolower(x) == "dd")
	x <- "%d"
else
	x <- "%m"	
}

format.date <- function (dataset, datevar, varname = NULL, format, origin = NULL) 
{
datasets.name <- dataset
if (is.character(dataset)) 
  dataset <- eval(parse(text = dataset), envir = .GlobalEnv)
if (is.null(varname) || varname == "") 
  varname <- "Data"
datevar <- eval(parse(text = paste("dataset$", datevar, sep = "")))
if(nchar(as.character(datevar[1]))!= 10)
format <- "%d%m%Y"
new.date <- as.Date(datevar, format = format, origin = origin)
names <- names(dataset)
if(!is.null(varname))
  { 
	dataset[varname] <- new.date
	new.dataset <- dataset
	}
else
	{
  new.dataset <- cbind.data.frame(dataset, new.date)
  colnames(new.dataset) <- c(names, varname)
	}
  assign(datasets.name, new.dataset, envir = .GlobalEnv)
}

change.dir <- function(dir)
# change the working directory (it must extend setwd in order to be justifiable)
{
try(setwd(dir),silent=TRUE)
}

