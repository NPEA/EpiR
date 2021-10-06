# data management tools

read.tabwin <- function(file,row.names=FALSE,check.names=TRUE) 
{
# improvement based on the function distributed with Tabwin
a <- readLines(file)
n <- 0
# reading headers
for (i in 1:length(a))
	{
	if (substring(a[i],1,8) == "Titulo1=")
		title <- substring(a[i],9,100)
	else 
		title <- ""
	if (substring(a[i],1,8) == "Titulo2=")
		subtitle <- substring(a[i],9,100)
	else 
		subtitle <- ""
	if (substring(a[i],1,7) == "Rodape=")
		footer <- substring(a[i],8,100)
	else 
		footer <- ""
	if (substring(a[i],1,9) == "Nomemapa=")
		mapname <- substring(a[i],10,100)
	else 
		mapname <- ""
	if (substring(a[i],1,1) != "\"" )
		n <- n+1
	else
		break
	}
# reading data
if (row.names)
	data <- read.table(file,sep=";",dec=",",skip=n,header=TRUE,row.names=1,check.names=check.names)
else
	data <- read.table(file,sep=";",dec=",",skip=n,header=TRUE,row.names=NULL,check.names=check.names)

# defining attributes
attr(data,"title") <- title
attr(data,"subtitle") <- subtitle
attr(data,"footer") <- footer
attr(data,"mapname") <- mapname

return(data)
}


import.data <- function(file,object=NULL,sep=NULL,dec=NULL,envir=.GlobalEnv,...)
# import dataset
{
ext <- getExtension(file) 
if(toupper(ext)=="DTA")
	try(dataobj <- read.dta(file))
else if(toupper(ext)=="SAV")
	{
	if(!is.windows())
		test <- try(dataobj <- as.data.frame(read.spss(file,reencode="latin1")))
	else
		try(dataobj <- read.spss(file,to.data.frame=TRUE))
	}
else if(toupper(ext)=="REC")
	try(dataobj <- read.epiinfo(file))
else if(toupper(ext)=="DBF")
	try(dataobj <- read.dbf(file))
else if(toupper(ext)=="TAB")
	try(dataobj <- read.tabwin(file))
# else if(toupper(ext)=="XLS")
# 	{
# 	if(is.windows())
# 		{
# 		require("xlsReadWrite",quietly=TRUE,warn.conflicts=FALSE)
# 		try(dataobj <- read.xls(file,colNames=TRUE,...))
# 		} 
# 	else
# 		error("xlsfilter")
# 	}
else if(toupper(ext)=="RDA")
	dataobj <- try(load(file,envir=.GlobalEnv))
else
	{
	sep <- switch(sep,
		tab="\t",
		space=" ",
		sep)
	dataobj <- read.table(file,header=TRUE,sep=sep,dec=dec,fill=TRUE,...)
	}
if(!is.null(object))
	assign(object,dataobj,envir=envir)
}


save.data <- function(...,file)
# save r binary files
{
try(save(...,file=file))
}


load.data <- function(file)
# load r binary files
{
try(load(file,envir=.GlobalEnv))
}


clear.all <- function()
# clear the workspace
{
list <- ls(all=TRUE,envir=.GlobalEnv)
ilegal <- c(".EpiREnv",".EpiRTrash")
ilegal.pos <- match(ilegal,list)
ok <- list[-ilegal.pos]
for (i in ok)
	{
	tmp <- get(i,envir=.GlobalEnv)
	assign(i,tmp,envir=.EpiRTrash)
	rm(list=i,envir=.GlobalEnv)
	}
}

remove.data <- function(...)
# remove objects
{
objects <- paste(match.call(expand.dots = FALSE)$...)
for (i in objects)
	{
	if(exists(i,envir=.GlobalEnv))
		{
		tmp <- get(i,envir=.GlobalEnv)
		assign(i,tmp,envir=.EpiRTrash)
		rm(list=i,envir=.GlobalEnv)
		}
	}
}


erase.trash <- function(confirm=FALSE)
# erase the trash can
{
if(confirm)
	{
	list <- ls(all=TRUE,envir=.EpiRTrash)
	rm(list=list,envir=.EpiRTrash)
	}
}


restore.deleted <- function(...)
# restore deleted objects
{
objects <- paste(match.call(expand.dots = FALSE)$...)
for (i in objects)
	{
	if(exists(i,envir=.EpiRTrash))
		{
		tmp <- get(i,envir=.EpiRTrash)
		assign(i,tmp,envir=.GlobalEnv)
		rm(list=i,envir=.EpiRTrash)
		}
	}
}


list.data <- function()
# list data in the workspace
{
ls(all=FALSE,envir=.GlobalEnv)
}


drop.columns <- function(dataset,...,object=dataset,all.but=FALSE)
# drop columns listed in ...
{
dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
columns <- paste(match.call(expand.dots = FALSE)$...)
allcolumns <- colnames(dataset)
col.idx <- match(columns,allcolumns)
col.idx.c <- match(setdiff(allcolumns,columns),allcolumns)
if (sum(!is.na(col.idx))>0)
	{
	if(all.but)
		kept <- dataset[-col.idx.c]
	else
		kept <- dataset[-col.idx]
	}
assign(object,kept,envir=.GlobalEnv)
}


select.rows <- function(dataset,query,object=dataset)
# drop rows accordingly to filters in ...
{
dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
selected <- subset(dataset,eval(parse(text=query),envir=dataset))
assign(object,selected,envir=.GlobalEnv)
}


transform.data <- function(dataset,...,object=dataset)
# create new variables in the dataset
{
newdata <- eval(parse(text=dataset),envir=.GlobalEnv)
operations <- paste(match.call(expand.dots = FALSE)$...)
with(newdata,{
	for(i in operations)
		eval(parse(text=paste("newdata$",i,sep="")))
	assign(object,newdata,envir=.GlobalEnv)})
}


start.odbc <- function(dsn,uid,pwd,case="nochange",conn="ODBCconn")
# connect to an ODBC data source
{
channel <- try(odbcConnect(dsn,uid=uid,pwd=pwd,case=case),silent=TRUE)
assign(conn,channel,envir=.GlobalEnv)
}

end.odbc <- function(conn="ODBCconn")
# close an active odbc connection
{
channel <- try(get(conn,envir=.GlobalEnv),silent=TRUE)
if(inherits(channel,"RODBC"))
	{
	try(odbcClose(channel),silent=TRUE)
	try(rm(channel,envir=.GlobalEnv),silent=TRUE)
	}
}

query.odbc <- function(conn="ODBCconn",sql,object)
# submit a SQL query to the server through an ODBC connection
{
channel <- try(get(conn,envir=.GlobalEnv),silent=TRUE)
if(inherits(channel,"RODBC"))
	{
	result <- try(sqlQuery(channel,sql,error=FALSE),silent=TRUE)
	assign(object,result,envir=.GlobalEnv)
	}
}

row.aggregate <- function(dataset,x=NULL,groups,FUN,object=NULL,filter=NULL,...)
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
else
	xname = NULL
if (!is.null(groups))
	{
	byname <- groups
	if(length(groups)==1)
		by <- list(as.factor(eval(parse(text=groups),envir=dataset)))
	else
		{
		by <- NULL	
		for(i in 1:length(groups))
			by[i] <- list(as.factor(eval(parse(text=groups[i]),envir=dataset))) 
		}
	}
else
	{
	byname <- ""
	by <- as.factor(rep("",dim(dataset)[1]))
	}
if(FUN!="count")
	{	
	if(is.null(x))
		data <- suppressWarnings(aggregate(x=data.frame(dataset),by=by,FUN=FUN))
	else
		data <- suppressWarnings(aggregate(data.frame(x),by=by,FUN=FUN))
	}
else
	data <- suppressWarnings(aggregate(x=data.frame(rep(1,dim(dataset)[1])),by=by,FUN="sum"))
retval <- list(data=data,byname=byname,xname=xname,FUN=FUN,object=object)
class(retval) <- "row.aggregate"
return(retval)
}


print.row.aggregate <- function(x,digits=EPIR_OPTION_DIGITS,...)
{
data.aggregate = x$data
if(x$FUN != "count")
	colnames(data.aggregate) <- c(x$byname,x$xname)
else
	colnames(data.aggregate) <- c(x$byname,"n")
if(!is.null(x$object))
    assign(x$object,data.aggregate,envir=.GlobalEnv)
else
	 assign("var_agregada",data.aggregate,envir=.GlobalEnv)
cat("\n")
print(data.aggregate,digits)
}

 merge.dataset <- function(dataset.x,dataset.y,by.row=T,by.x=NULL,by.y=NULL,selection.x=NULL,selection.y=NULL,exeption.x=F,exeption.y=F,object=NULL,...)
{
dataset <- list(dataset.x,dataset.y)
by <- list(by.x,by.y)
if(by.row)
 selection.y <- selection.x
selection <- list(selection.x,selection.y)
exeption <- list(exeption.x,exeption.y)
dataname <- dataset[[1]]
for(i in 1:2)
 {
 dataset[[i]] <- eval(parse(text=dataset[[i]]),envir=.GlobalEnv)
 if(exeption[[i]]==FALSE)
  {
  if(!is.null(by[[i]]))
   if(any(selection[[i]]!= by[[i]]))
    selection[[i]] <- c(selection[[i]],by[[i]])
  dataset[[i]] <- dataset[[i]][names(dataset[[i]]) %in% selection[[i]]]
  }
  else
   dataset[[i]] <- dataset[[i]][!names(dataset[[i]]) %in% selection[[i]]]
  if(i==2)
   {
   if(by.row)
    data <- rbind.data.frame(as.data.frame(dataset[[1]]),as.data.frame(dataset[[2]]))
   else 
    data <- as.data.frame(merge(as.data.frame(dataset[[1]]),as.data.frame(dataset[[2]]),by.x=by[[1]],by.y=by[[2]],all=TRUE)) 
   }
 }
if(is.null(object))
 assign(paste(dataname,"_Agrupado",sep=""),data,envir=.GlobalEnv)
else
 assign(object,data,envir=.GlobalEnv)
}