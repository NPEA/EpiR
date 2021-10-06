# utility functions

setWindow <- function(window,parent="Main",module=NULL)
# setup a glade window
{
if(is.null(module))
	file <- paste(get("EPIR_GUI_DIR",envir=.EpiREnv),"/",window,".glade",sep="")
else
	file <- paste(get("EPIR_GUI_DIR",envir=.EpiREnv),"/",tolower(module),"_",window,".glade",sep="")

xmlwin <- gtkBuilderNew()
gtkBuilderAddFromFile(xmlwin,file)
assign(window,xmlwin,envir=.EpiREnv)
gtkBuilderConnectSignals(eval(parse(text=window),envir=.EpiREnv))

if((window!=parent) & (is.null(module)))
	{
	# make the window modal
	window <- getWidget(window)
	window$setTransientFor(getWidget(parent)) 
	window$setModal(TRUE)
	window$setDestroyWithParent(TRUE)
	}
}

getWidget <- function(window,widget=window)
# return the pointer to a control in the window
{
get(window,envir=.EpiREnv)
return(gtkBuilderGetObject(eval(parse(text=window),envir=.EpiREnv),widget))
}
 
 
closeWindow <- function(long,short=NULL)
# close a gtk window
{
if(!is.null(short))
	{
	if(!getWidget(long,paste("chk_",short,"_kwo",sep=""))$getActive())
		{
		win <- getWidget(long)
		gtkWidgetDestroy(win)
		if(exists(".EpiREnv"))
			if(exists(long,envir=.EpiREnv))
				rm(list=long,envir=.EpiREnv)
		}
	}
else
	{
	win <- getWidget(long)
	gtkWidgetDestroy(win)
	if(exists(".EpiREnv"))
		if(exists(long,envir=.EpiREnv))
			rm(list=long,envir=.EpiREnv)
	}
}


is.windows <- function()
# check if the OS is MS Windows
{
return(ifelse(tolower(EPIR_OS)=="windows",TRUE,FALSE))
}


getDataset <- function()
# check if the objects are data.frame
{
objects <- ls(envir=.GlobalEnv)
data <- NULL
for(i in objects)
	if(is.data.frame(eval(parse(text=i),envir=.GlobalEnv)))
		data <- c(data,i)
return(data)
}


getNumeric <- function(dataset)
# check if the objects are numeric
{
columns <- names(get(dataset,envir=.GlobalEnv))
data <- NULL
for(i in columns)
	if(class(eval(parse(text=paste(dataset,"$",i,sep="")),envir=.GlobalEnv)) %in% c("complex","integer","double","numeric"))
		data <- c(data,i)
return(data)
}

getDate <- function(dataset)
# check if the objects are Date
{
columns <- names(get(dataset,envir=.GlobalEnv))
data <- NULL
for(i in columns)
	if(class(eval(parse(text=paste(dataset,"$",i,sep="")),envir=.GlobalEnv)) == "Date")
		data <- c(data,i)
return(data)
}

getFactor <- function(dataset)
# check if the objects are factors
{
columns <- names(eval(parse(text=dataset),envir=.GlobalEnv))
data <- NULL
for(i in columns)
	if(class(eval(parse(text=paste(dataset,"$",i,sep="")),envir=.GlobalEnv)) %in% c("character","logical","factor"))
		data <- c(data,i)
return(data)
}


getNames <- function(dataset)
# get columns names
{
columns <- names(eval(parse(text=dataset),envir=.GlobalEnv))
return(columns)
}

getBy <- function(dataset)
{
columns <- names(get(dataset,envir=.GlobalEnv))
data <- NULL
for(i in columns)
	if(length(unique(eval(parse(text=paste(dataset,"$",i,sep="")),envir=.GlobalEnv))) <= 10)
		data <- c(data,i)
return(data)
}

showHelp <- function(topic,module=NULL)
# show the help file for the given issue
{
if(is.null(module))
	url <- paste("file://",EPIR_PATH_DOC,"/",topic,".html",sep="")
else
	url <- paste("file://",EPIR_PATH_DOC,"/",module,"_",topic,".html",sep="")
browseURL(url)
}

fillListView <- function(tv,data,update=TRUE,headers=NULL,sel.mode="multiple")	
# fill up a tree view widget
{
if(update)
	{
	cols <- tv$getColumns()
	for (c in cols)
		tv$removeColumn(c)
	tv$getModel()$clear()
	}
if(is.list(data))
	{
	lst <- gtkListStoreNew(rep("character",length(data)))
	ncols <- length(data)
	nrows <- length(data[[1]])
	for(i in 1:nrows)
		{
		iter <- lst$append()$iter
		for(j in 1:ncols)
			lst$setValue(iter,j-1,data[[j]][i])
		}
	}
else
	{
	ncols <- 1
	lst <- gtkListStoreNew("character")
	for(i in data)
		{
		iter <- lst$append()$iter
		lst$set(iter,0,i)
		}
	}
if(!is.null(headers))
	headers <- fixTVHeader(headers)
for(j in 1:ncols)
	{
	tv$setModel(lst)
	renderer <- gtkCellRendererTextNew()
	renderer$set(xalign=0.0)
	col <- tv$insertColumnWithAttributes(j-1,headers[j],renderer,text=j-1)
	}
#setting some properties
if(!update)
	tv$getSelection()$setMode(sel.mode)
}


fillColListView <- function(tv,data,color,update=TRUE,headers=NULL,sel.mode="multiple")	
# fill up a tree view widget
{
if(update)
	{
	cols <- tv$getColumns()
	for (c in cols)
		tv$removeColumn(c)
	tv$getModel()$clear()
	}
lst <- gtkListStoreNew("character","character")
for(i in 1:length(data))
	{
	iter <- lst$append()$iter
	lst$set(iter,0,data[i],1,color[i])
	}
tv$setModel(lst)
rend.var <- gtkCellRendererTextNew()
rend.var$set(xalign=0.0)
rend.col <- gtkCellRendererTextNew()
rend.col$set(xalign=0.0)
if(!is.null(headers))
	{
	col.var <- fixTVHeader(headers[1])
	col.col <- fixTVHeader(headers[2])
	}
col.var <- tv$insertColumnWithAttributes(0,col.var,rend.var,text=0)
col.col <- tv$insertColumnWithAttributes(1,col.col,rend.col,text=NULL,background=1)

#setting some properties
if(!update)
	tv$getSelection()$setMode(sel.mode)
}


getListViewData <- function(tv,col=1,fixspc=TRUE)
# get selected text from a list view
{
model <- tv$getModel()
if (!is.null(model))
	{
	iter <- model$getIterFirst()
	data <- ""
	havenext <- iter$retval
	while(havenext)
		{
		data <- c(data,setLocale(model$get(iter$iter,col-1),fixspc=fixspc))
		havenext <- model$iterNext(iter$iter)
		}
	data <- data[-1]
	}
if(length(data)==0)
	retval <- NULL
else
	retval <- unlist(data)
return(retval)
}


getListSelection <- function(tv,col=1,fixspc=TRUE)
# get selected text from a list view
{
selection <- tv$getSelection()
if(selection$countSelectedRows()==0)
	data <- NULL
else
	{
	selected <- selection$getSelectedRows()
	model <- selected$model
	if (!is.null(model))
		{
		path <- selected$retval
		data <- ""
		for(i in path)
			{
			iter <- model$getIter(i)$iter
			data <- c(data,setLocale(model$get(iter,col-1),fixspc=fixspc))
			}
		data <- data[-1]
		}
	}
if(length(data)==0)
	retval <- NULL
else
	retval <- unlist(data)
return(retval)
}


getObjDimension <- function(object)
# retrieve the size of an object based on its type
{
if(!is.null(dim(object)))
	retval <- paste(dim(object),collapse="x")
else
	retval <- length(object)
return(retval)
}


getObjSize <- function(object)
# retrieve objects size in memory (approximate)
{
size <- object.size(object)
retval <- ifelse(size < 1024,paste(round(size),"bytes"),ifelse(size/1024 < 1024,paste(round(size/1024),"Kbytes"),ifelse(size/1024^2 < 1024,paste(round(size/1024^2),"Mbytes"),paste(round(size/1024^3),"Gbytes"))))
return(retval)
}


fillObjList <- function(update=TRUE)
# fill up the var list
{
datanames <- ls(envir=.GlobalEnv)
if(length(datanames)>0)
	{	
	dimensions <- as.vector(sapply(datanames,function(x){getObjDimension(eval(parse(text=x),envir=.GlobalEnv))}))
	sizes <- as.vector(sapply(datanames,function(x){getObjSize(eval(parse(text=x),envir=.GlobalEnv))}))
	fillListView(getWidget("Main","tvwObjects"),list(datanames,dimensions,sizes),update,headers=c("Nome","Dimensão","Tamanho"),sel.mode="browse")
	}
else
	fillListView(getWidget("Main","tvwObjects"),NULL,update,headers=NULL,sel.mode="browse")
}


fillVarList <- function(update=TRUE)
# fill up the var list
{
dataname <- getListSelection(getWidget("Main","tvwObjects"))
if(!is.null(dataname))
	{
	data <- eval(parse(text=dataname[1]),envir=.GlobalEnv)
	if(class(data) %in% c("matrix","data.frame"))
		{
		nomes <- names(data)
		pos <- seq(1:length(nomes))
		classes <- as.vector(sapply(data,class))
		fillListView(getWidget("Main","tvwVariables"),list(pos,nomes,classes),update,headers=c("Pos","Nome","Tipo"),sel.mode="browse")
		}
	else if(typeof(data) %in% c("list"))
		{
		nomes <- names(data)
		classes <- as.vector(unlist(lapply(data,class)))
		fillListView(getWidget("Main","tvwVariables"),list(nomes,classes),update,headers=c("Nome","Tipo"),sel.mode="browse")
		}
	else
		{
		nome <- dataname[1]
		classe <- class(data)
		fillListView(getWidget("Main","tvwVariables"),list(nome,classe),update,headers=c("Nome","Tipo"),sel.mode="browse")
		}
	}
else
	fillListView(getWidget("Main","tvwVariables"),NULL,update,sel.mode="browse")
}


updateMainLists <- function()
# update
{
fillListView(getWidget("Main","tvwHistory"),get("EPIR_TEXT_HISTORY",envir=.EpiREnv))
fillObjList()
# fillVarList()
}


fillComboBox <- function(cb,data,update=TRUE,first=FALSE)	
# fill up a combo box widget
{
if(update)
	cb$getModel()$clear()
lst <- gtkListStoreNew("character")
for (i in data)
	{
    iter <- lst$append()$iter
	lst$set(iter,0,i)
	}
cb$setModel(lst)
if(first)
	cb$setActive(0)
}


getActiveData <- function(cb,fixspc=TRUE)
# get active text from the combo box
{
active <- cb$getActiveIter()
retval <- active$retval
if(!retval)
	return("")
iter <- active$iter
model <- cb$getModel()

if (!is.null(model))
	data <- setLocale(model$get(iter,0)[[1]],fixspc=fixspc)
return(data)
}


fillStatsData <- function(long,short,varlist=TRUE,doubleaxis=FALSE,sel.mode="multiple")
# fill the data combo of the header of the statistics windows
{
fillComboBox(getWidget(long,paste("cbx_",short,"_dataset",sep="")),data=getDataset(),FALSE)
if(varlist)
	{
	fillListView(getWidget(long,paste("tvw_",short,"_variables",sep="")),NULL,FALSE,sel.mode=sel.mode)
	if(doubleaxis)
		fillListView(getWidget(long,paste("tvw_",short,"_variablesx",sep="")),NULL,FALSE,sel.mode=sel.mode)
	} 
}


fillStatsHeader <- function(long,short,type="all",by=TRUE,plot=FALSE,doubleaxis=FALSE)
# fill the header of the statistics windows
{
dataset <- getActiveData(getWidget(long,paste("cbx_",short,"_dataset",sep="")))
if(type=="factor")
	rows <- getFactor(dataset)
else if(type=="numeric")
	rows <- getNumeric(dataset)
else if(type=="all")
	rows <- getNames(dataset)
else 
	rows <- NULL
if(!is.null(dataset))
	fillListView(getWidget(long,paste("tvw_",short,"_variables",sep="")),rows,TRUE)
else
	fillListView(getWidget(long,paste("tvw_",short,"_variables",sep="")),NULL,TRUE)
if(by)
	fillComboBox(getWidget(long,paste("cbx_",short,"_by",sep="")),data=c("",getBy(dataset)),TRUE)
if(plot)
	{
	if(doubleaxis)
		fillListView(getWidget(long,paste("tvw_",short,"_variablesx",sep="")),rows,TRUE)
	else
		fillComboBox(getWidget(long,paste("cbx_",short,"_xaxis",sep="")),data=c("",getNames(dataset)),TRUE)
	}
}


toggleByShowAll <- function(long,short)
# change by combo content to allow non-factor variable
{
dataset <- getActiveData(getWidget(long,paste("cbx_",short,"_dataset",sep="")))
if(getWidget(long,paste("chk_",short,"_showall",sep=""))$getActive())
	fillComboBox(getWidget(long,paste("cbx_",short,"_by",sep="")),data=c("",getNames(dataset)),TRUE)	
else
	fillComboBox(getWidget(long,paste("cbx_",short,"_by",sep="")),data=c("",getBy(dataset)),TRUE)	
}


toggleLegend <- function(long,short)
# change the sensitive status of widgets related to the legens
{
if(getWidget(long,paste("chk_",short,"_legend",sep=""))$getActive())
	{
	getWidget(long,paste("chk_",short,"_hlegend",sep=""))$setSensitive(TRUE)
	getWidget(long,paste("cbx_",short,"_legendpos",sep=""))$setSensitive(TRUE)
	}
else
	{
	getWidget(long,paste("chk_",short,"_hlegend",sep=""))$setSensitive(FALSE)
	getWidget(long,paste("cbx_",short,"_legendpos",sep=""))$setSensitive(FALSE)
	}
}


fillFilterConstructor <- function(dataset,update=TRUE)
# fill the treeviews for filtering setting options
{
long <- "SelectRows"
short <- "SR"
# setting values
if (!is.null(dataset))
	variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
else
	variables <- NULL
relations <- c("","!=","<","<=",">",">=","==","pertence","não-pertence")
values <- NULL
connectors <- c("E","OU","NULO")
fillListView(getWidget(long,paste("tvw_",short,"_variables",sep="")),variables,update,"Variável",sel.mode="browse")
fillListView(getWidget(long,paste("tvw_",short,"_relations",sep="")),relations,update,"Operador relacional",sel.mode="browse")
fillListView(getWidget(long,paste("tvw_",short,"_values",sep="")),values,update,"Valores",sel.mode="multiple")
fillListView(getWidget(long,paste("tvw_",short,"_connectors",sep="")),connectors,update,"Operador lógico",sel.mode="browse")
fillListView(getWidget(long,paste("tvw_",short,"_filters",sep="")),NULL,update,sel.mode="single")
}


updateFilterValues <- function(dataset)
# fill the treeviews for filtering setting options
{
selected <- getListSelection(getWidget("SelectRows","tvw_SR_variables"))
values <- levels(factor(eval(parse(text=paste(dataset,"$",selected,sep="")),envir=.GlobalEnv),exclude=NULL))
fillListView(getWidget("SelectRows","tvw_SR_values"),values,TRUE,"Valores")
}


fillFilterQuery <- function()
# build the filter query
{
long <- "SelectRows"
short <- "SR"
oldquery <- getListViewData(getWidget(long,paste("tvw_",short,"_filters",sep="")))
# getting values
variable <- getListSelection(getWidget(long,paste("tvw_",short,"_variables",sep="")))
relation <- getListSelection(getWidget(long,paste("tvw_",short,"_relations",sep="")))
values <- getListSelection(getWidget(long,paste("tvw_",short,"_values",sep="")))
connector <- getListSelection(getWidget(long,paste("tvw_",short,"_connectors",sep="")))
# building 
connector <- ifelse(connector=="E","&",ifelse(connector=="OU","|",""))
if(!is.null(relation))
	{	
	if(relation %in% c("pertence","não-pertence"))
		relation <- ifelse(relation=="pertence","%pertence%","%!pertence%")
	}
else
	{
	msgDialog(long,"error","É necessário selecionar uma relação antes de continuar.")
	return()
	}
if(!is.null(values))	
	{
	if(length(values)==1)
		query <- paste("(",variable," ",relation," ",eval(parse(text=values),envir=.GlobalEnv),") ",connector,sep="")
	else
		{
		values <- vectorToString(sapply(values,function(x)eval(parse(text=x),envir=.GlobalEnv)),quoted=FALSE)
		query <- paste("(",variable," ",relation," ","c(",values,")",") ",connector,sep="")
		}
	}
else
	{
	msgDialog(long,"error","É necessário selecionar ao menos um valor antes de continuar.")
	return()
	}

if (connector=="")
	query <- substring(query,1,nchar(query,type="chars")-1)
if(length(oldquery)>0)
	lastquery <- oldquery[length(oldquery)]
else
	lastquery <- ""
if((lastquery!="") & !(substring(lastquery,nchar(lastquery,type="chars")) %in% c("&","|")))
	{
	msgDialog(long,"error","Remova o filtro sem conector lógico antes de continuar.")
	return()
	}
else
	{
	if (!(query %in% oldquery))
		newquery <- c(oldquery,query)
	else
		{
		msgDialog(long,"info","Este filtro já foi definido.")
		newquery <- oldquery
		}
	fillListView(getWidget(long,paste("tvw_",short,"_filters",sep="")),newquery,TRUE,sel.mode="single")
	}  
}


removeSelectedRows <- function(tv)
# build the filter query
{
selected <- tv$getSelection()$getSelectedRows()
model <- selected$model
paths <- selected$retval
for (i in paths)
	{
	iter <- model$getIter(i)
	if (iter$retval)
		model$remove(iter$iter)
	}
}


fillOperationConstructor <- function(dataset,update=TRUE)
# fill the treeviews for filtering setting options
{
long <- "DataOperations"
short <- "DO"
# setting values
functions <- c("log","log10","exp","logit","alogit","quadrado","raiz2","identidade","inverso","+","-","*","/")
if (!is.null(dataset))
	variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
else
	variables <- NULL
fillListView(getWidget(long,paste("tvw_",short,"_functions",sep="")),functions,update,"Função",sel.mode="browse")
fillListView(getWidget(long,paste("tvw_",short,"_variables",sep="")),variables,update,"Variável",sel.mode="browse")
fillListView(getWidget(long,paste("tvw_",short,"_operations",sep="")),NULL,update,sel.mode="single")
}


fillOperationList <- function(fixspc=TRUE)
# build the filter query
{
long <- "DataOperations"
short <- "DO"
dataset <- eval(parse(text=getActiveData(getWidget(long,paste("cbx_",short,"_dataset",sep="")))),envir=.GlobalEnv)
newvar <- getWidget(long,paste("txt_",short,"_newvar",sep=""))$getText()
expression <- getWidget(long,paste("txt_",short,"_expression",sep=""))$getText()
tvwOperations <- getWidget(long,paste("tvw_",short,"_operations",sep=""))
oldlist <- getListViewData(tvwOperations,fixspc=fixspc)

with(dataset,{
	result <- try(eval(parse(text=expression)),silent=TRUE)
	if(class(result)=="try-error")
		{
		msgDialog(long,"error","Há um erro na expressão. Corrija-a e tente novamente.")
		return()
		}
    })

if((newvar=="") | (expression==""))
	{
	msgDialog(long,"error","O novo nome do objeto e a expressão devem estar definidos.")
	return()
	}
else
	{
	operation <- paste(newvar," = ",expression,sep="")
	newlist <- c(oldlist,operation)
	fillListView(tvwOperations,newlist,TRUE,sel.mode="single")
	}
}


fillGraphWindow <- function(long,short,doubleaxis=FALSE,sel.mode="multiple")
# fill the basic widget on a graphic window
{
fillStatsData(long,short,varlist=TRUE,doubleaxis,sel.mode)
# check type
type <- try(getWidget(long,paste("cbx_",short,"_type",sep="")),silent=TRUE)
if(inherits(type,"GtkComboBox"))
	{
	fillComboBox(type,data=get("EPIR_PLOT_TYPES",env=.EpiREnv),TRUE,TRUE)
	type$setActive(0)
	}
# check lwd
lty <- try(getWidget(long,paste("cbx_",short,"_lty",sep="")),silent=TRUE)
if(inherits(lty,"GtkComboBox"))
	{
	fillComboBox(lty,data=get("EPIR_PLOT_LINE_TYPES",env=.EpiREnv),TRUE,TRUE)
	lty$setActive(0)
	}
# check pch
pch <- try(getWidget(long,paste("cbx_",short,"_pch",sep="")),silent=TRUE)
if(inherits(pch,"GtkComboBoxEntry"))
	{
	fillComboBox(pch,data=get("EPIR_PLOT_POINT_TYPES",env=.EpiREnv),TRUE,TRUE)
	pch$setActive(0)
	}
# check lwd
size <- try(getWidget(long,paste("spb_",short,"_size",sep="")),silent=TRUE)
if(inherits(size,"GtkSpinButton"))
	size$setValue(1)
# check legendpos
legendpos <- try(getWidget(long,paste("cbx_",short,"_legendpos",sep="")),silent=TRUE)
if(inherits(legendpos,"GtkComboBox"))
	{
	fillComboBox(legendpos,data=get("EPIR_PLOT_LEGEND_POSITIONS",env=.EpiREnv),TRUE,TRUE)
	legendpos$setActive(1)
	}
# check legend
legend <- try(getWidget(long,paste("cbx_",short,"_legend",sep="")),silent=TRUE)
if(inherits(legend,"GtkCheckBox"))
	{
	getWidget(long,paste("chk_",short,"_hlegend",sep=""))$setSensitive(FALSE)
	getWidget(long,paste("cbx_",short,"_legendpos",sep=""))$setSensitive(FALSE)
	}
# check color
color <- try(getWidget(long,paste("tvw_",short,"_color",sep="")),silent=TRUE)
if(inherits(color,"GtkTreeView"))
	{
	if(doubleaxis)
		fillListView(color,NULL,update=FALSE,headers=NULL,sel.mode="single")	
	else
		fillListView(color,NULL,update=FALSE,headers=NULL,sel.mode="none")	
	}
# check palette
palette <- try(getWidget(long,paste("cbx_",short,"_palette",sep="")),silent=TRUE)
if(inherits(palette,"GtkComboBox"))
	{
	fillComboBox(palette,data=get("EPIR_PLOT_PALETTE",env=.EpiREnv),TRUE,TRUE)
	palette$setActive(0)
	}
# check hlinecol
try(setHlineColor(long,short),silent=TRUE)

# check overlay
overlay <- try(getWidget(long,paste("cbx_",short,"_overlay",sep="")),silent=TRUE)
if(inherits(overlay,"GtkComboBox"))
	{
	fillComboBox(overlay,data=NULL,TRUE,FALSE)
	overlay$setActive(0)
	}
}


setHlineColor <- function(long,short,name="hlinecol")
# set hline button background color
{
hlinecol <- getWidget(long,paste("drw_",short,"_",name,sep=""))
if(inherits(hlinecol,"GtkDrawingArea"))
	{
	hlinecol$modifyBg("normal",get("EPIR_DEFAULT_COLOR",env=.EpiREnv))
	getWidget(long,paste("btn_",short,"_",name,sep=""))$setSensitive(FALSE)
	assign("EPIR_CURRENT_COLOR",get("EPIR_DEFAULT_COLOR",env=.EpiREnv),envir=.EpiREnv)
	}
}
	

toggleExecute <- function(long,short,type="variables",button="execute",widget="treeview",selection=TRUE)
# toggle the execute button
{
if(widget=="treeview")
	{
	if(selection)
		{
		if(getWidget(long,paste("tvw_",short,"_",type,sep=""))$getSelection()$countSelectedRows()>0)
			active <- TRUE
		else
			active <- FALSE
		}
	else
		{
		if(length(getListViewData(getWidget(long,paste("tvw_",short,"_",type,sep=""))))>0)
			active <- TRUE
		else
			active <- FALSE
		}
	}
else if(widget=="textedit")
	{
	if(getWidget(long,paste("txt_",short,"_",type,sep=""))$getText()!="")
		active <- TRUE
	else
		active <- FALSE
	}
if(active)	
	getWidget(long,paste("btn_",short,"_",button,sep=""))$setSensitive(TRUE)
else
	getWidget(long,paste("btn_",short,"_",button,sep=""))$setSensitive(FALSE)
}


vectorToString <- function(data,quoted=TRUE,coll=",")
# convert a vector of strings to a comma separetaded string
{
if (quoted)
	string <- paste(data,sep="",collapse=paste("\"",coll,"\"",sep=""))
else
	string <- paste(data,sep="",collapse=coll)
return(string)
}


# functions to operating the output text
clearTv <- function(txt)
# clear a text view 
{
txt$modifyFont(pangoFontDescriptionFromString(EPIR_TEXT_FONT))
msg <- ""
txt$getBuffer()$setText(msg)
}


welcomeTv <- function(txt)
# print a welcome message on a text view 
{
msg <- paste("Bem vindo, ",Sys.info()["user"],"@",Sys.info()["nodename"],", ao EpiR versão ",EPIR_VER," em ",Sys.time(),"\n\n",sep="")
txt$getBuffer()$setText(msg)
}


appendTv <- function(txt,msg)
# add text to a text view
{
buf <- txt$getBuffer()
	for (i in msg)
		{
		cursor <- buf$getEndIter()$iter
		buf$insert(cursor,paste(i,"\n"))
		}
cursor <- buf$getEndIter()$iter
buf$insert(cursor,"\n")
cursor <- buf$getEndIter()$iter
mark <- buf$createMark(where=cursor)
txt$scrollToMark(mark,within.margin=0)	
}


insertTv <- function(txt,msg,fixspc=FALSE)
# add text to a text view at the cursor
{
msg <- setLocale(msg,fixspc=FALSE)
buf <- txt$getBuffer()
if(msg!="")
	buf$insertAtCursor(msg)
}


getTv <- function(txt)
# get the content of the text view
{
buf <- txt$getBuffer()
cursor.start <- buf$getStartIter()$iter
cursor.end <- buf$getEndIter()$iter
content <- buf$getText(cursor.start,cursor.end) 
return(content)
}


copyToClipboard <- function(window,widget)
{
widget <- getWidget(window,widget)
clipboard <- widget$getClipboard("CLIPBOARD")
if(inherits(widget,"GtkTextView"))
	{
	buf <- widget$getBuffer()
	buf$copyClipboard(clipboard)
	}
else if(inherits(widget,"GtkTreeView"))
	clipboard$setText(getListSelection(widget)[[1]])
}


pasteFromClipboard <- function(window,widget)
{
widget <- getWidget(window,widget)
if(inherits(widget,"GtkTextView"))
	{
	clipboard <-widget$getClipboard("CLIPBOARD")
	buf <- widget$getBuffer()
	buf$pasteClipboard(clipboard,NULL,TRUE)
	}
}


parseCmd <- function(cmd.text)
# parse the commands in a vector
{
if(cmd.text!="")
	{
	cmd.text <- gsub("\\\n","",cmd.text)		# remove line breaks
	cmd.vec <- unlist(strsplit(cmd.text,";",fixed=TRUE))		# break multiline commands
	runCommand(cmd.vec)	# it is a vector
	}
}


runCommand <- function(cmd)
# run a command and pipe the result to the output widget
{
for(c in cmd)
	{
	# translate
	cmd.exp <- parse(text=trim(c))
	file <- tempfile("EpiR.out")
	sink(file)
	result <- try(eval(cmd.exp,envir=.GlobalEnv),silent=TRUE)
	if(!inherits(result,"try-error"))
		{
		if ((!is.null(result)) & (!(class(result) %in% c("data.frame"))))
			print(result)
		history <- get("EPIR_TEXT_HISTORY",envir=.EpiREnv)
		if(c %notin% history) # eliminate redundancy in history
			assign("EPIR_TEXT_HISTORY",c(c,history),envir=.EpiREnv)
		}
	else
		eval(cat("Erro: Houve um erro ao executar o comando."),envir=.GlobalEnv)
	sink()
	cat("\n",file=file,append=TRUE)
	out <- iconv(readLines(file),to=EPIR_LOCALE)
	unlink(file)
	txtOutput <- getWidget("Main","txtOutput")
	appendTv(txtOutput,paste(">",c))
	appendTv(txtOutput,out)
	assign("EPIR_EXECUTE_STATUS",FALSE,envir=.EpiREnv)
	updateMainLists()
	}
}


cleanStr <- function(str,sep="")
# clean some invalid caracteres for file naming
{
valid.str <- gsub("[[:blank:][:punct:][:space:]]",sep,str)
return(valid.str)
}


fixPath <- function(filename)
# fix MS path
{
if(is.windows())
	return(gsub("\\\\","/",filename))
else
	return(filename)
}

getExtension <- function(path)
# get the extension part of a filename
{
parts <- try(strsplit(path, "\\.")[[1]],silent=TRUE)
if (length(parts) > 1)
		return(tolower(parts[length(parts)]))
else
		return("")
}

# message dialogs
msgDialog <- function(window,type,...)
{
if(!is.null(window))
	window <- getWidget(window)
msg <- gtkMessageDialog(window,c("modal","destroy-with-parent"),type,"ok",...)
msg$run()
msg$destroy()
}	

askDialog <- function(window,...)
# show a question dialog
{
if(!is.null(window))
	window <- getWidget(window)
dialog <- gtkMessageDialogNew(window, c("modal","destroy-with-parent"), "question","yes-no",...)
result <- dialog$run()
dialog$destroy()
if (result == GtkResponseType["yes"])
	return("yes")
else 
	return("no")
}

addFilter <- function(dialog,name,pattern)
# add file filter to a dialog
{
ff <- gtkFileFilterNew()
ff$setName(name)
for (p in pattern)
	{
	ff$addPattern(p)
	ff$addPattern(toupper(p))
	}
dialog$addFilter(ff)
}

fileDialog <- function(window,title,action,filter)
# show a file dialog
{
winname <- window
window <- getWidget(window)
if(action=="open")
	dialog <- gtkFileChooserDialog(title,window,"open","gtk-cancel", GtkResponseType["cancel"],"gtk-open", GtkResponseType["accept"])
else if(action=="save")
	dialog <- gtkFileChooserDialog(title,window,"save","gtk-cancel", GtkResponseType["cancel"],"gtk-save", GtkResponseType["accept"])
else if(action=="select-folder")
	dialog <- gtkFileChooserDialog(title,window,"select-folder","gtk-cancel", GtkResponseType["cancel"],"gtk-open", GtkResponseType["accept"])
else 
	return()
dialog$setTransientFor(window)
dialog$setLocalOnly(FALSE)
if(action=="save")
	dialog$setCurrentName(paste(EPIR_USER,"_",cleanStr(Sys.time()),sep=""))
# filters
if(!is.null(filter))
	for(f in 1:length(filter$type))
		addFilter(dialog,filter$type[f],filter$pat[f])
# end filters
if(dialog$run()!=GtkResponseType["accept"])
  	{
    dialog$destroy()
    return()
    }
filename <- setLocale(dialog$getFilename(),fixspc=FALSE)
folder <- setLocale(dialog$getCurrentFolder(),fixspc=FALSE)
if(action=="save")
	{
	type <- dialog$getFilter()$getName()
	pat <- filter[[2]][filter[[1]]==type]
	patextension <- getExtension(pat)
	fileextension <- getExtension(filename)
	if((getExtension(filename)!="") & (getExtension(filename)!=patextension))
		msgDialog(winname,"warning","O arquivo será salvo com uma extensão não compatível com o formato.")
	if(fileextension=="")
		filename <- paste(filename,".",patextension,sep="")
	else
		filename <- filename
	}
dialog$destroy()
if(file.exists(filename) & (action=="save"))
	{
	if(askDialog(winname,"O arquivo",filename,"já existe.\nDeseja substituí-lo?")=="no")
		filename <- NULL
	}
else if(!file.exists(filename) & (action=="open"))
	{
	msgDialog(winname,"error","O arquivo",filename,"não existe.")
	filename <- NULL
	}
return(list(filename=fixPath(filename),folder=fixPath(folder)))
}

colorDialog <- function(window,title,color)
# show a file dialog
{
winname <- window
window <- getWidget(window)
dialog <- gtkColorSelectionDialog(title,show=FALSE)
dialog$setTransientFor(window)
# get color selection
colorsel <- dialog[["colorsel"]]
colorsel$setPreviousColor(color)
colorsel$setCurrentColor(color)
colorsel$setHasPalette(TRUE)
if (dialog$run() == GtkResponseType["ok"])
	color <- gdkcolor2hex(colorsel$getCurrentColor()$color)
else
	color <- NULL
dialog$destroy()
return(color)
}

gdkcolor2hex <- function(color)
# convert gdkColor (RGB) to hexadecimal string
{
	fixm0 <- function(x) ifelse(x<10,sprintf("0%X",x),sprintf("%X",x))
                   
red <- fixm0(floor(color$red/256))
green <- fixm0(floor(color$green/256))
blue <- fixm0(floor(color$blue/256))
retval <- sprintf("#%s%s%s",red,green,blue)
return(retval)
}


genColors <- function(ncolors,palette="rainbow")
# generate a rainbow palette
{
if(ncolors>0)
	{
	if(palette=="rainbow")
		colors <- rainbow(ncolors)
	else if(palette=="gray")
		colors <- gray.colors(ncolors)
	else if(palette=="heat")
		colors <- heat.colors(ncolors)
	else if(palette=="terrain")
		colors <- terrain.colors(ncolors)
	else if(palette=="cm")
		colors <- cm.colors(ncolors)
	else if(palette=="topo")
		colors <- topo.colors(ncolors)
	else
		colors <- rainbow(ncolors)
	}
else
	colors <- NULL
return(substring(colors,1,7))
}


gray.colors <- function(n,intensity="normal")
# generate gray colors
{
if(intensity=="light")
	cols <- gray(seq(.1,.5,length.out=n))
else if (intensity=="dark")
	cols <- gray(seq(.5,.9,length.out=n))
else
	cols <- gray(seq(.1,.9,length.out=n))
return(cols)
}


trim <- function(text)
# remove leading and trailing spaces of a string 
{
# forward
s <- 1
e <- nchar(text,type="chars")
isspace <- TRUE
while(isspace)
	{
	if(substring(text,s,s)==" ")
		{
		s <- s+1
		isspace <- TRUE
		}
	else
		{
		newstr <- substring(text,s,e)
		isspace <- FALSE
		}
	}
# backward
s <- 1
e <- nchar(newstr,type="chars")
isspace <- TRUE
while(isspace)
	{
	if(substring(newstr,e,e)==" ")
		{
		e <- e-1
		isspace <- TRUE
		}
	else
		{
		cleanstr <- substring(newstr,s,e)
		isspace <- FALSE
		}
	}
return(cleanstr)
}

wait <- function(secs)
# wait sec seconds
{
t0 <- Sys.time() 
while((Sys.time()-t0) <= secs){}
}

years <- function(date,type="yyyy-mm-dd")
{
if(type=="yyyy-mm-dd")
  x <- substring(date,first=1,last=4)
return(x)
}

setLocale <- function(string,to="latin1",fixspc=TRUE)
# convert strings from to EPIR_LOCALE
{
if(l10n_info()$`Latin-1`)
	retval <- iconv(string,from=EPIR_LOCALE,to=to)
else
	retval <- string
if(fixspc)
	return(gsub(" ","_",retval)) #fix composite object names
else
	return(retval)
} 

checkInternet <- function()
{
site <- url("http://www.r-project.org/")
test.site <- try(open(site),silent=TRUE)
if(class(test.site) == "try-error")
	retval <- FALSE
else
	{
	retval <- TRUE
	close(site)
	}
return(retval)
}

fixTVHeader <- function(names)
{
if(!is.null(names))
	{
	names <- strsplit(names, "_")
	names <- lapply(names, paste, collapse="__")
	names <- unlist(names)
	}
return(names)
}

popUpMenu <- function(label,callback,event)
# callback must be a vector
{
popupmenu=gtkMenuNew()
for (i in 1:length(label))
	{
	item <- paste("item_",label[i],sep="")
	assign(item,gtkMenuItemNewWithLabel(label[i]))
	popupmenu$add(get(item))
	gSignalConnect(get(item),"activate",callback[[i]])
	}
gtkMenuPopup(popupmenu,NULL,NULL,NULL,NULL,event$button,event$time)
}

