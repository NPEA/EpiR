# editData callback functions

# main buttons
on_btn_ED_help_clicked <- function(widget,user.data)
{
showHelp("EditData")
}

on_btn_ED_cancel_clicked <- function(widget,user.data)
{
closeWindow("EditData")
}

on_btn_ED_execute_clicked <- function(widget,user.data)
{
object <- setLocale(getWidget("EditData","txt_ED_object")$getText())
tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
frame <- tvDataframe$getModel()
dataset <- as.data.frame(frame)
assign(object,dataset,envir=.GlobalEnv)
updateMainLists()
closeWindow("EditData")
}


# operations
on_cbx_ED_dataset_changed <- function(widget,user.data)
{
# load the dataframe
object <- getActiveData(getWidget("EditData","cbx_ED_dataset"))
if(trim(object) != "")
	{
	dataset <- eval(parse(text=object),envir=.GlobalEnv)
	nrow.dataset <- nrow(dataset)
	dataset <- dataset[1:min(EPIR_BROWSE_NROWS,nrow(dataset)),]
	column.names <- colnames(dataset)
  dataset <- sapply(1:ncol(dataset), function(i) as.character(dataset[,i]))
  dataset <- as.data.frame(dataset)
	dataset <- cbind(1:min(EPIR_BROWSE_NROWS,nrow(dataset)),dataset)
	colnames(dataset) <- c("Índice", column.names)
	
	getWidget("EditData","txt_ED_object")$setText(object)
	tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
	if(!is.null(tvDataframe$getModel()))
		{
		cols <- tvDataframe$getColumns()
		for(i in cols)
		tvDataframe$removeColumn(i)
		}
	frame <- rGtkDataFrame(frame=dataset)
	tvDataframe$setModel(frame)
	tvDataframe$setHeadersClickable(TRUE)
	renderer <- gtkCellRendererTextNew()
	renderer$set(xalign = 0.0,editable=TRUE)
	cols <- colnames(dataset)
	cols <- fixTVHeader(cols)
	for (i in 1:length(cols))
		col <- tvDataframe$insertColumnWithAttributes(-1,cols[i],renderer,text=(i-1))

	# other stuffs
	getWidget("EditData","btn_ED_edit")$setSensitive(TRUE)
	getWidget("EditData","btn_ED_execute")$setSensitive(TRUE)
	if(nrow.dataset > EPIR_BROWSE_NROWS)
		{
		getWidget("EditData","btn_ED_next")$setSensitive(TRUE)
		getWidget("EditData","btn_ED_last")$setSensitive(TRUE)
		}
	else
		{
		getWidget("EditData","btn_ED_next")$setSensitive(FALSE)
		getWidget("EditData","btn_ED_last")$setSensitive(FALSE)
		}
	getWidget("EditData","btn_ED_previous")$setSensitive(FALSE)
	getWidget("EditData","btn_ED_first")$setSensitive(FALSE)
	}
}

on_btn_ED_edit_clicked <- function(widget,user.data)
{
object <- getActiveData(getWidget("EditData","cbx_ED_dataset"))
dataset <- eval(parse(text=object),envir=.GlobalEnv)
output <- edit(dataset)
nrow.output <- nrow(output)
output <- output[1:min(EPIR_BROWSE_NROWS,nrow.output),]
columns.names <- colnames(output)
output <- sapply(1:ncol(output), function(i) as.character(output[,i]))
output <- as.data.frame(output)
output <- cbind(1:min(EPIR_BROWSE_NROWS,nrow.output),output)
colnames(output) <- c("Índice", column.names)
tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
if(!is.null(tvDataframe$getModel()))
	{
	cols <- tvDataframe$getColumns()
	for(i in cols)
	tvDataframe$removeColumn(i)
	}
frame <- rGtkDataFrame(frame=output)
tvDataframe$setModel(frame)
tvDataframe$setHeadersClickable(TRUE)
renderer <- gtkCellRendererTextNew()
renderer$set(xalign = 0.0,editable=TRUE)
cols <- colnames(output)
for (i in 1:length(cols))
	col <- tvDataframe$insertColumnWithAttributes(-1,cols[i],renderer,text=(i-1))

if(nrow.output > EPIR_BROWSE_NROWS)
	{
	getWidget("EditData","btn_ED_next")$setSensitive(TRUE)
	getWidget("EditData","btn_ED_last")$setSensitive(TRUE)
	}
else
	{
	getWidget("EditData","btn_ED_next")$setSensitive(FALSE)
	getWidget("EditData","btn_ED_last")$setSensitive(FALSE)
	}
getWidget("EditData","btn_ED_previous")$setSensitive(FALSE)
getWidget("EditData","btn_ED_first")$setSensitive(FALSE)
}

on_btn_ED_first_clicked <- function(widget,user.data)
{
object <- getActiveData(getWidget("EditData","cbx_ED_dataset"))
dataset <- eval(parse(text=object),envir=.GlobalEnv)
nrow.dataset <- nrow(dataset)
dataset <- dataset[1:min(EPIR_BROWSE_NROWS,nrow(dataset)),]
column.names <- colnames(dataset)
dataset <- cbind(1:min(EPIR_BROWSE_NROWS,nrow(dataset)),dataset)
dataset <- sapply(1:ncol(dataset), function(i) as.character(dataset[,i]))
dataset <- as.data.frame(dataset)
colnames(dataset) <- c("Índice", column.names)

getWidget("EditData","txt_ED_object")$setText(object)
tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
if(!is.null(tvDataframe$getModel()))
	{
	cols <- tvDataframe$getColumns()
	for(i in cols)
	tvDataframe$removeColumn(i)
	}
frame <- rGtkDataFrame(frame=dataset)
tvDataframe$setModel(frame)
tvDataframe$setHeadersClickable(TRUE)
renderer <- gtkCellRendererTextNew()
renderer$set(xalign = 0.0,editable=TRUE)
cols <- colnames(dataset)
for (i in 1:length(cols))
	col <- tvDataframe$insertColumnWithAttributes(-1,cols[i],renderer,text=(i-1))

if(nrow.dataset > EPIR_BROWSE_NROWS)
	{
	getWidget("EditData","btn_ED_next")$setSensitive(TRUE)
	getWidget("EditData","btn_ED_last")$setSensitive(TRUE)
	}
getWidget("EditData","btn_ED_previous")$setSensitive(FALSE)
getWidget("EditData","btn_ED_first")$setSensitive(FALSE)
}

on_btn_ED_last_clicked <- function(widget,user.data)
{
object <- getActiveData(getWidget("EditData","cbx_ED_dataset"))
dataset <- eval(parse(text=object),envir=.GlobalEnv)
nrow.dataset <- nrow(dataset)
first.row <- max(1, (nrow.dataset - EPIR_BROWSE_NROWS + 1)) 
dataset <- dataset[first.row:nrow.dataset,]
column.names <- colnames(dataset)
dataset <- cbind(first.row:nrow.dataset,dataset)
dataset <- sapply(1:ncol(dataset), function(i) as.character(dataset[,i]))
dataset <- as.data.frame(dataset)
colnames(dataset) <- c("Índice", column.names)

getWidget("EditData","txt_ED_object")$setText(object)
tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
if(!is.null(tvDataframe$getModel()))
	{
	cols <- tvDataframe$getColumns()
	for(i in cols)
	tvDataframe$removeColumn(i)
	}
frame <- rGtkDataFrame(frame=dataset)
tvDataframe$setModel(frame)
tvDataframe$setHeadersClickable(TRUE)
renderer <- gtkCellRendererTextNew()
renderer$set(xalign = 0.0,editable=TRUE)
cols <- colnames(dataset)
for (i in 1:length(cols))
	col <- tvDataframe$insertColumnWithAttributes(-1,cols[i],renderer,text=(i-1))

getWidget("EditData","btn_ED_next")$setSensitive(FALSE)
getWidget("EditData","btn_ED_last")$setSensitive(FALSE)
getWidget("EditData","btn_ED_previous")$setSensitive(TRUE)
getWidget("EditData","btn_ED_first")$setSensitive(TRUE)
}

on_btn_ED_next_clicked <- function(widget,user.data)
{
object <- getActiveData(getWidget("EditData","cbx_ED_dataset"))
dataset <- eval(parse(text=object),envir=.GlobalEnv)
tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
last.row <- getListViewData(getWidget("EditData","tvw_ED_dataframe"),1)
last.row <- last.row[length(last.row)]
last.row <- as.numeric(last.row)
rows.dataset <- nrow(dataset)

if(last.row + EPIR_BROWSE_NROWS  > rows.dataset)
	{
	last.row <- rows.dataset
	first.row <- max(1, (rows.dataset-EPIR_BROWSE_NROWS + 1))
	}
else
	{
	first.row <- last.row + 1
	last.row <- last.row + EPIR_BROWSE_NROWS
	}
dataset <- dataset[first.row:last.row,]
column.names <- colnames(dataset)
dataset <- cbind(first.row:last.row,dataset)
dataset <- sapply(1:ncol(dataset), function(i) as.character(dataset[,i]))
dataset <- as.data.frame(dataset)
colnames(dataset) <- c("Índice", column.names)

if(!is.null(tvDataframe$getModel()))
	{
	cols <- tvDataframe$getColumns()
	for(i in cols)
	tvDataframe$removeColumn(i)
	}
frame <- rGtkDataFrame(frame=dataset)
tvDataframe$setModel(frame)
tvDataframe$setHeadersClickable(TRUE)
renderer <- gtkCellRendererTextNew()
renderer$set(xalign = 0.0,editable=TRUE)
cols <- colnames(dataset)
for (i in 1:length(cols))
	col <- tvDataframe$insertColumnWithAttributes(-1,cols[i],renderer,text=(i-1))
if(last.row == rows.dataset)
		{
		getWidget("EditData","btn_ED_next")$setSensitive(FALSE)
		getWidget("EditData","btn_ED_last")$setSensitive(FALSE)
		}
getWidget("EditData","btn_ED_previous")$setSensitive(TRUE)
getWidget("EditData","btn_ED_first")$setSensitive(TRUE)
}

on_btn_ED_previous_clicked <- function(widget,user.data)
{
object <- getActiveData(getWidget("EditData","cbx_ED_dataset"))
dataset <- eval(parse(text=object),envir=.GlobalEnv)
tvDataframe <- getWidget("EditData","tvw_ED_dataframe")
first.row <- getListViewData(getWidget("EditData","tvw_ED_dataframe"),1)
first.row <- first.row[1]
first.row <- as.numeric(first.row)
rows.dataset <- nrow(dataset)

if(first.row - EPIR_BROWSE_NROWS  < 1)
	{
	last.row <- min(EPIR_BROWSE_NROWS, rows.dataset)
	first.row <- 1
	}
else
	{
	first.row <- first.row - EPIR_BROWSE_NROWS
	last.row <- first.row + EPIR_BROWSE_NROWS - 1
	}
dataset <- dataset[first.row:last.row,]
column.names <- colnames(dataset)
dataset <- cbind(first.row:last.row,dataset)
dataset <- sapply(1:ncol(dataset), function(i) as.character(dataset[,i]))
dataset <- as.data.frame(dataset)
colnames(dataset) <- c("Índice", column.names)

if(!is.null(tvDataframe$getModel()))
	{
	cols <- tvDataframe$getColumns()
	for(i in cols)
	tvDataframe$removeColumn(i)
	}
frame <- rGtkDataFrame(frame=dataset)
tvDataframe$setModel(frame)
tvDataframe$setHeadersClickable(TRUE)
renderer <- gtkCellRendererTextNew()
renderer$set(xalign = 0.0,editable=TRUE)
cols <- colnames(dataset)
for (i in 1:length(cols))
	col <- tvDataframe$insertColumnWithAttributes(-1,cols[i],renderer,text=(i-1))
if(first.row == 1)
	{
	getWidget("EditData","btn_ED_previous")$setSensitive(FALSE)
	getWidget("EditData","btn_ED_first")$setSensitive(FALSE)
	}
getWidget("EditData","btn_ED_next")$setSensitive(TRUE)
getWidget("EditData","btn_ED_last")$setSensitive(TRUE)
}