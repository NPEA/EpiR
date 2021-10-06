# plots sectors callback functions

# main buttons
on_btn_PS_help_clicked <- function(widget,user.data)
{
showHelp("PlotsSectors")
}

on_btn_PS_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsSectors")
}

on_btn_PS_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsSectors","cbx_PS_dataset"))
vars <- vectorToString(getListSelection(getWidget("PlotsSectors","tvw_PS_variables")))
nvars <- length(getListSelection(getWidget("PlotsSectors","tvw_PS_variables")))
by <- getActiveData(getWidget("PlotsSectors","cbx_PS_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsSectors","txt_PS_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsSectors","txt_PS_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsSectors","txt_PS_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsSectors","txt_PS_yaxis")$getText(),fixspc=FALSE)
colors <- getActiveData(getWidget("PlotsSectors","cbx_PS_col"))
if(nvars == 1)
	{
	new.label <- getListViewData(getWidget("PlotsSectors","tvw_PS_viewFactor"),2)
	if(!any(new.label == ""))
		change.labels <- TRUE
	else
		change.labels <- FALSE
	new.label <- paste("'",new.label,"'",sep="")
	new.label <- strsplit(paste(paste(new.label, ",",sep=""),collapse=""), "")
	new.label <- paste(new.label[[1]][-length(new.label[[1]])],collapse="")
	new.label <- paste("c(",new.label,")",sep="")
	}

# translate
if(nvars != 1)
	cmd <- paste("grafico.setores(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
else
	{
	if(!change.labels)
		cmd <- paste("grafico.setores(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
	else
		cmd <- paste("grafico.setores(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\",rotulos=",new.label,")",sep="")
	}
runCommand(cmd)
closeWindow("PlotsSectors","PS")
}

# operations
on_cbx_PS_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsSectors","PS","all")
getWidget("PlotsSectors","btn_PS_execute")$setSensitive(FALSE)
}

on_tvw_PS_variables_button_release_event <- function(widget,event,user.data)
{
dataset <- getActiveData(getWidget("PlotsSectors","cbx_PS_dataset"),fixspc=FALSE)
variables <- getListSelection(getWidget("PlotsSectors","tvw_PS_variables"))
toggleExecute("PlotsSectors","PS")
if(length(variables) != 1)
	getWidget("PlotsSectors","exp_PS_viewFactor")$setSensitive(FALSE)
else
	{
	getWidget("PlotsSectors","exp_PS_viewFactor")$setSensitive(TRUE)
	factor.columns <- unique(eval(parse(text=paste(dataset,"$",variables,sep=""))))
	factor.columns <- factor.columns[!is.na(factor.columns)]
	fillListView(getWidget("PlotsSectors","tvw_PS_viewFactor"), list(factor.columns,rep("",length(factor.columns))),update=TRUE,sel.mode="single",headers=c("Categoria original","Nova categoria"))
	}
}

on_chk_PS_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsSectors","PS")
}

on_tvw_PS_viewFactor_row_activated <- function(widget,path,column,user.data)
{
tv <- getWidget("PlotsSectors","tvw_PS_viewFactor")
model <- tv$getModel()
path <- gtkTreePathGetIndices(path)
old_factor <- getListViewData(getWidget("PlotsSectors","tvw_PS_viewFactor"))
old_factor <- old_factor[as.numeric(path)+1]
setWindow("ChangeLevel","PlotsSectors")
getWidget("ChangeLevel","lbl_CHL_oldLevel")$setText(old_factor)
}