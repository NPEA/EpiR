# plotsmosaic callback functions

# main buttons
on_btn_MOS_help_clicked <- function(widget,user.data)
{
showHelp("PlotsMosaic")
}

on_btn_MOS_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsMosaic")
}

on_btn_MOS_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsMosaic","cbx_MOS_dataset"))
pairs <- vectorToString(get("EPIR_SELECTED_GRAPHS",envir=.EpiREnv))
by <- getActiveData(getWidget("PlotsMosaic","cbx_MOS_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsMosaic","txt_MOS_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsMosaic","txt_MOS_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsMosaic","txt_MOS_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsMosaic","txt_MOS_yaxis")$getText(),fixspc=FALSE)
colors <- vectorToString(getListViewData(getWidget("PlotsMosaic","tvw_MOS_color"),2))
# translate
cmd <- paste("grafico.mosaico(\"",dataset,"\",\"",pairs,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
runCommand(cmd)
closeWindow("PlotsMosaic","MOS")
}


# operations
on_cbx_MOS_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsMosaic","MOS","all",plot=TRUE,doubleaxis=TRUE)
getWidget("PlotsMosaic","btn_MOS_execute")$setSensitive(FALSE)
getWidget("PlotsMosaic", "cbx_MOS_by")$setSensitive(TRUE)
fillListView(getWidget("PlotsMosaic","tvw_MOS_color"),NULL,TRUE)
}

on_tvw_MOS_variables_button_release_event <- function(widget,event,user.data)
{
if(!is.null(getListSelection(getWidget("PlotsMosaic","tvw_MOS_variables"))) & !is.null(getListSelection(getWidget("PlotsMosaic","tvw_MOS_variablesx"))))
	getWidget("PlotsMosaic","btn_MOS_add")$setSensitive(TRUE)
else
	getWidget("PlotsMosaic","btn_MOS_add")$setSensitive(FALSE)
}

on_tvw_MOS_variablesx_button_release_event <- function(widget,event,user.data)
{
if(!is.null(getListSelection(getWidget("PlotsMosaic","tvw_MOS_variables"))) & !is.null(getListSelection(getWidget("PlotsMosaic","tvw_MOS_variablesx"))))
	getWidget("PlotsMosaic","btn_MOS_add")$setSensitive(TRUE)
else
	getWidget("PlotsMosaic","btn_MOS_add")$setSensitive(FALSE)
}

on_btn_MOS_add_clicked <- function(widget,user.data)
{
getWidget("PlotsMosaic", "cbx_MOS_by")$setSensitive(FALSE)
tvwVariabley <- getWidget("PlotsMosaic","tvw_MOS_variables")
tvwVariablex <- getWidget("PlotsMosaic","tvw_MOS_variablesx")
variabley <- getListSelection(tvwVariabley)
variablex <- getListSelection(tvwVariablex)
tvColor <- getWidget("PlotsMosaic","tvw_MOS_color")
newpair <- paste(variabley,"~",variablex,sep="")
pairs <- c(getListViewData(tvColor),newpair)
colors <- genColors(length(pairs))
assign("EPIR_SELECTED_GRAPHS",pairs,envir=.EpiREnv)
fillColListView(tvColor,pairs,colors,update=TRUE,headers=c("Gráfico","Cor"))
toggleExecute("PlotsMosaic","MOS",type="color",button="execute",selection=FALSE)
tvwVariabley$getSelection()$unselectAll()    
tvwVariablex$getSelection()$unselectAll()    
getWidget("PlotsMosaic","btn_MOS_add")$setSensitive(FALSE)
toggleExecute("PlotsMosaic","MOS",type="color",button="execute",selection=FALSE)
}

on_btn_MOS_remove_clicked <- function(widget,user.data)
{
removeSelectedRows(getWidget("PlotsMosaic","tvw_MOS_color"))
toggleExecute("PlotsMosaic","MOS",type="color",button="remove")
toggleExecute("PlotsMosaic","MOS",type="color",button="execute",selection=FALSE)
if(is.null(getListViewData(getWidget("PlotsMosaic", "tvw_MOS_color"))))
	getWidget("PlotsMosaic", "cbx_MOS_by")$setSensitive(TRUE)
else
	getWidget("PlotsMosaic", "cbx_MOS_by")$setSensitive(FALSE)
}

on_tvw_MOS_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsMosaic","tvw_MOS_color")
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsMosaic","Selecione a cor da linha",cur.color)
if(!is.null(color))
	model$set(iter,1,color)
}

on_tvw_MOS_color_button_release_event <- function(widget,event,user.data)
{
toggleExecute("PlotsMosaic","MOS",type="color",button="remove")
}

on_chk_MOS_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsMosaic","MOS")
}