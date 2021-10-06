# qqplots callback functions

on_btn_QQ_help_clicked <- function(widget,user.data)
{
showHelp("PlotsQQ")
}

on_btn_QQ_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsQQ")
}

on_btn_QQ_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsQQ","cbx_QQ_dataset"))
vars <- vectorToString(getListSelection(getWidget("PlotsQQ","tvw_QQ_variables")))
by <- getActiveData(getWidget("PlotsQQ","cbx_QQ_by"))
 if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsQQ","txt_QQ_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsQQ","txt_QQ_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsQQ","txt_QQ_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsQQ","txt_QQ_yaxis")$getText(),fixspc=FALSE)
colors <- vectorToString(getListViewData(getWidget("PlotsQQ","tvw_QQ_color"),2))

cmd <- paste("grafico.normal(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
runCommand(cmd)
closeWindow("PlotsQQ","QQ")
}


# operations
on_cbx_QQ_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsQQ","QQ","numeric")
getWidget("PlotsQQ","btn_QQ_execute")$setSensitive(FALSE)
fillListView(getWidget("PlotsQQ","tvw_QQ_color"),NULL,TRUE)
}

on_tvw_QQ_variables_button_release_event <- function(widget,event,user.data)
{
variables <- getListSelection(getWidget("PlotsQQ","tvw_QQ_variables"))
if(!is.null(variables))
	{
	colors <- genColors(length(variables))
	tvColor <- getWidget("PlotsQQ","tvw_QQ_color")
	fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
	toggleExecute("PlotsQQ","QQ")
	}
}

on_tvw_QQ_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsQQ","tvw_QQ_color")
if(is.null(getListViewData(tvColor)))
	return()
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsQQ","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	model$set(iter,1,color)
	}
}

on_chk_QQ_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsQQ","QQ")
}