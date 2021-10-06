# plots box callback functions

# main buttons
on_btn_BX_help_clicked <- function(widget,user.data)
{
showHelp("PlotsBox")
}

on_btn_BX_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsBox")
}

on_btn_BX_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsBox","cbx_BX_dataset"))
vars <- vectorToString(getListSelection(getWidget("PlotsBox","tvw_BX_variables")))
by <- getActiveData(getWidget("PlotsBox","cbx_BX_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsBox","txt_BX_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsBox","txt_BX_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsBox","txt_BX_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsBox","txt_BX_yaxis")$getText(),fixspc=FALSE)
colors <- vectorToString(getListViewData(getWidget("PlotsBox","tvw_BX_color"),2))

cmd <- paste("grafico.caixa(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
runCommand(cmd)
closeWindow("PlotsBox","BX")

}


# operations
on_cbx_BX_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsBox","BX","numeric")
getWidget("PlotsBox","btn_BX_execute")$setSensitive(FALSE)
fillListView(getWidget("PlotsBox","tvw_BX_color"),NULL,TRUE)
}

on_tvw_BX_variables_button_release_event <- function(widget,event,user.data)
{
variables <- getListSelection(getWidget("PlotsBox","tvw_BX_variables"))
if(!is.null(variables))
	{
	colors <- genColors(length(variables))
	tvColor <- getWidget("PlotsBox","tvw_BX_color")
	fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
	toggleExecute("PlotsBox","BX")
	}
}

on_tvw_BX_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsBox","tvw_BX_color")
if(is.null(getListViewData(tvColor)))
	return()
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsBox","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	model$set(iter,1,color)	
	}
}

on_chk_BX_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsBox","BX")
}