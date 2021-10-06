# plotsbar callback functions

# main buttons
on_btn_DT_help_clicked <- function(widget,user.data)
{
showHelp("PlotsDots")
}

on_btn_DT_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsDots")
}

on_btn_DT_execute_clicked <- function(widget,user.data)
{

dataset <- getActiveData(getWidget("PlotsDots","cbx_DT_dataset"))
vars <- vectorToString(getListSelection(getWidget("PlotsDots","tvw_DT_variables")))
by <- getActiveData(getWidget("PlotsDots","cbx_DT_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsDots","txt_DT_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsDots","txt_DT_sub")$getText(),fixspc=FALSE)
colors <- vectorToString(getListViewData(getWidget("PlotsDots","tvw_DT_color"),2))
xaxis <- setLocale(getWidget("PlotsDots","txt_DT_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsDots","txt_DT_yaxis")$getText(),fixspc=FALSE)

# translate
cmd <- paste("grafico.pontos(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
runCommand(cmd)
closeWindow("PlotsDots","DT")
}
# operations
on_cbx_DT_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsDots","DT","numeric")
getWidget("PlotsDots","btn_DT_execute")$setSensitive(FALSE)
fillListView(getWidget("PlotsDots","tvw_DT_color"),NULL,TRUE)
}

on_tvw_DT_variables_button_release_event <- function(widget,event,user.data)
{
variables <- getListSelection(getWidget("PlotsDots","tvw_DT_variables"))
if(!is.null(variables))
	{
	colors <- genColors(length(variables))
	tvColor <- getWidget("PlotsDots","tvw_DT_color")
	fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
	toggleExecute("PlotsDots","DT")
	}
}
 
on_tvw_DT_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsDots","tvw_DT_color")
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsDots","Selecione a cor da linha",cur.color)
if(!is.null(color))
	model$set(iter,1,color)
}

on_chk_DT_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsDots","DT")
}