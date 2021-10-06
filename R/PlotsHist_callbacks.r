# plotshist callback functions

# main buttons
on_btn_PH_help_clicked <- function(widget,user.data)
{
showHelp("PlotsHist")
}

on_btn_PH_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsHist")
}

on_btn_PH_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsHist","cbx_PH_dataset"))
vars <- vectorToString(getListSelection(getWidget("PlotsHist","tvw_PH_variables")))
by <- getActiveData(getWidget("PlotsHist","cbx_PH_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsHist","txt_PH_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsHist","txt_PH_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsHist","txt_PH_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsHist","txt_PH_yaxis")$getText(),fixspc=FALSE)
nbreaks <- getWidget("PlotsHist","spb_PH_nbreaks")$getValue()
colors <- vectorToString(getListViewData(getWidget("PlotsHist","tvw_PH_color"),2))
if(getWidget("PlotsHist","chk_PH_frequency")$getActive())
	frequency <- "VERDADEIRO"
else
	frequency <- "FALSO"

# translate
cmd <- paste("grafico.histograma(\"",dataset,"\",\"",vars,"\",estratos=",by,",barras=",nbreaks,",frequencia=",frequency,",cor=c(\"",colors,"\"),titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
runCommand(cmd)
closeWindow("PlotsHist","PH")
}


# operations
on_cbx_PH_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsHist","PH","numeric",plot=FALSE)
getWidget("PlotsHist","btn_PH_execute")$setSensitive(FALSE)
fillListView(getWidget("PlotsHist","tvw_PH_color"),NULL,TRUE)
}

on_tvw_PH_variables_button_release_event <- function(widget,event,user.data)
{
variables <- getListSelection(getWidget("PlotsHist","tvw_PH_variables"))
if(!is.null(variables))
	{
	colors <- genColors(length(variables))
	tvColor <- getWidget("PlotsHist","tvw_PH_color")
	fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
	toggleExecute("PlotsHist","PH")
	}
}

on_tvw_PH_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsHist","tvw_PH_color")
if(is.null(getListViewData(tvColor)))
	return()
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsHist","Selecione a cor da linha",cur.color)
if(!is.null(color))
	model$set(iter,1,color)
}

on_chk_PH_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsHist","PH")
}