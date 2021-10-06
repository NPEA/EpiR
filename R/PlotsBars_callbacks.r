# PlotsBars callback functions

# main buttons

on_btn_PB_help_clicked <- function(widget,user.data)
{
showHelp("PlotsBars")
}

on_btn_PB_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsBars")
}

on_btn_PB_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsBars","cbx_PB_dataset"))
vars <- vectorToString(getListSelection(getWidget("PlotsBars","tvw_PB_variables")))
by <- getActiveData(getWidget("PlotsBars","cbx_PB_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsBars","txt_PB_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsBars","txt_PB_sub")$getText(),fixspc=FALSE)
colors <- vectorToString(getListViewData(getWidget("PlotsBars","tvw_PB_color"),2))
xaxis <- setLocale(getWidget("PlotsBars","txt_PB_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsBars","txt_PB_yaxis")$getText(),fixspc=FALSE)
lwd <- getWidget("PlotsBars","spb_PB_lwd")$getValue()
lty <- getActiveData(getWidget("PlotsBars","cbx_PB_lty"))
if(getWidget("PlotsBars","txt_PB_hline")$getText()!= "")
	{
	hline <- setLocale(getWidget("PlotsBars","txt_PB_hline")$getText(),fixspc=FALSE)
	hline <- as.numeric(hline)
	if(is.na(hline))
		hline <- "NULO"
	hlinecol <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
	}
else
	{
	hline <- "NULO"
	hlinecol <- "auto"
	}
if(getWidget("PlotsBars","chk_PB_legend")$getActive())
	legend <- VERDADEIRO
else
	legend <- FALSO
if(getWidget("PlotsBars","chk_PB_horiz")$getActive())
	horiz <- VERDADEIRO
else
	horiz <- FALSO

# translate
cmd <- paste("grafico.barras(\"",dataset,"\",\"",vars,"\",estratos=",by,",cor=c(\"",colors,"\"), cor.linha = \"",hlinecol,"\",titulo=\"",
title,"\",linha.horizontal=",hline,",linha=\"",lty,"\",espessura.linha=",lwd,",subtitulo=\"",subtitle,
"\",barras.horizontais=",horiz,",legenda=",legend,",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")

runCommand(cmd)
closeWindow("PlotsBars","PB")
}

# operations
on_cbx_PB_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsBars","PB","all")
getWidget("PlotsBars","btn_PB_execute")$setSensitive(FALSE)
fillListView(getWidget("PlotsBars","tvw_PB_color"),NULL,TRUE)
}

on_chk_PB_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsBars","PB")
}

on_tvw_PB_variables_button_release_event <- function(widget,event,user.data)
{
variables <- getListSelection(getWidget("PlotsBars","tvw_PB_variables"))
if(!is.null(variables))
	{
	colors <- genColors(length(variables))
	tvColor <- getWidget("PlotsBars","tvw_PB_color")
	fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
	toggleExecute("PlotsBars","PB")
	}
}
 
on_tvw_PB_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsBars","tvw_PB_color")
if(is.null(getListViewData(tvColor)))
	return()
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsBars","Selecione a cor da linha",cur.color)
if(!is.null(color))
	model$set(iter,1,color)
}

on_btn_PB_hlinecol_clicked <- function(widget,user.data)
{
cur.color <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
color <- colorDialog("PlotsBars","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	getWidget("PlotsBars","drw_PB_hlinecol")$modifyBg("normal",color)
	assign("EPIR_CURRENT_COLOR",color,envir=.EpiREnv)
	}
}

on_txt_PB_hline_changed <- function(widget,user.data)
{
toggleExecute("PlotsBars","PB",type="hline",button="hlinecol",widget="textedit")
}