# plotsX callback functions

# main buttons
on_btn_PX_help_clicked <- function(widget,user.data)
{
showHelp("PlotsX")
}

on_btn_PX_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsX")
}

on_btn_PX_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsX","cbx_PX_dataset"))
x <- getActiveData(getWidget("PlotsX","cbx_PX_xaxis"))
if(x=="")
	{
	msgDialog("PlotsX","error","É necessário especificar a variável do eixo X.")
	return()
	}
vars <- vectorToString(getListSelection(getWidget("PlotsX","tvw_PX_variables")))
by <- getActiveData(getWidget("PlotsX","cbx_PX_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsX","txt_PX_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsX","txt_PX_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsX","txt_PX_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsX","txt_PX_yaxis")$getText(),fixspc=FALSE)
type <- getActiveData(getWidget("PlotsX","cbx_PX_type"))
lty <- getActiveData(getWidget("PlotsX","cbx_PX_lty"))
pch <- getActiveData(getWidget("PlotsX","cbx_PX_pch"))
size <- getWidget("PlotsX","spb_PX_size")$getValue()
colors <- vectorToString(getListViewData(getWidget("PlotsX","tvw_PX_color"),2))
overlay <- getActiveData(getWidget("PlotsX","cbx_PX_overlay"))
if(getWidget("PlotsX","txt_PX_hline")$getText()!="")
	{
	hline <- getWidget("PlotsX","txt_PX_hline")$getText()
	hlinecol <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
	}
else
	{
	hline <- "NULO"  # must be translated
	hlinecol <- "auto"
	}
if(getWidget("PlotsX","chk_PX_legend")$getActive())
	{
	legend <- "VERDADEIRO"
	if(getWidget("PlotsX","chk_PX_legend")$getActive())
		hlegend <- "VERDADEIRO"
	else
		hlegend <- "FALSO"
	legendpos <- getActiveData(getWidget("PlotsX","cbx_PX_legendpos"))
	}
else
	{
	legend <- "FALSO"
	hlegend <- "FALSO"
	legendpos <- "alto_direito"
	}

# translate
cmd <- paste("grafico.simples(\"",dataset,"\",\"",x,"\",\"",vars,"\",estratos=",by,",tipo=\"",type,"\",cor=c(\"",colors,"\"),linha=\"",lty,"\",ponto=\"",pch,"\",tamanho=",size,",sobrepor=\"",overlay,"\",linha.horizontal=c(",hline,"),cor.linha.horizontal=\"",hlinecol,"\",titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\",legenda=",legend,",posicao.legenda=\"",legendpos,"\",legenda.horizontal=",hlegend,")",sep="")
runCommand(cmd)
closeWindow("PlotsX","PX")
}


# operations
on_cbx_PX_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsX","PX","numeric",plot=TRUE)
getWidget("PlotsX","btn_PX_execute")$setSensitive(FALSE)
# workaroud for overlaying
overlay <- getWidget("PlotsX","cbx_PX_overlay")
fillComboBox(overlay,data=get("EPIR_PLOT_OVERLAY_OPTIONS",env=.EpiREnv)[-2],TRUE,FALSE)
overlay$setActive(0)
fillListView(getWidget("PlotsX","tvw_PX_color"),NULL,TRUE)
}

on_cbx_PX_by_changed <- function(widget,user.data)
{
overlay <- getWidget("PlotsX","cbx_PX_overlay")
by <- getActiveData(getWidget("PlotsX","cbx_PX_by"))
if(by=="")
	{
	fillComboBox(overlay,data=get("EPIR_PLOT_OVERLAY_OPTIONS",env=.EpiREnv)[-2],TRUE,FALSE)
	overlay$setActive(0)
	}
else
	{
	fillComboBox(overlay,data=get("EPIR_PLOT_OVERLAY_OPTIONS",env=.EpiREnv),TRUE,FALSE)
	overlay$setActive(0)
	}
}

on_cbx_PX_type_changed <- function(widget,user.data)
{
type <- getWidget("PlotsX","cbx_PX_type")
pch <- getWidget("PlotsX","cbx_PX_pch")
lty <- getWidget("PlotsX","cbx_PX_lty")
if(getActiveData(type)=="ponto")
	{
	pch$setSensitive(TRUE)
	lty$setSensitive(FALSE)
	}
else if(getActiveData(type)=="ponto_linha")
	{
	pch$setSensitive(TRUE)
	lty$setSensitive(TRUE)
	}
else
	{
	pch$setSensitive(FALSE)
	lty$setSensitive(TRUE)
	}
}

on_tvw_PX_variables_button_release_event <- function(widget,event,user.data)
{
variables <- getListSelection(getWidget("PlotsX","tvw_PX_variables"))
if(!is.null(variables))
	{
	colors <- genColors(length(variables))
	tvColor <- getWidget("PlotsX","tvw_PX_color")
	fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
	toggleExecute("PlotsX","PX")
	}
}

on_cbx_PX_overlay_changed <- function(widget,user.data)
{
tvColor <- getWidget("PlotsX","tvw_PX_color")
if(is.null(getListViewData(tvColor)))
	return()
overlay <- getWidget("PlotsX","cbx_PX_overlay")$getActiveText()
variables <- getListSelection(getWidget("PlotsX","tvw_PX_variables"))
dataset <- getActiveData(getWidget("PlotsX","cbx_PX_dataset"))
if(!is.null(dataset))
	dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
if(!is.null(overlay))
	{
	if(overlay=="niveis")
		{
		getWidget("PlotsX","exp_PX_legend")$setSensitive(TRUE)
		by <- getActiveData(getWidget("PlotsX","cbx_PX_by"))
		if(by!="")
			{
			by <- eval(parse(text=by),envir=dataset)
			if(!is.factor(by))
				by <- as.factor(by)
			levels <- levels(by)
			colors <- genColors(length(levels))
			fillColListView(tvColor,levels,colors,update=TRUE,headers=c("Nível","Cor"))
			}
		}
	else if(overlay=="variaveis")
		{
		colors <- genColors(length(variables))
		fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
		getWidget("PlotsX","exp_PX_legend")$setSensitive(TRUE)
		}
	else
		{
		colors <- genColors(length(variables))
		fillColListView(tvColor,variables,colors,update=TRUE,headers=c("Variável","Cor"))
		getWidget("PlotsX","chk_PX_legend")$setActive(FALSE)
		getWidget("PlotsX","exp_PX_legend")$setSensitive(FALSE)
		}
	}
}

on_tvw_PX_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsX","tvw_PX_color")
if(is.null(getListViewData(tvColor)))
	return()
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsX","Selecione a cor da linha",cur.color)
if(!is.null(color))
	model$set(iter,1,color)
}

on_btn_PX_hlinecol_clicked <- function(widget,user.data)
{
cur.color <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
color <- colorDialog("PlotsX","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	getWidget("PlotsX","drw_PX_hlinecol")$modifyBg("normal",color)
	assign("EPIR_CURRENT_COLOR",color,envir=.EpiREnv)
	}
}

on_chk_PX_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsX","PX")
}

on_txt_PX_hline_changed <- function(widget,user.data)
{
toggleExecute("PlotsX","PX",type="hline",button="hlinecol",widget="textedit")
}

on_chk_PX_legend_toggled <- function(widget,user.data)
{
toggleLegend("PlotsX","PX")
}

