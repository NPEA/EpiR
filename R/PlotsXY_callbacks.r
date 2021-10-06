# plotsXy callback functions

# main buttons
on_btn_PXY_help_clicked <- function(widget,user.data)
{
showHelp("PlotsXY")
}

on_btn_PXY_cancel_clicked <- function(widget,user.data)
{
closeWindow("PlotsXY")
}

on_btn_PXY_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("PlotsXY","cbx_PXY_dataset"))
pairs <- vectorToString(get("EPIR_SELECTED_GRAPHS",envir=.EpiREnv))
by <- getActiveData(getWidget("PlotsXY","cbx_PXY_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
title <- setLocale(getWidget("PlotsXY","txt_PXY_main")$getText(),fixspc=FALSE)
subtitle <- setLocale(getWidget("PlotsXY","txt_PXY_sub")$getText(),fixspc=FALSE)
xaxis <- setLocale(getWidget("PlotsXY","txt_PXY_xaxis")$getText(),fixspc=FALSE)
yaxis <- setLocale(getWidget("PlotsXY","txt_PXY_yaxis")$getText(),fixspc=FALSE)
pch <- getActiveData(getWidget("PlotsXY","cbx_PXY_pch"))
size <- getWidget("PlotsXY","spb_PXY_size")$getValue()
colors <- vectorToString(getListViewData(getWidget("PlotsXY","tvw_PXY_color"),2))
overlay <- getActiveData(getWidget("PlotsXY","cbx_PXY_overlay"))
if(getWidget("PlotsXY","txt_PXY_hline")$getText()!="")
	{
	hline <- getWidget("PlotsXY","txt_PXY_hline")$getText()
	hlinecol <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
	}
else
	{
	hline <- "NULO"  # must be translated
	hlinecol <- "auto"
	}
if(getWidget("PlotsXY","chk_PXY_legend")$getActive())
	{
	legend <- "VERDADEIRO"
	if(getWidget("PlotsXY","chk_PXY_legend")$getActive())
		hlegend <- "VERDADEIRO"
	else
		hlegend <- "FALSO"
	legendpos <- getActiveData(getWidget("PlotsXY","cbx_PXY_legendpos"))
	}
else
	{
	legend <- "FALSO"
	hlegend <- "FALSO"
	legendpos <- "alto_direito"
	}

# translate
cmd <- paste("grafico.dispersao(\"",dataset,"\",\"",pairs,"\",estratos=",by,",cor=c(\"",colors,"\"),ponto=\"",pch,"\",tamanho=",size,",sobrepor=\"",overlay,"\",linha.horizontal=c(",hline,"),cor.linha.horizontal=\"",hlinecol,"\",titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\",legenda=",legend,",posicao.legenda=\"",legendpos,"\",legenda.horizontal=",hlegend,")",sep="")
runCommand(cmd)
closeWindow("PlotsXY","PXY")
}


# operations
on_cbx_PXY_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("PlotsXY","PXY","numeric",plot=TRUE,doubleaxis=TRUE)
getWidget("PlotsXY","btn_PXY_execute")$setSensitive(FALSE)
fillListView(getWidget("PlotsXY","tvw_PXY_color"),NULL,TRUE)
# workaround for eliminating the variables overlay
overlay <- getWidget("PlotsXY","cbx_PXY_overlay")
fillComboBox(overlay,data=get("EPIR_PLOT_OVERLAY_OPTIONS",env=.EpiREnv)[c(-2,-3)],TRUE,FALSE)
overlay$setActive(0)
}

on_cbx_PXY_by_changed <- function(widget,user.data)
{
overlay <- getWidget("PlotsXY","cbx_PXY_overlay")
by <- getActiveData(getWidget("PlotsXY","cbx_PXY_by"))
if(by=="")
	{
	fillComboBox(overlay,data=get("EPIR_PLOT_OVERLAY_OPTIONS",env=.EpiREnv)[-c(2,3)],TRUE,FALSE)
	overlay$setActive(0)
	}
else
	{
	fillComboBox(overlay,data=get("EPIR_PLOT_OVERLAY_OPTIONS",env=.EpiREnv)[-3],TRUE,FALSE)
	overlay$setActive(0)
	}
}

on_tvw_PXY_variables_button_release_event <- function(widget,event,user.data)
{
if(!is.null(getListSelection(getWidget("PlotsXY","tvw_PXY_variables"))) & !is.null(getListSelection(getWidget("PlotsXY","tvw_PXY_variablesx"))))
	getWidget("PlotsXY","btn_PXY_add")$setSensitive(TRUE)
else
	getWidget("PlotsXY","btn_PXY_add")$setSensitive(FALSE)
}

on_tvw_PXY_variablesx_button_release_event <- function(widget,event,user.data)
{
if(!is.null(getListSelection(getWidget("PlotsXY","tvw_PXY_variables"))) & !is.null(getListSelection(getWidget("PlotsXY","tvw_PXY_variablesx"))))
	getWidget("PlotsXY","btn_PXY_add")$setSensitive(TRUE)
else
	getWidget("PlotsXY","btn_PXY_add")$setSensitive(FALSE)
}

on_btn_PXY_add_clicked <- function(widget,user.data)
{
tvwVariabley <- getWidget("PlotsXY","tvw_PXY_variables")
tvwVariablex <- getWidget("PlotsXY","tvw_PXY_variablesx")
variabley <- getListSelection(tvwVariabley)
variablex <- getListSelection(tvwVariablex)
tvColor <- getWidget("PlotsXY","tvw_PXY_color")
newpair <- paste(variabley,"~",variablex,sep="")
pairs <- c(getListViewData(tvColor),newpair)
pairs <- pairs[pairs != "NULL"]
colors <- genColors(length(pairs))
assign("EPIR_SELECTED_GRAPHS",pairs,envir=.EpiREnv)
fillColListView(tvColor,pairs,colors,update=TRUE,headers=c("Gráfico","Cor"))
toggleExecute("PlotsXY","PXY",type="color",button="execute",selection=FALSE)
tvwVariabley$getSelection()$unselectAll()    
tvwVariablex$getSelection()$unselectAll()    
getWidget("PlotsXY","btn_PXY_add")$setSensitive(FALSE)
toggleExecute("PlotsXY","PXY",type="color",button="execute",selection=FALSE)
getWidget("PlotsXY","cbx_PXY_overlay")$setSensitive(TRUE)
}

on_btn_PXY_remove_clicked <- function(widget,user.data)
{
removeSelectedRows(getWidget("PlotsXY","tvw_PXY_color"))
toggleExecute("PlotsXY","PXY",type="color",button="remove")
toggleExecute("PlotsXY","PXY",type="color",button="execute",selection=FALSE)
getWidget("PlotsXY","cbx_PXY_overlay")$setSensitive(FALSE)
}

on_cbx_PXY_overlay_changed <- function(widget,user.data)
{
overlay <- getWidget("PlotsXY","cbx_PXY_overlay")$getActiveText()
if(!is.null(overlay) & getWidget("PlotsXY","cbx_PXY_overlay")$sensitive)
	{
	tvColor <- getWidget("PlotsXY","tvw_PXY_color")
	if(is.null(getListViewData(tvColor)))
		return()
	pairs <- getListViewData(tvColor)
	dataset <- getActiveData(getWidget("PlotsXY","cbx_PXY_dataset"))
	if(!is.null(dataset))
		dataset <- eval(parse(text=dataset),envir=.GlobalEnv)
	if(!is.null(overlay))
		{
		if(overlay=="niveis")
			{
			by <- getActiveData(getWidget("PlotsXY","cbx_PXY_by"))
			if(by!="")
				{
				assign("EPIR_SELECTED_GRAPHS",pairs,envir=.EpiREnv)
				by <- eval(parse(text=by),envir=dataset)
				if(!is.factor(by))
					by <- as.factor(by)
				levels <- levels(by)
				colors <- genColors(length(levels))
				fillColListView(tvColor,levels,colors,update=TRUE,headers=c("Nível","Cor"))
				}
			}
		else
			{
			pairs <- get("EPIR_SELECTED_GRAPHS",envir=.EpiREnv)
			colors <- genColors(length(pairs))
			fillColListView(tvColor,pairs,colors,update=TRUE,headers=c("Gráfico","Cor"))
			}
		}
	if(!is.null(overlay))
		{
		if(overlay != "nenhum")
			getWidget("PlotsXY", "exp_PXY_legend")$setSensitive(TRUE)
		else
			getWidget("PlotsXY", "exp_PXY_legend")$setSensitive(FALSE)
		}
	}
}

on_tvw_PXY_color_row_activated <- function(widget,path,column,user.data)
{
tvColor <- getWidget("PlotsXY","tvw_PXY_color")
model <-tvColor$getModel()
iter <- model$getIter(path)$iter
cur.color <- model$get(iter,1)[[1]]
color <- colorDialog("PlotsXY","Selecione a cor da linha",cur.color)
if(!is.null(color))
	model$set(iter,1,color)
}

on_tvw_PXY_color_button_release_event <- function(widget,event,user.data)
{
toggleExecute("PlotsXY","PXY",type="color",button="remove")
}

on_btn_PXY_hlinecol_clicked <- function(widget,user.data)
{
cur.color <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
color <- colorDialog("PlotsXY","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	getWidget("PlotsXY","drw_PXY_hlinecol")$modifyBg("normal",color)
	assign("EPIR_CURRENT_COLOR",color,envir=.EpiREnv)
	}
}

on_chk_PXY_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("PlotsXY","PXY")
}

on_txt_PXY_hline_changed <- function(widget,user.data)
{
toggleExecute("PlotsXY","PXY",type="hline",button="hlinecol",widget="textedit")
}

on_chk_PXY_legend_toggled <- function(widget,user.data)
{
toggleLegend("PlotsXY","PXY")
}