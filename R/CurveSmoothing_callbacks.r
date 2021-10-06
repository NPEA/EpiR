# CurveSmoothing callback functions

# main buttons
on_btn_CS_help_clicked <- function(widget,user.data)
{
showHelp("CurveSmoothing")
}

on_btn_CS_cancel_clicked <- function(widget,user.data)
{
closeWindow("CurveSmoothing")
}

on_btn_CS_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CurveSmoothing","cbx_CS_dataset"))
x <-  getActiveData(getWidget("CurveSmoothing","cbx_CS_xaxis"))
y <-  getActiveData(getWidget("CurveSmoothing","cbx_CS_yaxis"))
method <- getActiveData(getWidget("CurveSmoothing","cbx_CS_type"))
sp <- getWidget("CurveSmoothing","spb_CS_df")$getValue()
title <- setLocale(getWidget("CurveSmoothing","txt_CS_main")$getText())
subtitle <- setLocale(getWidget("CurveSmoothing","txt_CS_sub")$getText())
xaxis <- setLocale(getWidget("CurveSmoothing","txt_CS_xaxis")$getText())
yaxis <- setLocale(getWidget("CurveSmoothing","txt_CS_yaxis")$getText())
lty <- getActiveData(getWidget("CurveSmoothing","cbx_CS_tline"))
lwd <- getWidget("CurveSmoothing","spb_CS_size")$getValue()
linecol <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
object <- setLocale(getWidget("CurveSmoothing","txt_CS_object")$getText())
if(method == "regressao_local")
sp <- sp/100
if(object == "")
	object <- "NULO"

# translate
if(object == "NULO")
{
cmd <- paste("suavizar.curva(\"",dataset,"\"",",\"",x,"\"",",\"",y,"\",metodo=\"",method,"\",objeto=",object,",ps=,",sp,",linha=\"",lty,"\",tamanho=",lwd,",cor.linha=\"",linecol,"\",titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
}
else
{
cmd <- paste("suavizar.curva(\"",dataset,"\"",",\"",x,"\"",",\"",y,"\",metodo=\"",method,"\",objeto=\"",object,"\",ps=,",sp,",linha=\"",lty,"\",tamanho=",lwd,",cor.linha=\"",linecol,"\",titulo=\"",title,"\",subtitulo=\"",subtitle,"\",rotulo.x=\"",xaxis,"\",rotulo.y=\"",yaxis,"\")",sep="")
}
runCommand(cmd)
closeWindow("CurveSmoothing","CS")
}


# operations
on_cbx_CS_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CurveSmoothing","cbx_CS_dataset"))
fillComboBox(getWidget("CurveSmoothing","cbx_CS_yaxis"),c("",getNumeric(dataset)))
names.x.axis <- c(getNumeric(dataset),getDate(dataset))
fillComboBox(getWidget("CurveSmoothing","cbx_CS_xaxis"),c("",names.x.axis))
getWidget("CurveSmoothing","cbx_CS_type")$setActive(0)
getWidget("CurveSmoothing","cbx_CS_tline")$setActive(0)
getWidget("CurveSmoothing","btn_CS_execute")$setSensitive(FALSE)
getWidget("CurveSmoothing","btn_CS_hlinecol")$setSensitive(FALSE)
}


on_cbx_CS_yaxis_changed <- function(widget,user.data)
{
if(getActiveData(getWidget("CurveSmoothing","cbx_CS_xaxis"))!="" && getActiveData(getWidget("CurveSmoothing","cbx_CS_yaxis")) !="")
	{
	getWidget("CurveSmoothing","btn_CS_execute")$setSensitive(TRUE)
	getWidget("CurveSmoothing","btn_CS_hlinecol")$setSensitive(TRUE)
	}
}

on_cbx_CS_xaxis_changed <- function(widget,user.data)
{
if(getActiveData(getWidget("CurveSmoothing","cbx_CS_yaxis"))!=""&& getActiveData(getWidget("CurveSmoothing","cbx_CS_xaxis"))!="")
	{
	getWidget("CurveSmoothing","btn_CS_execute")$setSensitive(TRUE)
	getWidget("CurveSmoothing","btn_CS_hlinecol")$setSensitive(TRUE)
	}
}

on_chk_CS_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("CurveSmoothing","CS")
}

on_btn_CS_hlinecol_clicked <- function(widget,user.data)
{
cur.color <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
color <- colorDialog("CurveSmoothing","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	getWidget("CurveSmoothing","drw_CS_hlinecol")$modifyBg("normal",color)
	assign("EPIR_CURRENT_COLOR",color,envir=.EpiREnv)
	}
}