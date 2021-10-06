on_btn_CGP_help_clicked <- function(widget,user.data)
{
showHelp("ControlCharts")
}

on_btn_CGP_cancel_clicked <- function(widget,user.data)
{
closeWindow("ControlCharts")
}

on_btn_CGP_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("ControlCharts","cbx_CGP_dataset"))
variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_var"))
date <- getActiveData(getWidget("ControlCharts","cbx_CGP_date"))
period <- getActiveData(getWidget("ControlCharts","cbx_CGP_period"))
month.variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_monthVariable"))
year.variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_yearVariable"))
frequency <- getActiveData(getWidget("ControlCharts","cbx_CGP_frequency"))
limit <- getWidget("ControlCharts","spb_CGP_limit")$getValue()
include.years <- getWidget("ControlCharts","chk_CGP_includeYears")$getActive()
if(include.years)
	include.years <- "VERDADEIRO"
else
	include.years <- "FALSO"
central.line <- getWidget("ControlCharts","chk_CGP_centralLine")$getActive()
if(central.line)
	central.line <- "VERDADEIRO"
else
	central.line <- "FALSO"
mean.graph <- getWidget("ControlCharts","rb_CGP_mean")$getActive()
if(mean.graph)
	method <- "media"
else
	method <- "mediana"

title <- setLocale(getWidget("ControlCharts","txt_CGP_main")$getText())
sub <- setLocale(getWidget("ControlCharts","txt_CGP_sub")$getText())
xlab <- setLocale(getWidget("ControlCharts","txt_CGP_xaxis")$getText())
ylab <- setLocale(getWidget("ControlCharts","txt_CGP_yaxis")$getText())
lty <- getActiveData(getWidget("ControlCharts","cbx_CGP_tline"))
size <- getWidget("ControlCharts","spb_CGP_size")$getValue()
line.color <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)

#translate
if(frequency == "diario")
	{
	if(method == "media")
		cmd <- paste("grafico.controle(\"",dataset,"\",variavel=\"",variable,"\",data=\"",date,"\",periodo.analise=\"",period,"\",metodo=\"",method ,"\",periodicidade=\"",frequency,"\",desvios.padrao=",limit,",periodos.futuros=",include.years,",linha.central=",central.line,",titulo=\"",title,"\",subtitulo=\"",sub,"\",rotulo.x=\"",xlab,"\",rotulo.y=\"",ylab,"\",linha=\"",lty,"\",espessura=",size,",cor.linha=\"",line.color,"\")",sep="")
	else
		cmd <- paste("grafico.controle(\"",dataset,"\",variavel=\"",variable,"\",data=\"",date,"\",periodo.analise=\"",period,"\",metodo=\"",method ,"\",periodicidade=\"",frequency,"\",periodos.futuros=",include.years,",linha.central=",central.line,",titulo=\"",title,"\",subtitulo=\"",sub,"\",rotulo.x=\"",xlab,"\",rotulo.y=\"",ylab,"\",linha=\"",lty,"\",espessura=",size,",cor.linha=\"",line.color,"\")",sep="")
	}
else
	{
	if(method == "media")
		cmd <- paste("grafico.controle(\"",dataset,"\",variavel=\"",variable,"\",variavel.mes=\"",month.variable,"\",variavel.ano=\"",year.variable,"\",periodo.analise=\"",period,"\",metodo=\"",method ,"\",periodicidade=\"",frequency,"\",desvios.padrao=",limit,",periodos.futuros=",include.years,",linha.central=",central.line,",titulo=\"",title,"\",subtitulo=\"",sub,"\",rotulo.x=\"",xlab,"\",rotulo.y=\"",ylab,"\",linha=\"",lty,"\",espessura=",size,",cor.linha=\"",line.color,"\")",sep="")
	else
		cmd <- paste("grafico.controle(\"",dataset,"\",variavel=\"",variable,"\",variavel.mes=\"",month.variable,"\",variavel.ano=\"",year.variable,"\",periodo.analise=\"",period,"\",metodo=\"",method ,"\",periodicidade=\"",frequency,"\",periodos.futuros=",include.years,",linha.central=",central.line,",titulo=\"",title,"\",subtitulo=\"",sub,"\",rotulo.x=\"",xlab,"\",rotulo.y=\"",ylab,"\",linha=\"",lty,"\",espessura=",size,",cor.linha=\"",line.color,"\")",sep="")
	}

runCommand(cmd)
closeWindow("ControlCharts", "CGP")
}

on_cbx_CGP_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("ControlCharts","cbx_CGP_dataset"))
if(dataset != "")
	{
	fillComboBox(getWidget("ControlCharts","cbx_CGP_var"),c("",getNumeric(dataset)))
	fillComboBox(getWidget("ControlCharts","cbx_CGP_date"),c("",getDate(dataset)))
	fillComboBox(getWidget("ControlCharts","cbx_CGP_monthVariable"),c("",getNames(dataset)))
	fillComboBox(getWidget("ControlCharts","cbx_CGP_yearVariable"),c("",getNames(dataset)))
	getWidget("ControlCharts","btn_CGP_hlinecol")$setSensitive(TRUE)
	if(dataset != "")
		getWidget("ControlCharts","btn_CGP_date")$setSensitive(TRUE)
	else
		getWidget("ControlCharts","btn_CGP_date")$setSensitive(FALSE)
	}
}

on_cbx_CGP_var_changed <- function(widget,user.data)
{
variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_var"))
date <- getActiveData(getWidget("ControlCharts","cbx_CGP_date"))
period <- getActiveData(getWidget("ControlCharts","cbx_CGP_period"))
frequency <- getActiveData(getWidget("ControlCharts","cbx_CGP_frequency"))
month <- getActiveData(getWidget("ControlCharts","cbx_CGP_monthVariable"))
if(frequency == "diario")
	{
	if(any(c(variable,date,period) == ""))
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
	else
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
	}
else
	{
	if(any(c(variable,period,month) == ""))
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
	else
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
	}
}

on_cbx_CGP_frequency_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("ControlCharts","cbx_CGP_dataset"))
frequency <- getActiveData(getWidget("ControlCharts","cbx_CGP_frequency"))
variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_var"))
month.variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_monthVariable"))
year.variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_yearVariable"))
period <- getActiveData(getWidget("ControlCharts","cbx_CGP_period"))
if(frequency != "diario" & frequency != "")
	{
	getWidget("ControlCharts","hbx_CGP_monthVariable")$setSensitive(TRUE)
	if(!getWidget("ControlCharts","hbx_CGP_yearVariable")$sensitive)
		fillComboBox(getWidget("ControlCharts","cbx_CGP_date"),c("",getDate(dataset)))
	getWidget("ControlCharts","hbx_CGP_yearVariable")$setSensitive(TRUE)
	getWidget("ControlCharts","hbx_CGP_dateVariable")$setSensitive(FALSE)
	if(month.variable != "" & year.variable != "" & variable != "" & period != "")
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
	else
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
	}
else
	{
	date <- getActiveData(getWidget("ControlCharts","cbx_CGP_date"))	
	if(dataset != "")
		{
		fillComboBox(getWidget("ControlCharts","cbx_CGP_monthVariable"),c("",getNames(dataset)))
		fillComboBox(getWidget("ControlCharts","cbx_CGP_yearVariable"),c("",getNames(dataset)))
		}
	getWidget("ControlCharts","hbx_CGP_monthVariable")$setSensitive(FALSE)
	getWidget("ControlCharts","hbx_CGP_yearVariable")$setSensitive(FALSE)
	getWidget("ControlCharts","hbx_CGP_dateVariable")$setSensitive(TRUE)
	if(frequency != "" & period != "" & date != "" & variable != "")
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
	else
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
	}
}

on_cbx_CGP_period_changed <- function(widget,user.data)
{
variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_var"))
period <- getActiveData(getWidget("ControlCharts","cbx_CGP_period"))
frequency <- getActiveData(getWidget("ControlCharts","cbx_CGP_frequency"))
month <- getActiveData(getWidget("ControlCharts","cbx_CGP_monthVariable"))

if(frequency == "diario")
	{
	if(any(c(variable,period) == ""))
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
	else
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
	}
else
	{
	if(any(c(variable,month,period) == ""))
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
	else
		getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
	}
}

on_btn_CGP_hlinecol_clicked <- function(widget,user.data)
{
cur.color <- get("EPIR_CURRENT_COLOR",envir=.EpiREnv)
color <- colorDialog("ControlCharts","Selecione a cor da linha",cur.color)
if(!is.null(color))
	{
	getWidget("ControlCharts","drw_CGP_hlinecol")$modifyBg("normal",color)
	assign("EPIR_CURRENT_COLOR",color,envir=.EpiREnv)
	}
}

on_cbx_CGP_date_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("ControlCharts","cbx_CGP_dataset"))
date <- getActiveData(getWidget("ControlCharts","cbx_CGP_date"))

if(date != "")
	{
	dates <- eval(parse(text = paste(dataset, "$",date,sep="")))
	try(dates <- format(dates,"%Y"),silent=T)
	if(class(dates) != "try-error")
		{
		dates <- sort(unique(dates))
		fillComboBox(getWidget("ControlCharts","cbx_CGP_period"),dates)
		}
	}
else
	fillComboBox(getWidget("ControlCharts","cbx_CGP_period"),c(""))
getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
}

on_btn_CGP_date_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("ControlCharts","cbx_CGP_dataset"))
setWindow("FormatDate","ControlCharts")
cbxDataset <- getWidget("FormatDate","cbx_FD_dataset")
fillComboBox(cbxDataset,c(dataset))
cbxDataset$setActive(0)
cbxDataset$SetSensitive(FALSE)
fillComboBox(getWidget("FormatDate","cbx_FD_datevar"),c("",getNames(dataset)))
fillComboBox(getWidget("FormatDate","cbx_FD_left"),EPIR_DATE_FORMAT)
fillComboBox(getWidget("FormatDate","cbx_FD_center"),EPIR_DATE_FORMAT)
fillComboBox(getWidget("FormatDate","cbx_FD_right"),EPIR_DATE_FORMAT)
fillComboBox(getWidget("FormatDate","cbx_FD_separator"),c("","/","-","."))
fillComboBox(getWidget("FormatDate","cbx_FD_origin"),c("","1900","2000"))
fillListView(getWidget("FormatDate","tvw_FD_preview"),NULL,update=FALSE)
}

on_cbx_CGP_yearVariable_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("ControlCharts","cbx_CGP_dataset"))
date <- getActiveData(getWidget("ControlCharts","cbx_CGP_yearVariable"))
frequency <- getActiveData(getWidget("ControlCharts","cbx_CGP_frequency"))
if(frequency != "diario" & frequency != "")
	{
	if(date != "")
		{
		dates <- eval(parse(text = paste(dataset, "$",date,sep="")))
		dates <- sort(unique(dates))
		fillComboBox(getWidget("ControlCharts","cbx_CGP_period"),dates)
		}
	else
		fillComboBox(getWidget("ControlCharts","cbx_CGP_period"),c(""))
	}
}

on_cbx_CGP_monthVariable_changed <- function(widget,user.data)
{
monthVariable <- getActiveData(getWidget("ControlCharts","cbx_CGP_monthVariable"))
variable <- getActiveData(getWidget("ControlCharts","cbx_CGP_var"))
period <- getActiveData(getWidget("ControlCharts","cbx_CGP_period"))
if(any(c(monthVariable,variable,period) == ""))
	getWidget("ControlCharts","btn_CGP_execute")$setSensitive(FALSE)
else
	getWidget("ControlCharts","btn_CGP_execute")$setSensitive(TRUE)
}

on_rb_CGP_mean_toggled <- function(widget,user.data)
{
mean.method <- getWidget("ControlCharts","rb_CGP_mean")$getActive()
if(mean.method)
	getWidget("ControlCharts","hbx_CGP_sd")$setSensitive(TRUE)
else
	getWidget("ControlCharts","hbx_CGP_sd")$setSensitive(FALSE)
}