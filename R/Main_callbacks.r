# main windows callback functions

# main menu
on_mnChangeWorkingDir_activate <- function(widget,user.data)
{
setWindow("ChangeWorkingDir")
getWidget("ChangeWorkingDir","lbl_CWD_currentdir")$setMarkup(paste("<b>",getwd(),"</b>",sep=""))
}

on_mnOpenData_activate <- function(widget,user.data)
{
setWindow("OpenData")
fillListView(getWidget("OpenData","tvw_OD_viewdataset"),NULL,update=FALSE)
}

on_mnSaveData_activate <- function(widget,user.data)
{
selected <- getListSelection(getWidget("Main","tvwObjects"))
objects <- vectorToString(selected)
if(!is.null(objects))
	{
	filename <- fileDialog("Main","Salvar arquivo de dados","save",list(type=c("Arquivos R"),pat=c("*.rda")))$filename
	if (!is.null(filename))
		{
		# translate
		cmd <- paste("salvar.dados(\"",objects,"\",arquivo=\"",filename,"\")",sep="")
		runCommand(cmd)
		getWidget("Main","mnSaveData")$setSensitive(FALSE)
		getWidget("Main","btnSaveData")$setSensitive(FALSE)
		}
	}
}

on_mnConnectODBC_activate <- function(widget,user.data)
{
setWindow("ODBCConnect")
# fill the case combo box
fillComboBox(getWidget("ODBCConnect","cbx_OC_case"),data=EPIR_RODBC_CASE,FALSE)
getWidget("ODBCConnect","txt_OC_password")$setVisibility(FALSE)
getWidget("ODBCConnect","txt_OC_password")$setInvisibleChar(0x002A)
clearTv(getWidget("ODBCConnect","txt_OC_sql"))
}

on_mnSaveOutput_activate <- function(widget,user.data)
{
filename <- fileDialog("Main","Salvar resultados","save",list(type=c("Arquivos Texto","Todos os arquivos"),pat=c("*.txt","*")))$filename
if (!is.null(filename))
	{
	# translate
	cmd <- paste("salvar.resultados(\"",filename,"\")",sep="")
	runCommand(cmd)
	}
}

on_mnLoadOutput_activate <- function(widget,user.data)
{
filename <- fileDialog("Main","Carregar resultados","open",list(type=c("Arquivos Texto","Todos os arquivos"),pat=c("*.txt","*")))$filename
if (!is.null(filename))
	{
	# translate
	cmd <- paste("carregar.resultados(\"",filename,"\")",sep="")
	runCommand(cmd)
	}
}

on_mnPrintOutput_activate <- function(widget,user.data)
{
filename <- paste(tempdir(),"/","EpiR_",EPIR_USER,"_",cleanStr(Sys.time()), sep="")
save.output(filename) # untill better solution
operation <- gtkPrintOperation()
data <- list()
data$filename <- filename
data$font_size <- 10.0
operation$setData("print_data",data)

gSignalConnect(operation,"begin-print",begin_print_txt)
gSignalConnect(operation,"draw-page",draw_page_txt)

result <- operation$run("print-dialog",NULL)
if (!is.null(result$error))
	msgDialog("Main","error",result$error$message)
if(file.exists(filename))
	file.remove(filename)
}

on_mnSaveHistory_activate <- function(widget,user.data)
{
history <- get("EPIR_TEXT_HISTORY",envir=.EpiREnv)
if(length(history)>0)
	{
	filename <- fileDialog("Main","Salvar histórico","save",list(type=c("Arquivos de histórico","Todos os arquivos"),pat=c("*.his","*")))$filename
	if (!is.null(filename))
		save(history,file=filename)
	}
}

on_mnLoadHistory_activate <- function(widget,user.data)
{
filename <- fileDialog("Main","Carregar histórico","open",list(type=c("Arquivos de histórico","Todos os arquivos"),pat=c("*.his","*")))$filename
if (!is.null(filename))
	{
	history <- try(get(load(filename)))
	assign("EPIR_TEXT_HISTORY",history,envir=.EpiREnv)
	fillListView(getWidget("Main","tvwHistory"),history)
	}
}

on_mnExportHistory_activate <- function(widget,user.data)
{
history <- rev(get("EPIR_TEXT_HISTORY",envir=.EpiREnv))
if(length(history)>0)
	{
	filename <- fileDialog("Main","Salvar histórico com script","save",list(type=c("Arquivos de script","Todos os arquivos"),pat=c("*.r","*")))$filename
	if (!is.null(filename))
		{
		# outputing to the file
		file <- file(filename, "w")
		cat(history,file=file,sep="\n")
		close(file)
		}
	}
}

on_mnQuit_activate <- function(widget,user.data)
{
if(askDialog("Main","Deseja realmente sair do aplicativo?\nTodos os dados da área de trabalho serão perdidos.")=="yes")
	{
	closeWindow("Main")
	try(get("trayIcon",envir=.EpiREnv)$setVisible(FALSE),silent=TRUE)
	unload(hard=TRUE)
	}
}

on_mnCopy_activate <- function(widget,user.data)
{
if(getWidget("Main","txtOutput")$isFocus())
	copyToClipboard("Main","txtOutput")
else if(getWidget("Main","txtCommand")$isFocus())
	copyToClipboard("Main","txtCommand")
else if(getWidget("Main","tvwHistory")$isFocus())
	copyToClipboard("Main","tvwHistory")
else if(getWidget("Main","tvwObjects")$isFocus())
	copyToClipboard("Main","tvwObjects")
else if(getWidget("Main","tvwVariables")$isFocus())
	copyToClipboard("Main","tvwVariables")
}

on_mnPaste_activate <- function(widget,user.data)
{
pasteFromClipboard("Main","txtCommand")
}

on_mnRefresh_activate <- function(widget,user.data)
{
updateMainLists()
}

on_mnClearHistory_activate <- function(widget,user.data)
{
if(askDialog("Main","Deseja realmente apagar todas as entradas do histórico?")=="yes")
	{
	assign("EPIR_TEXT_HISTORY",NULL,envir=.EpiREnv)
	fillListView(getWidget("Main","tvwHistory"),NULL)
	}
}

on_mnClearOutput_activate <- function(widget,user.data)
{
if(askDialog("Main","Deseja realmente apagar a janela de resultados?")=="no")
	return()
txtOutput <- getWidget("Main","txtOutput")
clearTv(txtOutput)
welcomeTv(txtOutput)
}

on_mnEditData_activate <- function(widget,user.data)
{
setWindow("EditData")
fillStatsData("EditData","ED",varlist=FALSE)
fillListView(getWidget("EditData","tvw_ED_dataframe"),NULL,FALSE)
}

on_mnDataOperations_activate <- function(widget,user.data)
{
setWindow("DataOperations")
fillStatsData("DataOperations","DO",varlist=FALSE)
fillListView(getWidget("DataOperations","tvw_DO_operations"),NULL,FALSE,sel.mode="single")
}

on_mnFormatDate_activate <- function(widget,user.data)
{
setWindow("FormatDate")
fillStatsData("FormatDate","FD",FALSE)
fillComboBox(getWidget("FormatDate","cbx_FD_left"),EPIR_DATE_FORMAT)
fillComboBox(getWidget("FormatDate","cbx_FD_center"),EPIR_DATE_FORMAT)
fillComboBox(getWidget("FormatDate","cbx_FD_right"),EPIR_DATE_FORMAT)
fillComboBox(getWidget("FormatDate","cbx_FD_separator"),c("","/","-","."))
fillComboBox(getWidget("FormatDate","cbx_FD_origin"),c("1900","2000"),TRUE,TRUE)
fillListView(getWidget("FormatDate","tvw_FD_preview"),NULL,update=FALSE)
}

on_mnDropColumns_activate <- function(widget,user.data)
{
setWindow("DropColumns")
fillStatsData("DropColumns","DC")
}

on_mnSelectRows_activate <- function(widget,user.data)
{
setWindow("SelectRows")
fillStatsData("SelectRows","SR",varlist=FALSE)
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
fillListView(getWidget("SelectRows","tvw_SR_variables"),NULL,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
}

on_mnAggregateRow_activate <- function(widget,user.data)
{
setWindow("AggregateRows")
fillStatsData("AggregateRows","AR",varlist=FALSE)
fillComboBox(getWidget("AggregateRows","cbx_AR_function"),c("média","soma","frequencia"),TRUE,TRUE)
}

on_mnRecodeVar_activate <- function(widget,user.data)
{
setWindow("RecodeVar")
fillStatsData("RecodeVar","RV",varlist=FALSE)
fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"),NULL,FALSE)
fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),NULL,FALSE)
}

on_mnMerge_activate <- function(widget,user.data)
{
setWindow("MergeDataset")
fillComboBox(getWidget("MergeDataset","cbx_MD_dataset"),getDataset(),FALSE)
fillComboBox(getWidget("MergeDataset","cbx_MD_newDataset"),getDataset(),FALSE)
fillListView(getWidget("MergeDataset","tvw_MD_variables1"),NULL,FALSE)
fillListView(getWidget("MergeDataset","tvw_MD_variables2"),NULL,FALSE)
}

on_mnRemoveData_activate <- function(widget,user.data)
{
setWindow("RemoveData")
fillListView(getWidget("RemoveData","tvw_RD_variables"),ls(envir=.GlobalEnv),FALSE)  # fill Objects list in the window
}

on_mnClearAll_activate <- function(widget,user.data)
{
if(askDialog("Main","Deseja realmente apagar todos os objetos da área de trabalho?")=="no")
	return()
cmd <- paste("remover.todos()")
runCommand(cmd)
}

on_mnManageTrash_activate <- function(widget,user.data)
{
setWindow("ManageTrash")
fillListView(getWidget("ManageTrash","tvw_MT_variables"),ls(envir=.EpiRTrash),FALSE)  # fill Objects list in the window
}

on_mnBriefSummary_activate <- function(widget,user.data)
{
setWindow("BriefSummary")
fillStatsData("BriefSummary","BS")
}

on_mnSummaryMeasures_activate <- function(widget,user.data)
{
setWindow("SummaryMeasures")
fillStatsData("SummaryMeasures","SM")
}

on_mnFrequencyTables_activate <- function(widget,user.data)
{
setWindow("FrequencyTables")
fillStatsData("FrequencyTables","FT")
}

on_mnCrossTables_activate <- function(widget, user.data)
{
setWindow("CrossTables")
fillStatsData("CrossTables","CT",varlist=FALSE)
fillComboBox(getWidget("CrossTables","cbx_CT_relative_margin"),EPIR_TABLE_MARGINALS)
fillComboBox(getWidget("CrossTables","cbx_CT_total"),EPIR_TABLE_MARGINALS)
}

on_mnStemLeaf_activate <- function(widget,user.data)
{
setWindow("StemLeaf")
fillStatsData("StemLeaf","SL")
}

on_mnCorrelation_activate <- function(widget,user.data)
{
setWindow("Correlation")
fillListView(getWidget("Correlation","tvw_CO_variables"),NULL,FALSE)
fillComboBox(getWidget("Correlation","cbx_CO_dataset"),c("",getDataset()),TRUE)
}

on_mnLinearRegression_activate <- function(widget,user.data)
{
setWindow("LinearRegression")
fillStatsData("LinearRegression","LR")
fillComboBox(getWidget("LinearRegression","cbx_LR_func"),EPIR_REGRESSION_FUNCTIONS)
}

on_mnLogistic_activate <- function(widget,user.data)
{
setWindow("LogisticRegression")
fillStatsData("LogisticRegression","LG",sel.mode="single")
}

on_mnCurveSmoothing_activate <- function(widget,user.data)
{
setWindow("CurveSmoothing")
fillStatsData("CurveSmoothing","CS",FALSE)
fillComboBox(getWidget("CurveSmoothing","cbx_CS_tline"),EPIR_PLOT_LINE_TYPES)
fillComboBox(getWidget("CurveSmoothing","cbx_CS_type"),data=EPIR_SMOOTHER_METHODS,FALSE)
getWidget("CurveSmoothing","drw_CS_hlinecol")$modifyBg("normal",get("EPIR_DEFAULT_COLOR",envir=.EpiREnv))
}

on_mnMeanTest_activate <- function(widget,user.data)
{
setWindow("MeanTest")
fillStatsData("MeanTest","MTST",FALSE)
fillComboBox(getWidget("MeanTest","cbx_MTST_test_type"),data=EPIR_HTESTS_ALTERNATIVES,TRUE,TRUE)
}

on_mnVarTest_activate <- function(widget,user.data)
{
setWindow("VarTest")
fillStatsData("VarTest","VT",FALSE)
fillComboBox(getWidget("VarTest","cbx_VT_test_type"),data=EPIR_HTESTS_ALTERNATIVES,TRUE,TRUE)
}

on_mnAssociationTest_activate <- function(widget,user.data)
{
setWindow("AssociationTest")
fillStatsData("AssociationTest","AT",FALSE)
fillComboBox(getWidget("AssociationTest","cbx_AT_test_type"),data=EPIR_HTESTS_ALTERNATIVES,TRUE,TRUE)
}

#non parametric tests

on_mnFriedmanTest_activate <- function(widget,user.data)
{
setWindow("FriedmanTest")
fillStatsData("FriedmanTest","FTST",FALSE)
}

on_mnRankTest_activate <- function(widget,user.data)
{
setWindow("RankTest")
fillStatsData("RankTest","RT",FALSE)
fillComboBox(getWidget("RankTest","cbx_RT_test_type"),data=EPIR_HTESTS_ALTERNATIVES,TRUE,TRUE)
}

on_mnWilcoxonTest_activate <- function(widget,user.data)
{
setWindow("WilcoxonTest")
fillStatsData("WilcoxonTest","WT",FALSE)
fillComboBox(getWidget("WilcoxonTest","cbx_WT_test_type"),data=EPIR_HTESTS_ALTERNATIVES,TRUE,TRUE)
}

on_mnCaseControlStudies_activate <- function(widget,user.data)
{
setWindow("CaseControlStudies")
fillStatsData("CaseControlStudies","CCS",FALSE)
}

on_mnCohortStudies_activate <- function(widget,user.data)
{
setWindow("CohortStudies")
fillStatsData("CohortStudies","CCT",FALSE)
}

on_mnControlCharts_activate <- function(widget,user.data)
{
setWindow("ControlCharts")
fillStatsData("ControlCharts","CGP",FALSE)
fillComboBox(getWidget("ControlCharts","cbx_CGP_tline"),EPIR_PLOT_LINE_TYPES)
fillComboBox(getWidget("ControlCharts","cbx_CGP_frequency"),c("diario","semanal","mensal"))
}

on_mnPlotsX_activate <- function(widget,user.data)
{
setWindow("PlotsX")
fillGraphWindow("PlotsX","PX")
}

on_mnPlotsDots_activate <- function(widget,user.data)
{
setWindow("PlotsDots")
fillGraphWindow("PlotsDots","DT")
}

on_mnPlotsHist_activate <- function(widget,user.data)
{
setWindow("PlotsHist")
fillGraphWindow("PlotsHist","PH")
}

on_mnPlotsQQ_activate <- function(widget,user.data)
{
setWindow("PlotsQQ")
fillGraphWindow("PlotsQQ","QQ")
}

on_mnPlotsXY_activate <- function(widget,user.data)
{
setWindow("PlotsXY")
fillGraphWindow("PlotsXY","PXY",doubleaxis=TRUE,sel.mode="browse")
}

on_mnPlotsBars_activate <- function(widget,user.data)
{
setWindow("PlotsBars")
fillGraphWindow("PlotsBars","PB")
fillComboBox(getWidget("PlotsBars","cbx_PB_lty"),EPIR_PLOT_LINE_TYPES)
}

on_mnPlotsSectors_activate <- function(widget,user.data)
{
setWindow("PlotsSectors")
fillGraphWindow("PlotsSectors","PS")
fillComboBox(getWidget("PlotsSectors","cbx_PS_col"),data=EPIR_PLOT_PALETTE)
fillListView(getWidget("PlotsSectors","tvw_PS_viewFactor"),NULL,update=FALSE)
}

on_mnPlotsMosaic_activate <- function(widget,user.data)
{
setWindow("PlotsMosaic")
fillGraphWindow("PlotsMosaic","MOS", doubleaxis = TRUE)
}

on_mnPlotsBox_activate <- function(widget,user.data)
{
setWindow("PlotsBox")
fillGraphWindow("PlotsBox","BX")
}

on_mnInstallPlugin_activate <- function(widget,user.data)
{
setWindow("InstallPlugins")
}

on_mnSettings_activate <- function(widget,user.data)
{
setWindow("AppSettings")
}

on_mnHelp_activate <- function(widget,user.data)
{
showHelp("EpiR")
}

on_mnHelpR_activate <- function(widget,user.data)
{
out <- capture.output(help.start())
}

on_mnHomepageR_activate <- function(widget,user.data)
{
browseURL("http://www.r-project.org")
}

on_mnHomepage_activate <- function(widget,user.data)
{
browseURL(EPIR_HOMEPAGE)
}

on_mnAbout_activate <- function(widget,user.data)
{
setWindow("About")
nameVer <- getWidget("About","lbl_A_nameVer")
nameVer$setMarkup(paste("<b><big>Epi-R",EPIR_VER,"</big></b>"))
loadCredits("About","A")
getWidget("About","txt_A_credits")$grabFocus()
}


# main toolbar
on_btnOpenData_clicked <- function(widget,user.data)
{
on_mnOpenData_activate(widget,user.data)
}

on_btnSaveData_clicked <- function(widget,user.data)
{
on_mnSaveData_activate(widget,user.data)
}

on_btnCopy_clicked <- function(widget,user.data)
{
on_mnCopy_activate(widget,user.data)
}

on_btnPaste_clicked <- function(widget,user.data)
{
on_mnPaste_activate(widget,user.data)
}

on_btnRefresh_clicked <- function(widget,user.data)
{
on_mnRefresh_activate(widget,user.data)
}

on_btnClearOutput_clicked <- function(widget,user.data)
{
on_mnClearOutput_activate(widget,user.data)
}

on_btnHelp_clicked <- function(widget,user.data)
{
on_mnHelp_activate(widget,user.data)
}

on_btnQuit_clicked <- function(widget,user.data)
{
on_mnQuit_activate(widget,user.data)
}

on_btnRun_clicked <- function(widget,user.data)
{
cmd <- getWidget("Main","txtCommand")
cmd.text <- setLocale(getTv(cmd),fixspc=FALSE)
parseCmd(cmd.text)
clearTv(cmd)
}

on_Main_destroy <- function(widget,user.data)
{
try(get("trayIcon",envir=.EpiREnv)$setVisible(FALSE),silent=TRUE)
unload(hard=TRUE)
}

on_Main_delete <- function(widget,user.data)
{
if(askDialog("Main","Deseja realmente sair do aplicativo?\nTodos os dados da área de trabalho serão perdidos.")!="yes")
	return(TRUE) # prevent main window from closing
}

# list views
on_tvwObjects_button_release_event <- function(widget,event,user.data)
{
fillVarList()
mnSaveData <- getWidget("Main","mnSaveData")
btnSaveData <- getWidget("Main","btnSaveData")
if(getWidget("Main","tvwObjects")$getSelection()$countSelectedRows()>0)
	{
	mnSaveData$setSensitive(TRUE)
	btnSaveData$setSensitive(TRUE)
	}
else
	{
	mnSaveData$setSensitive(FALSE)
	btnSaveData$setSensitive(FALSE)
	}
}

on_txtOutput_focus_in_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(TRUE)
getWidget("Main","btnCopy")$setSensitive(TRUE)
}

on_txtOutput_focus_out_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(FALSE)
getWidget("Main","btnCopy")$setSensitive(FALSE)
}

on_txtCommand_focus_in_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(TRUE)
getWidget("Main","btnCopy")$setSensitive(TRUE)
getWidget("Main","mnPaste")$setSensitive(TRUE)
getWidget("Main","btnPaste")$setSensitive(TRUE)
}

on_txtCommand_focus_out_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(FALSE)
getWidget("Main","btnCopy")$setSensitive(FALSE)
getWidget("Main","mnPaste")$setSensitive(FALSE)
getWidget("Main","btnPaste")$setSensitive(FALSE)
}

on_tvwHistory_focus_in_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(TRUE)
getWidget("Main","btnCopy")$setSensitive(TRUE)
}

on_tvwHistory_focus_out_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(FALSE)
getWidget("Main","btnCopy")$setSensitive(FALSE)
}

on_tvwObjects_focus_in_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(TRUE)
getWidget("Main","btnCopy")$setSensitive(TRUE)
}

on_tvwObjects_focus_out_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(FALSE)
getWidget("Main","btnCopy")$setSensitive(FALSE)
}

on_tvwVariables_focus_in_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(TRUE)
getWidget("Main","btnCopy")$setSensitive(TRUE)
}

on_tvwVariables_focus_out_event <- function(widget,user.data)
{
getWidget("Main","mnCopy")$setSensitive(FALSE)
getWidget("Main","btnCopy")$setSensitive(FALSE)
}

on_tvwHistory_button_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	{
	source <- getWidget("Main","tvwHistory")
	destination <- getWidget("Main","txtCommand")
	content <- getListSelection(source,fixspc=FALSE)
	if(!is.null(content))
		insertTv(destination,content)
	}
}

on_tvwObjects_button_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	{
	source <- getWidget("Main","tvwObjects")
	destination <- getWidget("Main","txtCommand")
	content <- getListSelection(source,1)
	if(!is.null(content))
		insertTv(destination,content)
	}
}

on_tvwVariables_button_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	{
	source <- getWidget("Main","tvwVariables")
	destination <- getWidget("Main","txtCommand")
	if(class(eval(parse(text=getListSelection(getWidget("Main","tvwObjects"))),envir=.GlobalEnv)) %in% c("matrix","data.frame")) # check if object is bidimensional
		content <- getListSelection(source,2)
	else
		content <- getListSelection(source,1)
	if(!is.null(content))
		insertTv(destination,content)
	}
else if(event$type=="GDK_BUTTON_PRESS" & event$button==3)
	popUpMenu(c("Resumo breve"),c(on_cmnBriefSummary_activate),event)
}


# context menus

on_cmnBriefSummary_activate <- function(widget,user.data)
{
dataset <- vectorToString(getListSelection(getWidget("Main","tvwObjects")))
vars <- vectorToString(getListSelection(getWidget("Main","tvwVariables"),col=2))
if(!is.null(objects))
	{
	# translate
	cmd <- paste("resumo.breve(\"",dataset,"\",\"",vars,"\")",sep="")
	runCommand(cmd)
	}
}
