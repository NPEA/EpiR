on_btn_RT_help_clicked <- function(widget,user.data)
{
showHelp("RankTest")
}

on_btn_RT_cancel_clicked <- function(widget,user.data)
{
closeWindow("RankTest")
}

on_btn_RT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RankTest","cbx_RT_dataset"))
sample1 <- getActiveData(getWidget("RankTest","cbx_RT_sample1"))
sample2 <- getActiveData(getWidget("RankTest","cbx_RT_sample2"))
alternative <- getActiveData(getWidget("RankTest","cbx_RT_test_type"))
filter <- setLocale(getWidget("RankTest","txt_RT_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = dataset))
filter_test <- try(eval(parse(text = filter),dataset.sample),silent=TRUE)
if(class(filter_test) == "try-error")
	{
	msgDialog("RankTest","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(filter_test) & trim(filter) != "")
	{
	msgDialog("RankTest","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	if(trim(filter) == "")
		filter <- "NULO"
	else
		filter <- paste("\"",filter,"\"",sep="")
	cmd <- paste("teste.sinais(\"",dataset,"\",variavel.1 =\"",sample1,"\",variavel.2=\"",sample2,"\",hip.alternativa=\"",alternative,"\",filtro=",filter,")",sep="")
	runCommand(cmd)
	closeWindow("RankTest", "RT")
	}
}

on_cbx_RT_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RankTest","cbx_RT_dataset"))
if(trim(dataset) != "")
	{
	getWidget("RankTest", "btn_RT_expconstructor")$setSensitive(TRUE)
	fillComboBox(getWidget("RankTest","cbx_RT_sample1"),c("",getNumeric(dataset)))
	fillComboBox(getWidget("RankTest","cbx_RT_sample2"),c("",getNumeric(dataset)))
	}
else
	getWidget("RankTest", "btn_RT_expconstructor")$setSensitive(FALSE)
}

on_cbx_RT_sample1_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("RankTest","cbx_RT_sample1"))
sample2 <- getActiveData(getWidget("RankTest","cbx_RT_sample2"))
if(any(c(trim(sample1), trim(sample2)) == ""))
		getWidget("RankTest","btn_RT_execute")$setSensitive(FALSE)
	else
		getWidget("RankTest","btn_RT_execute")$setSensitive(TRUE)
}

on_cbx_RT_sample2_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("RankTest","cbx_RT_sample1"))
sample2 <- getActiveData(getWidget("RankTest","cbx_RT_sample2"))
if(any(c(trim(sample1), trim(sample2)) == ""))
		getWidget("RankTest","btn_RT_execute")$setSensitive(FALSE)
	else
		getWidget("RankTest","btn_RT_execute")$setSensitive(TRUE)
}

on_btn_RT_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RankTest","cbx_RT_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="RankTest")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}