on_btn_WT_help_clicked <- function(widget,user.data)
{
showHelp("WilcoxonTest")
}

on_btn_WT_cancel_clicked <- function(widget,user.data)
{
closeWindow("WilcoxonTest")
}

on_btn_WT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("WilcoxonTest","cbx_WT_dataset"))
sample1 <- getActiveData(getWidget("WilcoxonTest","cbx_WT_sample1"))
sample2 <- getActiveData(getWidget("WilcoxonTest","cbx_WT_sample2"))
par <- getWidget("WilcoxonTest","txt_WT_statistic")$getText()

if(par == "")
	par <- 0

if(getWidget("WilcoxonTest","chk_WT_paired")$getActive())
	paired <- "VERDADEIRO"
else
	paired <- "FALSO"

conf.level <- getWidget("WilcoxonTest","spb_WT_conf")$getValue()
conf.level <- conf.level/100
alternative <- getActiveData(getWidget("WilcoxonTest","cbx_WT_test_type"))

filter <- setLocale(getWidget("WilcoxonTest","txt_WT_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = dataset))
filter_test <- try(eval(parse(text = filter),dataset.sample),silent=TRUE)
if(class(filter_test) == "try-error")
	{
	msgDialog("WilcoxonTest","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(filter_test) & trim(filter) != "")
	{
	msgDialog("WilcoxonTest","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	if(trim(filter) == "")
		filter <- "NULO"
	else
		filter <- paste("\"",filter,"\"",sep="")
	cmd <- paste("teste.wcoxon(\"",dataset,"\",variavel.1 =\"",sample1,"\",variavel.2=\"",sample2,"\",parametro=",par,",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",pareado=",paired,",filtro=",filter,")",sep="")
	runCommand(cmd)
	closeWindow("WilcoxonTest", "WT")
	}
}

on_cbx_WT_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("WilcoxonTest","cbx_WT_dataset"))
if(trim(dataset) != "")
	{
	getWidget("WilcoxonTest", "btn_WT_expconstructor")$setSensitive(TRUE)
	fillComboBox(getWidget("WilcoxonTest","cbx_WT_sample1"),c("",getNumeric(dataset)))
	fillComboBox(getWidget("WilcoxonTest","cbx_WT_sample2"),c("",getNumeric(dataset)))
	getWidget("WilcoxonTest","txt_WT_statistic")$setText(0)
	}
else
	getWidget("WilcoxonTest", "btn_WT_expconstructor")$setSensitive(FALSE)
}

on_cbx_WT_sample1_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("WilcoxonTest","cbx_WT_sample1"))
sample2 <- getActiveData(getWidget("WilcoxonTest","cbx_WT_sample2"))
if(any(c(trim(sample1), trim(sample2)) == ""))
		getWidget("WilcoxonTest","btn_WT_execute")$setSensitive(FALSE)
	else
		getWidget("WilcoxonTest","btn_WT_execute")$setSensitive(TRUE)

}

on_cbx_WT_sample2_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("WilcoxonTest","cbx_WT_sample1"))
sample2 <- getActiveData(getWidget("WilcoxonTest","cbx_WT_sample2"))
if(any(c(trim(sample1), trim(sample2)) == ""))
		getWidget("WilcoxonTest","btn_WT_execute")$setSensitive(FALSE)
	else
		getWidget("WilcoxonTest","btn_WT_execute")$setSensitive(TRUE)
}

on_btn_WT_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("WilcoxonTest","cbx_WT_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="WilcoxonTest")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}