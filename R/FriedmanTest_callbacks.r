on_btn_FTST_help_clicked <- function(widget,user.data)
{
showHelp("FriedmanTest")
}

on_btn_FTST_cancel_clicked <- function(widget,user.data)
{
closeWindow("FriedmanTest")
}

on_btn_FTST_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FriedmanTest","cbx_FTST_dataset"))
sample1 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample1"))
sample2 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample2"))
sample3 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample3"))
if(trim(sample3) == "")
	sample3 <- "NULO"
else
	sample3 <- paste("\"", sample3, "\"")
factor <- getWidget("FriedmanTest","chk_FTST_factor")$getActive()

filter <- setLocale(getWidget("FriedmanTest","txt_FTST_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = dataset))
filter_test <- try(eval(parse(text = filter),dataset.sample),silent=TRUE)
if(class(filter_test) == "try-error")
	{
	msgDialog("FriedmanTest","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(filter_test) & trim(filter) != "")
	{
	msgDialog("FriedmanTest","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	if(trim(filter) == "")
		filter <- "NULO"
	else
		filter <- paste("\"",filter,"\"",sep="")
	if(factor)
		{
		factor <- "VERDADEIRO"
		cmd <- paste("teste.friedman(\"",dataset,"\",variavel.1 =\"",sample1,"\",variavel.2=\"",sample2,"\",variavel.3=",sample3,",variaveis.categoricas=",factor,",filtro=",filter,")",sep="")
		}
	else
		{
		factor <- "FALSO"
		cmd <- paste("teste.friedman(\"",dataset,"\",variavel.1 =\"",sample1,"\",variavel.2=\"",sample2,"\",variavel.3=",sample3,",variaveis.categoricas=",factor,",filtro=",filter,")",sep="")
		}
	runCommand(cmd)
	closeWindow("FriedmanTest", "FTST")
	}
}

on_cbx_FTST_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FriedmanTest","cbx_FTST_dataset"))
if(trim(dataset) != "")
	{
	getWidget("FriedmanTest", "btn_FTST_expconstructor")$setSensitive(TRUE)
	factor <- getWidget("FriedmanTest","chk_FTST_factor")$getActive()
	fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample1"),c("",getNumeric(dataset)))
	if(factor)
		{
		fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample2"),c("",getBy(dataset)))
		fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample3"),c("",getBy(dataset)))
		}
	else
		{
		fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample2"),c("",getNumeric(dataset)))
		fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample3"),c("",getNumeric(dataset)))	
		}
	}
else
	getWidget("FriedmanTest", "btn_FTST_expconstructor")$setSensitive(FALSE)
}

on_cbx_FTST_sample1_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample1"))
sample2 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample2"))
sample3 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample3"))
factor <- getWidget("FriedmanTest","chk_FTST_factor")$getActive()
if(factor)
	{
	if(any(c(trim(sample1), trim(sample2), trim(sample3)) == ""))
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(FALSE)
	else
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(TRUE)
	}
else
	{
	if(trim(sample1) == "" || trim(sample2) == "")
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(FALSE)
	else
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(TRUE)
	}
}

on_cbx_FTST_sample2_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample1"))
sample2 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample2"))
sample3 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample3"))
factor <- getWidget("FriedmanTest","chk_FTST_factor")$getActive()
if(factor)
	{
	if(any(c(trim(sample1), trim(sample2), trim(sample3)) == ""))
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(FALSE)
	else
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(TRUE)
	}
else
	{
	if(trim(sample1) == "" || trim(sample2) == "")
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(FALSE)
	else
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(TRUE)
	}
}

on_cbx_FTST_sample3_changed <- function(widget,user.data)
{
sample1 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample1"))
sample2 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample2"))
sample3 <- getActiveData(getWidget("FriedmanTest","cbx_FTST_sample3"))
factor <- getWidget("FriedmanTest","chk_FTST_factor")$getActive()
if(factor)
	{
	if(any(c(trim(sample1), trim(sample2), trim(sample3)) == ""))
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(FALSE)
	else
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(TRUE)
	}
else
	{
	if(trim(sample1) == "" || trim(sample2) == "")
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(FALSE)
	else
		getWidget("FriedmanTest","btn_FTST_execute")$setSensitive(TRUE)
	}
}

on_chk_FTST_factor_toggled <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FriedmanTest","cbx_FTST_dataset"))
factor <- getWidget("FriedmanTest","chk_FTST_factor")$getActive()
if(factor)
	{
	fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample2"),c("",getBy(dataset)))
	fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample3"),c("",getBy(dataset)))
	}
else
	{
	fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample2"),c("",getNumeric(dataset)))
	fillComboBox(getWidget("FriedmanTest","cbx_FTST_sample3"),c("",getNumeric(dataset)))	
	}
}

on_btn_FTST_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FriedmanTest","cbx_FTST_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="FriedmanTest")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}