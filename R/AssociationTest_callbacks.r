on_btn_AT_help_clicked <- function(widget,user.data)
{
showHelp("AssociationTest")
}

on_btn_AT_cancel_clicked <- function(widget,user.data)
{
closeWindow("AssociationTest")
}

on_btn_AT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AssociationTest","cbx_AT_dataset"))
variable1 <- getActiveData(getWidget("AssociationTest","cbx_AT_variable1"))
variable2 <- getActiveData(getWidget("AssociationTest","cbx_AT_variable2"))
variable3.Mtest <- getActiveData(getWidget("AssociationTest","cbx_AT_sample_Mtest"))
if(getWidget("AssociationTest","rb_AT_Mtest")$getActive())
	test <- "mantelhaen"
else
	test <- "q_quadrado"
conf.level <- getWidget("AssociationTest","spb_AT_conf")$getValue()
if(conf.level > 1)
	conf.level <- conf.level/100
alternative <- getActiveData(getWidget("AssociationTest","cbx_AT_test_type"))
if(alternative=="")
	alternative <- "igualdade"
if(getWidget("AssociationTest","chk_AT_pvalue")$getActive())
	pvalue <- "VERDADEIRO"
else
	pvalue <- "FALSO"
if(getWidget("AssociationTest","chk_AT_showtable")$getActive())
	show.table <- "VERDADEIRO"
else
	show.table <- "FALSO"
if(getWidget("AssociationTest","chk_AT_ND")$getActive())
	na.remove <- "VERDADEIRO"
else
	na.remove <- "FALSO"
filter <- setLocale(getWidget("AssociationTest","txt_AT_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = paste(dataset,"[1:5,]",sep="")))
filter_test <- try(eval(parse(text = filter),dataset.sample),silent=TRUE)
if(class(filter_test) == "try-error")
	{
	msgDialog("AssociationTest","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(filter_test) & trim(filter) != "")
	{
	msgDialog("AssociationTest","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	if(trim(filter) == "")
		filter <- NULL
	#translate
	if(test == "mantelhaen" & !is.null(filter))
		cmd <- paste("teste.associacao(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=\"",variable2,"\",estrato=\"",variable3.Mtest,"\",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",filtro=\"",filter,"\",mostrar.tabela=",show.table,",remove.na=",na.remove,")",sep="")
	else if(test == "mantelhaen")
		cmd <- paste("teste.associacao(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=\"",variable2,"\",estrato=\"",variable3.Mtest,"\",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",mostrar.tabela=",show.table,",remove.na=",na.remove,")",sep="")
	else if(!is.null(filter))
		cmd <- paste("teste.associacao(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=\"",variable2,"\",teste=\"",test,"\",filtro=\"",filter,"\",mostrar.tabela=",show.table,",valor_p=",pvalue,",remove.na=",na.remove,")",sep="")
	else
		cmd <- paste("teste.associacao(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=\"",variable2,"\",teste=\"",test,"\",mostrar.tabela=",show.table,",valor_p=",pvalue,",remove.na=",na.remove,")",sep="")	

	runCommand(cmd)
	closeWindow("AssociationTest", "AT")
	}
}
on_cbx_AT_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AssociationTest","cbx_AT_dataset"))
fillComboBox(getWidget("AssociationTest","cbx_AT_variable1"),c("",getNames(dataset)))
fillComboBox(getWidget("AssociationTest","cbx_AT_variable2"),c("",getNames(dataset)))
fillComboBox(getWidget("AssociationTest","cbx_AT_sample_Mtest"),c("",getNames(dataset)))
if(dataset == "")
	getWidget("AssociationTest","btn_AT_expconstructor")$setSensitive(FALSE)
else
	getWidget("AssociationTest","btn_AT_expconstructor")$setSensitive(TRUE)
}

on_cbx_AT_variable1_changed <- function(widget,user.data)
{
variable1 <- trim(getActiveData(getWidget("AssociationTest","cbx_AT_variable1"),fix=FALSE))
variable2 <- trim(getActiveData(getWidget("AssociationTest","cbx_AT_variable2"),fix=FALSE))
variable3.Mtest <- trim(getActiveData(getWidget("AssociationTest","cbx_AT_sample_Mtest"),fix=FALSE))
chisq.test <- getWidget("AssociationTest","rb_AT_chisqtest")$getActive()
if(chisq.test)
	{
	if(variable1 != "" && variable2 != "")
		getWidget("AssociationTest","btn_AT_execute")$setSensitive(TRUE)
	else
		getWidget("AssociationTest","btn_AT_execute")$setSensitive(FALSE)
	}
else
	{
	if(variable1 != "" && variable2 != "" && variable3.Mtest != "")
		getWidget("AssociationTest","btn_AT_execute")$setSensitive(TRUE)
	else
		getWidget("AssociationTest","btn_AT_execute")$setSensitive(FALSE)
	}
}

on_cbx_AT_variable2_changed <- function(widget,user.data)
{
on_cbx_AT_variable1_changed ()
}

on_cbx_AT_sample_Mtest_changed <- function(widget,user.data)
{
variable1 <- trim(getActiveData(getWidget("AssociationTest","cbx_AT_variable1"),fix=FALSE))
variable2 <- trim(getActiveData(getWidget("AssociationTest","cbx_AT_variable2"),fix=FALSE))
variable3.Mtest <- trim(getActiveData(getWidget("AssociationTest","cbx_AT_sample_Mtest"),fix=FALSE))
if(variable1 != "" && variable2 != "" && variable3.Mtest != "")
	getWidget("AssociationTest","btn_AT_execute")$setSensitive(TRUE)
else
	getWidget("AssociationTest","btn_AT_execute")$setSensitive(FALSE)
}

on_rb_AT_chisqtest_toggled <- function(widget,user.data)
{
variable1 <- getActiveData(getWidget("AssociationTest","cbx_AT_variable1"))
variable2 <- getActiveData(getWidget("AssociationTest","cbx_AT_variable2"))
getWidget("AssociationTest","cbx_AT_sample_Mtest")$setSensitive(FALSE)
getWidget("AssociationTest","cbx_AT_test_type")$setSensitive(FALSE)
getWidget("AssociationTest","spb_AT_conf")$setSensitive(FALSE)
getWidget("AssociationTest","chk_AT_pvalue")$setSensitive(TRUE)
if(variable1 != "" && variable2 != "")
	getWidget("AssociationTest","btn_AT_execute")$setSensitive(TRUE)
else
	getWidget("AssociationTest","btn_AT_execute")$setSensitive(FALSE)

}

on_rb_AT_Mtest_toggled <- function(widget,user.data)
{
variable1 <- getActiveData(getWidget("AssociationTest","cbx_AT_variable1"))
variable2 <- getActiveData(getWidget("AssociationTest","cbx_AT_variable2"))
variable3.Mtest <- getActiveData(getWidget("AssociationTest","cbx_AT_sample_Mtest"))
getWidget("AssociationTest","cbx_AT_test_type")$setSensitive(TRUE)
getWidget("AssociationTest","spb_AT_conf")$setSensitive(TRUE)
getWidget("AssociationTest","chk_AT_pvalue")$setSensitive(FALSE)
getWidget("AssociationTest","cbx_AT_sample_Mtest")$setSensitive(TRUE)
if(variable1 != "" && variable2 != "" && variable3.Mtest != "")
	getWidget("AssociationTest","btn_AT_execute")$setSensitive(TRUE)
else
	getWidget("AssociationTest","btn_AT_execute")$setSensitive(FALSE)
}

on_btn_AT_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AssociationTest","cbx_AT_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="AssociationTest")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}