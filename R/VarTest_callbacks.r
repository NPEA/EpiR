on_btn_VT_help_clicked <- function(widget,user.data)
{
showHelp("VarTest")
}

on_btn_VT_cancel_clicked <- function(widget,user.data)
{
closeWindow("VarTest")
}

on_btn_VT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("VarTest","cbx_VT_dataset"))
sample1 <- getActiveData(getWidget("VarTest","cbx_VT_sample1"))
sample2 <- getActiveData(getWidget("VarTest","cbx_VT_sample2"))

par <- getWidget("VarTest","txt_VT_statistic")$getText()
if(par=="")
	par <- "NULO"
conf.level <- getWidget("VarTest","spb_VT_conf")$getValue()
if(conf.level > 1)
	conf.level <- conf.level/100
na.remove <- getWidget("VarTest","chk_VT_ND")$getActive()
if(na.remove)
	na.remove <- "VERDADEIRO"
else
	na.remove <- "FALSO"
alternative <- getActiveData(getWidget("VarTest","cbx_VT_test_type"))
if(alternative=="")
	alternative <- "igualdade"
if(getWidget("VarTest","rb_VT_Ftest")$getActive())
	test <- "teste.Fligner"
else if(getWidget("VarTest","rb_VT_btest")$getActive())
	test <- "teste.bartlett"
else
	test <- "teste.F"
if(getWidget("VarTest","chk_VT_agrup")$getActive())
	group <- "VERDADEIRO"
else
	group <- "FALSO"
filter <- setLocale(getWidget("VarTest","txt_VT_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = paste(dataset,"[1:5,]",sep="")))
filter_test <- try(eval(parse(text = filter),dataset.sample),silent=TRUE)
if(class(filter_test) == "try-error")
	{
	msgDialog("VarTest","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(filter_test) & trim(filter) != "")
	{
	msgDialog("VarTest","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	if(trim(filter) == "")
		filter <- NULL
	#translate
	if(test=="teste.F" & !is.null(filter))
		cmd <- paste("teste.var(\"",dataset,"\",amostra.1=\"",sample1,"\",amostra.2=\"",sample2,"\",parametro=",par,",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",remove.na=",na.remove,",filtro=\"",filter,"\",agrupamento=",group,")",sep="")
	else if(test == "teste.F")
		cmd <- paste("teste.var(\"",dataset,"\",amostra.1=\"",sample1,"\",amostra.2=\"",sample2,"\",parametro=",par,",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",remove.na=",na.remove,",agrupamento=",group,")",sep="")
	else if(!is.null(filter))
		cmd <- paste("teste.var(\"",dataset,"\",amostra.1=\"",sample1,"\",amostra.2=\"",sample2,"\",teste=\"",test,"\",remove.na=",na.remove,",filtro=\"",filter,"\",agrupamento=",group,")",sep="")
	else
		cmd <- paste("teste.var(\"",dataset,"\",amostra.1=\"",sample1,"\",amostra.2=\"",sample2,"\",teste=\"",test,"\",remove.na=",na.remove,",agrupamento=",group,")",sep="")
	runCommand(cmd)
	closeWindow("VarTest", "VT")
	}
}

on_cbx_VT_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("VarTest","cbx_VT_dataset"))
fillComboBox(getWidget("VarTest","cbx_VT_sample1"),c("",getNumeric(dataset)))
fillComboBox(getWidget("VarTest","cbx_VT_sample2"),c("",getNames(dataset)))
if(dataset == "")
	getWidget("VarTest","btn_VT_expconstructor")$setSensitive(FALSE)
else
	getWidget("VarTest","btn_VT_expconstructor")$setSensitive(TRUE)
}

on_cbx_VT_sample1_changed <- function(widget,user.data)
{
sample1 <- trim(getActiveData(getWidget("VarTest","cbx_VT_sample1"),fix=FALSE))
sample2 <- trim(getActiveData(getWidget("VarTest","cbx_VT_sample2"),fix=FALSE))
if(sample1 !="" && sample2 !="")
	getWidget("VarTest","btn_VT_execute")$setSensitive(TRUE)
else
	getWidget("VarTest","btn_VT_execute")$setSensitive(FALSE)
}

on_cbx_VT_sample2_changed <- function(widget,user.data)
{
on_cbx_VT_sample1_changed()
}

on_chk_VT_agrup_toggled <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("VarTest","cbx_VT_dataset"))
names.dataset <- getNames(dataset)
group <- getWidget("VarTest","chk_VT_agrup")$getActive()
if(group)
	{
	if(dataset != "")
		{
		ret.names.dataset <- NULL
		for(i in names.dataset)
			if(nlevels(as.factor(eval(parse(text = paste(dataset,"$",i,sep=""))))) == 2)
				ret.names.dataset <- c(ret.names.dataset,i)
		fillComboBox(getWidget("VarTest","cbx_VT_sample2"),c("",ret.names.dataset))
		}
	}
else
	{
	if(dataset != "")
		fillComboBox(getWidget("VarTest", "cbx_VT_sample2"), c("",names.dataset))
	}

}

on_rb_VT_vtest_group_toggled <- function(widget,user.data)
{
button <- getWidget("VarTest","rb_VT_vtest")$getActive()
if(button)
	{
	getWidget("VarTest","spb_VT_conf")$setSensitive(TRUE)
	getWidget("VarTest","cbx_VT_test_type")$setSensitive(TRUE)
	getWidget("VarTest","txt_VT_statistic")$setSensitive(TRUE)
	}
else
	{
	getWidget("VarTest","spb_VT_conf")$setSensitive(FALSE)
	getWidget("VarTest","cbx_VT_test_type")$setSensitive(FALSE)
	getWidget("VarTest","txt_VT_statistic")$setSensitive(FALSE)	
	}
}

on_rb_VT_btest_group_changed <- function(widget,user.data)
{

getWidget("VarTest","spb_VT_conf")$setSensitive(FALSE)
getWidget("VarTest","cbx_VT_test_type")$setSensitive(FALSE)
getWidget("VarTest","txt_VT_statistic")$setSensitive(FALSE)
}

on_rb_VT_Ftest_group_changed <- function(widget,user.data)
{
getWidget("VarTest","spb_VT_conf")$setSensitive(FALSE)
getWidget("VarTest","cbx_VT_test_type")$setSensitive(FALSE)
getWidget("VarTest","txt_VT_statistic")$setSensitive(FALSE)
}

on_btn_VT_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("VarTest","cbx_VT_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="VarTest")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}