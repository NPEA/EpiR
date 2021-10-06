# aggregaterows callbaack functions
# main buttons
on_btn_AR_help_clicked <- function(widget,user.data)
{
showHelp("AggregateRows")
}

on_btn_AR_cancel_clicked <- function(widget,user.data)
{
closeWindow("AggregateRows")
}

on_btn_AR_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AggregateRows","cbx_AR_dataset"))
aggregate.variables <- vectorToString(getListSelection(getWidget("AggregateRows","tvw_AR_agregVariables")))
variable <- getActiveData(getWidget("AggregateRows","cbx_AR_variable"))
if(variable == "")
	variable <- "NULO"
else
	variable <- paste("\"",variable,"\"",sep="")
functions <- getActiveData(getWidget("AggregateRows","cbx_AR_function"))
object <- setLocale(getWidget("AggregateRows","txt_AR_object")$getText())
expression <- setLocale(getWidget("AggregateRows","txt_AR_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = paste(dataset,"[1:5,]",sep="")))
test <- try(eval(parse(text = expression),dataset.sample),silent=TRUE)
if(class(test) == "try-error")
	{
	msgDialog("AggregateRows","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(test) & trim(expression) != "")
	{
	msgDialog("AggregateRows","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	#translate
	if(expression == "")
		cmd <- paste("agregar.linhas(\"",dataset,"\",variavel=",variable,",variaveis.agregacao=c(\"",aggregate.variables,"\"),funcao=\"",functions,"\",objeto=\"",object,"\")",sep="")
	else
		cmd <- paste("agregar.linhas(\"",dataset,"\",variavel=",variable,",variaveis.agregacao=c(\"",aggregate.variables,"\"),funcao=\"",functions,"\",objeto=\"",object,"\",filtro=\"",expression,"\")",sep="")
	
	runCommand(cmd)
	closeWindow("AggregateRows","AR")
	}
}
# operations
on_cbx_AR_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AggregateRows","cbx_AR_dataset"))
fillComboBox(getWidget("AggregateRows","cbx_AR_variable"),getNames(dataset))
fillListView(getWidget("AggregateRows","tvw_AR_agregVariables"),getNames(dataset),FALSE)
if(dataset != "")
	getWidget("AggregateRows","btn_AR_expconstructor")$setSensitive(TRUE)
else
	getWidget("AggregateRows","btn_AR_expconstructor")$setSensitive(FALSE)
}

on_cbx_AR_variable_changed <- function(widget,user.data)
{
aggregate.variables <- vectorToString(getListSelection(getWidget("AggregateRows","tvw_AR_agregVariables")))
variable <- getActiveData(getWidget("AggregateRows","cbx_AR_variable"))
functions <- getActiveData(getWidget("AggregateRows","cbx_AR_function"))
object <- setLocale(getWidget("AggregateRows","txt_AR_object")$getText())
if(functions != "frequencia")
	{
	if(aggregate.variables[1]!= "" & variable != "" & functions != "contar" & object != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
	}
else
	{
	if(aggregate.variables[1]!= "" & object != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
	}
}


on_tvw_AR_agregVariables_button_release_event <- function(widget,user.data)
{
aggregate.variables <- vectorToString(getListSelection(getWidget("AggregateRows","tvw_AR_agregVariables")))
variable <- getActiveData(getWidget("AggregateRows","cbx_AR_variable"))
functions <- getActiveData(getWidget("AggregateRows","cbx_AR_function"))
object <- setLocale(getWidget("AggregateRows","txt_AR_object")$getText())
if(functions != "frequencia")
	{
	if(aggregate.variables[1]!= "" & variable != "" & object != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
	}
else
	{
	if(aggregate.variables[1]!= "" & object != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
	}
}

on_btn_AR_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AggregateRows","cbx_AR_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="AggregateRows")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}

on_cbx_AR_function_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("AggregateRows","cbx_AR_dataset"))
fillComboBox(getWidget("AggregateRows","cbx_AR_variable"),getNames(dataset))
functions <- getActiveData(getWidget("AggregateRows","cbx_AR_function"))
if(functions == "frequencia")
	{
	aggregate.variables <- vectorToString(getListSelection(getWidget("AggregateRows","tvw_AR_agregVariables")))
	object <- setLocale(getWidget("AggregateRows","txt_AR_object")$getText())
	if(aggregate.variables[1] != "" & object != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
		
	}
}

on_txt_AR_object_changed <- function(widget,user.data)
{
aggregate.variables <- vectorToString(getListSelection(getWidget("AggregateRows","tvw_AR_agregVariables")))
variable <- getActiveData(getWidget("AggregateRows","cbx_AR_variable"))
functions <- getActiveData(getWidget("AggregateRows","cbx_AR_function"))
object <- setLocale(getWidget("AggregateRows","txt_AR_object")$getText())
if(functions == "frequencia")
	{
	if(aggregate.variables[1]!= "" & object != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
	}
else
	{
	if(aggregate.variables[1]!= "" & object != "" & variable != "")
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(TRUE)
	else
		getWidget("AggregateRows","btn_AR_execute")$setSensitive(FALSE)
	}
}