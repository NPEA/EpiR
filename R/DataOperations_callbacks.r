
# dataOperations callback functions

# main buttons
on_btn_DO_help_clicked <- function(widget,user.data)
{
showHelp("DataOperations")
}

on_btn_DO_cancel_clicked <- function(widget,user.data)
{
closeWindow("DataOperations")
}

on_btn_DO_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("DataOperations","cbx_DO_dataset"))
object <- setLocale(getWidget("DataOperations","txt_DO_object")$getText())    
operations <- getListViewData(getWidget("DataOperations","tvw_DO_operations"),fixspc=FALSE)

cmd <- paste("transformar.dados(\"",dataset,"\",\"",vectorToString(operations,quoted=TRUE,coll=","),"\",objeto=\"",object,"\")",sep="")
# translate
runCommand(cmd)
getWidget("DataOperations", "btn_DO_execute")$setSensitive(FALSE)
fillListView(getWidget("DataOperations", "tvw_DO_operations"),NULL)
closeWindow("DataOperations", "DO")
}


# operations
on_cbx_DO_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("DataOperations","cbx_DO_dataset"))
object <- setLocale(getWidget("DataOperations","txt_DO_newvar")$getText())
getWidget("DataOperations","txt_DO_object")$setText(dataset)
fillListView(getWidget("DataOperations","tvw_DO_operations"),NULL,TRUE,sel.mode="single")
if(dataset == "" | object == "")
	getWidget("DataOperations","btn_DO_calculator")$setSensitive(FALSE)
else
	getWidget("DataOperations","btn_DO_calculator")$setSensitive(TRUE)
}


on_tvw_DO_operations_button_release_event <- function(widget,event,user.data)
{
toggleExecute("DataOperations","DO",type="operations",button="remove")
}

on_btn_DO_add_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("DataOperations","cbx_DO_dataset"))
dataset <- eval(parse(text = dataset))
variable.name <- getWidget("DataOperations","txt_DO_newvar")$getText()
expression <- getWidget("DataOperations","txt_DO_expression")$getText()
expression <- paste(variable.name, "<-",expression,sep="")
result <- try(eval(parse(text=expression),envir=dataset),silent=TRUE)
if(class(result)=="try-error")
	{
	msgDialog("DataOperations","error","Há um erro na expressão. Corrija-a e tente novamente.")
	return()
	}
else
	{
	if(variable.name %in% names(dataset))
		{
		if(askDialog("DataOperations","ATENÇÃO! Você irá alterar uma variável do banco de dados. Deseja continuar?")=="yes")
			{
			fillOperationList(fixspc=FALSE)	
			getWidget("DataOperations","txt_DO_newvar")$setText("")
			getWidget("DataOperations","txt_DO_expression")$setText("")
			getWidget("DataOperations","btn_DO_add")$setSensitive(FALSE)
			toggleExecute("DataOperations","DO",type="operations",button="execute",selection=FALSE)
			}
		}
	else
		{
		fillOperationList(fixspc=FALSE)	
		getWidget("DataOperations","txt_DO_newvar")$setText("")
		getWidget("DataOperations","txt_DO_expression")$setText("")
		getWidget("DataOperations","btn_DO_add")$setSensitive(FALSE)
		toggleExecute("DataOperations","DO",type="operations",button="execute",selection=FALSE)
		}
	}
}

on_btn_DO_remove_clicked <- function(widget,user.data)
{
removeSelectedRows(getWidget("DataOperations","tvw_DO_operations"))
toggleExecute("DataOperations","DO",type="operations",button="remove")
toggleExecute("DataOperations","DO",type="operations",button="execute",selection=FALSE)
}

on_btn_DO_calculator_clicked <- function(widget,user.data)
{
setWindow("Calculator",parent="DataOperations")
varname <- setLocale(getWidget("DataOperations","txt_DO_newvar")$getText())
varname <- paste(varname, " = ",sep="")
functions <- EPIR_CALC_FUNCTIONS
fillListView(getWidget("Calculator","tvw_CAL_function"),functions,FALSE,headers="Funções",sel.mode="browse")
dataset <- getActiveData(getWidget("DataOperations","cbx_DO_dataset"))
fillComboBox(getWidget("Calculator", "cbx_CAL_dataset"),dataset,TRUE,TRUE)
getWidget("Calculator", "cbx_CAL_dataset")$setSensitive(FALSE)
dataset <- getActiveData(getWidget("Calculator","cbx_CAL_dataset"))
fillListView(getWidget("Calculator","tvw_CAL_variables"),getNames(dataset),headers="Variável",FALSE)
#to use the same font
clearTv(getWidget("Calculator","txt_CAL_expression"))
getWidget("Calculator","lbl_CAL_variablename")$setText(varname)
}

on_txt_DO_newvar_changed <- function(widget,user.data)
{
object <- setLocale(getWidget("DataOperations","txt_DO_newvar")$getText())
dataset <- getActiveData(getWidget("DataOperations","cbx_DO_dataset"))
expression <- setLocale(getWidget("DataOperations","txt_DO_expression")$getText(),fixspc=FALSE)
if(object != "" & dataset != "")
	getWidget("DataOperations","btn_DO_calculator")$setSensitive(TRUE)
else
	getWidget("DataOperations","btn_DO_calculator")$setSensitive(FALSE)
if(any(c(expression,dataset,object) == "") | expression == " ")
	getWidget("DataOperations","btn_DO_add")$setSensitive(FALSE)
else
	getWidget("DataOperations","btn_DO_add")$setSensitive(TRUE)
}

on_txt_DO_expression_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("DataOperations","cbx_DO_dataset"))
object <- setLocale(getWidget("DataOperations","txt_DO_newvar")$getText())
expression <- setLocale(getWidget("DataOperations","txt_DO_expression")$getText(),fixspc=FALSE)
if(any(c(expression,dataset,object) == "") | expression == " ")
	getWidget("DataOperations","btn_DO_add")$setSensitive(FALSE)
else
	getWidget("DataOperations","btn_DO_add")$setSensitive(TRUE)
}