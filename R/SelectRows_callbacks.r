# selectRows callback functions

# main buttons
on_btn_SR_help_clicked <- function(widget,user.data)
{
showHelp("SelectRows")
}

on_btn_SR_cancel_clicked <- function(widget,user.data)
{
closeWindow("SelectRows")
}

on_btn_SR_execute_clicked <- function(widget,user.data)
{
query <- getTv(getWidget("SelectRows","txt_SR_filters"))
query <- trim(query)
if(query == "")
	{
	msgDialog("SelectRows","error","O campo de expressão está vazio.")
	return()
	}
else
	{
	dataset <- getActiveData(getWidget("SelectRows","cbx_SR_dataset"))
	object <- setLocale(getWidget("SelectRows","txt_SR_object")$getText())
	if(object == "")
		object <- paste("novo_",dataset,sep="")
	
	cmd <- paste("selecionar.linhas(\"",dataset,"\",\"",vectorToString(query,quoted=FALSE,coll=" "),"\",objeto=\"",object,"\")",sep="")
	
	dataset <- eval(parse(text = dataset))
	test <- try(eval(parse(text = query),dataset),silent=TRUE)
	
	if(class(test) == "try-error")
		{
		msgDialog("SelectRows","error","Há um erro na expressão. Corrija-a e tente novamente.")
		return()
		}
	else if(!any(test))
		{
		msgDialog("SelectRows","error","Nenhuma linha do banco de dados será selecionada.")
		return()
		}
	else
		{
		window <- getWidget("SelectRows")
		#Is it just a expression constructor?
		if(window$"transient-for"$name %in% c("MeanTest", "AssociationTest","VarTest","AggregateRows","FriedmanTest","RankTest","WilcoxonTest"))
			{
			short <- switch(window$"transient-for"$name,
				MeanTest = "MTST",
				AssociationTest = "AT",
				VarTest = "VT",
				AggregateRows = "AR",
				FriedmanTest = "FTST",
				RankTest = "RT",
				WilcoxonTest="WT",
				NULL)
			query <- paste(query,collapse="")
			getWidget(window$"transient-for"$name,paste("txt_",short,"_filter",sep=""))$setText(query)
			closeWindow("SelectRows")
			}
		else
			{
			# translate
			runCommand(cmd)
			closeWindow("SelectRows")
			}
		}
	}
}


# operations
on_cbx_SR_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("SelectRows","cbx_SR_dataset"))
if(dataset == "")
	getWidget("SelectRows","btn_SR_execute")$setSensitive(FALSE)
else
	getWidget("SelectRows","btn_SR_execute")$setSensitive(TRUE)
getWidget("SelectRows","txt_SR_object")$setText(paste("novo_",dataset,sep=""))
# setting values
if(!is.null(dataset))
	variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
else
	variables <- NULL
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,TRUE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,TRUE,"Operador relacional",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,TRUE,"Valores",sel.mode="multiple")
}

on_tvw_SR_variables_button_release_event <- function(widget,event,user.data)
{
dataset <- getActiveData(getWidget("SelectRows","cbx_SR_dataset"))
updateFilterValues(dataset)
toggleExecute("SelectRows","SR",type="variables",button="add")
relation <- getListSelection(getWidget("SelectRows","tvw_SR_relations"))
variable <- getListSelection(getWidget("SelectRows","tvw_SR_variables"))
values <- getListSelection(getWidget("SelectRows","tvw_SR_values"))
if(any(c(relation[1],variable[1],values[1]) == ""))
	getWidget("SelectRows","btn_SR_add")$setSensitive(FALSE)
else
	getWidget("SelectRows","btn_SR_add")$setSensitive(TRUE)
}

on_tvw_SR_relations_button_release_event <- function(widget,event,user.data)
{
relation <- getListSelection(getWidget("SelectRows","tvw_SR_relations"))
variable <- getListSelection(getWidget("SelectRows","tvw_SR_variables"))
values <- getListSelection(getWidget("SelectRows","tvw_SR_values"))
tvw_values <- getWidget("SelectRows","tvw_SR_values")
if(relation %in% c("==","!=","<","<=",">",">="))
	tvw_values$getSelection()$setMode("browse")
else
	tvw_values$getSelection()$setMode("multiple")
if(any(c(relation[1],variable[1],values[1]) == ""))
	getWidget("SelectRows","btn_SR_add")$setSensitive(FALSE)
else
	getWidget("SelectRows","btn_SR_add")$setSensitive(TRUE)
}

on_tvw_SR_values_button_release_event <- function(widget,event,user.data)
{
toggleExecute("SelectRows","SR",type="values",button="add")
relation <- getListSelection(getWidget("SelectRows","tvw_SR_relations"))
variable <- getListSelection(getWidget("SelectRows","tvw_SR_variables"))
values <- getListSelection(getWidget("SelectRows","tvw_SR_values"))
if(any(c(relation[1],variable[1],values[1]) == ""))
	getWidget("SelectRows","btn_SR_add")$setSensitive(FALSE)
else
	getWidget("SelectRows","btn_SR_add")$setSensitive(TRUE)
}

on_btn_SR_add_clicked <- function(widget,user.data)
{
oldquery <-  getTv(getWidget("SelectRows","txt_SR_filters"))
variable <- getListSelection(getWidget("SelectRows","tvw_SR_variables"),fixspc=FALSE)
relation <- getListSelection(getWidget("SelectRows","tvw_SR_relations"),fixspc=FALSE)
values <- getListSelection(getWidget("SelectRows","tvw_SR_values"),fixspc=FALSE)
if(any(is.na(as.numeric(values))))
	values <- paste("\"'",values,"'\"",sep="")
# building 
if(!is.null(relation))
	{	
	if(relation %in% c("pertence","não-pertence"))
		relation <- ifelse(relation=="pertence","%pertence%","%!pertence%")
	}
else
	{
	msgDialog("SelectRows", "error", "É necessário selecionar uma relação antes de continuar.")
	return()
	}
if(!is.null(values))	
	{
	if(length(values)==1)
		query <- paste("(",variable," ",relation," ",eval(parse(text=values),envir=.GlobalEnv),") ",sep="")
	else
		{
		values <- vectorToString(sapply(values,function(x)eval(parse(text=x),envir=.GlobalEnv)),quoted=FALSE)
		query <- paste("(",variable," ",relation," ","c(",values,")",") ",sep="")
		}
	}
else
	{
	msgDialog("SelectRows","error","É necessário selecionar ao menos um valor antes de continuar.")
	return()
	}

if(length(oldquery)>0)
	lastquery <- oldquery[length(oldquery)]
else
	lastquery <- ""
if (!(query %in% oldquery))
	newquery <- c(oldquery,query)
else
	{
	msgDialog("SelectRows","info","Este filtro já foi definido.")
	newquery <- oldquery
	}	
newquery <- paste(newquery, collapse="")
clearTv(getWidget("SelectRows","txt_SR_filters"))
insertTv(getWidget("SelectRows","txt_SR_filters"),newquery)
}

on_btn_SR_remove_clicked <- function(widget,user.data)
{
clearTv(getWidget("SelectRows","txt_SR_filters"))
}

on_btn_SR_open_clicked <- function(widget,user.data)
{
insertTv(getWidget("SelectRows","txt_SR_filters"),"(")
getWidget("SelectRows", "btn_SR_remove")$setSensitive(TRUE)
}

on_btn_SR_close_clicked <- function(widget,user.data)
{
insertTv(getWidget("SelectRows","txt_SR_filters"),")")
getWidget("SelectRows", "btn_SR_remove")$setSensitive(TRUE)
}

on_btn_SR_and_clicked <- function(widget,user.data)
{
insertTv(getWidget("SelectRows","txt_SR_filters"),"&")
getWidget("SelectRows", "btn_SR_remove")$setSensitive(TRUE)
}

on_btn_SR_OR_clicked <- function(widget,user.data)
{
insertTv(getWidget("SelectRows","txt_SR_filters"),"|")
getWidget("SelectRows", "btn_SR_remove")$setSensitive(TRUE)
}