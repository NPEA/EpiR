# main buttons
on_btn_LR_help_clicked <- function(widget,user.data)
{
showHelp("LinearRegression")
}

on_btn_LR_cancel_clicked <- function(widget,user.data)
{
closeWindow("LinearRegression")
}

on_btn_LR_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("LinearRegression","cbx_LR_dataset"))
formula <-  setLocale(getWidget("LinearRegression","txt_LR_expression")$getText())
object <- setLocale(getWidget("LinearRegression","txt_LR_object")$getText())
weigths <- getActiveData(getWidget("LinearRegression","cbx_LR_weigths"))
if(getWidget("LinearRegression","chk_LR_nointercept")$getActive())
	no.intercept <- "VERDADEIRO"
else
	no.intercept <- "FALSO"
if(!weigths == "")
	weigths <- paste("\"",weigths,"\"", sep="")
else
	weigths <- "NULO"
if(object == "")
	object <- NULO
if(getWidget("LinearRegression","chk_LR_anov")$getActive())
	anova <- "VERDADEIRO" else
	anova <- "FALSO"
if(getWidget("LinearRegression","chk_LR_DW")$getActive())
	dwatson <- "VERDADEIRO" else
	dwatson <- "FALSO"
if(getWidget("LinearRegression","chk_LR_GQ")$getActive())
	goldfQ <- "VERDADEIRO" else
	goldfQ <- "FALSO"
if(is.null(object))
cmd <- paste("regressao.linear(\"",dataset,"\",formula =\"",formula,"\",goldfeld.quandt=",goldfQ,",durbin.watson=",dwatson,",anova=",anova,",pesos=",weigths,",retirar.intercepto=",no.intercept,")",sep="")
else
cmd <- paste("regressao.linear(\"",dataset,"\",formula =\"",formula,"\",goldfeld.quandt=",goldfQ,",durbin.watson=",dwatson,",objeto=\"",object,"\",anova=",anova,",pesos=",weigths,",retirar.intercepto=",no.intercept,")",sep="")

runCommand(cmd)
closeWindow("LinearRegression","LR")
}

# operations

on_cbx_LR_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("LinearRegression","LR","numeric", by = FALSE)
dataset <- getActiveData(getWidget("LinearRegression","cbx_LR_dataset"))
fillComboBox(getWidget("LinearRegression","cbx_LR_depvar"),c("",getNumeric(dataset)))
fillListView(getWidget("LinearRegression","tvw_LR_variables"),getNames(dataset),TRUE)
fillComboBox(getWidget("LinearRegression","cbx_LR_weigths"),c("",getNumeric(dataset)))
#if the dataset had been changed
eraser.text <- ""
getWidget("LinearRegression","txt_LR_expression")$setText(eraser.text)
getWidget("LinearRegression","btn_LR_execute")$setSensitive(FALSE)
getWidget("LinearRegression","btn_LR_add")$setSensitive(FALSE)
getWidget("LinearRegression","btn_LR_clear")$setSensitive(FALSE)
getWidget("LinearRegression","tvw_LR_variables")$setSensitive(FALSE)
}

on_cbx_LR_depvar_changed <- function(widget,user.data)
{
depvar_text <- getActiveData(getWidget("LinearRegression","cbx_LR_depvar"))
if(trim(depvar_text) != "")
	{
	func_text <- getActiveData(getWidget("LinearRegression","cbx_LR_func"),fix=FALSE)
	old_expression <- setLocale(getWidget("LinearRegression","txt_LR_expression")$getText())
	if(trim(func_text) != "")
		{
		if(func_text == "raiz")
			new_expression <- paste("raiz2(",depvar_text,")",sep="") 
		else if(func_text == "quadratica")
			new_expression <- paste("quadrado(",depvar_text,")",sep="")
		else if(func_text == "inversa")
			new_expression <- paste("inversa(",depvar_text,")",sep="")
		else
			new_expression <- paste("log(",depvar_text,")",sep="")
		}
	else
		new_expression <- depvar_text
	
	if(trim(old_expression) != "")
		{
		broked_expression <- strsplit(old_expression, split="~")
		broked_expression <- ifelse(!is.na(broked_expression[[1]][2]),broked_expression[[1]][2],"")
		new_expression <- trim(paste(new_expression, "~", broked_expression,sep=""))
		}
	else
		new_expression <- paste(new_expression,"~",sep="")
	getWidget("LinearRegression","txt_LR_expression")$setText(new_expression)
	getWidget("LinearRegression","tvw_LR_variables")$setSensitive(TRUE)
	getWidget("LinearRegression","btn_LR_clear")$setSensitive(TRUE)
	}
}

on_btn_LR_add_clicked <- function(widget,user.data)
{
dep.var_text <- getListSelection(getWidget("LinearRegression","tvw_LR_variables"))	
old_text <-  setLocale(getWidget("LinearRegression","txt_LR_expression")$getText())
try.class <- "character"
try(try.class <- as.formula(old_text),silent = TRUE)
if(class(try.class) == "character")
	new_text <- paste(old_text, dep.var_text, sep="")
else
	if(!is.null(dep.var_text))
		new_text <- paste(old_text,"+", dep.var_text, sep="")
	else
		new_text <- old_text
getWidget("LinearRegression","txt_LR_expression")$setText(new_text)
getWidget("LinearRegression","btn_LR_clear")$setSensitive(TRUE)
if(!is.null(dep.var_text))
	getWidget("LinearRegression","btn_LR_execute")$setSensitive(TRUE)
}

on_btn_LR_clear_clicked <- function(widget,user.data)
{
old_expression <- setLocale(getWidget("LinearRegression","txt_LR_expression")$getText())
new_expression <- strsplit(old_expression, split="~")
new_expression <- paste(new_expression[[1]][1],"~",sep="")
getWidget("LinearRegression","txt_LR_expression")$setText(new_expression)
getWidget("LinearRegression","btn_LR_execute")$setSensitive(FALSE)
getWidget("LinearRegression","tvw_LR_variables")$getSelection()$UnselectAll()
getWidget("LinearRegression","btn_LR_add")$setSensitive(FALSE)
}
on_tvw_LR_variables_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	
	{
		on_btn_LR_add_clicked()
	}
}

on_cbx_LR_func_changed <- function(widget, user.data)
{
depvar_text <- getActiveData(getWidget("LinearRegression","cbx_LR_depvar"))
func_text <- getActiveData(getWidget("LinearRegression","cbx_LR_func"),fix=FALSE)
old_expression <- setLocale(getWidget("LinearRegression","txt_LR_expression")$getText())
if(depvar_text !="")
	{
	if(trim(func_text) != "")
		{
		if(func_text == "raiz")
			new_expression <- paste("raiz2(",depvar_text,")",sep="") 
		else if(func_text == "quadratica")
			new_expression <- paste("quadrado(",depvar_text,")",sep="")
		else if(func_text == "inversa")
			new_expression <- paste("inversa(",depvar_text,")",sep="")
		else
			new_expression <- paste("log(",depvar_text,")",sep="")
		}
	else
		new_expression <- depvar_text
	broked_expression <- strsplit(old_expression, split="~")
	broked_expression <- ifelse(!is.na(broked_expression[[1]][2]),broked_expression[[1]][2],"")
	new_expression <- trim(paste(new_expression, "~", broked_expression,sep=""))
	getWidget("LinearRegression","txt_LR_expression")$setText(new_expression)
	}
}

on_tvw_LR_variables_button_release_event <- function(widget,user.data)
{
getWidget("LinearRegression","btn_LR_add")$setSensitive(TRUE)
}