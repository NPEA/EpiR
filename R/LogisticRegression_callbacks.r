# main buttons
on_btn_LG_help_clicked <- function(widget,user.data)
{
showHelp("Logistic")
}

on_btn_LG_cancel_clicked <- function(widget,user.data)
{
closeWindow("Logistic")
}

on_btn_LG_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("Logistic","cbx_LG_dataset"))
formula <-  setLocale(getWidget("Logistic","txt_LG_expression")$getText(),fixspc=FALSE)
object <- setLocale(getWidget("Logistic","txt_LG_object")$getText())
weigths <- getActiveData(getWidget("Logistic","cbx_LG_weigths"))
offset <- getActiveData(getWidget("Logistic","cbx_LG_offset"))
tests <- "no_objects"
if(!weigths == "")
	tests <- "weigths"

if(object != "")
	{
	if(tests == "no_objects")
		tests <- "object"
	else
		tests <- paste(tests, "_object",sep="")
	}

if(offset != "")
	{
	if(tests == "no_objects")
		tests <- "offset"
	else
		tests <- paste(tests, "_offset",sep="")
	}

if(getWidget("Logistic","chk_LG_testehlemeshow")$getActive())
	esthlemeshow <- "VERDADEIRO" 
else
	esthlemeshow <- "FALSO"

if(getWidget("Logistic","chk_LG_rchance")$getActive())
	odds.ratio <- "VERDADEIRO" 
else
	odds.ratio <- "FALSO"

if(getWidget("Logistic","chk_LG_D.Yhat")$getActive())
	D.yhat <- "VERDADEIRO" 
else
	D.yhat <- "FALSO"

if(getWidget("Logistic","chk_LG_beta.Yhat")$getActive())
	beta.Yhat <- "VERDADEIRO" 
else
	beta.Yhat <- "FALSO"

if(getWidget("Logistic","chk_LG_D.h")$getActive())
	D.h <- "VERDADEIRO" 
else
	D.h <- "FALSO"

if(getWidget("Logistic","chk_LG_X.Yhat")$getActive())
	X.Yhat <- "VERDADEIRO" 
else
	X.Yhat <- "FALSO"

if(getWidget("Logistic","chk_LG_beta.h")$getActive())
	beta.h <- "VERDADEIRO" 
else
	beta.h <- "FALSO"

if(getWidget("Logistic","chk_LG_X.h")$getActive())
	X.h <- "VERDADEIRO" 
else
	X.h <- "FALSO"


cmd <- switch(tests,
	no_objects = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	weigths = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",pesos=\"",weigths,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	object = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",objeto=\"",object,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	offset = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",offset=\"",offset,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	weigths_object = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",pesos=\"",weigths,"\",objeto=\"",object,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	weigths_offset = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",pesos=\"",weigths,"\",offset=\"",offset,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	object_offset = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",objeto=\"",object,"\",offset=\"",offset,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep=""),
	weigths_object_offset = paste("regressao.logistica(\"",dataset,"\",formula =\"",formula,"\",pesos=\"",weigths,"\",objeto=\"",object,"\",offset=\"",offset,"\",razao.chances=",odds.ratio,",teste.HLemeshow=",esthlemeshow,",D.por.Y.estimado=",D.yhat,",beta.por.Y.estimado=",beta.Yhat,",D.por.h=",D.h,",X.por.Y.estimado=",X.Yhat,",beta.por.h=",beta.h,",X.por.h=",X.h,")",sep="")	
	)


runCommand(cmd)
closeWindow("Logistic","LG")
}

# operations

on_cbx_LG_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("Logistic","cbx_LG_dataset"))
names.depvar <- NULL
for(i in names(eval(parse(text=dataset))))
	{
	if(!any(unique(eval(parse(text=paste(dataset,"$",i,sep="")))) %in% c(0,1) == FALSE))
		names.depvar <- c(names.depvar,i)
	}
fillComboBox(getWidget("Logistic","cbx_LG_depvar"),names.depvar)
fillListView(getWidget("Logistic","tvw_LG_variables"),getNames(dataset),TRUE)
fillComboBox(getWidget("Logistic","cbx_LG_weigths"),c("",getNumeric(dataset)))
fillComboBox(getWidget("Logistic","cbx_LG_offset"),c("",getNames(dataset)))
#if the dataset had been changed
eraser.text <- ""
getWidget("Logistic","txt_LG_expression")$setText(eraser.text)
getWidget("Logistic","btn_LG_execute")$setSensitive(FALSE)
getWidget("Logistic","btn_LG_add")$setSensitive(FALSE)
getWidget("Logistic","btn_LG_clear")$setSensitive(FALSE)
getWidget("Logistic","tvw_LG_variables")$setSensitive(FALSE)
}

on_cbx_LG_depvar_changed <- function(widget,user.data)
{
depvar_text <- getActiveData(getWidget("Logistic","cbx_LG_depvar"),fixspc=FALSE)
if(depvar_text != " ")
	{
	old_expression <- setLocale(getWidget("Logistic","txt_LG_expression")$getText(),fixspc=FALSE)
        new_expression <- depvar_text
        if(trim(old_expression) != "")
		{
		broked_expression <- strsplit(old_expression, split="~")
		broked_expression <- ifelse(!is.na(broked_expression[[1]][2]),broked_expression[[1]][2],"")
		new_expression <- trim(paste(new_expression, "~", broked_expression,sep=""))
		}
	else
		new_expression <- paste(new_expression,"~",sep="")
	getWidget("Logistic","txt_LG_expression")$setText(new_expression)	
	getWidget("Logistic","tvw_LG_variables")$setSensitive(TRUE)
	getWidget("Logistic","btn_LG_clear")$setSensitive(TRUE)
	}
}

on_tvw_LG_variables_button_release_event <- function(widget,user.data)
{
getWidget("Logistic","btn_LG_add")$setSensitive(TRUE)
}

on_btn_LG_add_clicked <- function(widget,user.data)
{
dep.var_text <- getListSelection(getWidget("Logistic","tvw_LG_variables"))	
old_text <-  setLocale(getWidget("Logistic","txt_LG_expression")$getText(),fixspc=FALSE)
try.class <- "character"
try(try.class <- as.formula(old_text),silent = TRUE)
if(class(try.class) == "character")
	new_text <- paste(old_text, dep.var_text, sep="")
else
	if(!is.null(dep.var_text))
		new_text <- paste(old_text,"+", dep.var_text, sep="")
	else
		new_text <- old_text
getWidget("Logistic","txt_LG_expression")$setText(new_text)
getWidget("Logistic","btn_LG_clear")$setSensitive(TRUE)
if(!is.null(dep.var_text))
	getWidget("Logistic","btn_LG_execute")$setSensitive(TRUE)
	
}
on_btn_LG_clear_clicked <- function(widget,user.data)
{
old_expression <- setLocale(getWidget("Logistic","txt_LG_expression")$getText())
new_expression <- strsplit(old_expression, split="~")
new_expression <- paste(new_expression[[1]][1],"~",sep="")
getWidget("Logistic","txt_LG_expression")$setText(new_expression)
getWidget("Logistic","btn_LG_execute")$setSensitive(FALSE)
getWidget("Logistic","tvw_LG_variables")$getSelection()$UnselectAll()
getWidget("Logistic","btn_LG_add")$setSensitive(FALSE)
}
on_tvw_LG_variables_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	
	{
		on_btn_LG_add_clicked()
	}
}