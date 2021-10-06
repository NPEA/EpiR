# main buttons
on_btn_RV_help_clicked <- function(widget,user.data)
{
showHelp("RecodeVar")
}

on_btn_RV_cancel_clicked <- function(widget,user.data)
{
closeWindow("RecodeVar")
}

on_btn_RV_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"))
variable <- getActiveData(getWidget("RecodeVar","cbx_RV_variable"))
new_factor <- getListViewData(getWidget("RecodeVar","tvw_RV_viewFactor"),2)
new_factor <- paste("'",new_factor,"'",sep="")
new_factor <- strsplit(paste(paste(new_factor, ",",sep=""),collapse=""), "")
new_factor <- paste(new_factor[[1]][-length(new_factor[[1]])],collapse="")
new_factor <- paste("c(",new_factor,")",sep="")
na.level <- getWidget("RecodeVar","chk_RV_NAFactor")$getActive()
if(na.level)
	na.level <- "VERDADEIRO"
else
	na.level <- "FALSO"

if(gtkNotebookGetCurrentPage(getWidget("RecodeVar","nbk_RV_options"))==1)
	factor.var.rbutton <- "VERDADEIRO"
else
	factor.var.rbutton <- "FALSO"
object <- setLocale(getWidget("RecodeVar", "txt_RV_object")$getText())
if(factor.var.rbutton=="FALSO")
	{
	if(getWidget("RecodeVar","rb_RV_values")$getActive())
		{
		options <- "valores"
		values <- paste("c(",getWidget("RecodeVar","txt_RV_values")$getText(),")",sep="")
		}
	else if(getWidget("RecodeVar","rb_RV_centile")$getActive())
		options <- "quintis"
	else if(getWidget("RecodeVar","rb_RV_quantile")$getActive())
		options <- "quartis"
	else if(getWidget("RecodeVar","rb_RV_median")$getActive())
		options <- "mediana"
	else
		{
		options <- "percentis"
		probs <- paste("c(",getWidget("RecodeVar","txt_RV_percentiles")$getText(),")",sep="")
		}
		
	}
ref <- getActiveData(getWidget("RecodeVar","cbx_RV_refFactor"))
order.factor <- getWidget("RecodeVar","chk_RV_order")$getActive()
if(order.factor)
	order.factor <- "VERDADEIRO"
else
	order.factor <- "FALSO"
rigth <- !getWidget("RecodeVar","chk_RV_left_include")$getActive()
if(rigth)
	rigth <- "VERDADEIRO"
else
	rigth <- "FALSO"

variable.as.numeric <- getWidget("RecodeVar","chk_RV_asnumeric")$getActive()
if(variable.as.numeric)
	variable.as.numeric <- "VERDADEIRO"
else
	variable.as.numeric <- "FALSO"
if(variable.as.numeric == "VERDADEIRO")
	cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",como.numerico=",variable.as.numeric,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
else
	{
	if(eval(parse(text = new_factor))[1] == "")
		{
		if(factor.var.rbutton=="VERDADEIRO")
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",numerico=FALSO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
		else
			{
			if(options=="valores")
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",categorizar.por=\"",options,"\",valores=",values,",numerico=VERDADEIRO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",incluir.esquerda=",rigth,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
			else if(options=="percentis")
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",categorizar.por=\"",options,"\",probabilidades=",probs,",numerico=VERDADEIRO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",incluir.esquerda=",rigth,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
			else
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",categorizar.por=\"",options,"\",numerico=VERDADEIRO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",incluir.esquerda=",rigth,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")			
			}
		}
	else
		{
		if(factor.var.rbutton=="VERDADEIRO")
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",nova.categoria=",new_factor,",numerico=FALSO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
		else
			{
			if(options=="valores")
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",categorizar.por=\"",options,"\",valores=",values,",nova.categoria=",new_factor,",numerico=VERDADEIRO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",incluir.esquerda=",rigth,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
			else if(options=="percentis")
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",categorizar.por=\"",options,"\",probabilidades=",probs,",nova.categoria=",new_factor,",numerico=VERDADEIRO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",incluir.esquerda=",rigth,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
			else
				cmd <- paste("categorizar.variavel(\"",dataset,"\",variavel=\"",variable,"\",categorizar.por=\"",options,"\",nova.categoria=",new_factor,",numerico=VERDADEIRO,ordenar.fator=",order.factor,",categoria.referencia=\"",ref,"\",incluir.esquerda=",rigth,",objeto=\"",object,"\",NA.como.fator=",na.level,")",sep="")
			}
		}
	}	

runCommand(cmd)
closeWindow("RecodeVar", "RV")
}

# operations

on_cbx_RV_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"))
fillComboBox(getWidget("RecodeVar","cbx_RV_variable"),getNames(dataset))
#reset window
getWidget("RecodeVar","nbk_RV_options")$setSensitive(TRUE)
getWidget("RecodeVar","chk_RV_asnumeric")$setSensitive(TRUE)
getWidget("RecodeVar","chk_RV_NAFactor")$setSensitive(TRUE)
getWidget("RecodeVar","btn_RV_apply")$setSensitive(TRUE)
getWidget("RecodeVar","btn_RV_reload")$setSensitive(FALSE)
getWidget("RecodeVar","lbl_RV_load")$setText("Aplicar")
fillComboBox(getWidget("RecodeVar","cbx_RV_refFactor"),"")

}

on_cbx_RV_variable_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"))
variable <- trim(getActiveData(getWidget("RecodeVar","cbx_RV_variable"),fixspc=FALSE))
fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"),NULL)
na_like_factor <- getWidget("RecodeVar","chk_RV_NAFactor")$getActive()
if(getWidget("RecodeVar","chk_RV_asnumeric")$getActive())
	{
	if(variable != "")
		getWidget("RecodeVar","btn_RV_execute")$setSensitive(TRUE)
	else
		getWidget("RecodeVar","btn_RV_execute")$setSensitive(FALSE)
	}
else
	{
	if(variable != "")
		{
		getWidget("RecodeVar","btn_RV_apply")$setSensitive(TRUE)
		factor.columns <- sort(unique(eval(parse(text=paste(dataset,"$",variable,sep="")))),na.last=TRUE)
		if(!na_like_factor)
			factor.columns <- factor.columns[!is.na(factor.columns)]
		factor.columns <- as.character(factor.columns)
		if(gtkNotebookGetCurrentPage(getWidget("RecodeVar","nbk_RV_options"))==1)
			fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),factor.columns)
		}
	else
		getWidget("RecodeVar","btn_RV_apply")$setSensitive(FALSE)
	}
}

on_nbk_RV_options_switch_page <- function(widget,page,page.num,user.data)
{
dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"),fixspc=FALSE)
variable <- trim(getActiveData(getWidget("RecodeVar","cbx_RV_variable"),fixspc=FALSE))
fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"),NULL)
getWidget("RecodeVar","btn_RV_reload")$setSensitive(FALSE)
na_like_factor <- getWidget("RecodeVar","chk_RV_NAFactor")$getActive()
if(page.num==1)
	{
	getWidget("RecodeVar","chk_RV_left_include")$setSensitive(FALSE)
	getWidget("RecodeVar","chk_RV_left_include")$setActive(FALSE)
	if(variable != "")
		{
		factor.columns <- unique(eval(parse(text=paste(dataset,"$",variable,sep=""))))
		if(na_like_factor)
			factor.columns <- as.character(sort(factor.columns,na.last=TRUE))
		else
			factor.columns <- as.character(sort(factor.columns))
		if(is.null(getWidget("RecodeVar","tvw_RV_oldFactor")$getColumns()[1][[1]]))
			fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),factor.columns,update=FALSE)
		else			
			fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),factor.columns,update=TRUE)
		}
	}
else
	getWidget("RecodeVar","chk_RV_left_include")$setSensitive(TRUE)	
}

on_btn_RV_apply_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"))
variable <- getActiveData(getWidget("RecodeVar","cbx_RV_variable"))
na_like_factor <- getWidget("RecodeVar","chk_RV_NAFactor")$getActive()

if(gtkNotebookGetCurrentPage(getWidget("RecodeVar","nbk_RV_options"))==1)
	{
	new_factor <- getListViewData(getWidget("RecodeVar","tvw_RV_oldFactor"))
	if(!na_like_factor)
		new_factor <- new_factor[!is.na(new_factor)]
	if(is.null(getWidget("RecodeVar","tvw_RV_viewFactor")$getColumns()[1][[1]]))
		fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"), list(new_factor, rep("",length(new_factor))),update=FALSE,sel.mode="single", headers=c("Categoria original","Nova categoria"))
	else
		fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"), list(new_factor,rep("",length(new_factor))),update=TRUE,sel.mode="single",headers=c("Categoria original","Nova categoria"))
# 	fillComboBox(getWidget("RecodeVar","cbx_RV_refFactor"),new_factor)
	}
else
	{	
	if(getWidget("RecodeVar","rb_RV_values")$getActive())
		{		
		breaks <- paste("c(",getWidget("RecodeVar","txt_RV_values")$getText(),")",sep="")		
		old_factor <- unique(recodeVar(dataset = dataset,x=variable,numeric=TRUE,options="values",breaks=eval(parse(text=breaks)),probs=NULL, return=TRUE,na.level=na_like_factor))
		}
	else if(getWidget("RecodeVar","rb_RV_centile")$getActive())
		old_factor <- unique(recodeVar(dataset = dataset,x=variable,numeric=TRUE,options="centiles",breaks=NULL,probs=NULL, return=TRUE,na.level=na_like_factor))
	else if(getWidget("RecodeVar","rb_RV_quantile")$getActive())
		old_factor <- unique(recodeVar(dataset = dataset,x=variable,numeric=TRUE,options="quantile",breaks=NULL,probs=NULL, return=TRUE,na.level=na_like_factor))
	else if(getWidget("RecodeVar","rb_RV_median")$getActive())
		old_factor <- unique(recodeVar(dataset = dataset,x=variable,numeric=TRUE,options="median",breaks=NULL,probs=NULL, return=TRUE,na.level=na_like_factor))
	else
		{
		probs <- paste("c(",getWidget("RecodeVar","txt_RV_percentiles")$getText(),")",sep="")
		old_factor <- unique(recodeVar(dataset = dataset,x=variable,numeric=TRUE,options="percentiles",breaks=NULL,probs=eval(parse(text=probs)), return=TRUE,na.level=na_like_factor))
		}
	if(!na_like_factor)
		old_factor <- old_factor[!is.na(old_factor)]
	new_factor <- rep("",length(old_factor))
	list_factor <- list(Categoria_original=old_factor,Novo_fator=new_factor)		
	if(is.null(getWidget("RecodeVar","tvw_RV_viewFactor")$getColumns()[1][[1]]))
		fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"), list_factor, update=FALSE, headers=c("Categoria original","Nova categoria"))
	else
		fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"), list_factor, update=TRUE, headers=c("Categoria modificada","Renomear"))
	getWidget("RecodeVar","btn_RV_reload")$setSensitive(TRUE)
# 	fillComboBox(getWidget("RecodeVar","cbx_RV_refFactor"),old_factor)
	}
}

on_tvw_RV_viewFactor_row_activated <- function(widget,path,column,user.data)
{
tv <- getWidget("RecodeVar","tvw_RV_viewFactor")
model <- tv$getModel()
path <- gtkTreePathGetIndices(path)
old_factor <- getListViewData(getWidget("RecodeVar","tvw_RV_viewFactor"))
old_factor <- old_factor[as.numeric(path)+1]
setWindow("ChangeLevel","RecodeVar")
getWidget("ChangeLevel","lbl_CHL_oldLevel")$setText(old_factor)
}

on_btn_RV_reload_clicked <- function(widget,user.data)
{
button_name <- setLocale(getWidget("RecodeVar","lbl_RV_load")$getText())
object <- setLocale(getWidget("RecodeVar", "txt_RV_object")$getText())
if(button_name == "Aplicar")
	{
	getWidget("RecodeVar","nbk_RV_options")$setSensitive(FALSE)
    	getWidget("RecodeVar","btn_RV_apply")$setSensitive(FALSE)
	getWidget("RecodeVar","chk_RV_NAFactor")$setSensitive(FALSE)
	getWidget("RecodeVar","cbx_RV_variable")$setSensitive(FALSE)
	if(object != "")
		getWidget("RecodeVar","btn_RV_execute")$setSensitive(TRUE)
	getWidget("RecodeVar","lbl_RV_load")$setText("Voltar")	
	getWidget("RecodeVar","chk_RV_asnumeric")$setSensitive(FALSE)
	new_factor <- getListViewData(getWidget("RecodeVar","tvw_RV_viewFactor"),2)
	if(gtkNotebookGetCurrentPage(getWidget("RecodeVar","nbk_RV_options"))==0) #numerical variable
		{
		if(new_factor[1] == "")
			new_factor <- getListViewData(getWidget("RecodeVar","tvw_RV_viewFactor"),1)
			if(object != "")
				getWidget("RecodeVar","btn_RV_execute")$setSensitive(TRUE)
		}
	else
		getWidget("RecodeVar","tvw_RV_oldFactor")$getSelection()$UnselectAll()
	fillComboBox(getWidget("RecodeVar","cbx_RV_refFactor"),unique(new_factor),first=TRUE)
	}
else
	{
	variable <- getActiveData(getWidget("RecodeVar","cbx_RV_variable"))
	fillComboBox(getWidget("RecodeVar","cbx_RV_refFactor"), "")
	getWidget("RecodeVar","nbk_RV_options")$setSensitive(TRUE)
	getWidget("RecodeVar","btn_RV_apply")$setSensitive(TRUE)
	fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"),NULL)
	getWidget("RecodeVar","lbl_RV_load")$setText("Aplicar")
	getWidget("RecodeVar","btn_RV_reload")$setSensitive(FALSE)
	getWidget("RecodeVar","btn_RV_execute")$setSensitive(FALSE)
	getWidget("RecodeVar","chk_RV_asnumeric")$setSensitive(TRUE)
	getWidget("RecodeVar","chk_RV_NAFactor")$setSensitive(TRUE)
	getWidget("RecodeVar","cbx_RV_variable")$setSensitive(TRUE)
	}
}

on_txt_RV_object_changed <- function(widget,user.data)
{
object <- setLocale(getWidget("RecodeVar", "txt_RV_object")$getText())
label_apply <- getWidget("RecodeVar","lbl_RV_load")$getText()
copy.numeric <- getWidget("RecodeVar","chk_RV_asnumeric")$getActive()
if(object=="")
	getWidget("RecodeVar","btn_RV_execute")$setSensitive(FALSE)
else if(label_apply == "Voltar" | copy.numeric)
	getWidget("RecodeVar","btn_RV_execute")$setSensitive(TRUE)
}

on_chk_RV_asnumeric_toggled <- function(widget,user.data)
{
object <- getWidget("RecodeVar","txt_RV_object")$getText()
if(getWidget("RecodeVar","chk_RV_asnumeric")$getActive())
	{
	variable <- trim(getActiveData(getWidget("RecodeVar","cbx_RV_variable"),fixspc=FALSE))	
# 	getWidget("RecodeVar","txt_RV_object")$setText("")
# 	getWidget("RecodeVar","txt_RV_object")$setSensitive(FALSE)
	getWidget("RecodeVar","nbk_RV_options")$setSensitive(FALSE)
	getWidget("RecodeVar","hbtn_RV_preview")$setSensitive(FALSE)
	getWidget("RecodeVar","frm_RV_options")$setSensitive(FALSE)
	getWidget("RecodeVar","chk_RV_NAFactor")$setSensitive(FALSE)
	getWidget("RecodeVar","chk_RV_NAFactor")$setActive(FALSE)
	fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"),NULL)
	fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),NULL)
	if(variable != "" & object != "")
		getWidget("RecodeVar","btn_RV_execute")$setSensitive(TRUE)
	else
		getWidget("RecodeVar","btn_RV_execute")$setSensitive(FALSE)
	getWidget("RecodeVar","btn_RV_reload")$setSensitive(FALSE)
	}
else
	{
	getWidget("RecodeVar","nbk_RV_options")$setSensitive(TRUE)
	getWidget("RecodeVar","hbtn_RV_preview")$setSensitive(TRUE)
	getWidget("RecodeVar","frm_RV_options")$setSensitive(TRUE)
	getWidget("RecodeVar","btn_RV_execute")$setSensitive(FALSE)
	getWidget("RecodeVar","chk_RV_NAFactor")$setSensitive(TRUE)
	variables <- getActiveData(getWidget("RecodeVar","cbx_RV_variable"))
	if(gtkNotebookGetCurrentPage(getWidget("RecodeVar","nbk_RV_options"))==1 & variables != "")
		{
		dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"))
		factor.columns <- unique(eval(parse(text=paste(dataset,"$",variables,sep=""))))
		factor.columns <- as.character(factor.columns[!is.na(factor.columns)])
		fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),factor.columns)
		}
	getWidget("RecodeVar","btn_RV_execute")$setSensitive(FALSE)
# 	getWidget("RecodeVar","txt_RV_object")$setSensitive(TRUE)
	}	
}

on_chk_RV_NAFactor_toggled <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("RecodeVar","cbx_RV_dataset"))
variable <- getActiveData(getWidget("RecodeVar","cbx_RV_variable"))
na_like_factor <- getWidget("RecodeVar","chk_RV_NAFactor")$getActive()
fillListView(getWidget("RecodeVar","tvw_RV_viewFactor"),NULL)
fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),NULL)
if(variable != "")
	{
	old_factor <- unique(eval(parse(text = paste(dataset,"$",variable,sep=""))))
	if(!na_like_factor)
		old_factor <- sort(old_factor)
	else
		old_factor <- sort(old_factor,na.last=TRUE)
	fillListView(getWidget("RecodeVar","tvw_RV_oldFactor"),as.character(old_factor))
	}
}