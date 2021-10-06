# correlation callbaack functions
# main buttons
on_btn_CO_help_clicked <- function(widget,user.data)
{
showHelp("Correlation")
}

on_btn_CO_cancel_clicked <- function(widget,user.data)
{
closeWindow("Correlation")
}

on_btn_CO_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("Correlation","cbx_CO_dataset"))
by <- getActiveData(getWidget("Correlation","cbx_CO_by"))
variables <- vectorToString(getListSelection(getWidget("Correlation","tvw_CO_variables")))
covariance <- getWidget("Correlation", "rb_CO_cov")$getActive()
if(getWidget("Correlation", "chk_CO_pvalue")$getActive())
	p.value <- "VERDADEIRO"
else
	p.value <- "FALSO"

if(getWidget("Correlation", "chk_CO_na")$getActive())
	n.print <- "VERDADEIRO"
else
	n.print <- "FALSO"
if(getWidget("Correlation","rb_CO_spearman")$getActive())
	method <- "spearman"
else if(getWidget("Correlation","rb_CO_kendall")$getActive())
	method <- "kendall"
else
	method <- "pearson"
if(getWidget("Correlation","chk_CO_pairwise")$getActive())
	pairwise <- "VERDADEIRO"
else
	pairwise <- "FALSO"

if(!covariance)
#translate
	cmd <- paste("correlacao(\"",dataset,"\",variaveis=\"",variables,"\",estratos=\"",by,"\",metodo=\"",method,"\",valor.p=",p.value,",elementos.validos=",n.print,",excluir.2.a.2=",pairwise,")",sep="")
else
	cmd <- paste("correlacao(\"",dataset,"\",variaveis=\"",variables,"\",estratos=\"",by,"\",metodo=\"",method,"\",valor.p=",p.value,",elementos.validos=",n.print,",covariancia=VERDADEIRO",",excluir.2.a.2=",pairwise,")",sep="")

runCommand(cmd)
closeWindow("Correlation","CO")
}
# operations
on_cbx_CO_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("Correlation","cbx_CO_dataset"))
if(dataset != "")
	{
	fillComboBox(getWidget("Correlation","cbx_CO_by"),c("",getBy(dataset)))
	fillListView(getWidget("Correlation","tvw_CO_variables"),getNumeric(dataset),TRUE)
	}
else
	{
	fillComboBox(getWidget("Correlation","cbx_CO_by"),c(""))
	fillListView(getWidget("Correlation","tvw_CO_variables"),NULL,TRUE)
	}
}

on_rb_CO_corr_toggled <- function(widget,user.data)
{
correlation <- getWidget("Correlation","rb_CO_corr")$getActive()
if(correlation)
	getWidget("Correlation","frm_CO_method")$setSensitive(TRUE)
else
	getWidget("Correlation","frm_CO_method")$setSensitive(FALSE)
}

on_tvw_CO_variables_button_release_event <- function(widget,user.data)
{
variables <- getListSelection(getWidget("Correlation","tvw_CO_variables"))
if(!is.null(variables))
	getWidget("Correlation","btn_CO_execute")$setSensitive(TRUE)
else
	getWidget("Correlation","btn_CO_execute")$setSensitive(FALSE)
}