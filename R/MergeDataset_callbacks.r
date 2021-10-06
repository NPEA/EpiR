on_btn_MD_help_clicked <- function(widget,user.data)
{
showHelp("MergeDataset")
}

on_btn_MD_cancel_clicked <- function(widget,user.data)
{
closeWindow("MergeDataset")
}

on_btn_MD_execute_clicked <- function(widget,user.data)
{
dataset1 <- getActiveData(getWidget("MergeDataset","cbx_MD_dataset"))
dataset2 <- getActiveData(getWidget("MergeDataset","cbx_MD_newDataset"))
link1 <- getActiveData(getWidget("MergeDataset","cbx_MD_link1"))
link2 <- getActiveData(getWidget("MergeDataset","cbx_MD_link2"))
variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
variables2 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables2")))
if(getWidget("MergeDataset","chk_MD_exclude1")$getActive())
	exclude1 <- "VERDADEIRO"
else
	exclude1 <- "FALSO"
if(getWidget("MergeDataset","chk_MD_exclude2")$getActive())
	exclude2 <- "VERDADEIRO"
else
	exclude2 <- "FALSO"
if(getWidget("MergeDataset","chk_MD_byrow")$getActive())
	by.row <- "VERDADEIRO"
else
	by.row <- "FALSO"
object <- setLocale(getWidget("MergeDataset","txt_MD_object")$getText())
if(by.row == "FALSO")
	cmd <- paste("combinar.bancos(banco1=\"",dataset1,"\",banco2=\"",dataset2,"\",variavel.ligacao1=\"",link1,"\",variavel.ligacao2=\"",link2,"\",selecao.variavel.1=c(\"",variables1,"\"),selecao.variavel.2=c(\"",variables2,"\"),exceto.selecionadas.1=",exclude1,",exceto.selecionadas.2=",exclude2,",mesmas.variaveis=",by.row,",objeto=\"",object,"\")",sep="")
else
	cmd <- paste("combinar.bancos(banco1=\"",dataset1,"\",banco2=\"",dataset2,"\",selecao.variavel.1=c(\"",variables1,"\"),exceto.selecionadas.1=",exclude1,",mesmas.variaveis=",by.row,",objeto=\"",object,"\")",sep="")
runCommand(cmd)
closeWindow("MergeDataset","MD")
}

on_cbx_MD_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MergeDataset","cbx_MD_dataset"))
fillComboBox(getWidget("MergeDataset","cbx_MD_link1"),getNames(dataset))
fillListView(getWidget("MergeDataset","tvw_MD_variables1"),getNames(dataset))
}

on_cbx_MD_link1_changed <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
link1 <- getActiveData(getWidget("MergeDataset","cbx_MD_link1"))
variables2 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables2")))
link2 <- getActiveData(getWidget("MergeDataset","cbx_MD_link2"))
if(variables1 != "" & variables2 != "" & link1 != "" & link2 != "" & object != "")
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
else
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
}

on_cbx_MD_link2_changed <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
link1 <- getActiveData(getWidget("MergeDataset","cbx_MD_link1"))
variables2 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables2")))
link2 <- getActiveData(getWidget("MergeDataset","cbx_MD_link2"))
by.row <- getWidget("MergeDataset","chk_MD_byrow")$getActive()
new.dataset <- getActiveData(getWidget("MergeDataset","cbx_MD_newDataset"))
if(variables1 != "" & variables2 != "" & link1 != "" & link2 != "" & object != "")
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
else
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
}

on_cbx_MD_newDataset_changed <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
dataset2 <- getActiveData(getWidget("MergeDataset","cbx_MD_newDataset"))
fillComboBox(getWidget("MergeDataset","cbx_MD_link2"),getNames(dataset2))
if(getWidget("MergeDataset","chk_MD_byrow")$getActive())
	{
	variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
	if(variables1 != "" & dataset2 != "" & object != "")
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
	}
else
	fillListView(getWidget("MergeDataset","tvw_MD_variables2"),getNames(dataset2))
}

on_tvw_MD_variables1_button_release_event <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
dataset2 <- getActiveData(getWidget("MergeDataset","cbx_MD_newDataset"))
variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
link1 <- getActiveData(getWidget("MergeDataset","cbx_MD_link1"))
if(getWidget("MergeDataset","chk_MD_byrow")$getActive())
	{
	if(variables1 != "" &  dataset2 != "" & object != "")
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
	else
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
	}
else
	{
	variables2 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables2")))
	link2 <- getActiveData(getWidget("MergeDataset","cbx_MD_link2"))
	if(variables1 != "" & variables2 != "" & link1 != "" & link2 != "" & object != "")
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
	else
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
	}
}

on_tvw_MD_variables2_button_release_event <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
link1 <- getActiveData(getWidget("MergeDataset","cbx_MD_link1"))
variables2 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables2")))
link2 <- getActiveData(getWidget("MergeDataset","cbx_MD_link2"))
if(variables1 != "" & variables2 != "" & link1 != "" & link2 != "" & object != "")
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
else
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
}

on_chk_MD_byrow_toggled <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
dataset1 <- getActiveData(getWidget("MergeDataset","cbx_MD_dataset"))
dataset2 <- getActiveData(getWidget("MergeDataset","cbx_MD_newDataset"))
if(getWidget("MergeDataset","chk_MD_byrow")$getActive())
	{	
	variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
	fillComboBox(getWidget("MergeDataset","cbx_MD_link1"),getNames(dataset1))
	fillComboBox(getWidget("MergeDataset","cbx_MD_link2"),getNames(dataset2))
	fillListView(getWidget("MergeDataset","tvw_MD_variables2"),NULL)
	getWidget("MergeDataset","cbx_MD_link1")$setSensitive(FALSE)
	getWidget("MergeDataset","cbx_MD_link2")$setSensitive(FALSE)
	getWidget("MergeDataset","tvw_MD_variables2")$setSensitive(FALSE)
	getWidget("MergeDataset","chk_MD_exclude2")$setActive(FALSE)
	getWidget("MergeDataset","chk_MD_exclude2")$setSensitive(FALSE)
	if(dataset1 != "" & dataset2 != "" & variables1 != "" & object != "")
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
	else
		getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
	}
else
	{
	fillListView(getWidget("MergeDataset","tvw_MD_variables2"),getNames(dataset2))
	getWidget("MergeDataset","cbx_MD_link1")$setSensitive(TRUE)
	getWidget("MergeDataset","cbx_MD_link2")$setSensitive(TRUE)
	getWidget("MergeDataset","tvw_MD_variables2")$setSensitive(TRUE)
	getWidget("MergeDataset","chk_MD_exclude2")$setSensitive(TRUE)
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
	}
}

on_txt_MD_object_changed <- function(widget,user.data)
{
object <- getWidget("MergeDataset","txt_MD_object")$getText()
if(object != "")
	{
	if(getWidget("MergeDataset","chk_MD_byrow")$getActive())
		{
		dataset1 <- getActiveData(getWidget("MergeDataset","cbx_MD_dataset"))
		dataset2 <- getActiveData(getWidget("MergeDataset","cbx_MD_newDataset"))
		variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
		if(dataset1 != "" & dataset2 != "" & variables1 != "" & object != "")
			getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
		else
			getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
		}
	else
		{
		variables1 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables1")))
		link1 <- getActiveData(getWidget("MergeDataset","cbx_MD_link1"))
		variables2 <- vectorToString(getListSelection(getWidget("MergeDataset","tvw_MD_variables2")))
		link2 <- getActiveData(getWidget("MergeDataset","cbx_MD_link2"))
		if(variables1 != "" & variables2 != "" & link1 != "" & link2 != "")
			getWidget("MergeDataset","btn_MD_execute")$setSensitive(TRUE)
		}
	}
else
	getWidget("MergeDataset","btn_MD_execute")$setSensitive(FALSE)
}