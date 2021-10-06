# dropColumns callback functions

# main buttons
on_btn_DC_help_clicked <- function(widget,user.data)
{
showHelp("DropColumns")
}

on_btn_DC_cancel_clicked <- function(widget,user.data)
{
closeWindow("DropColumns")
}

on_btn_DC_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("DropColumns","cbx_DC_dataset"))
selected <- vectorToString(getListSelection(getWidget("DropColumns","tvw_DC_variables")))
object <- setLocale(getWidget("DropColumns","txt_DC_object")$getText())
all.but <- ifelse(getWidget("DropColumns","chk_DC_allbut")$getActive(),TRUE,FALSE) 
if (all.but)
	cmd <- paste("excluir.colunas(\"",dataset,"\",\"",selected,"\",objeto=\"",object,"\",exceto=VERDADEIRO)",sep="")
else
	cmd <- paste("excluir.colunas(\"",dataset,"\",\"",selected,"\",objeto=\"",object,"\",exceto=FALSO)",sep="")

# translate
runCommand(cmd)
closeWindow("DropColumns")
}


# operations
on_cbx_DC_dataset_changed <- function(widget,user.data)
{
getWidget("DropColumns","txt_DC_object")$setText(getActiveData(getWidget("DropColumns","cbx_DC_dataset")))
fillStatsHeader("DropColumns","DC","all",by=FALSE)
}

on_tvw_DC_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("DropColumns","DC")
}



