# manageTrash callback functions

# main buttons
on_btn_MT_help_clicked <- function(widget,user.data)
{
showHelp("ManageTrash")
}

on_btn_MT_cancel_clicked <- function(widget,user.data)
{
closeWindow("ManageTrash")
}

on_btn_MT_execute_clicked <- function(widget,user.data)
{
selected <- vectorToString(getListSelection(getWidget("ManageTrash","tvw_MT_variables")))
cmd <- paste("restaurar.excluidos(\"",selected,"\")",sep="")

# translate
runCommand(cmd)
closeWindow("ManageTrash")
}


# operations
on_tvw_MT_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("ManageTrash","MT")
}

on_btn_MT_empty_clicked <- function(widget,user.data)
{
if(askDialog("ManageTrash","Os objetos de dados serão apagados definitivamente.\nDeseja continuar?")=="no")
	return()
cmd <- paste("esvaziar.lixeira(confirmar=VERDADEIRO)",sep="")

# translate
runCommand(cmd)
closeWindow("ManageTrash")
}

on_btn_MT_selectall_clicked <- function(widget,user.data)
{
getWidget("ManageTrash","tvw_MT_variables")$getSelection()$selectAll()
toggleExecute("ManageTrash","MT")
}


