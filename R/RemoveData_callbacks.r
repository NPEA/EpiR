# removeData callback functions

# main buttons
on_btn_RD_help_clicked <- function(widget,user.data)
{
showHelp("RemoveData")
}

on_btn_RD_cancel_clicked <- function(widget,user.data)
{
closeWindow("RemoveData")
}

on_btn_RD_execute_clicked <- function(widget,user.data)
{
selected <- vectorToString(getListSelection(getWidget("RemoveData","tvw_RD_variables")))
cmd <- paste("remover.dados(\"",selected,"\")",sep="")

# translate
runCommand(cmd)
closeWindow("RemoveData")
}


# operations
on_tvw_RD_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("RemoveData","RD")
}

on_btn_RD_selectall_clicked <- function(widget,user.data)
{
getWidget("RemoveData","tvw_RD_variables")$getSelection()$selectAll()
toggleExecute("RemoveData","RD")
}



