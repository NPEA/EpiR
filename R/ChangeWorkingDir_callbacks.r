# ChangeWorkingDir callback functions

# main buttons
on_btn_CWD_help_clicked <- function(widget,user.data)
{
showHelp("ChangeWorkingDir")
}

on_btn_CWD_cancel_clicked <- function(widget,user.data)
{
closeWindow("ChangeWorkingDir")
}

on_btn_CWD_execute_clicked <- function(widget,user.data)
{
dirname <- setLocale(getWidget("ChangeWorkingDir","txt_CWD_newdir")$getText())

cmd <- paste("alterar.diretorio(\"",dirname,"\")",sep="")

# translate
runCommand(cmd)
closeWindow("ChangeWorkingDir")
}
	
# operations
on_btn_CWD_browse_clicked <- function(widget,user.data)
{
dirname <- fileDialog("ChangeWorkingDir","Selecionar diretório de trabalho","select-folder",NULL)$folder
if (!is.null(dirname))
	txtNewdir <- getWidget("ChangeWorkingDir","txt_CWD_newdir")$setText(dirname)
}

	
on_txt_CWD_newdir_changed <- function(widget,user.data)
{
toggleExecute("ChangeWorkingDir","CWD",type="newdir",button="execute",widget="textedit")
}


