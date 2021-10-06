# briefSummary callbaack functions

# main buttons
on_btn_BS_help_clicked <- function(widget,user.data)
{
showHelp("BriefSummary")
}

on_btn_BS_cancel_clicked <- function(widget,user.data)
{
closeWindow("BriefSummary")
}

on_btn_BS_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("BriefSummary","cbx_BS_dataset"))
by <- getActiveData(getWidget("BriefSummary","cbx_BS_by"))
vars <- vectorToString(getListSelection(getWidget("BriefSummary","tvw_BS_variables")))

# translate
if(by!="")
	cmd <- paste("resumo.breve(\"",dataset,"\",\"",vars,"\",estratos=\"",by,"\")",sep="")
else
	cmd <- paste("resumo.breve(\"",dataset,"\",\"",vars,"\")",sep="")
runCommand(cmd)
closeWindow("BriefSummary","BS")
}


# operations

on_cbx_BS_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("BriefSummary","BS","numeric")
getWidget("BriefSummary","btn_BS_execute")$setSensitive(FALSE)
}

on_tvw_BS_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("BriefSummary","BS")
}

on_chk_BS_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("BriefSummary","BS")
}

