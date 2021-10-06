# frequencyTables callbaack functions

# main buttons
on_btn_FT_help_clicked <- function(widget,user.data)
{
showHelp("FrequencyTables")
}

on_btn_FT_cancel_clicked <- function(widget,user.data)
{
closeWindow("FrequencyTables")
}

on_btn_FT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FrequencyTables","cbx_FT_dataset"))
vars <- vectorToString(getListSelection(getWidget("FrequencyTables","tvw_FT_variables")))
by <- getActiveData(getWidget("FrequencyTables","cbx_FT_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")

if(getWidget("FrequencyTables","chk_FT_exclude")$getActive())
	exclude <- "VERDADEIRO"
else
	exclude <- "FALSO"

# translate
cmd <- paste("tabela.frequencia(\"",dataset,"\",\"",vars,"\",estratos=",by,",remove.na=", exclude,")",sep="")
runCommand(cmd)
closeWindow("FrequencyTables","FT")
}

# operations
on_cbx_FT_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("FrequencyTables","FT","all")
getWidget("FrequencyTables","btn_FT_execute")$setSensitive(FALSE)
}


on_tvw_FT_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("FrequencyTables","FT")
}

on_chk_FT_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("FrequencyTables","FT")
}