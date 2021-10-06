# stem and leaf callback functions

# main buttons
on_btn_SL_help_clicked <- function(widget,user.data)
{
showHelp("StemLeaf")
}

on_btn_SL_cancel_clicked <- function(widget,user.data)
{
closeWindow("StemLeaf")
}

on_btn_SL_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("StemLeaf","cbx_SL_dataset"))
vars <- vectorToString(getListSelection(getWidget("StemLeaf","tvw_SL_variables")))
by <- getActiveData(getWidget("StemLeaf","cbx_SL_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
scale <- getWidget("StemLeaf","spb_SL_scale")$getValue()
width <- getWidget("StemLeaf","spb_SL_width")$getValue()

# translate
cmd <- paste("ramo.folha(\"",dataset,"\",\"",vars,"\",estratos=",by,",escala=",scale,",largura=",width,")",sep="")
runCommand(cmd)
closeWindow("StemLeaf","SL")
}


# operations
on_cbx_SL_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("StemLeaf","SL","numeric",plot=FALSE) # it is set to FALSE because it has nox axis
getWidget("StemLeaf","btn_SL_execute")$setSensitive(FALSE)
}

on_tvw_SL_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("StemLeaf","SL")
}

on_chk_SL_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("StemLeaf","SL")
}



