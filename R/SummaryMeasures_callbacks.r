# descriptiveStats callbaack funDSions

on_btn_SM_help_clicked <- function(widget,user.data)
{
showHelp("SummaryMeasures")
}

on_btn_SM_cancel_clicked <- function(widget,user.data)
{
closeWindow("SummaryMeasures")
}

on_btn_SM_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("SummaryMeasures","cbx_SM_dataset"))
vars <- vectorToString(getListSelection(getWidget("SummaryMeasures","tvw_SM_variables")))
extremevalues <- getWidget("SummaryMeasures","txt_SM_trim")$getText()
probs <- getWidget("SummaryMeasures","txt_SM_percentiles")$getText()
if(!probs=="")
	probs <- paste("c(",probs,")",sep = "")
else
	probs = "NULO"
if(extremevalues == "")
	extremevalues <- 0
dec <- getWidget("SummaryMeasures","spb_SM_size")$getValue()
by <- getActiveData(getWidget("SummaryMeasures","cbx_SM_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
stats1 <- NULL
if(getWidget("SummaryMeasures","chk_SM_mean")$getActive())
	stats1 <- "media"
	
if(getWidget("SummaryMeasures","chk_SM_median")$getActive())  
	stats1 <- c(stats1, "mediana")
	
if(getWidget("SummaryMeasures","chk_SM_stdev")$getActive())
	stats1 <- c(stats1, "desv.padrao")
	
if(getWidget("SummaryMeasures","chk_SM_n")$getActive())
	stats1 <- c(stats1,"n")
	
if(getWidget("SummaryMeasures","chk_SM_min")$getActive())
	stats1 <- c(stats1,"min")
	
if(getWidget("SummaryMeasures","chk_SM_max")$getActive())
	stats1 <- c(stats1,"max")
	
if(getWidget("SummaryMeasures","chk_SM_quintiles")$getActive())
	stats1 <- c(stats1,"quintil")
	
if(getWidget("SummaryMeasures","chk_SM_quantile")$getActive())
	stats1 <- c(stats1,"quartil")
	
if(getWidget("SummaryMeasures","chk_SM_vc")$getActive())
	stats1 <- c(stats1,"coef.variacao")
	
if(getWidget("SummaryMeasures","chk_SM_na")$getActive())
	stats1 <- c(stats1,"nd")	

if(getWidget("SummaryMeasures","chk_SM_range")$getActive())
		stats1 <- c(stats1,"amplitude")

if(getWidget("SummaryMeasures","chk_SM_variance")$getActive())
	stats1 <- c(stats1,"var")

if(getWidget("SummaryMeasures","chk_SM_skewness")$getActive())	
	stats1 <- c(stats1,"assimetria")		

if(getWidget("SummaryMeasures","chk_SM_kurtosis")$getActive())
	stats1 <- c(stats1,"curtose")

stats1 =  paste("c(\"",vectorToString(stats1),"\")", sep = "")

# translate
cmd <- paste("estatisticas.descritivas(\"",dataset,"\",\"",vars,"\",estratos=",by,",estatisticas=",stats1,",probabilidades=",probs,",valores.extremos=",extremevalues,",decimais=",dec,")",sep="")

runCommand(cmd)
closeWindow("SummaryMeasures", "DS")
}

# operations

on_cbx_SM_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("SummaryMeasures","DS","numeric")
getWidget("SummaryMeasures","btn_SM_execute")$setSensitive(FALSE)
}

on_tvw_SM_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("SummaryMeasures","DS")
}

on_chk_SM_quantile_toggled <- function(widget,user.data)
{
if(getWidget("SummaryMeasures","chk_SM_quantile")$getActive())
	getWidget("SummaryMeasures","chk_SM_quintiles")$setSensitive(FALSE)
else
	getWidget("SummaryMeasures","chk_SM_quintiles")$setSensitive(TRUE)
}

on_chk_SM_quintiles_toggled <- function(widget,user.data)
{
if(getWidget("SummaryMeasures","chk_SM_quintiles")$getActive())
	getWidget("SummaryMeasures","chk_SM_quantile")$setSensitive(FALSE)
else
		getWidget("SummaryMeasures","chk_SM_quantile")$setSensitive(TRUE)
}

on_chk_SM_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("SummaryMeasures","DS")
}

on_chk_SM_percentiles_toggled <- function(widget,user.data)
{
if(getWidget("SummaryMeasures","chk_SM_percentiles")$getActive())
	getWidget("SummaryMeasures", "txt_SM_percentiles")$setSensitive(TRUE)
else
	getWidget("SummaryMeasures", "txt_SM_percentiles")$setSensitive(FALSE)
}