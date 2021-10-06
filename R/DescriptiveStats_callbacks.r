# descriptiveStats callbaack funDSions

on_btn_DS_help_clicked <- function(widget,user.data)
{
showHelp("DescriptiveStats")
}

on_btn_DS_cancel_clicked <- function(widget,user.data)
{
closeWindow("DescriptiveStats")
}

on_btn_DS_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("DescriptiveStats","cbx_DS_dataset"))
vars <- vectorToString(getListSelection(getWidget("DescriptiveStats","tvw_DS_variables")))
extremevalues <- getWidget("DescriptiveStats","txt_DS_trim")$getText()
probs <- getWidget("DescriptiveStats","txt_DS_percentiles")$getText()
if(!probs=="")
	probs <- paste("c(",probs,")",sep = "")
else
	probs = "NULO"
if(extremevalues == "")
	extremevalues <- 0
dec <- getWidget("DescriptiveStats","spb_DS_size")$getValue()
by <- getActiveData(getWidget("DescriptiveStats","cbx_DS_by"))
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
stats1 <- NULL
if(getWidget("DescriptiveStats","chk_DS_mean")$getActive())
	stats1 <- "media"
	
if(getWidget("DescriptiveStats","chk_DS_median")$getActive())  
	stats1 <- c(stats1, "mediana")
	
if(getWidget("DescriptiveStats","chk_DS_stdev")$getActive())
	stats1 <- c(stats1, "desv.padrao")
	
if(getWidget("DescriptiveStats","chk_DS_n")$getActive())
	stats1 <- c(stats1,"n")
	
if(getWidget("DescriptiveStats","chk_DS_min")$getActive())
	stats1 <- c(stats1,"min")
	
if(getWidget("DescriptiveStats","chk_DS_max")$getActive())
	stats1 <- c(stats1,"max")
	
if(getWidget("DescriptiveStats","chk_DS_quintiles")$getActive())
	stats1 <- c(stats1,"quintil")
	
if(getWidget("DescriptiveStats","chk_DS_quantile")$getActive())
	stats1 <- c(stats1,"quartil")
	
if(getWidget("DescriptiveStats","chk_DS_vc")$getActive())
	stats1 <- c(stats1,"coef.variacao")
	
if(getWidget("DescriptiveStats","chk_DS_na")$getActive())
	stats1 <- c(stats1,"nd")	

if(getWidget("DescriptiveStats","chk_DS_range")$getActive())
		stats1 <- c(stats1,"amplitude")

if(getWidget("DescriptiveStats","chk_DS_variance")$getActive())
	stats1 <- c(stats1,"var")

if(getWidget("DescriptiveStats","chk_DS_skewness")$getActive())	
	stats1 <- c(stats1,"assimetria")		

if(getWidget("DescriptiveStats","chk_DS_kurtosis")$getActive())
	stats1 <- c(stats1,"curtose")

stats1 =  paste("c(\"",vectorToString(stats1),"\")", sep = "")

# translate
cmd <- paste("estatisticas.descritivas(\"",dataset,"\",\"",vars,"\",estratos=",by,",estatisticas=",stats1,",probabilidades=",probs,",valores.extremos=",extremevalues,",decimais=",dec,")",sep="")

runCommand(cmd)
closeWindow("DescriptiveStats", "DS")
}

# operations

on_cbx_DS_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("DescriptiveStats","DS","numeric")
getWidget("DescriptiveStats","btn_DS_execute")$setSensitive(FALSE)
}

on_tvw_DS_variables_button_release_event <- function(widget,event,user.data)
{
toggleExecute("DescriptiveStats","DS")
}

on_chk_DS_quantile_toggled <- function(widget,user.data)
{
if(getWidget("DescriptiveStats","chk_DS_quantile")$getActive())
	getWidget("DescriptiveStats","chk_DS_quintiles")$setSensitive(FALSE)
else
	getWidget("DescriptiveStats","chk_DS_quintiles")$setSensitive(TRUE)
}

on_chk_DS_quintiles_toggled <- function(widget,user.data)
{
if(getWidget("DescriptiveStats","chk_DS_quintiles")$getActive())
	getWidget("DescriptiveStats","chk_DS_quantile")$setSensitive(FALSE)
else
		getWidget("DescriptiveStats","chk_DS_quantile")$setSensitive(TRUE)
}

on_chk_DS_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("DescriptiveStats","DS")
}

on_chk_DS_percentiles_toggled <- function(widget,user.data)
{
if(getWidget("DescriptiveStats","chk_DS_percentiles")$getActive())
	getWidget("DescriptiveStats", "txt_DS_percentiles")$setSensitive(TRUE)
else
	getWidget("DescriptiveStats", "txt_DS_percentiles")$setSensitive(FALSE)
}