# CrossTables callback functions

# main buttons
on_btn_CT_help_clicked <- function(widget,user.data)
{
showHelp("CrossTables")
}

on_btn_CT_cancel_clicked <- function(widget,user.data)
{
closeWindow("CrossTables")
}

on_btn_CT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CrossTables","cbx_CT_dataset"))
by <- getActiveData(getWidget("CrossTables","cbx_CT_by"))
vars_line <- getActiveData(getWidget("CrossTables","cbx_CT_line"))
vars_col <- getActiveData(getWidget("CrossTables","cbx_CT_column"))
calc_margin <- "NULO"
print_margin <- "NULO"
if(getWidget("CrossTables","chk_CT_frequency")$getActive())
	{
	frequency <- "FALSO"
	calc_margin <- getActiveData(getWidget("CrossTables","cbx_CT_relative_margin"))
	calc_margin <- paste("\"",calc_margin,"\"",sep="")
	}	
else
	frequency <- "VERDADEIRO"

if(getWidget("CrossTables","chk_CT_total")$getActive())
	{
	paste_margin <- "VERDADEIRO"
	print_margin <- getActiveData(getWidget("CrossTables","cbx_CT_total"))
	print_margin <- paste("\"",print_margin,"\"",sep="")
	}
else
	paste_margin <- "FALSO"
	
if(by=="")
	by <- "NULO"
else
	by <- paste("\"",by,"\"",sep="")
	
# to be translated
cmd <- paste("tabela.contingencia(\"",dataset,"\"",",","\"",vars_line,"\"",",","\"",vars_col,"\"",",estratos=",by,",frequencia=",frequency,",calculo_frequencia_relativa=",calc_margin,",totais_marginais=",paste_margin,",margem=",print_margin,")",sep="")

runCommand(cmd)
closeWindow("CrossTables","CT")
}

# operations

on_cbx_CT_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CrossTables","cbx_CT_dataset"))
fillComboBox(getWidget("CrossTables","cbx_CT_line"),c("",getNames(dataset)))
fillComboBox(getWidget("CrossTables","cbx_CT_column"),c("",getNames(dataset)))
fillComboBox(getWidget("CrossTables","cbx_CT_by"),c("",getBy(dataset)))
getWidget("CrossTables","btn_CT_execute")$setSensitive(FALSE)
}


on_cbx_CT_line_changed <- function(widget,user.data)
{
if(getActiveData(getWidget("CrossTables","cbx_CT_column")) != "" && getActiveData(getWidget("CrossTables","cbx_CT_line")) != "")
	getWidget("CrossTables","btn_CT_execute")$setSensitive(TRUE)
else
	getWidget("CrossTables","btn_CT_execute")$setSensitive(FALSE)
}	

on_cbx_CT_column_changed <- function(widget,user.data)
{
if(getActiveData(getWidget("CrossTables","cbx_CT_column")) != "" && getActiveData(getWidget("CrossTables","cbx_CT_line")) != "")
	getWidget("CrossTables","btn_CT_execute")$setSensitive(TRUE)
else
	getWidget("CrossTables","btn_CT_execute")$setSensitive(FALSE)
}	



on_chk_CT_showall_toggled <- function(widget,user.data)
{
toggleByShowAll("CrossTables","CT")
}

on_chk_CT_frequency_toggled <- function(widget,user.data)
{
if(getWidget("CrossTables","chk_CT_frequency")$getActive())
	getWidget("CrossTables","cbx_CT_relative_margin")$setSensitive(TRUE)
else
	getWidget("CrossTables","cbx_CT_relative_margin")$setSensitive(FALSE)
}

on_chk_CT_total_toggled <- function(widget,user.data)
{
if(getWidget("CrossTables","chk_CT_total")$getActive())
	getWidget("CrossTables","cbx_CT_total")$setSensitive(TRUE)
else
	getWidget("CrossTables","cbx_CT_total")$setSensitive(FALSE)
}

