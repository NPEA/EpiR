on_btn_FD_help_clicked <- function(widget,user.data)
{
showHelp("FormatDate")
}

on_btn_FD_cancel_clicked <- function(widget,user.data)
{
closeWindow("FormatDate")
}

on_btn_FD_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FormatDate","cbx_FD_dataset"))
datevar <- getActiveData(getWidget("FormatDate","cbx_FD_datevar"))
varname <- setLocale(getWidget("FormatDate","txt_FD_varname")$getText())
left <- getActiveData(getWidget("FormatDate","cbx_FD_left"))
center <- getActiveData(getWidget("FormatDate","cbx_FD_center"))
right <- getActiveData(getWidget("FormatDate","cbx_FD_right")) 
separator <- getActiveData(getWidget("FormatDate","cbx_FD_separator"))
origin <- getActiveData(getWidget("FormatDate","cbx_FD_origin"))
left <- fixFormat(left)
center <- fixFormat(center)
right <- fixFormat(right)
manual <- getWidget("FormatDate","chk_FD_manual")$getActive()
if(manual)
	format <- setLocale(getWidget("FormatDate","txt_FD_manual")$getText())
else
	format <- paste(left,separator,center,separator,right,sep="")

#translate
if(origin != "")
	cmd <- paste("formatar.data(\"",dataset,"\",variavel.data=\"",datevar,"\",nome.variavel=\"",varname,"\",formato=\"",format,"\",seculo=\"",origin,"\")",sep="")
else
	cmd <- paste("formatar.data(\"",dataset,"\",variavel.data=\"",datevar,"\",nome.variavel=\"",varname,"\",formato=\"",format,"\")",sep="")
runCommand(cmd)

window <- getWidget("FormatDate")
if(window$"transient-for"$name == "ControlCharts")
	fillComboBox(getWidget("ControlCharts","cbx_CGP_date"),c("",getDate(dataset)))

closeWindow("FormatDate", "FD")
}

on_cbx_FD_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FormatDate","cbx_FD_dataset"))
fillComboBox(getWidget("FormatDate","cbx_FD_datevar"),c("",getNames(dataset)))
}

on_cbx_FD_datevar_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FormatDate","cbx_FD_dataset"))
datevar <- getActiveData(getWidget("FormatDate","cbx_FD_datevar"))
if(datevar != "")
	{
	date.raw <- eval(parse(text=paste(dataset,"$",datevar,sep="")),envir=.GlobalEnv)
	date.raw <- unique(date.raw)
	date.raw <- date.raw[!is.na(date.raw)]
	getWidget("FormatDate","lbl_raw")$setMarkup(paste("<b>",date.raw,"</b>",sep=""))
	}
else
	getWidget("FormatDate","lbl_raw")$setText("...")
}

on_cbx_FD_left_changed <- function(widget,user.data)
{
left <- getActiveData(getWidget("FormatDate","cbx_FD_left"))
center <- getActiveData(getWidget("FormatDate","cbx_FD_center"))
right <- getActiveData(getWidget("FormatDate","cbx_FD_right")) 
separator <- getActiveData(getWidget("FormatDate","cbx_FD_separator"))
if(left != "" && center != "" && right != "")
	{
	getWidget("FormatDate","btn_FD_preview")$setSensitive(TRUE)
	getWidget("FormatDate","btn_FD_execute")$setSensitive(TRUE)
	}
else
	{
	getWidget("FormatDate","btn_FD_preview")$setSensitive(FALSE)
	getWidget("FormatDate","btn_FD_execute")$setSensitive(FALSE)
	}
}

on_cbx_FD_center_changed <- function(widget,user.data)
{
on_cbx_FD_left_changed()
center <- getActiveData(getWidget("FormatDate","cbx_FD_center"))
}

on_cbx_FD_right_changed <- function(widget,user.data)
{
on_cbx_FD_left_changed()
right <- getActiveData(getWidget("FormatDate","cbx_FD_right")) 

}

on_btn_FD_preview_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("FormatDate","cbx_FD_dataset"))
datevar <- getActiveData(getWidget("FormatDate","cbx_FD_datevar"))
left <- getActiveData(getWidget("FormatDate","cbx_FD_left"))
center <- getActiveData(getWidget("FormatDate","cbx_FD_center"))
right <- getActiveData(getWidget("FormatDate","cbx_FD_right")) 
separator <- getActiveData(getWidget("FormatDate","cbx_FD_separator"))
origin <- getActiveData(getWidget("FormatDate","cbx_FD_origin"))
date.raw <- eval(parse(text=paste(dataset,"$",datevar,sep="")),envir=.GlobalEnv)
date.raw <- unique(date.raw)
date.raw <- date.raw[!is.na(date.raw)]
left <- fixFormat(left)
center <- fixFormat(center)
right <- fixFormat(right)
format <- paste(left,separator,center,separator,right,sep="")
date.preview <- as.character(format.date.core(datevar=date.raw,format=format,origin=origin))
fillListView(getWidget("FormatDate","tvw_FD_preview"),date.preview[1:min(length(date.preview),30)],update=TRUE)
}

on_chk_FD_manual_toggled <- function(widget,user.data)
{
format <- getWidget("FormatDate","chk_FD_manual")$getActive()
left <- getActiveData(getWidget("FormatDate","cbx_FD_left"))
center <- getActiveData(getWidget("FormatDate","cbx_FD_center"))
right <- getActiveData(getWidget("FormatDate","cbx_FD_right")) 
separator <- getActiveData(getWidget("FormatDate","cbx_FD_separator"))
if(format)
	{
	getWidget("FormatDate","btn_FD_execute")$setSensitive(FALSE)
	getWidget("FormatDate","hbx_FD_view")$setSensitive(FALSE)
	getWidget("FormatDate","cbx_FD_separator")$setSensitive(FALSE)
	getWidget("FormatDate","txt_FD_manual")$setSensitive(TRUE)
	getWidget("FormatDate","btn_FD_preview")$setSensitive(FALSE)
	}
else
	{
	getWidget("FormatDate","txt_FD_manual")$setText("")
	getWidget("FormatDate","hbx_FD_view")$setSensitive(TRUE)
	getWidget("FormatDate","cbx_FD_separator")$setSensitive(TRUE)
	getWidget("FormatDate","txt_FD_manual")$setSensitive(FALSE)	
	if(!any(c(left,center,right,separator)==""))
		{
		getWidget("FormatDate","btn_FD_execute")$setSensitive(TRUE)
		getWidget("FormatDate","btn_FD_preview")$setSensitive(TRUE)
		}	
	}	
}

on_txt_FD_manual_changed <- function(widget,user.data)
{
date.text <- setLocale(getWidget("FormatDate","txt_FD_manual")$getText())
format.manual <- getWidget("FormatDate","chk_FD_manual")$getActive()
left <- getActiveData(getWidget("FormatDate","cbx_FD_left"))
center <- getActiveData(getWidget("FormatDate","cbx_FD_center"))
right <- getActiveData(getWidget("FormatDate","cbx_FD_right")) 
separator <- getActiveData(getWidget("FormatDate","cbx_FD_separator"))

if(date.text == "")
	{
	if(!format.manual)
		{
		if(any(c(left,center,right,separator)==""))
			{
			getWidget("FormatDate","btn_FD_preview")$setSensitive(FALSE)
			getWidget("FormatDate","btn_FD_execute")$setSensitive(FALSE)
			}
		else
			{
			getWidget("FormatDate","btn_FD_preview")$setSensitive(TRUE)
			getWidget("FormatDate","btn_FD_execute")$setSensitive(TRUE)
			}
		}
	else
		{
		getWidget("FormatDate","btn_FD_preview")$setSensitive(FALSE)
		getWidget("FormatDate","btn_FD_execute")$setSensitive(FALSE)
		}
	}
else
	{
	getWidget("FormatDate","btn_FD_preview")$setSensitive(TRUE)
	getWidget("FormatDate","btn_FD_execute")$setSensitive(TRUE)
	}
}
