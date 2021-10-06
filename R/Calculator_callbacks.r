# selectRows callback functions

# main buttons
on_btn_CAL_help_clicked <- function(widget,user.data)
{
showHelp("Calculator")
}

on_btn_CAL_cancel_clicked <- function(widget,user.data)
{
closeWindow("Calculator")
}

on_btn_CAL_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("Calculator","cbx_CAL_dataset"))
dataset.names <- dataset
dataset <- eval(parse(text = dataset))
varname <- getWidget("Calculator","lbl_CAL_variablename")$getText()
varname <- strsplit(varname,"=")[[1]][1]
expression <- trim(getTv(getWidget("Calculator","txt_CAL_expression")))
raw_expression <- paste(varname, "<- ",expression,sep="")
getWidget("DataOperations","txt_DO_expression")$setText(trim(expression))
if(trim(expression) != "")
	{
	result <- try(eval(parse(text=expression),envir=dataset),silent=TRUE)
	if(class(result)=="try-error" | length(strsplit(raw_expression,"<-")[[1]][-1])==0)
		{
		msgDialog("DataOperations","error","Há um erro na expressão. Corrija-a e tente novamente.")
		return()
		}
	else
		{
		getWidget("DataOperations","txt_DO_newvar")$setText(trim(strsplit(raw_expression,"<-")[[1]][1]))
		on_btn_DO_add_clicked()
		closeWindow("Calculator")
		}
	}
}

on_cbx_CAL_dataset_changed <- function(widget,user.data)
{
fillStatsHeader("Calculator","CAL","all",by=FALSE)
}

#buttons

on_btn_CAL_one_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"1")
}

on_btn_CAL_two_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"2")
}

on_btn_CAL_three_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"3")
}

on_btn_CAL_four_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"4")
}

on_btn_CAL_five_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"5")
}

on_btn_CAL_six_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"6")
}

on_btn_CAL_seven_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"7")
}

on_btn_CAL_eight_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"8")
}

on_btn_CAL_nine_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"9")
}

on_btn_CAL_zero_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"0")
}

on_btn_CAL_plus_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"+")
}

on_cbx_CAL_less_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"-")
}

on_btn_CAL_times_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"*")
}

on_btn_CAL_divide_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"/")
}

on_btn_CAL_point_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),".")
}

on_btn_CAL_lesser_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"<")
}

on_btn_CAL_lesserequal_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"<=")
}

on_btn_CAL_notequal_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"!=")
}

on_btn_CAL_equal_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"==")
}

on_btn_CAL_open_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"(")
}

on_btn_CAL_close_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),")")
}

on_btn_CAL_exp_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"exp")
}

on_btn_CAL_log_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"log")
}

on_btn_CAL_not_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"!")
}

on_btn_CAL_or_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"|")
}

on_btn_CAL_and_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"&")
}

on_btn_CAL_greater_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),">")
}

on_btn_CAL_greaterequal_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),">=")
}

on_btn_CALC_potence_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"^")
}

#variables and functions

on_btn_CAL_zeros_clicked <- function(widget,user.data)
{
insertTv(getWidget("Calculator","txt_CAL_expression"),"000")
}

on_tvw_CAL_variables_button_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	{
	variables <- getListSelection(getWidget("Calculator","tvw_CAL_variables"))
	insertTv(getWidget("Calculator","txt_CAL_expression"),variables)
	}
}

on_tvw_CAL_function_button_press_event <- function(widget,event,user.data)
{
if(event$type=="GDK_2BUTTON_PRESS")
	{
	functions <- getListSelection(getWidget("Calculator","tvw_CAL_function"))
	insertTv(getWidget("Calculator","txt_CAL_expression"),functions)
	}
}

