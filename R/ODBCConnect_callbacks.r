# odbcConnect callbaack functions

# main buttons
on_btn_OC_help_clicked <- function(widget,user.data)
{
showHelp("ODBCConnect")
}

on_btn_OC_cancel_clicked <- function(widget,user.data)
{
if(exists(".TempODBCConnection",envir=.GlobalEnv))
	end.odbc(conn=".TempODBCConnection")
closeWindow("ODBCConnect")
}

on_btn_OC_execute_clicked <- function(widget,user.data)
{
sql <- getTv(getWidget("ODBCConnect","txt_OC_sql"))
object <- setLocale(getWidget("ODBCConnect","txt_OC_object")$getText())
if(object=="")
	{
	msgDialog("ODBCConnect","error","É necessário indicar o nome do objeto.")
	return()
	}
if(sql=="")
	{
	msgDialog("ODBCConnect","error","É necessário especificar uma consulta SQL.")
	return()
	}
else	
	query.odbc(conn=".TempODBCConnection",sql=sql,object=object)
if(get(object,envir=.GlobalEnv)<0) 
	{
	msgDialog("ODBCConnect","error","Não foi possível executar a consulta SQL.")
	rm(object,envir=.GlobalEnv)
	return()
	}
end.odbc(conn=".TempODBCConnection")
updateMainLists()
closeWindow("ODBCConnect","OC")
}


# operations
on_cbx_OC_case_changed <- function(widget,user.data)
{
dsn <- setLocale(getWidget("ODBCConnect","txt_OC_dsn")$getText())
user <- setLocale(getWidget("ODBCConnect","txt_OC_user")$getText())
password <- setLocale(getWidget("ODBCConnect","txt_OC_password")$getText())
if((dsn!="") & (user!="") & (password!=""))
	getWidget("ODBCConnect","btn_OC_connect")$setSensitive(TRUE)
else
	getWidget("ODBCConnect","btn_OC_connect")$setSensitive(FALSE)
}

on_btn_OC_connect_clicked <- function(widget,user.data)
{
dsn <- setLocale(getWidget("ODBCConnect","txt_OC_dsn")$getText())
user <- setLocale(getWidget("ODBCConnect","txt_OC_user")$getText())
password <- setLocale(getWidget("ODBCConnect","txt_OC_password")$getText())
case <- getActiveData(getWidget("ODBCConnect","cbx_OC_case"))
start.odbc(dsn,uid=user,pwd=password,case=case,conn=".TempODBCConnection")
if(get(".TempODBCConnection",envir=.GlobalEnv) != (-1))
	{
	getWidget("ODBCConnect","img_OC_yes")$setSensitive(TRUE)
	getWidget("ODBCConnect","img_OC_no")$setSensitive(FALSE)
	getWidget("ODBCConnect","btn_OC_execute")$setSensitive(TRUE)
	}
else
	{
	getWidget("ODBCConnect","img_OC_yes")$setSensitive(FALSE)
	getWidget("ODBCConnect","img_OC_no")$setSensitive(TRUE)
	getWidget("ODBCConnect","btn_OC_execute")$setSensitive(FALSE)
	msgDialog("ODBCConnect","error","Não foi possível conectar com o servidor.")
	}
}

on_btn_OC_sqlbuilder_clicked <- function(widget,user.data)
{
msgDialog("ODBCConnect","warn","Ainda não implementado. Instale uma nova versão.")
}


