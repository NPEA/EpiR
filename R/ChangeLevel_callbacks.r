on_btn_CHL_cancel_clicked <- function(widget,user.data)
{
closeWindow("ChangeLevel")
}

on_btn_CHL_execute_clicked <- function(widget,user.data)
{
window <- getWidget("ChangeLevel")
parent.name <- window$"transient-for"$name
short.name <- switch(parent.name,
		     RecodeVar = "RV",
		     PlotsSectors = "PS")
old_factor <- getListViewData(getWidget(parent.name,paste("tvw_",short.name,"_viewFactor",sep="")))
new_factor <- getListViewData(getWidget(parent.name,paste("tvw_",short.name,"_viewFactor",sep="")),2)
changed_level <- setLocale(getWidget("ChangeLevel","lbl_CHL_oldLevel")$getText())
new_changed_level <- setLocale(getWidget("ChangeLevel","txt_CHL_newLevel")$getText())
new_factor[old_factor==changed_level] <- new_changed_level
list <- list(Categoria_original=old_factor,Novo_fator=new_factor)
fillListView(getWidget(parent.name,paste("tvw_",short.name,"_viewFactor",sep="")), list, update=TRUE,headers=c("Categoria original","Nova categoria"))
if(parent.name == "RecodeVar")
	{
	if(getWidget("RecodeVar","lbl_RV_load")$getText() == "Aplicar")
		{
		if(any(new_factor == ""))
			getWidget("RecodeVar","btn_RV_reload")$setSensitive(FALSE)
		else
			getWidget("RecodeVar","btn_RV_reload")$setSensitive(TRUE)
		}
	else
		{
		getWidget("RecodeVar","lbl_RV_load")$setText("Aplicar")
		getWidget("RecodeVar","btn_RV_reload")$setSensitive(FALSE)
		}
	}
	
closeWindow("ChangeLevel")
}

on_txt_CHL_newLevel_changed <- function(widget,user.data)
{
text <- setLocale(getWidget("ChangeLevel","txt_CHL_newLevel")$getText())
if(text == "")
	getWidget("ChangeLevel", "btn_CHL_execute")$setSensitive(FALSE)
else
	getWidget("ChangeLevel", "btn_CHL_execute")$setSensitive(TRUE)
}

on_txt_CHL_newLevel_activate <- function(entry,user.data)
{
on_btn_CHL_execute_clicked()
}