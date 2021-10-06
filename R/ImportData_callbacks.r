# ImportData callback functions

# main buttons
on_btn_ID_help_clicked <- function(widget,user.data)
{
showHelp("ImportData")
}

on_btn_ID_cancel_clicked <- function(widget,user.data)
{
closeWindow("ImportData")
}

on_btn_ID_execute_clicked <- function(widget,user.data)
{
fillListView(getWidget("ImportData","tvw_ID_viewdataset"),NULL)
filename <- setLocale(getWidget("ImportData","txt_ID_filename")$getText(),fixspc=FALSE)
object <- setLocale(getWidget("ImportData","txt_ID_object")$getText())

if(object %in% ls(envir=.GlobalEnv))
	if(askDialog("ImportData","Já existe um objeto chamado '",object,"' na área de trabalho.\nDeseja substituir o objeto existente?")=="no")
		{
		getWidget("ImportData","txt_ID_object")$deleteText(0,-1)
		return()
		}

if(!any(tolower(getExtension(filename)) %in% c("rda","dta","sav","rec","dbf","tab")))
	{
	sep <- getWidget("ImportData","txt_ID_separator")$getText()
	dec <- getWidget("ImportData","txt_ID_decimal")$getText()
	cmd <- paste("importar.dados(\"",filename,"\",\"",object,"\",","sepvar=\"",sep,"\",","sepdec=\"",dec,"\")",sep="")
	}
else
	{
	if(trim(object) != "")
		cmd <- paste("importar.dados(\"",filename,"\",\"",object,"\")",sep="")
	else
		cmd <- paste("importar.dados(\"",filename,"\")",sep="")
	}

# translate
runCommand(cmd)
closeWindow("ImportData","ID")
}
	
# operations
on_btn_ID_browse_clicked <- function(widget,user.data)
{
fillListView(getWidget("ImportData","tvw_ID_viewdataset"),NULL,TRUE)
if(is.windows())
	filter <- list(type=c("Arquivos R","Arquivos CSV","Arquivos Texto","Arquivos Tabwin","Arquivos EpiInfo","Arquivos Stata","Arquivos SPSS","Arquivos dBase","Todos os arquivos"),pat=c("*.rda","*.csv","*.txt","*.tab","*.rec","*.dta","*.sav","*.dbf","*"))
else
	filter <- list(type=c("Arquivos R","Arquivos CSV","Arquivos Texto","Arquivos Tabwin","Arquivos EpiInfo","Arquivos Stata","Arquivos SPSS","Arquivos dBase","Todos os arquivos"),pat=c("*.rda","*.csv","*.txt","*.tab","*.rec","*.dta","*.sav","*.dbf","*"))
choose <- fileDialog("ImportData","Abrir dados","open",filter)
filename <- choose$filename
if (!is.null(filename))
	{
	txtFilename <- getWidget("ImportData","txt_ID_filename")
	txtFilename$setText(filename)
	boxTextfile <- getWidget("ImportData","box_ID_textfile")
	if(!any(tolower(getExtension(filename)) %in% c("rda","dta","sav","rec","dbf","tab")))
		boxTextfile$show()
	else
		boxTextfile$hide()
	newdir <- choose$folder
	olddir <- getwd()
	if (newdir!=olddir)
		if(askDialog("Main","O diretório de trabalho atual é ",olddir,".\nDeseja alterar para",newdir,"?")=="yes")
			setwd(newdir)
	}
if(!is.null(filename))
	getWidget("ImportData","btn_ID_preview")$setSensitive(TRUE)
else
	getWidget("ImportData","btn_ID_preview")$setSensitive(FALSE)
if(tolower(getExtension(filename)) == "rda")
	{
	getWidget("ImportData","btn_ID_execute")$setSensitive(TRUE)
	getWidget("ImportData","hbx_ID_datasetname")$hide()
	}
else
	{
	getWidget("ImportData","btn_ID_execute")$setSensitive(FALSE)
	getWidget("ImportData","hbx_ID_datasetname")$show()
	}

}

on_btn_ID_preview_clicked <- function(widget,user.data)
{
filename <- setLocale(getWidget("ImportData","txt_ID_filename")$getText(),fixspc=FALSE)
ext <- getExtension(filename)
sep <- getWidget("ImportData","txt_ID_separator")$getText()
dec <- getWidget("ImportData","txt_ID_decimal")$getText()
if(toupper(ext) == "RDA")
	errormsg <- get(load(filename))
else
	errormsg <- try(import.data(filename,"dataobj",sep=sep,dec=dec,envir=environment()),silent=TRUE)
if(class(errormsg) == "try-error")
	msgDialog("ImportData","error","Houve um erro ao carregar o banco. Verifique se as opções estão corretas.")	
else
	{
	dataobj <- errormsg
	tvwViewdataset <- getWidget("ImportData","tvw_ID_viewdataset")
	tvwViewdataset$setGridLines("both")
	fillListView(tvwViewdataset,dataobj[1:min(10,nrow(dataobj)),],update=TRUE,headers=names(dataobj))   
	}
}

	
on_txt_ID_filename_changed <- function(widget,user.data)
{
toggleExecute("ImportData","ID",type="object",button="execute",widget="textedit")
}


on_txt_ID_object_changed <- function(widget,user.data)
{
txt <- setLocale(getWidget("ImportData","txt_ID_object")$getText())
if(txt == "")
	getWidget("ImportData","btn_ID_execute")$setSensitive(FALSE)
else
	toggleExecute("ImportData","ID",type="filename",button="execute",widget="textedit")
}

