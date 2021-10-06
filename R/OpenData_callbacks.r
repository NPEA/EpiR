# OpenData callback functions

# main buttons
on_btn_OD_help_clicked <- function(widget,user.data)
{
showHelp("OpenData")
}

on_btn_OD_cancel_clicked <- function(widget,user.data)
{
closeWindow("OpenData")
}

on_btn_OD_execute_clicked <- function(widget,user.data)
{
fillListView(getWidget("OpenData","tvw_OD_viewdataset"),NULL)
filename <- setLocale(getWidget("OpenData","txt_OD_filename")$getText(),fixspc=FALSE)
object <- setLocale(getWidget("OpenData","txt_OD_object")$getText())

if(object %in% ls(envir=.GlobalEnv))
	if(askDialog("OpenData","Já existe um objeto chamado '",object,"' na área de trabalho.\nDeseja substituir o objeto existente?")=="no")
		{
		getWidget("OpenData","txt_OD_object")$deleteText(0,-1)
		return()
		}

if(!any(tolower(getExtension(filename)) %in% c("rda","dta","sav","rec","dbf","tab")))
	{
	sep <- getWidget("OpenData","txt_OD_separator")$getText()
	dec <- getWidget("OpenData","txt_OD_decimal")$getText()
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
closeWindow("OpenData","OD")
}
	
# operations
on_btn_OD_browse_clicked <- function(widget,user.data)
{
fillListView(getWidget("OpenData","tvw_OD_viewdataset"),NULL,TRUE)
if(is.windows())
	filter <- list(type=c("Arquivos R","Arquivos CSV","Arquivos Texto","Arquivos Tabwin","Arquivos EpiInfo","Arquivos Stata","Arquivos SPSS","Arquivos dBase","Todos os arquivos"),pat=c("*.rda","*.csv","*.txt","*.tab","*.rec","*.dta","*.sav","*.dbf","*"))
else
	filter <- list(type=c("Arquivos R","Arquivos CSV","Arquivos Texto","Arquivos Tabwin","Arquivos EpiInfo","Arquivos Stata","Arquivos SPSS","Arquivos dBase","Todos os arquivos"),pat=c("*.rda","*.csv","*.txt","*.tab","*.rec","*.dta","*.sav","*.dbf","*"))
choose <- fileDialog("OpenData","Abrir dados","open",filter)
filename <- choose$filename
if (!is.null(filename))
	{
	txtFilename <- getWidget("OpenData","txt_OD_filename")
	txtFilename$setText(filename)
	hbxTextfile <- getWidget("OpenData","hbx_OD_textfile")
	if(!any(tolower(getExtension(filename)) %in% c("rda","dta","sav","rec","dbf","tab")))
		hbxTextfile$show()
	else
		hbxTextfile$hide()
	newdir <- choose$folder
	olddir <- getwd()
	if (newdir!=olddir)
		if(askDialog("Main","O diretório de trabalho atual é ",olddir,".\nDeseja alterar para",newdir,"?")=="yes")
			setwd(newdir)
	}
if(!is.null(filename))
	getWidget("OpenData","btn_OD_preview")$setSensitive(TRUE)
else
	getWidget("OpenData","btn_OD_preview")$setSensitive(FALSE)
if(tolower(getExtension(filename)) == "rda")
	getWidget("OpenData","hbx_OD_preview")$hide()
else
	getWidget("OpenData","hbx_OD_preview")$show()
}

on_btn_OD_preview_clicked <- function(widget,user.data)
{
filename <- setLocale(getWidget("OpenData","txt_OD_filename")$getText(),fixspc=FALSE)
ext <- getExtension(filename)
sep <- getWidget("OpenData","txt_OD_separator")$getText()
dec <- getWidget("OpenData","txt_OD_decimal")$getText()
errormsg <- try(import.data(filename,"dataobj",sep=sep,dec=dec,envir=environment()),silent=TRUE)
if(class(errormsg) == "try-error")
	msgDialog("OpenData","error","Houve um erro ao carregar o banco. Verifique se as opções estão corretas.")	
else
	{
	dataobj <- errormsg
	tvwViewdataset <- getWidget("OpenData","tvw_OD_viewdataset")
	tvwViewdataset$setGridLines("both")
	fillListView(tvwViewdataset,dataobj[1:min(10,nrow(dataobj)),],update=TRUE,headers=names(dataobj))   
	}
}

	
on_txt_OD_filename_changed <- function(widget,user.data)
{
txtObject <- getWidget("OpenData","txt_OD_object")
fullfilename <- setLocale(getWidget("OpenData","txt_OD_filename")$getText(),fixspc=FALSE)
# if(is.windows())
# 	parts <- try(strsplit(fullfilename,split="\\\\")[[1]],silent=TRUE)
# else
parts <- try(strsplit(fullfilename,split="\\/")[[1]],silent=TRUE)
filename <- parts[length(parts)]
objectname <- try(strsplit(filename,split="\\.")[[1]],silent=TRUE)
if(tolower(ext <- getExtension(filename)) != "rda")
	txtObject$setText(cleanStr(objectname,sep="_"))
else
	getWidget("OpenData","btn_OD_execute")$setSensitive(TRUE)
}


on_txt_OD_object_changed <- function(widget,user.data)
{
txt <- setLocale(getWidget("OpenData","txt_OD_object")$getText())
if(txt == "")
	getWidget("OpenData","btn_OD_execute")$setSensitive(FALSE)
else
	toggleExecute("OpenData","OD",type="object",button="execute",widget="textedit")
}

