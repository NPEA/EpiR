# Main program functions
EpiR.setup <- function(gui)
# set up some environment objects
{
setup_env(gui)   
if(gui)
	{
    require("RGtk2",character.only=TRUE,quietly=TRUE,warn.conflicts=FALSE)
    required <- get("required", envir=.EpiREnv)
    installed <-  required[!(required %in% installed.packages())]
	if(length(installed) != 0)
		{
		if(askDialog(NULL,"Alguns pacotes de dependência do R não estão disponíveis. Eles são necessários para o funcionamento do Epi-R. \nDeseja instalá-los agora?") == "yes")
			{
			if(!checkInternet())
				{
				msgDialog(NULL, "error","O computador não está conectado à Internet e não será possível atualizar os pacotes.")
				unload()
				}
			else
				{
				r <- getOption("repos")
				r["CRAN"] <- get("EPIR_CRAN",env=.EpiREnv)
				options(repos=r)
				install.packages(installed,lib=.libPaths()[1])
				}
			}
		else
			{
			unload()
			}
		}
	showSplashScreen()
	}
else
	{
	# loading necessary packages
	required <- get("required", envir=.EpiREnv)
	installed <-  required[!(required %in% installed.packages())]
	if(length(installed) != 0)
		{
		package.test <- FALSE
		while(!package.test)
			{
			cat("Alguns pacotes de dependência do R não estão disponíveis. Eles são necessários para o funcionamento do Epi-R. \nDeseja instalá-los agora?(s/n)? \n")
			package.test <- scan(what="character",n=1,quiet=T)
			if(tolower(package.test) %in% c("s","n"))
				package.test <- FALSE
			}
			if(tolower(package.test) == "s")
			{
			if(!checkInternet())
				{
				cat("O computador não está conectado à Internet e não será possível atualizar os pacotes.\n")
				unload()
				}
			else
				{
				r <- getOption("repos")
				r["CRAN"] <- get("EPIR_CRAN",env=.EpiREnv)
				options(repos=r)
				install.packages(installed,lib=.libPaths()[1])
				}
			}
		else
			{
			unload()
			}
		}
	}
# Output status
if(gui)
	setLoadStatus("Definindo outras variáveis de ambiente...")
	

if(gui)
	setLoadStatus("Colocando ambiente no caminho de busca...")
# putting all this in the search path
attach(.EpiREnv,warn.conflicts=FALSE)

if(gui)
	{
	# output some some message through the status bar
	for (i in required)
		{
		setLoadStatus(paste("Carregando dependência:",i))
		require(i,character.only=TRUE,quietly=TRUE,warn.conflicts=FALSE)
		}
# 	if(is.windows())
# 		{
# 		setLoadStatus(paste("Carregando dependência (Windows):","xlsReadWrite"))
# 		require("xlsReadWrite",character.only=TRUE,quietly=TRUE,warn.conflicts=FALSE)
# 		}
	}
else
	{
	# have to repeat due to the splash screen
	for (i in required)
		require(i,character.only=TRUE,quietly=TRUE,warn.conflicts=FALSE)
# 	if(is.windows())
# 		require("xlsReadWrite",character.only=TRUE,quietly=TRUE,warn.conflicts=FALSE)
	}

# set home as default directory
try(setwd(get("EPIR_USER_HOME",envir.EpiREnv)),silent=TRUE)

# set close button visible on the splash screen
getWidget("SplashScreen","btn_SS_close")$setSensitive(TRUE)
setLoadStatus("Clique no botão \'Fechar\' para continuar...")

# return TRUE (must have some function in the future)
return(TRUE)
}


EpiR <- function(gui=TRUE)
# load EpiR and the gui
{
if(gui)
	{
	if(EpiR.setup(gui))
		{	
		if(exists("Main",envir=.EpiREnv))
			{
			msgDialog(NULL,"warning","Já existe uma instância do Epi-R sendo executada.")
			getWidget("Main")$present()
			}
		else
			loadGui()
		}
	}
else
	EpiR.setup(gui)

loadPlugins()
}

unload <- function(hard=TRUE)
# end the Epir session
{
#if(exists(".EpiREnv"))
#	{
#	if(EPIR_GUI)
#		if(exists("Main",envir=.EpiREnv))
#			{
#			closeWindow("Main")
#			get("trayIcon",envir=.EpiREnv)$setVisible(FALSE)
#			}
#	if(exists("EPIR_WARN_STATE",envir=.EpiREnv))
#		options(warn=get("EPIR_WARN_STATE",envir=.EpiREnv))
#	}
#else
#	return()
save.workspace <- get("EPIR_OPTION_SAVE_WORKSPACE",envir=.EpiREnv)
if(exists(".EpiREnv"))
	{
	rm(list=ls(all=TRUE,envir=.EpiREnv),envir=.EpiREnv)
	rm(.EpiREnv,envir=.GlobalEnv)
	}
if(exists(".EpiRTrash"))
	{
	rm(list=ls(all=TRUE,envir=.EpiRTrash),envir=.EpiRTrash)
	rm(.EpiRTrash,envir=.GlobalEnv)
	}
while (sum(search()==".EpiREnv")>0)
	detach(.EpiREnv) 
gc(verbose=FALSE)  # memory clean up
if(hard)
	{
	while (sum(search()=="package:EpiR")>0)
		detach(package:EpiR) 
	unloadNamespace("EpiR")
	if(save.workspace)
		q("yes")
	else
		q("no")
	}
}


loadGui <- function()
# load the gui
{
# loading main window
setWindow("Main")
sbaMain <- getWidget("Main","sbaMain")
sbaMain$push(0,paste("Epi-R versão",EPIR_VER))

#switching things
getWidget("Main","mnSaveData")$setSensitive(FALSE)
getWidget("Main","btnSaveData")$setSensitive(FALSE)

# Filling the lists for the first time
fillListView(getWidget("Main","tvwHistory"),get("EPIR_TEXT_HISTORY",envir=.EpiREnv),FALSE,sel.mode="browse")
fillObjList(FALSE)
fillVarList(FALSE)

# Setting the welcome message
txtOutput <- getWidget("Main","txtOutput")
clearTv(txtOutput)
welcomeTv(txtOutput)

# set focus on command widget
getWidget("Main","txtCommand")$grabFocus()

# disabling some resources
getWidget("Main","mnCopy")$setSensitive(FALSE)
getWidget("Main","btnCopy")$setSensitive(FALSE)
getWidget("Main","mnPaste")$setSensitive(FALSE)
getWidget("Main","btnPaste")$setSensitive(FALSE)

# tray icon
setTrayIcon()

# Not yet implemented !!!!
getWidget("Main","mnInstallPlugin")$setSensitive(FALSE)
getWidget("Main","mnSettings")$setSensitive(FALSE)
# plots

}

showSplashScreen <- function()
# show a warning nag screen
{
file <- paste(get("EPIR_GUI_DIR",envir=.EpiREnv),"/","SplashScreen",".glade",sep="")
xmlwin <- gtkBuilderNew()
gtkBuilderAddFromFile(xmlwin,file)
assign("SplashScreen",xmlwin,envir=.EpiREnv)
gtkBuilderConnectSignals(eval(parse(text="SplashScreen"),envir=.EpiREnv))
# make the window modal
getWidget("SplashScreen")$setModal(TRUE)
# set data on the window
getWidget("SplashScreen","lbl_SS_nameVer")$setMarkup(paste("<b><big>Epi-R",get("EPIR_VER",envir=.EpiREnv),"</big></b>"))
loadCredits("SplashScreen","SS")
getWidget("SplashScreen","txt_SS_credits")$grabFocus()
}

setLoadStatus <- function(status)
# set loading status on the status bar
{
statusbar <- getWidget("SplashScreen","sb_SS_loading")
statusbar$push(0,status)
}

setTrayIcon <- function()
# set the tray icon
{
# Note: the callback functions are in the other_callbacks.r file
assign("trayIcon",gtkStatusIconNewFromFile(paste(get("EPIR_GUI_DIR",envir=.EpiREnv),"/epir_icon.png",sep="")),.EpiREnv)
get("trayIcon",envir=.EpiREnv)$setTooltip(paste("Epi-R versão",get("EPIR_VER",envir=.EpiREnv)))
gSignalConnect(get("trayIcon",envir=.EpiREnv),"activate",on_trayIcon_activate)
}

loadCredits <- function(long,short)
# load credit from file
{
txt <- getWidget(long,paste("txt_",short,"_credits",sep=""))
conn <- file(paste(get("EPIR_DIR",envir=.EpiREnv),"/credits.txt",sep=""),"r")
credits <- readLines(conn)
close(conn)
appendTv(txt,credits)
buf <- txt$getBuffer()
cursor <- buf$getStartIter()$iter
mark <- buf$createMark(where=cursor)
txt$scrollToMark(mark,within.margin=0)	
}



