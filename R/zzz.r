# initialization functions

.onLoad <- function(libname,pkgname)
# start up options
{
msg <- "\nEpi-R é uma interface gráfica para análise de dados epidemiológicos em R,
desenvolvido no Instituto de Medicina Social da UERJ em parceria com o Ministério da Saúde.
\n\n"
# Digite \"EpiR()\" para iniciar o programa principal. ### for safe keeping
#if (tolower(Sys.info()["sysname"])=="windows")
#	cat(iconv(msg,from="UTF-8",to="ISO-8859-1"))
#else
installed <- installed.packages()
if(!"RGtk2" %in% installed)
	{
	site <- url("http://www.r-project.org/")
	test.site <- try(open(site),silent=TRUE)
	if(class(test.site) == "try-error")
		{
		cat("O computador não consegue se conectar à Internet e não será possível atualizar o pacote RGtk2.\n")
		return()
		}		
	else
		{
		close(site)
		r <- getOption("repos")
		r["CRAN"] <- "http://cran.fiocruz.br/"
		options(repos=r)
		install.packages("RGtk2",lib=.libPaths()[1])
		}
	}
cat(msg)
library.dynam("EpiR","EpiR",lib.loc=NULL)
}


.onAttach <- function(libname,pkgname)
# start up options
{
EpiR()
}


.onUnload <- function(libpath)
{
# unloading process 
library.dynam.unload("EpiR",libpath)
}
 
