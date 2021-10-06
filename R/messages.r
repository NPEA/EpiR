# error functions

error <- function(errorcode)
# generate a proper error message
{
message <- switch(errorcode,
	xlsfilter = "A opção XLS funciona apenas no Windows",
	missingx = "A variável não foi informada",
	executing = "Erro: Não foi possível executar o comando",
	missingdata = "O banco de dados está faltando",
	fillLevels="Variável deve conter pelo menos 4 valores distintos",
	# otherwise
	"Erro desconhecido"
	)

if(get("EPIR_GUI",envir=.EpiREnv))
	msgDialog(NULL,"error",message)
else
	stop(paste("\b\b\b:",message),call.=FALSE)	
}
	
	
