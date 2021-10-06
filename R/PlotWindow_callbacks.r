# plot window callback functions

on_mnPlotSave_activate <- function(widget,user.data)
{
if(is.windows())
	filter <- list(type=c("Arquivos PNG","Arquivos JPG","Arquivos WMF","Arquivos PDF","Arquivos EPS","Arquivos SVG","Todos os arquivos"),pat=c("*.png","*.jpg","*.wmf","*.pdf","*.eps","*.svg","*"))
else
	filter <- list(type=c("Arquivos PNG","Arquivos JPG","Arquivos PDF","Arquivos EPS","Arquivos SVG","Todos os arquivos"),pat=c("*.png","*.jpg","*.pdf","*.eps","*.svg","*"))

filename <- fileDialog("PlotWindow","Salvar gráfico","save",filter)$filename
if (!is.null(filename))
	{
	# translate
	cmd <- paste("salvar.grafico(\"",filename,"\")",sep="")
	runCommand(cmd)
	}
}

on_mnPlotCopy_activate <- function(widget,user.data)
{
copyPlot()
}

on_mnPlotPrint_activate <- function(widget,user.data)
{
printPlot()
}

on_mnPlotQuit_activate <- function(widget,user.data)
{
closeWindow("PlotWindow")
}

on_mnPlotHelp_activate <- function(widget,user.data)
{
showHelp("PlotWindow")
}



# toolbar
on_btnPlotSave_clicked <- function(widget,user.data)
{
on_mnPlotSave_activate(widget,user.data)
}

on_btnPlotCopy_clicked <- function(widget,user.data)
{
on_mnPlotCopy_activate(widget,user.data)
}

on_btnPlotPrint_clicked <- function(widget,user.data)
{
on_mnPlotPrint_activate(widget,user.data)
}

on_btnPlotQuit_clicked <- function(widget,user.data)
{
on_mnPlotQuit_activate(widget,user.data)
}

on_btnPlotHelp_clicked <- function(widget,user.data)
{
on_mnPlotHelp_activate(widget,user.data)
}

on_PlotWindow_destroy <- function(widget,user.data)
{
# close all devices but the null device
while(dev.cur()>1)
	dev.off()
rm(list="PlotWindow",envir=.EpiREnv)
}
