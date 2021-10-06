setPlotWindow <- function(name=dev.cur()-1)
# create a plot device using cairo
{
if(!exists("PlotWindow",envir=.EpiREnv))
	{
	setWindow("PlotWindow","PlotWindow") # no modal window
	dev <- getWidget("PlotWindow","drw_PW_device")
	asCairoDevice(dev)
	#title <- getWidget("PlotWindow")$getTitle()
	#getWidget("PlotWindow")$setTitle(paste(title,":",name))
	# remove later
	}
else
	{
	dev <- getWidget("PlotWindow","drw_PW_device")
	asCairoDevice(dev)
	}
}

printPlot <- function(dev=dev.cur(),horizontal=TRUE) 
# print out current plot window
{
cur <- dev.cur()
dev.set(dev)
dev.print(horizontal=horizontal)
dev.set(cur)
}

copyPlot <- function(dev=dev.cur())
# copy current plot window to clipboard
{
filename <- paste(tempfile(),".svg",sep="")
dev.set(dev)
dev.copy(devSVG,file=filename)
dev.off()
img <- gdkPixbufNewFromFile(filename)$retval
gtkClipboardGet("CLIPBOARD")$setImage(img)
file.remove(filename)
}

save.plot <- function(filename,dev=dev.cur())
# save current plot window
{
cur <- dev.cur()
dev.set(dev)
ext <- getExtension(filename)
if(ext=="pdf")
	dev.copy(pdf,file=filename,width=10,height=10)
else if(ext=="png")
	dev.copy(png,file=filename,width=1000,height=1000)
else if(ext=="jpg")
	dev.copy(jpeg,file=filename,width=1000,height=1000)
else if(ext=="svg")
	dev.copy(devSVG,file=filename,width=10,height=10)
else if(ext=="eps")
	{
	dev.copy2eps(file=filename,width=10,height=10)
	return()
	}
else if((ext=="wmf") & is.windows())
	dev.copy(win.metafile,file=filename,width=10,height=10)
else
	return(NA)
dev.off()
dev.set(cur)
}