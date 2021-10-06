# plugins api

loadPlugins <- function()
# load the plugin chain
{
# list the plugins
plugins <- list.files(path=EPIR_PATH_PLUGINS,pattern="\\.[Rr]$",full.names=TRUE)
# load the plugins
if(length(plugins)>0)
	for (i in plugins)
		source(i,local=TRUE)
rm(i,plugins)
functions <- ls()
detach(.EpiREnv)
for (f in functions)
	assign(f,eval(parse(text=f)),envir=.EpiREnv)
attach(.EpiREnv,warn.conflicts=FALSE)
}


addPluginMenu <- function()
# adds a plugin menu
{

}


loadPluginGui <- function()
# adds a plugin menu
{

}


