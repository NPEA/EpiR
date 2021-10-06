# about window callback functions

on_btn_SS_homepage_clicked <- function(window,action)
{
browseURL(EPIR_HOMEPAGE)
}

on_btn_SS_close_clicked <- function(widget,user.data)
{
closeWindow("SplashScreen")
}
