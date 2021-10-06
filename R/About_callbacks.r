# about window callback functions

on_btn_A_homepage_clicked <- function(widget,user.data)
{
browseURL(EPIR_HOMEPAGE)
}

on_btn_A_close_clicked <- function(widget,user.data)
{
closeWindow("About")
}


