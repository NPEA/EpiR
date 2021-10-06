# some orphan callbacks

on_trayIcon_activate <- function(widget,user.data)
{
getWidget("Main")$present()
}
