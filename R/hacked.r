# hacked function

dev.copy2eps <- function(...)
# this is a hacker to the original dev.copy2eps function in order to include Cairo
{
current.device <- dev.cur()
nm <- names(current.device)[1]
if (nm == "null device")
	stop("no device to print from")
if (!(nm %in% c("Cairo","X11", "GTK", "gnome", "windows", "quartz")))
	stop("can only print from screen device")
oc <- match.call()
oc[[1]] <- as.name("dev.copy")
oc$device <- postscript
oc$onefile <- FALSE
oc$horizontal <- FALSE
if (is.null(oc$paper))
	oc$paper <- "special"
din <- graphics::par("din")
w <- din[1]
h <- din[2]
if (is.null(oc$width))
	oc$width <- if (!is.null(oc$height))
		w/h * eval.parent(oc$height)
	else w
if (is.null(oc$height))
	oc$height <- if (!is.null(oc$width))
		h/w * eval.parent(oc$width)
	else h
if (is.null(oc$file))
	oc$file <- "Rplot.eps"
dev.off(eval.parent(oc))
dev.set(current.device)
}

stem <- function (x, scale = 1, width = 80, atom = 1e-08)
{
if (!is.numeric(x))
	stop("'x' must be numeric")
x <- x[!is.na(x)]
if (length(x) == 0)
	stop("no non-missing values")
if (scale <= 0)
	stop("'scale' must be positive")
.C("stemleaf", as.double(x), length(x), as.double(scale),
	as.integer(width), as.double(atom), PACKAGE = "EpiR")
invisible(NULL)
}



