# printing system
# I don't know how to deal with that. I'm just using it

# text
begin_print_txt <- function(operation,context,user_data)
{
data <- operation$getData("print_data")
height <- context$getHeight()-EPIR_HEADER_HEIGHT-EPIR_HEADER_GAP
data$lines_per_page <- floor(height/data$font_size)
data$lines <- readLines(data$filename)
data$num_pages <- ceiling((length(data$lines)-1)/data$lines_per_page)
operation$setNPages(data$num_pages)
operation$setData("print_data",data)
}

draw_page_txt <- function(operation,context,page_nr,user_data)
{
data <- operation$getData("print_data")
cr <- context$getCairoContext()
width <- context$getWidth()
# shade / outline header using cairo
cr$rectangle(0,0,width,EPIR_HEADER_HEIGHT)
cr$setSourceRgb(0.8,0.8,0.8)
cr$fillPreserve()
cr$setSourceRgb(0,0,0)
cr$setLineWidth(1)
cr$stroke()
# set up pango layout
layout <- context$createPangoLayout()
desc <- pangoFontDescriptionFromString("sans 10")
layout$setFontDescription(desc)
# write file name at top of page
layout$setText(basename(data$filename))
layout$setWidth(width)
layout$setAlignment("center")
layout_height <- layout$getSize()$height
text_height <- layout_height/PANGO_SCALE
cr$moveTo(width/2,(EPIR_HEADER_HEIGHT-text_height)/2)
pangoCairoShowLayout(cr,layout)
# write page at top right
page_str <- paste(page_nr+1,"/",data$num_pages,sep="")
layout$setText(page_str)
layout$setAlignment("right")
cr$moveTo(width-2,(EPIR_HEADER_HEIGHT-text_height)/2)
pangoCairoShowLayout(cr,layout)
# prepare to write text in different font / size
layout <- context$createPangoLayout()
desc <- pangoFontDescriptionFromString("mono")
desc$setSize(data$font_size*PANGO_SCALE)
layout$setFontDescription(desc)
# write text
cr$moveTo(0,EPIR_HEADER_HEIGHT+EPIR_HEADER_GAP)
first <- page_nr*data$lines_per_page
sapply(data$lines[first:(first+data$lines_per_page-1)],function(line){
	if(is.na(line))
		return()
	layout$setText(line)
	pangoCairoShowLayout(cr,layout)
	cr$relMoveTo(0,data$font_size)
	})
}


# images
begin_print_img <- function(operation,context,user_data)
{
data <- operation$getData("print_data")
height <- context$getHeight()-EPIR_HEADER_HEIGHT-EPIR_HEADER_GAP
data$lines_per_page <- floor(height/data$font_size)
data$lines <- readLines(data$filename)
data$num_pages <- ceiling((length(data$lines)-1)/data$lines_per_page)
operation$setNPages(data$num_pages)
operation$setData("print_data",data)
}

draw_page_img <- function(operation,context,page_nr,user_data)
{
data <- operation$getData("print_data")
cr <- context$getCairoContext()
width <- context$getWidth()
# shade / outline header using cairo
cr$setSourceSurface(cairoImageSurfaceCreateFromPng("teste.png"),0,0)
}

