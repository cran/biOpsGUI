# 
# This file is part of biOpsGUI.
# 
#     biOpsGUI is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
# 
#     biOpsGUI is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with biOpsGUI; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# 


#
#	Title: Display imagedata Operations
#


imgDisplay <- function(imgdata){
	depth <- if (attr(imgdata, "type") == "grey") 1 else dim(imgdata)[3] # get images depth
	width <- dim(imgdata)[2]
	height <- dim(imgdata)[1]

	window <- gtkWindow("toplevel", show = F)

	window$setDefaultSize(if (width > 400) 400 else width, if (height > 400) 400 else height)
	vbox <- gtkVBox(FALSE, FALSE)
	darea <- gtkDrawingArea()
	darea$setSizeRequest(width, height)
	gtkWidgetAddEvents(darea, 4)
	# 4 = 1 << 2 => GdkEventMask['pointer-motion-mask']
	status_bar <- gtkStatusbar()

	sw <- gtkScrolledWindow()
	gtkScrolledWindowSetPolicy(sw, GtkPolicyType['automatic'], GtkPolicyType['automatic'])
	gtkScrolledWindowAddWithViewport(sw, darea)
	gtkBoxPackStart(vbox, sw)
	gtkBoxPackEnd(vbox, status_bar, FALSE, FALSE)
	window$add(vbox)

	if (depth == 1){
		r <- as.vector(t(imgdata))
		g <- r
		b <- r
	}else{
		r <- as.vector(t(imgdata[,,1]))
		g <- as.vector(t(imgdata[,,2]))
		b <- as.vector(t(imgdata[,,3]))
	}
	buf <- rbind(r, g, b)

	gSignalConnect(darea, "motion-notify-event", .mouse_move_callback, list(status_bar, buf, width, height, depth))
	gSignalConnect(darea, "expose-event", .on_darea_expose, buf)

	window$showAll()

	gdk_win <- gtkWidgetGetWindow(darea)
	cross_cursor <- gdkCursorNew(as.integer(30))
	# cursors: http://developer.gnome.org/doc/API/2.0/gdk/gdk-Cursors.html
	gdkWindowSetCursor(gdk_win, cross_cursor)
}

.on_darea_expose <- function(widget, event, buf){
	window <- gtkWidgetGetWindow(widget)
	size <- gtkWidgetGetSizeRequest(widget)
	style <- gtkWidgetGetStyle(widget)
	fcgc <- gtkStyleGetFgGc(style)
	sn <- GtkStateType["normal"]+1
	
	gdkDrawRgbImage(window, fcgc[[sn]], 0, 0, size$width, size$height, "max", buf, size$width * 3)

	return(TRUE)
}

# .on_darea_expose_gray <- function(widget, event, buf){
# 	window <- gtkWidgetGetWindow(widget)
# 	size <- gtkWidgetGetSizeRequest(widget)
# 	style <- gtkWidgetGetStyle(widget)
# 	fcgc <- gtkStyleGetFgGc(style)
# 	sn <- GtkStateType["normal"]+1
# 	
# 	gdkDrawGrayImage(window, fcgc[[sn]], 0, 0, size$width, size$height, "max", buf)
# }


# Handling button-press events
.mouse_move_callback <- function(darea, event, data){
	status_bar <- data[[1]]
	imgdata <- data[[2]]
	width <- data[[3]]
	height <- data[[4]]
	depth <- data[[5]]

	x <- event[["x"]]
	y <- event[["y"]]

	gtkStatusbarPop(status_bar, 1)
	if (x >= width || y >= height)
		return(TRUE)

	if (depth == 1){
		V <- imgdata[1, x + y * width]
		msg <- paste("x : ", x, ", y :", y, "        V : ", V)
	}else{
		R <- imgdata[1, x + y * width]
		G <- imgdata[2, x + y * width]
		B <- imgdata[3, x + y * width]
		msg <- paste("x : ", x, ", y :", y, "        R : ", R, " G : ", G, " B : ", B)
	}
	
	gtkStatusbarPush(status_bar, 1, msg)
	# Returning TRUE means we handled the event, so the signal
	# emission should be stopped (don't call any further
	# callbacks that may be connected). Return FALSE
	# to continue invoking callbacks.
	return(TRUE)
}

