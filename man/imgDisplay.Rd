\name{imgDisplay}
\alias{imgDisplay}
\title{Display an imagedata object}
\description{
	This function shows a window that displays the imagedata.
}
\usage{imgDisplay(imgdata)}
\arguments{
	\item{imgdata}{The image}
}
\examples{
	x <- readJpeg(system.file("samples", "violet.jpg", package="biOps"))
	imgDisplay(x)
}
\keyword{misc}
