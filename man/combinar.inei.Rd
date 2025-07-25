% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combinar.inei.R
\name{combinar.inei}
\alias{combinar.inei}
\title{Combinar bases de la ENAHO}
\usage{
combinar.inei(x, combinarsolocomunes = FALSE)
}
\arguments{
\item{x}{una lista de bases leídas con \code{\link{leer.inei}}.}

\item{combinarsolocomunes}{un valor lógico que indica si sólo deben combinarse las columnas comunes.}
}
\value{
un data frame.
}
\description{
Combina bases de la ENAHO descargadas con \code{\link{descargar.inei}}. y leídas con
\code{\link{leer.inei}}.
}
\examples{
directorio = system.file("extdata", package = "enaho")
leido <- leer.inei(modulo = 37, periodos = 2011, directorio = directorio, tipo = c("t1","t2"))
combinar.inei(leido)

}
