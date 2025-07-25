#' Identificar variables
#'
#' Identifica las variables disponibles por periodo y tipo de bases de la ENAHO
#' descargadas con \code{\link{descargar.inei}}.
#'
#' @inheritParams leer.inei
#' @param ultimaetiqueta un valor lógico que indica si debe usarse la última etiqueta.
#' Dado que la etiqueta de las variables puede cambiar con los años, se puede elegir qué etiqueta usar,
#' la del período más reciente que tiene esa variable  \code{ultimaetiqueta = TRUE} o del primer período
#' que tiene esa variable \code{ultimaetiqueta = FALSE}.
#' @param mostraretiqueta un valor lógico que indica si deben mostrarse las etiquetas.
#'
#' @examples
#' directorio = system.file("extdata", package = "enaho")
#' variablesxperiodo(modulo = 37, periodos = 2011, directorio = directorio, tipo = c("t1","t2"))
#'
#' @returns un data frame donde 1 significa que la variable está disponible y 0 que no está disponible.
#'
#' @export
#'
#'

variablesxperiodo <- function(encuesta = "ENAHO",
                              modulo,
                              periodos,
                              directorio = getwd(),
                              tipo = "anual",
                              ultimaetiqueta = TRUE,
                              mostraretiqueta = FALSE){


  blei <- .leer.inei(encuesta = encuesta,
                    modulo = modulo,
                    periodos = periodos,
                    directorio = directorio,
                    tipo = tipo,
                    soloatributos = TRUE,
                    ensilencio = TRUE,
                    unlist = FALSE)



  lna <- lapply(blei,colnames)
  ulna <- unique(unlist(lna))

  varlab <- lapply(blei,function(i) get.varlab(i))
  varlab <- lapply(ulna,function(i){
    sapply(varlab,function(j){
      j[j[,1]%in%i,2]
    })
  })



  ets <- as.vector(sapply(varlab,function(i){
    out <- i[length(i)!=0]
    out <- unlist(i)[1]
    if(ultimaetiqueta)
      return(out[length(out)])

    return(out[1])

  }))


  avai <- sapply(lna,function(i) (ulna%in%i)*1)
  colnames(avai) <- sapply(strsplit(colnames(avai),"_"),function(i) paste0(i[1],"_",i[3]))
  colnames(avai) <- gsub("_anual","",colnames(avai))
  avai <- cbind.data.frame(Etiqueta = ets, Nombre = ulna,(avai))
  if(!mostraretiqueta){
    avai <- avai[,-1]
  }


  return(avai)

}
