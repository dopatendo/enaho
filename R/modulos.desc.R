#' Módulos de una encuesta
#'
#' Muestra los módulos de una encuesta y en que año se encuentran disponibles.
#'
#' @inheritParams descargar.inei
#' @param modulos el código de los módulos que mostrar. Si es \code{NULL} muestra todos
#' los disponibles.
#' @param mostrarannos un valor lógico que indica si se deben mostrar los años disponibles.
#'
#' @examples
#' modulos.desc()
#'
#' @returns un data frame.
#'
#' @export
#'
#'

modulos.desc <- function(encuesta = "ENAHO",
                    modulos = NULL,
                    mostrarannos = FALSE){

  encuesta <- tolower(encuesta)

  mods <- readRDS(file.path(system.file("extdata",package = "enaho"),
                            "modulosdata.rds"))


  out <- mods[mods$Encuesta%in%encuesta,-1]

  if(!is.null(modulos)){
    out <- out[as.numeric(out[,1])%in%as.numeric(modulos),]
  }




  if(mostrarannos){
    out <- out[,c(2,1,3)]
  }else{
    out <- out[,c(2,1)]
  }


  return(out)


}

