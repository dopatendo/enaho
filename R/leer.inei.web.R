#' Leer bases de la ENAHO desde la web
#'
#' Lee bases de la ENAHO deste la web del INEI. Aplica las mismas reglas que
#' \code{\link{leer.inei}}.
#'
#' @inheritParams leer.inei
#'
#' @examples
#' leer.inei.web(modulo = 37, periodos = 2011, tipo = "t1")
#'
#' @returns un data frame o una lista.
#'
#' @export
#'
#'




leer.inei.web <- function(encuesta = "ENAHO",
                          modulo, periodos,
                          tipo = "anual",
                          ensilencio = FALSE,
                          combinar = FALSE,
                          solocomunes = FALSE){



  unlink(file.path(tempdir(),"LEER"),recursive = TRUE)
  dir.create(file.path(tempdir(),"LEER"))

  encuesta <- tolower(encuesta)
  tipo <- tolower(tipo)


  descargar.inei(encuesta = encuesta,
                 modulo = modulo,
                 periodos = periodos,
                 dirdescarga = file.path(tempdir(),"LEER"),
                 tipo = tipo,
                 ensilencio = ensilencio)

  arks <- list.files(file.path(tempdir(),"LEER"),full.names = TRUE)
  arkn <- gsub(paste0(encuesta,"_"),"",basename(arks))


  if(!ensilencio){
    cat(paste0("Leyendo ",length(arks)," archivo",
               ifelse(length(arks)==1,"","s"),".\n"))
  }



  # lei <- .leer(arks = arks, arkn = arkn,ensilencio = ensilencio)
  # unlink(file.path(tempdir(),"LEER"),recursive = TRUE)
  # return(lei)


  out <- .leer(arks = arks, arkn = arkn,ensilencio = ensilencio)
  unlink(file.path(tempdir(),"LEER"),recursive = TRUE)

  if(length(out)==1)
    return(out[[1]])

  if(!combinar)
    return(out)

  return(combinar.inei(out,solocomunes = solocomunes))

}
