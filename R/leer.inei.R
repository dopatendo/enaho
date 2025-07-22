#' Leer bases de la ENAHO
#'
#' Lee bases de la ENAHO descargadas con \code{\link{descargar.inei}}. Si más de una base es leída,
#' se carga una lista, cuyos elementos son cada una de las bases.
#' En caso que el INEI provea más de una base por módulo y período se leerá la base
#' con mayor peso.
#'
#' @inheritParams descargar.inei
#' @param directorio el directorio de donde están las carpetas generadas por \code{\link{descargar.inei}}.
#' @param combinar un valor lógico que indica si las bases deben ser combinadas en un único data frame.
#' @param solocomunes un valor lógico que indica si sólo deben combinarse las columnas comunes.
#'
#' @examples
#' directorio = system.file("extdata", package = "enaho")
#' leer.inei(modulo = 37, annos = 2011, directorio = directorio, tipo = "t1")
#'
#' @returns un data frame o una lista.
#'
#' @export
#'
#'

leer.inei <- function(encuesta = "ENAHO",
                      modulo, annos,
                      directorio = getwd(),
                      tipo = "anual",
                      ensilencio = FALSE,
                      combinar = FALSE,
                      solocomunes = FALSE){



  encuesta <- tolower(encuesta)
  tipo <- tolower(tipo)

  if(as.numeric(modulo)<10){
    modulo <- paste0("0",as.numeric(modulo))
  }

  arkobj <- paste0(directorio,"/",encuesta,"_",rep(annos,each = length(tipo)),"_Modulo",modulo,"_",rep(tipo,length(annos)))

  arks <- list.dirs(directorio,recursive = FALSE)
  arks <- intersect(arks,arkobj)

  if(length(arks)>4)
    stop(paste0(paste0("\n",length(arks)," bases encontradas."),
                "\nPara evitar sobrecargar la RAM, solo puede cargar hasta 4 bases a la vez.",
                "\nReduzca la cantidad de bases deseadas."),call. = FALSE)

  arkn <- gsub(paste0(encuesta,"_"),"",gsub("/","",gsub(directorio,"",arks)))

  if(!ensilencio){
    cat(paste0("Leyendo ",length(arks)," archivo",
               ifelse(length(arks)==1,"","s"),".\n"))
  }



    out <- .leer(arks = arks, arkn = arkn,ensilencio = ensilencio)

  if(length(out)==1)
    return(out[[1]])

  if(!combinar)
    return(out)

  return(combinar.inei(out,solocomunes = solocomunes))



}


# funcion de lectura de una base
.leerspss <- function(x, n_max = Inf){

  requireNamespace("haven", quietly = TRUE)

  out <- try(haven::read_spss(file = x,
                              user_na = TRUE,
                              col_select = NULL,
                              skip = 0,
                              n_max = n_max,
                              .name_repair = "unique"),
             silent = TRUE)

  if("try-error"%in%class(out)){
    out <- haven::read_sav(file = x,
                           user_na = TRUE,
                           col_select = NULL,
                           skip = 0,
                           n_max = n_max,
                           .name_repair = "unique",
                           encoding = "latin1")
  }

  return(out)


}

# funcion de lectura de multiples bases
.leer <- function(arks, arkn, ensilencio = FALSE){

  lei <- vector("list",length = length(arks))

  for(i in 1:length(arks)){

    arki <- file.info(list.files(arks[i],pattern = "sav$",
                                 ignore.case = TRUE,full.names = T))
    arki <- rownames(arki)[which.max(arki$size)]

    if(!ensilencio){
      cat(paste0("Leyendo archivo ",i," de ",
                 length(arks),".\n"))
    }

    lei[[i]] <- .leerspss(arki)
  }

  names(lei) <- arkn

  return(lei)


}
