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
#' @param combinarsolocomunes un valor lógico que indica si sólo deben combinarse las columnas comunes.
#' @param columnas una cadena de texto que indica qué columnas seleccionar.
#' @param solocomunes un valor lógico que indica si sólo deben leerse las columnas comunes.
#'
#' @examples
#' directorio = system.file("extdata", package = "enaho")
#' leer.inei(modulo = 37, periodos = 2011, directorio = directorio, tipo = "t1")
#'
#' @returns un data frame o una lista.
#'
#' @export
#'
#'

leer.inei <- function(encuesta = "ENAHO",
                      modulo, periodos,
                      directorio = getwd(),
                      tipo = "anual",
                      columnas = NULL,
                      solocomunes = FALSE,
                      ensilencio = FALSE,
                      combinar = FALSE,
                      combinarsolocomunes = FALSE
                      ){





  if(solocomunes){

    vxp <- variablesxperiodo(encuesta = encuesta,
                             modulo = modulo,
                             periodos = periodos,
                             directorio = directorio,
                             tipo = tipo)

    vxp <- vxp[rowSums(vxp[,-1],na.rm = TRUE)==(ncol(vxp)-1),1]

    if(is.null(columnas)){
      columnas <- vxp
    }else{
      columnas <- intersect(vxp,columnas)
    }


  }

.leer.inei(encuesta = encuesta,
           modulo = modulo,
           periodos = periodos,
           directorio = directorio,
           tipo = tipo,
           ensilencio = ensilencio,
           combinar = combinar,
           combinarsolocomunes = combinarsolocomunes,
           soloatributos = FALSE,
           col_select = columnas,
           unlist = TRUE)


}


.leer.inei <- function(encuesta = "ENAHO",
                      modulo, periodos,
                      directorio = getwd(),
                      tipo = "anual",
                      ensilencio = TRUE,
                      combinar = FALSE,
                      combinarsolocomunes = TRUE,
                      soloatributos = FALSE,
                      col_select = NULL,
                      unlist = TRUE){



  encuesta <- tolower(encuesta)
  tipo <- tolower(tipo)

  if(as.numeric(modulo)<10){
    modulo <- paste0("0",as.numeric(modulo))
  }

  arkobj <- paste0(directorio,"/",encuesta,"_",rep(periodos,each = length(tipo)),"_Modulo",modulo,"_",rep(tipo,length(periodos)))

  arks <- list.dirs(directorio,recursive = FALSE)
  arks <- intersect(arks,arkobj)

  # if(length(arks)>4)
  #   warning(paste0(paste0("\n",length(arks)," bases encontradas."),
  #               "\nPara evitar sobrecargar la RAM, se recomienda no cargar hasta 4 bases a la vez.",
  #               "\nReduzca la cantidad de bases deseadas."),call. = FALSE)

  arkn <- gsub(paste0(encuesta,"_"),"",basename(arks))

  if(!ensilencio){
    cat(paste0("Leyendo ",length(arks)," archivo",
               ifelse(length(arks)==1,"","s"),".\n"))
  }



  out <- .leer(arks = arks, arkn = arkn,
               ensilencio = ensilencio,
               n_max = ifelse(soloatributos,0,Inf),
               col_select = col_select)

  if(length(out)==1)
    if(unlist){
      return(out[[1]])
    }else{
      return(out)
    }


  if(!combinar)
    return(out)

  return(combinar.inei(out,combinarsolocomunes = combinarsolocomunes))



}


# funcion de lectura de una base
.leerspss <- function(x, n_max = Inf, col_select = NULL){

  requireNamespace("haven", quietly = TRUE)

  if(is.null(col_select)){
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
  }else{
    # outn <- try(haven::read_spss(file = x,
    #                             user_na = TRUE,
    #                             col_select = NULL,
    #                             skip = 0,
    #                             n_max = 0,
    #                             .name_repair = "unique"),
    #            silent = TRUE)
    #
    # if("try-error"%in%class(outn)){
    #   outn <- haven::read_sav(file = x,
    #                          user_na = TRUE,
    #                          col_select = NULL,
    #                          skip = 0,
    #                          n_max = 0,
    #                          .name_repair = "unique",
    #                          encoding = "latin1")
    # }
    #
    # coll <- intersect(col_select,colnames(outn))



    out <- try(haven::read_spss(file = x,
                                 user_na = TRUE,
                                 col_select = tidyselect::any_of(col_select),
                                 skip = 0,
                                 n_max = n_max,
                                 .name_repair = "unique"),
                silent = TRUE)

    if("try-error"%in%class(out)){
      out <- haven::read_sav(file = x,
                              user_na = TRUE,
                              col_select = tidyselect::any_of(col_select),
                              skip = 0,
                              n_max = n_max,
                              .name_repair = "unique",
                              encoding = "latin1")
    }


    return(out)
  }




}

# funcion de lectura de multiples bases
.leer <- function(arks, arkn, ensilencio = FALSE, n_max = Inf, col_select = NULL){

  lei <- vector("list",length = length(arks))

  for(i in 1:length(arks)){

    arki <- file.info(list.files(arks[i],pattern = "sav$",
                                 ignore.case = TRUE,full.names = T))
    arki <- rownames(arki)[which.max(arki$size)]

    if(!ensilencio){
      cat(paste0("Leyendo archivo ",i," de ",
                 length(arks),".\n"))
    }

    lei[[i]] <- .leerspss(arki,n_max = n_max, col_select = col_select)
  }

  names(lei) <- arkn

  return(lei)


}


