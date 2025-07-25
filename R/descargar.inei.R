#' Descargar
#'
#' Descarga datos disponibles del portal de Microdatos del INEI.
#'
#' @param encuesta el nombre de una encuesta del INEI. Las opciones dispobibles son
#' \code{"ENAHO"}, \code{"ENAHOpanel"} y \code{"ENDES"}. No importa si está escrito en mayúscula o minúscula.
#' @param modulo el código del módulo deseado.
#' @param periodos los años deseados.
#' @param dirdescarga el directorio de descarga.
#' @param tipo el tipo de período deseado. Las opciones son \code{"anual"},
#'   \code{"t1"} (primer trimestre),
#'   \code{"t2"} (segundo trimestre),
#'   \code{"t3"} (tercer trimestre),
#' y \code{"t4"} (cuarto trimestre). Las opciones pueden combinarse.
#' @param ensilencio un valor lógico que indica si el progreso de la función debe ser omitido.
# @param descomprimir un valor lógico que indica si la descarga debe ser descomprimida.
#'
#' @examples
#' descargar.inei(modulo = 37, periodos = 2011, dirdescarga = tempdir(), tipo = "t1")
#'
#' @returns guarda los archivos descargados en el disco.
#'
#' @export
#'
#'

descargar.inei <- function(encuesta = "ENAHO",
                           modulo, periodos,
                           dirdescarga = getwd(),
                           tipo = "anual",
                           # descomprimir = TRUE,
                           ensilencio = FALSE){

  tmis <- getOption("timeout")
  options(timeout = Inf)
  on.exit(options(timeout = tmis))

  encuesta <- tolower(encuesta)
  tipo <- tolower(tipo)

  fixurl <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS"

  if(as.numeric(modulo)<10){
    modulo <- paste0("0",as.numeric(modulo))
  }




  if(tipo[1]=="anual"){

    mm <- readRDS(file.path(system.file("extdata",package = "enaho"),
                              "modulosdata.rds"))[,c(2,3,4)]

    CE <- readRDS(file.path(system.file("extdata",package = "enaho"),
                                      "codigosencuesta.rds"))
    CE <- CE[(CE$encuesta%in%encuesta)&(CE[,2]%in%strsplit(mm[,3],";")[[1]])&(CE$tipo==tipo),]
    CE <- CE[CE[,2]%in%periodos,]
  }else{
    mm <- readRDS(file.path(system.file("extdata",package = "enaho"),
                            "modulosdata.rds"))
    mm <- mm[as.numeric(mm[,2])%in%as.numeric(modulo),]
    CE <-   readRDS(file.path(system.file("extdata",package = "enaho"),
                                      "codigosencuesta.rds"))
    CE <- CE[CE$encuesta%in%encuesta,]
    CE <- CE[paste0(CE[,2],"-",CE[,4])%in%unlist(strsplit(mm[,5],";")),]
    CE <- CE[CE$tipo%in%tipo,]

    CE <- CE[CE[,2]%in%periodos,]


  }




  desc <- paste0(CE$codigo,"-Modulo",modulo,".zip")
  desc2 <- sapply(1:nrow(CE),function(i){
    paste0(CE$encuesta[i],"_",CE[,2][i],"_Modulo",modulo,"_",CE$tipo[i],".zip")
  })
  desc3 <- paste0(gsub(".zip","",desc2))


  # if(descomprimir){
    unlink(file.path(tempdir(),"desc"),recursive = TRUE)
    dir.create(file.path(tempdir(),"desc"))

    if(!ensilencio){
      cat(paste0("Descargando ",length(file.path(fixurl,desc))," archivo",
                 ifelse(length(file.path(fixurl,desc))==1,"","s"),".\n"))
    }


    suppressWarnings( utils::download.file(url = file.path(fixurl,desc),
                                    destfile = file.path(tempdir(),"desc",desc2),quiet = TRUE))



    # for(i in 1:length(file.path(dirdescarga,desc2))){
    #   unzip(zipfile = file.path(tempdir(),"desc",desc2)[i],
    #         exdir = file.path(dirdescarga,desc3[i]),
    #         junkpaths = TRUE)
    # }
    #

    for(i in 1:length(file.path(dirdescarga,desc2))){
      tr <- suppressWarnings(try(utils::unzip(zipfile = file.path(tempdir(),"desc",desc2)[i],
                                       exdir = file.path(dirdescarga,desc3[i]),
                                       junkpaths = TRUE),silent = TRUE))
      if(inherits(tr,"try-error")){

        li <- utils::unzip(file.path(tempdir(),"desc",desc2)[i], list = TRUE)
        for(k in 2:nrow(li)){
          suppressWarnings(try(utils::unzip(zipfile = file.path(tempdir(),"desc",desc2)[i],
                                     exdir = file.path(dirdescarga,desc3[i]),
                                     files = li$Name[k],
                                     junkpaths = TRUE),silent = TRUE))
        }


      }

    }








    unlink(file.path(tempdir(),"desc"),recursive = TRUE)
  # }else{
  #   suppressWarnings( download.file(url = file.path(fixurl,desc),
  #                                   destfile = file.path(dirdescarga,desc2),quiet = TRUE))
  # }



}


