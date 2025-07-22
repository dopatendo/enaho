#' Módulos de una encuesta
#'
#' Muestra los módulos de una encuesta y en que año se encuentran disponibles.
#'
#' @inheritParams descargar.inei
#' @param modulo el código de los módulos que mostrar. Si es \code{NULL} muestra todos
#' los disponibles. Si no es \code{NULL} muestra los períodos disponibles.
#'
#' @examples
#' modulos()
#'
#' @returns un data frame.
#'
#' @export
#'
#'



modulos <- function(encuesta = "ENAHO",
                    modulo = NULL){

  encuesta <- tolower(encuesta)

  mods <- readRDS(file.path(system.file("extdata",package = "enaho"),
                            "modulosdata.rds"))


  out <- mods[mods$Encuesta%in%encuesta,-1]

  if(is.null(modulo)){
    out <- out[,c(2,1)]
    return(out)
  }


  outf <- vector("list",length(modulo))
  for(i in 1:length(modulo)){
    ann <- unique(unlist(strsplit(out[,3],";")))

    outi <- out[as.numeric(out[,1])%in%as.numeric(modulo[i]),]
    nom <- outi$Nombre
    mod <- outi[,1]
    dia <- as.numeric(ann%in%strsplit(out[,3],";")[[1]])

    di1 <- as.numeric(paste0(ann,"-t1")%in%strsplit(out[,4],";")[[1]])
    di2 <- as.numeric(paste0(ann,"-t2")%in%strsplit(out[,4],";")[[1]])
    di3 <- as.numeric(paste0(ann,"-t3")%in%strsplit(out[,4],";")[[1]])
    di4 <- as.numeric(paste0(ann,"-t4")%in%strsplit(out[,4],";")[[1]])


    outi <- cbind.data.frame(ann,dia,di1,di2,di3,di4)
    colnames(outi)[1] <- substr(colnames(mods)[4],1,3)
    colnames(outi)[-1] <- c("Anual","T1","T2","T3","T4")
    outi <- cbind.data.frame(Nombre = nom,mod,outi)
    colnames(outi)[2] <- colnames(mods)[2]
    outf[[i]] <- outi

  }

  outf <- do.call(rbind,outf)

  return(outf)


}
