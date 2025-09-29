#' Proporción
#'
#' Estima proporciones de variables en bases leídas con \code{\link{leer.inei}} o
#' \code{\link{leer.inei.web}}.
#'
#' @param x una cadena de caracteres con los nombres de las variables a estimar.
#' @param base una base leída con \code{\link{leer.inei}} o \code{\link{leer.inei.web}} en forma de tibble
#' o en forma de lista.
#' @param estratos una cadena de caracteres con los nombres de las variables que deben ser consideradas como estratos.
#' Además se puede usar (sólo en las bases de la ENAHO) \code{"area"} y \code{"region"}, con las que se calcularán los resultados para los
#' estratos urbano y rural, y para cada región.
#' @param combinarestratos un valor lógico que indica si debe calcularse también la combinación entre estratos.
#' Por ejemplo, si los estatos son \code{"area"} y \code{"region"}, se estimará también para cada área de
#' cada región.
#' @param dividirperiodos un valor lógico que indica si los resultados deben ser separados por período.
#' @param ruralidad un vector numérico que indica que valores de la variable \code{"ESTRATO"} deben ser considerados
#' rurales. Usualmente del 6 al 8.
#' @param pesos una cadena de caracteres con el nombre de la(s) variable(s) que representan los pesos.
#' Si es \code{NULL} las proporciones se estiman sin pesarlas.
#' @param forzaretiquetas un valor lógico que indica si las etiquetas del período con más etiquetas deben
#' ser forzadas para todos los períodos. No es poco común que las etiquetas de una variable cambien con los
#' años, si esto ocurre, esta función las interpretará como distintas. Por ejemplo \code{"INCIAL"} y
#' \code{"EDUCACION INICIAL"}. Sin embargo, usando este argumento se puede forzar a que se interpreten los
#' mismos valores todos los años. Usar con discreción, y, si es posible, siempre después de una exploración
#' de datos usando \code{forzaretiquetas = FALSE}.
#' @param porcentaje un valor lógico que indica si los resultados deben presentarse como porcentaje en vez de
#' proporción.
#' @param decimales un valor numérico que indica cuántos decimales se necesitan. Si es \code{NULL} no se redondea.
#' @param formatolargo un valor lógico que indica si los resultados debes estar en formato largo.
#' Si se incluye más de una variable, se darán los resultados en formato largo.
#'
#' @examples
#' directorio = system.file("extdata", package = "enaho")
#' leido <- leer.inei(modulo = 37, periodos = 2011, directorio = directorio, tipo = c("t1","t2"))
#' proporcion.inei(x = c("P7061"),base = leido,pesos = "FACTRIM")
#'
#' @returns un data frame o una lista.
#'
#' @export
#'
#'


proporcion.inei <- function(x,
                             base,
                             estratos = NULL,
                             combinarestratos = FALSE,
                             dividirperiodos = FALSE,
                             ruralidad = 6:8,
                             pesos = NULL,
                             forzaretiquetas = FALSE,
                             porcentaje = FALSE,
                             decimales = NULL,
                             formatolargo = FALSE){


  if(length(x)>1){
    formatolargo = TRUE
  }

  out <- vector("list",length(x))
  for(i in 1:length(x)){
    out[[i]] <-   .proporcion.inei(x = x[i],
                                    base = base,
                                    estratos = estratos,
                                    combinarestratos = combinarestratos,
                                    dividirperiodos = dividirperiodos,
                                    ruralidad = ruralidad,
                                    pesos = pesos,
                                    forzaretiquetas = forzaretiquetas,
                                    porcentaje = porcentaje,
                                    decimales = decimales,
                                    formatolargo = formatolargo)
  }

  if(length(x)==1)
    return(out[[1]])


  do.call(rbind,out)





}

.proporcion.inei <- function(x,
                       base,
                       estratos = NULL,
                       combinarestratos = FALSE,
                       dividirperiodos = FALSE,
                       ruralidad = 6:8,
                       pesos = NULL,
                       forzaretiquetas = FALSE,
                       porcentaje = FALSE,
                       decimales = NULL,
                       formatolargo = FALSE){


  if(inherits(base,"list")){
    per <- unlist(lapply(names(base),function(i) paste0(strsplit(i,"_")[[1]][c(1,3)],collapse = "_")))
    per <- gsub("_anual","",per)

    uper <- unique(per)
  }else{
    per <- untibble(base[,1])[,1]
    uper <- unique(per)
  }





  outk <- vector("list",length(uper))
  if(length(uper)==1){dividirperiodos <- FALSE}

  if(length(pesos)==1){
    pesos <- rep(pesos,length(uper))
  }

  for(k in 1:length(uper)){





    if(inherits(base,"list")){
      basek <- base[[k]]
    }else{
      basek <- base[per%in%uper[k],]
    }

    if(!x%in%colnames(basek)){
      warning(paste0("\nVariable ",x," no encontrada en elemento ",k,"."))
      outk[k] <- NULL
    }else{
      if(sum(!is.na(untibble(basek[,x])[,1]))==0){
        warning("\nVariable ",x," sin datos en elemento ",k,".")
        outk[k] <- NULL
      }else{
        if(sum(!is.na(untibble(basek[,x])[,1]))==0)
          stop("\nVariable sin datos.")




        indirectos <- c("region","area")

        directos <- setdiff(estratos,indirectos)
        names(directos) <- directos

        if("region"%in%estratos){
          reg <- list(.regiones(basek,conunicos = TRUE))
          names(reg) <- .tildes("regi")
        }else{
          reg <- NULL
        }
        if("area"%in%estratos){
          rur <- list(.ruralidad(basek,ruralidad = ruralidad))
          names(rur) <- .tildes("area")
        }else{
          rur <- NULL
        }

        nac <- list(Nacional = list(vec = rep("Nacional",nrow(basek)),
                                    unq = "Nacional"))

        nestratosL <- c(reg,rur,lapply(directos,function(i) .vectorizarlbl(i,basek)))


        nestratos <- lapply(nestratosL,function(i) i[[1]])

        unestratos <- lapply(nestratosL,function(i) i[[2]])


        if(length(nestratos)<2){
          combinarestratos <- FALSE
        }

        if(combinarestratos){

          exg <- expand.grid(unestratos[length(unestratos):1])
          exg <- exg[,names(nestratos)]

          vec <- do.call(paste, c(nestratos, list(sep = "_")))
          unq <- do.call(paste,c(c(exg),list(sep = "_")))

          nestratos <- c(nestratos,list(vec))
          names(nestratos) <- c(names(unestratos),paste0(names(unestratos),collapse = "_"))

          unestratos <- c(unestratos,list(unq))
          names(unestratos) <- names(nestratos)
        }

        nestratos <- c(Nacional = list(rep("Nacional",nrow(basek))),nestratos)
        unestratos <- c(Nacional = list("Nacional"),unestratos)



        xx <- untibble(basek[,x])[,1]
        pesok <- pesos[k]

        if(is.null(pesok)){
          pp <- rep(1,nrow(basek))
        }else{
          pp <- untibble(basek[,pesok])[,1]
        }



        lbs <- as.vector(get.atr(basek[,x],"labels"))[[1]]
        if(is.na(lbs)[1]){
          lbs <- sort(unique(untibble(basek[,x])[,1]))
          names(lbs) <- lbs
        }


        madi <- vector("list",length(lbs))
        for(i in 1:length(lbs)){

          madi[[i]] <- 1*(xx%in%lbs[i])

        }
        madi <- do.call(cbind.data.frame,madi)
        colnames(madi) <- names(lbs)

        # madi <- sapply(lbs,function(i){
        #
        #   out <- 1*(xx%in%lbs[i])
        #   # out <- 1*(as.character(xx)%in%as.character(lbs[i]))
        #   # out[is.na(xx)] <- NA
        #   out
        #
        #
        # })

        madi <- (madi*pp)

        outi <- vector("list",length(nestratos))
        for(i in 1:length(nestratos)){
          outj <- vector("list",length(unestratos[[i]]))
          for(j in 1:length(unestratos[[i]])){

            isj <- nestratos[[i]]%in%unestratos[[i]][j]

            madij <- madi[isj,,drop = FALSE]

            xxj <- xx[isj]
            ppj <- pp[isj]

            num <- colSums(madij[!is.na(xxj)&!is.na(ppj),,drop = FALSE])
            den <- sum(ppj[!is.na(xxj)&!is.na(ppj)])

            nd <- (num*(1+99*porcentaje))/den
            if(!is.null(decimales)){
              nd <- round(nd,digits = decimales)
            }

            outj[[j]] <- nd

          }
          names(outj) <- unestratos[[i]]
          outj <- (cbind.data.frame(Periodo = uper[k],
                                    Estrato = names(nestratos)[i],
                                    Nombre = unestratos[[i]],
                                    Variable = x,
                                    do.call(rbind,outj)))
          colnames(outj)[1] <- .tildes("peri")
          rownames(outj) <- NULL
          outi[[i]] <- outj
        }
        outi <- do.call(rbind,outi)

        # ##?????
        # if(!formatolargo){
          outk[[k]] <- outi
        # }else{
        #   novan <- c(.tildes("peri"),"Estrato", "Nombre","Variable")
        #   outi <- stats::reshape(outi,
        #                   idvar = novan,
        #                   varying = list(names(outi)[!names(outi) %in% novan]),
        #                   direction = "long",
        #                   timevar = .tildes("cate"),
        #                   times = names(outi)[!names(outi) %in% novan],
        #                   v.names = .tildes("prop"))
        #   rownames(outi) <- NULL
        #   outk[[k]] <- outi
        # }
      }

    }







  }



  outf <- try(do.call(rbind,outk),silent = TRUE)

  if(inherits(outf,"try-error")){
    outf <- .comb.prop(outk,forzaretiquetas = forzaretiquetas)
  }


  ##?????
  if(!formatolargo){
    outf <- outf
  }else{
    novan <- c(.tildes("peri"),"Estrato", "Nombre","Variable")
    outf <- stats::reshape(outf,
                           idvar = novan,
                           varying = list(names(outf)[!names(outf) %in% novan]),
                           direction = "long",
                           timevar = .tildes("cate"),
                           times = names(outf)[!names(outf) %in% novan],
                           v.names = .tildes("prop"))
    rownames(outf) <- NULL
    outf <- outf
  }


  if(!dividirperiodos)
    return(outf)

  outf <- lapply(unique(outf[,1]),function(i) outf[outf[,1]%in%i,])
  names(outf) <- uper
  return(outf)



}


.comb.prop <- function(lista, forzaretiquetas = FALSE){
  novan <- c(.tildes("peri"),"Estrato", "Nombre","Variable")

  LNA <- lapply(lista,function(x) colnames(x)[!colnames(x)%in%novan])
  LLNA <- (unique(sapply(LNA,length)))
  if(length(LLNA)>1){
    ncol <- TRUE
  }else{

    if(LLNA!=length(unique(unlist(LNA)))){
      ncol <- TRUE
    }else{
      ncol <- FALSE
    }

  }


    if(ncol){
      lna <- lapply(LNA,function(x){
        x <- gsub(" ","",iconv(x=toupper(x),
                               from = "UTF-8",
                               to= "ASCII//TRANSLIT"))
        x <- gsub("['`^~\"]", "", x)
        x <- gsub("-", "", x)
      })
    }else{
      lna <- LNA
    }

  if(!forzaretiquetas){

    ord <- do.call(rbind,lapply(1:length(lista),function(i) {
      cbind.data.frame(num = 1:length(LNA[[i]]),
                       # LNA = LNA[[i]],
                       lna = lna[[i]])
    }))


    unq <- unique(ord[order(ord$num),2])

    out <- do.call(rbind,lapply(1:length(lista),function(i){

      idi <- lista[[i]][,c(novan)]
      col <- LNA[[i]][match(unq,lna[[i]])]
      col[is.na(col)] <- "NOMATCH"
      listi <- cbind(lista[[i]],NOMATCH=NA)[,col]
      colnames(listi) <- unq
      cbind(idi,listi)

    }))

    return(out)
  }

  if(forzaretiquetas){
    cual <- length(lna)-which.max(rev(sapply(lna,length)))+1
    vars <- lna[[cual]]
    ll <- length(vars)

    do.call(rbind,lapply(lista,function(i){
      if(ncol(i)==ll){
        io <- i
      }else{
        io <- cbind(i,matrix(NA,ncol = (ll-ncol(i)+length(novan)),nrow = nrow(i)))
      }

      colnames(io) <- c(novan,vars)
      io

    }))
  }



}


