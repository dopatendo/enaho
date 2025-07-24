#' Media
#'
#' Estima medias de variables en bases leídas con \code{\link{leer.inei}} o
#' \code{\link{leer.inei.web}}.
#'
#' @inheritParams proporcion.enaho
#' @param var una cadena de texto que indica el método a usar para la varianza:
#' \code{"insesgado"} calcula la estimación insesgada (n-1);
#' \code{"MV"} calcula la estimación de máxima verosimilitud.
#'
#' @examples
#' directorio = system.file("extdata", package = "enaho")
#' leido <- leer.inei(modulo = 37, periodos = 2011, directorio = directorio, tipo = c("t1","t2"))
#' media.enaho(x = c("P7061"),base = leido,pesos = "FACTRIM")
#'
#' @returns un data frame o una lista.
#'
#' @export
#'
#'
media.enaho <- function(x,
                        base,
                        estratos = NULL,
                        combinarestratos = FALSE,
                        dividirperiodos = FALSE,
                        ruralidad = 6:8,
                        pesos = "FACTOR07",
                        var = c("insesgado"),
                        decimales = NULL,
                        formatolargo = FALSE){


  if(var=="insesgado"){
    mod <- 1
  }
  if(var=="MV"){
    mod <- 0
  }


  if(length(x)>1){
    formatolargo = TRUE
  }

  out <- vector("list",length(x))
  for(i in 1:length(x)){
    out[[i]] <-   .media.enaho(x = x[i],
                               base = base,
                               estratos = estratos,
                               combinarestratos = combinarestratos,
                               dividirperiodos = dividirperiodos,
                               ruralidad = ruralidad,
                               pesos = pesos,
                               mod = mod,
                               decimales = decimales,
                               formatolargo = formatolargo)
  }

  if(length(x)==1)
    return(out[[1]])


  do.call(rbind,out)





}

.media.enaho <- function(x,
                         base,
                         estratos = NULL,
                         combinarestratos = FALSE,
                         dividirperiodos = FALSE,
                         ruralidad = 6:8,
                         pesos = "FACTOR07",
                         mod = mod,
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



        xx <- as.numeric(untibble(basek[,x])[,1])

        if(is.null(pesos)){
          pp <- rep(1,nrow(basek))
        }else{
          pp <- untibble(basek[,pesos])[,1]
        }


        #
        # lbs <- as.vector(get.atr(basek[,x],"labels"))[[1]]
        # if(is.na(lbs)[1]){
        #   lbs <- sort(unique(untibble(basek[,x])[,1]))
        #   names(lbs) <- lbs
        # }
        #

        # madi <- vector("list",length(lbs))
        # for(i in 1:length(lbs)){
        #
        #   madi[[i]] <- 1*(xx%in%lbs[i])
        #
        # }
        # madi <- do.call(cbind.data.frame,madi)
        # colnames(madi) <- names(lbs)

        # madi <- sapply(lbs,function(i){
        #
        #   out <- 1*(xx%in%lbs[i])
        #   # out <- 1*(as.character(xx)%in%as.character(lbs[i]))
        #   # out[is.na(xx)] <- NA
        #   out
        #
        #
        # })

        # madi <- (xx*pp)

        outi <- vector("list",length(nestratos))
        for(i in 1:length(nestratos)){
          outj <- vector("list",length(unestratos[[i]]))
          outjV <- vector("list",length(unestratos[[i]]))
          for(j in 1:length(unestratos[[i]])){

            isj <- nestratos[[i]]%in%unestratos[[i]][j]


            nma <- cbind(xx[isj], pp[isj])
            nma <- nma[!is.na(nma[, 1]) & !is.na(nma[, 2]), ]
            wm <- sum(nma[, 1] * nma[, 2])/sum(nma[, 2])
            wv <- sum(nma[, 2] * (nma[, 1] - wm)^2)/(sum(nma[, 2]) - mod)



            if(!is.null(decimales)){
              wm <- round(wm,digits = decimales)
              wm <- round(wv,digits = decimales)
            }

            outj[[j]] <- wm
            outjV[[j]] <- wv




          }
          names(outj) <- unestratos[[i]]
          outj <- cbind.data.frame(Periodo = uper[k],
                                   Estrato = names(nestratos)[i],
                                   Nombre = unestratos[[i]],
                                   Variable = x,
                                   Media = unlist(outj),
                                   DE = sqrt(unlist(outjV)),
                                   Varianza = unlist(outjV))
          colnames(outj)[1] <- .tildes("peri")
          rownames(outj) <- NULL
          outi[[i]] <- outj
        }
        outi <- do.call(rbind,outi)

        ##?????
        # if(!formatolargo){
        outk[[k]] <- outi
        # }else{
        #   novan <- c(.tildes("peri"),"Estrato", "Nombre","Variable")
        #   outi <- reshape(outi,
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

  outk

  outf <- do.call(rbind,outk)


  if(!dividirperiodos)
    return(outf)

  outf <- lapply(unique(outf[,1]),function(i) outf[outf[,1]%in%i,])
  names(outf) <- uper
  return(outf)



}



