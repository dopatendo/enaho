#' Combinar bases de la ENAHO
#'
#' Combina bases de la ENAHO descargadas con \code{\link{descargar.inei}}. y leídas con
#' \code{\link{leer.inei}}.
#'
#' @inheritParams leer.inei
#' @param x una lista de bases leídas con \code{\link{leer.inei}}.
#'
#' @examples
#' directorio = system.file("extdata", package = "enaho")
#' leido <- leer.inei(modulo = 37, annos = 2011, directorio = directorio, tipo = c("t1","t2"))
#' combinar.inei(leido)
#'
#' @returns un data frame.
#'
#' @export
#'
#'

combinar.inei <- function(x, solocomunes = FALSE){

  cln <- lapply(x,colnames)
  ucln <- unique(unlist(cln))
  tt <- as.data.frame(table(unlist(cln)),stringsAsFactors = FALSE)

  inall <- intersect(ucln,tt[tt[,2]==length(x),1])
  notinall <- setdiff(ucln,tt[tt[,2]==length(x),1])
  inall <- do.call(rbind,lapply(x,function(i){i[,inall]}))

  if(solocomunes)
    return(inall)


  if(length(notinall)==0)
    return(inall)

  notinall <- do.call(rbind,
                      lapply(1:length(x), function(i){
                        uno <- x[[i]][,intersect(cln[[i]],notinall)]
                        # uno <- x[[i]][,intersect(cln[[i]],"notinall")]
                        dos <- setdiff(notinall,cln[[i]])

                        if(length(dos)==0)
                          return(uno)

                        nva <- vector("list",length(dos))
                        for(j in 1:length(dos)){
                          tib <- x[[max(which(sapply(cln,function(w) dos[j]%in%w)))]][,dos[j]]
                          tib <- tib[1,1]
                          tib[1,1] <- NA
                          nvaj <- as.data.frame(matrix(data = NA,nrow = (nrow(x[[i]])-1)))
                          colnames(nvaj) <- dos[j]
                          nva[[j]] <- rbind(tib,nvaj)
                        }
                        nva <- tibble::as_tibble(do.call(cbind,nva))


                        tibble::as_tibble(cbind(uno,nva))[,notinall]

                      })
  )

  return(tibble::as_tibble(cbind(inall,notinall)))


}
