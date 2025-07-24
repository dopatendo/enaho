#' Internal functions
#'


# Castellano --------------------------------------------------------------

.tildes <- function(x){
  li <- list(prop = "Proporción",
             cate = "Categoría",
             peri = "Período",
             area = "Área",
             regi = "Región")

  li[[x]]

}


# Convertir labels --------------------------------------------------------



.vectorizarlbl <- function(x,base){

  if(!haven::is.labelled(base[,x,drop = TRUE])){
    vec <- untibble(base[,x])[,1]
    unq <- sort(unique(vec))

  }else{
    lbs <- as.vector(get.atr(base[,x],"labels"))[[1]]
    vec <- untibble(base[,x])[,1]
    vec <- names(lbs)[match(vec,lbs)]
    unq <- names(lbs)
  }



  return(list(vec=vec,unq=unq))
}



# inei --------------------------------------------------------------------

.regiones <- function(base,conunicos = TRUE){

  csv <- "/Users/andreschristiansen/RandA Dropbox/Andrés Christiansen/khipuverse/enaho/ztools/UBIGEOS_2022_1891_distritos.csv"
  csv <- read.csv2(csv,colClasses = "character")
  csv <- csv[,1:2]
  csv$IDDIST <- substr(csv$IDDIST,1,2)
  csv <- (unique(csv))

  ubi <- substr(ILSAmerge::untibble(base[,"UBIGEO"])[,1],1,2)
  ubi <- csv$NOMBDEP[match(ubi,csv$IDDIST)]

  if(!conunicos)
    return(ubi)

  return(list(vec = ubi, unq = sort(unique(ubi))))
}


.ruralidad <- function(base, ruralidad = 6:8){
  rur <- ILSAmerge::untibble(base[,"ESTRATO"])[,1]
  vec <- rep("Urbana",length(rur))
  vec[rur%in%ruralidad] <- "Rural"

  unq <- c("Urbana","Rural")

  return(list(vec=vec,unq=unq))
}


# Get attributes ----------------------------------------------------------

untibble <- function(tibble, mistoNAs = FALSE){



  # Checks ------------------------------------------------------------------

  if(!inherits(tibble,"list")){
    if(!inherits(tibble, "tbl_df"))
      stop(c("\nInvalid input for 'tibble'.",
             "\nIt should be a tibble or a list of tibbles."),call. = FALSE)
  }else{
    if(!all(sapply(tibble, inherits, what = "tbl_df")))
      stop(c("\nInvalid input for 'tibble'.",
             "\nIt should be a tibble or a list of tibbles."),call. = FALSE)

  }


  if(!(isTRUE(mistoNAs)|isFALSE(mistoNAs)))
    stop(c("\nInvalid input for 'mistoNAs'.",
           "\nIt should be a logical value."),call. = FALSE)


  # Process & Output --------------------------------------------------------



  if(!inherits(tibble,"list"))
    return(.untibble(tibble, mistoNAs = mistoNAs))

  out <- vector("list",length(tibble))
  for(i in 1:length(out)){
    out[[i]] <- .untibble(tibble[[1]], mistoNAs = mistoNAs)
  }
  return(out)

}

.untibble <- function(tibble, mistoNAs = FALSE){


  otibble <- as.data.frame(tibble)
  for(i in 1:ncol(tibble)){
    otibble[,i] <- as.vector(otibble[,i,drop = TRUE])
  }

  if(mistoNAs){

    try(haven::print_labels(),silent = TRUE)

    otibble[is.na(tibble)] <- NA

  }


  return(otibble)

}


get.varlab <- function(tibble){


  # Checks ------------------------------------------------------------------

  if(!inherits(tibble, "tbl_df"))
    stop(c("\nInvalid input for 'tibble'.",
           "\nIt should be a tibble."),call. = FALSE)


  # Process & Output --------------------------------------------------------

  out <- unlist(get.atr(tibble,"label",NULLasNA = TRUE))
  out <- cbind.data.frame(name = names(out),
                          varlab = out)
  rownames(out) <- NULL
  return(out)
}


get.atr <- function(tibble, which, NULLasNA = TRUE, exact = FALSE){

  # Checks ------------------------------------------------------------------

  if(!inherits(tibble, "tbl_df"))
    stop(c("\nInvalid input for 'tibble'.",
           "\nIt should be a tibble."),call. = FALSE)

  if(!(isTRUE(NULLasNA)|isFALSE(NULLasNA)))
    stop(c("\nInvalid input for 'NULLasNA'.",
           "\nIt should be a logical value."),call. = FALSE)

  # Process & Output --------------------------------------------------------


  out <- lapply(1:ncol(tibble), function(i){
    atr <- attr(tibble[,i,drop = TRUE],which, exact = exact)

    if(NULLasNA&&is.null(atr)){return(NA)}

    return(atr)

  })
  names(out) <- colnames(tibble)

  return(out)

}


get.nas <- function(tibble, aslist = TRUE){

  # Checks ------------------------------------------------------------------

  if(!inherits(tibble, "tbl_df"))
    stop(c("\nInvalid input for 'tibble'.",
           "\nIt should be a tibble."),call. = FALSE)

  if(!(isTRUE(aslist)|isFALSE(aslist)))
    stop(c("\nInvalid input for 'aslist'.",
           "\nIt should be a logical value."),call. = FALSE)


  # Process & Output --------------------------------------------------------

  nav <- get.atr(tibble,"na_values",NULLasNA = FALSE)
  nar <- get.atr(tibble,"na_range",NULLasNA = FALSE)
  nar <- lapply(nar,function(i) if(is.null(i)){NULL}else{min(i):max(i)})

  out <- lapply(1:length(nav),function(i) sort(c(nav[[i]],nar[[i]])))
  names(out) <- colnames(tibble)
  out

  if(aslist)
    return(out)


  out <- cbind.data.frame(name = names(out),
                          NAs = unlist(lapply(out,function(i) paste0(i,collapse = ";"))))
  rownames(out) <- NULL
  return(out)

}


