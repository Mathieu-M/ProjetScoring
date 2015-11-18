
# Fonctions basiques pour les tests -----------------------------------------------

any.negative <- function(x){
  return(any(x<0,na.rm=TRUE))
}

any.na <- function(x){
  return(any(is.na(x)))
}


# Equivalence entre missing values et level d'un facteur ------------------

#'
#'@param x a factor with 2 levels.
#'@param y a numeric with the same length as x. It can be the other way around.
#'@return logical indicating whether the missing values from the numeric are all associated with one levels
#'of the factor.
#'
similFactorNum <- function(x,y){
  if(is.factor(x) & nlevels(x)==2 & (is.numeric(y) || is.integer(y)) || is.factor(y) & nlevels(y)==2 & 
     (is.numeric(x) || is.integer(x))){
    if(is.factor(x)){
      x <- x
      y <- y
    }
    if(is.factor(y)){
      x <- y
      y <- x
    }
    table <- data.frame(x,y)
    cond1 <- isTRUE(all.equal(table[x==levels(x)[1],],table[is.na(y),]))
    cond2 <- isTRUE(all.equal(table[x==levels(x)[2],],table[is.na(y),]))
    return(cond1 || cond2)
  }
  else{
    if(is.factor(x) & is.factor(y)){
      stop("'x' or 'y' must be numeric.")
    }
    if(!is.factor(x) & !is.factor(y)){
      stop("'x' or 'y' must be a factor.")
    }
    if(is.factor(x) & nlevels(x)!=2){
      stop("'x' must have only 2 levels.")
    }
    if(is.factor(y) & nlevels(y)!=2){
      stop("'y' must have only 2 levels.")
    }
    if(length(x)!=length(y)){
      stop("'x' and 'y' must have the same length.")
    }
  }
}


# Profilsligne et colonne pour deux facteurs ------------------------------

#'
#'@param: x et y des vecteurs. 
#'@return la table de contingence, le profils ligne et les profils colonne.
#'
fprofils <- function(x,y,out=c('t','l','c','a','p')){
  if(!is.atomic(x)){
    stop("'x' must be atomic.")
  }
  if(!is.atomic(y)){
    stop("'y' must be atomic.")
  }
  if(length(x)!=length(y)){
    stop("'x' and 'y' must have the same length length.")
  }
  if(out[1]=='t'){
    table <- table(x,y)
    output <- table
  }
  else if(out[1]=='l'){
    table <- table(x,y)
    table <- rbind(table,colSums(table))
    table <- cbind(table,rowSums(table))
    Ligne <- apply(table,2,function(x){x/table[,ncol(table)]})
    output <- Ligne
  }
  else if(out[1]=='c'){
    table <- table(x,y)
    table <- rbind(table,colSums(table))
    table <- cbind(table,rowSums(table))
    Colonne <- t(apply(table,1,function(x){x/table[nrow(table),]}))
    output <- Colonne
  }
  else if(out[1]=='a'){
    table <- table(x,y)
    table <- rbind(table,colSums(table))
    table <- cbind(table,rowSums(table))
    Ligne <- apply(table,2,function(x){x/table[,ncol(table)]})
    Colonne <- t(apply(table,1,function(x){x/table[nrow(table),]}))
    output <- list(table=table,Ligne=Ligne, Colonne=Colonne)
  }
  else if(out[1]=='p'){
    table <- table(x,y)
    table <- rbind(table,colSums(table))
    table <- cbind(table,rowSums(table))
    Ligne <- apply(table,2,function(x){x/table[,ncol(table)]})
    Colonne <- t(apply(table,1,function(x){x/table[nrow(table),]}))
    output <- list(Ligne=Ligne, Colonne=Colonne)
  }
  return(output)
}


# V-Cramer ----------------------------------------------------------------

#'
#'@param: x et y deux facteurs.
#'@return le v de cramer.
#'
cramer <- function(x,y){
  if(!is.factor(x)){
    stop("'x' must be a factor.")
  }
  if(!is.factor(y)){
    stop("'y' must be a factor.")
  }
  if(length(x)!=length(y)){
    stop("'x' and 'y' must have the same length length.")
  }
  cramer <- sqrt(chisq.test2(table(x,y))$statistic/length(y))
  return(cramer)
}


# Taux de mal classés -----------------------------------------------------

#'
#'@param x et y deux facteurs.
#'@return le taux de mal classé.
#'
tauxmc <- function(x,y){
  if(length(x)!=length(y)){
    stop("'x' and 'y' must have the same length length.")
  }
  if(!is.factor(x)){
    stop("'x' must be a factor.")
  }
  if(!is.factor(y)){
    stop("'y' must be a factor.")
  }
  if(nlevels(x)!=nlevels(x)){
    stop("'x' and 'y' must have the same number of levels.")
  }
  taux <- 1.0-sum(diag(table(x,y)))/length(x)
  cat("Taux de mal classés: ",taux)
}


# Discretization ----------------------------------------------------------

#'
#' This function disretize a continuous variable and add the new discretized variable to the data set.
#' It is possible to give cut points to the function so that it does not compute the 'smbinning' function.
#' @param data: the data in which is the variable to be discretized and the target variable.
#' This function uses the 'smbinning' function.
#' @param x: the variable to be discretized. Argument used in 'smbinning'.
#' @param y: the target variable. Argument used in 'smbinning'.
#' @param p: percentage of records per bin. Default 5% (0.05). This parameter only accepts
#' values greater that 0.00 (0%) and lower than 0.5 (50%). Argument used in 'smbinning'.
#' @param out: the name of the variable to be added to the data set.
#' @param cuts: cut points used to discretize the variable. If missing the cut points will be obtained
#' with the 'smbining' function.
#' @return the data set with the categorized variable.
#' 
fdiscretize <- function(data,x,y,p=0.05,out,cuts=NULL){
  names <- names(data)
  if(is.null(cuts)){
    cuts <- smbinning(df=data,y=deparse(substitute(y)),x=deparse(substitute(x)),p=p)$cuts
  }
  if(class(cuts)!="numeric"){
    stop("'cuts' must be numeric")
  }
  var_discrete <- cut(x,breaks=c(-Inf,cuts,Inf),
                      right=FALSE,labels=0:length(cuts))
  if(any.na(x)){
    levels(var_discrete) <- c(0:length(cuts),"NA")
  }
  data <- cbind(data,var_discrete)
  names(data)[ncol(data)] <- out
  return(data)
}


# Data train and data test ------------------------------------------------

# https://rstudio-pubs-static.s3.amazonaws.com/50943_1fc4d7b7c9924277b8b67b6ce15f393a.html

strat_sample <- function(data, gr_variab, tr_percent, thresh_test = 0, seed) {
  stopifnot(tr_percent > 0 & tr_percent < 1)
  if(require(dplyr) & require(magrittr)) {
    if(!missing(seed)) set.seed(seed)
    names0 <- names(data)
    gr_variab <- which(names0 == gr_variab)
    names(data) <- make.unique(c("n", "tRows", "SET", names0))[-(1:3)]
    gr_variab <- names(data)[gr_variab]        
    data %<>% 
      sample_frac %>% 
      group_by_(gr_variab) %>%
      mutate(n = n(), tRows = round(tr_percent * n))
    with(data, if(any(n - tRows < thresh_test))        
      warning("Zero or too few observations in one or more groups"))
    data %<>%
      mutate(SET = ifelse(row_number() <= tRows, "Train", "Test")) %>%
      select(-n, -tRows) %>%
      ungroup
    names(data) <- make.unique(c(names0, "SET"))
    data
  }
}

extract_set <- function(data, whichSET) {
  stopifnot(is.element(whichSET, c("Train", "Test")))
  if(require(dplyr)) {
    variab <- names(data)[ncol(data)]
    condit <- get(variab, data) == whichSET
    data %>%
      filter_(~ condit) %>%
      select_(paste0("-", variab)) 
  }
} 
