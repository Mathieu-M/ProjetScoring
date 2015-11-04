

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
