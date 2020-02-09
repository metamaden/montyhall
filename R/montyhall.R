#!/usr/bin/env R

# Run and visualize Monty Hall problem simulations.

# The simulation function

#' Run simulations of the Monty Hall problem
#'
#' Run simulations of the Monty Hall problem. Default settings are canonical/classic game parameters, with arguments to change various parameters.
#' @param niter Number of simulation iterations or games.
#' @param seed Seed integer to set in `set.seed()`.
#' @param ndoors Total quantity of doors for game simulations.
#' @param nprize Total number of prizes for game simulations.
#' @param ndec1 Door index to *always* choose in decision 1.
#' @param ndec2 Door index to *always* choose in decision 2.
#' @param nrevealdif Number of doors Monty does not reveal between player decisions 1 and 2.
#' @param prize.index Door index for prize location in each game.
#' @param selectdec1 Either "random" or a door index to choose for decision 1.
#' @param doorswitch Whether the player switches their door choice. Either "always" (100%) or some number between 0-1.
#' @param montyselect Indices of doors Monty reveals between decisions 1 and 2.
#' @param verbose.results Whether to return iteration/game details alongside results.
#' @return A vector of game results, with simulation details if verbose.results = TRUE.
#' @export
mhsim <- function(niter = 100, seed = 1, ndoors = 3, 
                  nprize = 1, ndec1 = 1, ndec2 = 1,
                  nrevealdif = 1, prize.index = NULL,
                  selectdec1 = "random", doorswitch = "always", 
                  montyselect = "random", verbose.results = FALSE){
  set.seed(seed)
  lr <- c() # new return object
  if(verbose.results){
    lv <- list()
  }
  doorseq <- seq(1, ndoors, 1)
  for(i in 1:niter){
    # pick prize
    if(is.null(prize.index)){
      which.prize <- sample(doorseq, nprize)
    } else{
      if(prize.index %in% doorseq){
        which.prize <- prize.index
      } else{
        stop("Invalid prize index specified.")
      }
    }
    # run decision 1
    if(selectdec1 == "random"){
      dec1select <- sample(doorseq, ndec1)
    } else{
      if(is.numeric(selectdec1) & selectdec1 %in% doorseq){
        dec1select <- selectdec1
      } else{
        stop("Invalid decision 1 selectoin specified.")
      }
    }
    # run montyselect
    doorremain1 <- doorseq[!doorseq == dec1select]
    nr <- length(doorremain1) - nrevealdif
    if(nr < 0 | nr > length(doorremain1) - 1){
      stop("Too many revealed doors. Decrease nrevealdif.")
    }
    if(montyselect == "random"){
      mdooroptions <- doorremain1[!doorremain1 %in% which.prize]
      if(length(mdooroptions) < 2){
        mselect <- mdooroptions
      } else{
        mselect <- sample(mdooroptions, nr)
      }
    }
    # run decision 2
    doorexclude2 <- c(mselect, dec1select)
    doorremain2 <- doorseq[!doorseq %in% doorexclude2]
    if(doorswitch == "always"){
      if(length(doorremain2) > 1){
        dec2select <- sample(doorremain2, ndec2)
      } else{
        dec2select <- doorremain2
      }
    }
    # evaluate results
    lr <- c(lr, ifelse(dec2select == which.prize, "win", "loss"))
    if(verbose.results){
      lv[[length(lv) + 1]] <- list("which.prize" = which.prize,
                                   "dec1select" = dec1select,
                                   "nr" = nr,
                                   "montyselect" = mselect,
                                   "dec2select" = dec2select)
    }
  }
  
  if(verbose.results){
    return(list("results" = lr, "details" = lv))
  } else{
    return(lr) # return iterations results
  }
}

# Visualization utilities

#' Get results data formatted for ggplot2 functions.
#'
#' Takes a list of win fractions and results the data in ggplot2 function-ready format.
#' @param ld List of win fractions. 
#' @return Formatted dataset.
#' @export
getggdat <- function(ld){
  dfp <- matrix(nrow = 0, ncol = 2)
  for(i in 1:length(lnd)){
    nrep <- length(lnd[[i]])
    ndi <- as.numeric(names(lnd)[i])
    ndv <- rep(as.numeric(ndi), nrep)
    dati <- lnd[[i]]
    lmi <- matrix(c(dati, ndv), ncol = 2, byrow = FALSE)
    dfp <- rbind(dfp, lmi)
  }
  dfp <- as.data.frame(dfp, stringsAsFactors = F)
  colnames(dfp) <- c("fract.win", "ndoors")
  ulvl <- unique(dfp[,2])
  olvl <- ulvl[order(unique(dfp[,2]))]
  dfp[,2] <- factor(dfp[,2], levels = olvl)
  return(dfp)
}

#' Get results data formatted for ggplot2 line plots.
#'
#' Takes a list of win fractions and results the data in ggplot2 line plot function-ready format.
#' @param ld List of win fractions. 
#' @return Formatted dataset.
#' @export
getlinedat <- function(ld){
  dfp <- matrix(nrow = 0, ncol = 4)
  for(i in 1:length(lnd)){
    dati <- lnd[[i]]
    ndi <- as.numeric(names(lnd)[i])
    mindat <- min(dati)
    maxdat <- max(dati)
    meandat <- mean(dati)
    lmi <- matrix(c(meandat, maxdat, mindat, ndi), nrow = 1)
    dfp <- rbind(dfp, lmi)
  }
  dfp <- as.data.frame(dfp, stringsAsFactors = F)
  colnames(dfp) <- c("fract.win", "max", "min", "ndoors")
  return(dfp)
}

#' Generate a ggplot2 line plot.
#'
#' Generates ggplot2 line plot object from the list of win fraction results.
#' @param ld List of win fractions. 
#' @return Line plot object.
#' @export
getlineplot <- function(ld){
  dfp <- getlinedat(ld)
  plp <- ggplot(dfp, aes(ndoors)) +
    geom_line(aes(y = fract.win), colour = "blue") + 
    geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
    theme_bw()
  return(plp)
}

#' Generate a composite of 3 ggplot2 plots.
#'
#' Generates a composite of 3 ggplot2 plots from list of win fraction results. 
#' @param ld List of win fractions. 
#' @param topmain Top title of composite plot.
#' @return Composite image of 3 ggplot2 plots.
#' @export 
getprettyplots <- function(ld, topmain = "Top Title"){
  # fromat results data
  dfp <- getggdat(ld)
  
  # violin plots
  p1 <- ggplot(dfp, aes(x = ndoors, y = fract.win, fill = ndoors)) +
    geom_violin() + theme_bw() + theme(legend.position = "none") +
    ggtitle("Violin plots")
  
  # ridge plots
  p2 <- ggplot(dfp, aes(x = fract.win, y = ndoors, fill = ndoors)) +
    geom_density_ridges() + theme_bw() + theme(legend.position = "none") +
    ggtitle("Ridge plots")
  
  # line plot
  p3 <- getlineplot(ld)
  
  # make the composite plot
  grid.arrange(p1, p2, p3, ncol = 3, top = topmain)
}