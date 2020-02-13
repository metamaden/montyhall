#!/usr/bin/env R

# Run and visualize Monty Hall problem simulations.

# Game iterations and simulation run functions

#' Monty Hall game simulation
#' 
#' Simulate a single game of the Monty Hall Problem
#' @param doorseq Vector of sequential door indices for the game.
#' @param nprizes Total number of prizes for game simulations.
#' @param ndec1 Door index to *always* choose in decision 1.
#' @param ndec2 Door index to *always* choose in decision 2.
#' @param nrevealdif Number of doors Monty does not reveal between player decisions 1 and 2.
#' @param prize.index Door index for prize location in each game.
#' @param selectdec1 Either "random" or a door index to choose for decision 1.
#' @param doorswitch Some number between 0-100 (percent chance to switch). Defaults to 1 (100%).
#' @param montyselect Indices of doors Monty reveals between decisions 1 and 2.
#' @param verbose.results Whether to return iteration/game details alongside results.
#' @return A vector of game results, with simulation details if verbose.results = TRUE.
#' @export
mhgame <- function(doorseq, ndec1 = 1, ndec2 = 1, nprizes = 1,
                   nrevealdif = 1, prize.index = NULL,
                   selectdec1 = "random", doorswitch = 1, 
                   montyselect = "random", 
                   verbose.results = FALSE){
  # pick prize door index
  which.prize <- sample(doorseq, nprizes)
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
  doorremain1 <- doorseq[!doorseq == dec1select] # exclude player first selection
  nr <- length(doorremain1) - nrevealdif # calculate the reveal difference
  # validate reveal difference value
  if(nr < 0 | nr > length(doorremain1) - 1){
    stop("Too many doors specified for Monty to reveal. Increase `nrevealdif`.")
  }
  if(montyselect == "random"){
    # if more than 1 prize, allow monty to reveal n - 1 prizes
    if(length(which.prize) > 1){
      mdooroptions <- doorremain1
    } else{
      mdooroptions <- doorremain1[!doorremain1 %in% which.prize]
    }
    if(length(mdooroptions) < 2){
      mselect <- mdooroptions
    } else{
      mselect <- sample(mdooroptions, nr)
    }
  }
  # run decision 2
  # exclude monty's doors and decision 1 doors from switch options
  doorremain2 <- doorseq[!doorseq %in% c(mselect, dec1select)]
  # parse switch likelihood
  if(is.numeric(doorswitch) & doorswitch >= 0 & doorswitch <= 1){
    ssvar <- ifelse(doorswitch == 1, "switch", 
                    sample(c(rep("switch", 100*doorswitch), 
                             rep("stay", 100 - 100*doorswitch)), 1))
  } else{
    stop("Invalid doorswitch value.")
  }
  # evalue switch decision
  if(ssvar == "switch"){
    if(length(doorremain2) > 1){
      dec2select <- sample(doorremain2, ndec2)
    } else{
      dec2select <- doorremain2
    }
  } else{
    dec2select <- dec1select
  }
  # evaluate and return results
  uiter <- ifelse(dec2select %in% c(which.prize), "win", "loss")
  if(verbose.results){
    uiter <- list("outcome" = uiter,
                  "which.prize" = which.prize,
                  "dec1select" = dec1select,
                  "nr" = nr,
                  "montyselect" = mselect,
                  "dec2select" = dec2select)
  }
  return(uiter)
}

#' Complete a simulation run of the Monty Hall Problem
#'
#' Run simulations of the Monty Hall problem. Default settings are canonical/classic game parameters, with arguments to change various parameters.
#' @param niter Number of simulation iterations or games.
#' @param seed Seed integer to set in `set.seed()`.
#' @param ndoors Total quantity of doors for game simulations.
#' @param nprizes Total number of prizes for game simulations.
#' @param ndec1 Door index to *always* choose in decision 1.
#' @param ndec2 Door index to *always* choose in decision 2.
#' @param nrevealdif Number of doors Monty does not reveal between player decisions 1 and 2.
#' @param prize.index Door index for prize location in each game.
#' @param selectdec1 Either "random" or a door index to choose for decision 1.
#' @param doorswitch Some number between 0-100 (percent chance to switch). Defaults to 1 (100%).
#' @param montyselect Indices of doors Monty reveals between decisions 1 and 2.
#' @param verbose.results Whether to return iteration/game details alongside results.
#' @return A vector of game results, with simulation details if verbose.results = TRUE.
#' @export
mhsim <- function(niter = 100, seed = 1, ndoors = 3, 
                  ndec1 = 1, ndec2 = 1, nprizes = 1,
                  nrevealdif = 1, prize.index = NULL,
                  selectdec1 = "random", doorswitch = 1, 
                  montyselect = "random", verbose.results = FALSE){
  doorseq <- seq(1, ndoors, 1)
  set.seed(seed)
  lr <- lapply(seq(niter, 1), function(x){
    mhgame(doorseq, ndec1 = ndec1, ndec2 = ndec2,
           nprizes = nprizes, nrevealdif = nrevealdif,
           prize.index = prize.index, selectdec1 = selectdec1,
           doorswitch = doorswitch, montyselect = montyselect,
           verbose.results = verbose.results)
  })
  if(!verbose.results){
    lr <- unlist(lr)
  }
  return(lr)
}

# Win fractions utility

#' Compute game win frequencies across game simulations of the Monty Hall Problem.
#'
#' Wrapper to compute win frequencies across simulations.
#' @param nsimulations Number of simulations to run.
#' @param niterations Number of iterations/games per simulation run.
#' @param ndoors Quantity of doors per game.
#' @param prize.index Door index for prize in each game.
#' @param doorswitch Frequency (0 - 100%) with which player switches doors when given the option.
#' @return Vector of win fractions across simulations run, reflecting the numnber of iterations "won" over the total for that simulation.
#' @export
getfw = function(nsimulations = 5, niterations = 2, ndoors = 3, 
                 prize.index = NULL, doorswitch = 1,
                 verbose.results = FALSE){
  fw <- c() # vector of win fractions across simulations
  vr <- list() # results for verbose results option
  for(s in 1:nsimulations){
    lrs <- mhsim(seed = s, niter = niterations, ndoors = ndoors,
                 prize.index = prize.index, doorswitch = doorswitch,
                 verbose.results = verbose.results)
    if(verbose.results){
      vr[[paste0(s)]] <- lrs
    } else{
      fw <- c(fw, length(which(lrs == "win"))/length(lrs))
    }
  }
  if(verbose.results){
    return(vr)
  } else{
    return(fw)
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
#' @param ribbontype How to calculate gray ribbon ovelay. Either "sd" for standard deviation, or "minmax" for min and max of run.
#' @param xtitle Name of variable to be in x-axis of line plot.
#' @return Formatted dataset.
#' @export
getlinedat <- function(ld, ribbontype = c("sd", "minmax"), xtitle = "ndoors"){
  dfp <- matrix(nrow = 0, ncol = 4)
  for(i in 1:length(ld)){
    dati <- ld[[i]]
    ndi <- as.numeric(names(ld)[i])
    meandat <- mean(dati)
    # parse ribbon overlay
    if(ribbontype == "sd"){
      mindat <- meandat - sd(dati)
      maxdat <- meandat + sd(dati)
    }
    if(ribbontype == "minmax"){
      mindat <- min(dati)
      maxdat <- max(dati)
    }
    lmi <- matrix(c(meandat, maxdat, mindat, ndi), nrow = 1)
    dfp <- rbind(dfp, lmi)
  }
  dfp <- as.data.frame(dfp, stringsAsFactors = F)
  colnames(dfp) <- c("fract.win", "max", "min", xtitle)
  return(dfp)
}

#' Generate a ggplot2 line plot.
#'
#' Generates ggplot2 line plot object from the list of win fraction results.
#' @param ld List of win fractions. 
#' @param ptitle Plot main title from `ggtitle()`.
#' @param ribbontype Metric to calculate ribbon overlay (either "sd" or "minmax").
#' @param xlim Vector of 2 x-axis coordinates (min and max), or NULL.
#' @param ylim Vector of 2 y-axis coordinates (min and max) or NULL.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @return Line plot object.
#' @export
getlineplot <- function(ld, ptitle = "Plot title", ribbontype = c("sd", "minmax"),
                        xlim = NULL, ylim = NULL, xlab = "ndoors",
                        ylab = "fract.win"){
  require(ggplot2)
  dfp <- getlinedat(ld, ribbontype = ribbontype)
  if(is.null(xlim) & is.null(ylim)){
    plp <- ggplot(dfp, aes(ndoors)) +
      geom_line(aes(y = fract.win), colour = "blue") + 
      geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
      theme_bw() + ggtitle(ptitle) + xlab(xlab) + ylab(ylab)
  }
  if(!is.null(xlim) & !is.null(ylim)){
    plp <- ggplot(dfp, aes(ndoors)) +
      geom_line(aes(y = fract.win), colour = "blue") + 
      geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
      theme_bw() + ggtitle(ptitle) +
      ylim(ylim[1], ylim[2]) +
      xlim(xlim[1], xlim[2]) + xlab(xlab) + ylab(ylab)
  }
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
  require(ggplot2)
  require(ggridges)
  require(gridExtra)
  
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
  p3 <- getlineplot(ld, ptitle = "Line plot")
  
  # make the composite plot
  grid.arrange(p1, p2, p3, ncol = 3, top = topmain)
}

#' Generate a gif composite of violin and line plot animations
#'
#' Generates a gif of composite plot animations for violin and line plots. Currently supports results from the `ndoors` experiment.
#' @param ld List of win fractions.
#' @param gifname Name of gif file to store.
#' @param plottype Type of plots for gif animation (either "composite_ndoors" or "lineplots_winfractions").
#' @return Saves a gif file of the composite plots.
#' @export 
getprettygif <- function(ld, gifname = "mh_ndoors.gif",
                         plottype = c("composite_ndoors", 
                                     "lineplots_winfractions")){
  require(gganimate)
  require(ggplot2)
  require(magick)
  
  if(plottype == "composite_ndoors"){
    # get data for plot animations
    dfp2 <- getlinedat(ld, ribbontype = "sd")
    dfp1 <- getggdat(ld)
    
    # get gif objects from plot objects
    g1 <- ggplot(dfp1, aes("", fract.win, fill = ndoors)) + 
      geom_violin() + theme_bw() + 
      xlab("") + ylab("Win Fraction") +
      theme(legend.position = "none") +
      ylim(0.5, 1) +
      transition_states(
        ndoors,
        transition_length = 1,
        state_length = 1
      ) +
      labs(title = "Num. Doors = {closest_state}") +
      enter_fade() + 
      ease_aes('sine-in-out')
    
    g2 <- ggplot(dfp2, aes(ndoors)) +
      geom_point(aes(y = fract.win, color = "Red")) +
      geom_line(aes(y = fract.win), colour = "blue") + 
      xlab("Number of Doors") + ylab("") + 
      ggtitle("") + ylim(0, 1) +
      geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
      theme_bw() +
      theme(legend.position = "none") +
      transition_reveal(ndoors)
    
    gif1 <- animate(g1, width = 200, height = 200, fps = 20)
    gif2 <- animate(g2, width = 200, height = 200, fps = 20)
    # write and read new gif image files
    fn1 <- "gif1.gif"; fn2 <- "gif2.gif"
    image_write(gif1, fn1); image_write(gif2, fn2)
    img1 <- image_read(fn1); img2 <- image_read(fn2)
    # clean up files
    file.remove(fn1); file.remove(fn2)
    new_gif <- image_append(c(img1[1], img2[1]))
    for(i in 2:100){
      combined <- image_append(c(img1[i], img2[i]))
      new_gif <- c(new_gif, combined)
    }
    # store the composite gif
    image_write(new_gif, gifname)
  }
  if(plottype == "lineplots_winfractions"){
    dfp <- getlinedat(ld[[1]], ribbontype = "sd")
    dfp$switchfreq <- names(ld)[1]
    for(i in 2:length(ld)){
      dfpi <- getlinedat(ld[[i]], ribbontype = "sd")
      dfpi$switchfreq <- names(ld)[i]
      dfp <- rbind(dfp, dfpi)
    }
    dfp$switchfreq <- as.numeric(dfp$switchfreq)
    # get plot animation data
    plp <- ggplot(dfp, aes(ndoors)) +
      geom_line(aes(y = fract.win), colour = "blue") +
      geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
      theme_bw() + xlab("Number of Doors") + ylab("Win Fraction") +
      transition_states(switchfreq) +
      labs(title = "Switch Frequency = {closest_state}")
    gif1 <- animate(plp, width = 300, height = 300, fps = 10)
    # write new plot gif
    image_write(gif1, gifname)
  }
  return("Successfully completed.")
}