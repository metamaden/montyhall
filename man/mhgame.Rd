% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhall.R
\name{mhgame}
\alias{mhgame}
\title{Monty Hall game simulation}
\usage{
mhgame(
  doorseq,
  ndec1 = 1,
  ndec2 = 1,
  nprizes = 1,
  nrevealdif = 1,
  prize.index = NULL,
  selectdec1 = "random",
  doorswitch = 1,
  montyselect = "random",
  verbose.results = FALSE
)
}
\arguments{
\item{doorseq}{Vector of sequential door indices for the game.}

\item{ndec1}{Door index to *always* choose in decision 1.}

\item{ndec2}{Door index to *always* choose in decision 2.}

\item{nprizes}{Total number of prizes for game simulations.}

\item{nrevealdif}{Number of doors Monty does not reveal between player decisions 1 and 2.}

\item{prize.index}{Door index for prize location in each game.}

\item{selectdec1}{Either "random" or a door index to choose for decision 1.}

\item{doorswitch}{Some number between 0-100 (percent chance to switch). Defaults to 1 (100 percent).}

\item{montyselect}{Indices of doors Monty reveals between decisions 1 and 2.}

\item{verbose.results}{Whether to return iteration/game details alongside results.}
}
\value{
A vector of game results, with simulation details if verbose.results = TRUE.
}
\description{
Simulate a single game of the Monty Hall Problem
}
