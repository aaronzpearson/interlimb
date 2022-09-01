#' Track Asymmetries
#'
#' \code{track.asym} is an intermediary step within the \code{interlimb::interlimb()} function. It provides the following
#' variables before retuning the data.frame to the user: \code{asym.cv}, \code{asym.change}, and \code{rel.change}.
#'
#' Three variables are calculated and bound to the `long` formatted data set: \code{asym.cv}, \code{asym.change}, and \code{rel.change}.
#'
#' \code{asym.cv}: calculated the asymmetry coefficient of variance from week to week
#'
#' \code{asym.change}: assesses the difference in asymmetry values from week to week
#'
#' \code{rel.change}: assesses the difference in asymmetry values against the original assessment
#'
#' Users must format data via `asym.long()` to properly feed into \code{track.asym}.
#'
#' @param data.set Long format data set
#' @param na.fill If \code{TRUE}, assumes that missing asymmetry values remain constant from the previous week
#'
#' @return A dataframe that contains raw asymmetry values for a given asymmetry index AND the percent change from assessment to assessment
#' @export
#'
#' @examples
#' # for reproducibility
#' set.seed(1)
#'
#' # fabricate data
#' asymmetry.tests <- data.frame(
#'     test.date = rep(LETTERS[1:5], each = 3),
#'     trial = rep(1:3, times = 5),
#'     parameter = "example",
#'     left.limb = rnorm(15, mean = 100, 1),
#'     right.limb = rnorm(15, mean = 100, 1)
#'     )
#'
#' # must be in long format
#' # must only contain 1 asymmetry index
#'
#' # calculate asymmetries for all tests
#' # assume dominant limb is the right limb
#'
#' # wide data set
#' asymmetry.wide <- interlimb.wide(asymmetry.tests, test.dates = "test.date", dominant.limb = "r", parameter = "example", right.limb = "right.limb", left.limb = "left.limb")
#' # long data set
#' asymmetry.long <- interlimb.long(asymmetry.wide)
#'
#' # filter for 1 asymmetry index
#' asymmetry.long <- subset(asymmetry.long, asymmetry.index == "percent.diff")
#'
#' # track changes of trial 1 per week for simplicity
#' asymmetry.long <- subset(asymmetry.long, trial == "1")
#'
#' # track asymmetry
#' asymmetry.track <- track.asym(asymmetry.long, asym.algo = "percent.diff")
#' asymmetry.track[, -2]
track.asym <- function(data.set,
                       asym.algo,
                       na.fill = TRUE) {

  df <- subset(data.set, data.set$asymmetry.index == asym.algo)

  if(na.fill == TRUE) { df$asymmetry <- fill.na(df$asymmetry)
  } else {
    df$asymmetry <- df$asymmetry
  }

  df$asym.cv <- rolling.cv(df$asymmetry)
  df$asym.change <- c(0, diff(df$asymmetry))
  df$rel.change <- rel.change(df$asymmetry)

  row.names(df) <- 1:nrow(df)

  return(df)

}

# helper function
# not exported
fill.na <- function(x) {

  # adapted from question & answer on stackexchange
  which.na <- c(which(!is.na(x)), length(x) + 1)
  values <- na.omit(x)

  if (which.na[1] != 1) {
    which.na <- c(1, which.na)
    values <- c(values[1], values)
  }

  diffs <- diff(which.na)

  return(rep(values, times = diffs))

}
