#' Unabridged interlimb Data Frames
#'
#' \code{interlimb.wide()} and \code{interlimb.long()} return symmetry and asymmetry calculations for every asymmetry index that is
#' included in the package.
#'
#' Since these functions are helper functions, the inputted data *must* be properly formatted. Therefore, the \code{data.set} argument
#' requires that the data.frame contain the testing date, parameter tested, and left and right limb assessments. The variables names must be:
#' \code{"test.date"}, \code{"parameter"}, \code{"left.limb"}, and \code{"right.limb"}. To set the variables names, use \code{colnames()}.
#'
#' The `wide` and `long` functions are exported to help practitioners better decide which asymmetry index they want to choose for
#' the given testing modality. \code{interlimb.wide()} also provides the opportunity for researchers to explore relationships between asymmetry
#' indices.
#'
#' If users are interested in exploring the best attempt versus mean of all attempts, they are encouraged to \code{aggregate()} the data by date
#' retain the best or all values, and calculate the mean. Examples are availale in either the \code{interlimb.R} source code or the
#' accompanying publication.
#'
#' \code{interlimb.wide()} returns a data.frame where every asymmetry index is calculated as its own variable.
#'
#' \code{interlimb.long()} returns a data.frame that has `combined` columns to create \code{asymmetry.index} and \code{asymmetry} variables.
#'
#' @param data.set A properly formatted data set (see details)
#' @param dominant.limb Dominant limb set as \code{"r"} or \code{"l"}
#'
#' @return Unabridged wide or long data.frame containing calculations for all asymmetry indices
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
#'     left = rnorm(15, mean = 20, 1),
#'     right = rnorm(15, mean = 20, 1)
#'     )
#'
#' # calculate asymmetries for all tests
#' # assume dominant limb is the right limb
#'
#' # wide data set
#' asymmetry.wide <- interlimb.wide(
#'     test.data = asymmetry.tests,
#'     test.dates = "test.date",
#'     dominant.limb = "r",
#'      parameter = "jump height",
#'      right.limb = "right",
#'      left.limb = "left")
#' head(asymmetry.wide)
#'
#' # long data set
#' asymmetry.long <- interlimb.long(asymmetry.wide)
#' head(asymmetry.long)
#'
#' # filter for asymmetry index
#' # subset(asymmetry.long, asymmetry.index == "percent.diff")
interlimb.wide <- function(test.data,
                           test.dates = "date",
                           dominant.limb = "r",
                           parameter,
                           right.limb = "right",
                           left.limb = "left") {

  if(missing(parameter)) {parameter = NA} else {parameter = parameter}
  temp <- data.frame(test.date = test.data[, test.dates],
                     parameter = parameter,
                     right.limb = test.data[, right.limb],
                     left.limb = test.data[, left.limb])

  # rename variables for consistency
  colnames(temp) <- c("test.date", "parameter", "right.limb", "left.limb")

  temp$dominant.limb = dominant.limb

  r = temp$right.limb
  l = temp$left.limb

  dl = ifelse(temp$dominant.limb == "r", r, l)
  ndl = ifelse(temp$dominant.limb == "r", l, r)

  temp$stronger.limb = ifelse(r > l, "r", "l")

  temp$asym.index.1 = asym.index.1(dl, ndl)
  temp$asym.index.2 = asym.index.2(dl, ndl)

  temp$bilat.asym.index.1 = bilat.asym.index.1(dl, ndl)
  temp$bilat.asym.index.2 = bilat.asym.index.2(dl, ndl)

  temp$bilat.strength.asym.1 = bilat.strength.asym.1(l, r)
  temp$bilat.strength.asym.2 = bilat.strength.asym.2(l, r)

  temp$btwn.limb.imbalance <- btwn.limb.imbalance(l, r)

  temp$limb.sym.index.1 = limb.sym.index.1(dl, ndl)
  temp$limb.sym.index.2 = limb.sym.index.2(dl, ndl)
  temp$limb.sym.index.3 = limb.sym.index.3(l, r)
  temp$limb.sym.index.4 = limb.sym.index.4(dl, ndl)

  temp$percent.diff = percent.diff(l, r)

  temp$strength.asym <- strength.asym(dl, ndl)

  temp$sym.angle = sym.angle(l, r)

  temp$sym.index = sym.index(l, r)

  temp$sym.index.vect = sym.index.vect(l, r)

  return(temp)

}

#' @describeIn interlimb.wide interlimb.long
#' @export
interlimb.long <- function(data.set) {

  df <- reshape2::melt(data.set, c("test.date",
                                   "parameter",
                                   "left.limb",
                                   "right.limb",
                                   "dominant.limb",
                                   "stronger.limb"),
                       value.name = "asymmetry",
                       variable.name = "asymmetry.index"
  )

  df <- subset(df, asymmetry.index %in% c(
    "asym.index.1",
    "asym.index.2",
    "bilat.asym.index.1",
    "bilat.asym.index.2",
    "bilat.strength.asym.1",
    "bilat.strength.asym.2",
    "btwn.limb.imbalance",
    "limb.sym.index.1",
    "limb.sym.index.2",
    "limb.sym.index.3",
    "limb.sym.index.4",
    "percent.diff",
    "strength.asym",
    "sym.angle",
    "sym.index",
    "sym.index.vect"
  ))

  # can be used for SEM when assessing multiple athletes
  df$trial <- sequence(rle(as.vector(df$test.date))$lengths)

  df$asymmetry <- round(df$asymmetry, 2)

  df <- df[, c(1, 9, 2:8)]

  return(df)


}

