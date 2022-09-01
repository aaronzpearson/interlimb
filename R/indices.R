#' Asymmetry Indices
#'
#' Symmetry or asymmetry percentage as either scalar (always positive) or vector (indicates which leg is favoured).
#'
#' There are 17 symmetry and asymmetry indices included within the package. Dependent on their format, the indices
#' can refer to the limbs as right (\code{r}) vs. left (\code{l}), dominant (\code{dl}) vs. non-dominant (\code{ndl}),
#' strong vs. weak, and high vs. low. The index functions take on either limbs identified as \code{l} and \code{r} or
#' \code{dl} and \code{ndl}. For indices that refer to the limbs as either strong vs. weak or high vs. low, the
#' function can assess which limb value is greater before computing the symmetry or asymmetry value.
#'
#' The asymmetry index functions can be called upon explicitly via \code{interlimb:::}. Otherwise, users can call upon
#' \code{interlimb()} or \code{interlimb.wide()} to return one or all asymmetry index calculations, respectively.
#'
#' @note If users are interested in the algorithm, please see the accompanying publication or call upon the
#' asymmetry index function explicitly.
#'
#' @param r Right limb value of length 1L
#' @param l Left limb value of length 1L
#' @param dl Dominant limb value of length 1L
#' @param ndl Non-dominant limb value of length 1L
#'
#' @return symmetry and asymmetry scores
#'
#' @export
#'
#' @examples
#' # for reproducibility
#' set.seed(1)
#'
#' # fabricate data
#' left.limb = 20
#' right.limb = 25
#'
#' dominant.limb = 25
#' nondominant.limb = 20
#'
#' pdiff.asym <- interlimb:::percent.diff(left.limb, right.limb)
#' lsi1.asym <- interlimb:::limb.sym.index.1(dominant.limb, nondominant.limb)
#' bsi1.asym <- interlimb:::bilat.strength.asym.1(left.limb, right.limb)
#'
#' cat("pdiff: ", pdiff.asym,
#' "\nlsi1: ", lsi1.asym,
#' "\nbsi1: ", bsi1.asym)
asym.indices <- function(r, l, dl, ndl) {

  print("call ?asym.indices to view a list of all asymmetry indices")

}

#' @describeIn asym.indices Limb symmetry index 1
limb.sym.index.1 <- function(dl, ndl) {

  asym = (ndl/dl) * 100
  return(asym)

}

#' @describeIn asym.indices Limb symmetry index 2
limb.sym.index.2 <- function(dl, ndl) {

  asym = (1 - ndl/dl) * 100
  return(asym)

}

#' @describeIn asym.indices Limb symmetry index 3
limb.sym.index.3 <- function(l, r) {

  asym = ((r - l)/ ((r + l)/ 2)) * 100
  return(asym)

}

#' @describeIn asym.indices Limb symmetry index 4
limb.sym.index.4 <- function(dl, ndl) {

  asym = (dl/ndl) * 100
  return(asym)

}

#' @describeIn asym.indices Bilateral strength asymmetry 1
bilat.strength.asym.1 <- function(l, r) {

  strong = ifelse(r > l, r, l)
  weak = ifelse(r <= l, r, l)

  asym = ((strong - weak)/ strong) * 100
  return(asym)

}

#' @describeIn asym.indices Bilateral strength asymmetry 2
bilat.strength.asym.2 <- function(l, r) {

  strong = ifelse(r > l, r, l)
  weak = ifelse(r <= l, r, l)

  asym = ((weak - strong)/ strong) * 100
  return(asym)

}

#' @describeIn asym.indices Bilateral asymmetry index 1
bilat.asym.index.1 <- function(dl, ndl) {

  asym = ((dl - ndl)/ (dl + ndl)) * 100
  return(asym)

}

#' @describeIn asym.indices Bilateral asymmetry index 2
bilat.asym.index.2 <- function(dl, ndl) {

  asym = (2 * (dl - ndl)/ (dl + ndl)) * 100
  return(asym)

}

#' @describeIn asym.indices Bilateral strength asymmetry 1
asym.index.1 <- function(dl, ndl) {

  asym = ((dl - ndl)/ ((dl + ndl)/ 2)) * 100
  return(asym)

}

#' @describeIn asym.indices Symmetry index
sym.index <- function(l, r) {

  high = ifelse(r > l, r, l)
  low = ifelse(r <= l, r, l)

  asym = ((high - low)/ (high + low)) * 100
  return(asym)

}

#' @describeIn asym.indices Symmetry index, vectorized
sym.index.vect <- function(l, r) {

  asym = ((l - r)/ (l + r)) * 100
  return(asym)

}

#' @describeIn asym.indices Symmetry angle
sym.angle <- function(l, r) {

  asym = ((pi/4 - atan(l/ r))/ (pi/ 2)) * 100
  return(asym)

}

#' @describeIn asym.indices Percent difference
percent.diff <- function(l, r) {

  x = ifelse(r > l, r, l)
  y = ifelse(r <= l, r, l)

  asym = 100 + (((100/ x) * y) * -1)
  return(asym)
}

#' @describeIn asym.indices Strength asymmetry
strength.asym <- function(dl, ndl) {

  asym = 100 - ((ndl/dl) * 100)
  return(asym)

}

#' @describeIn asym.indices Asymmetry index 2
asym.index.2 <- function(l, r) {

  strong = ifelse(r > l, r, l)
  weak = ifelse(r <= l, r, l)

  asym = ((100/strong) * weak * -1) + 100
  return(asym)

}

#' @describeIn asym.indices Between limb imbalance
btwn.limb.imbalance <- function(l, r) {

  asym = log(l/r) * 100
  return(asym)

}
