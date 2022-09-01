#' interlimb
#'
#' This is the primary function in the \code{interlimb} package. This function returns a data set that contains the
#' best or mean of the best n trials, asymmetry values, and reliability values grouped by date.
#'
#' \code{interlimb::interlimb} is built with the understanding that users might be unfamiliar with R and tries to lower
#' the barrier of entry to maximize the user experience. The function takes on all necessary information and, in one
#' step, returns all symmetry and asymmetry information.
#'
#' Users should note that some arguments must be input whereas others can be inferred by the function. For
#' example, variable names must be explicitly named and \code{n.limbs} stated. Conversely, if \code{n.limbs}
#' is not clarified, asym.algo will select the most appropriate calculations based on testing modality.
#'
#' Since some asymmetry indices do not provide directionality (an indication of which limb is favored), the data.frame
#' that is returned to the user contains the variable \code{favored.limb}. In doing so, the user is always
#' provided limb symmetry or asymmetry directionality.
#'
#'
#'
#' @note Please see the accompanying publication for extensive descriptions and examples concerning \code{interlimb} and
#' all of its functions.
#'
#' @param test.data Original data set
#' @param test.dates Date variable name in quotation marks ("")
#' @param dominant.limb Individual's dominant limb as either \code{"r"} or \code{"l"}. Default is set to \code{"r"}
#' @param parameter The  parameter tested - does not need to be explicitly stated
#' @param right.limb Right limb variable name in quotation marks (""). Default set to \code{"right"}
#' @param left.limb Left limb variable name in quotation marks (""). Default set to \code{"left"}
#' @param n.limbs Number of limbs tested simultaneously as either \code{1} or \code{2}. Default set to \code{2}
#' @param best.n Takes on either an integer (i.e. 1, 2, 3) or \code{"all"} to assess either the best trial or mean of 2+ trials, respectively, per date.
#' If best.n is greater than the number of trials per date, all trials for the given date are selected. Default is set to \code{"all"}
#' @param asym.algo Asymmetry calculation/ algorithm to use, with default set to \code{"auto"}
#' @param na.fill Fill missing asymmetry values. Default set to \code{TRUE}
#'
#' @return A data.frame with the following variables: test.date, parameter, left.limb, right.limb,
#' dominant.limb, asymmetry.index, asymmetry, sd, cv, asym.change, rel.change, and rolling.cv
#'
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
#' # make the following assumptions:
#' # test was performed bilaterally (n.limbs = 2)
#' # dominant limb is right (dominant.limb = "r")
#' # the individual was tested during the countermovement jump
#'
#' # the best trial is maintained for further analyses (best.n = 1)
#' bilateral.asym.max <- interlimb::interlimb(
#'     test.data = asymmetry.tests,
#'     test.dates = "test.date", # column name for the test dates
#'     dominant.limb = "r", # state dominant limb
#'     parameter = "jump height", # declare what was tested, can be set to NA
#'     right.limb = "right", # column name for the right limb
#'     left.limb = "left", # column name for the left limb
#'     n.limbs = 2, # was the test unilateral (1) or bilateral (2)
#'     best.n = 1, # take the best of the three trials per date
#'     asym.algo = "auto", # let the package decide which asymmetry index to use
#'     na.fill = TRUE # if data are missing, assumes that asymmetries remain constant week to week
#'     )
#'
#' # test was performed unilaterally (n.limbs = 1)
#' # dominant limb is left (dominant.limb = "l")
#' # the individual was tested during the countermovement jump
#'
#' # take all trials per day (n.best = "all)
#' unilateral.asym.mean <- interlimb::interlimb(
#'     test.data = asymmetry.tests,
#'     test.dates = "test.date",
#'     dominant.limb = "l",
#'     parameter = "jump height",
#'     right.limb = "right",
#'     left.limb = "left",
#'     n.limbs = 1,
#'     best.n = "all", # take the best of the three trials per date
#'     asym.algo = "auto", # let the package decide which asymmetry index to use
#'     na.fill = TRUE # if data are missing, assumes that asymmetries remain constant week to week
#'     )
#'
#' bilateral.asym.max
#' unilateral.asym.mean
interlimb <- function(test.data,
                      test.dates = "date",
                      dominant.limb = "r",
                      parameter,
                      right.limb = "right",
                      left.limb = "left",
                      n.limbs = 2,
                      best.n = c(1, 3, "all"),
                      asym.algo = "auto",
                      na.fill = TRUE) {


  # set asymmetry index
  if(asym.algo != "auto") {asym.algo = asym.algo}

  if(length(asym.algo) > 1) {asym.algo = "bilat.asym.index.1"}

  if(asym.algo == "auto" &
     n.limbs == 1) {
    print("Index was automatically set to: Percent Difference")
    asym.algo = "percent.diff"
    }
  if(asym.algo == "auto" &
     n.limbs == 2) {
    print("Index was automatically set to: Bilateral Asymmetry Index 1")
    asym.algo = "bilat.asym.index.1"
  }

  if(missing(parameter)) {parameter = "NA"}

  # double check that dominant.limb is lower case
  dominant.limb <- tolower(dominant.limb)

  # take all trials per day
  best.n <- ifelse(best.n == "all", Inf, best.n)

  # build data.frame
  temp <- data.frame(test.date = test.data[, test.dates],
                     parameter = ifelse(parameter == "NA",
                                        rep(NA, nrow(test.data)),
                                        parameter),
                     right.limb = test.data[, right.limb],
                     left.limb = test.data[, left.limb])
  # rename variables for consistency
  colnames(temp) <- c("test.date", "parameter", "right.limb", "left.limb")


  # calculate mean of best n tests for l and r
  # if best.n = 1, max value is taken

  # right limb
  temp.r <- temp[, c(1, 2, 3)]
  # descending order of tests by date
  temp.r <- temp.r[order(temp.r$test.date, -temp.r$right.limb), ]
  # group by test.date
  temp.n.right <- Reduce(rbind,
                        by(temp.r,
                           temp.r["test.date"],
                           head,
                           n = best.n
                        ))
  temp.n.right.agg <- aggregate(temp.n.right$right.limb,
                            by = list(temp.n.right$test.date),
                            mean,
                            na.rm = TRUE)
  colnames(temp.n.right.agg) <- c("test.date", "right.limb")

  # see comments above for explanations of the following chunk
  temp.l <- temp[, c(1, 2, 4)]
  temp.l <- temp.l[order(temp.l$test.date, -temp.l$left.limb), ]
  temp.n.left <- Reduce(rbind,
                        by(temp.l,
                        temp.l["test.date"],
                        head,
                        n = best.n
                   ))
  # take mean of best n for l limb
  temp.n.left.agg <- aggregate(temp.n.left$left.limb,
                            by = list(temp.n.left$test.date),
                            mean,
                            na.rm = TRUE)
  colnames(temp.n.left.agg) <- c("test.date", "left.limb")

  # bind data.frames
  temp.cbind.agg <- cbind(temp.n.left.agg, temp.n.right.agg)[, c(1, 2, 4)]
  temp.cbind.agg$parameter = parameter




  # build wide data.frame
  temp.wide <- interlimb.wide(temp.cbind.agg,
                              test.dates = "test.date",
                              dominant.limb = dominant.limb,
                              parameter,
                              right.limb = "right.limb",
                              left.limb = "left.limb")

  # temp.wide <- interlimb.wide(data.set = temp.cbind.agg,
  #                           dominant.limb = dominant.limb)
  # build long data.frame
  temp.long <- interlimb.long(data.set = temp.wide)
  temp.long <- subset(temp.long, temp.long$asymmetry.index == asym.algo)

  # add reliability fns
  temp.wide.rel <- interlimb.wide(temp,
                                  test.dates = "test.date",
                                  dominant.limb = dominant.limb,
                                  parameter,
                                  right.limb = "right.limb",
                                  left.limb = "left.limb")

  # temp.wide.rel <- interlimb.wide(data.set = temp,
  #                               dominant.limb = dominant.limb)

  # if multiple trials are available, fix variable names
  col.na <- which(is.na(colnames(temp.wide.rel)))
  colnames(temp.wide.rel)[col.na] <- "parameter"
  temp.long.rel <- interlimb.long(data.set = temp.wide.rel)
  # remove trial "asymmetry.index"
  temp.long.rel <- subset(temp.long.rel, asymmetry.index != "trial")

  # daily mean, sd,
  temp.aggregate <- aggregate(x = abs(temp.long.rel$asymmetry),
                              by = list(
                                as.vector(temp.long.rel$asymmetry.index),
                                as.vector(temp.long.rel$test.date)
                              ),
                              mean,
                              na.rm = TRUE)
  # daily sd
  temp.aggregate$sd <- aggregate(x = abs(temp.long.rel$asymmetry),
                                 by = list(
                                   as.vector(temp.long.rel$asymmetry.index),
                                   as.vector(temp.long.rel$test.date)
                                 ),
                               sd,
                               na.rm = TRUE)[, 3]

  temp.aggregate$daily.cv <- abs(temp.aggregate$sd)/abs(temp.aggregate$x) * 100


  # remove mean asym calculations due to redundancy
  temp.aggregate <- temp.aggregate[, -3]


  # order aggregated df
  temp.aggregate <- temp.aggregate[do.call(order, temp.aggregate), ]
  # clean variable names
  colnames(temp.aggregate) <- c("asymmetry.index", "test.date",
                              "sd", "daily.cv")

  temp.aggregate <- temp.aggregate[, c(2, 1, 3, 4)]

  # filter for asym.index
  temp.aggregate <- subset(temp.aggregate, temp.aggregate$asymmetry.index == asym.algo)


  # bind test results and aggregate data.frames
  asym.df <- cbind(temp.long, temp.aggregate)[, c(-10, -11)]

  # add final reliability and changes in asym variables
  asym.df <- track.asym(data.set = asym.df,
                        na.fill = na.fill,

                        # updated track.asym function introduced redundancy here
                        asym.algo = asym.algo
                        )
  row.names(asym.df) <- 1:nrow(asym.df)

  # round output values
  asym.df$left.limb <- round(asym.df$left.limb, 2)
  asym.df$right.limb <- round(asym.df$right.limb, 2)
  asym.df$sd <- round(asym.df$sd, 2)
  asym.df$daily.cv <- round(asym.df$daily.cv, 2)
  asym.df$asym.cv <- round(asym.df$asym.cv, 2)

  # reorder columns
  asym.df <- asym.df[, -2]
  asym.df <- asym.df[, c(1, 3, 4, 2, 7, 5, 6, 8:13)]

  # remove asymmetry reliability measures when best.n == 1
  if(best.n == "1") {asym.df <- asym.df[, c(-9:-10)]}

  asym.df

}
