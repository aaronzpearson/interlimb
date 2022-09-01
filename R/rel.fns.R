# these functions are not exported

# weekly cv
rolling.cv <- function(x) {

  vect = abs(as.vector(x))
  cv.temp <- NULL

  for(i in 1:length(vect)) {

    cv.temp[i] = abs(sd(vect[1:i], na.rm = T))/ abs(mean(vect[1:i], na.rm = T)) * 100

  }

  cv.temp[1] <- 0
  abs(cv.temp)

}

# change relative to baseline value
rel.change <- function(x) {

  delta.x <- x - x[1]
  delta.x

}
