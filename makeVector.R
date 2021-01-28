makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(men) m <<- men
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}