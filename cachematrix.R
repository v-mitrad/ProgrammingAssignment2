## I have recreated the program with standard way as the previous 
## code i have submitted is rejected in peer review
## only changing mean() with solve()
## rest of the code is copied from example given by rdpeng

## we have to calculate inverse of a matrix
## if the inverse is already calculated and the matrix 
## is not changed the result will be come from cache

## this function will create a matrix and stores it's 
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## this funcion will return cached result or else compute 
## the inverse the matrix and stores in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## calculating the inverse of the given matrix
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}