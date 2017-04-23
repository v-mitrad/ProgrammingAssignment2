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
  ## checking if it is valid square matrix
  if (nrow(data)!=ncol(data)){
    message("matrix is not square")
    invisible(0)
  }
  else {
    ## checking if the determinant is zero
    if ((1/determinant(data)$modulus[1])==0){
      message("determinant is zero")
      invisible(0)
    }
    ## calculate result
    else{
      m <- solve(data, ...)
      x$setmatrix(m)
      m
    }
  }
}