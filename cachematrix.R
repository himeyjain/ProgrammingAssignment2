## This program is used to store a matrix
## calculate its inverse and store it in cache

## makeCacheMatrix is used for storing the matrix and its inverse
## It contains the following functions:
## 1. setMatrix     to set the value of matrix
## 2. getMatrix     to get the value of matrix
## 3. setCache      to set the inverse value in cache
## 4. getCache      to get the inverse value
makeCacheMatrix <- function(x = matrix()) {
  
  ## used to store the cached value. Initialized so assigned NULL
  cacheMatrix <- NULL
  
  ## store matrix
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    ## to remove the exisiting value of matrix as new value will be assigned
    cacheMatrix <<- NULL
  }
  
  ## fetch matrix
  getMatrix <- function() {
    x
  }
  
  ## cache the matrix
  setCache <- function(inverse) {
    cacheMatrix <<- inverse
  }
  
  ## fetch cached matrix
  getCache <- function() {
    cacheMatrix
  }
  
  ## to return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setCache = setCache, getCache = getCache)
  
}


## cacheSolve is used to check for exisitng cached value
## if cached value is null, it creates a matrix which is inverse of entered matrix

cacheSolve <- function(x, ...) {
  ## get cached value
  cacheMatrix <- x$getCache()
  
  ## checks for cached value. Returns the value if not null
  if(!is.null(cacheMatrix)) {
    message("Fetching data from Cache")
    return(cacheMatrix)
  }
  
  ## calculate the inverse of matrix and store in cache
  matrix <- x$getMatrix()
  cacheMatrix <- solve(matrix, ...)
  x$setCache(cacheMatrix)
  
  ## returns the inverse
  cacheMatrix
  
}
