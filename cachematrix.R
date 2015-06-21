## This script can be used to calculate the inverse of a matrix and store the result in cache.

## This Function will take a matrix as its argument and will return a list.
## NOTE: Even if the list returned is not assigned to a variable, it will still store some values in the cache
## set: this function will set the matrix to cache. It will also reset the inverse of any matrix stored in cache to NULL
## get: used to retrieve the matrix stored in cache
## setinverse: will take a matrix as an argument and put it in cache. The argument to this will be retrived from cacheSolve() function
## getinverse: will retrieve inverse stored by setinverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix()) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function will evaluate the matrix stored in cache by makeCacheMatrix, for its inverse.
## It will take the matrix in cache as its argument.
## ... can take arguments for solve() function
## This function will return the inverse if its not been cached, or cached inverse if it has already been cached.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached inverse data")
    return(s)
  }
  matrixData <- x$get()
  s <- solve(matrixData, ...)
  x$setinverse(s)
  s
}
