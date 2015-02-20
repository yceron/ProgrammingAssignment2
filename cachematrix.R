## These functions will cache the inverse of a matrix.

## This function handles storing the matrix and its reverse after it is calculated.
makeCacheMatrix <- function (m = matrix)
{
  inverseMatrix <- NULL
  set <- function (y) {
    inverseMatrix <<- NULL
  }
  get <- function () m
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list (set = set, get = get, setInverse =setInverse, getInverse = getInverse)
}

## Gets data. This function uses
## If it exists, it gets the cached version, if not, it calculates it and stores it in cache.
cacheSolve <- function (x, ...)
{
  m <- x$getInverse()
  if (!is.null(m))
  {
    ##Getting cached data
    return(m)
  }
  ##Calculate data
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
