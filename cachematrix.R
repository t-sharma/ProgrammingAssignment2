## All the comments are written inline for better understanding.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ## set the value of matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse ## set the value of inverse of the matrix
  getinverse <- function() inv ## get the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## Create/Display a list.
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) { ## If inverse is already computed
    message("getting cached data.") ## if cached data is returned, print this message
    return(inv) ## skip the computaton and return the inverse
  }
  data <- x$get() ## get the matrix
  inv <- solve(data) ## Compute Inverse
  x$setinverse(inv) ## set value in cache
  inv
}
## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,] 1.00 -0.25
## [2,] -0.25 1.00
## No cache in the first run
## > cacheSolve(m)
## [,1] [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 


