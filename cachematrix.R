## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## a) set the value of the matrix. b) get the value of the matrix
## c) set the value of the inverse of the matrix.
## d) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function calculates the mean of the special "vector" created with the   
## makeCacheMatrix function. However, it first checks to see if the 
## inverse has already been calculated.If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## TEST:
## > x = rbind(c(1, 2), c(2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1]  [,2]
## [1,]   1    2
## [2,]   2    1


## > cacheSolve(m)
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## >  cacheSolve(m)
## getting cached data.
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333



