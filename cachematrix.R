## 
## This is a part of learning of R Lexical Scoping. 
## The function makeCacheMatrix instantiate a matrix variable with the 
## type makeCacheMatrix(). The function cacheSolve solves (in this case 
## inverse) the input matrix. 
## 
## Example: from RStudio
## > c <- matrix (1:4, nrow=2, ncol=2)
## > c
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > dim(c)
## [1] 2 2
## > solve(c)      #Use this to validate result 
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## > d <- makeCacheMatrix(c)  #instantiate a matrix
## > cacheSolve(d)            # in the first time, it checks and set,
## inside getmatrix()
## inside get()
## inside setmatrix()
##     [,1] [,2]              # the result is correct
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(d)            # in the second time, it simply gets from cache.
## inside getmatrix()
## getting cached data     
##      [,1] [,2]             # the result is correct
## [1,]   -2  1.5
## [2,]    1 -0.5 
##
## Johns Hopkins online R Programming 
## Joshua Jin
## Last time updated: 4/19/2014

## The function makeCacheMatrix instantiate a matrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      message("inside set()")
      x <<- y
      m <<- NULL
    }
    get <- function() { 
      message("inside get()")
      x
    }
    
    setmatrix <- function(matrix) {
      message("inside setmatrix()")
      m <<- matrix
    }
    
    getmatrix <- function() {
      message("inside getmatrix()")
      m
    }
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The function cacheSolve solves (or inverse) the input  
## matrix and then caches the result at the first time; the 
## second it is called, it doesn't need to calculate it again
## . Instead it retrieves the object from memory cache.
cacheSolve <- function(x, ...) {
    m2 <- x$getmatrix()
    if(!is.null(m2)) {
      message("getting cached data")
      return(m2)
    }
    data <- x$get()
    m2 <- solve(data, ...)
    x$setmatrix(m2)
    m2  ## Return a matrix that is the inverse of 'x'
}
