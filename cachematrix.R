## This file contains two functions to compute and cache the inverse of a matrix
## Function 1: makeCacheMatrix()
## Function 2: cacheSolve()
## Sample code is provided at the end of the file to demonstrate functionality.


## makeCacheMatrix returns a list of functions to return the stored 
## values of the orignal matrix ($get) and its inverse ($getinverse).
## The function also provides the ability to set the initial value of
## the matrix ($set) and the value of the matrix inverse ($setinverse)
makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
      
}


## The cacheSolve function computes and stores (caches) the inverse of the matrix X
## If the inverse already exists ($getinverse), it just returns the stored value.
## For this assignment, we could assume the supplied matrix was always invertible.
## As such, the R function solve was used to compute the matrix inverse.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }      
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      return(m)
      
}

###########  sample code to demonstrate function use and functionality 

##### Create test matrix m1
## > m1 = makeCacheMatrix(rbind(c(1, 2), c(3, 4)))

##### Display test matrix
## > m1$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4

##### Solve for inverse of m1
## > cacheSolve(m1)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

##### Attempt to solve again, but result already exists so cached value was retrieved
## > cacheSolve(m1)
## getting cached data
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

##### Display stored inverse value
## > m1$getinverse()
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

##### Perform matrix multiplication to demonstrate m1$getinverse() actually contains inverse
##### since m1 x (inverse of m1) should equal identity matrix 
## > round(m1$get()%*%m1$getinverse())
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
