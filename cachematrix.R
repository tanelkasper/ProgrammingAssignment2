## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      m <- x$getinverse()
      if (!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
    }
        
## Testing

MX <- matrix(c(2,3,8,9),2,2)

MXX <- makeCacheMatrix(MX)

cacheSolve(MXX)

##getting cached data

cacheSolve(MXX)

##getting cached data
##[,1]       [,2]
##[1,] -1.5  1.3333333
##[2,]  0.5 -0.3333333
