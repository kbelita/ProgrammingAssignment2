## Make CacheMatrix and cacheSolve will cache the inverse of the matrix. 
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.

##  <<- operator can be used to assign a value to an object 
## in an environment that is different from the current environment

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It will create a list containing the a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           inv <- x$getinverse()
           if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
           }
           matrix.data <- x$get()
           inv <- solve(matrix.data, ...)
           x$setinverse(inv)
           inv
}






