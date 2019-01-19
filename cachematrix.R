## Put comments here that give an overall description of what your
## functions do

#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to


## Write a short comment describing this function

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

#Note that it is assumed that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Write a short comment describing this function
#The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see 
#if the inverse has already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, it calculates 
#the inverse of the data and sets the value of the inverse in the cache 
#via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#test
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
A1<-makeCacheMatrix(A)
cacheSolve(A1)
solve(A)
