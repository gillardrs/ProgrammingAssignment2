## Put comments here that give an overall description of what your
## functions do
## Finding the inverse of a matrix can be very time consuming.  So rather than
## having to find the inverse multiple times if the matrix hasn't changed it
## can be stored in the cache and simply retrieved.  Therefore, I have made
## two functions that first create the cache matrix and then finds the inverse
## if the inverse isn't cached, else it solves the inverse

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
