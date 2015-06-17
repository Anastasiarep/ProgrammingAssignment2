## Caching the Inverse of a Matrix

## makeCacheMatrix is a function that stores a list
## of functions, creating a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
        s <- NULL
        set <- function(y)
## changes the matrix solved in the main function
	{
                x <<- y
                s <<- NULL
        } 
        get <- function() x
## returns the vector x stored in the main function
        setsolve <- function(inverse) s <<- inverse
## stores the value of the input in a variable s
## inverse is supposed to be the inverse of matrix x
        getsolve <- function() s
## returns the value of the variable s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
## binds all four functions into a list
## when we assign makeCacheMatrix to an object, it has all the functions
}

## cacheSolve is a function that computes the inverse of the
## special "matrix" returned by makeCacheMatrix. If the inverse
## has already been calculated (and the matrix has not
## changed), it retrieves the inverse from the cache,
## rather than recalculate it

cacheSolve <- function(x, ...)
{
        s <- x$getsolve()
        if(!is.null(s))
## verifies the value s axists and is not NULL
	{
                message("getting cached data")
                return(s)
        }
## if s exists in memory, returns a message and the value s
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
## else, gets the matrix stored,
## calculates the inverse
## and stores it in the object generated
}
