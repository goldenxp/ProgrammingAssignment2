## CacheMatrix for Coursera Class by goldenxp
## Built to prevent excessive matrix calculations
## by caching their computations for later retrieval

## Create a special matrix object for caching purposes

makeCacheMatrix <- function(x = matrix()) {
    ## Declare properties
    inv <- NULL
    ## Build setter to bind matrix and properties
    set <- function(y)
    {
      ## Store the new matrix
      x <<- y
      ## This is important. NULL the stored inverse
      ## This ensures changed matrices can have
      ## their inverses re-calculated!
      inv <<- NULL
    }
    ## Build getter to retrieve special matrix
    get <- function() x
    ## Build setter for the inverse value
    setInverse <- function(inverse) inv <<- inverse
    ## Build getter for the inverse value
    getInverse <- function() inv
    ## Use list to compose tag/value pairs
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of a Cache Matrix by either
## 1) calculating it or
## 2) returning a precalculated value assuming the matrix
##    has been calculated once before and has not changed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Attempt to get inverse
    inv <- x$getInverse()
    ## If not null, we already have a value
    if (!is.null(inv))
    {
        message("Cached value found! Returning now!")
        return(inv)
    }
    
    message("No cache found. Calculating and caching now!")
    # Get stored matrix
    matrix <- x$get()
    # Solve the  matrix
    newInv <- solve(matrix)
    # Store new value in the special matrix (woah)
    x$setInverse(newInv)
    # Return the new inverse
    newInv
}

## Testing instructions used
## source("cachematrix.R")
## m <- matrix(rnorm(1:16)*10,4)
## sm <- makeCacheMatrix(m)
## cacheSolve(sm)
## (message = No cache found. Calculating and caching now! )
## cacheSolve(sm)
## (message = Cached value found! Returning now! )
