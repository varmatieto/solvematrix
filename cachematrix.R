## create a special list that contains the basic operations on a matrix: 
## set, get, make inverted matrix, get inverted matrix
## the function check if matrix is a square matrix, if not give a 
## message and quit it

makeCacheMatrix <- function(x = matrix() ) 
{
    
    if (dim(x)[1]!= dim(x)[2]) # here check for square matrix
    {
        message("not square matrix")
        return()
    }
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        
    }
    
    get <- function() x
    setsolve <- function(invmtx) m <<- invmtx
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## this function is a child of makeCacheMatrix function
## it check if a named  inverted matrix  is already stored and return it
## or calculate it, store and return it




cacheSolve <- function(x, ...) {
    m <- x$getsolve() 
    if(!is.null(m)) # check if the inverted matrix is already stored 
        {
        message("getting cached data")
        return(m)
        }
                        # if no inverted matrix is available get matrix
    data <- x$get()
    m <- solve (data)
    x$setsolve(m)       # invert and store it
    m                   # inverted is returned  
}
