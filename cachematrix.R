## functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        m <<- null
        
        ## To set the value of matrix
        set <- function(y){
                
                x <<- y
                m <<- NULL
                
        }
        
        ## To get value of matrix
        get <- function() x
        
        ## To set the value of inverse
        setInverse <- function(inverse) m <<- inverse
        
        ## To get the value of inverse
        getInverse <- function() m
        
        matrix(set = set, get = get, setInverse = setInverse, 
               getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above
cacheSolve <- function(x, ...) {
        
        m <- x$getmean()
        
        ## Checks to see if the inverse has already been calculated
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        ## If inverse is not calculated then it calculates the inverse
        m <- inv(data, ...)
        
        x$setinverse(m)
        
        ## Return a matrix 'm' that is the inverse of 'x'
        m
}
