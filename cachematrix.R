## Functions for calculate and cache the inverse of a matrix.
## Matrix must be square and invertible.
## Author: Paloma M.S.A.
## Version: 1.0.0. Date: 21/02/2015.


## Create a special matrix that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## arg x must be a square invertible matrix 
    ## Return a list of functions to get/set the matrix and its inverse
    
    m <- NULL             #inicializate m (aux.)
    
    ##create "set" function to set the original matrix
    ##arg y is the value to set x (it must be a matrix too)
    set <- function(y) {  
        x <<- y       #assign y to x and save in cache
        m <<- NULL    #clear m and save in cache
    }
    
    ##create "get" function to return the original matrix (no args)
    get <- function() { x } #return x
    
    ##create "setInverse" function to set the inverse matrix (not calculate)
    ##arg "inverse" is the inverse matrix to save in cache
    setInverse <- function(inverse) { m <<- inverse } #set m and save in cache
    
    ##create "getInverse" function to return the inverse matrix (no args)
    getInverse <- function() { m } # return m (from cache)
    
    ##create a list of functions and return
    ##each element of the list has the same name of the function
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Compute the inverse of a matrix caching the result
cacheSolve <- function(x, ...) {
    ## x is the special matrix created with makeCacheMatrix()
    ## ... is extra arguments for solve function
    ## Return a matrix that is the inverse of 'x'
    
    ##check if the inverse is calculated
    m <- x$getInverse()   #get the inverse from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)     #exit function
    }
    
    ##else calculates inverse and save in cache
    data <- x$get()       #get x (matrix)
##### next version: here check if matrix is singular to avoid error
    m <- solve(data, ...) #calculate the inverse of matrix
    x$setInverse(m)       #save the inverse in cache
    m                     #return m
    
}
