
## Programming Assignment 2
##  Johns Hopkins - R Programming
##  by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
##
## This solution is by Andreas Krueger 
## 
## Goal:
## A matrix inversion is calculated, and then cached.  
## To save computation time, a repeated call of cacheSolve 
## does not recalculate - but returns the previous result. 
## 
## N.B.: Contains not only the answer, but two functions to test them.


## makeCacheMatrix: 
## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## creates a special matrix object which can cache its inverse
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
        

## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## created by makeCacheMatrix above. 
## If the inverse has already been calculated, then cacheSolve 
## retrieves the inverse from the cache instead of recalculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if possible from previously cached inversion.
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        M <- x$get()
        inverse <- solve(M, ...)
        x$setinverse(inverse)
        inverse
}                

################## Programming Assignment ends here ###############
##
##     Still, interesting. This is how I was testing the above.
##
####### You can ignore the following in your evaluation  ##########


myTest1 <- function (){
        ## generates a matrix, asks for the inverse 
        ## and then for the inverse again
        
        r <- 3   # number of rows of square matrix        
        M   <- matrix (rnorm(r^2),r,r) # random matrix = invertible
        CM  <- makeCacheMatrix(M)      # create our "special matrix object"
        
        Mi <- cacheSolve(CM)           # first call
        
        # tests the result by multiplying the matrix with its inverse:
        multiplyPrint <- function (M, Mi) print (round (  M %*% Mi ,5)) 
        multiplyPrint(M, Mi)
        
        Mi <- cacheSolve(CM)           # 2nd call
        multiplyPrint(M, Mi)
}

myTest2 <- function (size = 1000){
        ## generates a HUGE matrix, asks for the inverse 
        ## and then for the inverse again. Timing measured.
        
        M   <- matrix (rnorm(size^2), size, size)  # random matrix
        CM  <- makeCacheMatrix(M)      # create our "special matrix"
        
        print(system.time(    Mi <- cacheSolve(CM)       ))    # 1st call
        print(system.time(    Mi <- cacheSolve(CM)       ))    # 2nd call
}

