
## Programming Assignment 2
##  R Programming - Johns Hopkins
##  by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
##
## This solution is by Andreas Krueger 20/6/2014
## 
## Goal:
## A matrix inversion is calculated, and then cached.  
## To save computation time, a repeated call of cacheSolve 
## does not recalculate - but returns the previous result. 
## 
## N.B.: Contains the answer, plus 2 functions to test it, at the bottom.
##


makeCacheMatrix <- function(x = matrix()) {
        ## creates a special matrix object 
        ## which can cache its inverse
        
        inverse <- NULL                            # init with no inverse
        
        set <- function(y) {                       # (re)sets matrix
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x                        # returns matrix
        setinverse <- function(i) inverse <<- i    # caches inverse 
        getinverse <- function() inverse           # returns inverse
        
        list(set = set, get = get,                 # special object 4 functions
             setinverse = setinverse,
             getinverse = getinverse)
}
        

cacheSolve <- function(x, ...) {
        ## Return the inverse of the matrix stored in 'x'
        ## if possible from previously cached inversion.
        
        inverse <- x$getinverse()                 # look for cached inverse
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)                   # return if exists already
        }
                                                  # else:
        
        M <- x$get()                              # get the matrix
        inverse <- solve(M, ...)                  # calculate the inverse
        
        x$setinverse(inverse)                     # cache result
        inverse                                   # return result
}                



################## Programming Assignment ends here ###############
##
##     Still, interesting. This is how I was testing the above.
##
####### You can ignore the following in your evaluation  ##########


myTest1 <- function (r=3){
        ## generates a square matrix with r rows,
        ## asks for the inverse, 
        ## and then for the inverse again
        
        # "special matrix object" from random matrix ( = probably invertible):
        CM  <- makeCacheMatrix(  matrix (rnorm(r^2),r,r)  )
        
        # tests the result by multiplying the matrix with its inverse:
        multiplyPrint <- function (M1, M2) print (round ( M1 %*% M2 , 1)) 
        
        Mi <- cacheSolve(CM)           # 1st call
        multiplyPrint( CM$get(), Mi )
        
        Mi <- cacheSolve(CM)           # 2nd call
        multiplyPrint( CM$get(), Mi )
}

myTest2 <- function (size = 1000){
        ## generates a HUGE matrix, asks for the inverse 
        ## and then for the inverse again. Timing measured.
        
        # "special matrix object" from random matrix (=probably invertible):
        CM  <- makeCacheMatrix(  matrix (rnorm(size^2),size,size)  )
        
        print(system.time(    Mi <- cacheSolve(CM)       ))    # 1st call
        print(system.time(    Mi <- cacheSolve(CM)       ))    # 2nd call
}

