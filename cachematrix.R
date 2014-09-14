## Coursera's R Programming Course
## Programming Assignment 2: Lexical Scoping
## Due date: Sun 21 Sep 4:30 pm PST
##
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This script contains a pair of functions that cache the inverse of a matrix
## and returns to user
##
## We will use the following matrix to test the output of the functions:
##   A = | 1  1  0 |
##       | 1  0  1 |
##       | 0  1  0 |
##
## If the functions work well, the resultant A(-1) (inverse matrix) will be:
##
##   A(-1) = |  1  0 -1 |
##           |  0  0  1 |
##           | -1  1  1 |


## This function creates a special "matrix" object that can cache its inverse.
## Original matrix could be initialized either via makeCacheMatrix( matrix(...) ) 
## call or via $set( matrix(...) ) method

makeCacheMatrix <- function( x = matrix() ) {
   cached <- NULL         ## when calling the function or when invoking the
                          # $set method we flush the cached matrix   

   ## setter method
   set <- function( y ) {
      x      <<- y        ## set or update original matrix
      cached <<- NULL     ## flush the cached matrix
   }
   
   ## getter method
   get <- function() x    ## returns the original matrix
   
   setMatrix <- function( matrix ) { cached <<- matrix }
   getMatrix <- function() cached

   list( set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix )
}


## This function computes and returns the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the original matrix has not changed), then the 
## cacheSolve should retrieve the cached inverse from the cache

cacheSolve <- function( x, ... ) {
   cached <- x$getMatrix() 
   
   if( !is.null( cached ) ) {
      message( "The inverse matrix is already computed. Getting cached data" )
      return( cached )               ## return the cached matrix and exit the function
                                     ## (not processing the rest of the code)
   }
   
   oMatrix <- x$get()                ## get the original matrix
   cached <- solve( oMatrix, ... )   ## compute the inverse of the matrix
   x$setMatrix( cached )             ## set the inverse matrix to parent function
   
   return( cached )                  ## return the computed (+cached) matrix
}
