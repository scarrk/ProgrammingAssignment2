#############################################################################
# Assignment:  Caching the Inverse of a Matrix
# Description: Provides a wrapper for caching the Inverse/Solve operation
#              for a matrix, to speed-up subsequent calls when the source
#              values are un-changed.
# Reference:   https://class.coursera.org/rprog-002/human_grading/view/courses/972078/assessments/3/submissions
#
# Author:  Kevin Scarr
# Date:    April 2014
# Version: 
#######################################


############################# Procedural Section #############################


#######################################
## makeCacheMatrix
##  This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#######################################
## cacheSolve
##  This function computes the inverse of the special "matrix" returned by
##   makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cacheSolve will retrieve the 
##   inverse from the cache (a message is displayed to confirm this).
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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


############################# Testing Section #############################

## Intentionally commented out
#a <- matrix(c(3,1,2,1),nrow=2,ncol=2)
#aa <- makeCacheMatrix(a)
#cacheSolve(aa)
#cacheSolve(aa) # Yields message getting cached data as expected
#b <- matrix(c(10,1,5,1),nrow=2,ncol=2)
#bb <- makeCacheMatrix(b)
#cacheSolve(bb)
#cacheSolve(bb) # Yields message getting cached data as expected


############################# Peer Grading Checklist #######################
# 1. [Done] Was a valid GitHub URL containing a git repository submitted?
# 2. [Done] Does the GitHub repository contain at least one commit beyond the original fork?
# 3. [Done] Does the GitHub repository contain an R file containing code implementing the completed assignment? 
# 4. [Done] Does the R file containing the code have any comments explaining what the code does?
# 5. [Done] Does the R code implementing the 'makeCacheMatrix' function appear to be correct, to the best of your ability to judge?
# 6. [Done] Does the R code implementing the 'cacheSolve' function appear to be correct, to the best of your ability to judge?

