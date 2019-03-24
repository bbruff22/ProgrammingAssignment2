## This pair of functions cache and display the inverse of a matrix, allowing the processing
## needed to only be done if the matrix has changed. I've tested using three simple matrices, one
## from the Coursera forum and two from math sites (very simple testing)

## This function creates a matrix object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invMx <- NULL
  #set Matrix value
  setMx <- function(y) {   #store x and invMx values in global environment
    x <<- y
    invMx <<- NULL
  }
  getMx <- function() x                             #get Matrix value
  setInv <- function(inverse) invMx <<- inverse     #set Matrix inverse
  getInv <- function() invMx                        #get Matrix inverse
  list(setMx = setMx, getMx = getMx, setInv = setInv, getInv = getInv) #create list with function values
}		 

## This function computes the inverse of a matrix, if it doesn't exist in memory.
cacheSolve <- function(x, ...) {
  #get inverse matrix value
  invMx <- x$getInv()
  if(!is.null(invMx)) {                   #if check to determine if inverse matrix value is NULL
    message("Getting Cached Value")       #cached value message, lets users know value is from cache 
    return(invMx)                         #return inverse matrix
  }
  #if matrix inverse value NULL solve for inverse  
  MxData <- x$getMx()                     #gets matrix data
  invMx <- solve(MxData, ...)             #solve function finds inverse of matrix
  x$setInv(invMx)                         #set inverse 
  return(invMx)                           #returns inverse
}		 


## a few test runs...one from the posting on the forum and a couple from math sites found "googling"
tm1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
tm1		
CacheMx <- makeCacheMatrix(tm1)		
CacheMx$getMx()		
CacheMx$getInv()		
cacheSolve(CacheMx)		
cacheSolve(CacheMx)		


# test 2  
tm2 <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
tm2		
CacheMx <- makeCacheMatrix(tm2)		
CacheMx$getMx()		
CacheMx$getInv()		
cacheSolve(CacheMx)		
cacheSolve(CacheMx)		


# test 3
tm3 <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)
tm3		
CacheMx <- makeCacheMatrix(tm3)		
CacheMx$getMx()		
CacheMx$getInv()		
cacheSolve(CacheMx)		
cacheSolve(CacheMx)		
