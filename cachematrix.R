## The object is to cache the inverse of the matrix so that it can be retrieved whenever required as opposed
## to computing it everytime it is needed. There are two functions required for this to be done.

## The makeCacheMatrix() function is for creating a R object that stores a matix and its inverse. This is
## comprised for two getter and setter functions each. 

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initializing NULL value to ix, which stands for inverse of matrix x. Value of x, 
  ##which is a matrix, is already assigned as part of function argument
  ix <- NULL 
  
  ##Setter function to get a new matrix and assign it to x. Since the value of x has to be passed to the
  ##makeCacheMatrix environment, we use the <<- operator. Also, inverse of X, which is specified by ix,
  ##is made null because we have a new matrix now
  setMatrix <- function (y) 
  {
    x <<- y
    ix <<- NULL
  }
  
  ##Getter function to get the value of matrix which is assigned to x
  getMatrix <- function () x 
  
  
  ##Setter function for setting inverse of new matrix, if new matrix is passed to x. This is called by 
  ##cacheSolve() function to set the inverse value of the new matrix to inverseX
  setInverse <- function(ixt) ix <<- ixt
  
  ##Getter function to get the value of inverse of matrix x
  getInverse <- function() ix
  
  ##list is given names so that $ operator can be used to call the functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse, setInverse = setInverse)
}


## This function is retrieve the inverse from the cache value that is stored in makeCacheMatrix()'s object
##environment. In case a cached value is not there, it is computed and set in makeCacheMatrix()'s environment
##using a setter function defined in makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ##First up, check whether an inverse of x already exists in the cache using the getter function
  xx <- x$getInverse()
  
  ##If inverse of x exists (by checking whether it is not NULL), return it and avoid unncecessary 
  ##computations
  if(!is.null(xx))
  {
    message("getting cached matrix")
    return(xx)
  }
  
  ##Now going to the part of the function where we get x and invert it. Also use the setter function to
  ##cache its value
  data <- x$getMatrix()
  xx<-solve(data, ...)
  x$setInverse(xx)
  xx
}
