## ... to the only wise God

## This couple of functions work together to compute, cache (store) and retrieve 
## the inverse of a given invertible matrix. The first time a given matrix is   
## passed to the workflow of these functions, the inverse is computed, cached and returned.
## After this first-time computation, whenever the inverse of the same matrix is required, 
## the functions do not re-compute; they rather retrieve the inverse from the cache. This saves 
## time and computational expenses.The first function, makeCacheMatrix, is essentially a 
## function constructor; it returns a list of functions for setting and retrieving the matrix as well as 
## functions for caching and retrieving the computed inverse. Function makeCacheMatrix
## also initializes placeholder objects for the matrix (x) and the inverse (Inv).
## The second function, cacheSolve, does the actual work by calling the functions listed in
## makeCacheMatrix. 
## The workings of both makeCacheMatrix and cacheSolve rely on two fundamental 
## concepts in R programming: lexical scoping and the use of superassignment operator (<<-) 
## to preserve state across repeated invocations of a closure (nested function). In simple terms, 
## lexical scoping stipulates that a function have access to the environment wherein it 
## was defined/created and looks therein for variables values. Superassignment operator ensures 
## values are binded (assigned) to variables in the parent (enclosing) environment of a function's
## environment. The parent environment wherein a function is defined remains, even after the 
## function's execution (temporary) environment has been discarded after the its call. In a 
## nutshell, the purpose of makeCacheMatrix and cacheSolve is made possible because superassignment 
## sends neccesary values to the parent environment and lexical scoping ensures the parent 
## environment is accessible to the functions to pick those values when they are being executed.

## Specifically, Function makeCacheMatrix constructs functions (setMatrix, getMatrix, setInverse 
## and getInverse) and returns a list containing these four functions. First, makeCacheMatrix 
## initializes the matrix, x (as an argument with a default value) and the inverse, Inv variables.
## Function setMatrix sets or resets the matrix to a desired value. Function getMatrix retrieves 
## the value of the matrix in order to supply it as a data for inverser computation. Function 
## setInverse does the caching by seting the object Inv to the value of the computed inverse. 
## Function getInverse retrieves the value of the cached object Inv from the cache, either to 
## be immediately returned (if it is not null) or be reset by Function setInverse and returned 
## therefater.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  setMatrix <- function(Matrix) { 
    x <<- Matrix # sets/resets x to desired matrix 
    Inv <<- NULL # initialilizes/reinitializes Inv object
  }
  getMatrix <- function() x # retrieves matrix value.
  setInverse <- function(Inverse) Inv <<- Inverse # stores inverse to cache
  getInverse <- function() Inv # retrieves inverse from cache.
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse) # accumulates all functions into a list to returned and passed to cacheSolve
}


## Function cacheSolve simply recieves and executes the list of functions constructed 
## by Function makeCacheMatrix. First, the value of inverse in the cache is retrieved with a call to 
## Function getInverse. The retrieved inverse value is tested to ascertain it is not null. Not 
## being null indicates that the inverse of the concerned matrix had been previously computed 
## and cached. In that case, Function caheSolve simply display a message to indicate the situation 
## and returns the retrieved inverse value. If the retrieved inverse value is null, Function 
## cacheSolve goes on to retrieve the matrix with a call to Function getMatrix and pass the retrieved
## matrix as data argument to the in-built Function solve to compute the inverse. Thereafter, the 
## computed inverser is stored in the cache with a call to Function setInverse and also returned as 
## output of Function cacheSolve.
## Note that without lexical scoping, the calls to those four functions would not have access to 
## the environment created by an instantation of the Function makeCacheMatrix. And without the 
## use of superassignment, the environment created at the instantation of makeCacheMatrix would 
## not contain the relevant values of object x and Inv for retrieval or setting.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("Retrieving inverse from cache")
    return(Inv)
  }
  data <- x$getMatrix()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}

## Caveat:
## Function cacheSolve would only recognize that the inverse of a matrix has been previously 
## computed and stored in the cache if Function makeCacheMatrix is not called to act on the 
## same matrix again. This is because everytime makeCacheMatrix is called, it re-initializes 
## the object Inv to null. Therefore, makeCacheMatrix should be called on a given matrix only once.
## Subsequently, the inverse of that same matrix should be obtained by simply calling cacheSolve 
## on the output of makeCacheMatrix for that matrix. For this reason, nesting Function makeCacheMatrix 
## in Function cacheSolve (i.e. cacheSolve(makeCacheMatrix(x))) would not retrieve a previously 
## computed and cached inverse from the cache; rather it will recompute the inverse again, defeating 
## the purpose of the functions. See example below.

## > specialMatrix <- makeCacheMatrix(testMatrix)
## > cacheSolve(specialMatrix)
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(specialMatrix)
## Retrieving inverse from cache
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4

## >cacheSolve(makeCacheMatrix(testMatrix))
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(makeCacheMatrix(testMatrix))
##       [,1] [,2]
##  [1,]    6    8
##  [2,]    2    4
