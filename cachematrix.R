## Following functions demonstrates the use of <<- operator and 
## R's lexical scoping rules to store the objects value in the 
## function’s parent environment. It is used to avoid costly 
## computation such as inverse of matrix calculation and 
## to simulate a cache behavior 


## Function makeCacheMatrix()
## Input Argument - None or Object of class matrix  
## Returns - Object of class list

## This function wraps the matrix object to create a list object with
## following functions as it elements 
## set: Function to set a matrix object
## get: Function to get a matrix object
## setinverse : Function to set a inverse of matrix object
## getinverse : Function to get a inverse of matrix object

## By use of <<- operator the function defines and assigns variables
## the value of the matrix ## and its inverse in  
## a functions  parent environment.

## The returned object is used by cacheSolve() function


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Function cacheSolve()
## Input Argument - Object of class list retured by makeCacheMatrix function  
## Returns - Inverse of matrix set in the input object

## This function calculates and set the inverse of matrix in the input object 
## once and then it simply returns the value stored in the input object


cacheSolve <- function(x, ...) {

      inv <- x$getinverse()
      
      if(!is.null(inv)) {
            message("getting cached inverse of x")
            return(inv)
      }
      
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinverse(inv)
      inv
}
