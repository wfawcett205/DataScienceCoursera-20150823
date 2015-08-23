## makeCacheMatrix() creates a virtual environment
## that is distinct and separate from the working 
## environment.

## makeCacheMatrix
## Creates a list of four virtual functions:
## 1) set.matrix()
## 2) get.matrix()
## 3) set.matrix.inv()
## 4) get.matrix.inv()
## This list is used as the input to cacheSolve().

makeCacheMatrix <- function(x = matrix(), ...) {
    matrix.inv <- NULL
    set.matrix <- function(y){
        x <<- y
        matrix.inv <<- NULL
    }
    get.matrix <- function() x
    set.matrix.inv <- function(solve) matrix.inv <<- solve
    get.matrix.inv <- function() matrix.inv

## Return a list of 4 variables
 
    list(set.matrix = set.matrix,
       get.matrix = get.matrix,
       set.matrix.inv = set.matrix.inv,
       get.matrix.inv = get.matrix.inv)
}

## cacheSolve(): Returns the inverse of the "matrix"
## object passed by makeCacheMatrix(). If the function
## has been previously called and the "matrix" 
## object is unchanged: returns the cached value.
## The return of the "cached" value can result in 
## significant saving of "computing time" depending
## on the complexity of the matrix to be inverted.
 
cacheSolve <- function(x, ...){
  matrix.inv <- x$get.matrix.inv()
  if(!is.null(matrix.inv)){
      message("getting cached data")
      return(matrix.inv)
  }
  matrix.data <- x$get.matrix()
  matrix.inv <- solve(matrix.data)
  x$set.matrix.inv(matrix.inv)
  return(matrix.inv)
}
