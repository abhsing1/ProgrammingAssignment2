## Put comments here that give an overall description of what your
## functions do

## This function will take input from the user as "matrix". Please make sure the matrix is invertible. It will generate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
          set <- function(y) {
            x <<- y
            
          }
  
          get <- function() x
          setinverse <- function(solve) { m <<- solve(x) }
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function will return the inverse of the matrix - when called with cacheSolve(x). x being the output of makeCacheVector function.
## for example: 
## Step 1: makevector<- makeCacheMatrix(matrix(rnorm(9), 3, 3)) ##  This is an invertible matrix
## Step 2: cache<- cacheSolve(makevector) 
## Step 3: cache                                                ## This will provide the inverse of the matrix. In case of repititive input, cache output will be displayed

cacheSolve <- function(x, ...) {
 
 m <- x$getinverse()
  
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }
          
         
          data <- x$get()
          m <- solve(data)
          x$setinverse(m)
          m
}
