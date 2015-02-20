## Put comments here that give an overall description of what your
## functions do

## This function creates a 'special' matrix object that contains a matrix which
## is stored as 'x' as well as a series of functions to perform operations with the matrix.

makeCacheMatrix <- function(x = matrix()) {

  # creates a NULL matrix object to store the inverse
  m <- NULL
  
  #creates a function to re-set the matrix object with a new matrix
  set <- function(y) {
    
    #re-sets the matrix x
    x <<- y
    
    #re-sets the inverse to NULL
    m <<- NULL
    
  } # end SET fucntion
  
  #createsa function to return the stored matrix.
  get <- function() x
  
  #creates a function to set the inverse matrix m.
  setinverse <- function(solve) m <<- solve(x)

    #creates a function to return the stored inverse matrix m.
  getinverse <- function() m
  
  #creates a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} # end function makeCacheMatrix


## This function takes in a special matrix object and CACHE / saves the inverse

cacheSolve <- function(x, ...) {
  
  # retrieves the inverse from the special object
  m <- x$getinverse()

  #checks to see if cache has happened previously, if so return cached inverse  
  if(!is.null(m)) {
      message("getting cached data")
      #returns the inverse
      return(m)
    }
  
  #gets the matrix object
  data <- x$get()
  #calcules the inverse
  m <- solve(data, ...) 
  #caches the inverse as a matrix within the object
  x$setinverse(m)
  #returns the inverse
  return (m)  
  
} # end fucntion cachSolve

