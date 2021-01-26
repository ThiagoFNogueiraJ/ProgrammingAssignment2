##These two functions were designed to save time when calculating matrix inverses, 
##creating a special type of matrix capable of saving its inverse.
 
#W The first function creates the matrix itself, 
##the object capable of holding a second matrix, which is its inverse;


# The function receives capital A as parameter, which is, by default, a matrix. 
makeCacheMatrix <- function(A = matrix()) {
  Ai <- NULL    ## set the var that will cache the inverse matrix as NULL 
  set <- function(y) {   ## Set the value of the matrix 
    A <<- y
    Ai <<- NULL          ## Clear the value of the inverse matrix if the original matrix change
  }
  get <- function() A ## Show the elements of the matrix 
  setInverse <- function(inverse) Ai <<- inverse ## Stores the inverse matrix in the var 
  getInverse <- function() Ai # Shows the inverse matrix, if already calculated  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Receive a special matrix created by makeCacheMatrix as input and calculates its inverse if
## the matrix is invertible  

cacheSolve <- function(A, ...) {
  Ai <- A$getInverse() # Get the var stored in the special matrix 
  if (!is.null(Ai)) {   #  checks if the inverse matrix has already been calculated and stored
    message("getting cached data") #If TRUE, just return the inverse matrix. 
    return(Ai)
  }
  data <- A$get()      # retrieves the elements from the original matrix
  Ai <- solve(data)    #calculates the inverse matrix (if possible)
  A$setInverse(Ai)     # stores the inverse matrix so that it can be consulted later
  Ai
}

