##The functions are to calculate the value of inverse matrix and assign the values in a different place from the
##actual workplace so it can be accessed in case the inverse matrix of a matrix has been calculate 

## Returns an object that has the getter and setters to show the value of the matrix, the inverse, set a new matrix 
## and set the new inverse, although some work with cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  #initializes the value for inv
  inv <- NULL
  
  #Set is for changing the values of the matrix 
  set <- function(newmatr) {
    #Assign the values of the new matrix to x and set Null in the inverse when the setter is called 
    x <<- newmatr
    inv <<- NULL
  }
  #Getter for the matrix 
  get <- function() x
  
  #Setter for the value of the inverse matrix
  setinv <- function(inverse) inv <<- inverse
  
  #Getter for the value of the matrix 
  getinv <- function() inv
  
  #It arranges the list so the object that makeCacheMatrix returns can be used
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if the inverse of the matrix has been calculated, if it has it returns the inverse matrix if 
## not the function calculates the inversematrix and set it in the object returned by makeCache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #check if the matrix has already been calculated and returns the inverse if it has
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Get the data and calculate the inverse of the matrix 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
