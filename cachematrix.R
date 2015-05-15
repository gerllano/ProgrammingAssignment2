## This assignment is pretty dificult to be understood theoretically. Despite of that, the
## example provided by DanieleP makes it lot easier: https://github.com/DanieleP/PA2-clarifying_instructions

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # This is where the result of inversion is stored
  # This is used to set a matrix to object created by makecacheMatrix function
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL # It initialises xinv to null
  }
  
  get <- function() x # It returns the input matrix
  setInv <- function(inv) xinv <<- inv # It sets the inversed matrix
  getInv <- function() xinv # It returns the inversed matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) # It returns a list that contains these functions. This, this
  # functions can be used conviniently.
}

cacheSolve <- function(x, ...) {
  m <- x$getInv() # It gets the inversed matrix from x. I'll be null if uncalculated.
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # it returns the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # then we solve it
  x$setInv(m) # and then we set it to the object
  m # Then, we return the solved result
}