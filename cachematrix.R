# Function 1: receives a matrix, the result is a list
# containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
   # Initializes the m variable in the function
   m <- NULL
   # This is the subfunction set. It will update the value
   # of the m variable:
   set <- function(y) {
      # update x and m in the enviroment
      x <<- y
      m <<- NULL
   }
   # get will retain the matrix passed to the function
   # makeCacheMatrix
   get <- function() x
   # Assigning to m in the environment the matrixx
   setmatrix <- function(matrixx) m <<- matrixx
   # Get m and assign to getmatrix
   getmatrix <- function() m
   # this is the list that will be returned. 4 functions
   # names in the list are: set, get, setmatrix, getmatrix
   list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

# Function 2: Return a matrix that is the inverse of 'x'
# in Function 1
cacheSolve <- function(x, ...) {
   # Use the function getmatrix to get m. m can be NULL
   # or contain data
   m <- x$getmatrix()
   # Check if m is null. If not then it will use the cached
   # data
   if(!is.null(m)) {
      message("getting cached data")
      # If true, return m and end the function
      return(m)
   }
   # If m is FALSE...
   # Getting the matrix using the function get()
   data <- x$get()
   # Calculate the inverse and assign to m
   m <- solve(data, ...)
   # Passing the result to setmatrix function, which will
   # reset the value of m in the enviroment m<<-matrixx
   x$setmatrix(m)
   # m is the inverse, the last action displayed when calling
   # the cacheSolve()
   m
}
