## Put comments here that give an overall description of what your
makeCacheMatrix <- function(x = matrix()) {     ## function responsible for storing the inverse
  inv <- NULL                                   # setting up inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                         #function to obtain x
  setINV <- function(inverse) inv <<- inverse
  getINV <- function() inv
  list(set = set,
       get = get,
       setINV = setINV,
       getINV = getINV)
}

cacheSolve <- function(x, ...) {                ## function responsible for solving the inverse
  inv <- x$getINV()
  if (!is.null(inv)) {                          # inspecting if inverse is null
    return(inv)                                 # take back inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)                    # computes for inverse value
  x$setINV(inv)
  inv
}
