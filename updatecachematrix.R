
# Function to create a matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize the inverse matrix as NULL
  inv <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix changes
  }
  
  # Method to get the matrix
  get <- function() {
    x
  }
  
  # Method to set the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Method to get the inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of a matrix, caching the result if possible
cacheSolve <- function(x) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If the inverse is not cached, compute it using solve()
  data <- x$get()
  inv <- solve(data)
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  inv
}

# Create a matrix object using makeCacheMatrix
mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse using cacheSolve

cacheSolve(mat)

