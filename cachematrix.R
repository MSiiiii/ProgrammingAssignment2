# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(matrix = matrix()) {
    # Initialize a variable to store the inverse
    inverse <- NULL
    
    # Setter function to set the matrix
    setMatrix <- function(new_matrix) {
        matrix <<- new_matrix
        # When the matrix changes, invalidate the cached inverse
        inverse <<- NULL
    }
    
    # Getter function to retrieve the matrix
    getMatrix <- function() {
        matrix
    }
    
    # Getter function to retrieve the cached inverse or compute it if not present
    getInverse <- function() {
        if (!is.null(inverse)) {
            # If the inverse is cached, return it
            message("Getting cached inverse")
            return(inverse)
        } else {
            # If the inverse is not cached, compute it using solve()
            message("Calculating inverse")
            inverse <- solve(matrix)
            # Cache the computed inverse
            return(inverse)
        }
    }
    
    # Return a list of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and retrieve from cache if available
cacheSolve <- function(cacheMatrix) {
    # Get the cached inverse or compute it using the getInverse function from makeCacheMatrix
    inverse <- cacheMatrix$getInverse()
    
    # Return the inverse
    inverse
}

# Example Usage:

# Create a cacheMatrix object
myMatrix <- makeCacheMatrix(matrix = matrix(c(4, 3, 2, 1), 2, 2))

# Get the original matrix
print("Original Matrix:")
print(myMatrix$getMatrix())

# Get the inverse (computes and caches)
print("Inverse:")
print(cacheSolve(myMatrix))

# Get the cached inverse without recomputing
print("Cached Inverse:")
print(cacheSolve(myMatrix))
