## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The below pair of functions cache the inverse of a matrix.
## Note: matrix must be invertible, e.g. a square matrix.

## Usage:
## Initialise a cached matrix and assign it to a variable, 
## e.g. cachedMatrix <- makeCacheMatrix(matrix(1:4,2,2))

## Solve any invertible matrix, retrieving from cache if present, by,
## e.g. cacheSolve(cachedMatrix, matrix(6:9,2,2))


## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## It is a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(x) m <<- x
    get <- function() m
    setim <- function(inv_matrix) im <<- inv_matrix
    getim <- function() im
    list(set = set, get = get,
         setim = setim, getim = getim)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Compare entered matrix with the cached matrix
    this_matrix <- as.matrix(...)
    cache_matrix <- x$get()
    print(this_matrix)
    print(cache_matrix)
    test_ident <- identical(this_matrix, cache_matrix)
    print(test_ident)
    if(test_ident) {
      im <- x$getim()
      print(im)
      if(!is.null(im)) {
        message("getting cached data")
        ## Return the cached matrix that is the inverse of the entered matrix
        return(im)
      }
    }
    
    ## If cached matrix is not the same as the entered matrix, invert the 
    ## entered matrix and save to cached value 
    im <- solve(this_matrix)
    x$set(this_matrix)
    x$setim(im)
    im
}
