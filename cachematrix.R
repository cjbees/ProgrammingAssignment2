## With 2 functions, cache the inverse of a matrix


## Creating a list of functions that can work with a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv_x <- NULL #in case you need to recalculate the inverse matrix in cacheSolve
		set <- function(y) {     #Change the stored vector 
				x <<- y #to make x visible across environments
				inv_x <<- NULL #to make M null across environments
		}
		get <- function() x #Return the stored vector 
		setinverse <- function(inverse) inv_x <<- inverse #store the value of the input in M into the function makeCacheMatrix
		getinverse <- function() inv_x #Return the value of the input M into the function
		list(set = set, get = get, #Assign the functions to a list
				setinverse = setinverse,
				getinverse = getinverse)
				


}


## Check if the inverse has been cached; if not, create the inverse matrix for the result of makeCacheMatrix above
cacheSolve <- function(x, ...) {
       inv_x <- x$getinverse() #Subsetting to use the function stored in makeCacheMatrix
       if(!is.null(inv_x)) {    #Check that M is not null
       		message("Getting cached data")
       		return(inv_x)
       }
       data <- x$get()
       inv_x <- solve(data, ...)
       x$setinverse(inv_x)
       inv_x
       
}

#matrix <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
