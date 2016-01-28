## makeCacheMatrix() is used to set a matrix, get this matrix
##along with setting inverse of the matrix and getting inverse of the matrix
##all possible from the "outside" enviroment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {  ##Setting matrix x from outside of the function enviroment via y
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ##Getting matrix x from outside enviroment of the function
        setinverse<- function(inverse) inv<<-inverse ##Setting inversion of matrix from outside enviroment of the function
        getinverse <- function() inv ##Getting inversion of matrix from outside enviroment of the function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

##cacheSolve() finds out if an inversion of the matrix has already been done
## if it has been already done, the function loads the data from cache (the data is saved in makeCacheMatrix())
##otherwise is computes the inverse and saves it to cache
## the data is saved to makeCacheMatrix() so the next time it doesnt have to be computed again

cacheSolve <- function(x, ...) { 
        if (!is.null(z$getinverse())) { ##checking if the inverse has not been already computed
                message("getting cached data")
                return(z$getinverse()) ##if the inverse has already been computed, function loads it from the cache
        }
        
        inverse <- solve(z$get()) ##if the inverse has not been computed yet, it computes it
        z$setinverse(inverse) ##and saves it to cache
        return(inverse)
}
