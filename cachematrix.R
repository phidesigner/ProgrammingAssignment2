## This function creates a special "matrix" object that can cache its inverse.
## The first part, *makeCacheMAtrix* creates a special "matrix", 
## which is a list to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {             # creates special matrix 
        m<-NULL                                         # initiaises the inverse property
        set<-function(y){                               # sets the matrix's value
                x<<-y
                m<<-NULL
        }
        get<-function() x                               # gets the matrix's value
        setinverse<-function(solve) m<<- solve          # sets the matrix's inverse
        getinverse<-function() m                        # gets the matrix's inverse
        list(set=set, get=get, setinverse=setinverse,   # creates list
             getinverse=getinverse)
}
## The next step, *cacheSolve* calculates the inverse of the special "matrix"
## previously created, by:
# 1. Checking to see if the inverse was already calculated 
# 2. if so, it gets the inverse from the cache 'm' variable, skipping the computation 
# 3. otherwise, it calculates the inverse of the matrix and sets the value 
#    of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {                           # function to calculate the inverse
        m<-x$getinverse()                                  # assigns the inverse matrix to 'm'
        if(!is.null(m)){                                   # returns inverse if already set
                message("previously computed inverse...")
                return(m)
        }
        matrix<-x$get()                                    # gets matrix from x
        m<-solve(matrix, ...)                              # calculates matrix's inverse
        x$setinverse(m)                                    # sets the inverse to 'm'
        m                                                  # prints the inverse
}