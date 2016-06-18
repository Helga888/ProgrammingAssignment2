## Creates an object which stores a matrix itself and its corresponding inverse value.
## It also provides getters and setters methods to get and set those values

makeCacheMatrix <- function(matr = matrix()) { #accepts a matrix as an arugument, the matrix is stored in the function
       inv<-NULL # Setting a cache value of inverse matrix to a default value of NULL
       
       set<-function(y) {  
       matr<<-y     # sets the value of our matrix stored in the main funtion to a new value
       inv<<-NULL   # as it's a new matrix, the inverse matix stored in the main function is set to NULL here
       }
       
       get<-function() matr # getting the matrix
       
       setInverseMatrix<-function(inverseMatrix) inv<<-inverseMatrix #sets a cache value of inverse matrix
       
       getInverseMatrix<-function() inv                 #gets inverse matrix stored in cache
       
       list(set=set,get=get,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix) #creates a list of 
       #all the 4 'getters' and 'setters' functions, the list is returned by MakeCacheMatrix to make the functions accessible
}
       

## retrieves the inverse value of a matrix. If it is NULL, then inverse matrix is evaluated and passed to makeCasheMatrix where it's stored as a cashe value

cacheSolve <- function(matr, ...) { #as an argument it takes an object created by makeCacheMatrix
       inv<-matr$getInverseMatrix() #retrieves inverse matrix
       if(!is.null(inv)){ #if inverse matrix is not NULL, CasheSolve returns it and finishes its own execution
               message("getting cached data")
               return(inv)   
       }
       
       data<-matr$get() 
       inv<-solve(data) # if inverse matrix is NULL then inverse matrix is evaluated
       matr$setInverseMatrix(inv) #the evaluated inverse matrix is passed to makeCacheMatrix where it's stored as a cache value
       inv # then cacheSolve returns the evaluated inverse matrix
            
}
