# Install function for packages. It will check whether a package is available
# If a package is not available, this function will install the package.
package_required<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x)
    require(x,character.only=TRUE)
  }
}

# Using the newly created function, load all the required packages for this 
# simulation jobs.
package_required(mvtnorm)
package_required(nnet)
package_required(e1071)
package_required(class)
package_required(rpart)
package_required(ipred)
package_required(randomForest)
package_required(caret)