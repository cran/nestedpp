generate_random_example <- function(seed = 1234, nrow = 10, ncol = 5, withNA = TRUE){
    #' @title Generates random data frame
    #' @description Function that returns a random data frames
    #' @param seed seed for random generation
    #' @param nrow number of rows
    #' @param ncol number of columns
    #' @param withNA bool to indicate if we want to include some NA values in the data frame
    #' @return a data frame with random data
    #' @examples
    #' example_data = generate_random_example(seed = 123456, nrow = 15, ncol=6)
    #'
    set.seed(seed)
    example = data.frame()
    if (withNA){
        for (i in 1:nrow){
            example = rbind(example,sample(c(stats::runif(ncol),NA),ncol,replace=TRUE,prob=c(rep(0.9/ncol,ncol),0.1)))
        }
    } else {
        for (i in 1:nrow){
            example = rbind(example,stats::runif(ncol))
        }
    }
    colnames(example) = paste("opt",1:ncol,sep="")
    return(example)
}
