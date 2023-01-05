performance_table <- function(data,
                  tolatex = FALSE
                  ){
    #' @title Table function
    #' @description Function to generate a table with different metrics
    #' @param data data frame with the data
    #' @param tolatex true to print the latex code of the table
    #' @return the data frame with the metrics
    #' @examples
    #' example_data = generate_random_example(seed = 1234)
    #' table(example_data)
    #'

    result = data.frame(matrix(nrow=0,ncol=ncol(data)))
    colnames(result) = colnames(data)

    measures = c("min","q1","median","mean","gmean","q3","max","number_of_na")
    measures_display_name = {}
    measures_display_name["min"] = "Minimum"
    measures_display_name["q1"] = "1st quartile"
    measures_display_name["median"] = "Median"
    measures_display_name["mean"] = "Mean"
    measures_display_name["gmean"] = "Geometric mena"
    measures_display_name["q3"] = "3rd quartile"
    measures_display_name["max"] = "Maximum"
    measures_display_name["number_of_na"] = "NA's"

    for (measure in measures){
        result[measures_display_name[[measure]],] = apply(data,2,measure,na.rm=TRUE)
    }
    if (tolatex){
        print(xtable(result))
    }
    return(result)
}

q1 <- function(data,...){
    #' @title 1st quartile function
    #' @description Function to compute the first quartile
    #' @param data vector with the data
    #' @param ... further arguments passed to or from other methods
    #' @return 1st quartile value
    #' @examples
    #' q1(1:10)
    #'
    return(stats::quantile(data,probs=c(0.25),...))
}

q3 <- function(data,...){
    #' @title 3st quartile function
    #' @description Function to compute the third quartile
    #' @param data vector with the data
    #' @param ... further arguments passed to or from other methods
    #' @return 3st quartile value
    #' @examples
    #' q3(1:10)
    #'
    return(stats::quantile(data,probs=c(0.75),...))
}

gmean <- function(data,...){
       #' @title geometric mean function
       #' @description Function to compute the geometric mean
       #' @param data vector with the data
       #' @param ... further arguments passed to or from other methods
       #' @return geometric mean value
       #' @examples
       #' gmean(1:10)
       #'
       return(exp(mean(log(data),...)))
}

number_of_na <- function(data,...){
    #' @title number of Na's
    #' @description Function to compute the number of Na's
    #' @param data vector with the data
    #' @param ... further arguments passed to or from other methods
    #' @return number of Na's
    #' @examples
    #' number_of_na(1:10)
    #'
    return(as.integer(sum(is.na(data))))
}
