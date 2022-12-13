performance_profile <- function(data,
                                minimize = TRUE,
                                logbase = 1,
                                legend_title = "",
                                xlab = expression(tau),
                                ylab = expression(rho),
                                xmax = 0,
                                colors = c(),
                                plot = TRUE,
                                xgrid = c()
                                ){
    #' @title Performance profile function
    #' @description Function that returns a ggplot object with the corresponding performance profile
    #' @param data data frame with the data
    #' @param minimize true if lower values mean best performance and false otherwise
    #' @param logbase base of the logarithm used to represent performance profiles
    #' @param legend_title title for the legend
    #' @param xlab x axis label
    #' @param ylab y axis label
    #' @param xmax maximum value of xaxis
    #' @param colors vector with the colors of each configuration
    #' @param plot bool to display the plot or not
    #' @param xgrid vector for using it as grid in ratios
    #' @return ggplot object with the corresponding performance profile
    #' @examples
    #' example_data = generate_random_example(seed = 1234)
    #' performance_profile(example_data)
    #'
    pp_data = data
    if (minimize){
        for (i in 1:nrow(pp_data)){
            pp_data[i,] = pp_data[i,]/min(pp_data[i,],na.rm=TRUE)
        }
    } else {
        for (i in 1:nrow(pp_data)){
            pp_data[i,] = max(pp_data[i,],na.rm=TRUE)/pp_data[i,]
        }
    }
    if (logbase > 1){
        pp_data = log(pp_data,base=logbase)
    }
    isna = F
    max_ratio = max(pp_data,na.rm=TRUE)*1.05
    if (sum(is.na(pp_data)) > 0){
        pp_data[is.na(pp_data)] = max_ratio
        isna = TRUE
    }

    x_values = xgrid
    if (length(xgrid) == 0){
        x_values = unlist(pp_data,use.names=F)
        x_values = sort(x_values[!duplicated(x_values)])
    }
    y_values = data.frame()
    i = 1
    for (x in x_values){
        if (xmax > 0 && x > xmax){
            y_values = rbind(y_values,colSums(pp_data<=xmax)/nrow(pp_data))
            y_values = rbind(y_values,colSums(pp_data<=xmax)/nrow(pp_data))
            x_values = c(x_values[1:(i-1)],xmax)
            break
        }
        y_values = rbind(y_values,colSums(pp_data<=x)/nrow(pp_data))
        y_values = rbind(y_values,colSums(pp_data<=x)/nrow(pp_data))
        i = i + 1
    }
    x_values = rep(x_values,times=rep(2,length(x_values)))[-1]
    y_values = y_values[-nrow(y_values),]
    colnames(y_values) = colnames(pp_data)
    y_values = cbind(y_values,x_values)
    value = variable = NULL
    melted = melt(y_values,id.vars="x_values")

    final_xmax = NULL
    if (isna && (max_ratio %in% x_values)){
        if (xmax > 0 && xmax < (max_ratio/1.05)){
            final_xmax = xmax
        } else {
            final_xmax = max_ratio/1.05
        }
    } else {
        if (xmax > 0){
            final_xmax = xmax
        } else {
            final_xmax = x_values[length(x_values)]
        }
    }
    melted = melted[melted$x_values <= final_xmax,]

    p <- ggplot(melted, aes(x_values,value,col=variable))
    p <- p + geom_line()
    if (length(colors) > 0){
        p <- p + scale_colour_manual(name = legend_title, values = rep(colors,ceiling(ncol(pp_data)/length(colors)))[1:ncol(pp_data)])
    } else {
        p <- p + scale_colour_discrete(name = legend_title)
    }
    p <- p + xlab(xlab) + ylab(ylab)
    p <- p + scale_x_continuous(limits = c(x_values[1],final_xmax))
    if (plot){
        print(p)
    }
    return(p)
}

