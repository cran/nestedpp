nested_performance_profile <- function(data,
                                minimize = TRUE,
                                logbase = 1,
                                legend_title = "",
                                best_rule = "winner",
                                xlab = expression(tau),
                                ylab = expression(rho),
                                xmax = 0,
                                colors = c(),
                                plot = TRUE,
                                xgrid = c()
){
    #' @title Nested performance profile function
    #' @description Function that returns a ggplot object with the corresponding nested performance profile
    #' @param data data frame with the data
    #' @param minimize true if lower values mean best performance and false otherwise
    #' @param logbase base of the logarithm used to represent nested performance profiles
    #' @param legend_title title for the legend
    #' @param best_rule option to choose the best solver eliminated in each iteration ("winner", "mean", "gmean", "median" or custom function)
    #' @param xlab x axis label
    #' @param ylab y axis label
    #' @param xmax maximum value of xaxis
    #' @param colors vector with the colors of each configuration
    #' @param plot bool to display the plot or not
    #' @param xgrid vector for using it as grid in ratios
    #' @return ggplot object with the corresponding nested performance profile
    #' @examples
    #' example_data = generate_random_example(seed = 1234)
    #' nested_performance_profile(example_data)
    #'
    pp_data = data
    current_pp_data = pp_data
    S = colnames(pp_data)
    S_prime = colnames(pp_data)
    if (minimize){
        for (i in 1:nrow(current_pp_data)){
            current_pp_data[i,] = current_pp_data[i,]/min(current_pp_data[i,],na.rm=T)
        }
    } else {
        for (i in 1:nrow(current_pp_data)){
            current_pp_data[i,] = max(current_pp_data[i,],na.rm=T)/current_pp_data[i,]
        }
    }
    isna = F
    max_ratio = max(current_pp_data,na.rm=TRUE)*1.05
    if (sum(is.na(pp_data)) > 0){
        current_pp_data[is.na(current_pp_data)] = max_ratio
        isna = TRUE
    }
    if (typeof(best_rule) == "character"){
        if (best_rule == "winner"){
            winners = as.vector(colSums(current_pp_data == 1))
            winners = which(winners == max(winners))
        } else {
            if (best_rule == "mean"){
                winners = as.vector(apply(current_pp_data,2,mean))
            } else if (best_rule == "gmean"){
                winners = as.vector(apply(current_pp_data,2,function(x) exp(mean(log(x)))))
            } else if (best_rule == "median"){
                winners = as.vector(apply(current_pp_data,2,stats::median))
            } else {
                winners = as.vector(apply(current_pp_data,2,best_rule))
            }
            winners = which(winners == min(winners))
        }
    } else {
        winners = as.vector(apply(current_pp_data,2,best_rule))
        winners = which(winners == min(winners))
    }
    if (length(winners) > 1){
        best = sample(winners,size=1)
    } else {
        best = winners
    }

    S_best = c(colnames(current_pp_data)[best])
    S_prime = S_prime[S_prime != colnames(current_pp_data)[best]]
    if (logbase > 1){
        current_pp_data = log(current_pp_data,base=logbase)
    }
    list_pp_data = list(current_pp_data)

    for (k in 2:(ncol(pp_data)-1)){
        current_pp_data = pp_data[,-which(colnames(pp_data) %in% S_best)]
        if (minimize){
            for (i in 1:nrow(current_pp_data)){
                current_pp_data[i,] = current_pp_data[i,]/min(current_pp_data[i,],na.rm=TRUE)
            }
        } else {
            for (i in 1:nrow(current_pp_data)){
                current_pp_data[i,] = max(current_pp_data[i,],na.rm=TRUE)/current_pp_data[i,]
            }
        }
        if (isna){
            current_pp_data[is.na(current_pp_data)] = max_ratio
        }
        if (typeof(best_rule) == "character"){
            if (best_rule == "winner"){
                winners = as.vector(colSums(current_pp_data == 1))
                winners = which(winners == max(winners))
            } else {
                if (best_rule == "mean"){
                    winners = as.vector(apply(current_pp_data,2,mean))
                } else if (best_rule == "gmean"){
                    winners = as.vector(apply(current_pp_data,2,function(x) exp(mean(log(x)))))
                } else if (best_rule == "median"){
                    winners = as.vector(apply(current_pp_data,2,stats::median))
                } else {
                    winners = as.vector(apply(current_pp_data,2,best_rule))
                }
                winners = which(winners == min(winners))
            }
        } else {
            winners = as.vector(apply(current_pp_data,2,best_rule))
            winners = which(winners == min(winners))
        }
        if (length(winners) > 1){
            best = sample(winners,size=1)
        } else {
            best = winners
        }
        S_best_new = c(S_best, colnames(current_pp_data)[best])
        S_prime = S_prime[S_prime != colnames(current_pp_data)[best]]
        if (logbase > 1){
            current_pp_data = log(current_pp_data,base=logbase)
        }
        for (s in S_best){
            current_pp_data[s] = list_pp_data[[k-1]][s]
        }
        S_best = S_best_new
        list_pp_data = append(list_pp_data,list(current_pp_data))
    }

    x_values = xgrid
    if (length(xgrid) == 0){
        x_values = unlist(list_pp_data,use.names=F)
        x_values = sort(x_values[!duplicated(x_values)])
    }
    y_values = data.frame()
    i = 1
    for (x in x_values){
        accum = data.frame()
        for (k in 1:length(list_pp_data)){
            if (k == 1){
                accum = rbind(accum,(colSums(list_pp_data[[k]]<=x)/nrow(list_pp_data[[k]])))
                colnames(accum) = colnames(pp_data)
            } else {
                accum = rbind(accum,t(data.frame(colSums(list_pp_data[[k]]<=x)/nrow(list_pp_data[[k]]))))
            }
        }
        accum = colSums(accum)/length(list_pp_data)
        if (xmax > 0 && x > xmax){
            y_values = rbind(y_values,accum)
            y_values = rbind(y_values,accum)
            x_values = c(x_values[1:(i-1)],xmax)
            break
        }
        y_values = rbind(y_values,accum)
        y_values = rbind(y_values,accum)
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

