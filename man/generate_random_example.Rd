% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_data.R
\name{generate_random_example}
\alias{generate_random_example}
\title{Generates random data frame}
\usage{
generate_random_example(seed = 1234, nrow = 10, ncol = 5, withNA = TRUE)
}
\arguments{
\item{seed}{seed for random generation}

\item{nrow}{number of rows}

\item{ncol}{number of columns}

\item{withNA}{bool to indicate if we want to include some NA values in the data frame}
}
\value{
a data frame with random data
}
\description{
Function that returns a random data frames
}
\examples{
example_data = generate_random_example(seed = 123456, nrow = 15, ncol=6)

}
