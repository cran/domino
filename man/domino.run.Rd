\name{domino.run}
\alias{domino.run}
\encoding{UTF-8}
\title{
domino.run
}
\description{
Runs your project on Domino servers with given parameters.
}
\usage{
domino.run(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{...}{
All the run arguments will be joined together using space character. \\
Ex. \code{domino.run("main.py", "-xvz", "my-file1.csv")}
}
}
\examples{\dontrun{
    my_data <- 4
    domino.run("main.R","1","my-file1.csv", my_data)
    #=> Runs "domino main.R 1 my-file1.csv 4"}
}
\keyword{ run }

