\name{SSM-package}
\alias{SSM-package}
\alias{SSM}
\docType{package}
\title{
\packageTitle{SSM}
}
\description{
\packageDescription{SSM}
}
\details{

Calculate local and cumulated loads and concentrations of a compound in a river network, following release from human intake and subsequent elimination in sewage treatment plants (STPs)
and any lakes in-between or downstream of those STPs, as well as different fractions of STP effluent in the river network.

Function calc_load() is the workhorse of this package, and its documentation should be consulted first to understand what this package does.
make_topology() is merely an internal helper function for calc_load().

In turn, function wrap_table() is a wrapper function to calc_load(). It uses a table input to calculate minimum and maximum loads and concentrations,
as well as different fractions of STP effluent in the river network. It also allows to run simulations for different scenarios years.

\packageDESCRIPTION{SSM}
\packageIndices{SSM}

}
\author{
\packageAuthor{SSM}

Maintainer: \packageMaintainer{SSM}
}
\references{
Ort et al. (ES&T, 2009, 43, pp. 3214-3220)
}

\keyword{ package }
