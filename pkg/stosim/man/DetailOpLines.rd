\name{DetailOpLines}
\alias{DetailOpLines}

\title{ Multiple OpLine Simulation Detail }

\description{
This is a function for combining multiple "Type 1" stochastic simulations in a common history.  It creates a dataframe detailing the operational status of each OpLine throughout the Simulation History.
}

\usage{
DetailOpLines(Model,Names=NULL) 
}

\arguments{
\item{Model}{ A list of dataframes constructed from the SimHistory function or some combination of histories using SimCombine.  All histories must have the same history length as developed by both SimulationYears and SimulationYearsPerPage.}
\item{Names}{ An optional vector of character labels for the OpLines contained in the Model.  If not provided the dataframe labels will be assigned to a progression of Train1, Train2, ..., etc. according to the number of SimHistories contained in the Model.  If Names are provided, the number of named OpLines must equal the number of SimHistories contained in the Model as this count is used to size the matrix the generated from the 'extern C' function in Rcpp.}
}

\value{
Returns a dataframe containing columns for Time and Duration for each change to the system status.  A matrix of 1's (operating) and 0's (not operating) identify the status of each OpLine through this detailed history.
}

\references{
Carazas et. al.,"Availability Analysis of Gas Turbines Used in Power Plants",International Journal of Thermodynamics, Vol. 12 (No.1), March 2009 
}

\examples{
GT_1 <- EventElement("GasTurbine2",1,101,"W",2562.5,0.95,0, "L",1.4,0.86,0,87)
seedVec<-GT_1[,length(GT_1)]
GT_2<-cbind(GT_1[,-length(GT_1)],"Seed"=seedVec*11)
GT_3<-cbind(GT_1[,-length(GT_1)],"Seed"=seedVec*22) 
\dontrun{
GT_1_sh<-SimHistory(GT_1)
GT_2_sh<-SimHistory(GT_2)
GT_3_sh<-SimHistory(GT_3)
Model<-list(GT_1_sh, GT_2_sh, GT_3_sh)
Names<-c("GT1", "GT2", "GT3")
TurbineArray<-DetailOpLines(Model,Names)
}
}

\keyword{ model }

