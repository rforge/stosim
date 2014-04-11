\name{MultiTrainWithInventory}
\alias{MultiTrainWithInventory}

\title{ Event Handler for Evaluating Multi Train Systems with Backup Inventory Support }

\description{
This function will evaluate a dataframe created by DetailOpLines on a line by line basis to generate
a history of production output based on various constraints and limitations of the overall system.
}

\usage{
MultiTrainWithInventory(ModelDetail, ReserveHrs, CapacityHrs, RefillHrs, TurndownLimit=NULL, TurndownTime=NULL, ShowProgress=FALSE) 
}

\arguments{
\item{ModelDetail}{ A dataframe constructed by the DetailOpLines function.}
\item{ReserveHrs}{ A quantity of inventory in terms of hours of single active train production at which
 an operating policy will be implemented to preserve remaining inventory for support of  subsequent
 active train failures.}
\item{CapacityHrs}{ A quantity of inventory in terms of hours of single active train production defining
 the useable capacity of a storage vessel.}
\item{RefillHrs}{ The duration in hours that would be required to refill the entire capacity of a storage vessel
 utilizing a given accumulator of known capability.}
\item{TurndownLimit}{ A fraction of total production at which downstream processing can continue to function
 at minimum rates. This value is fixed at (number of active trains - 1)/number of active trains for the C++
 function currently called.} 
\item{TurndownTime}{ The duration in hours required to bring the downstream processing unit down to a new level
 of production, including potential human dialog time for confirmation of intent. This value is fixed at 1 hour
 for the C++ function currently called.} 
\item{ShowProgress}{ A boolean value indicating whether a progress bar should be displayed during execution
 of the function.} 
}

\value{
Returns a list containing two dataframes. The first containing columns for Time, Duration, and ProdRate for each
 total production rate state.  A matrix of 1's and 0's  identify the occurrence of specific production failures by
 cause. These causes may be discharge capacity exceeded (SuddenDrop), inventory RunOut, or demand on empty storage.
}

\references{
Carazas et. al.,"Availability Analysis of Gas Turbines Used in Power Plants",International Journal of Thermodynamics, Vol. 12 (No.1), March 2009
}

\examples{
data(ChargeTrain)	
Train1<-ChargeTrain		
Train2<-Train1[,-11]	
Train2seeds<-data.frame(Seed=Train1$Seed+200)	
Train2<-cbind(Train2,Train2seeds)	
Train3<-Train2[,-11]	
Train3seeds<-data.frame(Seed=Train2$Seed+200)	
Train3<-cbind(Train3,Train3seeds)	
Train1_sh<-SimHistory(Train1)	
Train2_sh<-SimHistory(Train2)	
Train3_sh<-SimHistory(Train3)	
ChargeSystem<-list(Train1_sh,Train2_sh,Train3_sh)	
ChargeTrainParallelHistory<-DetailOpLines(ChargeSystem)	
ChgTwInv<-MultiTrainWithInventory(ChargeTrainParallelHistory, 8, 16, 336)	
}

\keyword{ model }

