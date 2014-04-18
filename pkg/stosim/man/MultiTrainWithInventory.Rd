\name{MultiTrainWithInventory}
\alias{MultiTrainWithInventory}

\title{ Event Handler for Evaluating Multi Train Systems with Backup Inventory Support }

\description{
This function will evaluate a dataframe created by DetailOpLines on a line by line basis to generate
a history of production output based on various constraints and limitations of the overall system.
}

\usage{
MultiTrainWithInventory(ModelDetail, CapacityHrs, ReserveHrs, RefillHrs, TurndownLimit=0, TurndownTime=1, ShowProgress=FALSE) 
}

\arguments{
\item{ModelDetail}{ A dataframe constructed by the DetailOpLines function.}
\item{CapacityHrs}{ A quantity of inventory in terms of hours of single active train production defining
 the useable capacity of a storage vessel.}
\item{ReserveHrs}{ A quantity of inventory in terms of hours of single active train production at which
 an operating policy will be implemented to preserve remaining inventory for support of  subsequent
 active train failures.}
\item{RefillHrs}{ The duration in hours that would be required to refill the entire capacity of a storage vessel
 utilizing a given accumulator of known capability.}
\item{TurndownLimit}{ A fraction of total production at which downstream processing can continue to function
 at minimum rates. This value defaults to zero, which is most permissive.} 
\item{TurndownTime}{ The duration in hours required to bring the downstream processing unit down to a new level
 of production, including potential human dialog time for confirmation of intent. This value defaults at 1 hour,
 which has proven to be a very typical of process plant systems requiring human confirmation of turndown request.} 
\item{ShowProgress}{ A boolean value indicating whether a progress bar should be displayed during execution
 of the function.} 
}

\value{
Returns a list containing two dataframes. The first containing columns for Time, Duration, and ProdRate for each
 total production rate state.  A matrix of 1's and 0's  identify the occurrence of specific production failures by
 cause. These causes may be discharge capacity exceeded, inventory run out, or demand on empty storage.
}

\references{
Ormerod JT, "Using Stochastic RAM Analysis to Establish an Optimal Operating Policy
",8th IMA International Conference on Modelling in Industrial Maintenance and Reliability (MIMAR), July 2014
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
ChgTwInv<-MultiTrainWithInventory(ChargeTrainParallelHistory, 16, 8, 336)	
}

\keyword{ model }

