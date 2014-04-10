


#include <R.h>
#include <Rcpp.h>
#include "stosim.h"


 SEXP MultiTrainSingleBU4a(SEXP arg1, SEXP arg2, SEXP arg3,
      SEXP arg4,  SEXP arg5, SEXP arg6,
        SEXP arg7,  SEXP arg8)

{     using namespace Rcpp;

//	src <- '			
// set dataframe arguments from R into Rcpp vector classes for use in C++	
	Rcpp::NumericVector inTime(arg1);
	Rcpp::NumericVector inDuration(arg2);
	Rcpp::NumericVector GenLevel(arg3);
	Rcpp::NumericVector CapacityHrs(arg4);
	Rcpp::NumericVector ServiceHrs(arg5);
	Rcpp::NumericVector RefillHrs(arg6);
	Rcpp::NumericVector NumTrains(arg7);
	Rcpp::NumericVector ReserveHrs(arg8);
// initialize the output object vectors	
// avoid multiple function calls to get the same info	
	double onesize = inTime.size();
	Rcpp::NumericVector outTime(onesize);
	Rcpp::NumericVector outDuration(onesize);
	Rcpp::NumericVector outProd(onesize);
	Rcpp::IntegerVector RunDown(onesize);
	Rcpp::IntegerVector RunOut(onesize);
	Rcpp::IntegerVector SuddenDrop(onesize);
	Rcpp::NumericVector outStartLevel(onesize);
	Rcpp::NumericVector outEndLevel(onesize);
	Rcpp::NumericVector wsdEndLevel(onesize);
// make first entries into out vectors	
	outTime[0]=0.0;
	outDuration[0]=0.0;
	outProd[0]=GenLevel[0];
	RunDown[0]=0;
	RunOut[0]=0;
	SuddenDrop[0]=0;
	wsdEndLevel[0]=CapacityHrs[0];
	outStartLevel[0]=CapacityHrs[0];
	outEndLevel[0]=CapacityHrs[0];
// separate pointer to the out vectors	
	int y=1;
	int x=1;
//Initialize a special test values	
	bool MoreThanOneTrainDown=false;
	double ThisCaseServiceHrs=ServiceHrs[0];
// Initialize Refill activity variables	
//bool RefillinProgress=false;	
//The StartOfStorageRefill time setting will fall out of accuracy at times.  It is only valid when RefillinProgress=TRUE	
//double StartOfStorageRefill=0.0;	
// needed for <>At Repair Case, because covered events may not have this info	
	double LastLevel=CapacityHrs[0];
	double StartLevel=LastLevel;
	bool ApparentBackupSuccess=false;
// function scope initialization will be needed for Rcpp	
	double ThisEventTime=0.0;
// begin a line by line loop starting at line 2 of OpLineDetail	
	for(x=1;  x < onesize; x++)  {
	
	
	
//<> AtFailure condition	
// if this number of plants operating is less than last event number of plants operating	
// if(GenLevel[x]<outProd[y-1])  {  // Hideous bug!	
	if(GenLevel[x]<GenLevel[x-1])  {
// Needed for new entry	
	StartLevel=LastLevel;
	
// it is not certain this has to be done here,	
	ApparentBackupSuccess=false;
// but it is possible for a TRUE setting to reach this location	
// having just been proven FALSE	
	
	
	
	
// No longer calculate storage level based on refill here, this is done At Repair	
	
	
	
	
	
	
// Since we are done with intended action on RefillinProgress variable it will be reset now	
//RefillinProgress=false;	
	
// pre-establish an important part of logic	
	MoreThanOneTrainDown=false;
	//if(GenLevel[x]<(NumTrains[0]-1)/NumTrains[0])  {
     // GenLevel[x] may be a truncated repeat like 0.66666667, hence it tests poorly against a new calc in C++                                                
     if((GenLevel[x]*NumTrains[0])<(NumTrains[0]-1.0001))  {                                                
	 MoreThanOneTrainDown=true;
 	 }
	
// It is noted at this location that the pending test with comparison to thisCaseServiceHrs	
// is testing for the backup system being in service due to coverage of the previous event	
// therefore it is appropriate NOT to alter thisCaseServiceHrs or this event at this point in sequence	
// this is handled in the else block, since it can only be altered if a sudden drop event did not occur	
	
	
// Two possible cases end up being processed much the same way	
// Case1:  backup still in progress from a previous event	
// Case 2:  demand meets empty storage (perhaps more accurate if the test is for <1 hr left in storage)	
	
	    if(MoreThanOneTrainDown&&((inTime[x]-inTime[x-1])< ThisCaseServiceHrs||StartLevel < 1.0))  {
// customer demand stops, storage level will no longer deplete, log the sudden product supply loss event at this time	
 // log the sudden product supply loss event at this time	
// Fill in Last Event Log values that we now know	
	outDuration[y-1]=inTime[x]-outTime[y-1];
	
	
	
// No longer are we backfilling LastLevel, we know this on each case now	
	
	
	LastLevel=0.0;
// R code seemed to go a long way about identifying inDuration[x-1], which is  known	
	if((inDuration[x])<ThisCaseServiceHrs)  {
	LastLevel=StartLevel-inDuration[x-1];
	}
	
	
	
//enter the new elements to the out vectors, as far as we know	
// no backup support available, it is possible that Product goes to zero here due to customer failure!!!!	
	outTime[y]=inTime[x];
	outDuration[y]=0.0;
	outProd[y]=GenLevel[x];
	
	
	RunDown[y]=0;
	RunOut[y]=0;
	SuddenDrop[y]=1;
	
	
	wsdEndLevel[y]=LastLevel;
	y++;
	
	
	
	}else{
// not sudden, will storage cover?	
// ServiceHrs may depend on GenLevel, but can only be changed on a non-SuddenDrop event	
	//if(MoreThanOneTrainDown&&Event2Consumption)   {
      if(MoreThanOneTrainDown)   {
    if(StartLevel > ReserveHrs[0]+ServiceHrs[0]) {
        ThisCaseServiceHrs=StartLevel-ReserveHrs[0];
        }else{
        if(StartLevel > ServiceHrs[0]){
                ThisCaseServiceHrs=ServiceHrs[0];
            }else{
                ThisCaseServiceHrs=StartLevel;
            }
        }
    }                                                      
	//ThisCaseServiceHrs=StartLevel;}
	
//thisCaseServiceHrs will remain at this value until reset At Repair	
// Actually the first program flow control here should regard a fully successful backup	
	if(inDuration[x]<ThisCaseServiceHrs&&inDuration[x]<StartLevel)  {
// All we do is record the LastLevel for the next loop	
// No entry for successful backup	
	LastLevel=StartLevel-inDuration[x];
// need to send this to the next line evaluation, because there could have been a third train fail	
	ApparentBackupSuccess=true;
	}else{
// Storage will  run for thisCaseServiceHrs, or run out before then	
	if(StartLevel>ThisCaseServiceHrs)  {
	ThisEventTime=inTime[x]+ThisCaseServiceHrs ;
	LastLevel=StartLevel-ThisCaseServiceHrs;
	RunDown[y]=1;
	RunOut[y]=0;
	}else{
	ThisEventTime=inTime[x]+StartLevel ;
	LastLevel=0.0;
	RunDown[y]=0;
	RunOut[y]=1;
	}
// in any event there is a line entry to be made	
// Fill in Last Event Log values that we now know	
// must know what this event time is	
	outDuration[y-1]=ThisEventTime-outTime[y-1];
// No longer are we backfilling LastLevel, we know this on each case now	
	
	
// now fill in rest of entries to this line of OpldWs	
// Build a new line for this event in the OpldWs dataframe, as far as we know	
	outTime[y]=ThisEventTime;
	outDuration[y]=0.0;
	outProd[y]=GenLevel[x];
// RunDown has already been handled above	
// RunOut has already been handled above	
	SuddenDrop[y]=0;
	
	wsdEndLevel[y]=LastLevel;
	y++;
	}
	}
	
	
	
	
	
// <> At Repair condition	
	}else{
// Needed for new entry	
	StartLevel=LastLevel;
	
	
	if(GenLevel[x]==1)  {
// RefillinProgress=true;	
// StartOfStorageRefill=inTime[x];	
	
// Needed for new entry	
	if (StartLevel + inDuration[x] * CapacityHrs[0]/RefillHrs[0]  > CapacityHrs[0]) {
	LastLevel = CapacityHrs[0];
	}else{ 
	LastLevel = StartLevel + inDuration[x] * CapacityHrs[0]/RefillHrs[0]; 
	}
	ThisCaseServiceHrs=ServiceHrs[0];
	}
	
// note that if GenLevel is <1 LastLevel is unchanged	
	
	if(!ApparentBackupSuccess)  {
// Fill in Last Event Log values that we now know	
	outDuration[y-1]=inTime[x]-outTime[y-1];
// No longer are we backfilling LastLevel, we know this on each case now	
	
	
// Build a new line for this event in the OpldWs dataframe, as far as we know	
	outTime[y]=inTime[x];
	outDuration[y]=0.0;
	outProd[y]=GenLevel[x];
	RunDown[y]=0;
	RunOut[y]=0;
	SuddenDrop[y]=0;
	
	
	
	wsdEndLevel[y]=LastLevel;
	y++;
	}
	
	
// Note there is no entry or other action for a BackupSuccess	
// Reset the ApparentBackupSuccess variable	
	ApparentBackupSuccess=false;
	
	
	}
// Before returning to the main loop make entries for the new Levels dataframe here	
	outStartLevel[x]=StartLevel;
	outEndLevel[x]=LastLevel;
	
// return to the main loop	
	}
// With bug in termination of loop fixed,  y-1 is the last row of wsd	
	int last_y=y-1;
// x should have persisted beyond the loop since it was declared in function scope	
// It is not obvious that it would have been advanced, so it is helpful to clarify it here	
	int last_x=onesize-1;
// complete the last duration	
	outDuration[last_y]=inTime[last_x]+inDuration[last_x]-outTime[last_y];
// No longer are we backfilling LastLevel, we know this on each case now	
	
	Rcpp::DataFrame NDF1 =
	Rcpp::DataFrame::create(Rcpp::Named("Time")=outTime,
	                  Rcpp::Named("Duration")=outDuration,
	                  Rcpp::Named("Product")=outProd,
	                  Rcpp::Named("RunDown")=RunDown,
	                  Rcpp::Named("RunOut")=RunOut,
	                  Rcpp::Named("SuddenDrop")=SuddenDrop,
	                  Rcpp::Named("EndLevel")=wsdEndLevel);
	
	Rcpp::DataFrame NDF2 =
	Rcpp::DataFrame::create(Rcpp::Named("StartLevel")=outStartLevel,
	                  Rcpp::Named("EndLevel")=outEndLevel);
	
	Rcpp::List L=
	Rcpp::List::create(NDF1,NDF2);
	
	return(L);

//	'
}
		
