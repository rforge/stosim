// stosim.h file
 /*
 * Author: Jacob T. Ormerod
 *         (c) 2014 OpenReliability.org
 */

#ifndef _stosim_H
#define _stosim_H

#include <Rcpp.h>

RcppExport SEXP withStorage(SEXP a, SEXP b, SEXP c);

RcppExport   SEXP SimulationHistory(Rcpp::NumericVector& OpLine_Vec,
                                    Rcpp::NumericVector& Event_ID_Vec,
                                    Rcpp::NumericVector& FD_Vec,
                                    Rcpp::NumericVector& FP1_Vec,
                                    Rcpp::NumericVector& FP2_Vec,
                                    Rcpp::NumericVector& FP3_Vec,
                                    Rcpp::NumericVector& RD_Vec,
                                    Rcpp::NumericVector& RP1_Vec,
                                    Rcpp::NumericVector& RP2_Vec,
                                    Rcpp::NumericVector& RP3_Vec,
                                    Rcpp::IntegerVector& Seed_Vec,
                                    Rcpp::NumericVector& SimulationYears);

 RcppExport  SEXP addWpush(SEXP arg1, SEXP arg2, SEXP arg3,
      SEXP arg4,  SEXP arg5, SEXP arg6,
        SEXP arg7,  SEXP arg8);

  RcppExport  SEXP addOverlay(SEXP arg1, SEXP arg2, SEXP arg3,
      SEXP arg4,  SEXP arg5, SEXP arg6,
        SEXP arg7,  SEXP arg8);
        
   RcppExport  SEXP DetailOpLines(SEXP arg1, SEXP arg2, SEXP arg3,
      SEXP arg4); 
      
   RcppExport  SEXP MultiTrainSingleBU(SEXP arg1, SEXP arg2, SEXP arg3,
      SEXP arg4,  SEXP arg5, SEXP arg6,
        SEXP arg7,  SEXP arg8);          

#endif
