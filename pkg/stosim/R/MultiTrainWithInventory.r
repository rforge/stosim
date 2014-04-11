MultiTrainWithInventory<-function(ModelDetail, ReserveHrs, CapacityHrs, RefillHrs, TurndownLimit=NULL, TurndownTime=NULL, ShowProgress=FALSE) {			
	  OutputDF1=NULL		
	  OutputDF2=NULL		
	## adjustment of input arguments to match MultiTrainSingleBU4a		
	ServiceHrs<-CapacityHrs-ReserveHrs		
	ReserveHrs<-0		
	## note TurndownLimit is effectively 1 train down with ReserveHrs=0		
	## TurndownTime is fixed at 1 hr		
	  ## since the first column of the input file is the Page entry we find the last one		
	  Pages=ModelDetail[length(ModelDetail[,1]),1]		
	  		
	  ##if(ShowProgress==TRUE)  {		
	      ##pb <- tkProgressBar(title = "MultiTrainWithInventory Progress", min = 0,		
	               ##max = Pages, width = 300)		
	    ##}		
	    nextPageStarts<-1		
	    startTime<-proc.time()		
	    		
	    for(p in 1:Pages)  {		
	    		
	    		
	    	thisPageStarts<-nextPageStarts	
	    	nextPageStarts=NULL	
	    	if(p<Pages) {	
	    		nextPage<-match(p+1,ModelDetail[,1])
	    		##nextPageStarts<-c(nextPageStarts,nextPage)
	    		## not sure this variable was needed in this simpler case than OpDetail Dev required
	    		nextPageStarts<-nextPage
	    	}else{	
	    		## this makes the last page entry work
	    		nextPage=length(ModelDetail[,1])+1
	    	}	
	    myMat<-as.matrix(ModelDetail[thisPageStarts:(nextPage-1),4:ncol(ModelDetail)])		
	    GenLevel<-rowMeans(myMat)		
	    DurationVec<-ModelDetail$Duration[thisPageStarts:(nextPage-1)]		
	    TimeVec<-ModelDetail$Time[thisPageStarts:(nextPage-1)]		
	    NumTrains<-ncol(ModelDetail)-3		
	    		
	    ## this is the call to the C++ dll in the stosim library		
	      RcppList<-.Call("MultiTrainSingleBU4a",TimeVec, DurationVec, GenLevel, CapacityHrs, ServiceHrs , RefillHrs,		
	       NumTrains,ReserveHrs, PACKAGE="stosim")		
	    		
	   		
	    lastline<-match(max(RcppList[[1]][,1]),RcppList[[1]][,1])		
	    RcppList[[1]]<-RcppList[[1]][1:lastline,]		
	    Page<-rep(p,length(RcppList[[1]][,1]))		
	    PageCol<-data.frame(Page)		
	    RcppList[[1]]<-cbind(PageCol,RcppList[[1]])		
	    OutputDF1<-rbind(OutputDF1,RcppList[[1]])		
	    		
	    Page<-rep(p,length(RcppList[[2]][,1]))		
	    PageCol<-data.frame(Page)		
	    RcppList[[2]]<-cbind(PageCol,RcppList[[2]])		
	    OutputDF2<-rbind(OutputDF2,RcppList[[2]])		
	    rm(RcppList)		
	    		
	    		
	    		
	    		
	    		
	    if(p==1)  { 		
	      oneCycle<-proc.time()		
	      TimeTest<-(oneCycle[3]-startTime[3])*Pages		
	      ## still need to find the correct TimeTest value		
	      if(TimeTest > .5)  {		
	      pb <- tkProgressBar(title = "MultiTrainSingleBU Progress", min = 0,		
	               max = Pages, width = 300)		
	      ShowProgress=TRUE  }		
	      }		
	      if(ShowProgress==TRUE)  {		
	      setTkProgressBar(pb, p, label=paste( round(p/Pages*100, 0),"% done"))		
	    }		
	    		
	  ## return for next page		
	  }		
	  		
	  if(ShowProgress==TRUE)  {		
	  close(pb)		
	  }		
	  		
	  OutputList<-list(OutputDF1,OutputDF2)		
   return(OutputList)  			
}			
