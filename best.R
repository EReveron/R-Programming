best <- function(state, outcome) {
## Read outcome data
    dt_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Create a Outcome Desease Data Table
   
    outcome_names <- function(outcome) {

	if (grepl(".from.",outcome)) {
		## Get the string before ".from."

		x <- as.data.frame(strsplit(as.character(outcome),".from."))[2,]

		## Substitute "." by " " and put in lowercase
		x <- tolower(gsub("\\."," ",x[1]))

         	return(x)

	}
	else
		{return(NA)}
    }    
   
    outcome_dt_names <- unique(lapply(colnames(dt_outcome),outcome_names))
	
    ### Remove NA values

    outcome_dt_names <- outcome_dt_names[!(is.na(outcome_dt_names))]

## Check that outcome are valid
   
    if (!(outcome %in% outcome_dt_names)) 
	{ stop("invalid outcome") }



## Check that state and outcome are valid
   
    if (!(state %in% dt_outcome$State)) 
	{ stop("invalid state") }


## Convert the outcome to the form of the Data Table

	capwords <- function(s, strict = FALSE) {
     		cap <- function(s) paste(toupper(substring(s, 1, 1)),
                              {s <- substring(s, 2); if(strict) tolower(s) else s},
                              sep = "", collapse = " " )
     	sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
 	}

	print(outcome.name)
	outcome.name <- gsub(" ","\\.",capwords(outcome))
	print(outcome.name)
	outcome.name <- paste("Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.",outcome.name,sep ="")

	dt_outcome[outcome.name]
	
	print(outcome.name)

## Return hospital name in that state with lowest 30-day death
## rate
}
