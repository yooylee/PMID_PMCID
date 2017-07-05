# 1. Load R packages (rentrez and plyr)

library("rentrez")
library("plyr")

# 2. Set your working directory

# 3. Load csv file and create a data frame

PMID_PMCID <- read.csv("exportforlinkoutinPubMed.csv", header=TRUE)

# 4. Create two colums with empty value: One for PMID and the other for PMCID

PMID_PMCID$pmid <- " "
PMID_PMCID$pmcid <- " "

# 5. Function to get Pubmed ID

myPubmedID <- function(x) {
  Pubmed <- entrez_search(db="pubmed", term=x)
  Pubmed_ID <- Pubmed$ids
  
  if(length(Pubmed_ID) == 1 ) {
    PMID <- Pubmed_ID
  } else {
    PMID <- NULL
  }
  
  return(PMID)
}

# 6. Get Pubmed ID

for (i in 1:nrow(PMID_PMCID)) {
  
  if (is.null(myPubmedID(as.character(PMID_PMCID$doi[i])))) {
    PMID_PMCID$pmid[i] <- "NULL"
  } else {
    PMID_PMCID$pmid[i] <- myPubmedID(as.character(PMID_PMCID$doi[i]))
  }
  
}

## 7: Issue & Solution from Step 6.
### Issue: The function of myPubmedID returns one Pubmed ID, but associated one. 
### Solution: Identify duplicated ID values and remove NULL value

duplicated_IDs <- data.frame(PMID_PMCID$pmid[duplicated(PMID_PMCID$pmid)])
# This returns n-1
colnames(duplicated_IDs) <- c("value")
# Change the column name
duplicated_IDs <- subset(duplicated_IDs, value != 'NULL')
# Remove NULL value
duplicated_IDs <- data.frame(count(duplicated_IDs))

## 7-1: If the freq is more than 1,

PMID_PMCID <- within(PMID_PMCID, pmid[pmid==24313031] <- c("NULL"))
# Repeat this process 

## 7-2: If the freq is equal to 1, check manually. Sometimes, they are correct PMID. 
# If not, do the process of 7-1.

# 8: Function to get Pubmed Central ID

myPMCID <- function(x) {
  taxize_summ <- entrez_summary(db="pubmed", id=x)
  PMC <- data.frame(taxize_summ$articleids)
  
  PMC_ID <- grep("pmc-id:", PMC$value, value=TRUE)
  
  if (identical(PMC_ID, character(0))) {
    PMCID <- NULL
  } else {
    PMCID <- PMC_ID
  }
  
  return(PMCID)
}

# 9: Get Pubmed Central ID

for (i in 1:nrow(PMID_PMCID)) {
  if (PMID_PMCID$pmid[i] == "NULL") {
    PMID_PMCID$pmcid[i] <- "NULL"
  } else {
    if (is.null(myPMCID(PMID_PMCID$pmid[i]))) {
      PMID_PMCID$pmcid[i] <- "NULL"
    } else {
      PMID_PMCID$pmcid[i] <- myPMCID(PMID_PMCID$pmid[i])
    }  
  }
}

# 10: Save the file in the csv format

write.table(PMID_PMCID, file="PMID_PMCID.csv", sep=",", row.names=F)


