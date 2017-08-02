# 1. Load R packages (rentrez and plyr)

library("rentrez")
library("plyr")

# 2. Set your working directory

# 3. Load csv file and create a data frame

PMID_PMCID <- read.csv("FileName.csv", header=TRUE)

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

PMID_PMCID <- within(PMID_PMCID, pmid[pmid==23104645] <- c("NULL"))
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



# 10: Delete all rows with PMID value NULL
PMID_PMCID <- PMID_PMCID[ which(PMID_PMCID$pmid != "NULL"), ]

# 11: Delete all rows with PMCID value not NULL
PMID_PMCID <- PMID_PMCID[ which(PMID_PMCID$pmcid=="NULL"), ]

# 12:  Load csv file of items under embargo and create a data frame
Embargo <- read.csv("FileName.csv", header=TRUE)

# 13: Identify items under Embargo and change column name
items_under_embargo <- data.frame(intersect(PMID_PMCID$id, Embargo$ItemID))
colnames(items_under_embargo) <- c("ID")

# 14: Remove items under Embargo from PMID_PMCID data table
PMID_PMCID <- subset(PMID_PMCID, !(id %in% items_under_embargo$ID), select=id:pmcid)

# 15: Final Report
PMID_PMCID$ProviderID <- "YourID"
PMID_PMCID$Database <- "PubMed"
PMID_PMCID$IconURL <- "IconURL"
PMID_PMCID$UrlName <- "Full Text from"
PMID_PMCID$SubjectType <- " "
PMID_PMCID$Attribute <- "Full-text PDF"

PMID_PMCID <- subset(PMID_PMCID, select=c(ProviderID, Database, pmid, handle, IconURL, UrlName, SubjectType, Attribute))
colnames(final) <- c("ProviderID", "Database", "UID", "URL", "IconURL", "UrlName", "SubjectType", "Attribute")

# 16: Save the file in the csv format

write.table(PMID_PMCID, file="PMID_PMCID.csv", sep=",", row.names=F)

# 17: Identify items under embargo but released soon
items_under_embargo_from_PMID_PMCID <- subset(PMID_PMCID, (id %in% items_under_embargo$ID), select=id:pmcid)
items_under_embargo_from_Embargo <- subset(Embargo, (ItemID %in% items_under_embargo$ID), select=c(ItemID,Embargo))
colnames(items_under_embargo_from_Embargo) <- c("id", "Embargo")

# 18: Combine two tables by id
All_Embargo <- merge(items_under_embargo_from_PMID_PMCID, items_under_embargo_from_Embargo, by=c("id"))

# 19: Remove items under indefinite embargo
Removed_under_indefinite_embargo <- All_Embargo[ which(All_Embargo$Embargo!="9999-01-01"), ]

# 20: Save the file of items under definite embargo
write.table(Removed_under_indefinite_embargo, file="ItemsUnderEmbargo.csv", sep=",", row.names=F)

