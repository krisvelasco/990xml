## This script converts IRS Form 990 XML files into a single file R data frame.
## XML files can be downloaded from: https://www.irs.gov/charities-non-profits/form-990-series-downloads
## Must have the XML, xml2, plyr, dplyr packages.

library(XML)
library(xml2)
library(plyr)
library(dplyr)


############################################################################################################
###############################################2015#########################################################
############################################################################################################

## Downloading ZIP from IRS website
temp <- tempfile()
download.file("https://apps.irs.gov/pub/epostcard/990/xml/2015/download990xml_2015_1.zip",temp)
unzip(temp, exdir="~/Desktop/IRS XML temp 2015")

## save list of all files as vector
all.files <- list.files(pattern="\\.xml", path="~/Desktop/IRS XML temp 2015", full.names=TRUE)


## I am splitting the list of files into 7 chunks. This is because there are roughly ~65,000 XMLs in each zip file 
## and my local computer has problems processing more than 10,000 at once.
## If your local computer/server can process more files at once (or if you are only wanting a more limited set of variables, 
## then this can be altered)

chunk_number <- 7
all.files.split <- split(all.files,             # Applying split() function
                         cut(seq_along(all.files),
                             chunk_number,
                             labels = FALSE))

# Now, I will process each of the seven chunks and save them as .CSVs.
## In the following sequence, I'm converting the XMLs into dataframes
### First, I am getting rid of repeated column names
### Second, I am retaining variables of interest.

dfList <- lapply(all.files.split$`1`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit but this is the common naming structure for variables I collected)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  ## This is the set of code that is most important to adjust depending on one's research projects. Once opening XML files in a NoteEditor-type application,
  ## you will be able to see the naming conventions for each variable and how they are applied here.
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# Combine data frames saved in dfList
## Note, sometimes you'll get an error if a particular XML was not able to be processed correctly.
## If that happens, use the following code to delete that record from the list
## dfList[[1]] <- NULL
df_all <- bind_rows(dfList)

#Exporting as .csv
write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_1.csv")


# Now, I am just repeating this sequence for all remaining "chunks" from the 2015_1 zip from the IRS website.
# Then, I will repeat for each zip file on the IRS website (e.g., 2015_1.zip, 2015_2.zip)

dfList <- lapply(all.files.split$`2`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# combine data frames saved in dfList
df_all <- bind_rows(dfList)

#Exporting as .csv
write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_2.csv")


## loop through all files, read and parse XML, append df to dfList
dfList <- lapply(all.files.split$`3`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# combine data frames saved in dfList
df_all <- bind_rows(dfList)

write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_3.csv")

## loop through all files, read and parse XML, append df to dfList
dfList <- lapply(all.files.split$`4`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# combine data frames saved in dfList
df_all <- bind_rows(dfList)

write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_4.csv")


## loop through all files, read and parse XML, append df to dfList
dfList <- lapply(all.files.split$`5`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# combine data frames saved in dfList
df_all <- bind_rows(dfList)

write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_5.csv")


## loop through all files, read and parse XML, append df to dfList
dfList <- lapply(all.files.split$`6`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# combine data frames saved in dfList
df_all <- bind_rows(dfList)

write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_6.csv")


## loop through all files, read and parse XML, append df to dfList
dfList <- lapply(all.files.split$`7`, function(x){
  x <- read_html(x)
  doc = htmlParse(x)
  
  # creates list of nodes
  xml_list <- xmlToList(doc)
  names(xmlRoot(doc))
  
  # returns unnested data frame to store in list
  df <- as.data.frame(do.call(rbind, lapply(xml_list, unlist)))
  # remove duplicate column names (e.g., Schedule O supplemental info explanatory text)
  df <- df[!duplicated(colnames(df))]
  
  # example truncation of variable names (can further edit)
  colnames(df) <- gsub('return.returnheader.|return.returndata.|irs990|form990|irs990ez','', colnames(df))
  colnames(df) <- gsub('additionalinformation.|additionalfilerinformation.|supplementalinformationdetail.','', colnames(df))
  colnames(df) <- gsub('preparerfirmgrp.|.attrs.|referencedocument|.form990||^[.\\]','', colnames(df))
  
  # save only required variables (%in$ allows loop to continue even if column missing for particular obs)
  df <- df[, names(df) %in% c('returntypecd','@returnversion','returnts', 'taxperiodenddt', 'grossreceiptsamt', 'filer.usaddress.addressline1txt','filer.usaddress.citynm', 
                              'filer.usaddress.stateabbreviationcd','filer.usaddress.zipcd','taxyr', 'filer.ein',
                              'formationyr','legaldomicilestatecd','activityormissiondesc', 'filer.businessname.businessnameline1txt',
                              'filer.businessname.businessnameline2txt','expenseamt','missiondesc','schedulef.totalspentamt',
                              'totalemployeecnt', 'totalvolunteerscnt', 'cycontributionsgrantsamt','cyprogramservicerevenueamt',
                              'cytotalrevenueamt', 'netassetsorfundbalanceseoyamt', 'totalfunctionalexpensesgrp.fundraisingamt',
                              'totalfunctionalexpensesgrp.managementandgeneralamt','totalfunctionalexpensesgrp.programservicesamt',
                              'totalfunctionalexpensesgrp.totalamt', 'governmentgrantsamt', 'noncashcontributionsamt', 'allothercontributionsamt',
                              'fundraisingamt', 'form990partviisectionagrp.fundraisingamt','form990partviisectionagrp.governmentgrantsamt',
                              'form990partviisectionagrp.allothercontributionsamt', 'form990partviisectionagrp.noncashcontributionsamt',
                              'form990partviisectionagrp.totalcontributionsamt', 'form990partviisectionagrp.membershipduesamt',
                              'primaryexemptpurposetxt', 'contributionsgiftsgrantsetcamt', 'totalrevenueamt','otherexpensestotalamt',
                              'totalexpensesamt', 'programservicerevenueamt', 'membershipduesamt', 'fundraisinggrossincomeamt',
                              'programsrvcaccomplishmentgrp.totalprogramserviceexpensesamt', 'programsrvcaccomplishmentgrp.descriptionprogramsrvcaccomtxt',
                              'programsrvcaccomplishmentgrp.programserviceexpensesamt','desc', 'expenseamt','progsrvcaccomacty2grp.expenseamt',
                              'progsrvcaccomacty2grp.desc','progsrvcaccomacty3grp.expenseamt','progsrvcaccomacty3grp.desc')]
  
  return(df)
})   

# combine data frames saved in dfList
dfList[[679]] <- NULL
df_all <- bind_rows(dfList)

write.csv(df_all,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015/XML_990_2015_1_7.csv")

#######################################################################################################################################################################
### Note, at this point, you now repeat for each zip file within the IRS XML site. You can even create a large loop that repeats this process.

#Combining all 2015 CSVs into a single dataset
setwd("~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/2015")
xml990_2015 <- ldply(list.files(), read.csv, header=TRUE)
write.csv(xml990_2015,"~/Documents/Research Projects/International 990s/foreign990/Datasets/IRS 990 XMLs/Combined/xml990_2015.csv")

########################################################################################################################


