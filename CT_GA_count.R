

CT_GA_count <- function(SampleID,Ref_Base,Alt_Base){
# #<---------------------------->
# # You must include this section when:
# # Distributing, Using and/or Modifying this code.
# # Please read and abide by the terms of the included LICENSE.
# # Copyright 2020, Deepankar Chakroborty, All rights reserved.
# #
# #  Author : Deepankar Chakroborty (https://gitlab.utu.fi/deecha)
# #  Report issues: https://gitlab.utu.fi/deecha/shared_scripts/-/issues
# #  License: https://gitlab.utu.fi/deecha/shared_scripts/-/blob/master/LICENSE
# #<---------------------------->
  
# # PURPOSE:
# # This function takes in three vectors:
# #     SampleID = Sample IDs
# #     Ref_Base = Reference Base
# #     Alt_Base = altered base that created the mutation.

# #  And calculates the number of C > T and G > A changes are there (per sample)
# #  The function returns a data frame listing the number of mutations (per sample):
# #    SampleID = Sample ID
# #    Total = Total number of mutations
# #    CT = C > T changes
# #    GA = G > A changes
# #    Others = all other types of transitions and transversions combined.

  MutMatrix <- data.frame(SampleID,
                          Ref_Base,
                          Alt_Base,
                          stringsAsFactors = F)
  
  return.df <- data.frame(SampleID=NA,
                          Total=0,
                          CT=0,
                          GA=0,
                          Others=0)
  
  for(SampleID in levels(MutMatrix$SampleID)){
    set  <-  MutMatrix[ MutMatrix$SampleID == SampleID, ]

    # if(dim(set)[1]==0){
    #   return.df <- rbind.data.frame(return.df,c(SampleID,0,0,0,0))
    #   next
    # }
    
    Total <- dim(set)[1]
    
    CT <- dim(subset(set, set$Ref_Base == "C" & set$Alt_Base == "T"))[1]
    
    GA <- dim(subset(set, set$Ref_Base == "G" & set$Alt_Base == "A"))[1]
    
    Others=Total-CT-GA
    
    return.df <- rbind.data.frame(return.df,list(SampleID,Total,CT,GA,Others),stringsAsFactors = F)
    
    Total <- 0;CT <- 0;GA <- 0;Others <- 0 # re-initialize
    rm(set)
  }
  return(return.df[-1,]) # Removes the first empty row
}
