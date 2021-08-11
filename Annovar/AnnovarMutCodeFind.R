annovarMutCodeFind=function(MutationColumn,isoform){
# #<---------------------------->
# # You must include this section when:
# # Distributing, Using and/or Modifying this code.
# # Please read and abide by the terms of the included LICENSE.
# # Copyright 2020, Deepankar Chakroborty, All rights reserved.
# #
# #  Author : Deepankar Chakroborty (https://github.com/dchakro)
# #  Website: https://www.dchakro.com
# #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
# #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE
# #<---------------------------->

# # PURPOSE:
# # This function takes a mutation info column from ANNOVAR output 
# # and selects the protein change based on the isoform provided.
# # More about annovar: https://en.wikipedia.org/wiki/ANNOVAR

# # INPUT:
# # MutationColumn = Mutation column from ANNOVAR output 
# #                  (usually called: "AAChange.refGene")
# # isoform = the GenBank Identifier, you can find it from the the ANNOVAR mutation info column.
# # Default Isoform codes:
# # EGFR  = NM_005228
# # ERBB2 = NM_004448
# # ERBB3 = NM_001982
# # ERBB4 (Jm-A, Cyt-1) =  NM_005235
# # ERBB4 (Jm-A, Cyt-2) =  NM_001042599

# # Example usage:
# # annovarMutCodeFind(MutationColumn = yourData$AAChange.refGene,
# #  isoform = "NM_005228")

  MutationList=c("List of mutations")
  for(i in seq(1:length(MutationColumn))){
    MutInfo=MutationColumn[i]
    l=sort(unique(unlist(strsplit(MutInfo,","))))
    l2=l[grep(isoform,l)]
    l2.s=unique(unlist(strsplit(l2,":")))
    l3=l2.s[grep("^p",l2.s)]
    l3=gsub("p.","",l3)
    l3=gsub("X","*",l3)
    MUTATION=l3
    if(length(MUTATION)==0) MUTATION=" "
    MutationList=c(MutationList,MUTATION)
  }
  return(MutationList[-1])
}
