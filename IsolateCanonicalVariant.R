# Installing dependencies
dependencies <- c("stringi", "doParallel")
missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages) 
rm(missing_packages,dependencies)

IsolateCanonicalVariant <- function (AAchangeAnnotations){
# #<---------------------------->
# # You must include this section when:
# # Distributing, Using and/or Modifying this code. 
# # Please read and abide by the terms of the included LICENSE.
# # Copyright 2020, Deepankar Chakroborty, All rights reserved.
# #
# #  Author : Deepankar Chakroborty (https://github.com/dchakro)
# #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
# #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE
# #<---------------------------->


# #  PURPOSE:
# #  From a given vector of annotations for a particular DNA change
# #  this function selects the canonical variant (if present)
# #  by cross referencing the MANE Select and RefSeq Select sets.

# #  LOGIC FLOW:
# #  - If there is only one annotation; that is selected
# #  - If canonical transcript is not found in MANE Select + RefSeq select 
# #     or a matching transcript ID is not found in the annotation then;
# #     The mutation with to the the highest position (residue number) is selected.
# #  - If a match for canonical isoform is found then; 
# #        that particular mutation is selected
  
  # importing resources
  library(doParallel)
  refseq <- readRDS(url("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/asset/RefSeqSelect_Gene_Transcript.RDS"),"rb")
  source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/MutSiteFind.R")
  
  # initializing cluster
  myCluster <- makeCluster(parallel::detectCores(), 
                           type = "FORK",
                           useXDR=F,
                           .combine=cbind)
  registerDoParallel(myCluster)
  print(myCluster)
  
  file.create("log.txt")
  message(paste0("Logging processed mutations at: ",getwd(),"/log.txt"))
  
  # computation
  results <- foreach(MutInfo = AAchangeAnnotations,.combine = c) %dopar% {
    GENE <- can.isoform <- l <- l2 <- NA
    l=unique(unlist(
      stringi::stri_split_fixed(
        str = MutInfo,
        pattern = ","),
      use.names = F,
      recursive = F))
    
    if(length(l)==1){
      l2 <- unlist(stringi::stri_split_fixed( 
          str = l, 
          pattern = ":"),
        use.names = F,
        recursive = F)
      MUTATION <- stringi::stri_replace_first_fixed(
        str = l2[stringi::stri_detect_regex(
          str = l2, 
          use.names = F, 
          pattern = "^p\\.")],
        pattern = "p.",
        replacement = "")
    } else {
      GENE <- stringi::stri_sub(
        str = l[1],
        from = 1,
        to = stringi::stri_locate_first_fixed(
          str = l[1],
          pattern = ":")[,"end"]-1)
      
      can.isoform <- refseq$transcript_accession[refseq$gene_id==GENE]
      
      if(length(can.isoform)==0 | !any(stringi::stri_detect_fixed(str = l,pattern = can.isoform))){
        # Canonical isoform not found in RefSeq, or Match for Canonical isoform not found in AAchangeAnnotations
        l2 <- unlist(
          stringi::stri_split_fixed( 
            str = l, 
            pattern = ":"),
          use.names = F,
          recursive = F)
        
        l3 <- stringi::stri_replace_all_fixed(
          str = l2[stringi::stri_startswith_fixed(
            str = l2,
            "p.")],
          pattern = "p.",
          replacement = "")
        
        MUTATION <- l3[which.max(MutSiteFind(l3))]
      } else {
        # Canonical isoform found
        l=l[grep(can.isoform,l)]
        
        l2 <- unlist(
          stringi::stri_split_fixed( 
            str = l, 
            pattern = ":"),
          use.names = F,
          recursive = F)
        
        MUTATION <- stringi::stri_replace_first_fixed(
          str = l2[stringi::stri_detect_regex(
            str = l2, 
            use.names = F, 
            pattern = "^p\\.")],
          pattern = "p.",
          replacement = "")
      }
    }
    if(length(MUTATION)==0){
      MUTATION <- NA
    }
    cat(paste(MUTATION,"\n"),file="log.txt", append=T)
    return(MUTATION)
  }
  stopCluster(myCluster)
  return(results)
}
