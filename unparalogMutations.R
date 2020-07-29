# #<---------------------------->
# #  Author : Deepankar Chakroborty
# #  URL: https://gitlab.utu.fi/deecha
# #  Report issues: https://gitlab.utu.fi/deecha/shared_scripts/-/issues
# #  Please include this when distributing my code. 
# #  In the gene column in your SNV annotation if you see something like:
# #  e.g. PRAMEF7;PRAMEF8  OR  PRAMEF7,PRAMEF8
# #       then your mutations annotations have gene paralogs.
# #
# #  This script aims to de-couple those paralogs into individual their rows.
# #
# #<---------------------------->

### Info:
# Assign correct paralog_separator found in the gene column of your SNV annotations # e.g. if the Gene column has entries like PRAMEF7;PRAMEF8 
# then the paralog_separator is ";" 
# or set it to whatever separator is used by your SNV annotation software.

# Assign correct annotation_separator in the Amino acid change column of your SNV annotations
# "PRAMEF8:NM_001012276:exon3:c.C541A:p.Q181K,PRAMEF7:NM_001012277:exon3:c.C541A:p.Q181K"
# In the above example the annotation_separator is ","

# GeneColName = Column name in the SNV annotation table where the Genes are listed

# AnnotationColName = Column name in the SNV annotation table where the Amino acid changes are listed

# <--------------->

unparalog <- function(DATA, paralog_separator = ";", annotation_separator = ",", GeneColName , AnnotationColName ){
   # Installing missing dependencies
   dependencies <- c("stringi", "progress","data.table")
   missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
   if(length(missing_packages)) install.packages(missing_packages)      
   
   # Sanity checks
   check_paralog_sep <- !any(stringi::stri_detect_fixed(str = DATA$Gene.refGene,pattern = paralog_separator))
   check_annotation_sep <- !any(stringi::stri_detect_fixed(str =  DATA$AAChange.refGene, pattern = annotation_separator))
   
   idx_Gene <- which(colnames(DATA) == GeneColName)
   idx_Annotation <- which(colnames(DATA) == AnnotationColName)
   
   if(length(idx_Gene) == 0 | length(idx_Annotation) == 0 | check_paralog_sep | check_annotation_sep) {
      message(stringi::stri_c("You entered :-->",
                              "\nparalog_separator = ",paralog_separator,
                              "\nannotation_separator = ",annotation_separator,
                              "\nGeneColName = ", GeneColName,
                              "\nAnnotationColName = ",AnnotationColName))
   stop("Inconsistencies with these input parameters.\n Ensure they are correct and try again.")
   }
   # Cleanup
   rm(missing_packages,dependencies,check_paralog_sep,check_annotation_sep)
   gc()
   
   current.idx <- 1 # nrow(DATA)+1
   paralog.idx <- which(stringi::stri_detect_fixed(str = DATA$Gene.refGene,pattern = paralog_separator))
   pb <- progress::progress_bar$new(total=length(paralog.idx),format = " [:bar] :current/:total (:percent)",); pb$tick(0)
   
   Number_of_paralogs <- sum(stringi::stri_count_fixed(str = DATA$Gene.refGene,pattern = paralog_separator))
   message(stringi::stri_c("There are ",length(paralog.idx)," annotations with ", Number_of_paralogs," paralogs."))
   
   # copying structure of original DATA
   DATA.new <- DATA[1,]; DATA.new <- DATA.new[-1,] 
   
   # Creating empty table which will be populated in the for loop
   DATA.new <- dplyr::bind_rows(DATA.new,data.frame(matrix(nrow = (length(paralog.idx)+Number_of_paralogs), ncol = ncol(DATA),dimnames = list(c(),colnames(DATA))),stringsAsFactors = F))
   
   # Beginning isolation of the paralogs
   for(i in paralog.idx){
      Muts <- unlist(stringi::stri_split_fixed(DATA$AAChange.refGene[i],annotation_separator),use.names = F,recursive = F)
      for (gene in unlist(stringi::stri_split_fixed(DATA$Gene.refGene[i],pattern = paralog_separator),use.names = F,recursive = F)){
         DATA.new[current.idx,] <- DATA[i,]
         DATA.new$Gene.refGene[current.idx] <- gene
         DATA.new$AAChange.refGene[current.idx] <- paste0(Muts[grep(gene,Muts,fixed = T)],collapse=annotation_separator)
         current.idx <- current.idx+1
      }
      pb$tick(1)
   }
   # removing the original rows with the paralogs as they are all unparalogged now
   DATA <- DATA[-paralog.idx,]
   DATA <- dplyr::bind_rows(DATA,DATA.new)
   rm(DATA.new) ; gc()
   
   rownames(DATA) <- as.character(seq(1,length(DATA[,1])))
   return (DATA)
}
