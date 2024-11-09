# Randomisation and sampling results for screening

library(synthesisr)
library(dplyr) 
refs <- read_refs("searching/deduplicated_results_incl_cchasing.txt")

#randomise refs
refs <- refs[sample(1:nrow(refs)), ]
#make 2 sets and the rest
consistency_check1 <- refs[1:150,]
consistency_check2 <- refs[151:300,]
rest <- refs[301:20632,]

#export
consistency_check1_ris <- write_refs(consistency_check1, format="ris")
consistency_check2_ris <- write_refs(consistency_check2, format="ris")
consistency_rest <- write_refs(rest, format="ris")
writeLines(consistency_check1_ris, "screening/consistency_check1.txt")
writeLines(consistency_check2_ris, "screening/consistency_check2.txt")
writeLines(consistency_rest, "screening/rest.txt")
