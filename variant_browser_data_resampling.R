vb_data_samp <- lapply(cohort_list, function(x){
                x <- x[sample(c(1:nrow(x)),size = nrow(x)*0.25),]
                return(x)
              })

