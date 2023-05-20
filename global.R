library(eurostat)
library(dplyr)
hikp_coicop <- get_eurostat("prc_hicp_midx", filters = list(geo = "DE", unit = "I15"))
hicp_coicop2 <- label_eurostat(hikp_coicop, "coicop", eu_order = TRUE, code = c("coicop", "geo"))
coicop_set <- select(hicp_coicop2, coicop_code, coicop)
coicop_set <- unique(coicop_set)
coicop_set$code_label <- paste(coicop_set$coicop_code, coicop_set$coicop, sep = " ")
clean_eurostat_cache()

new_plot_data<-FALSE
new_rebased_data<-FALSE
print(paste("Global newplotdata:",new_plot_data))