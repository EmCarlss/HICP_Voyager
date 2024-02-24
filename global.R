library(eurostat)
library(dplyr)
library(stringr)
hikp_coicop <- get_eurostat("prc_hicp_midx", filters = list(geo = "DE", unit = "I15"))
hicp_coicop2 <- label_eurostat(hikp_coicop, "coicop", eu_order = TRUE, code = c("coicop", "geo"))
coicop_set <- select(hicp_coicop2, coicop_code, coicop)
coicop_set <- unique(coicop_set)
coicop_set$code_label <- paste(coicop_set$coicop_code, coicop_set$coicop, sep = " ")

coicop_set_hierarchy <- coicop_set[grepl("^CP\\d{2,5}$", coicop_set$coicop_code), ]
coicop_set_hierarchy$level <- nchar(gsub("\\D", "", coicop_set_hierarchy$coicop_code))
coicop_set_hierarchy$level <- ifelse(coicop_set_hierarchy$coicop_code == "CP00", 1, nchar(gsub("\\D", "", coicop_set_hierarchy$coicop_code)))
coicop_set_hierarchy$parent_code <- ifelse(coicop_set_hierarchy$level > 2, substr(coicop_set_hierarchy$coicop_code, 1, nchar(coicop_set_hierarchy$coicop_code) - 1), NA)
coicop_set_hierarchy$parent_code <- ifelse(coicop_set_hierarchy$level == 2 & coicop_set_hierarchy$coicop_code != "CP00", "CP00", ifelse(coicop_set_hierarchy$level > 2, substr(coicop_set_hierarchy$coicop_code, 1, nchar(coicop_set_hierarchy$coicop_code) - 1), NA))
coicop_set_hierarchy$parent_code<-  ifelse(coicop_set_hierarchy$coicop_code == "CP0820", "CP08",coicop_set_hierarchy$parent_code)
coicop_set_hierarchy$parent_code<-  ifelse(coicop_set_hierarchy$coicop_code == "CP0830", "CP08",coicop_set_hierarchy$parent_code)

label_set<-select(coicop_set_hierarchy, coicop_code, code_label)

clean_eurostat_cache()

new_plot_data<-FALSE
new_rebased_data<-FALSE
new_plot_ar_data<-FALSE
new_plot_w_data<-FALSE

#print(paste("Global newplotdata:",new_plot_data))