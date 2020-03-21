library(tidyverse)
library(readxl)

static_variables_list <- c(
  "CountryCodes",
  # "Overall",
  "Prevent",
  "Detect",
  "Respond",
  "Health",
  "Norms",
  "Risk",
  "GDP_bill",
  "GDP_percapita",
  "Population_mill",
  "HumanDevelopmentIndex_2018",
  "EIUDemocracyIndexScore_2019",
  "UNOnlineServicesIndexScore_2018",
  "GlobalPeaceIndex",
  "CorruptionsPerceptionIndex_2018",
  "HumanCapitalIndex_2017",
  "SDGIndexScore_2018"
)

for(i in 1:length(static_variables_list)){
  var_name <- static_variables_list[i]
  if(var_name %in% c(  "Prevent", "Detect", "Respond", "Health", "Norms", "Risk")){
    var_name_with_prefix <- paste0("GHS_",var_name)
  }else{
    var_name_with_prefix <- var_name
  }
  assign(var_name, read_excel("GHSindex_data_static.xlsx", sheet = var_name, col_names = F))
  if(var_name == "CountryCodes"){
    assign(var_name, eval(parse(text = var_name)) %>% rename("ISO3" = "...1") )
    assign(var_name, eval(parse(text = var_name)) %>% rename("FullName" = "...2") )
  }else{
    assign(var_name, eval(parse(text = var_name)) %>% rename("FullName" = "...1") )
    assign(var_name, eval(parse(text = var_name)) %>% rename(!!var_name_with_prefix := "...2") )
  }
  
  if(i == 1){
    merge_all <- eval(parse(text = static_variables_list[i]))
  }
  if(i > 1){
    merge_all <- merge(merge_all, eval(parse(text = static_variables_list[i])), by = "FullName")
  }
}

dim(merge_all)


