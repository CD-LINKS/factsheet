 
require(openxlsx)

wb <- createWorkbook()


# Analyse Scenario Submissions --------------------------------------------


tt <- all %>% filter (Scope == "national") %>% group_by(model, scenario ) %>% summarize (value = any(value)) %>%  ungroup()  %>% spread(key= model, value = value)
tt[is.na(tt) ] = FALSE

addWorksheet(wb, sheetName = "national scenarios")
writeDataTable(wb, "national scenarios", tt)

tt.nscenarios <- tt



tt <- all %>% filter (Scope == "global") %>% group_by(model, scenario ) %>% summarize (value = any(value)) %>%  ungroup()  %>% spread(key= model, value = value)
tt[is.na(tt) ] = FALSE

addWorksheet(wb, sheetName = "global scenarios")
writeDataTable(wb, "global scenarios", tt)

tt.gscenarios <- tt


# Analyse Variable Submissions ----------------------------------------------------

tt <- all %>% group_by(model, variable ) %>% summarize (value = any(value)) %>%  ungroup()  %>% spread(key= model, value = value)
tt[is.na(tt) ] = FALSE
# addWorksheet(tt, file = "SubmissionOverview.xlsx",sheetName = "Variables")

addWorksheet(wb, sheetName = "Variables")
writeDataTable(wb, "Variables", tt)

tt.variables <- tt
rm(tt)

saveWorkbook(wb,"SubmissionOverview.xlsx", overwrite = TRUE)