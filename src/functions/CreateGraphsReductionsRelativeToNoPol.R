library(xtable)

source("functions/check_policy_implementation_function.R")

create_graphs <- 1
Scen1 <- "NPi"
Scen2 <- "NoPOL"
Year1 <- 2030
Year2 <- 2030

# World
z2 <- ShowGHGReductionRelToScenario("economy wide", "World", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "World"; z2$sector <- "economy wide"
z_total <- z2

z2 <- ShowGHGReductionRelToScenario("energy supply", "World", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "World"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "World", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "World"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "World", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "World"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "World", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "World"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "World", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "World"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Brazil
z2 <- ShowGHGReductionRelToScenario("economy wide", "BRA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "BRA"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "BRA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "BRA"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "BRA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "BRA"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "BRA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "BRA"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "BRA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "BRA"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "BRA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "BRA"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# China
z2 <- ShowGHGReductionRelToScenario("economy wide", "CHN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "CHN"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "CHN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "CHN"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "CHN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "CHN"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "CHN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "CHN"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "CHN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "CHN"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "CHN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "CHN"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# India
z2 <- ShowGHGReductionRelToScenario("economy wide", "IND", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "IND"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "IND", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "IND"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "IND", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "IND"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "IND", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "IND"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "IND", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "IND"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "IND", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "IND"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Japan
z2 <- ShowGHGReductionRelToScenario("economy wide", "JPN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "JPN"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "JPN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "JPN"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "JPN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "JPN"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "JPN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "JPN"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "JPN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "JPN"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "JPN", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "JPN"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# EU
z2 <- ShowGHGReductionRelToScenario("economy wide", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Russia 
z2 <- ShowGHGReductionRelToScenario("economy wide", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "EU", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "EU"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# USA
z2 <- ShowGHGReductionRelToScenario("economy wide", "USA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "USA"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "USA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "USA"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "USA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "USA"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "USA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "USA"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "USA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "USA"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "USA", Scen1, Scen2, Year1, Year2, create_graphs)
z2$region <- "USA"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

z_table <- xtable(z_total, digits=c(0,0,2,2,8,0,0)) 
fname_html = paste("Reductions per model by 2030", Sys.Date(),"_", gsub(":", "_", strftime(Sys.time(), format="%H:%M:%S")), ".html")
print.xtable(z_table, type="html", file=paste("graphs/paper/",fname_html))