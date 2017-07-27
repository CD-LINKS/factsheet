create_graphs <- 0

# World
z2 <- ShowGHGReductionRelToScenario("economy wide", "World", create_graphs)
z2$region <- "World"; z2$sector <- "economy wide"
z_total <- z2

z2 <- ShowGHGReductionRelToScenario("energy supply", "World", create_graphs)
z2$region <- "World"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "World", create_graphs)
z2$region <- "World"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "World", create_graphs)
z2$region <- "World"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "World", create_graphs)
z2$region <- "World"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "World", create_graphs)
z2$region <- "World"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Brazil
z2 <- ShowGHGReductionRelToScenario("economy wide", "BRA", create_graphs)
z2$region <- "BRA"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "BRA", create_graphs)
z2$region <- "BRA"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "BRA", create_graphs)
z2$region <- "BRA"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "BRA", create_graphs)
z2$region <- "BRA"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "BRA", create_graphs)
z2$region <- "BRA"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "BRA", create_graphs)
z2$region <- "BRA"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# China
z2 <- ShowGHGReductionRelToScenario("economy wide", "CHN", create_graphs)
z2$region <- "CHN"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "CHN", create_graphs)
z2$region <- "CHN"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "CHN", create_graphs)
z2$region <- "CHN"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "CHN", create_graphs)
z2$region <- "CHN"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "CHN", create_graphs)
z2$region <- "CHN"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "CHN", create_graphs)
z2$region <- "CHN"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# India
z2 <- ShowGHGReductionRelToScenario("economy wide", "IND", create_graphs)
z2$region <- "IND"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "IND", create_graphs)
z2$region <- "IND"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "IND", create_graphs)
z2$region <- "IND"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "IND", create_graphs)
z2$region <- "IND"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "IND", create_graphs)
z2$region <- "IND"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "IND", create_graphs)
z2$region <- "IND"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Japan
z2 <- ShowGHGReductionRelToScenario("economy wide", "JPN", create_graphs)
z2$region <- "JPN"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "JPN", create_graphs)
z2$region <- "JPN"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "JPN", create_graphs)
z2$region <- "JPN"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "JPN", create_graphs)
z2$region <- "JPN"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "JPN", create_graphs)
z2$region <- "JPN"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "JPN", create_graphs)
z2$region <- "JPN"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# EU
z2 <- ShowGHGReductionRelToScenario("economy wide", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Russia 
z2 <- ShowGHGReductionRelToScenario("economy wide", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "EU", create_graphs)
z2$region <- "EU"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# USA
z2 <- ShowGHGReductionRelToScenario("economy wide", "USA", create_graphs)
z2$region <- "USA"; z2$sector <- "economy wide"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("energy supply", "USA", create_graphs)
z2$region <- "USA"; z2$sector <- "energy supply"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("industry", "USA", create_graphs)
z2$region <- "USA"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("residential and commercial", "USA", create_graphs)
z2$region <- "USA"; z2$sector <- "residential and commercial"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("transportation", "USA", create_graphs)
z2$region <- "USA"; z2$sector <- "transportation"
z_total <- rbind(z_total, z2)

z2 <- ShowGHGReductionRelToScenario("AFOLU", "USA", create_graphs)
z2$region <- "USA"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

z_table <- xtable(z_total, digits=c(0,0,2,2,8,0,0)) 
fname_html = paste("Reductions per model by 2030", Sys.Date(),"_", gsub(":", "_", strftime(Sys.time(), format="%H:%M:%S")), ".html")
print.xtable(z_table, type="html", file=paste("graphs/paper/",fname_html))