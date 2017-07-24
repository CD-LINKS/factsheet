# World
ShowGHGReductionRelToScenario("economy-wide", "World")
z2 <- z
z2$region <- "World"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "World")
z2 <- z
z2$region <- "World"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "World")
z2 <- z
z2$region <- "World"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "World")
z2 <- z
z2$region <- "World"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "World")
z2 <- z
z2$region <- "World"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "World")
z2 <- z
z2$region <- "World"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Brazil
ShowGHGReductionRelToScenario("economy-wide", "BRA")
z2 <- z
z2$region <- "BRA"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "BRA")
z2 <- z
z2$region <- "BRA"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "BRA")
z2 <- z
z2$region <- "BRA"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "BRA")
z2 <- z
z2$region <- "BRA"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "BRA")
z2 <- z
z2$region <- "BRA"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "BRA")
z2 <- z
z2$region <- "BRA"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# China
ShowGHGReductionRelToScenario("economy-wide", "CHN")
z2 <- z
z2$region <- "CHN"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "CHN")
z2 <- z
z2$region <- "CHN"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "CHN")
z2 <- z
z2$region <- "CHN"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "CHN")
z2 <- z
z2$region <- "CHN"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "CHN")
z2 <- z
z2$region <- "CHN"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "CHN")
z2 <- z
z2$region <- "CHN"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# India
ShowGHGReductionRelToScenario("economy-wide", "IND")
z2 <- z
z2$region <- "IND"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "IND")
z2 <- z
z2$region <- "IND"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "IND")
z2 <- z
z2$region <- "IND"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "IND")
z2 <- z
z2$region <- "IND"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "IND")
z2 <- z
z2$region <- "IND"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "IND")
z2 <- z
z2$region <- "IND"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Japan
ShowGHGReductionRelToScenario("economy-wide", "JPN")
z2 <- z
z2$region <- "JPN"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "JPN")
z2 <- z
z2$region <- "JPN"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "JPN")
z2 <- z
z2$region <- "JPN"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "JPN")
z2 <- z
z2$region <- "JPN"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "JPN")
z2 <- z
z2$region <- "JPN"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "JPN")
z2 <- z
z2$region <- "JPN"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# EU
ShowGHGReductionRelToScenario("economy-wide", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# Russia 
ShowGHGReductionRelToScenario("economy-wide", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "EU")
z2 <- z
z2$region <- "EU"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)

# USA
ShowGHGReductionRelToScenario("economy-wide", "USA")
z2 <- z
z2$region <- "USA"; z2$sector <- "economy-wide"
z_total <- z2
ShowGHGReductionRelToScenario("energy_supply", "USA")
z2 <- z
z2$region <- "USA"; z2$sector <- "energy_supply"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("industry", "USA")
z2 <- z
z2$region <- "USA"; z2$sector <- "industry"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("buildings", "USA")
z2 <- z
z2$region <- "USA"; z2$sector <- "buildings"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("transport", "USA")
z2 <- z
z2$region <- "USA"; z2$sector <- "transport"
z_total <- rbind(z_total, z2)

ShowGHGReductionRelToScenario("AFOLU", "USA")
z2 <- z
z2$region <- "USA"; z2$sector <- "AFOLU"
z_total <- rbind(z_total, z2)