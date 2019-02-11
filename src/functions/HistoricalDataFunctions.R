CreateEmptyIEA_energy <- function(IEA_energy, regions_iea, iea_flows, iea_products)
{
  # region, FLOW, PRODUCT, period
  periods <- sort(unique(IEA_energy$period)) 
  #iea_tfc_flows <- c("TFC", "TOTIND", "TOTTRANS", "RESIDENT", "COMMPUB",
  #                   "ELMAINE","ELAUTOE", "ELMAINC", "ELAUTOC", 
  #                   "HEMAINH", "HEMAINC", "HEAUTOH", "HEAUTOC")
  #iea_products <- c("GEOTHERM", "SOLARPV", "SOLARTH", "WIND", "OTHER", 
  #                  "PRIMSBIO", "CHARCOAL", "BIOGASES", "BIODIESEL", "BIOGASOL", "BIOJETKERO", "OBIOLIQ", "INDWASTE", "MUNWASTEN", "MUNWASTER",
  #                  "NUCLEAR", "HYDRO",
  #                  "ELECTR", "HEAT", 
  #                  "TOTAL")
  iea_full_products <- unique(IEA_energy$PRODUCT)
  iea_full_flows <- unique(IEA_energy$FLOW)
  dim_tfc <- length(regions_iea)*length(iea_flows)*length(iea_products)*length(periods)
  period <- rep(periods, length.out=dim_tfc)
  PRODUCT <- rep(iea_products, each=length(periods), length.out=dim_tfc)
  FLOW <- rep(iea_flows, each=length(periods)*length(iea_products), length.out=dim_tfc)
  region <- rep(regions_iea, each=length(periods)*length(iea_products)*length(iea_flows), length.out=dim_tfc)
  IEA_empty <- cbind(region, FLOW) %>% cbind(PRODUCT) %>% cbind(period)
  IEA_empty <- as.data.frame(IEA_empty)

  #IEA_empty$period <- as.numeric(IEA_empty$period)
  IEA_empty$period <- as.integer(trimws(IEA_empty$period))  
  IEA_empty$PRODUCT <- factor(PRODUCT, levels=iea_full_products)
  IEA_empty$FLOW <- factor(FLOW, levels=iea_full_flows)
  IEA_empty$region <- factor(IEA_empty$region, levels=unique(IEA_energy$region))
  # add other columns, same as IEA_energy
  # region, FLOW, PRODUCT, period, value, unit, ISO3, IMAGE, TIMER_CARRIER
  IEA_empty <- mutate(IEA_empty, value=0) %>% mutate(unit="TJ")
  IEA_empty$unit <- factor(IEA_empty$unit)
  IEA_empty <- select(IEA_empty, region, FLOW, PRODUCT, period, value, unit)
  IEA_empty <- as.data.frame(IEA_empty)
}



ConvertPRIMAP2IAM <- function(data, CATEGORY="CAT0", ENTITY="KYOTOGHGAR4", variable="Emissions|Kyoto Gases")
{
  data <- filter(data, category%in%CATEGORY, entity%in%ENTITY)
  data <- select(data, scenario, region, category, entity, unit, num_range("X", 1990:2015))
  colnames(data) = gsub("X", "", colnames(data))
  data <- gather(data, 6:ncol(data), key="period", value=value)
  data$value <- data$value/1000
  data <- group_by(data, scenario, region, entity, unit, period) %>%
          summarise(value=sum(value))
  data <- mutate(data, Category="Historical") %>% mutate(Baseline="") %>% mutate(model="PRIMAP") %>% mutate(Scope="") %>%
          mutate(variable=variable)
  data$scenario <- ""
  data <- ungroup(data)
  data$period <- as.numeric(as.character(data$period))
  data <- select(data, scenario, Category, Baseline, model, region, period, Scope, value, unit, variable)
  data <- as.data.frame(data)
}

ConvertIEA2IAM <- function(data, flow="TFC", product="TOTAL", variable="Final Energy")
{ 
  data <- filter(data, FLOW%in%flow, PRODUCT%in%product) %>% 
              group_by(region, period, unit) %>%
              summarise(value=sum(value)) %>%
              mutate(Category="Historical") %>% mutate(scenario="") %>% mutate(Baseline="") %>% mutate(model="IEA") %>% mutate(Scope="") %>%
              mutate(variable=variable)  %>%
              select(scenario, Category, Baseline, model, region, period, Scope, value, unit, variable)
  data$value <- data$value/1000
  data$unit <- "EJ/yr"
  data <- as.data.frame(data)
}