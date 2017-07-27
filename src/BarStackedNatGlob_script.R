source("functions/CDLINKS_StackedBarContinuousX.R")

#stackedbar plots
regs = c("BRA", "CHN", "IND", "RUS", "EU", "JPN", "USA")

vars <- c("Primary Energy",
          "Primary Energy|Coal|w/o CCS",
          "Primary Energy|Coal|w/ CCS",
          "Primary Energy|Oil|w/o CCS",
          "Primary Energy|Oil|w/ CCS",
          "Primary Energy|Gas|w/o CCS",
          "Primary Energy|Gas|w/ CCS",
          "Primary Energy|Biomass|w/o CCS",
          "Primary Energy|Biomass|w/ CCS","Primary Energy|Nuclear",
          "Primary Energy|Hydro","Primary Energy|Wind","Primary Energy|Solar",
          "Primary Energy|Geothermal",
          "Primary Energy|Other",
          "Primary Energy|Secondary Energy Trade")

ylab <-  "Primary Energy [EJ/yr]"
titletag <- "PE"
file.prefix <- "PE-"

scensglob = c("NPi2020_1000")
catsnat <- c( "NPi","INDC",  "2030_high", "2020_high",   "2030_low", "2020_low")

tt = 2050

dtg <- filter(all, scenario %in% scensglob, region %in% regs, variable %in% vars, period == tt, Scope == "global") %>%
  mutate(model.cat = model) %>% factor.data.frame()

dtg$model.cat  <- plyr::mapvalues(dtg$model.cat, levels(dtg$model.cat),plotstyle(levels(dtg$model.cat), out = "legend") %>% as.character())

dtg <- mutate(dtg, model.cat = paste0(model.cat, "_2C" )) %>% factor.data.frame()


dtn <- filter(all, Category %in% catsnat, region %in% regs, variable %in% vars, period == tt, Scope == "national")

dtn$model <- plyr::mapvalues(dtn$model,
                             c("*AIM/Enduse 3.0", "*COPPE-MSB_v2.0", "*DNE21+ V.14 (national)", "*India MARKAL",
                               "*RU-TIMES 3.2", "*China TIMES", "*IPAC-AIM/technology V1.0",
                               "*AIM/Enduse[Japan]", "*PRIMES_V1"),
                             c("AE-IIM-", "MSB","DNE", "IM-TERI",
                               "RUT", "CHT","IPAC", "AE-JPN","PRI" ))
dtn <- order.levels(dtn, Category = catsnat) %>% arrange (variable, Category)

dtn <- dtn %>%
  unite(model.cat, model, Category, remove = F)

dtn$model.cat <- factor(dtn$model.cat, unique(dtn$model.cat))

dt = rbind(dtn, dtg) %>% factor.data.frame()

dt =  order.levels(dt, variable  = vars, Category = c(catsnat, setdiff(levels(dt$Category), catsnat)), model = unique(dt$model), Scope = c("national", "global"))  %>%
  arrange (  model, variable, Category, Scope)




# FIXME: temporarily blank CHT_2030_low
dt <- filter(dt, ! (model.cat %in% c("CHT_2030_low", "CHT_2030_high")))


for (reg in regs)
{
  x = filter(dt, region == reg, variable %in% vars) %>% factor.data.frame()
  x <- mutate(x, barwidth = ifelse(Scope == "national", 0.9, 0.55))
  x <- mutate(x, x.pos = ifelse(Scope == "national", as.numeric(model.cat),
                                as.numeric(model.cat) + 0.5 - 0.4*(as.numeric(model.cat)- filter(x, Scope == "national") %>% getElement("model.cat") %>% unique() %>% length()  )))

  p = CDLINKS_StackedBarDiscreteX(x = filter(x, variable != vars[1]), total = filter(x, variable == vars[1]),
                                 x.dim = "model.cat", ylab = ylab) +
    scale_x_continuous(breaks = unique(x$x.pos),
                       labels = unique(x$model.cat)) +
    theme_bw(base_size = 10)+
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))

  ggsave(file=paste0("plots/BarStackedNatGlob_",file.prefix,"_",reg,as.character(tt),cfg$format), width=11, height=9.5, dpi=300, units  = "cm")
}


#stackedbar plots
regs = c("BRA", "CHN", "IND", "RUS", "EU", "JPN", "USA")


vars <- c(
  "Emissions|CO2|Energy",
  "Emissions|CO2|Energy|Supply",
  "Emissions|CO2|Energy|Demand|Industry",
  #          "Emissions|CO2|Industrial Processes",
  "Emissions|CO2|Energy|Demand|Residential and Commercial",
  "Emissions|CO2|Energy|Demand|Transportation",
  #          "Emissions|CO2|Energy|Solids",
  #          "Emissions|CO2|Energy|Liquids",
  #          "Emissions|CO2|Energy|Gases",
  # "Carbon Sequestration|CCS|Biomass",
  "Emissions|CO2|AFOLU"
)



ylab <-  "CO2 Emissions [GtCO2/yr]"
titletag <- "CO2 Energy"
file.prefix <- "CO2Ene-"

scensglob = c("NPi2020_1000")
catsnat <- c( "NPi","INDC",  "2030_high", "2020_high",   "2030_low", "2020_low")

tt = 2050

dtg <- filter(all, scenario %in% scensglob, region %in% regs, variable %in% vars, period == tt, Scope == "global") %>%
  mutate(model.cat = model) %>% factor.data.frame()

dtg$model.cat  <- plyr::mapvalues(dtg$model.cat, levels(dtg$model.cat),plotstyle(levels(dtg$model.cat), out = "legend") %>% as.character())

dtg <- mutate(dtg, model.cat = paste0(model.cat, "_2C" )) %>% factor.data.frame()


dtn <- filter(all, Category %in% catsnat, region %in% regs, variable %in% vars, period == tt, Scope == "national")

dtn$model <- plyr::mapvalues(dtn$model,
                             c("*AIM/Enduse 3.0", "*COPPE-MSB_v2.0", "*DNE21+ V.14 (national)", "*India MARKAL",
                               "*RU-TIMES 3.2", "*China TIMES", "*IPAC-AIM/technology V1.0",
                               "*AIM/Enduse[Japan]", "*PRIMES_V1"),
                             c("AE-IIM-", "MSB","DNE", "IM-TERI",
                               "RUT", "CHT","IPAC", "AE-JPN","PRI" ))
dtn <- order.levels(dtn, Category = catsnat) %>% arrange (variable, Category)

dtn <- dtn %>%
  unite(model.cat, model, Category, remove = F)

dtn$model.cat <- factor(dtn$model.cat, unique(dtn$model.cat))

dt = rbind(dtn, dtg) %>% factor.data.frame()
dt =  order.levels(dt, variable  = vars, Category = c(catsnat, setdiff(levels(dt$Category), catsnat)), model = unique(dt$model), Scope = c("national", "global"))  %>%
  arrange (  model, variable, Category, Scope) %>% mutate (value = value *1e-3)



# FIXME: temporarily blank CHT_2030_low
# dt <- filter(dt, ! (model.cat %in% c("CHT_2030_low", "CHT_2030_high")))


for (reg in regs)
{
  x = filter(dt, region == reg, variable %in% vars) %>% factor.data.frame()
  x = order.levels(x,variable  = vars, Category = c(catsnat, setdiff(levels(x$Category), catsnat)), model = unique(x$model), Scope = c("national", "global"))  %>%
    arrange (  model, variable, Category, Scope)


  x <- mutate(x, barwidth = ifelse(Scope == "national", 0.9, 0.55))
  x <- mutate(x, x.pos = ifelse(Scope == "national", as.numeric(model.cat),
                                as.numeric(model.cat) + 0.5 - 0.4*(as.numeric(model.cat)- filter(x, Scope == "national") %>% getElement("model.cat") %>% unique() %>% length()  )))

  p = CDLINKS_StackedBarDiscreteX(x = filter(x, variable != vars[1]), total = filter(x, variable == vars[1]),
                                 x.dim = "model.cat", ylab = ylab) +
    scale_x_continuous(breaks = unique(x$x.pos),
                       labels = unique(x$model.cat)) +
    theme_bw(base_size = 10)+
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))

  ggsave(file=paste0("plots/BarStackedNatGlob_",file.prefix,"_",reg,as.character(tt),cfg$format), width=11, height=10, dpi=300, units  = "cm")
}

