

#### processing function
process_data <- function(all,scens){
    # move years from rows to a new column
    all <- invisible(melt(all,measure.vars=names(all)[grep("[0-9]+",names(all))],variable.name = "period",variable.factor=FALSE))
    all$period <- as.numeric(all$period)
    all <- all[!period %in% c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2001,2002,2003,2004,2006,2007,2008,2009,2011,2012,2013,2014,2016,2017,2018,2019,2021,2022,2023,2024,2026,2027,2028,2029,2031,2032,2033,2034,2036,2037,2038,2039,2041,2042,2043,2044,2046,2047,2048,2049,2051,2052,2053,2054,2056,2057,2058,2059,2061,2062,2063,2064,2066,2067,2068,2069,2071,2072,2073,2074,2076,2077,2078,2079,2081,2082,2083,2084,2086,2087,2088,2089,2091,2092,2093,2094,2096,2097,2098,2099,2101,2102,2103,2104,2106,2107,2108,2109)]
    # Rename columns
    setnames(all, "MODEL", "model")
    setnames(all, "SCENARIO", "scenario")
    setnames(all, "REGION", "region")
    setnames(all, "VARIABLE", "variable")
    setnames(all, "UNIT", "unit")

    all   <- merge(scens, all, by=c("scenario"), all=TRUE)

    all  <- na.omit(all) # remove rows containing NAs

    #add column for sorting into national and global models
    all$Scope <- factor("global",levels = c("global","national"))

    #make variables a factor, so that the order in facets can be manipulated easily
    all$variable <- factor(all$variable)
    #baseline scenarios will thus be exclude from loop over "Baselines of baselines"
    all[all$Baseline=="-",]$Baseline <- NA

    return(all)
}

#### function for adding variables
add_variables <- function(all,scens){
    ####Additional variables
    #source functions for creation of additional variables
    source("functions/calcVariable.R")
    source("functions/calcRel2Base.R")

    # changed, since most models only report Emissions|CO2
    all <- calcVariable(all,'`Emis|EI` ~ `Emissions|CO2|Energy and Industrial Processes` ' , newUnit='Mt CO2/yr')
    #FIXME? Sowehow needed a dummy because on first try error: aggregate function missing, defaulting to 'length'
    all <- calcVariable(all,'`Emissions|CO2|FFI` ~ `Emissions|CO2|Energy and Industrial Processes` ' , newUnit='Mt CO2/yr')
    all <- calcVariable(all,'`Emissions Intensity of GDP|MER` ~ `Emissions|CO2|FFI`/`GDP|MER` ' , newUnit='kg CO2/$US 2005')
    all <- calcVariable(all,'`Emissions Intensity of GDP|PPP` ~ `Emissions|CO2|FFI`/`GDP|PPP` ' , newUnit='kg CO2/$US 2005')
    all <- calcVariable(all,'`Carbon Intensity of GDP|MER` ~ `Emissions|CO2`/`GDP|MER` ' , newUnit='kg CO2/$US 2005')
    all <- calcVariable(all,'`GHG Intensity of GDP|MER` ~ `Emissions|Kyoto Gases`/`GDP|MER` ' , newUnit='kg CO2e/$US 2005')
    all <- calcVariable(all,'`Emissions per capita` ~ `Emissions|CO2|FFI`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`LU Emissions per capita` ~ `Emissions|CO2|AFOLU`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`GHG emissions per capita` ~ `Emissions|Kyoto Gases`/`Population` ' , newUnit='t CO2eq/cap')
    all <- calcVariable(all,'`Final Energy per capita` ~ `Final Energy`/`Population` * 1000 ' , newUnit='GJ/cap')
    all <- calcVariable(all,'`BECCS per capita` ~ `Carbon Sequestration|CCS|Biomass` / `Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Non-CO2 GHG per capita` ~ (`Emissions|Kyoto Gases` - `Emissions|CO2` )  / `Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Emissions|Kyoto Gases|Excl. AFOLU CO2` ~ `Emissions|Kyoto Gases` - `Emissions|CO2|AFOLU` ' , newUnit='Mt CO2-equiv/yr')
    all <- calcVariable(all,'`Emissions|CO2|FFI|gross` ~ `Emissions|CO2|Energy and Industrial Processes` +  `Carbon Sequestration|CCS|Biomass`' , newUnit='Mt CO2/yr')
    all <- calcVariable(all,'`Fossil emissions per cap` ~ `Emissions|CO2|FFI|gross` / `Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Wind and Solar Share` ~ ( `Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Nuclear Share` ~ ( `Secondary Energy|Electricity|Nuclear`  ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    #FIX ME? Returns NA for some countries/scenarios (now excluding ocean) 
    all <- calcVariable(all,'`Renewables Share|Incl. Hydro and Nuclear` ~ ( `Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` + `Secondary Energy|Electricity|Nuclear` + `Secondary Energy|Electricity|Hydro` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Geothermal`) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Renewables Share|Excl. Hydro` ~ ( `Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` + `Secondary Energy|Electricity|Nuclear` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Geothermal` ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Renewables Share|Excl. Nuclear` ~ ( `Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` + `Secondary Energy|Electricity|Hydro` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Geothermal` ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Low-carbon Electricity Share|All excl. Fossil w/o CCS` ~ ( `Secondary Energy|Electricity` - `Secondary Energy|Electricity|Fossil|w/o CCS`) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Fossil Share` ~ ( `Secondary Energy|Electricity|Coal` + `Secondary Energy|Electricity|Oil` + `Secondary Energy|Electricity|Gas` ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Renewables Share|TPES|Excl. Nuclear` ~ ( `Primary Energy|Solar` + `Primary Energy|Wind` + `Primary Energy|Hydro` + `Primary Energy|Biomass` + `Primary Energy|Geothermal` ) / `Primary Energy` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Renewables Share|TPES|Excl. Nuclear & Geothermal` ~ ( `Primary Energy|Solar` + `Primary Energy|Wind` + `Primary Energy|Hydro` + `Primary Energy|Biomass`) / `Primary Energy` * 100 ' , newUnit='%')
    
    all <- calcVariable(all,'`Share of Elec in FE` ~  `Final Energy|Electricity`   / `Final Energy` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Share of Elec in Transport` ~  `Final Energy|Transportation|Electricity`   / `Final Energy|Transportation` * 100 ' , newUnit='%')
    # all <- calcVariable(all,'`Carbon Intensity of Electricity` ~ `Emissions|CO2|Energy|Supply|Electricity`/ `Secondary Energy|Electricity` ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`Carbon Intensity of FE` ~ `Emissions|CO2|FFI`/`Final Energy` ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`Total CO2 Intensity of FE` ~ `Emissions|CO2`/`Final Energy` ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`Carbon Intensity of Electricity` ~ `Emissions|CO2|Energy|Supply`/`Final Energy|Electricity` ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`Carbon Intensity of Fuel` ~ `Emissions|CO2|Energy|Demand`/(`Final Energy`-`Final Energy|Electricity`) ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`GHG Intensity of FE` ~ `Emissions|Kyoto Gases`/`Final Energy` ' , newUnit='kg CO2eq/GJ')
    all <- calcVariable(all,'`Energy Intensity of GDP|MER` ~ `Final Energy`/`GDP|MER` ' , newUnit='GJ/$2005')
    all <- calcVariable(all,'`Energy Intensity of GDP|PPP` ~ `Final Energy`/`GDP|PPP` ' , newUnit='GJ/$2005')
    all <- calcVariable(all,'`GDP per capita|MER` ~ `GDP|MER`/`Population` ' , newUnit='1000 $US 2005/cap')
    all <- calcVariable(all,'`GDP per capita|PPP` ~ `GDP|PPP`/`Population` ' , newUnit='1000 $US 2005/cap')
    all <- calcRel2Base(all,var="Emissions|CO2|FFI",baseEq1=F,"relative Abatement|CO2",scens)
    all <- calcRel2Base(all,var="Carbon Intensity of FE",baseEq1=T,"Carbon intensity rel. to Base",scens)
    all <- calcRel2Base(all,var="Carbon Intensity of FE",baseEq1=F,"Carbon intensity improvement rel. to Base",scens)
    all <- calcVariable(all,'`Emissions|CO2|rel2Base` ~ 100.0 - `relative Abatement|CO2` ' , newUnit='%')
    all <- calcVariable(all,'`Reduction rel to 2010` ~ 100.0 - `relative Abatement|CO2` ' , newUnit='%')
    all <- calcRel2Base(all,var="Energy Intensity of GDP|MER",baseEq1=T,"Energy intensity rel. to Base",scens)
    all <- calcRel2Base(all,var="Energy Intensity of GDP|MER",baseEq1=F,"Energy intensity improvement rel. to Base",scens)
    all <- calcVariable(all,'`CI over EI indicator` ~ `Carbon intensity rel. to Base`/`Energy intensity rel. to Base` ' , newUnit='')
    all <- calcVariable(all,'`CI over EI indicator` ~ `Carbon intensity rel. to Base`/`Energy intensity rel. to Base` ' , newUnit='')
    # calcualte non-co2 emisisons, energy CH4 emissoins for industry, buildings and transportation are very small and left out
    #all <- calcVariable(all,'`Emissions|Non-CO2` ~ 25 * (`Emissions|CH4|Energy|Supply` + `Emissions|CH4|AFOLU`) + 0.298 * (`Emissions|N2O|Energy` + `Emissions|N2O|AFOLU`)' , newUnit='Mt CO2-equiv/yr')
    all <- calcVariable(all,'`Emissions|CO2|Industry` ~ `Emissions|CO2|Energy|Demand|Industry` + `Emissions|CO2|Industrial Processes`' , newUnit='Mt CH4/yr')
    all <- calcVariable(all,'`Emissions|CH4|Waste` ~ `Emissions|CH4` - `Emissions|CH4|Energy|Supply` - `Emissions|CH4|Energy|Demand|Industry`- `Emissions|CH4|Energy|Demand|Residential and Commercial` - `Emissions|CH4|Energy|Demand|Transportation`- `Emissions|CH4|AFOLU`' , newUnit='Mt CH4/yr')
    all <- calcVariable(all,'`Emissions|N2O|Waste` ~ `Emissions|N2O` - `Emissions|N2O|Energy` - `Emissions|N2O|Other` - `Emissions|N2O|AFOLU`' , newUnit='kt N2O/yr')
    
    # Check if this is possible with new snapshot?
    #all <- calcVariable(all,'`FE freight/tkm` ~ `Final Energy|Transportation|Freight`/`Energy Service|Transportation|Freight` ' , newUnit='EJ/bn tkm')
    #all <- calcVariable(all,'`FE passenger/pkm` ~ `Final Energy|Transportation|Passenger`/`Energy Service|Transportation|Passenger` ' , newUnit='EJ/bn pkm')
    #all <- calcVariable(all,'`FE residential and commercial/floor space` ~ `Final Energy|Residential and Commercial`/`Energy Service|Residential and Commercial|Floor Space` ' , newUnit='EJ/bn m2')
    #all <- calcVariable(all,'`FE residential/floor space` ~ `Final Energy|Residential`/`Energy Service|Residential|Floor Space` ' , newUnit='EJ/bn m2')
    #all <- calcVariable(all,'`FE commercial/floor space` ~ `Final Energy|Commercial`/`Energy Service|Commercial|Floor Space` ' , newUnit='EJ/bn m2')
    
    all <- calcVariable(all,'`Policy Cost` ~ `Policy Cost|Area under MAC Curve` ' , newUnit='billion US$2010/yr')
    all <- calcVariable(all,'`Policy Cost` ~ `Policy Cost|Consumption Loss` ' , newUnit='billion US$2010/yr')
#    all <- calcVariable(all,'`Policy Cost` ~ `Policy Cost|Other` ' , newUnit='billion US$2010/yr')
    all <- calcVariable(all,'`Mitigation Costs` ~ `Policy Cost` / `GDP|MER` *100 ' , newUnit='% of GDP')
    
    all <- calcVariable(all,'`Emissions|Non-CO2` ~ (`Emissions|CH4`*25)+(`Emissions|N2O`*0.298) + `Emissions|F-Gases`' , newUnit='Mt CO2-equiv/yr') #`Emissions|F-Gases`
    #Demand sector emissions and final energy per capita for comparison across countries
    all <- calcVariable(all,'`Transport CO2 per capita` ~ `Emissions|CO2|Energy|Demand|Transportation`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Industry CO2 per capita` ~ `Emissions|CO2|Energy|Demand|Industry`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Buildings CO2 per capita` ~ `Emissions|CO2|Energy|Demand|Residential and Commercial`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Energy supply CO2 per capita` ~ `Emissions|CO2|Energy|Supply`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Transport FE per capita` ~ `Final Energy|Transportation`/`Population` ' , newUnit='TJ/cap')
    all <- calcVariable(all,'`Industry FE per capita` ~ `Final Energy|Industry`/`Population` ' , newUnit='TJ/cap')
    all <- calcVariable(all,'`Buildings FE per capita` ~ `Final Energy|Residential and Commercial`/`Population` ' , newUnit='TJ/cap')
    
    #if(cfg$r!="RUS"){all <- calcRel2Base(all,var="Emissions|Non-CO2",baseEq1=F,"Non-CO2 emissions rel. to Base",scens[!scenario=="NPi2020_verylow_V4"] )} #later include 2020_verylow, when submited
    all <- calcRel2Base(all,var="Emissions|CO2",baseEq1=F,"CO2 emissions rel. to Base",scens)
    source("functions/calcBudget.R")
    all <- calcBudget(all,'Emissions|CO2','Carbon budget')
    all <- calcBudget(all,'Emissions|CO2|Energy and Industrial Processes','Carbon budget|Energy and Industry')
    all <- calcBudget(all,'Emissions|CO2|Energy','Carbon budget|Energy')
    source("functions/calcPeak.R")
    all <- calcPeak(all,'Emissions|CO2','Peak year|CO2')
    all <- calcPeak(all,'Emissions|Kyoto Gases','Peak year|Kyoto Gases')
    source("functions/calcRate.R")
    all <- calcRate(all, c("Emissions Intensity of GDP|MER","Carbon Intensity of GDP|MER","GHG Intensity of GDP|MER","Carbon Intensity of FE","Energy Intensity of GDP|MER","Emissions|CO2|FFI","Renewables Share|Excl. Nuclear","GHG Intensity of FE"))

#    all <- overwrite(remind::calcCumulatedDiscount(all, discount = 0.05, nameVar = "GDP|MER"), all)
    
    all=all[!c(region=="Bunkers"&variable%in%c("GHG Intensity of FE","Emissions|Kyoto Gases|Excl. AFOLU CO2","Non-CO2 GHG per capita","GHG emissions per capita"))]

    hist=all[period==2010& Category=="NPi"]
    hist$Category<-"Historical"

    # add variables indexed relative to baseyear
    source("functions/calcRel2BaseYear.R")

    vars <- c("Emissions|CO2", "Emissions|CO2|FFI", "Emissions|CO2|FFI|gross","Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2",
              "Carbon Intensity of FE","Energy Intensity of GDP|MER","Total CO2 Intensity of FE","Carbon Intensity of Electricity","Carbon Intensity of Fuel",
              "Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply",
              "Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial")
    all <- rbind(all, calcRel2BaseYear(df=all,vars=vars))

    all <- calcVariable(all,'`Reduction rel to 2010` ~ 100.0 - `Emissions|CO2|FFI|rel2010` * 100 ' , newUnit='%')
    all <- rbind(all,hist)
    return(all)
}

