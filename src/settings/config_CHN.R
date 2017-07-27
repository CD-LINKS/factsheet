######################################################
################ Configuration file         ##########
################ with user defined settings ##########
######################################################

cfg <- list()

######################################################
################ General settings ####################
######################################################

# csv file that contains input data and will be read (only file name, without folder and extension)
cfg$infile    <- "cdlinks_compare_20170328-093644"

# region for which fact sheet will be produced
cfg$r         <- "CHN"
cfg$rname     <- "China"

# Name of corresponding national model(s)
cfg$model_nat <- c("China TIMES","IPAC-AIM/technology V1.0")

# Name of directory plots are stored in
cfg$outdir    <- "graphs"

# file format for plots
cfg$format <- ".png"


######################################################
############### Define plot styles ###################
######################################################

# Color palette
cfg$colors <- c("#0072B2","#000000","#D55E00","#33cc00","#cc0000","#999999")

# Shapes
cfg$man_shapes=c("POLES 2014"        =  11, 
                 "POLES AMPERE"      = 11, 
                 "POLES EMF27"       = 11,
                 "POLES CDL"       = 11,
                 "REMIND-MAgPIE 1.7-3.0"  =  2, 
                 "REMIND 1.5"        =  2,
                 "MESSAGE V.4"       =  3,
                 "MESSAGE-GLOBIOM_1.0"=  3,
                 "MESSAGEix-GLOBIOM_1.0" = 3,
                 "DNE21+ V.12A"      =  4, 
                 "DNE21+ V.14"      =  4, 
                 "DNE21+ V.12E"      =  4,
                 "WITCH2016"         =  5, 
                 "WITCH2013"         =  5, 
                 "WITCH"         =  5, 
                 "IMAGE 3.0"         =  6, 
                 "IMAGE 2.4"         =  6, 
                 "GEM-E3_V1"         =  7, 
                 "GEM-E3_IPTS_World" =  7,
                 "AIM/CGE"           =  8,
                 "GCAM4"             = 9, 
                 "GCAM_LAMP"         = 9, 
                 "COPPE-COFFEE 1.0"  =  10,
                 "DNE21+ V.14 (national)"=1,
                 #"COPPE-MSB_v1.3.2"  = 1,
                 "*GCAM4.2_CDLINKS" = 1,
                 "*RU-TIMES 3.2"       = 1,
                 "*China TIMES"       = 1,
                 "*IPAC-AIM/technology V1.0" = 2,
                 "*PRIMES_V1"         = 1,
                 "*GEM-E3_V1"         =  2, 
                 "*COPPE-MSB_v2.0"  = 1,
                 "*AIM/Enduse[Japan]" = 1,
                 "*DNE21+ V.14 (national)" = 2,
                 "*GCAM-USA_CDLINKS" = 1,
                 "*India MARKAL"     = 1,
                 "*AIM/Enduse 3.0"   = 2)

# Linestyles         
cfg$man_lines=c("POLES 2014"        =  "dotted", 
                "POLES AMPERE"      = "dotted", 
                "POLES EMF27"       = "dotted",
                "POLES CDL"       = "dotted",
                "REMIND-MAgPIE 1.7-3.0"  =  "dashed", 
                "REMIND 1.5"        =  "dashed", 
                "WITCH2016"         =  "longdash",
                "WITCH2013"         =  "longdash", 
                "WITCH"             =  "longdash", 
                "IMAGE 3.0"         =  "twodash", 
                "IMAGE 2.4"         =  "twodash", 
                "COPPE-COFFEE 1.0"  =  "solid", 
                "AIM/CGE"           =  "dotdash", 
                "MESSAGE-GLOBIOM_1.0"= "dotted", 
                "MESSAGEix-GLOBIOM_1.0" = "dotted",
                "MESSAGE V.4"       =  "dotted",
                "DNE21+ V.14"      =  "dashed", 
                "DNE21+ V.12A"      =  "dashed", 
                "DNE21+ V.12E"      =  "dashed",
                "GEM-E3_V1"         =  "longdash", 
                "GEM-E3_IPTS_World" =  "longdash", 
                "GCAM4"             = "twodash", 
                "GCAM_LAMP"         = "twodash", 
                "DNE21+ V.14 (national)"="solid",
                #"COPPE-MSB_v1.3.2"  = "solid",
                #"*GCAM4.2_CDLINKS" = "solid",
                "*RU-TIMES 3.2"       = "solid",
                "*China TIMES"       = "solid",
                "*IPAC-AIM/technology V1.0" = "dashed",
                "*PRIMES_V1"         = "solid",
                "*GEM-E3_V1"         =  "dashed", 
                "*COPPE-MSB_v2.0"  = "solid",
                "*AIM/Enduse[Japan]" = "solid",
                "*DNE21+ V.14 (national)" = "dashed",
                "*GCAM-USA_CDLINKS" = "solid",
                "*India MARKAL"     = "solid",
                "*AIM/Enduse 3.0"   = "dashed")