######################################################
################ Configuration file         ##########
################ with user defined settings ##########
######################################################

cfg <- list()

######################################################
################ General settings ####################
######################################################

# csv file that contains input data and will be read (only file name, without folder and extension)
cfg$infile    <- "cdlinks_compare_20170316-092210"

# region for which fact sheet will be produced
cfg$r         <- "World"
cfg$rname     <- "World"

# Name of corresponding national model(s)
cfg$model_nat <- c("REMIND 1.6")

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
cfg$man_shapes=c("POLES 2014"        =  1, 
                 "REMIND-MAgPIE 1.7-3.0"  =  2, 
                 "WITCH2016"         =  5, 
                 "IMAGE 3.0"         =  6, 
                 "COPPE-COFFEE 1.0"  =  1, 
                 "AIM/CGE"           =  8, 
                 "REMIND 1.5"        =  2, 
                 "MESSAGE V.4"       =  3,
                 "MESSAGE-GLOBIOM_1.0"=  3,
                 "DNE21+ V.14"      =  4, 
                 "DNE21+ V.12A"      =  4, 
                 "WITCH2013"         =  5, 
                 "WITCH"         =  5, 
                 "IMAGE 2.4"         =  6, 
                 "GEM-E3_V1"         =  7, 
                 "GEM-E3_IPTS_World" =  7, 
                 "DNE21+ V.12E"      =  8,
                 "GCAM4"             = 9, 
                 "GCAM_LAMP"         = 9, 
                 "POLES AMPERE"      = 10, 
                 "POLES EMF27"       = 10,
                 "*China TIMES"       = 1,
                 "*PRIMES_V1"         = 1,
                 "*COPPE-MSB_v2.0"  = 1,
                 "*AIM/Enduse[Japan]" = 1,
                 "*DNE 21+ V.14 (national)" = 1)

# Linestyles         
cfg$man_lines=c("POLES 2014"        =  "solid", 
                "REMIND-MAgPIE 1.7-3.0"  =  "dashed", 
                "WITCH2016"         =  "longdash", 
                "IMAGE 3.0"         =  "twodash", 
                "COPPE-COFFEE 1.0"  =  "solid", 
                "AIM/CGE"           =  "dotdash", 
                "REMIND 1.5"        =  "dashed", 
                "MESSAGE-GLOBIOM_1.0"= "dotted", 
                "MESSAGE V.4"       =  "dotted",
                "DNE21+ V.14"      =  "dotdash", 
                "DNE21+ V.12A"      =  "dotdash", 
                "DNE21+ V.12E"      =  "dotdash",
                "WITCH2013"         =  "longdash", 
                "WITCH"             =  "longdash", 
                "IMAGE 2.4"         =  "twodash", 
                "GEM-E3_V1"         =  "solid", 
                "GEM-E3_IPTS_World" =  "solid", 
                "GCAM4"             = "dashed", 
                "GCAM_LAMP"         = "dashed", 
                "POLES AMPERE"      = "dotted", 
                "POLES EMF27"       = "dotted",
                "*China TIMES"       = "solid",
                "*PRIMES_V1"         = "solid",
                "*COPPE-MSB_v2.0"  = "solid",
                "*AIM/Enduse[Japan]" = "solid",
                "*DNE 21+ V.14 (national)" = "solid") #"dotdash"
