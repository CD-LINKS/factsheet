# factsheet
Source code for generating national fact sheets from IAMC template data (originally developed using CD-LINKS and MILES data)
CD-LINKS: http://www.cd-links.org/ and the database: https://db1.ene.iiasa.ac.at/CDLINKSstocktakingDB/dsd?Action=htmlpage&page=welcome
MILES database: https://tntcat.iiasa.ac.at/MILESDB/dsd?Action=htmlpage&page=about

# folder structure
In the main folder 'src' you find:
- a data folder - save your database snapshot (IAMC style) here
- a functions folder - contains functions, which are called upon in other scripts
- a graphs folder - for storing output
- a settings folder - for settings files that can be country-specific
- a markdown folder - contains the Rmarkdown scripts for creating fact sheets (pdfs)
- the main scripts
-- source_all.R calls the other main scripts, if you want to run everything in one go
-- main_factsheet.R is the script for generating national fact sheets
-- main_crosscut.R is the script for generating plots comparing countries

