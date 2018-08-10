# factsheet
Source code for generating national fact sheets from IAMC template data (originally developed using CD-LINKS and MILES data)
CD-LINKS: http://www.cd-links.org/ and the database: https://db1.ene.iiasa.ac.at/CDLINKSstocktakingDB/dsd?Action=htmlpage&page=welcome
MILES database: https://tntcat.iiasa.ac.at/MILESDB/dsd?Action=htmlpage&page=about

# folder structure
In the main folder 'src' you find:
- a data folder - save your database snapshot (IAMC style) here
- a functions folder - contains functions, which are called upon in other scripts - adjust / add (e.g. graph) functions here, suiting your project requirements
- a graphs folder - for storing output
- a settings folder - for settings files that can be country-specific - adjust these for project specific settings, such as participating models, variables to be analysed, scenario names
- a markdown folder - contains the Rmarkdown scripts for creating fact sheets (pdfs) - adjust these to include other graphs, as required for your project
- the main scripts
-- source_all.R calls the other main scripts, if you want to run everything in one go
-- main_factsheet.R is the script for generating national fact sheets - adjust the database name, required variables, scenario categorization etc. to project-specific ones here (see comments)
-- main_crosscut.R is the script for generating plots comparing countries - adjust some names to project-specific ones here (see comments)
-- cross_cut.R is called upon by main_crosscut, and generates graphs - a few examples of how to use the functions are included, add your project-required graphs here

