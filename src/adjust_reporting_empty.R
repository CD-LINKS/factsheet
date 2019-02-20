# 1. change AIM|Enduse 3.0 to AIM-India[IIMA]
all=data.table(all)
all[model=="AIM/Enduse 3.0"]$model <- "AIM-India [IIMA]"
