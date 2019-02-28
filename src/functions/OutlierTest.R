DDT <- read.csv2("data/DeanDixonTable.csv")
d1 <- filter(all_paper, Category=="NDC", region=="EU", period==2030, variable=="Emissions|Kyoto Gases") %>%
      select(Category, model, region, period, value, variable) %>%
      arrange(value)
d2 <- group_by(d1, Category, region, period, variable) %>% 
      summarise(first=first(value), second=nth(value,2), last=last(value)) %>%
      mutate(Q=abs(second-first)/abs(last-first))

