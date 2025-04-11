library(vars)
### all of these are data processing, the processed data is already stored.
### mobility <- read.csv("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/processed_mobility.csv")
### mobility_data <- mobility %>%
###   group_by(`YEARMONTHWEEK`) %>%
###   summarise(mobility = sum(mobility))
### weekly_mobility <- mobility_data[75:152, ] # from 2020/05/31(start of 2020-06-01) to 2021/11/27(end of 2021-11-04)
### incidence_data <- read_excel("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/incidence.xlsx", sheet = 1, range = cell_cols("A:B"))
### incidence_data <- incidence_data[-1, ] %>%
###   rename(date = `구분`, incidences = `총합계`)
  ### daily_incidence <- incidence_data[97:642, ]
### weekly_incidence <- daily_incidence %>%
###   mutate(order = ceiling(row_number() / 7)) %>%
###   group_by(order) %>%
###   summarise(incidences = sum(as.numeric(incidences), na.rm = TRUE))
### new_data <- cbind(weekly_mobility[1], incidences = weekly_incidence[[2]], mobility = weekly_mobility[[2]])
### new_data$policy <- as.numeric(0)
## the policy data comes from an open source website
## https://www.phwr.org/journal/view.html?doi=10.56786/PHWR.2023.16.5.1

### ## our data is collected by weekly, but policy data is collected by specific
### ## date. For any week containing two policies of different strengths, we average 
### ## the number of days the policies were in effect.

### new_data$policy[5:11] <- 1/3
### new_data$policy[12:19] <- 2/3
### new_data$policy[20] <- 8/21
### new_data$policy[21:22] <- 1/3
### new_data$policy[23] <- 11/35
### new_data$policy[24] <- 1/5
### new_data$policy[25] <- 10/35
### new_data$policy[26] <- 19/35
### new_data$policy[27] <- 3/5
### new_data$policy[28] <- 26/35
### new_data$policy[29:37] <- 4/5
### new_data$policy[38] <- 22/35
### new_data$policy[39:58] <- 3/5
### new_data$policy[59] <- 33/35
### new_data$policy[60:74] <- 1
### new_data$policy[75] <- 1/7
### write.csv(new_data, file = "/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/vardata.csv", row.names = FALSE)
