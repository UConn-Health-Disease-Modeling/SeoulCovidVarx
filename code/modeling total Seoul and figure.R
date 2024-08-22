### mobility in million, incidences in hundred
library(vars)
library(ggplot2)
library(tidyr)
library(gridExtra)

varnew <- read.csv("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/total_Seoul_PMI.csv")
start_time <- c(2020, 27)

varnew_ts <- ts(varnew[, c("incidences", "mobility", "policy")], start = start_time, frequency = 52)
(lag_selection <- VARselect(varnew_ts, lag.max = 10, type = "both"))

var_modelnew <- VAR(varnew_ts, p = 2, type = "both")
summary(var_modelnew$varresult$mobility)
varnew$x_label <- paste0(substr(varnew$YEARMONTHWEEK, 6, 7))
remove_nonconsecutive_duplicates <- function(labels) {
  last_seen <- NULL
  for (i in seq_along(labels)) {
    if (!is.null(last_seen) && labels[i] == last_seen) {
      labels[i] <- ""
    } else {
      last_seen <- labels[i]
    }
  }
  return(labels)
}

varnew$x_label <- remove_nonconsecutive_duplicates(varnew$x_label)

p1 <- ggplot(varnew, aes(x = YEARMONTHWEEK, y = policy, group = 1)) +
  geom_line(size = 1, color = "blue") +
  labs(title = "", x = "", y = "Policy Level") +
  theme_minimal(base_size = 15) +
  theme(text = element_text(size = 20, face = "bold"), 
    plot.title = element_text(face = "bold", hjust = 0.5, size = 25),
    axis.ticks.x = element_line(size = 0.5), # 显示x轴的刻度线
    panel.grid.major = element_line(color = "grey90"),  
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  scale_x_discrete(labels = varnew$x_label) 
p1
p2 <- ggplot(varnew, aes(x = YEARMONTHWEEK, y = incidences, group = 1)) +
  geom_line(size = 1, color="red") +
  labs(title = "", x = "", y = "Incidences (in hundred)") +
  theme_minimal(base_size = 15) +
  theme(text = element_text(size = 20, face = "bold"), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 25),
        axis.ticks.x = element_line(size = 0.5), # 显示x轴的刻度线
        panel.grid.major = element_line(color = "grey90"),  
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  scale_x_discrete(labels = varnew$x_label) 
p2
# 创建第三个图
p3 <- ggplot(varnew, aes(x = YEARMONTHWEEK, y = mobility, group = 1)) +
  geom_line(size = 1,color = "purple") +
  labs(title = "", x = "", y = "Mobility (in million)") +
  theme_minimal(base_size = 15) +
  theme(text = element_text(size = 20, face = "bold"), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 25),
        axis.ticks.x = element_line(size = 0.5), # 显示x轴的刻度线
        panel.grid.major = element_line(color = "grey90"),  
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  scale_x_discrete(labels = varnew$x_label)
p3





