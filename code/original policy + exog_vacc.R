## deal with vacc
library(readxl)
data <- read_excel("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/owid-covid-data (4).xlsx")
south_korea_data <- data %>%
  filter(location == "South Korea")
korea_vacc <- south_korea_data %>% dplyr::select(1:4, people_fully_vaccinated)
vacctargetdate <- korea_vacc %>%
  slice(176:672) # 2020.6.28~2021.11.6

vacctotal <- vacctargetdate %>%
  dplyr::select(date, people_fully_vaccinated) %>%  # 选择所需的列
  slice(seq(7, n(), by = 7))  # 选择7的倍数行

vacctotal$people_fully_vaccinated_rate <- vacctotal$people_fully_vaccinated/51854000
vacctotal <- vacctotal %>%
  mutate(people_fully_vaccinated_rate = replace_na(people_fully_vaccinated_rate, 0))


########################
library(vars)
varnew <- read.csv("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/total_Seoul_PMI.csv")
varnew$vac_rate <- vacctotal$people_fully_vaccinated_rate
start_time <- c(2020, 27)

# 将内生变量（incidence, mobility, policy）转换为时间序列数据
varnew_ts <- ts(varnew[, c("incidences", "mobility", "policy")], start = start_time, frequency = 52)

# 将外生变量 vacc_rate 也转换为时间序列数据
vacc_rate_ts <- ts(varnew$vac_rate, start = start_time, frequency = 52)
vacc_rate_ts_df <- data.frame(vacc_rate = vacc_rate_ts)

# 滞后选择（基于 AIC、HQ 和 SC）
(lag_selection <- VARselect(varnew_ts, lag.max = 10, type = "both"))
# 查看AIC, HQ, SC选择的滞后阶数
lag_selection$selection[c("AIC(n)", "HQ(n)", "SC(n)")]

# 构建 VARX 模型，p = 2 表示选择了 2 阶滞后
varx_model <- VAR(varnew_ts, p = 2, type = "both", exogen = vacc_rate_ts_df)

summary(varx_model)
irf_result <- irf(varx_model, impulse = "incidences", response ="incidences" , n.ahead = 10, boot = TRUE)
plot(irf_result)
irf_result <- irf(varx_model, impulse = "incidences", response ="mobility" , n.ahead = 10, boot = TRUE)
plot(irf_result)
irf_result <- irf(varx_model, impulse = "incidences", response ="policy" , n.ahead = 10, boot = TRUE)
plot(irf_result)

irf_result <- irf(varx_model, impulse = "mobility", response ="incidences" , n.ahead = 10, boot = TRUE)
plot(irf_result)
irf_result <- irf(varx_model, impulse = "mobility", response ="mobility" , n.ahead = 10, boot = TRUE)
plot(irf_result)
irf_result <- irf(varx_model, impulse = "mobility", response ="policy" , n.ahead = 10, boot = TRUE)
plot(irf_result)

irf_result <- irf(varx_model, impulse = "policy", response ="incidences" , n.ahead = 10, boot = TRUE)
plot(irf_result)
irf_result <- irf(varx_model, impulse = "policy", response ="mobility" , n.ahead = 10, boot = TRUE)
plot(irf_result)
irf_result <- irf(varx_model, impulse = "policy", response ="policy" , n.ahead = 10, boot = TRUE)
plot(irf_result)



#############for each district
dist_order <- c("total", "Jung", "Jongno", "Gangnam", "Yeongdeungpo", "Seocho", "Geumcheon", 
                "Yongsan", "Mapo", "Seongdong", "Dongdaemun", "Seodaemun", "Guro", 
                "Songpa", "Gangseo", "Seongbuk", "Gwangjin", "Dongjak", "Gangdong", 
                "Nowon", "Gangbuk", "Yangcheon", "Jungnang", "Dobong", "Gwanak", 
                "Eunpyeong")
dist_incidence <- read.csv("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/dist_incidence.csv")
dist_incidence[, 2:27] <- dist_incidence[, 2:27] / 100
dist_mobility <- read.csv("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/dist_mobility.csv")
dist_mobility[, 2:27] <- dist_mobility[, 2:27] / 1000000

PMI <- read.csv("/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/data/total_Seoul_PMI.csv")
# Get all region names
regions <- colnames(dist_incidence)[2:ncol(dist_incidence)]
# Merge data tables with renaming columns
merged_data <- merge(
  merge(
    setNames(dist_incidence, c(colnames(dist_incidence)[1], paste0(regions, "_incidence"))),
    setNames(dist_mobility, c(colnames(dist_mobility)[1], paste0(regions, "_mobility"))),
    by = "YEARMONTHWEEK"
  ), 
  PMI, 
  by = "YEARMONTHWEEK"
)

# Initialize a list to store VAR models
var_models <- list()

# Start time
start_time <- c(2020, 27)

# Loop to build VAR model for each region
for (region in regions) {
  # Create new data frame
  varnew <- data.frame(
    incidences = merged_data[[paste0(region, "_incidence")]],
    mobility = merged_data[[paste0(region, "_mobility")]],
    policy = merged_data$policy
  )
  
  # Convert data to time series
  varnew_ts <- ts(varnew, start = start_time, frequency = 52)
  
  # Build VAR model
  var_modelnew <- VAR(varnew_ts, p = 2, type = "both", exogen = vacc_rate_ts_df)
  
  # Store the model in the list
  var_models[[region]] <- var_modelnew
}
summary(var_models$total)

# 初始化一个空的数据框来存储结果
results <- data.frame(
  DIST = character(),
  Incidence.l1 = character(), Incidence.l2 = character(),
  Mobility.l1 = character(), Mobility.l2 = character(),
  Policy.l1 = character(), Policy.l2 = character(), trend = character(),
  Vacc = character(),  # 直接加入Vacc列，无滞后项
  Incidence.l1_mobility = character(), Incidence.l2_mobility = character(),
  Mobility.l1_mobility = character(), Mobility.l2_mobility = character(),
  Policy.l1_mobility = character(), Policy.l2_mobility = character(), trend_mobility = character(),
  Vacc_mobility = character(),  # 直接加入Vacc列，无滞后项（Mobility）
  Incidence.l1_policy = character(), Incidence.l2_policy = character(),
  Mobility.l1_policy = character(), Mobility.l2_policy = character(),
  Policy.l1_policy = character(), Policy.l2_policy = character(), trend_policy = character(),
  Vacc_policy = character(),  # 直接加入Vacc列，无滞后项（Policy）
  stringsAsFactors = FALSE
)

# 定义函数格式化系数和标准误差
format_coeff <- function(coef, se, pval) {
  formatted <- sprintf("%.3f (%.3f)", coef, se)
  if (pval < 0.05) {
    formatted <- paste0(formatted, "**")
  } else if (pval < 0.1) {
    formatted <- paste0(formatted, "*")
  }
  return(formatted)
}

# 遍历所有地区，提取每个VAR模型的结果
for (region in names(var_models)) {
  model <- var_models[[region]]
  summary_model <- summary(model)
  
  # 初始化一行数据
  row <- c(DIST = region)
  
  # 定义要提取的变量及其滞后项（不包含外生变量）
  variables <- c("incidences.l1", "incidences.l2",
                 "mobility.l1", "mobility.l2",
                 "policy.l1", "policy.l2", "trend")
  
  # 为每个模型提取系数和标准误差
  for (response in c("incidences", "mobility", "policy")) {
    coefficients <- coef(summary_model$varresult[[response]])
    
    for (var in variables) {
      coef <- coefficients[var, "Estimate"]
      se <- coefficients[var, "Std. Error"]
      pval <- coefficients[var, "Pr(>|t|)"]
      row <- c(row, format_coeff(coef, se, pval))
    }
    
    # 提取Vacc外生变量的系数及其标准误差和p值
    vacc_coef <- coefficients["vacc_rate", "Estimate"]
    vacc_se <- coefficients["vacc_rate", "Std. Error"]
    vacc_pval <- coefficients["vacc_rate", "Pr(>|t|)"]
    row <- c(row, format_coeff(vacc_coef, vacc_se, vacc_pval))
  }
  
  # 添加这一行到结果数据框
  results <- rbind(results, row)
}

# 定义新的列名（包含 Vacc 变量列）
colnames(results) <- c("DIST",
                       "Incidence.l1", "Incidence.l2",
                       "Mobility.l1", "Mobility.l2",
                       "Policy.l1", "Policy.l2", "trend",
                       "Vacc",  # 外生变量Vacc
                       "Incidence.l1_mobility", "Incidence.l2_mobility",
                       "Mobility.l1_mobility", "Mobility.l2_mobility",
                       "Policy.l1_mobility", "Policy.l2_mobility", "trend_mobility",
                       "Vacc_mobility",  # 外生变量Vacc（Mobility）
                       "Incidence.l1_policy", "Incidence.l2_policy",
                       "Mobility.l1_policy", "Mobility.l2_policy",
                       "Policy.l1_policy", "Policy.l2_policy", "trend_policy",
                       "Vacc_policy")  # 外生变量Vacc（Policy）

# 格式化 DIST 列，去除 "-gu" 后缀
results$DIST <- sub("-gu", "", results$DIST)

# 根据指定顺序对DIST进行排序
results <- results[match(dist_order, results$DIST), ]

# 写入Excel文件
write.xlsx(results, "~/Desktop/var_model_summary_with_vacc.xlsx", rowNames = FALSE)

