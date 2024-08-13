library(vars)
dist_order <- c("Jung", "Jongno", "Gangnam", "Yeongdeungpo", "Seocho", "Geumcheon", 
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
regions <- colnames(dist_incidence)[3:ncol(dist_incidence)]
# Merge data tables with renaming columns
merged_data <- merge(
  merge(
    setNames(dist_incidence, c(colnames(dist_incidence)[1:2], paste0(regions, "_incidence"))),
    setNames(dist_mobility, c(colnames(dist_mobility)[1:2], paste0(regions, "_mobility"))),
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
  var_modelnew <- VAR(varnew_ts, p = 2, type = "both")
  
  # Store the model in the list
  var_models[[region]] <- var_modelnew
}









### NO NEED TO RUN THESE CODE BELOW, JUST FOR SAVING RESULTS IN EXCEL


# 初始化一个空的数据框来存储结果
results <- data.frame(
  DIST = character(),
  Incidence.l1 = character(),
  Incidence.l2 = character(),
  Mobility.l1 = character(),
  Mobility.l2 = character(),
  Policy.l1 = character(),
  Policy.l2 = character(),
  trend = character(),
  Incidence.l1_mobility = character(),
  Incidence.l2_mobility = character(),
  Mobility.l1_mobility = character(),
  Mobility.l2_mobility = character(),
  Policy.l1_mobility = character(),
  Policy.l2_mobility = character(),
  trend_mobility = character(),
  Incidence.l1_policy = character(),
  Incidence.l2_policy = character(),
  Mobility.l1_policy = character(),
  Mobility.l2_policy = character(),
  Policy.l1_policy = character(),
  Policy.l2_policy = character(),
  trend_policy = character(),
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
  
  # 定义要提取的变量及其滞后项
  variables <- c("incidences.l1", "incidences.l2", 
                 "mobility.l1", "mobility.l2", 
                 "policy.l1", "policy.l2","trend")
  
  # 为每个模型提取系数和标准误差
  for (response in c("incidences", "mobility", "policy")) {
    coefficients <- coef(summary_model$varresult[[response]])
    
    for (var in variables) {
      coef <- coefficients[var, "Estimate"]
      se <- coefficients[var, "Std. Error"]
      pval <- coefficients[var, "Pr(>|t|)"]
      
      row <- c(row, format_coeff(coef, se, pval))
    }
  }
  
  # 添加这一行到结果数据框
  results <- rbind(results, row)
}

# 定义列名
colnames(results) <- c("DIST", 
                       "Incidence.l1", "Incidence.l2", 
                       "Mobility.l1", "Mobility.l2", 
                       "Policy.l1", "Policy.l2", "trend",
                       "Incidence.l1_mobility", "Incidence.l2_mobility", 
                       "Mobility.l1_mobility", "Mobility.l2_mobility", 
                       "Policy.l1_mobility", "Policy.l2_mobility", "trend_mobility",
                       "Incidence.l1_policy", "Incidence.l2_policy", 
                       "Mobility.l1_policy", "Mobility.l2_policy", 
                       "Policy.l1_policy", "Policy.l2_policy", "trend_policy")

results$DIST <- sub("-gu", "", results$DIST)
results <- results[match(dist_order, results$DIST), ]


# 写入Excel文件
write.xlsx(results, "/Users/huzeyu/Desktop/UConnHealth/Policy_Incidence_Mobility/var_model_summary_V2.xlsx", rowNames = FALSE)
