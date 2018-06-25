library(boot)

# read in original sample data (cleveland dataset from uci)
cols <- c("age", "sex", "chest_pain", "resting_bp", "cholesterol", "fast_sugar",
          "resting_ecg", "max_hrate", "exer_angina", "oldpeak", "slope", "ca_mavesel",
          "heart_def_status", "diag")

df_data <- read.csv("data/cleveland14_capstone.csv", header = FALSE)

colnames(df_data) <- cols

# recode diagnosis (diag) to 1 if heart disease exists, or 0 if no heart disease exists
df_data$diag[df_data$diag > 0] <- 1

# statistic function for boot
bs <- function(formula, data, indices){
  d <- data[indices,]
  fit <- glm(formula, family=binomial(link = "logit"), data=d)
  return(fitted.values(fit))
}

set.seed(306)
results <- boot(data = df_data, statistic = bs, R=20000, 
                formula=diag~age+sex+chest_pain+resting_bp+cholesterol+fast_sugar+
                  resting_ecg+max_hrate+exer_angina+oldpeak+slope+ca_mavesel+heart_def_status)
print(results)
plot(results, index=1)
boot.ci(results, type = "norm", index = 1)

