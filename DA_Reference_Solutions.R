# =====================================================================
#  ISBAT University - Faculty of ICT | MSc IT
#  Data Analytics Coursework - REFERENCE SOLUTIONS (facilitator only)
#  Verified with R 4.x, base R + stats only
# =====================================================================

cat("\n================ QUESTION 1: MICROFINANCE LOAN DEFAULT ================\n")
loans <- data.frame(
  id        = sprintf("B%02d", 1:20),
  income    = c(850,420,1200,300,650,980,510,360,1500,700,450,820,390,1100,610,280,930,540,1300,470),  # UGX '000 per month
  loan      = c(2000,1800,2500,1500,1200,2200,2600,1400,3000,1600,2400,1500,1700,2000,2300,1300,1800,2100,2600,2200),  # UGX '000
  momo_txn  = c(42,12,55,8,30,47,15,10,60,35,9,40,14,50,22,6,45,18,58,11),  # monthly mobile money transactions
  prior_def = c(0,1,0,1,0,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0),
  default   = c(0,1,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0))
loans$lti <- round(loans$loan / loans$income, 3)

# (a) descriptive comparison
print(aggregate(cbind(income, lti, momo_txn) ~ default, data = loans, FUN = function(x) round(mean(x), 2)))
cat("Default rate:", mean(loans$default), "\n")

# (b) logistic regression
m <- glm(default ~ lti + momo_txn, data = loans, family = binomial)
print(round(summary(m)$coefficients, 4))
cat("Odds ratios:\n"); print(round(exp(coef(m)), 4))
cat("AIC:", round(AIC(m), 3), "\n")
# (b-ii) adding prior_def produces quasi-complete separation:
print(table(prior_def = loans$prior_def, default = loans$default))
m2 <- glm(default ~ lti + momo_txn + prior_def, data = loans, family = binomial)
print(round(coef(m2), 3))   # prior_def coefficient explodes (~21) with a huge standard error
# Remedies: penalised (Firth) logistic regression, e.g. logistf package; collect more data;
# or treat prior_def as a business rule rather than a model predictor.

# (c) new applicant
newb <- data.frame(lti = 1800/600, momo_txn = 20)
p_new <- predict(m, newb, type = "response")
cat("P(default) for applicant income 600, loan 1800, 20 txns:", round(p_new, 4), "\n")

# (d) confusion matrices at 0.5 and 0.3
p <- fitted(m)
for (th in c(0.5, 0.3)) {
  pred <- as.integer(p >= th)
  TP <- sum(pred==1 & loans$default==1); FP <- sum(pred==1 & loans$default==0)
  FN <- sum(pred==0 & loans$default==1); TN <- sum(pred==0 & loans$default==0)
  cat(sprintf("Threshold %.1f: TP=%d FP=%d FN=%d TN=%d | accuracy=%.3f precision=%.3f recall=%.3f\n",
      th, TP, FP, FN, TN, (TP+TN)/20, TP/(TP+FP), TP/(TP+FN)))
}


cat("\n================ QUESTION 2: FERTILISER TRIAL & MOBILE MONEY SURVEY ================\n")
yield <- data.frame(
  fert = factor(rep(c("NPK","Urea","Organic"), each = 6)),
  t_ha = c(3.8,4.1,3.6,4.4,3.9,4.2,   4.6,4.9,4.3,5.1,4.7,4.8,   3.5,3.9,3.3,3.7,3.6,4.0))
print(tapply(yield$t_ha, yield$fert, function(x) round(c(mean=mean(x), sd=sd(x)), 3)))
print(tapply(yield$t_ha, yield$fert, function(x) round(shapiro.test(x)$p.value, 4)))   # Shapiro-Wilk p-values
print(bartlett.test(t_ha ~ fert, data = yield))
a <- aov(t_ha ~ fert, data = yield)
print(summary(a))
print(TukeyHSD(a))

momo <- matrix(c(182, 38,
                 141, 59,
                  96, 84,
                 150, 50), ncol = 2, byrow = TRUE,
               dimnames = list(Region = c("Central","Eastern","Northern","Western"), Uses = c("Yes","No")))
ct <- chisq.test(momo)
print(ct)
cat("Expected counts:\n"); print(round(ct$expected, 2))
cat("Standardised residuals:\n"); print(round(ct$stdres, 3))
cat("Cramer's V:", round(sqrt(ct$statistic / (sum(momo) * (min(dim(momo)) - 1))), 4), "\n")


cat("\n================ QUESTION 3: MONTHLY SALES FORECASTING ================\n")
sales <- c(410,395,430,445,460,450,470,485,500,515,540,690,
           505,490,525,540,560,545,575,590,605,625,650,820)   # UGX millions, Jan 2024 - Dec 2025
month <- 1:24
ma3 <- stats::filter(sales, rep(1/3, 3), sides = 1)
cat("3-month trailing MA, last 3 values:", round(tail(ma3, 3), 2), "\n")

train <- 1:18; test <- 19:24
fit <- lm(sales ~ month, subset = train)
print(round(coef(fit), 4))
pred_trend <- predict(fit, data.frame(month = test))
pred_naive <- rep(sales[18], 6)
mape <- function(a, f) mean(abs((a - f) / a)) * 100
cat("MAPE linear trend (test):", round(mape(sales[test], pred_trend), 2), "%\n")
cat("MAPE naive (test):", round(mape(sales[test], pred_naive), 2), "%\n")

# seasonal (ratio-to-trend) model on all 24 months
fit_all <- lm(sales ~ month)
ratio <- sales / fitted(fit_all)
s_index <- tapply(ratio, rep(1:12, 2), mean); s_index <- s_index / mean(s_index)
cat("Seasonal index for December:", round(s_index[12], 4), "\n")
fut <- 25:27
fc <- predict(fit_all, data.frame(month = fut)) * s_index[1:3]
cat("Seasonal-trend forecast Jan-Mar 2026:", round(fc, 2), "\n")
# seasonal-trend model evaluated on the test window (fit on train, indices from train year)
ratio_tr <- sales[train] / fitted(fit)
si_tr <- tapply(ratio_tr, c(1:12, 1:6), mean)
si_tr <- si_tr / mean(si_tr)
pred_st <- pred_trend * si_tr[7:12]
cat("MAPE seasonal-trend (test):", round(mape(sales[test], pred_st), 2), "%\n")


cat("\n================ QUESTION 4: CUSTOMER SEGMENTATION (RFM) ================\n")
rfm <- data.frame(
  cust = sprintf("K%02d", 1:12),
  recency   = c(5, 40, 3, 90, 12, 120, 7, 60, 2, 150, 25, 8),       # days since last purchase
  frequency = c(24, 6, 30, 2, 15, 1, 20, 4, 35, 1, 10, 18),         # purchases in last 12 months
  monetary  = c(4.8, 0.9, 6.2, 0.3, 2.5, 0.2, 3.9, 0.7, 7.5, 0.1, 1.6, 3.1))  # UGX millions
X <- scale(rfm[, 2:4])
wss <- sapply(1:6, function(k) { set.seed(42); kmeans(X, centers = k, nstart = 25)$tot.withinss })
cat("Total within-cluster SS for k = 1..6:", round(wss, 3), "\n")
set.seed(42)
km <- kmeans(X, centers = 3, nstart = 25)
rfm$cluster <- km$cluster
prof <- aggregate(cbind(recency, frequency, monetary) ~ cluster, data = rfm, FUN = function(x) round(mean(x), 2))
prof$size <- as.vector(table(rfm$cluster))
print(prof)
print(rfm)
cat("Between SS / Total SS:", round(km$betweenss / km$totss * 100, 2), "%\n")
