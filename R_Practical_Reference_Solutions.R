# =====================================================================
#  ISBAT University - Faculty of ICT
#  R Programming Coursework (Practical Set) - REFERENCE SOLUTIONS
#  For the course facilitator only. Do not distribute to students.
#  Verified with R 4.x (base R only; no packages required)
# =====================================================================

cat("\n================ QUESTION 1: SACCO LOAN PORTFOLIO ================\n")
sacco <- data.frame(
  member   = c("Nakato","Okello","Mugisha","Atim","Ssemwogerere","Namubiru",
               "Opio","Kyomuhendo","Wasswa","Akello","Tumusiime","Nabirye"),
  district = c("Wakiso","Gulu","Mbarara","Lira","Wakiso","Mukono",
               "Gulu","Mbarara","Mukono","Lira","Mbarara","Wakiso"),
  loan     = c(2500000,1200000,5000000,800000,3500000,1500000,
               2000000,4200000,950000,1800000,6000000,3000000),
  rate     = c(0.24,0.24,0.20,0.26,0.22,0.24,0.24,0.20,0.26,0.24,0.18,0.22),
  term     = c(12,12,24,6,18,12,12,24,6,12,36,18),
  paid     = c(5,9,10,3,7,11,2,15,4,6,12,8),
  arrears  = c(0,45,12,200,0,95,31,0,150,5,60,0),
  savings  = c(900000,500000,1500000,400000,1000000,700000,
               500000,1600000,300000,800000,1800000,1200000),
  stringsAsFactors = FALSE)

# (a) monthly instalment (EMI)
emi <- function(P, annual_rate, n) {
  r <- annual_rate / 12
  P * r * (1 + r)^n / ((1 + r)^n - 1)
}
sacco$instalment <- round(emi(sacco$loan, sacco$rate, sacco$term))
print(sacco[, c("member","instalment")])
cat("Total expected monthly instalments (UGX):", format(sum(sacco$instalment), big.mark=","), "\n")
cat("Member with the largest instalment:", sacco$member[which.max(sacco$instalment)], "\n")

# (b) outstanding balance after k payments
outstanding_balance <- function(P, annual_rate, n, k) {
  if (any(k > n)) stop("payments made cannot exceed loan term")
  r <- annual_rate / 12
  A <- emi(P, annual_rate, n)
  P * (1 + r)^k - A * ((1 + r)^k - 1) / r
}
sacco$balance <- round(outstanding_balance(sacco$loan, sacco$rate, sacco$term, sacco$paid))
print(sacco[, c("member","balance")])
cat("Highest outstanding balance:", sacco$member[which.max(sacco$balance)],
    format(max(sacco$balance), big.mark=","), "\n")
cat("Total portfolio outstanding (UGX):", format(sum(sacco$balance), big.mark=","), "\n")

# (c) PAR30, classification and provisioning
par30 <- 100 * sum(sacco$balance[sacco$arrears > 30]) / sum(sacco$balance)
cat("PAR30 (%):", round(par30, 2), "\n")
sacco$class <- cut(sacco$arrears, breaks = c(-Inf, 0, 30, 90, 180, Inf),
                   labels = c("Current","Watch","Substandard","Doubtful","Loss"))
prov_rate <- c(Current=0.01, Watch=0.05, Substandard=0.25, Doubtful=0.50, Loss=1.00)
sacco$provision <- round(sacco$balance * prov_rate[as.character(sacco$class)])
print(table(sacco$class))
cat("Total required provision (UGX):", format(sum(sacco$provision), big.mark=","), "\n")

# (d) over-borrowing and district risk
over <- sacco$loan > 3 * sacco$savings
cat("Members whose loan exceeds 3 x savings:", paste(sacco$member[over], collapse=", "), "\n")
dist_par <- sapply(split(sacco, sacco$district), function(d)
  round(100 * sum(d$balance[d$arrears > 30]) / sum(d$balance), 2))
print(sort(dist_par, decreasing = TRUE))
cat("Riskiest district(s) by PAR30:", paste(names(dist_par)[dist_par == max(dist_par)], collapse = " and "), "\n")

# (e) Tumusiime pays an extra UGX 50,000 every month from month 13 onward
i  <- which(sacco$member == "Tumusiime")
r  <- sacco$rate[i] / 12
A  <- emi(sacco$loan[i], sacco$rate[i], sacco$term[i])
bal <- outstanding_balance(sacco$loan[i], sacco$rate[i], sacco$term[i], 12)
months <- 0; interest_paid <- 0
while (bal > 0.005) {
  interest <- bal * r
  payment  <- min(A + 50000, bal + interest)
  bal <- bal + interest - payment
  interest_paid <- interest_paid + interest
  months <- months + 1
}
interest_original <- A * 24 - outstanding_balance(sacco$loan[i], sacco$rate[i], sacco$term[i], 12)
cat("Months needed to clear after month 12:", months, "(instead of 24)\n")
cat("Interest paid with extra payments (UGX):", round(interest_paid), "\n")
cat("Interest on original schedule for the same period (UGX):", round(interest_original), "\n")
cat("Interest saved (UGX):", round(interest_original - interest_paid), "\n")


cat("\n================ QUESTION 2: MOBILE MONEY AGENT LOG ================\n")
log_lines <- c(
 "TXN0401|2026-03-02 08:15|DEPOSIT|150000|AGT-KLA-017",
 "TXN0402|2026-03-02 09:40|WITHDRAW|45000|AGT-KLA-017",
 "TXN0403|2026-03-02 22:35|WITHDRAW|320000|AGT-GUL-004",
 "TXN0404|2026-03-03 06:05|DEPOSIT|980000|AGT-MBR-022",
 "TXN0405|2026-03-03 11:20|WITHDRAW|1250000|AGT-KLA-031",
 "TXN0406|2026-03-03 13:55|DEPOSIT|960000|AGT-MBR-022",
 "TXN0407|2026-03-03 23:10|WITHDRAW|75000|AGT-KLA-017",
 "TXN0408|2026-03-04 02:45|WITHDRAW|500000|AGT-GUL-004",
 "TXN0409|2026-03-04 10:30|DEPOSIT|20000|AGT-GUL-004",
 "TXN0410|2026-03-05 09:05|DEPOSIT|950000|AGT-KLA-031",
 "TXN0411|2026-03-05 09:48|DEPOSIT|980000|AGT-KLA-031",
 "TXN0412|2026-03-05 10:22|DEPOSIT|990000|AGT-KLA-031",
 "TXN0413|2026-03-05 15:00|WITHDRAW|250000|AGT-MBR-022",
 "TXN0414|2026-03-05 16:40|WITHDRAW|50000|AGT-KLA-017",
 "TXN0415|2026-03-06 10:15|DEPOSIT|abc|AGT-GUL-004",
 "TXN0416|2026-03-06 05:59|WITHDRAW|180000|AGT-MBR-022",
 "TXN0417|2026-03-06 12:30|WITHDRAW|1000000|AGT-KLA-031",
 "TXN0418|2026-03-07 21:59|DEPOSIT|300000|AGT-KLA-017",
 "TXN0419|2026-03-07 22:00|WITHDRAW|600000|AGT-GUL-004",
 "TXN0420|2026-03-08 14:10|DEPOSIT|995000|AGT-MBR-022")

# (a) parse into a data frame
parts <- strsplit(log_lines, "|", fixed = TRUE)
txn <- data.frame(do.call(rbind, parts), stringsAsFactors = FALSE)
names(txn) <- c("id","datetime","type","amount","agent")
txn$amount   <- suppressWarnings(as.numeric(txn$amount))
txn$datetime <- as.POSIXct(txn$datetime, format = "%Y-%m-%d %H:%M", tz = "Africa/Kampala")
bad <- txn$id[is.na(txn$amount)]
cat("Malformed record(s) removed:", bad, "\n")
txn <- txn[!is.na(txn$amount), ]
cat("Valid transactions:", nrow(txn), "\n")

# (b) per-agent totals by type
print(tapply(txn$amount, list(txn$agent, txn$type), sum))
net <- tapply(ifelse(txn$type == "DEPOSIT", txn$amount, -txn$amount), txn$agent, sum)
print(net)
cat("Agent with largest net cash inflow:", names(which.max(net)), "\n")
cat("Agent with largest net cash outflow:", names(which.min(net)), "\n")

# (c) night transactions 22:00 - 05:59
hr <- as.integer(format(txn$datetime, "%H"))
night <- txn[hr >= 22 | hr < 6, ]
cat("Night transactions:", nrow(night), "->", paste(night$id, collapse=", "), "\n")
cat("Value of night transactions (UGX):", format(sum(night$amount), big.mark=","), "\n")

# (d) structuring: >= 3 deposits of 900,000-999,999 by the same agent on the same day
txn$day <- as.Date(format(txn$datetime, "%Y-%m-%d"))
sus <- txn[txn$type == "DEPOSIT" & txn$amount >= 900000 & txn$amount <= 999999, ]
cnt <- table(sus$agent, sus$day)
flag <- which(cnt >= 3, arr.ind = TRUE)
for (k in seq_len(nrow(flag)))
  cat("FLAG:", rownames(cnt)[flag[k,1]], "on", colnames(cnt)[flag[k,2]],
      "with", cnt[flag[k,1], flag[k,2]], "near-threshold deposits\n")
cat("Near-threshold deposits by AGT-MBR-022 (not flagged):", sum(sus$agent == "AGT-MBR-022"), "\n")

# (e) tiered withdrawal commission
commission <- function(amount) {
  if (amount <= 50000) 500
  else if (amount <= 250000) 0.01 * amount
  else if (amount <= 1000000) min(0.008 * amount, 6000)
  else 6000
}
wd <- txn[txn$type == "WITHDRAW", ]
wd$commission <- vapply(wd$amount, commission, numeric(1))
print(wd[, c("id","amount","commission")])
comm_agent <- tapply(wd$commission, wd$agent, sum)
print(comm_agent)
cat("Total commission (UGX):", sum(wd$commission), "\n")
cat("Agent earning most commission:", names(which.max(comm_agent)), "\n")


cat("\n================ QUESTION 3: EXAMINATION RESULTS MATRIX ================\n")
scores <- matrix(c(
  68, 74, NA, 81, 59,
  55, 62, 70, 48, 66,
  90, 85, 88, 92, 79,
  72, NA, 65, 70, 81,
  45, 52, 58, 61, NA,
  83, 79, 91, 77, 88,
  61, 70, 66, 73, 69,
  77, 68, 72, 85, 74),
  nrow = 8, byrow = TRUE,
  dimnames = list(c("S01","S02","S03","S04","S05","S06","S07","S08"),
                  c("Algorithms","Databases","Networks","WebTech","Statistics")))
credits <- c(4, 3, 3, 4, 2)

# (a) missing values and imputation by course median
cat("Missing values per course:\n"); print(colSums(is.na(scores)))
raw_avg <- round(rowMeans(scores, na.rm = TRUE), 2)
imp <- scores
for (j in seq_len(ncol(imp))) imp[is.na(imp[, j]), j] <- median(imp[, j], na.rm = TRUE)
print(imp)

# (b) credit-weighted average via matrix multiplication
weighted <- as.vector(imp %*% credits) / sum(credits)
names(weighted) <- rownames(imp)
res <- data.frame(raw_avg = raw_avg, weighted = round(weighted, 2))
res$rank_raw <- rank(-res$raw_avg, ties.method = "min")
res$rank_w   <- rank(-res$weighted, ties.method = "min")
print(res)
cat("Students whose rank changes:", paste(rownames(res)[res$rank_raw != res$rank_w], collapse=", "), "\n")

# (c) z-scores per course; students with z < -1 in any course
z <- round(scale(imp), 3)
print(z)
at_risk <- rownames(z)[apply(z < -1, 1, any)]
cat("At-risk students (z < -1 in at least one course):", paste(at_risk, collapse=", "), "\n")
low_z <- which(z == min(z), arr.ind = TRUE)
cat("Lowest z-score:", min(z), "for", rownames(z)[low_z[1]], "in", colnames(z)[low_z[2]], "\n")

# (d) most strongly correlated pair of courses
cm <- cor(imp)
cm[lower.tri(cm, diag = TRUE)] <- NA
best <- which(cm == max(cm, na.rm = TRUE), arr.ind = TRUE)
cat("Most correlated pair:", rownames(cm)[best[1]], "&", colnames(cm)[best[2]],
    "r =", round(max(cm, na.rm = TRUE), 4), "\n")
cm2 <- cor(imp); diag(cm2) <- NA
cat("Least correlated pair r =", round(min(cm2, na.rm=TRUE), 4), "\n")
lw <- which(cm2 == min(cm2, na.rm=TRUE), arr.ind=TRUE)[1,]
cat("  ->", rownames(cm2)[lw[1]], "&", colnames(cm2)[lw[2]], "\n")

# (e) minimum mark needed in a supplementary Statistics paper for S05 to reach weighted 60
s05 <- imp["S05", ]
needed <- (60 * sum(credits) - sum(s05[-5] * credits[-5])) / credits[5]
cat("S05 weighted average now:", round(weighted["S05"], 2), "\n")
cat("Statistics mark S05 needs for weighted average of 60:", ceiling(needed), "(exact", round(needed, 2), ")\n")


cat("\n================ QUESTION 4: WATER BILLING ================\n")
cust <- data.frame(
  id   = sprintf("C%02d", 1:10),
  type = c("Domestic","Domestic","Commercial","Domestic","Commercial",
           "Domestic","Domestic","Commercial","Domestic","Domestic"),
  prev = c(4512, 9980, 1203, 7750, 3321, 615, 8890, 9940, 2210, 5432),
  curr = c(4530, 22, 1290, NA, 3405, 641, 8951, 37, 2219, 5490),
  stringsAsFactors = FALSE)
history <- matrix(c(17,19,16, 40,44,41, 85,90,88, 23,25,24, 80,79,86,
                    24,27,26, 55,58,60, 92,95,99, 8,9,10, 50,61,57),
                  nrow = 10, byrow = TRUE, dimnames = list(cust$id, c("M1","M2","M3")))

# (a) consumption with meter rollover and estimation
cons <- ifelse(cust$curr >= cust$prev, cust$curr - cust$prev, (10000 - cust$prev) + cust$curr)
est  <- is.na(cust$curr)
cons[est] <- round(rowMeans(history)[est])
cust$units <- cons; cust$estimated <- est
print(cust)

# (b) block tariff
tariff_bill <- function(units, type = "Domestic") {
  if (is.na(units) || units < 0) stop("invalid consumption: ", units)
  if (type == "Commercial") {
    charge <- units * 4300
  } else {
    blocks <- c(10, 15, 25, Inf)          # widths of 0-10, 11-25, 26-50, >50
    prices <- c(2800, 3400, 4100, 4900)
    remaining <- units; charge <- 0
    for (b in seq_along(blocks)) {
      u <- min(remaining, blocks[b]); charge <- charge + u * prices[b]
      remaining <- remaining - u; if (remaining <= 0) break
    }
  }
  round((charge + 3000) * 1.18)
}
cust$bill <- mapply(tariff_bill, cust$units, cust$type)
print(cust[, c("id","type","units","bill")])
cat("Total billed (UGX):", format(sum(cust$bill), big.mark=","), "\n")
cat("Highest bill:", cust$id[which.max(cust$bill)], format(max(cust$bill), big.mark=","), "\n")

# (c) error handling
safe_bill <- function(units, type = "Domestic")
  tryCatch(tariff_bill(units, type), error = function(e) { message("Error: ", conditionMessage(e)); NA })
print(safe_bill(-5)); print(safe_bill(NA))

# (d) flat-rate comparison for domestic customers
dom <- cust[cust$type == "Domestic", ]
flat <- round((dom$units * 3700 + 3000) * 1.18)
diff <- dom$bill - flat
print(data.frame(id = dom$id, units = dom$units, block = dom$bill, flat = flat, saving_if_flat = diff))
cat("Domestic customers better off on flat rate:", paste(dom$id[diff > 0], collapse=", "), "\n")
# break-even consumption
be <- uniroot(function(u) {
  ch <- 0; rem <- u; bl <- c(10,15,25,Inf); pr <- c(2800,3400,4100,4900)
  for (b in 1:4) { x <- min(rem, bl[b]); ch <- ch + x*pr[b]; rem <- rem - x; if (rem <= 0) break }
  ch - 3700*u }, c(1, 200))$root
cat("Break-even consumption (m3):", round(be, 2), "\n")

# (e) simulation
set.seed(2026)
sim_units <- rpois(10000, lambda = mean(history["C07", ]))
sim_bills <- vapply(sim_units, tariff_bill, numeric(1))
cat("C07 mean of last 3 months:", round(mean(history["C07", ]), 3), "\n")
cat("P(C07 bill > 280,000):", mean(sim_bills > 280000), "\n")
cat("Mean simulated bill:", round(mean(sim_bills)), "\n")
cat("95th percentile bill:", quantile(sim_bills, 0.95), "\n")
