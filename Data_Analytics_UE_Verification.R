# Verification of every calculation in the MSc Data Analytics (Unit 1) examination
r4 <- function(x) round(x, 4)
cat("--- MCQ ---\n")
x <- c(45,52,38,61,52,47,70,55,49,52); cat("Q3 mean", mean(x), "\n")
x <- c(12,7,15,9,20,11); cat("Q4 median", median(x), "mean", r4(mean(x)), "\n")
x <- c(4,6,8,10,12); cat("Q5 var", var(x), "sd", r4(sd(x)), "\n")
cat("Q6 upper fence", 48 + 1.5*(48-30), "\n")
cat("Q7 CV", 50000/200000*100, "%\n")
cat("Q9 P(X=0) binom(4,0.1)", dbinom(0,4,0.1), " P(X>=1)", 1-dbinom(0,4,0.1), "\n")
cat("Q10 mean", 50*0.2, "var", 50*0.2*0.8, "\n")
cat("Q11 Poisson P(X=0) lambda 2", r4(dpois(0,2)), "\n")
cat("Q12 z", (75-60)/10, "\n")
cat("Q14 SE", 12000/sqrt(144), "\n")
cat("Q20 df", (3-1)*(4-1), "\n")

cat("\n--- DQ22 central tendency ---\n")
s <- c(85, 92, 78, 95, 88, 92, 410)
tab <- table(s); cat("mean", r4(mean(s)), "median", median(s), "mode", names(tab)[tab==max(tab)], "\n")
cat("mean without 410:", r4(mean(s[s!=410])), "median without:", median(s[s!=410]), "\n")

cat("\n--- DQ23 dispersion ---\n")
A <- c(8,10,12,10,10); B <- c(2,6,10,14,18)
for (v in list(A=A,B=B)) cat("mean", mean(v), "range", diff(range(v)), "var", var(v), "sd", r4(sd(v)), "CV%", r4(100*sd(v)/mean(v)), "\n")

cat("\n--- DQ24 quartiles/outliers (median-of-halves method) ---\n")
y <- sort(c(12,15,14,18,16,13,17,15,48,14,16)); print(y)
med <- median(y); lo <- y[1:5]; hi <- y[7:11]
Q1 <- median(lo); Q3 <- median(hi); IQR_ <- Q3-Q1
cat("median", med, "Q1", Q1, "Q3", Q3, "IQR", IQR_, "fences", Q1-1.5*IQR_, Q3+1.5*IQR_, "\n")
cat("mean", r4(mean(y)), "\n")

cat("\n--- DQ25 binomial n=8 p=0.10 ---\n")
cat("P0", r4(dbinom(0,8,.1)), "P2", r4(dbinom(2,8,.1)), "P(X>=2)", r4(1-pbinom(1,8,.1)), "mean", 8*.1, "sd", r4(sqrt(8*.1*.9)), "\n")

cat("\n--- DQ26 Poisson lambda=3 ---\n")
cat("P0", r4(dpois(0,3)), "P(X<=2)", r4(ppois(2,3)), "P(X>4)", r4(1-ppois(4,3)), "two weeks P0", r4(dpois(0,6)), "P(X>=10 in 2wks)", r4(1-ppois(9,6)), "\n")

cat("\n--- DQ27 Normal(62, 8) ---\n")
cat("P(X>70)", r4(1-pnorm(70,62,8)), "P(54<X<70)", r4(pnorm(70,62,8)-pnorm(54,62,8)), "P(X<50)", r4(pnorm(50,62,8)), "top10% cut", r4(qnorm(.9,62,8)), "z90", r4(qnorm(.9)), "\n")

cat("\n--- DQ28 CLT mu=45000 sigma=12000 n=64 ---\n")
se <- 12000/sqrt(64); cat("SE", se, "z", (48000-45000)/se, "P(xbar>48000)", r4(1-pnorm(48000,45000,se)), "\n")
cat("P(|xbar-mu|<2000)", r4(pnorm(2000/se)-pnorm(-2000/se)), "n to halve SE", 64*4, "\n")

cat("\n--- DQ29 CI mean n=25 xbar=1.8m s=0.4m ---\n")
tc <- qt(.975, 24); se <- 0.4/sqrt(25); cat("t", r4(tc), "SE", se, "ME", r4(tc*se), "CI", r4(1.8-tc*se), r4(1.8+tc*se), "\n")
cat("n for ME 50000 (z):", (1.96*400000/50000)^2, "-> ", ceiling((1.96*400000/50000)^2), "\n")

cat("\n--- DQ30 CI proportion 228/400 ---\n")
p <- 228/400; se <- sqrt(p*(1-p)/400); cat("p", p, "SE", r4(se), "ME", r4(1.96*se), "CI", r4(p-1.96*se), r4(p+1.96*se), "\n")
cat("n for +/-3% p=0.5:", (1.96^2*.25)/.03^2, "->", ceiling((1.96^2*.25)/.03^2), "\n")

cat("\n--- DQ31 one-sample t: n=16 xbar=11.2 s=2.4 mu0=10 ---\n")
t <- (11.2-10)/(2.4/4); cat("t", t, "df 15 crit one-tail", r4(qt(.95,15)), "p one-tail", r4(1-pt(t,15)), "\n")

cat("\n--- DQ32 proportion z: 148/200 vs 0.80 ---\n")
ph <- 148/200; z <- (ph-.8)/sqrt(.8*.2/200); cat("phat", ph, "SE", r4(sqrt(.8*.2/200)), "z", r4(z), "p two-tailed", r4(2*pnorm(z)), "\n")

cat("\n--- DQ33 two-sample t (NARO) ---\n")
FA <- c(4.2,4.5,3.9,4.8,4.4,4.6); FB <- c(3.8,4.0,3.6,4.1,3.7,4.2)
cat("A mean", r4(mean(FA)), "sd", r4(sd(FA)), "| B mean", r4(mean(FB)), "sd", r4(sd(FB)), "\n")
tt <- t.test(FA, FB, var.equal = TRUE); print(tt)
sp <- sqrt(((6-1)*var(FA)+(6-1)*var(FB))/10); cat("pooled sd", r4(sp), "SE diff", r4(sp*sqrt(1/6+1/6)), "t crit", r4(qt(.975,10)), "\n")
print(t.test(FA, FB)$statistic); print(r4(t.test(FA, FB)$p.value))
cat("Cohen d", r4((mean(FA)-mean(FB))/sp), "\n")

cat("\n--- DQ34 chi-square MTN vs Airtel ---\n")
M <- matrix(c(120,80, 90,60, 50,70, 100,50), ncol=2, byrow=TRUE,
            dimnames=list(c("Central","Eastern","Northern","Western"), c("MTN","Airtel")))
ct <- chisq.test(M, correct=FALSE); print(ct); print(round(ct$expected,2))
cat("contributions:\n"); print(round((M-ct$expected)^2/ct$expected, 4))
cat("crit 5% df3", r4(qchisq(.95,3)), "Cramer V", r4(sqrt(ct$statistic/(sum(M)*1))), "\n")
cat("MTN share overall", r4(sum(M[,1])/sum(M)), "\n")

cat("\n--- DQ35 SACCO loan sample ---\n")
L <- c(1.2, 2.5, 0.8, 3.0, 1.5, 2.0, 1.8, 9.5, 2.2, 1.5)
cat("mean", r4(mean(L)), "median", median(L), "sd", r4(sd(L)), "CV%", r4(100*sd(L)/mean(L)), "\n")
tc <- qt(.975, 9); se <- sd(L)/sqrt(10)
cat("t9", r4(tc), "SE", r4(se), "CI", r4(mean(L)-tc*se), r4(mean(L)+tc*se), "\n")
L2 <- L[L != 9.5]; tc8 <- qt(.975,8); se2 <- sd(L2)/3
cat("without 9.5: mean", r4(mean(L2)), "sd", r4(sd(L2)), "CI", r4(mean(L2)-tc8*se2), r4(mean(L2)+tc8*se2), "t8", r4(tc8), "\n")
srt <- sort(L); print(srt); cat("Q1 (median lower half)", median(srt[1:5]), "Q3", median(srt[6:10]), "\n")

cat("\n--- DQ36 fintech claim n=25 xbar=38 s=15 ---\n")
tc <- qt(.975,24); cat("SE", 15/5, "ME", r4(tc*3), "CI", r4(38-tc*3), r4(38+tc*3), "\n")
t <- (38-30)/3; cat("t vs 30%:", r4(t), "p one-tail", r4(1-pt(t,24)), "\n")
