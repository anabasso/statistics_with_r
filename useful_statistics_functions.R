# Probability Z is larger than 2 (two-sided)
# P(|Z| > 2)
pnorm(2, lower.tail = F) * 2

# Probability that T with 50 degrees of freedom is larger than 2 (two-sided)
# P(|tdf = 50| > 2)
pt(2, df = 50, lower.tail = F) * 2

# Probability that T with 10 degrees of freedom is larger than 2 (two-sided)
# P(|tdf = 10| > 2)
pt(2, df = 10, lower.tail = F) * 2

# Probability that T with 1000 degrees of freedom is larger than 2 (two-sided)
# P(|tdf = 1000| > 2)
pt(2, df = 1000, lower.tail = F) * 2

# Power of a T-Test: Finding N
power.t.test(power = 0.8, delta = -3, sd = 12, type = 'two.sample', alternative = 'two.sided')

# Finding p with a Z score -- normal curve
pnorm(-0.2)

# Finding Z with the quartile -- normal curve
qnorm(0.8)
