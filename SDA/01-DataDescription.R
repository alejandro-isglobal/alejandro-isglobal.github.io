# ============================================================
# Chapter 1 — Reproducible Code Script
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

set.seed(1)

suppressPackageStartupMessages({
  library(ggplot2)
})

theme_book <- theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 20)
  )

# ------------------------------------------------------------
# 1. Simulated categorical data (mouse coat color)
# ------------------------------------------------------------

mouse_color <- factor(
  c(rep("grey", 88),
    rep("black", 31),
    rep("albino", 37)),
  levels = c("grey", "black", "albino")
)

mouse_color <- sample(mouse_color)

# Frequency tables
tb_color <- table(mouse_color)
df_color <- data.frame(
  outcome = names(tb_color),
  ni = as.numeric(tb_color),
  fi = as.numeric(prop.table(tb_color))
)

df_color$outcome <- factor(
  df_color$outcome,
  levels = c("grey", "black", "albino")
)

df_color

# ------------------------------------------------------------
# 2. Bar plot of relative frequencies
# ------------------------------------------------------------

ggplot(df_color, aes(x = outcome, y = fi)) +
  geom_col(fill = c("grey", "black", "white"), color = "black") +
  labs(x = "Outcome", y = "Relative frequency", title = "") +
  theme_book

# ------------------------------------------------------------
# 3. Pie chart of categorical distribution
# ------------------------------------------------------------

df_pie <- as.data.frame(table(mouse_color))
colnames(df_pie) <- c("color", "count")

ggplot(df_pie, aes(x = "", y = count, fill = color)) +
  geom_col(width = 1, color = "black", size = 0.8) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "grey" = "grey",
    "black" = "black",
    "albino" = "white"
  )) +
  labs(title = "Relative frequency") +
  theme_void() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 20)
  )

# ------------------------------------------------------------
# 4. Misophonia severity data
# ------------------------------------------------------------

misophonia <- read.delim("./data/Misophonia.txt")

tb_miso <- table(misophonia$Misophonia.Severity)

df_miso <- data.frame(
  outcome = 0:4,
  ni = as.vector(tb_miso),
  fi = as.vector(prop.table(tb_miso)),
  Ni = cumsum(tb_miso),
  Fi = cumsum(prop.table(tb_miso))
)

df_miso

# Empirical cumulative distribution (step plot)

dat_ecdf <- data.frame(
  x = c(-1, df_miso$outcome, 5),
  Fi = c(0, df_miso$Fi, 1)
)

ggplot(dat_ecdf, aes(x = x, y = Fi)) +
  geom_step(color = "black") +
  labs(x = "x", y = "F(x)", title = "") +
  theme_book

# ------------------------------------------------------------
# 5. Tektite age data
# ------------------------------------------------------------

age <- unlist(read.table("./data/40Ar_39Ar.txt"))

# Histogram (coarse bins)

ggplot(data.frame(age = age), aes(x = age)) +
  geom_histogram(bins = 9, fill = "steelblue", color = "black") +
  labs(x = "Age (Ma)", y = "Absolute frequency", title = "") +
  theme_book

# Histogram (fine bins)

ggplot(data.frame(age = age), aes(x = age)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(x = "Age (Ma)", y = "Absolute frequency", title = "") +
  theme_book

# ------------------------------------------------------------
# 6. Empirical CDF
# ------------------------------------------------------------

ggplot(data.frame(age = age), aes(x = age)) +
  stat_ecdf(geom = "step", color = "black") +
  labs(x = "Age (Ma)", y = "F(x)") +
  theme_book

# ------------------------------------------------------------
# 7. Mean and median illustrations
# ------------------------------------------------------------

mean_age <- mean(age)
median_age <- median(age)

ggplot(data.frame(age = age), aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 fill = "lightgray",
                 color = "black") +
  geom_point(aes(x = mean_age, y = -0.02),
             shape = 17, size = 3, color = "black") +
  geom_point(aes(x = median_age, y = -0.02),
             shape = 16, size = 3, color = "black") +
  annotate("text", x = mean_age - 0.9, y = -0.05,
           label = "Mean", vjust = 1) +
  annotate("text", x = median_age + 1.1, y = -0.05,
           label = "Median", vjust = 1) +
  labs(x = "Age (Ma)", y = "Density") +
  coord_cartesian(ylim = c(-0.1, 0.8)) +
  theme_book

# ------------------------------------------------------------
# 8. Dispersion simulation
# ------------------------------------------------------------

set.seed(1)

group1 <- data.frame(age = rnorm(1000, 100, 7.5))
group2 <- data.frame(age = rnorm(1000, 100, 20))

ggplot() +
  geom_histogram(data = group1,
                 aes(x = age, y = ..density..),
                 bins = 20,
                 fill = "steelblue",
                 alpha = 0.5,
                 color = "black") +
  geom_histogram(data = group2,
                 aes(x = age, y = ..density..),
                 bins = 20,
                 fill = "lightgrey",
                 alpha = 0.2,
                 color = "black") +
  labs(x = "X", y = "Density") +
  theme_book +
  theme(legend.position = "none")

# ------------------------------------------------------------
# 9. Quartiles and IQR
# ------------------------------------------------------------

q1 <- quantile(age, 0.25)
q3 <- quantile(age, 0.75)

ggplot(data.frame(age = age), aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 fill = "lightgray",
                 color = "black") +
  geom_point(aes(x = q1, y = -0.02),
             shape = 17, size = 2, color = "blue") +
  geom_point(aes(x = q3, y = -0.02),
             shape = 17, size = 2, color = "red") +
  annotate("text", x = q1 - 1.5, y = -0.01,
           label = "q0.25", color = "blue") +
  annotate("text", x = q3 + 1.5, y = -0.01,
           label = "q0.75", color = "red") +
  labs(x = "Age (Ma)", y = "Density") +
  coord_cartesian(ylim = c(-0.1, 0.8)) +
  theme_book

# ------------------------------------------------------------
# 10. Outlier filtering and boxplot
# ------------------------------------------------------------

sel_outlier <- abs(age - median(age)) > 1.5 * IQR(age)

ggplot(data.frame(age = age[!sel_outlier]),
       aes(x = "", y = age)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "", y = "Age (Ma)") +
  theme_book

# ------------------------------------------------------------
# 11. Additional boxplot example (misophonia)
# ------------------------------------------------------------

df_convex <- data.frame(
  Convexity = misophonia$Convexity.Angle
)

ggplot(df_convex, aes(x = "", y = Convexity)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "", y = "Convexity angle") +
  theme_book

# ------------------------------------------------------------
# 12. Random experiment examples
# ------------------------------------------------------------

set.seed(123)
outcomes_discrete <- sample(1:12, 8, replace = TRUE)

set.seed(123)
outcomes_continuous <- runif(10, 0, 10)

# ============================================================
# End of Chapter 1 script
# ============================================================
