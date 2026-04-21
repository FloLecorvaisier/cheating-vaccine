## Needed libraries

library(ggplot2)   # Plots
library(patchwork) # Assembling plots
library(tidyr)     # Data manipulation


## Figure 1

df = expand.grid(
  alpha = c(0.5, 1, 1.5),
  eta = c(0, 0.5, 1),
  g = c(0, 0.1, 0.2),
  q1 = seq(0, 1, 0.01)
)

df = within(df, {
  W1 = (1 - q1 - g) * ((1 - eta) * q1 + g) ** alpha
})

my_lab = as_labeller(c("0.5" = "α==0.5",
                       "1"   = "α==1.0",
                       "1.5" = "α==1.5",
                       "0"   = "g==0",
                       "0.1" = "g==0.1",
                       "0.2" = "g==0.2"),
                     default = label_parsed)

ggplot(df) +
  facet_grid(g ~ alpha, labeller = my_lab) +
  geom_line(aes(x = q1, y = W1, group = eta, color = as.factor(eta))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = paste0("grey", c(20, 50, 80))) + 
  labs(
    color = bquote("Vaccine efficacy ("*η*")"),
    x = bquote("Public good production ("*q[1]*")"), 
    y = bquote("Fitness ("*W[1]*")"))+ 
  coord_cartesian(ylim = c(0, .4)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-onestrain-W.pdf", device = cairo_pdf, width = 14, height = 10, units = "cm")


## Figure 2

df = expand.grid(
  alpha = c(0.5, 1, 1.5),
  eta = seq(0, 1, 0.01),
  g = c(0, 0.1, 0.2)
)

df = within(df, {
  q1star = alpha / (alpha + 1) - g * (alpha * (1 - eta) + 1) / ((1 - eta) * (alpha + 1))
})

my_lab = as_labeller(
  c("0" = "g==0", "0.1" = "g==0.1", "0.2" = "g==0.2"),
  default = label_parsed
)

ggplot(df) +
  facet_wrap(~ g, labeller = my_lab) +
  geom_line(aes(x = eta, y = q1star, group = alpha, color = as.factor(alpha))) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  scale_color_manual(values = paste0("grey", c(20, 50, 80))) + 
  labs(
    color = bquote("Shape parameter ("*α*")"),
    x = bquote("Vaccine efficacy ("*η*")"), 
    y = bquote("Optimal public good prod. ("*q[1]^"\u2605"*")"))+ 
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-onestrain-qstar.pdf", device = cairo_pdf, width = 14, height = 8, units = "cm")


## Figure 3

df = expand.grid(
  alpha = c(0.5, 1, 1.5),
  g = seq(0.0, 1, length.out = 101)
)

df = within(df, {
  eta1prime = 1 - g / (alpha * (1 - g))
})

ggplot(df) +
  geom_line(aes(x = g, y = eta1prime, color = as.factor(alpha))) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  labs(
    x = bquote("Private good production ("*g*")"),
    y = bquote("Vaccine efficacy threshold ("*η[1]^minute*")"),
    color = bquote("Shape parameter ("*α*")")
  ) +
  scale_color_grey() +
  coord_cartesian(xlim = c(0, 1.0), ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-onestrain-etaprime.pdf", device = cairo_pdf, width = 10, height = 8, units = "cm")


## Figure 4

df = expand.grid(
  alpha = 0.5,
  eta = 0.4,
  g = 0.05,
  q2.1 = c(0.25, 0.5, 0.75),
  lambda = c(0.2, 0.4, 0.6),
  x2.1 = seq(0, 1, 0.01)
)

df = within(df, {
  W2.1 = (1 - q2.1 - g) * ((1 - lambda) * (1 - eta) * q2.1 + lambda * (1 - eta) * x2.1 * q2.1 + g) ** alpha
  W2.2 = (1 - g) * (lambda * (1 - eta) * x2.1 * q2.1 + g) ** alpha
})

df = pivot_longer(df, cols = c("W2.1", "W2.2"))

my_lab = as_labeller(c("0.2"  = "λ==0.2",
                       "0.4"  = "λ==0.4",
                       "0.6"  = "λ==0.6",
                       "0.25" = "q[2.1]==0.25",
                       "0.5"  = "q[2.1]==0.5",
                       "0.75" = "q[2.1]==0.75"), 
                     default = label_parsed)

ggplot(df) +
  facet_grid(q2.1 ~ lambda, labeller = my_lab) +
  geom_line(aes(x = x2.1, y = value, linetype = name)) +
  scale_linetype(labels = c("2.1", "2.2")) +
  scale_color_grey() +
  labs(
    x = bquote("Proportion of cooperator cells ("*x[2.1]*")"),
    y = bquote("Fitness ("*W[i]*")"),
    linetype = "Cell type") +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-coopcheat-W.pdf", device = cairo_pdf, width = 14, height = 10, units = "cm")


## Figure 5

alpha = 0.5
eta = 0.4
g = 0.05
q2.1 = c(0.35, 0.5, 0.65)
lambda = 0.2

tmax = 100
nind = 1e4

xall = numeric()
sall = numeric()
iall = numeric()

for (seed in 1:5) {
  for (i in 1:3) {
    set.seed(seed)
    ncoop = sample(1:nind, 1)
    q = rep(c(q2.1[i], 0), times = c(ncoop, nind - ncoop))
    x2.1 = sum(q == q2.1[i]) / length(q)
    
    for (t in 2:tmax) {
      W = (1 - q - g) * ((1 - lambda) * (1 - eta) * q + lambda * (1 - eta) * sum(q) / length(q) + g) ** alpha
      q = sample(q, length(q), replace = T, prob = W)
      x2.1[t] = sum(q == q2.1[i]) / length(q)
    }
    xall = c(xall, x2.1)
    sall = c(sall, rep(seed, tmax))
    iall = c(iall, rep(i, tmax))
  }
  df = data.frame(
      x = xall,
      t = 1:tmax,
      s = sall,
      i = iall
    )
}

x2.1star = ((1 - q2.1 - g) ** (1 / alpha) * ((1 - lambda) * (1 - eta) * q2.1 + g) - (1 - g) ** (1 / alpha) * g) /
  (lambda * (1 - eta) * ((1 - g) ** (1 / alpha) - (1 - q2.1 - g) ** (1 / alpha)) * q2.1)

ggplot(df) +
  geom_line(aes(x = t, y = x, group = interaction(s, i), color = as.factor(i))) +
  geom_hline(yintercept = x2.1star, linetype = "dotted") +
  scale_color_manual(values = paste0("grey", c(20, 50, 80)), labels = q2.1) + 
  labs(
    x = "Time (arbitrary unit)",
    y = bquote("Proportion of cooperator cells ("*x[2.1]*")"),
    color = bquote("Public good production ("*q[2.1]*")")
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom")

# ggsave("fig/fig-coopcheat-stocha.pdf", device = cairo_pdf, width = 14, height = 9, units = "cm")


## Figure 6

df = expand.grid(
  alpha = 0.5,
  eta = c(0.3, 0.5, 0.7),
  g = c(0.00, 0.05, 0.1, 0.15),
  q2.1 = seq(0, 1, 0.01),
  lambda = c(0.2, 0.4, 0.6)
)

df = within(df, {
  x2.1star = 
    ((1 - q2.1 - g) ** (1 / alpha) * ((1 - lambda) * (1 - eta) * q2.1 + g) - (1 - g) ** (1 / alpha) * g) /
    (lambda * (1 - eta) * ((1 - g) ** (1 / alpha) - (1 - q2.1 - g) ** (1 / alpha)) * q2.1)
})

my_lab = as_labeller(c("0.2" = "λ==0.2",
                       "0.4" = "λ==0.4",
                       "0.6" = "λ==0.6",
                       "0.3" = "η==0.25",
                       "0.5" = "η==0.5",
                       "0.7" = "η==0.75"), 
                     default = label_parsed)

ggplot(df) +
  facet_grid(eta ~ lambda, labeller = my_lab) +
  geom_line(aes(x = q2.1, y = x2.1star, color = as.factor(g))) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  labs(
    x = bquote("Public good production ("*q[2.1]*")"),
    y = bquote("Optimal proportion of cooperator cells ("*x[2.1]^"\u2605"*")"),
    color = bquote("Private good production ("*g*")")
  ) +
  scale_color_grey() +
  coord_cartesian(xlim = c(0, 0.6), ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-coopcheat-xstar.pdf", device = cairo_pdf, width = 14, height = 10, units = "cm")


## Figure 7

df = expand.grid(
  lambda = c(0.2, 0.4, 0.6),
  q2.1 = seq(0, 1, 0.01),
  g = c(0.05, 0.1, 0.15),
  alpha = c(0.5, 1.0, 1.5)
)

df = within(df, {
  etaprime = 1 - (((1 - g) ** (1 / alpha) - (1 - q2.1 - g) ** (1 / alpha)) * g) / ((1 - lambda) * (1 - q2.1 - g) ** (1 / alpha) * q2.1)
  etaprime[q2.1 > 1 - g - 0.001] = -1 # To prevent ggplot from showing non-mathematical lines
})

my_lab = as_labeller(c("0.2"  = "λ==0.2",
                       "0.4"  = "λ==0.4",
                       "0.6"  = "λ==0.6",
                       "0.05" = "g==0.05",
                       "0.1"  = "g==0.1",
                       "0.15" = "g==0.15"), 
                     default = label_parsed)

ggplot(df) +
  facet_grid(g ~ lambda, labeller = my_lab) +
  geom_line(aes(x = q2.1, y = etaprime, group = alpha, color = as.factor(alpha))) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  scale_color_manual(values = paste0("grey", c(20, 50, 80))) + 
  labs(
    color = bquote("Shape parameter ("*α*")"),
    x = bquote("Public good production ("*q[2.1]*")"), 
    y = bquote("Vaccine efficacy theshold ("*η[2.1]^minute*")"))+ 
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-coopcheat-etaprime.pdf", device = cairo_pdf, width = 14, height = 10, units = "cm")


## Figure 8

lambda = 0.1
eta = 0.2
g = .05
alpha = 0.5

df = expand.grid(
  q3.1 = seq(0, 1 - g, length.out = 501), 
  q3.2 = seq(0, 1 - g, length.out = 501)
)

W3.1 = (1 - df$q3.1 - g) * ((1 - eta) * df$q3.1 + g) ** alpha
W3.2 = (1 - df$q3.2 - g) * ((1 - lambda) * (1 - eta) * df$q3.2 + lambda * (1 - eta) * df$q3.1 + g) ** alpha

df$omega = W3.2 - W3.1

xstar = ((1 - df$q3.2 - g) ** (1 / alpha) * ((1 - eta) * df$q3.2 + g) - 
  (1 - df$q3.1 - g) ** (1 / alpha) * ((1 - eta) * ((1 - lambda) * df$q3.1 + lambda * df$q3.2) + g)) /
  (lambda *(1 - eta)*((1 - df$q3.1 - g) ** (1 / alpha) - (1 - df$q3.2 - g) ** (1 / alpha)) * (df$q3.1 - df$q3.2))

xstar[xstar < 0] = 0
xstar[xstar > 1] = 1
xstar[xstar < 1 & xstar > 0] = 0.5
xstar[is.nan(xstar)] = 0

ggplot(df) +
  geom_tile(aes(x = q3.1, y = q3.2, fill = as.factor(xstar)), show.legend = F) +
  labs(
    x = bquote("Resident public good production ("*q[3.1]*")"),
    y = bquote("Mutant public good production ("*q[3.2]*")")
  ) +
  scale_fill_grey(start = 0.1, end = 0.9) +
  coord_cartesian(expand = F, xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-adaptive-pip.pdf", device = cairo_pdf, width = 12, height = 8, units = "cm")


## Figure 9

set.seed(42)

alpha = 0.5
eta = 0.2
g = 0.05
q3.1 = 0.75
lambda = 0.4

tmax = 5e3
nind = 1e3
mu = 1e-3

xall = numeric(tmax)
q = rep(q3.1, nind)

n = numeric()
val = numeric()
tval = numeric()

for (t in 1:tmax) {
  id_mut = which(rbinom(nind, 1, mu) == 1)
  q[id_mut] = q[id_mut] + runif(length(id_mut), -0.1, 0.1)
  q[q < 0] = 0
  q[q > 1 - g] = 1 - g

  W = (1 - q - g) * ((1 - lambda) * (1 - eta) * q + lambda * (1 - eta) * sum(q) / length(q) + g) ** alpha

  q = sample(q, length(q), replace = T, prob = W)

  n = c(n, table(q))
  val = c(val, names(table(q)))
  tval = c(tval, rep(t, times = length(table(q))))
}

qavg = numeric(tmax)
for (i in 1:tmax) {
  qavg[i] = as.numeric(names(sort(n[tval == i], decreasing = T)[1]))
}

val2 = val[val %in% qavg]
tval2 = tval[val %in% qavg]
n2 = n[val %in% qavg]

p1 = ggplot() +
  geom_line(aes(x = tval2, y = n2 / nind, group = val2, 
    color = factor(val2, levels = sample(unique(val2), length(unique(val2))))), show.legend = F) +
  scale_color_grey() +
  labs(x = "Time (arbitrary unit)", y = bquote("Prop. of cells ("*x[i]*")")) +
  theme_bw(base_family = "serif")

q3.1star = alpha / (alpha + 1 / (1 - lambda)) - g * (alpha * (1 - eta) + 1 / (1 - lambda)) / ((1 - eta) * (alpha + 1 / (1 - lambda)))
q1star   = alpha / (alpha + 1               ) - g * (alpha * (1 - eta) + 1               ) / ((1 - eta) * (alpha + 1 )              )

p2 = ggplot() +
  geom_hline(yintercept = q3.1star, linetype = "dashed") +
  geom_hline(yintercept = q1star, linetype = "dotted") +
  geom_line(aes(x = 1:tmax, y = qavg), color = "grey50") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time (arbitrary unit)", y = bquote("Public good prod. ("*q[i]*")")) +
  theme_bw(base_family = "serif")

Wavg  = (1 - qavg  - g) * ((1 - lambda) * (1 - eta) * qavg  + lambda * (1 - eta) * qavg  + g) ** alpha
W1star   = (1 - q1star   - g) * ((1 - lambda) * (1 - eta) * q1star   + lambda * (1 - eta) * q1star   + g) ** alpha
W3.1star = (1 - q3.1star - g) * ((1 - lambda) * (1 - eta) * q3.1star + lambda * (1 - eta) * q3.1star + g) ** alpha

p3 = ggplot() +
  geom_line(aes(x = 1:tmax, y = Wavg), color = "grey50") +
  geom_hline(yintercept = W3.1star, linetype = "dashed") +
  geom_hline(yintercept = W1star, linetype = "dotted") +
  labs(x = "Time (arbitrary unit)", y = bquote("Clonal fitness ("*W[i]*")")) +
  theme_bw(base_family = "serif")

p1 / (p2 | p3) + plot_annotation(tag_levels = "A")

# ggsave("fig/fig-adaptive-stocha.pdf", device = cairo_pdf, width = 14, height = 11, units = "cm")


## Figure 10

df = expand.grid(
  alpha = c(0.5, 1, 1.5),
  lambda = c(0.2, 0.4, 0.6),
  g = seq(0, 1, length.out = 101)  
)

df = within(df, {
  eta3.1prime = 1 - g / (alpha * (1 - lambda) * (1 - g))
})

my_lab = as_labeller(c("0.2" = "λ==0.2",
                       "0.4" = "λ==0.4",
                       "0.6" = "λ==0.6"),
                     default = label_parsed)

ggplot(df) +
  facet_grid(~ lambda, labeller = my_lab) +
  geom_line(aes(x = g, y = eta3.1prime, color = as.factor(alpha))) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  scale_color_grey() + 
  labs(
    color = bquote("Shape parameter ("*α*")"),
    x = bquote("Private good production ("*g*")"), 
    y = bquote("Vaccine efficacy theshold ("*η[3.1]^minute*")"))+ 
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("fig/fig-adaptive-etaprime.pdf", device = cairo_pdf, width = 14, height = 8, units = "cm")