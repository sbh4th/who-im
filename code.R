library(tidyverse)

Quintile:1
Deaths= 91  Pop= 1086
Estimated IMR= 83.8  SE(IMR)= 8.8

Quintile:2
Deaths= 56  Pop= 840
Estimated IMR= 66.7  SE(IMR)= 8.9

Quintile:3
Deaths= 63  Pop= 845
Estimated IMR= 74.6  SE(IMR)= 9.4

Quintile:4
Deaths= 38  Pop= 718
Estimated IMR= 52.9  SE(IMR)= 8.6

Quintile:5
Deaths= 26  Pop= 671
Estimated IMR= 38.7  SE(IMR)= 7.6

set.seed(4861)
q1pop <- 1000
q1r <- rbinom(q1pop, size = 1, prob = 100/1000)

q2pop <- 850
q2r <- rbinom(q2pop, size = 1, prob = 85/1000)

q3pop <- 700
q3r <- rbinom(q3pop, size = 1, prob = 75/1000)

q4pop <- 550
q4r <- rbinom(q4pop, size = 1, prob = 60/1000)

q5pop <- 400
q5r <- rbinom(q5pop, size = 1, prob = 50/1000)

dat <- tibble(
  q = rep(c(1,2,3,4,5), c(1000,850,700,550,400)),
  y = c(q1r, q2r, q3r, q4r, q5r)
)

dat <- dat %>% arrange(q) %>%
  mutate(ppop = 1/length(q),
    rank = (cumsum(ppop) - 0.5 * ppop)/sum(ppop)) %>%
  group_by(q) %>%
  mutate(avrank = mean(rank)) %>%
  ungroup()

library(RStata)
options("RStata.StataVersion" = 16)
options("RStata.StataPath"= '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')

s_est <- '
glm y avrank, link(logit) family(binomial) robust nolog cformat(%4.3f)
margins, at(avrank=(0 1)) post
nlcom (SII: (_b[2._at] - _b[1._at])), cformat(%4.3f)
* symmetric 95%CIs using nlcom for ratio
nlcom (RII: _b[2._at] / _b[1._at]), cformat(%4.3f)

* use the lnRII instead and then exponentiate
nlcom (lnRII: ln(_b[2._at] / _b[1._at])), post

* should be non-symmetric
lincom lnRII, eform cformat(%4.3f)
'
stata(s_est, data.in=dat)

library(marginaleffects)
mod <- glm(y ~ avrank, data = dat, family = "binomial")

```{r sii-est, message = F}
t2m <- lm(svi_rate ~ ridit, data = t2)
summary(t2m)

sii <- avg_comparisons(t2m, comparison = function(hi, lo) 
  hi - lo, vcov = "HC2")
rii <- avg_comparisons(t2m, comparison = function(hi, lo) 
  hi / lo, vcov = "HC2")

sii %>% bind_rows(rii) %>%
  add_column(measure = c("SII", "RII")) %>%
  select(measure, estimate, conf.low, conf.high) %>%
  kbl(digits = 2, escape = F,
      col.names = c("Measure", "Estimate", "95% LL", "95% UL")) %>%
  add_header_above(c(" " = 1, 
                     "Slope and Relative Index of Inequality (SVI)" = 3)) %>%
  kable_classic(html_font = "Helvetica", full_width = F) %>%
  footnote(general = "Standard errors adjusted for heteroskedasticity")
```


