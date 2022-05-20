# source("helpers.R")
# requiet("flexsurv")
# mod <- flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = "weibull")
#
# get_predict(mod, newdata = head(bc))
#
# get_vcov(mod)
#
# get_predict(mod, newdata = head(bc))
# marginaleffects(mod, variables = "group")
#
# nd <- datagrid(newdata = bc)
# comparisons(mod, newdata = nd)
#
# b <- get_coef(mod)
# b <- setNames(rep(0, length(b)), names(b))
# mod2 <- set_coef(mod, b)
#
# predict(mod2, newdata = head(bc))
# predict(mod, newdata = head(bc))
#
