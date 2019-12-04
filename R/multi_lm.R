library(magrittr)
library(tidyr)
library(dplyr)
library(microbenchmark)

?lm.fit

edata <- as.data.frame(matrix(runif(50*80000), nrow = 80000, ncol = 50))
pdata <- data.frame(x = c(1:50))
rownames(pdata) <- colnames(edata)

edata
pdata

mod = model.matrix( ~ pdata$x)
mod
str(mod)

fit = lm.fit(mod, t(edata))
fit

str(fit)
fit$residuals

t(fit$residuals)

#New Method
microbenchmark::microbenchmark(fit = lm.fit(mod, t(edata)),
                               unit = "s",
                               times = 10)

pdata$ID <- rownames(pdata)
edata$eID <- rownames(edata)

tidy_d <- edata %>%
  tidyr::gather(key = "ID", value = "cor", -eID) %>%
  dplyr::left_join(pdata, by = "ID")

df_list <-  tidy_d %>%
  base::split(.$eID)

str(df_list)

fit2 <- map(df_list,
    function(d) { lm(cor ~ x, data = d) })

#Old Method
microbenchmark::microbenchmark(fit2 <- map(df_list,
                                           function(d) { lm(cor ~ x, data = d) }),
                               unit = "s",
                               times = 5)





#TIME TO DO 1 Million Regressions
edata <- as.data.frame(matrix(runif(50*1000000), nrow = 1000000, ncol = 50))
pdata <- data.frame(x = c(1:50))
rownames(pdata) <- colnames(edata)

edata
pdata

mod = model.matrix( ~ pdata$x)
mod
str(mod)

fit = lm.fit(mod, t(edata))
fit

str(fit)
fit$residuals

t(fit$residuals)

#New Method
microbenchmark::microbenchmark(fit = lm.fit(mod, t(edata)),
                               unit = "s",
                               times = 10)

#WOW ~ 2 seconds
