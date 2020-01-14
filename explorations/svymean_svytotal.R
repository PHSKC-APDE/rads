
library('survey', quietly = T)
data(api)
apiclus1$elem = apiclus1$stype=='E'
a = svydesign(id=~dnum, weights=~pw, data=apiclus1)
r1 = svymean(~stype,a)
r2 = svytotal(~stype, a)

all(r2/sum(r2) == r1)

r1 * sum(1/a$prob)
SE(r1) * sum(1/a$prob)


r3 = svymean(~elem,a)
r4 = svytotal(~elem, a)

#blah
b = data.table(pw = 1/a$prob, val = a$variables$stype)
setorder(b, val)
b = b[, .(pw = sum(pw)), by = val]
b[, psum := sum(pw)]
b[, smean := as.numeric(r1)]
b[, stotal := as.numeric(r2)]
b[, smean_se := SE(r1)]
b[, stotal_se := SE(r2)]
#svy mean
pweights<-1/design$prob
psum<-sum(pweights)
x*pweights/psum

#svy total
x/design$prob
