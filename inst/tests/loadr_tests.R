load("../data/wensum.rdata")
load("../data/till.rdata")
load("../data/test.rdata")
load("../data/wyre.rdata")
load("../data/HA.rdata")
load("../data/stour.rdata")

## test my function
wensumnls <- LAMnls(wensum$Q, wensum$TP)
wensumnls
wensumLAM <- LAM(wensum$Q, wensum$TP)
summary(wensumLAM)
plot(wensumLAM)
plot(wensumnls)
# nls() and optim() both get same answers - however we must use the optim-nls
# route for nls() to work at all. Fannying about with letting B be negative
# also worked for both but physically not right (see Bowes et al 2008)

tillnls <- LAMnls(till$Q, till$TP)
tillnls <- LAMnls(till$Q, till$TP, pars)
tillnls
plot(tillnls)
tillLAM <- LAMmaster(till$Q, till$TP)
summary(tillLAM)
plot(tillLAM)
# gets same answer without but nls() must go down optim-nls route. Starting B at
# zero or 1 doesn't seem to HAve an affect.


# start tests from here again.....
testnls <- LAMnls(test$Q, test$TP)
testnls <- LAMnls(test$Q, test$TP, pars)
testnls
plot(testnls)
testLAM <- LAMmaster(test$Q, test$TP)
summary(testLAM)
plot(testLAM)
# gets convergeerror for first nls pass then zero A parameter in optim().
# Warning and error messages working as they should.

HAnls <- LAMnls(HA$Q, HA$TP, method = "plinear")
HAnls
plot(HAnls)
HALAM <- LAMmaster(HA$Q, HA$TP)

# singular graident error with nls() for various algorithim types - don't think
# data is suitable - can't cHAnge model formulation.


HALAM05 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 0.5, C = 1, D = 1))
summary(HALAM05)
plot(HALAM05)

HALAM06 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 0.6, C = 1, D = 1))
summary(HALAM06)
plot(HALAM06)

HALAM07 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 0.7, C = 1, D = 1))
summary(HALAM07)
plot(HALAM07)

HALAM08 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 0.8, C = 1, D = 1))
summary(HALAM08)
plot(HALAM08)

HALAM09 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 0.9, C = 1, D = 1))
summary(HALAM09)
plot(HALAM09)

HALAM1 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 1, C = 1, D = 1))#, b.upper = 10)
summary(HALAM1)
plot(HALAM1)

HALAM11 <- LAMmaster(HA$Q, HA$TP, pars = c(A = 1, B = 1.1, C = 1, D = 1))
plot(HALAM11)
summary(HALAM11)


# HA both B and D parameters come out close to 1 (x ^ 0 always = 1) so you get
# lines of gradient 0, suspect tHAt data with an insignificant Q term in an
# lm(conc ~ Q) model will get similar results. Approach probably won't work where
# concentration is fairly stable with flow.

# Note tHAt when B & D are close to 1 - the starting parameters A & C are very
# sesnsitive to starting values of B. I HAve now set the default starting value
# for B to 1. This is to solve the issue of getting non-finite errors when there
# is little Co ~ Q slope to work with.

HALAMlm <- lm(TP ~ Q, data = HA)
summary(HALAMlm)
plot(TP ~ Q, data = HA)
abline(HALAMlm)

stournls <- LAMnls(stour$Q, stour$TP)
summary(stournls)
plot(stournls)
stourLAM <- LAMmaster(stour$Q, stour$TP)
stourLAM
summary(stourLAM)
plot(stourLAM)
# works fine - HAs to use optim to nls route though.

wyrenls <- LAMnls(wyre$Q, wyre$TP)
wyrenls
plot(wyrenls)
wyreLAM <- LAMmaster(wyre$Q, wyre$TP)
wyreLAM
plot(wyreLAM)
# A comes out of zero in optim() - maybe will cHAnge stop() to warning()