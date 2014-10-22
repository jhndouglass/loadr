data(HA)
data(till)
data(test)
data(wyre)
data(wensum)
data(stour)

## wensum
wensumLAM <- LAM(wensum$Q, wensum$TP)
summary(wensumLAM)
LAMplot(wensumLAM)

wensumLAM0 <- LAM0(wensum$Q, wensum$TP)
summary(wensumLAM0)
LAMplot(wensumLAM0)

wensumLAMnlxb <- LAMnlxb(wensum$Q, wensum$TP)
summary(wensumLAMnlxb)
LAMnlplot(wensumLAMnlxb)

wensumLAMnlxb0 <- LAMnlxb0(wensum$Q, wensum$TP)
summary(wensumLAMnlxb0)
LAMnlplot(wensumLAMnlxb0)

## till
tillLAM <- LAM(till$Q, till$TP)
summary(tillLAM)
LAMplot(tillLAM)

tillLAM0 <- LAM0(till$Q, till$TP)
summary(tillLAM0)
LAMplot(tillLAM0)

tillLAMnlxb <- LAMnlxb(till$Q, till$TP)
summary(tillLAMnlxb)
LAMnlplot(tillLAMnlxb)

tillLAMnlxb0 <- LAMnlxb0(till$Q, till$TP)
summary(tillLAMnlxb0)
LAMnlplot(tillLAMnlxb0)

## test
testLAM <- LAM(test$Q, test$TP)
summary(testLAM)
LAMplot(testLAM)

testLAM0 <- LAM0(test$Q, test$TP)
summary(testLAM0)
LAMplot(testLAM0)

testLAMnlxb <- LAMnlxb(test$Q, test$TP)
summary(testLAMnlxb)
LAMnlplot(testLAMnlxb)

testLAMnlxb0 <- LAMnlxb0(test$Q, test$TP)
summary(testLAMnlxb0)
LAMnlplot(testLAMnlxb0)

## HALAM
HALAM <- LAM(HA$Q, HA$TP)
summary(HALAM)
LAMplot(HALAM)

HALAM0 <- LAM0(HA$Q, HA$TP)
summary(HALAM0)
LAMplot(HALAM0)

HALAMnlxb <- LAMnlxb(HA$Q, HA$TP)
summary(HALAMnlxb)
LAMnlplot(HALAMnlxb)

HALAMnlxb0 <- LAMnlxb0(HA$Q, HA$TP)
summary(HALAMnlxb0)
LAMnlplot(HALAMnlxb0)

## stour
stourLAM <- LAM(stour$Q, stour$TP)
summary(stourLAM)
LAMplot(stourLAM)

stourLAM0 <- LAM0(stour$Q, stour$TP)
summary(stourLAM0)
LAMplot(stourLAM0)

stourLAMnlxb <- LAMnlxb(stour$Q, stour$TP)
summary(stourLAMnlxb)
LAMnlplot(stourLAMnlxb)

stourLAMnlxb0 <- LAMnlxb0(stour$Q, stour$TP)
summary(stourLAMnlxb0)
LAMnlplot(stourLAMnlxb0)

## wyre
wyreLAM <- LAM(wyre$Q, wyre$TP)
summary(wyreLAM)
LAMplot(wyreLAM)

wyreLAM0 <- LAM0(wyre$Q, wyre$TP)
summary(wyreLAM0)
LAMplot(wyreLAM0)

wyreLAMnlxb <- LAMnlxb(wyre$Q, wyre$TP)
summary(wyreLAMnlxb)
LAMnlplot(wyreLAMnlxb)

wyreLAMnlxb0 <- LAMnlxb0(wyre$Q, wyre$TP)
summary(wyreLAMnlxb0)
LAMnlplot(wyreLAMnlxb0)
