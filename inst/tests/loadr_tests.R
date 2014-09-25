data(HA)
data(till)
data(test)
data(wyre)
data(wensum)
data(stour)

wensumLAM <- LAM(wensum$Q, wensum$TP)
summary(wensumLAM)
LAMplot(wensumLAM)
wensumLAM

tillLAM <- LAM(till$Q, till$TP)
summary(tillLAM)
LAMplot(tillLAM)

testLAM <- LAM(test$Q, test$TP)
summary(testLAM)
LAMplot(testLAM)

HALAM <- LAM(HA$Q, HA$TP)
summary(HALAM)
LAMplot(HALAM)

stourLAM <- LAM(stour$Q, stour$TP)
summary(stourLAM)
LAMplot(stourLAM)

wyreLAM <- LAM(wyre$Q, wyre$TP)
summary(wyreLAM)
LAMplot(wyreLAM)

## nls
wensumLAMnls <- LAMnls(wensum$Q, wensum$TP)
summary(wensumLAMnls)
LAMnlsplot(wensumLAMnls)

tillLAMnls <- LAMnls(till$Q, till$TP)
summary(tillLAMnls)
LAMnlsplot(tillLAMnls)

testLAMnls <- LAMnls(test$Q, test$TP)
summary(testLAMnls)
LAMnlsplot(testLAMnls)

HALAMnls <- LAMnls(HA$Q, HA$TP)
summary(HALAMnls)
LAMnlsplot(HALAMnls)

stourLAMnls <- LAMnls(stour$Q, stour$TP)
summary(stourLAMnls)
LAMnlsplot(stourLAMnls)

wyreLAMnls <- LAMnls(wyre$Q, wyre$TP)
summary(wyreLAMnls)
LAMnlsplot(wyreLAMnls)
