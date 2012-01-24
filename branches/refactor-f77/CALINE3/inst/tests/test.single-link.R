context("single link")

XR <- 30.0
YR <- 0.0
ZR <- 1.8

XL1 <- 0.0
YL1 <- -5000.0
XL2 <- 0.0
YL2 <- 5000.0
WL <- 30.0
HL <- 0.0
NTYP <- 1
VPHL <- 7500.0
EFL <- 30.0

U <- 1.0
BRG <- 270.0
CLAS <- 6
MIXH <- 1000.0

ATIM <- 60.0
Z0 <- 10.0
VS <- 0.0
VD <- 0.0

test_that('single link', {

	C.ugm3 <- CALINE3.array(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD
	)

	C.ppm <- C.ugm3 * 0.0245 / 28.0
	expect_equal(
		round(C.ppm, digits=1),
		array(4.6, c(1, 1))
	)

})