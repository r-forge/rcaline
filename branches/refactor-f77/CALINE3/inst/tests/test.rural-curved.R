require(CALINE3)
require(testthat)

context("libcaline3")

NR <- 4
XR <- c(400, 100, 200, 100)
YR <- c(1700, 1500, 1300, 350)
ZR <- rep(1.8, NR)

NL <- 10
XL1 <- c(-707, 0, 120, 150, 150, 175, 265, 350, 475, 650)
YL1 <- c(-707, 0, 175, 350, 1350, 1510, 1640, 1760, 1830, 1850)
XL2 <- c(0, 120, 150, 150, 175, 265, 350, 475, 650, 1650)
YL2 <- c(0, 175, 350, 1350, 1510, 1640, 1760, 1830, 1850, 1850)
NTYP <- rep(1, NL)
HL <- rep(0, NL)
WL <- rep(28, NL)
VPHL <- rep(8500, NL)
EFL <- rep(30, NL)

U <- 1
BRG <- 45
CLAS <- 6
MIXH <- 1000

ATIM <- 60.0
Z0 <- 50.0
VS <- 0.0
VD <- 0.0

C.ugm3 <- CALINE3.array(
	XR, YR, ZR,
	XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
	U, BRG, CLAS, MIXH,
	ATIM, Z0, VS, VD
)

C.ppm <- C.ugm3 * 0.0245 / 28.0

expect_equal(round(C.ppm, digits=1),
	structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.8, 
		0, 0, 0, 0, 0, 1.5, 0, 0, 0, 3.7, 0, 0, 0, 2.1, 0, 0, 3.1, 0.4, 
		0.1, 0, 0, 0, 1.3, 0.5), .Dim = c(4L, 10L)
	)
)