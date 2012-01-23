context("urban freeway")

NR <- 12
XR <- c(-350, 0, 750, 850, -850, -550, -350, 50, 450, 800, -550, -550)
YR <- c(30, 30, 100, 30, -100, -100, -100, -100, -100, -100, 25, 25)
ZR <- c(rep(1.8, NR-1), 6.1)

NL <- 6
XL1 <- c(500, 500, -3000, -3000, -500, -100)
YL1 <- c(0, 0, 0, -75, 200, 200)
XL2 <- c(3000, 1000, 500, 3000, -500, -100)
YL2 <- c(0, 100, 0, -75, -300, -200)
NTYP <- factor(c('AG', 'DP', 'AG', 'AG', 'BR', 'BR'), levels=c('AG', 'BR', 'FL', 'DP'))
HL <- c(0, -2.0, 0, 0, 6.1, 6.1)
WL <- c(23, 13, 23, 23, 27, 27)
VPHL <- c(9700, 1200, 10900, 9300, 4000, 5000)
EFL <- c(30, 150, 30, 30, 50, 50)

test_that('array lengths', {
	expect_equal(length(XR), NR)
	expect_equal(length(YR), NR)
	expect_equal(length(ZR), NR)
	expect_equal(length(XL1), NL)
	expect_equal(length(YL1), NL)
	expect_equal(length(XL2), NL)
	expect_equal(length(YL2), NL)
	expect_equal(length(HL), NL)
	expect_equal(length(WL), NL)
	expect_equal(length(VPHL), NL)
	expect_equal(length(EFL), NL)
})

U <- 1
CLAS <- 6
MIXH <- 1000

ATIM <- 60.0
Z0 <- 100.0
VS <- 0.0
VD <- 0.0

test_that('case 1', {
	BRG <- 0
	C.ugm3 <- CALINE3.array(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD
	)
	C.ppm <- C.ugm3 * 0.0245 / 28.0
	expect_equal(
		round(C.ppm, digits=1),
		structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 3.2, 0, 0, 0, 0, 0, 2.8, 
			0, 0, 0, 0, 0, 1.4, 0, 0, 0, 0, 0, 0, 3.6, 3.6, 3.6, 3.6, 3.6, 
			0, 0, 0, 0, 0, 0, 0, 6, 6, 6, 6, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0.3, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
			.Dim = c(NR, NL))
	)
})

test_that('case 2', {
	BRG <- 90
	C.ugm3 <- CALINE3.array(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD
	)
	C.ppm <- C.ugm3 * 0.0245 / 28.0
	expect_equal(
		round(C.ppm, digits=1),
		structure(c(5.7, 8, 3.1, 12, 3.1, 3.5, 3.8, 3.9, 3.5, 3.1, 4.9, 
			4.8, 1.3, 2.1, 2.6, 0, 0.4, 0.3, 0.3, 0.1, 0, 0, 1, 1, 8.9, 5.8, 
			0, 0, 1.8, 1.1, 0.7, 0, 0, 0, 12.2, 11.6, 3.9, 3.6, 0.9, 2.6, 
			15.4, 15.1, 15, 14.5, 14.1, 13.5, 4.3, 4.3, 0, 0, 0, 0, 1.1, 
			2.3, 0, 0, 0, 0, 2.3, 2, 1.6, 0, 0, 0, 0.9, 1.2, 1.6, 0, 0, 0, 1.2, 1.2), 
			.Dim = c(NR, NL))
	)
})

test_that('case 3', {
	BRG <- 180
	C.ugm3 <- CALINE3.array(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD
	)
	C.ppm <- C.ugm3 * 0.0245 / 28.0
	expect_equal(
		round(C.ppm, digits=1),
		structure(c(0, 0, 3.2, 5.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.5, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 6.5, 6.5, 0, 0, 0, 0, 0, 0, 0, 0, 
			7, 3.8, 3, 3, 2.3, 3, 0, 0, 0, 0, 0, 0, 3.1, 2.6, 0, 0, 0, 0, 
			0, 0.1, 0, 0, 0, 0, 0.4, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
			.Dim = c(NR, NL))
	)
})

test_that('case 4', {
	BRG <- 270
	C.ugm3 <- CALINE3.array(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD
	)
	C.ppm <- C.ugm3 * 0.0245 / 28.0
	expect_equal(
		round(C.ppm, digits=1),
		structure(c(0, 0, 0, 3.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.2, 
			0, 0, 0, 0, 0, 0, 0, 0, 14.3, 14.8, 5.3, 11.9, 3.4, 3.8, 4.1, 
			4.6, 5, 5.3, 16.3, 15.7, 3.3, 3.6, 2, 4.3, 13.4, 13.9, 14.2, 
			14.6, 15, 15.3, 3.3, 3.2, 1.6, 0.9, 0.5, 0.5, 0, 0, 1.6, 0.9, 
			0.7, 0.6, 0, 0, 0, 2.4, 0.8, 0.8, 0, 0, 0, 2, 1.1, 0.8, 0, 0), 
			.Dim = c(NR, NL))
	)
})

warning('
For the urban freeway cases:
  Some concentrations differ by <0.1 ppm CO compared to CALINE3 User Guide.
  Not sure if this is data input error, compiler differences, or something else.
  Tests have not been written exactly against CALINE3 User Guide examples.')