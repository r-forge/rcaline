read.cal3qhc <- function(file) {
	
	fd <- file(file, "rt")
	readline <- function(...) scan(fd, what="", quote="'", nlines=1, ...)
	
	line1 <- readline()
	
	JOB 	<- as.character(line1[1])
	ATIM 	<- as.real(line1[2])
	Z0 		<- as.real(line1[3])
	VS 		<- as.real(line1[4])
	VD 		<- as.real(line1[5])
	NR 		<- as.integer(line1[6])
	SCAL 	<- as.real(line1[7])
	IOPT 	<- as.integer(line1[8])
	IDEBUG 	<- as.integer(line1[9])
	
	receptors <- data.frame(
		x = numeric(0),
		y = numeric(0),
		z = numeric(0))
		
	for(i in 1:NR) {
		line2 = readline()
		RCP <- as.character(line2[1])
		XR <- as.real(line2[2])
		YR <- as.real(line2[2])
		ZR <- as.real(line2[2])
		receptor.record <- data.frame(
			x = XR, y = YR, z = ZR, row.names = RCP)
		receptors <- rbind(receptors, receptor.record)
	}
	
	line3 <- readline()
	print(line3)
	
	RUN <- as.character(line3[1])
	NL <- as.integer(line3[2])
	NM <- as.integer(line3[3])
	PRINT2<- as.integer(line3[4])
	MODE <- as.character(line3[5])
	
	links <- data.frame(
		x0 = numeric(0),
		y0 = numeric(0),
		x1 = numeric(0),
		y1 = numeric(0),
		width = numeric(0),
		height = numeric(0))
	
	for(i in 1:NL) {
		line4 <- readline()
		IQ <- as.integer(line4[1])
		
		print(IQ)
		
		if(IQ == 2) {
		
			stop("Unsupported input format. Only free flow links supported at this time.")
		
			line5a <- readline()
			LNK <- as.character(line5a[1])
			TYP <- as.character(line5a[2])
			XL1 <- as.real(line5a[3])
			YL1 <- as.real(line5a[4])
			XL2 <- as.real(line5a[5])
			YL2 <- as.real(line5a[6])
			HL <- as.real(line5a[7])
			WL <- as.real(line5a[8])
			NLANES <- as.integer(line5a[9])
			
			line5b <- readline()
			CAVG <- as.integer(line5b[1])
			RAVG <- as.integer(line5b[2])
			YFAC <- as.real(line5b[3])
			IV <- as.integer(line5b[4])
			IDLFAC <- as.real(line5b[5])
			SFR <- as.integer(line5b[6])
			ST <- as.integer(line5b[7])
			AT <- as.integer(line5b[8])
			
		} else {
				
			line5c <- readline()
			LNK <- as.character(line5c[1])
			TYP <- as.character(line5c[2])
			XL1 <- as.real(line5c[3])
			YL1 <- as.real(line5c[4])
			XL2 <- as.real(line5c[5])
			YL2 <- as.real(line5c[6])
			HL <- as.real(line5c[7])
			WL <- as.real(line5c[8])
			
			link.record <- data.frame(
				x0 = XL1, y0 = YL1, x1 = XL2, y2 = YL2,
				classification = TYP,
				width = WL, height = HL,
				row.names = LNK)
				
		}
		
		links <- rbind(links, link.record)
	}
	
	close(fd)
	
	list(
		receptors = receptors,
		links = links
	)
}

#setwd('~/Dropbox/Projects/Rcaline/caline/')
#fn <- 'extdat/CAL3QHC/EX1.DAT'
#read.cal3qhc(fn)