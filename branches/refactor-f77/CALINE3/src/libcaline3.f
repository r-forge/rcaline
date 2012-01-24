C     Copyright © 2011 The Regents of the University of California (Regents).
C     All Rights Reserved. Permission to use, copy, modify, and distribute
C     this software and its documentation for educational, research, and
C     not-for-profit purposes, without fee and without a signed licensing
C     agreement, is hereby granted, provided that the above copyright notice,
C     this paragraph and the following two paragraphs appear in all copies,
C     modifications, and distributions. Contact The Office of Technology
C     Licensing, UC Berkeley, 2150 Shattuck Avenue, Suite 510, Berkeley, CA
C     94720-1620, (510) 643-7201, for commercial licensing opportunities.
C
C     Created by David Holstius, Department of Environmental Health Sciences,
C     University of California, Berkeley.
C
C     IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
C     SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
C     ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
C     REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C
C     REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
C     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
C     PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY,
C     PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION TO
C     PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
C
      SUBROUTINE CALINE3_MATRIX(
     +                   NR,      ! number of receptors
     +                   XR,      ! x-coordinates of receptors
     +                   YR,      ! y-coordinates of receptors
     +                   ZR,      ! z-coordinates (heights) of receptors
     +                   NL,      ! number of links
     +                   XL1,     ! x-coordinates of link vertices
     +                   YL1,     ! y-coordinates of link vertices
     +                   XL2,     ! x-coordinates of link vertices
     +                   YL2,     ! y-coordinates of link vertices
     +                   WL,      ! link widths
     +                   HL,      ! link heights
     +                   NTYP,    ! link classifications *as integers*
     +                   VPHL,    ! traffic volume (per link)
     +                   EFL,     ! emission factor (per link)
     +                   U,       ! wind speed
     +                   BRG,     ! wind bearing
     +                   CLAS,    ! atmospheric stability class
     +                   MIXH,    ! mixing height
     +                   ATIM,    ! averaging time
     +                   Z0,      ! surface roughness
     +                   VS,      ! settling velocity
     +                   VD,      ! deposition velocity
     +                   C)       ! resulting concentrations (per receptor)

      PARAMETER(PI=3.1415926,RAD=PI/180.,DEG=180./PI)

      DIMENSION C(NR,NL)

      DOUBLE PRECISION HYP,SIDE,FAC2,PD,A,B,L,D,
     +    XPRI,YPRI,APRI,BPRI,LPRI,DPRI,XD,YD,D1,D2,
     +    LL(NL),INTG(6)

      INTEGER CLAS

      REAL NE,LIM,KZ,LB,INC
      REAL V1,YE,Z,EXP1,EXP2,DVIR,CSL2

      REAL*4 XR(NR),YR(NR),ZR(NR)
      REAL*4 XL1(NL),YL1(NL),XL2(NL),YL2(NL),WL(NL),HL(NL)
      REAL*4 VPHL(NL),EFL(NL)
      REAL*4 U,BRG,MIXH

      INTEGER NTYP(NL)
      PARAMETER(NTYP_AG=1,NTYP_BR=2,NTYP_FL=3,NTYP_DP=4)

      REAL*4 AY1(6),AY2(6),AZ(6)
      REAL*4 Y(6),WT(5)

      DATA AZ/1112.,556.,353.,219.,124.,56./
      DATA AY1/0.46,0.29,0.18,0.11,0.087,0.057/
      DATA AY2/1831.,1155.,717.,438.,346.,227./
      DATA WT/0.25,0.75,1.,0.75,0.25/

      DREF=ALOG(10000.)

      V1=VD-VS/2.

      DO 1050 I=1,NL
        LL(I)=SQRT((XL1(I)-XL2(I))**2+(YL1(I)-YL2(I))**2)
 1050 CONTINUE

      BRG=BRG+180.
      IF (BRG.GE.360.) BRG=BRG-360.

      AFAC=(ATIM/3.0)**.2
      IF (CLAS.GT.6) CLAS=6
      SY1=ALOG(AY1(CLAS)*((Z0/3.)**.2)*AFAC)
      SY10=ALOG(AY2(CLAS)*((Z0/3.)**.07)*AFAC)
      PY1=EXP(SY1)
      PY2=(SY10-SY1)/DREF
      SZ10=ALOG(AZ(CLAS)*((Z0/10.)**.07)*AFAC)

C *****  LINK LOOP  *****
      DO 8000 IL=1,NL

      IF (NTYP(IL).EQ.NTYP_DP .OR. NTYP(IL).EQ.NTYP_FL) THEN
        H=0.
      ELSE
        H=HL(IL)
      END IF
      W=WL(IL)
      W2=W/2.

      Q1=0.1726*VPHL(IL)*EFL(IL)

      XD=XL2(IL)-XL1(IL)
      YD=YL2(IL)-YL1(IL)
      ABSXD=DABS(XD)
      IF(ABSXD.GT.LL(IL)) LL(IL)=ABSXD
      LB=DEG*(DACOS(DABS(XD)/LL(IL)))
      IF (XD.GT.0. .AND. YD.GE.0.) LB=90.-LB
      IF (XD.GE.0. .AND. YD.LT.0.) LB=90.+LB
      IF (XD.LT.0. .AND. YD.LE.0.) LB=270.-LB
      IF (XD.LE.0. .AND. YD.GT.0.) LB=270.+LB

      PHI=ABS(BRG-LB)
      IF (PHI.LE.90.) GO TO 7600
      IF (PHI.GE.270.) GO TO 5000
      PHI=ABS(PHI-180.)
      GO TO 7600
 5000 PHI=ABS(PHI-360.)
 7600 IF (PHI.LT.20.) GO TO 7630
      IF (PHI.LT.50.) GO TO 7620
      IF (PHI.LT.70.) GO TO 7610
      BASE=4.
      GO TO 7650
 7610 BASE=2.
      GO TO 7650
 7620 BASE=1.5
      GO TO 7650
 7630 BASE=1.1

 7650 PHI=RAD*(PHI)
      IF (PHI.GT.1.5706) PHI=1.5706
      IF (PHI.LT.0.00017) PHI=0.00017

C *****  DEPRESSED SECTION  *****
      IF (HL(IL).LT.-1.5) GO TO 7700
      DSTR=1.
      HDS=1.
      GO TO 7800
 7700 HDS=HL(IL)
      DSTR=0.72*ABS(HDS)**0.83
C     ! RESIDENCE TIME FACTOR

C *****  SIGMA Z POWER CURVE  *****
 7800 TR=DSTR*W2/U
C     ! RESIDENCE TIME
      SGZ1=ALOG((1.8+0.11*TR)*(ATIM/30.)**0.2)
C     ! ALOG(SIGMA Z) AT W2
      PZ2=(SZ10-SGZ1)/(DREF-ALOG(W2))
      PZ1=EXP((SZ10+SGZ1-PZ2*(DREF+ALOG(W2)))/2.)

C *****  END OF LINK ROUTINE  *****

C *****  RECEPTOR LOOP  *****
      DO 6000 IR=1,NR

      A=(XR(IR)-XL1(IL))**2+(YR(IR)-YL1(IL))**2
      B=(XR(IR)-XL2(IL))**2+(YR(IR)-YL2(IL))**2
      L=(B-A-LL(IL)**2)/(2.*LL(IL))
C     ! OFFSET LENGTH
      IF (A.GT.L**2) D=DSQRT(A-L**2)
      IF (A.LE.L**2) D=0.
C     ! RECEPTOR DISTANCE
      UWL=LL(IL)+L
C     ! UPWIND LENGTH
      DWL=L
C     ! DOWNWIND LENGTH
      IF(D.EQ.0.D0)DVIR=1.D0
      IF(D.NE.0.D0)DVIR=D
      XPRI=XR(IR)+DVIR*COS(RAD*(450.-BRG))
      YPRI=YR(IR)+DVIR*SIN(RAD*(450.-BRG))
      APRI=(XPRI-XL1(IL))**2+(YPRI-YL1(IL))**2
      BPRI=(XPRI-XL2(IL))**2+(YPRI-YL2(IL))**2
      LPRI=(BPRI-APRI-LL(IL)**2)/(2.*LL(IL))
      IF (APRI.GT.LPRI**2) DPRI=DSQRT(APRI-LPRI**2)
      IF (APRI.LE.LPRI**2) DPRI=0.
      IF (DPRI.LT.D) D=-D

      IF (LPRI-L) 5725,5735,5735
 5725 TEMP=UWL
      UWL=-DWL
      DWL=-TEMP
 5735 IF (NTYP(IL).EQ.NTYP_AG .OR. NTYP(IL).EQ.NTYP_BR) GO TO 5750
C
      D1=W2+2.*ABS(HL(IL))
      D2=W2
C     ! SINGLE PRECISION TO DOUBLE PRECISION FOR LOGICAL 'IF'
      IF (DABS(D).GE.D1) GO TO 5750
C     ! 2:1 SLOPE ASSUMED
      IF (DABS(D).LE.D2) THEN
        Z=ZR(IR)-HL(IL)
      ELSE
        Z=ZR(IR)-HL(IL)*(1.-(DABS(D)-W2)/(2.*ABS(HL(IL))))
      END IF
      GO TO 3050
 5750 Z=ZR(IR)

C *****************************
C *****  CALINE3 ROUTINE  *****
C *****************************
C
 3050 SGN=1.
 3060 NE=0.
      STP=1.
      FINI=1.
      IF (SGN.EQ.1. .AND. UWL.LE.0. .AND. DWL.LT.0.) SGN=-1.
 3080 IF (SGN.EQ.-1. .AND. UWL.GT.0. .AND. DWL.GE.0.) GO TO 6000

C *****  ELEMENT LOOP  *****
      ED1=0.
      ED2=SGN*W

 3110 IF (SGN.EQ.-1.) GO TO 3160
      IF (ED1.LE.DWL .AND. ED2.LE.DWL) GO TO 3770
      IF (ED1.GT.DWL .AND. ED2.LT.UWL) GO TO 3250
      IF (ED1.LE.DWL) ED1=DWL
      IF (ED2.LT.UWL) GO TO 3250
      ED2=UWL
      SGN=-1.
      NE=-1.
      GO TO 3250

 3160 IF (ED1.GE.UWL .AND. ED2.GE.UWL) GO TO 3770
      IF (ED1.LT.UWL .AND. ED2.GT.DWL) GO TO 3250
      IF (ED1.GE.UWL) ED1=UWL
      IF (ED2.GT.DWL) GO TO 3250
      ED2=DWL
      FINI=0.

 3250 EL2=ABS(ED2-ED1)/2.
      ECLD=(ED1+ED2)/2.
      ELL2=W2/COS(PHI)+(EL2-W2*TAN(PHI))*SIN(PHI)
      IF (PHI.GE.ATAN(W2/EL2)) THEN
        CSL2=W2/SIN(PHI)
      ELSE
        CSL2=EL2/COS(PHI)
      END IF
      EM2=ABS((EL2-W2/TAN(PHI))*SIN(PHI))
      EN2=(ELL2-EM2)/2.

C *****  RECEPTOR DISTANCE LOOP  *****
      QE=Q1*CSL2/W2
      FET=(ECLD+D*TAN(PHI))*COS(PHI)
      HYP=ECLD**2+D**2
      SIDE=FET**2
      IF (SIDE.GT.HYP) YE=0.
      IF (SIDE.LE.HYP) YE=DSQRT(HYP-SIDE)

C *****  DETERMINE SIGMA Y AND SIGMA Z  *****
      IF (FET.LE.-CSL2) GO TO 3830
      IF (FET.GE.CSL2) GO TO 3320
      QE=QE*(FET+CSL2)/(2.*CSL2)
      FET=(CSL2+FET)/2.
 3320 SGZ=PZ1*FET**PZ2
      KZ=SGZ**2*U/(2.*FET)
      SGY=PY1*FET**PY2
      FAC1=0.399/(SGZ*U)

C *****  ADJUSTMENT FOR ELEMENT END EFFECT  *****
C           (POLYNOMIAL APPROXIMATION)
      Y(1)=YE+ELL2
      Y(2)=Y(1)-EN2
      Y(3)=Y(2)-EN2
      Y(4)=Y(3)-2*EM2
      Y(5)=Y(4)-EN2
      Y(6)=Y(5)-EN2

C ***  SUB-ELEMENT SOURCE STRENGTH LOOP
      DO 3480 I=1,6
        LIM=ABS(Y(I)/SGY)
        T=1./(1.+0.23164*LIM)
        ARG=LIM**2/(-2.)
        IF (LIM.GT.5.) THEN
            INTG(I)=0.
        ELSE
          INTG(I)=0.3989*EXP(ARG)*(0.3194*T-0.3566*T**2+
     *    1.7815*T**3-1.8213*T**4+1.3303*T**5)
        END IF
 3480 CONTINUE

      FAC2=0.
      DO 3530 I=1,5
        IF ((SIGN(1.,Y(I))).EQ.(SIGN(1.,Y(I+1)))) THEN
          PD=DABS(INTG(I+1)-INTG(I))
        ELSE
          PD=1.-INTG(I)-INTG(I+1)
        END IF
        FAC2=FAC2+PD*QE*WT(I)
 3530 CONTINUE

      FACT=FAC1*FAC2

C *****  DEPRESSED SECTION  *****
C
      IF (HDS.LT.-1.5 .AND. DABS(D).LT.(W2-3.*HDS)) THEN
 3560   IF (DABS(D).LE.W2) THEN
          FACT=FACT*DSTR
        ELSE
          FACT=FACT*(DSTR-(DSTR-1.)*(DABS(D)-W2)/(-3.*HDS))
        END IF
      END IF

C *****  DEPOSITION CORRECTION  *****
C
 3580 FAC3=0.
      IF (V1.NE.0.) THEN
        ARG=V1*SGZ/(KZ*SQRT(2.))+(Z+H)/(SGZ*SQRT(2.))
        IF (ARG.GT.5.) GO TO 3770
        T=1./(1.+0.47047*ARG)
        EFRC=(.3480242*T-.0958798*T**2+.7478556*T**3)*EXP(-1.*ARG**2)
        FAC3=(SQRT(2.*PI)*V1*SGZ*EXP(V1*(Z+H)/KZ+.5*(V1*SGZ/KZ)**2)
     *    *EFRC)/KZ
        IF (FAC3.GT.2.) FAC3=2.
      END IF

C *****  SETTLING CORRECTION  *****
 3670 IF (VS.NE.0.) THEN
        FAC4=EXP(-VS*(Z-H)/(2.*KZ)-(VS*SGZ/KZ)**2/8.)
        FACT=FACT*FAC4
      END IF

C *****  INCREMENTAL CONCENTRATION  *****
 3710 FAC5=0.
      CNT=0.
 3720 EXLS=0.
 3730 ARG1=-0.5*((Z+H+2.*CNT*MIXH)/SGZ)**2
      IF (ARG1.LT.-44.) THEN
        EXP1=0.
      ELSE
        EXP1=EXP(ARG1)
      END IF
      ARG2=-0.5*((Z-H+2.*CNT*MIXH)/SGZ)**2
      IF (ARG2.LT.-44.) THEN
        EXP2=0.
      ELSE
        EXP2=EXP(ARG2)
      END IF
      FAC5=FAC5+EXP1+EXP2
      IF (MIXH.GE.1000.) GO TO 3760
      IF ((EXP1+EXP2+EXLS).EQ.0. .AND. CNT.LE.0.) GO TO 3760
 3740 IF (CNT.GT.0.) GO TO 3750
      CNT=ABS(CNT)+1.
      GO TO 3720
 3750 CNT=-1.*CNT
      EXLS=EXP1+EXP2
      GO TO 3730

 3760 INC=FACT*(FAC5-FAC3)
      C(IR,IL)=C(IR,IL)+INC

 3770 IF (FINI.EQ.0.) GO TO 6000
      NE=NE+1.
      STP=BASE**NE
      IF (NE.EQ.0.) GO TO 3080
      ED1=ED2
      ED2=ED2+SGN*STP*W
      GO TO 3110

 3830 IF (SGN.EQ.1.) GO TO 3770
C
C *****  END OF CALINE3 ROUTINE  *****
C
 6000 CONTINUE

 8000 CONTINUE

 9000 END SUBROUTINE