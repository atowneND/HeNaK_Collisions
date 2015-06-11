      double precision FUNCTION DSIMP(N,F)
C     DSIMP:  D. L. HUESTIS, SRI 04-19-85
C     LAST MODIFICATION:  04-19-85
C
C     SIMPSON'S RULE NUMERICAL INTEGRATION, based on
C     Isaacson and Keller, "Analysis of Numerical Methods,"
C     Wiley (1966) Chapter 7
C
C     For even numbers of points we use the 3-point rule until
C     the center of the interval, and then connect these either with
C     the 4-point rule or integration over a single interval using
C     four data points:
C       INT(x1,x2) = (h/24)*(- f0 + 13*f1 + 13*f2 - f3)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION F(*)
      DSIMP=0.0D0
      IF(N-2)900,100,110
C
C     For two points use trapezoidal rule ------------
C
100   DSIMP=0.5D0*(F(1)+F(2))
      GO TO 900
C
C     Check for odd or even number of points ---------
C
110   ODD=0.0D0
      EVEN=0.0D0
      M=N/4
      M2=2*M
      GO TO (300,200,320,200),(N-4*M+1)
C
C     Odd Number of Points --------------------------
C
200   IF(N.LT.5)GO TO 220
      DO 210 I=3,N-2,2
210   ODD=ODD+F(I)
220   DO 230 J=2,N-1,2
230   EVEN=EVEN+F(J)
      DSIMP=(F(1)+F(N)+2.0D0*ODD+4.0D0*EVEN)/3.0D0
      GO TO 900
C
C     Even number of points -------------------------
C
300   IF(N.GT.4)GO TO 310
      DSIMP=3.0D0*(F(1)+3.0D0*(F(2)+F(3))+F(4))/8.0D0
      GO TO 900
310   I1=M2-3
      DSIMP=17.0D0*(F(M2-1)+F(M2+2))+27.0D0*(F(M2)+F(M2+1))
      GO TO 330
C
320   I1=M2-1
      DSIMP=31.0D0*(F(M2)+F(M2+3))+21.0D0*(F(M2+1)+F(M2+2))
330   J1=M2-2
      N1=N+1
      DSIMP=DSIMP+8.0D0*(F(1)+F(N))
      IF(J1.LT.2)GO TO 350
      DO 340 J=2,J1,2
340   EVEN=F(J)+EVEN+F(N1-J)
350   IF(I1.LT.3)GO TO 390
      DO 360 I=3,I1,2
360   ODD=F(I)+ODD+F(N1-I)
390   DSIMP=(DSIMP+16.0D0*ODD+32.0D0*EVEN)/24.0D0
900   RETURN
      END
