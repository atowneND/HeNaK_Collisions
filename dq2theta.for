***** C:\wkarea\adcode\blam\dq2theta.for created Friday, June 14, 2013 at 7:55
***** Peet Hickman

*     program to read the list of d_q coefficients and calculate the corresponding
*     function of angle by evaluating the Legendre sum
*           f(theta)  = sum d(q) * pq(cos theta)

*     This version ignores comment lines (begining with #) in the input
*     except for extracting j and jp from one comment line.
*     The input file must be EXACTLY the output file produced by dqcalc

      implicit     none
      integer      in,out,j,jp,count,nchar,qend,i,QMAX,q,ntheta
      parameter    (QMAX=60)
      double precision d(0:QMAX),P(0:QMAX),tmp, x,PI,thdeg,thdegmin,
     -             thdegmax,thdegdel,ftheta,dzero
      parameter    (PI = 4.d0*atan(1.0d0))
      character    line*80

*     calls
      integer      GetLine
      character    fnbc     ! returns first nonblank character of string
      external     abort

      call stdio (in,out)


*     read the input file and echo the list of d(q).  check that q runs from
*     0 to 2 * min(j,jp)

!     read (in,*) j,jp,dzero
!     new section to scan through comments to find j and jp
      DO WHILE (GetLine(in,line,nchar).gt.0)
         i = index(line,'j and jp are')
         IF (i.gt.0) THEN
            read (line(i+12:),*) j,jp
            go to 10
         END IF
      END DO
10    continue
      ! now skip 1 header line plus 2*jmin+1 values of Blambda (all commented)
      DO i=1,2*min(j,jp)+2
         nchar = GetLine(in,line,nchar)
      END DO
 
      qend = 2*min(j,jp)
      count = -1
      DO WHILE (GetLine(in,line,nchar).gt.0)
         count = count + 1
         read (line,*) i,x,tmp   !normalized dq is in third column
         if (i.eq.0) dzero = x
         IF (i.eq.count .and. count.le.QMAX) THEN
            d(count) = tmp
         ELSE
            call abort('problem reading d(q); check size of QMAX')
         END IF
      END DO
      if (count.ne.qend) call abort ('problem: wrong number of d(q)')
      write (out,*) '# List of d(q) for j, j'' = ', j,jp
      write (out,'('' # '',i4,f12.8  )') (q,d(q),q=0,qend)

*     loop over theta.  for the present use simple values to loop
*     from theta=0 to 180 degrees in two degree increments.

      thdegmin=0.d0
      thdegmax=180.d0
      thdegdel=2.d0

      ntheta = nint( (thdegmax-thdegmin)/thdegdel )
      thdegdel = (thdegmax-thdegmin)/dble(ntheta)
      P(0) = 1.d0
      DO i=0,ntheta
         thdeg = thdegmin + dble(i)*thdegdel
         x = cos(thdeg*PI/180.d0)
         P(1) = x
         ftheta = d(0)
         if (qend.ge.1) ftheta = ftheta + 3.d0*d(1)*x
         DO q=1,qend-1           ! now get Pq(x) by recursion
            P(q+1) = (dble(2*q+1)*x*P(q) - dble(q)*P(q-1) )/dble(q+1)
            ftheta = ftheta + dble(2*q+3)*d(q+1)*P(q+1) ! 2q+3 = 2(q+1)+1
         END DO
         ftheta = dzero*ftheta/sqrt(dble((2*j+1)*(2*jp+1)))
         write(out,'(f10.1,f15.7)') thdeg, ftheta
      END DO

      end
