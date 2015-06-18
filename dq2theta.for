***** C:\wkarea\adcode\blam\dq2theta.for created Friday, June 14, 2013 at 7:55
***** Peet Hickman

*     program to read the list of d_q coefficients and calculate the corresponding
*     function of angle by evaluating the Legendre sum
*           f(theta)  = sum d(q) * pq(cos theta)

*     This version ignores comment lines (begining with #) in the input
*     except for extracting j and jp from one comment line.
*     The input file must be EXACTLY the output file produced by dqcalc

*     This version writes the coefficients Blambda(j,jp) to a file
*     named Blam_j_jp.  .

      implicit     none
      integer      in,out,j,jp,count,nchar,qend,i,QMAX,q,ntheta,blamfile
     -             ,ix,LAMMAX,lam,lammin
      parameter    (QMAX=200,LAMMAX=200)
      double precision d(0:QMAX),P(0:QMAX),tmp, x,PI,thdeg,thdegmin,
     -             thdegmax,thdegdel,ftheta,dzero,thw,thx,sumBlam,fsave,
     -             thxsave,y
      double precision Blamda(0:LAMMAX),lambda(0:LAMMAX),angle(0:LAMMAX)
      parameter    (PI = 4.d0*atan(1.0d0))
      character    line*80
      logical      :: found = .false.

*     calls
      integer      GetLine,fassign
      character    fnbc     ! returns first nonblank character of string
      external     abort
      double precision fmax

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

*     now construct a file name that includes j and jp
      write (line,'("Blam_",i2.2,"_",i2.2,"_dj",i2.2".dat")') j,jp,jp-j
      IF(fassign(blamfile,trim(line),2).gt.0) THEN
*         write (*,*) 'Blamda will be written to '//trim(line)
      ELSE
         call abort('problem opening file for Blambda')
      END IF

*     skip 1 header line, read 2*jmin+1 values of Blambda (all commented),
      nchar = GetLine(in,line,nchar)
      !write(blamfile,*)line(1:nchar)
      tmp = 0.d0   ! tmp will accumulate sum of (2*lam+1)*Blam
      DO i=1,2*min(j,jp)+1
         nchar = GetLine(in,line,nchar)
         ix = index(line,'#')+1
         read (line(ix:),*) lam,Blamda(lam),angle(lam)
         tmp = tmp + dble(2*lam+1)*Blamda(lam)
         !write(blamfile,*) line(ix:nchar)
      END DO
      sumBlam = tmp    ! will use this later
*      write (*,*) 'sum (2*lam+1)*Blam = ',sumBlam
      
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
      thdegdel=0.1d0

      ntheta = nint( (thdegmax-thdegmin)/thdegdel )
      thdegdel = (thdegmax-thdegmin)/dble(ntheta)
      thw = -2.d0
      thx = -1.d0
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
         write(out,'(f10.1,2f15.7)') thdeg, ftheta,
     -      ftheta*sin(thdeg*PI/180.d0)
         ! check if we've passed the first max of ftheta*sin(theta)
         tmp = ftheta * sin(thdeg*PI/180.d0)
         IF (.not.found .and. tmp.lt.thx) THEN
            fsave = fmax(thw,thx,tmp,y)
*            write (*,'('' first max found at '',f15.7)')
*     -         thdeg-thdegdel + y*thdegdel
            write (*,*) thdeg-thdegdel+y*thdegdel
*            write (*,'(f15.7,'' was first max'')') fsave
            write (*,*) fsave
            thxsave = thx
            found = .true.
         ELSE
            thw = thx
            thx = tmp
         END IF
      END DO

*     write Blamda to blamfile and include normalized Blamda such that
*     (2*lam_min + 1) * B(lam_min) = thx
      lammin = abs(j-jp)
      if (lammin.eq.0) call abort('oops -- do not choose j = jp')
      tmp = thxsave/dble(2*lammin+1)/Blamda(lammin)
      write (blamfile,*)'# lam  B-lambda(j,jp)  angle    '//
     -                          ' normed B-lam*(2*lam+1)'
      DO lam = abs(j-jp),j+jp
         write (blamfile,'(i5,2f12.4,es16.6)')
     -    lam, Blamda(lam), angle(lam), Blamda(lam)*tmp*dble(2*lam+1)
      END DO
 

      end


      
      function fmax(fm,f0,fp,x)
      
*     find max value of a function, given three equally spaced points
      
      implicit     none
      double precision fmax,fm,f0,fp,x,d1,d2
      double precision, parameter :: ZERO=0.d0, HALF=0.5d0, ONE=dble(1),
     -                                TWO=2.d0
      d1 = HALF*(fp-fm)
      d2 = fm - TWO*f0 + fp
      IF (d1.eq.ZERO) THEN
         fmax = f0
         x = ZERO
         return
      ELSE IF (d2.eq.ZERO) THEN
         IF (fm.lt.fp) THEN
            fmax = fp
            x = ONE
         ELSE
            fmax = fm
            x = -ONE
         END IF
      ELSE
         x = -d1 / d2
         fmax = f0 + x*d1 +HALF*x*x*d2
      END IF

      end
      
