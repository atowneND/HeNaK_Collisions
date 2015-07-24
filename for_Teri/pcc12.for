***** C:\wkarea\adcode\blam\pcc1.for created Sunday, June 26, 2011 at 14:14
***** Peet Hickman

      implicit     none
      integer      in,out,nchar,lp,jmin,jmax,maxlamb,j,jp,nj,lam,
     -             MAXNOJ,itmp,jj,k,MAXLAMBMAX,MAXJ,CSIZE,jB,jpB,lamB,
     -             count,jbarx2,jdel,OAfile
      parameter    (MAXNOJ=51,MAXLAMBMAX=100,MAXJ=50,CSIZE=200)
      character    line*128,rootname*64,filename*64
      logical      skip
      double precision Brow(MAXNOJ),fac(MAXNOJ),Blambda(0:MAXLAMBMAX),
     -             B(0:MAXLAMBMAX,(MAXNOJ*(MAXNOJ+1))/2),
!    -             frac(2,(MAXNOJ*(MAXNOJ+1))/2),
     -             angle(0:MAXLAMBMAX),
     -             frac(2,0:MAXNOJ,0:MAXNOJ),
     -             Ajdelj(0:MAXNOJ,-MAXNOJ:MAXNOJ),
     -             Ajsumdif(2*MAXNOJ,MAXNOJ,2),   ! sum,dif,moment (1 or 2)
     -             clist(CSIZE),Q(0:2*MAXJ), THRESH,sum,sig,
     -             pfac, phase, j1min,j1max,phase2,rawsig,ratio,x,
     -             PI,jph,jpph,lamph,scx,tmp   ! sc=semiclassical approx
      double precision amtrans(0:MAXLAMBMAX) ! sig(0 -> lambda)
      parameter    (THRESH=1.d-8, PI=4.d0*atan(1.0d0) )

*     calls:
      integer      GetLine,fassign,dassign,index
      character    strR8*16
      double precision d3j
      external     abort

      call stdio (in,out)

*     read the B file (which is specified as input file on command line)
*     get the root of the file name
      inquire(unit=in,name=line)
      itmp = index(line,'.')
      IF (itmp.gt.1) THEN
         rootname = line(1:itmp-1)
      ELSE
         stop 'having trouble getting the root file name'
      END IF

*     skip lines that have # in col 1 or col 2; then
*     skip one more line
      skip = .true.
      DO WHILE (skip)
         nchar = GetLine(in,line,nchar)
         call PutLine(out,line(1:nchar))
         lp = index(line(1:nchar),'#')
         if (lp.ne.1 .and. lp.ne.2) skip = .false.
      END DO
      nchar = GetLine(in,line,nchar)

      read (in,*) jmin,jmax,maxlamb
      write (out,*) 'jmin,jmax,maxlamb are ',jmin,jmax,maxlamb
      nj = jmax-jmin+1
      if (nj.gt.MAXNOJ) call abort('main: MAXNOJ too small')

      read (in,*) (fac(jj),jj=1,nj)

      count = (nj*(nj+1))/2
      DO lam=0,maxlamb
        read (in,*) itmp
        if (lam.ne.itmp) call abort('main: unexpected lambda')
        read (in,*) (B(lam,k),k=1,count)
      END DO

*     write(*,*) 'Enter j and jp'
*     read (*,*) j,jp

      DO jj=1,MAXNOJ
         DO k=1,MAXNOJ
            Ajsumdif(jj,k,1) = 0.d0
            Ajsumdif(jj,k,2) = 0.d0
         END DO
      END DO

      count = 0
      DO j = jmin,jmax
         DO jp = jmin,j
            count = count + 1

      write (out,*) 'j and jp are', j, jp
      IF (j.lt.jmin .or. j.gt.jmax .or. jp.lt.jmin .or. jp.gt.jmax) THEN
         call abort ('check the j and jp values in input')
      END IF
      IF (jp.le.j) THEN
         jB  = j
         jpB = jp
      ELSE       !    if jp > j, use B(jp,j)
         jB  = jp
         jpB = j
      END IF

      DO lam=0,maxlamb
         Blambda(lam) = B(lam,count)
         IF (j.eq.0 .or. jp.eq.0) THEN
            angle(lam) = 0.d0
         ELSE
            angle(lam) = dble( j*(j+1) + jp*(jp+1) - lam*(lam+1) ) /
     -        (2.d0 *  sqrt(   dble(j*(j+1)*jp*(jp+1) ) ) )
            angle(lam) = acos(angle(lam))  ! angle now in radians
         END IF
      END DO

*     write out table of B-lambda and corresponding angle

      write (out,*) ' lam  B-lambda(j,jp)  angle'
      write (out,'(i5,2f12.4)') (lam,Blambda(lam),angle(lam)*180.d0/PI,
     -                  lam=abs(j-jp),j+jp)


!     write (out,*) 'lam, 3j symbol squared, approx'
      jph  = dble(j)  + 0.5d0
      jpph = dble(jp) + 0.5d0
      DO lam=abs(j-jp),j+jp
         IF (mod(j+jp+lam,2).eq.0) THEN
            tmp = d3j(dble(j),dble(jp),dble(lam),0.d0,0.d0,0.d0)
            amtrans(lam) = Blambda(lam) * dble(2*lam+1)
     -       /dble(2*j+1)/dble(2*jp+1)/tmp/tmp
            lamph= dble(lam)+ 0.5d0
            scx = (2.d0/PI)/sqrt(
     -      ( (jph+jpph)**2-lamph**2 ) * ( lamph**2 - (jph-jpph)**2 ) )
!           write (out,'(i4,2f12.5)') lam,tmp**2,scx
         ELSE
            amtrans(lam) = 0.d0
         END IF
      END DO


      write (out,*) 'assess energy-sudden approximation:'
      write (out,*) ' lam  (pi/ksq)*sigma(0->lam)'
      DO lam=abs(j-jp),j+jp
         IF (mod(j+jp+lam,2).eq.0) THEN
           ! calculate cos(theta) from this lambda, j, j'
            IF(j.ne.0 .and. jp.ne.0) THEN
               jph  = dble(j)  + 0.5d0
               jpph = dble(jp) + 0.5d0
               lamph= dble(lam)+ 0.5d0
               x = (jph**2 + jpph**2 - lamph**2)/(2.d0*jph*jpph)
            ELSE
               x = 999.d0
            END IF
            write (out,'(i5,5f12.4)') lam,amtrans(lam)!,
!    -         acos(x)*180.d0/PI, x,
!    -         amtrans(lam)/lamph/sqrt(1.0-x*x)
         END IF
      END DO




      call PutLine(out,'^,results from the analytic formula:')
*     zero Q
      DO lam=0,2*MAXJ
         Q(lam) = 0.d0
      END DO

      DO lam=0,2*min(j,jp)
         IF (mod(lam+j+jp,2).eq.0) THEN  ! evaluate phase2=(-1)**(lam+j+jp)
            phase2=dble(1)
         ELSE
            phase2=dble(-1)
         END IF
         sum = 0.d0
         call dsixj(dble(j),dble(jp),dble(lam),dble(jp),dble(j),
     -       clist,j1min,j1max,CSIZE)
         phase=dble(1)
         if (mod(nint(j1min),2).eq.1) phase=-phase
         DO lamB=nint(j1min),nint(j1max)
            sum = sum + phase*clist(lamB-nint(j1min)+1)*Blambda(lamB)
     -           * dble(2*lamB+1)!/dble(2*lam+1)
            phase = -phase
         END DO
         Q(lam)=sum * phase2
      END DO

*     evaluate   SUM (2*lam+1) B_lambda(j,jp)
*                lam

      sum=dble(0)
      DO lam=abs(j-jp),j+jp
         sum = sum + dble(2*lam+1) * Blambda(lam)
      END DO

*     for closed channels, Q(0)=0.d0; check for this to avoid problems

      pfac = Q(0)
      IF (pfac.ne.0.d0) THEN               ! open channel
         write(out,'(''original Q(0) is          '',f21.8)') Q(0)
         write(out,'(''Q(0)/sqrt[(2j+1)(2jp+1)]  '',f21.8)')
     -                  Q(0)/sqrt(dble((2*j+1)*(2*jp+1)))
         ratio=sum/Q(0)
         write(out,'(''compare sum over B-lambda '',2f21.16)') sum,ratio
         DO lam=0,2*min(j,jp)
            Q(lam) = Q(lam)/pfac
            write (out,'(i5,f21.16)') lam,Q(lam)
         END DO
      ELSE                                 ! closed channel
         write(out,'(''original Q(0) is          '',f21.8)') Q(0)
         write(out,*) 'closed channel; all Q set to zero'
         DO lam=0,2*min(j,jp)
            Q(lam) = 0.d0
         END DO
      END IF
      

      write (out,*) '************************************'

      frac(1,j-jmin+1,jp-jmin+1) = Q(1)
      frac(2,j-jmin+1,jp-jmin+1) = Q(2)
      IF (j.gt.jp  .and. (j+jp).le.2*MAXNOJ) THEN
         Ajsumdif(j+jp,j-jp,1) = Q(1)
         Ajsumdif(j+jp,j-jp,2) = Q(2)
      END IF

      END DO   !!! end loop over jp
      END DO   !!! end loop over j


      write(out,*)
     -   'Probability for Transfer of Orientation (1st moment)'
      write(out,'(1x,51i7)') (jp,jp=jmin,jmax)
      DO j = jmin,jmax
         write (out,'(i2,1x,51f7.4)') j,
     -       (frac(1,j-jmin+1,jp-jmin+1),jp=jmin,j)
      END DO

      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Alignment (2nd moment)'
      write(out,'(1x,51i7)') (jp,jp=jmin,jmax)
      DO j = jmin,jmax
         write (out,'(i2,1x,51f7.4)') j,
     -       (frac(2,j-jmin+1,jp-jmin+1),jp=jmin,j)
      END DO

      DO j=0,jmax
         DO k = -jmax,jmax
            Ajdelj(j,k) = 0.d0
         END DO
      END DO

*     transform j,jp to j,jdel : orientation
      DO j=jmin,jmax
         DO jp=jmin,j
            jdel = j - jp
            Ajdelj(j,jdel) =  frac(1,j-jmin+1,jp-jmin+1)
         END DO
      END DO

      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Orientation (1st moment)'
     -   //' in terms of j and jdel; j is col, jdel is row'
      write(out,'(2x,51i7)') (j,j=0,jmax)
      DO jdel = 0,jmax
         write (out,'(i3,1x,102f7.4)') jdel,
     -      (Ajdelj(j,jdel),j=0,jmax)
      END DO

*     transform j,jp to j,jdel : orientation
      DO j=jmin,jmax
         DO jp=jmin,j
            jdel = j - jp
            Ajdelj(j,jdel) =  frac(2,j-jmin+1,jp-jmin+1)
         END DO
      END DO

      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Alignment (2nd moment)'
     -   //' in terms of j and jdel; j is col, jdel is row'
      write(out,'(2x,51i7)') (j,j=0,jmax)
      DO jdel = 0,jmax
         write (out,'(i3,1x,102f7.4)') jdel,
     -      (Ajdelj(j,jdel),j=0,jmax)
      END DO

      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Orientation (1st moment)'
     -   //' in terms of j+jp and jdel; jdel is col, j+jp is row'
      write(out,'(2x,51i7)') (jdel,jdel=1,jmax)
      DO jj = 1,jmax
         write (out,'(i3,1x,102f7.4)') jj,
     -      (Ajsumdif(jj,jdel,1),jdel=1,jmax)
      END DO


***** ------------------------------------------------------------------
*     write Orientation data for odd jbar to out and to separate file
      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Orientation (1st moment) in terms'
     -   //' of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(out,'(4x,51i7)') (jdel,jdel=1,jmax,2)
      DO jj = 1,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (out,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,1),jdel=1,jmax,2)
      END DO
      
      filename = trim(rootname)//'.ORodd'
      OAfile = fassign(OAfile,filename,2)
      write(OAfile,*)
     -   '# Probability for Transfer of Orientation (1st moment) in'
     -   //' terms of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(OAfile,'('' #'',4x,51i7)') (jdel,jdel=1,jmax,2)
      DO jj = 1,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (OAfile,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,1),jdel=1,jmax,2)
      END DO
      OAfile = dassign(OAfile)

***** ------------------------------------------------------------------
*     write Orientation data for even jbar to out and to separate file
      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Orientation (1st moment) in terms'
     -   //' of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(out,'(4x,51i7)') (jdel,jdel=2,jmax,2)
      DO jj = 2,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (out,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,1),jdel=2,jmax,2)
      END DO

      filename = trim(rootname)//'.OReven'
      OAfile = fassign(OAfile,filename,2)
      write(OAfile,*)
     -   '# Probability for Transfer of Orientation (1st moment) in'
     -   //' terms of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(OAfile,'('' #'',4x,51i7)') (jdel,jdel=2,jmax,2)
      DO jj = 2,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (OAfile,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,1),jdel=2,jmax,2)
      END DO
      OAfile = dassign(OAfile)

***** ------------------------------------------------------------------
*     write Alignment data for odd jbar to out and to separate file
      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Alignment (2nd moment) in terms'
     -   //' of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(out,'(4x,51i7)') (jdel,jdel=1,jmax,2)
      DO jj = 1,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (out,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,2),jdel=1,jmax,2)
      END DO

      filename = trim(rootname)//'.ALodd'
      OAfile = fassign(OAfile,filename,2)
      write(OAfile,*)
     -   '# Probability for Transfer of Alignment (2nd moment) in'
     -   //' terms of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(OAfile,'('' #'',4x,51i7)') (jdel,jdel=1,jmax,2)
      DO jj = 1,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (OAfile,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,2),jdel=1,jmax,2)
      END DO
      OAfile = dassign(OAfile)

***** ------------------------------------------------------------------
*     write Alignment data for even jbar to out and to separate file
      write (out,*)' '
      write(out,*)
     -   'Probability for Transfer of Alignment (2nd moment) in terms'
     -   //' of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(out,'(4x,51i7)') (jdel,jdel=2,jmax,2)
      DO jj = 2,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (out,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,2),jdel=2,jmax,2)
      END DO
      OAfile = dassign(OAfile)


      filename = trim(rootname)//'.ALeven'
      OAfile = fassign(OAfile,filename,2)
      write(OAfile,*)
     -   '# Probability for Transfer of Alignment (2nd moment) in'
     -   //' terms of jbar=(j+jp)/2 and jdel; jdel is col, jbar is row'
      write(OAfile,'('' #'',4x,51i7)') (jdel,jdel=2,jmax,2)
      DO jj = 2,2*jmax,2    !  jj = j+jp ==> jbar=0.5*jj
         write (OAfile,'(f5.1,1x,102f7.4)') dble(jj)/2.d0,
     -      (Ajsumdif(jj,jdel,2),jdel=2,jmax,2)
      END DO

      end
