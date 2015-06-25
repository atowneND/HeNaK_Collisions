***** C:\wkarea\adcode\blam\pcc1.for created Sunday, June 26, 2011 at 14:14
***** Peet Hickman

      implicit     none
      integer      in,out,nchar,lp,jmin,jmax,maxlamb,j,jp,nj,lam,
     -             MAXNOJ,itmp,jj,k,MAXLAMBMAX,MAXJ,CSIZE,jB,jpB,lamB,
     -             count,jbarx2,jdel,m,mp,lincnt
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
      double precision xsig(-MAXJ:MAXJ,-MAXJ:MAXJ)
                   ! xsig is a constant times sigma(jm -> j'm')
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
      write (out,*) '# jmin,jmax,maxlamb are ',jmin,jmax,maxlamb
      nj = jmax-jmin+1
      if (nj.gt.MAXNOJ) call abort('main: MAXNOJ too small')

      read (in,*) (fac(jj),jj=1,nj)

      count = (nj*(nj+1))/2
      DO lam=0,maxlamb
        read (in,*) itmp
        if (lam.ne.itmp) call abort('main: unexpected lambda')
        read (in,*) (B(lam,k),k=1,count)
      END DO

      write(*,*) 'Enter j and jp'
      read (*,*) j,jp

      write (out,*) '# j and jp are', j, jp
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
      
      count = (jB*(jB+1))/2 + jpB+1

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

      write (out,*) '# lam  B-lambda(j,jp)  angle'
      write (out,'(1h#,i5,2f12.4)') (lam,Blambda(lam),
     -                  angle(lam)*180.d0/PI,lam=abs(j-jp),j+jp)

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


*     calculate jm --> j'm' cross sections (w/o prefactor for now)

      lincnt=-1
      DO m = -jB,jB
         DO mp = -jpB,jpB
            tmp = 0.d0
            DO lam=abs(jB-jpB),jB+jpB
               IF (dble(abs(m-mp)).le.lam) THEN
                  x = d3j( dble(jB),dble(jpB),dble(lam),
     -                     dble(-m), dble(mp),dble(m-mp))
               ELSE
                  x = 0.d0
               END IF
               tmp = tmp + dble(2*lam+1) * x*x *  Blambda(lam)
            END DO
       !     xsig(m,mp) = tmp
            lincnt = lincnt+1
            write (out,'(2i4,f22.4,i4)') m,mp,tmp,lincnt  ! xsig(m,mp)
         END DO
         write (out,'(1h# )')
      END DO
         


 
    
      end
