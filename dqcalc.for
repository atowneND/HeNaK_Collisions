***** C:\wkarea\adcode\blam\dqcalc2.for created Monday, June 15, 2015 at 16:43
***** Peet Hickman

*     this version reads j and jp from the command line, which must be
*     dqcalc2 inputfile.blam outputfile j jp

      implicit     none
      integer      in,out,nchar,lp,jmin,jmax,maxlamb,j,jp,nj,lam,
     -             MAXNOJ,itmp,jj,k,MAXLAMBMAX,MAXJ,CSIZE,jB,jpB,lamB,
     -             count,jbarx2,jdel,m,mp,lincnt,iq
      parameter    (MAXNOJ=51,MAXLAMBMAX=100,MAXJ=50,CSIZE=200)
      character    line*128,rootname*64,filename*64
      logical      skip
      double precision Brow(MAXNOJ),fac(MAXNOJ),Blambda(0:MAXLAMBMAX),
     -             B(0:MAXLAMBMAX,(MAXNOJ*(MAXNOJ+1))/2),
     -             angle(0:MAXLAMBMAX),clist(CSIZE),sum,sig,
     -             pfac, phase, xjmin,xjmax,x,PI,jph,tmp,dq,dsave
      parameter    (PI=4.d0*atan(1.0d0) )

*     calls:
      integer      GetLine,fassign,dassign,index,iargc
      character    strR8*16
      double precision d3j
      external     abort

      integer     argc
      character    argv(10)*(10)

      call stdio (in,out)

      argc = iargc()
      IF (argc.eq.4) THEN
         DO k = 1,argc
            call getarg(k,argv(k))
         END DO
         read (argv(3),*) j
         read (argv(4),*) jp
      ELSE
         call abort ('wrong number of command line arguments')
      END IF

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

!      write(*,*) 'Enter j and jp'
!      read (*,*) j,jp

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

*     now calculate the dq coefficients for q = iq = 1,...,2*min(j,jp)
      DO iq=0,2*min(j,jp)
         call dsixj( dble(j),dble(jp),
     -      dble(iq),dble(jp),dble(j),clist,xjmin,xjmax,CSIZE)
         dq = 0.d0
       !  write (out,*) iq,xjmin,xjmax
       !  write (out,*)(k,clist(k),k=1,nint(xjmax-xjmin)+1)
         
         IF (mod(iq,2).eq.0) THEN
            phase = 1.d0
         ELSE
            phase = -1.d0
         END IF
         DO lam = abs(j-jp),j+jp
            dq = dq + phase*dble(2*lam+1)*clist(lam-nint(xjmin)+1)
     -                          * Blambda(lam)
            phase = -phase
         END DO
         if (iq.eq.0) dsave = dq
         write (out,'(i4,es15.5,f22.16)') iq,dq,dq/dsave
      END DO
         
      
      end
