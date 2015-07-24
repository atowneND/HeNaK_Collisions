***** C:\wkarea\adcode\blam\dthetqm.for created July 7, 2015
***** Peet Hickman

*     this version reads j and jp from the command line, which must be
*     dqcalc2 inputfile.blam outputfile j jp

      implicit     none
      integer      in,out,nchar,lp,jmin,jmax,maxlamb,j,jp,nj,lam,
     -             MAXNOJ,itmp,jj,k,MAXLAMBMAX,MAXJ,CSIZE,jB,jpB,lamB,
     -             count,jbarx2,jdel,m,mp,lincnt,iq,ialpha,q,qend
      parameter    (MAXNOJ=51,MAXLAMBMAX=100,MAXJ=50,CSIZE=200)
      character    line*128,rootname*64,filename*64
      logical      skip
      double precision Brow(MAXNOJ),fac(MAXNOJ),Blambda(0:MAXLAMBMAX),
     -             B(0:MAXLAMBMAX,(MAXNOJ*(MAXNOJ+1))/2),
     -             angle(0:MAXLAMBMAX),clist(CSIZE),sum,sig,sumnorm,
     -             pfac, phase, xjmin,xjmax,x,PI,jph,tmp,dq,dsave,sum2
      double precision dqbyd0(0:2*MAXNOJ),approx(0:10),averagelam,
     -             averageangle,altangle,hist(-18000:18000)
      parameter    (PI=4.d0*atan(1.0d0) )
      double precision P(0:200),Balpha(18000),ftheta,alpha,theta,phi,
     -              thprime,costh,sinth,cosal,sinal,bar,
     -              thbindeg,albindeg,phibindeg,  thbinrad
      integer       nthbin,  nalbin,  nphibin,  itheta,iphi

      integer, parameter :: KMAX=201
      double precision bb(0:KMAX),z

*     calls:
      integer      GetLine,fassign,dassign,index,iargc
      character    strR8*16
      double precision d3j
      external     abort

      integer     argc
      character    argv(10)*(10)

      call stdio (in,out)


      write (*,*) 'enter theta-bin, alpha-bin, and phi-bin in degrees'
      read  (*,*) thbindeg,albindeg,phibindeg

!      thbindeg = 1.0d0
      nthbin = nint(180.d0/thbindeg)
      thbindeg = 180.d0/dble(nthbin)
      
!      albindeg = 1.0d0
      nalbin = nint(180.d0/albindeg)
      albindeg = 180.d0/dble(nalbin)
      
!      phibindeg = 1.0d0
      nphibin = nint(180.d0/phibindeg)
      phibindeg = 180.d0/dble(nphibin)

      thbinrad = thbindeg*PI/180.d0

      DO k = -nalbin,nalbin
         hist(k) = 0.d0
      END DO


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
         !write (out,'(i4,es15.5,f22.16)') iq,dq,dq/dsave
         dqbyd0(iq) = dq/dsave
      END DO
         
*     now calculate <alpha> = average tipping angle
      bb(0) = 0.75d0
      !write (out,'(i5,3f15.10)') 0,bb(0)
      DO k=1,KMAX
         z=dble(k)
         bb(k) = bb(k-1)  *(z-.5d0)**2 * (z+0.75d0) /
     -                    ((z+1.d0)**2 * (z-0.25d0))
         !write (out,'(i5,3f15.10)') k,bb(k)
      END DO
      
      z = dble(1)
      DO k = 0,2*min(j,jp)
         z = z - bb(k) * dqbyd0(2*k+1)
      END DO
      z = z * PI/dble(2)

      write (out,*) '############################'

*     now calculate B(cos alpha)
      
      P(0)=1.d0
      qend = 2*min(j,jp)
      DO ialpha=1,nalbin
         alpha = (dble(ialpha) - 0.5d0)*albindeg*PI/180.d0  ! in radians
         ! calculate B(cos alpha)
         x = cos(alpha)
         P(1) = x
         ftheta = dqbyd0(0)
         if (qend.ge.1) ftheta = ftheta + 3.d0*dqbyd0(1)*x
         DO q=1,qend-1           ! now get Pq(x) by recursion
            P(q+1) = (dble(2*q+1)*x*P(q) - dble(q)*P(q-1) )/dble(q+1)
            ftheta = ftheta + dble(2*q+3)*dqbyd0(q+1)*P(q+1) ! 2q+3 = 2(q+1)+1
         END DO
         Balpha(ialpha) =  ftheta
!         write(out,'(i6,2f15.7)') ialpha, Balpha(ialpha),
!     -               Balpha(ialpha)*sin(alpha)
         Balpha(ialpha) = Balpha(ialpha)*sin(alpha)
      END DO

      DO itheta = 1,nthbin
         theta = (dble(itheta) - 0.5d0)*thbindeg*PI/180.d0  ! in radians
         sinth = sin(theta)
         costh = cos(theta)
         DO ialpha=1,nalbin
            alpha = (dble(ialpha) - 0.5d0)*albindeg*PI/180.d0  ! in radians
            sinal = sin(alpha)
            cosal = cos(alpha)
            DO iphi=1,nphibin  ! use symmetry -- careful to get correct range
               phi = (dble(iphi) - 0.5d0)*phibindeg*PI/180.d0  ! in radians
               thprime = acos(
     -         costh*cosal + sinth*sinal*cos(phi) )
               k = nint( (thprime-theta)/thbinrad )
               hist(k) = hist(k)+sinth*Balpha(ialpha) ! sinal factor is in Balpha
            END DO
         END DO
      END DO

      tmp = 0.d0
      DO k = -nthbin,nthbin
         tmp = tmp + hist(k)
      END DO
      tmp = tmp*thbindeg
      
      bar = 0.d0
      sig = 0.d0
      DO k=-nthbin,nthbin
         hist(k) = hist(k)/tmp
         bar = bar+dble(k)*hist(k)        ! theta (deg) = k*thbindeg
         sig = sig + dble(k*k)*hist(k)
      END DO
      bar = bar*thbindeg
      sig = sqrt(sig*thbindeg)*thbindeg

      write (out,'(''#bin size (deg) for theta,alpha,phi: '',3f10.5)')
     -  thbindeg,albindeg,phibindeg

      write (out,'(''# mean and rms of thprime are '',es15.4,f10.4)')
     -  bar,sig
      write (*,'(''# mean and rms of thprime are '',es15.4,f10.4)')
     -  bar,sig
      
      write(out,*) '# normalized histogram'
      DO k = -nthbin,nthbin
         hist(k) = hist(k)
         write (out,'(f10.4,f20.8)') dble(k)*thbindeg,hist(k)
      END DO
 
      end
