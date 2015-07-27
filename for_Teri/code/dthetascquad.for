***** C:\wkarea\adcode\blam\dqcalc2.for created Monday, June 15, 2015 at 16:43
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
      double precision fac(MAXNOJ),Blambda(0:MAXLAMBMAX),
     -             B(0:MAXLAMBMAX,(MAXNOJ*(MAXNOJ+1))/2),
     -             clist(CSIZE),sum,sig,sumnorm,
     -             pfac, phase, xjmin,xjmax,x,PI,jph,tmp,dq,dsave
      double precision dqbyd0(0:2*MAXNOJ),
     -                      dist(-1800:1800)
      parameter    (PI=4.d0*atan(1.0d0) )
      double precision P(0:200),Balpha(18000),ftheta,alpha,theta,phi,
     -              thprime,costh,sinth,cosal,sinal,bar,zint,
     -              delth,y,costhp,sinthp,delta
      double precision array(18001)
      integer      itheta,iphi,nquad,idelta
  
*     calls:
      integer      GetLine,fassign,dassign,index,iargc
      character    strR8*16
      double precision d3j,dsimp,Pfun
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
      END DO

*     now calculate the dq coefficients for q = iq = 1,...,2*min(j,jp)
      DO iq=0,2*min(j,jp)
         call dsixj( dble(j),dble(jp),
     -      dble(iq),dble(jp),dble(j),clist,xjmin,xjmax,CSIZE)
         dq = 0.d0
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
         dqbyd0(iq) = dq/dsave
      END DO

      delth =1.0d0   ! fraction of degree for thprime step
*     now calculate B(cos alpha)

$comment
      P(0)=1.d0
      qend = 2*min(j,jp)
      array(1) = 0.d0
      DO ialpha=1,nint(180.d0/delth)
         !alpha = (dble(ialpha) - 0.5d0)*PI/180.d0  ! in radians
         alpha =  dble(ialpha)*delth*PI/180.d0  ! in radians
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
         array(ialpha+1) = Balpha(ialpha)
      END DO
      tmp = delth*PI/dble(180) * dsimp(nint(180.d0/delth)+1,array)
      write (*,*) '# int over B(alpha) = ',tmp

 
      write(*,*) 'Enter theta in deg'
      read (*,*) theta
      write (out,*) '# theta in deg = ',theta
      theta =  theta * PI/180.d0   ! convert to radians
      costh = cos(theta)
      sinth = sin(theta)
      nquad=40
      pfac = PI/dble(2*nquad)
!      DO itheta = 0,nint(180.d0/delth)
      DO itheta = 0,nint(180.d0/delth)
         thprime =  dble(itheta)*delth*PI/180.d0  ! in radians
         costhp = cos(thprime)
         sinthp = sin(thprime)
         tmp = 0.d0
         dist(itheta) = Pfun(theta,thprime,dqbyd0,nquad,j,jp) * sinthp
         array(itheta+1) = dist(itheta)
         !write (*,'(f10.3,f12.5)')  thprime*180.d0/PI,tmp
      END DO
      zint = delth*PI/dble(180) * dsimp(nint(180.d0/delth)+1,array)
      write (*,*) '# integral over raw P(theta,thprime) = ',zint
      write (out,*) '# integral over raw P(theta,thprime) = ',zint
      write (out,*) '# unnormalized distribution'
      DO itheta = 0,nint(180.d0/delth)
         thprime =  dble(itheta) *delth ! in degrees
         write (out,'(f10.3,f18.12)')  thprime,dist(itheta)
      END DO
$end comment

*     now calculate distribution of delta theta

      nquad = 40
      write (out,*) '# delta theta, prob(delta theta)'
      DO idelta=0,nint(180.d0/delth)
         delta = dble(idelta)*delth*PI/180.d0  ! delta theta in radians
         DO itheta = idelta,nint(180.d0/delth)
            theta = dble(itheta)*delth*PI/180.d0  ! theta in radians
            thprime = theta - delta
            array(1+itheta-idelta)=Pfun(theta,thprime,dqbyd0,nquad,j,jp)
     -         * sin(theta)*sin(thprime)
        !    write (out,*) 1+itheta-idelta,array(1+itheta-idelta)
         END DO
         k = nint(180.d0/delth)-idelta+1
         dist(idelta) = (delth*PI/180.d0)*dsimp(k,array)/PI
         if (idelta.ne.0) dist(-idelta) = dist(idelta)
      END DO

      k = nint(180.d0/delth)
      DO idelta = -k,k
         array(idelta+k+1) = dist(idelta)
      END DO
      itmp = 2*k+1
      zint = delth*dsimp(itmp,array)
      write (out,*) '# int over delta theta distribution = ', zint
      write (out,*) '# normalized, -nint(180.d0/delth) to plus'
      DO idelta = -k,k   ! normalize for degrees
         dist(idelta) = dist(idelta)/zint
         write (out,'(f10.2,f16.8)') dble(idelta)*delth, dist(idelta)
      END DO
      end


      function Pfun(theta,thprime,dqbyd0,nquad,j,jp)
      implicit     none
      double precision Pfun,theta,thprime,dqbyd0(0:*)
      integer      nquad,j,jp
      
      integer, parameter :: MAXLPOLY=200
      double precision P(0:MAXLPOLY),
     -             costh,sinth,costhp,sinthp,tmp,pfac,y,ftheta
      integer      k,q,qend

      double precision, parameter ::  PI=4.d0*atan(1.0d0)

      costh = cos(theta)
      sinth = sin(theta)
      costhp = cos(thprime)
      sinthp = sin(thprime)
      qend = 2*min(j,jp)
      pfac = PI/dble(2*nquad)
      tmp = 0.d0
      
      P(0) = 1.d0
      DO k = 1,nquad
         y = costh*costhp + cos((2*k-1)*pfac)*sinth*sinthp
         P(1) = y
         ftheta = dqbyd0(0)
         if (qend.ge.1) ftheta = ftheta + 3.d0*dqbyd0(1)*y
         DO q=1,qend-1           ! now get Pq(y) by recursion
            P(q+1) = (dble(2*q+1)*y*P(q) - dble(q)*P(q-1) )/dble(q+1)
            ftheta = ftheta + dble(2*q+3)*dqbyd0(q+1)*P(q+1) ! 2q+3 = 2(q+1)+1
         END DO
         tmp = tmp + ftheta
      END DO
                  
      Pfun = tmp / dble(nquad)      ! tmp * (PI/nquad) /PI
        
      end
