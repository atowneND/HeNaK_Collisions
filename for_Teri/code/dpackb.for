
      function d3j(el1,el2,el3,em1,em2,em3)
      implicit     none
      integer      triangle,m2
      real*8       d3j,el1,el2,el3,em1,em2,em3,em2min,em2max
      real*8       clist(200)

      IF ((abs(em1+em2+em3).gt.0.001).or.
     -(triangle(real(el1),real(el2),real(el3)).eq.0)) THEN
        d3j=0.d0
      ELSE
        call double3j(el1,el2,el3,em1,clist,em2min,em2max,200)
        m2=nint(em2-em2min+1.d0)
        d3j=clist(m2)
      END IF
      end

      subroutine double3j(el1,el2,el3,em1,clist,em2min,em2max,n)
c
c     the 3j coefficient   ( el1   el2      el3    ) is put in
c                          ( em1   em2  -(em1+em2) )
c
c     values are calculated for all allowed em2
c
      implicit     none
      real*8       el1,el2,el3,em1,em2min,em2max,clist(*)
      integer      n

      real*8       ZERO,ONE,TWO,THREE
      parameter    (ZERO=0.d0, ONE=1.d0, TWO=2.d0, THREE=3.d0)
      real*8       c,d,em2dif,em2mid,ga,gb,gc,save,save2,
     -             fac,em2,sum,phase
      integer      m2,m2min,m2mid,m2max,i,numb

      c(em2)=sqrt((el2-em2+ONE)*(el2+em2)*(el3-em1-em2+ONE)*(el3
     -     +em1+em2))
      d(em2)=el2*(el2+ONE)+el3*(el3+ONE)-el1*(el1+ONE)+TWO*em2*
     -     (-em1-em2)
      m2(em2)=nint(em2-em2min+ONE)

      DO i=1,n
         clist(i)=ZERO
      END DO
      em2min=max(-el2,-(el3+em1))
      em2max=min(el2,(el3-em1))
      em2dif=em2max-em2min
      em2mid=em2min+em2dif/TWO
      if(nint(em2dif).le.3) em2mid=em2max
      m2min=m2(em2min)
      m2mid=m2(em2mid)
      m2max=m2(em2max)

c     forward recursion from em2min to em2mid
      ga=ONE
      clist(1)=ga
      if(nint(em2dif).le.0) go to 100
      gb=-d(em2min)/c(em2min+ONE)
      clist(2)=gb
      if(nint(em2dif).le.1) go to 100
      em2=em2min+ONE
25    gc=(d(em2)*gb+c(em2)*ga)/(-c(em2+ONE))
      ga=gb
      gb=gc
      clist(m2(em2+ONE))=gc
      em2=em2+ONE
      if(nint(em2mid-em2).ge.1) go to 25
      save=gc
      save2=ga

c     if necessary, backward recursion from em2max to em2mid
      if (nint(em2dif).le.3) go to 100
c     adjust matching point to avoid possible zero
      if(abs(save).ge.abs(save2)) go to 35
      em2mid=em2mid-ONE
      m2mid=m2mid-1
      save=save2
35    continue
      ga=ONE
      gb=-d(em2max)/c(em2max)
      clist(m2(em2max))=ga
      clist(m2(em2max-ONE))=gb
      em2=em2max-ONE
      numb=m2max-m2mid-1
      DO i=1,numb
         gc=-(c(em2+ONE)*ga+d(em2)*gb)/c(em2)
         ga=gb
         gb=gc
         clist(m2(em2-ONE))=gc
         em2=em2-ONE
      END DO
c     compare at em2mid and rescale
      fac=save/clist(m2mid)
      DO i=m2mid,m2max
           clist(i)=clist(i)*fac
      END DO

c     perform sum, normalize, and assign correct phase
100   continue
      sum=ZERO
      DO i=m2min,m2max
         sum=sum+clist(i)*clist(i)
      END DO
      sum=sum*(TWO*el1+ONE)
      sum=sqrt(sum)
c     iphase=abs(el1-el3-em1)
c     change made aug 4, 1987 following bug find by wkb
*     iphase=abs(el2-el3-em1)
*     phase=(-1)**mod(iphase,2)
      IF (mod(abs(nint(el2-el3-em1)),2).eq.0) then
         phase = ONE
      ELSE
         phase = -ONE
      END IF
      if((phase*clist(m2max)).lt.ZERO) sum=-sum
      DO i=m2min,m2max
         clist(i)=clist(i)/sum
      END DO
      end


      subroutine dsixj(j2,j3,l1,l2,l3,clist,j1min,j1max,n)
c
c     the 6-j coefficient (j1 j2 j3)  calculated for all allowed
c                         (l1 l2 l3)
c
c     values of j1.  j1 runs from j1min to j1max, which are calculated
c     and returned.  The value for a given j1 is returned in
c     clist(j1-j1min+1).  The method is based on recursion formulas
c     from Schulten and Gordon, J. Math. Phys. _16_, 1961 (1975).
c
c     written by Peet Hickman.  version 6-30-89 includes half
c     integral j, and least squares matching of forward and
c     backward recursion at three points.  still no overflow protection.
c     version 10-23-90 cleans up things a little more.
c     version 2-28-96 fixes a bug and makes it double precision

      implicit     none
      real*8       j2,j3,l1,l2,l3,j1min,j1max,clist(*)
      integer      n  ! dimension of array clist

      integer      triangle,nn,i,ij1
      real*8       j1,jdif,j1mid,e,f,ha,hb,hc,sum,save,savem1,savep1,
     -             phase,xxj2,xxl2,fac
      real*8       ZERO, ONE, TWO, FOUR
      parameter    (ZERO=0.d0, ONE=1.d0, TWO=2.d0, FOUR=4.d0)
      e(j1)=sqrt((j1*j1-(j2-j3)*(j2-j3))*((j2+j3+ONE)*(j2+j3+
     -  ONE)-j1*j1)*(j1*j1-(l2-l3)*(l2-l3))*((l2+l3+ONE)*(l2+
     -  l3+ONE) -j1*j1))
      f(j1)=(TWO*j1+ONE)*(j1*(j1+ONE)*(-j1*(j1+ONE)+j2*(j2+ONE)+j3*
     -  (j3+ONE))+l2*(l2+ONE)*(j1*(j1+ONE)+j2*(j2+ONE)-j3*(j3+ONE))
     -  +l3*(l3+ONE)*(j1*(j1+ONE)-j2*(j2+ONE)+j3*(j3+ONE))
     -  -TWO*j1*(j1+ONE)*l1*(l1+ONE))
      nn(j1) = nint(j1-j1min+ONE) ! position in clist array of j1 value

      external     abort

      DO i=1,n
         clist(i)=ZERO
      END DO
      j1min=max(abs(j2-j3),abs(l2-l3))
      j1max=min((j2+j3),(l2+l3))
c     return all zeros if triangle inequality not satisfied
      if(triangle(real(j1min),real(j2),real(j3)).eq.0
     -     .or. triangle(real(j1min),real(l2),real(l3)).eq.0
     -     .or. triangle(real(j2),real(l1),real(l3)).eq.0
     -     .or. triangle(real(j3),real(l1),real(l2)).eq.0) return
      if (nn(j1max).gt.n)
     -    call abort ('sixj: dimension of clist array too small')
      jdif=j1max-j1min
      j1mid = j1min + float(nint(jdif/TWO))
      if (nint(jdif).le.3) j1mid=j1max
c     start forward recursion from j1min to j1mid
      ha=ONE
      clist(nn(j1min))=ha
c     if jdif.ge.1, do second term in recursion
      IF (nint(jdif).ge.1) THEN
         IF(nint(j1min).gt.0) THEN
            hb=-f(j1min)/j1min/e(j1min+ONE)
         ELSE
            xxj2=j2*(j2+ONE)
            xxl2=l2*(l2+ONE)
            hb=-(xxj2+xxl2-l1*(l1+ONE))/sqrt(FOUR*xxj2*xxl2)
         END IF
         clist(nn(j1min+ONE))=hb
      END IF
c     if jdif.ge.2, do third term through j1mid-th term
      IF (nint(jdif).ge.2) THEN
!        DO j1=j1min+ONE,j1mid-ONE,ONE
         DO ij1= nint(j1min+ONE),nint(j1mid-ONE)
            j1=dble(ij1)
            hc=(f(j1)*hb+(j1+ONE)*e(j1)*ha)/(-j1*e(j1+ONE))
            ha=hb
            hb=hc
            clist(nn(j1+ONE))=hc
         END DO
      END IF
c     if jdif = 3, recursion is complete, because j1mid=j1max
c     if jdif.ge.4, do backward recursion from j1max to j1mid
      IF (nint(jdif).ge.4) THEN
         save=hc
         savem1=ha
         savep1=
     -    (f(j1mid)*hb+(j1mid+ONE)*e(j1mid)*ha)/(-j1mid*e(j1mid+ONE))
         ha=ONE
         clist(nn(j1max))=ha
         hb=-f(j1max)/(j1max+ONE)/e(j1max)
         clist(nn(j1max-ONE))=hb
         j1=j1max
         DO i=1,nint(j1max-j1mid-ONE)+1
            j1=j1-ONE
            hc=-(j1*e(j1+ONE)*ha+f(j1)*hb)/(j1+ONE)/e(j1)
            ha=hb
            hb=hc
            clist(nn(j1-ONE))=hc
         END DO
c        compare at j1mid-1, j1mid, j1mid+1 and rescale
         fac= (savem1*clist(nn(j1mid-ONE)) + save *
     -      clist(nn(j1mid))+savep1*clist(nn(j1mid+ONE)))
     -      /(clist(nn(j1mid-ONE))**2 + clist(nn(j1mid))**2
     -      + clist(nn(j1mid+ONE))**2)
!        DO j1=j1mid-ONE,j1max,ONE
         DO ij1=nint(j1mid-ONE),nint(j1max)
            j1=dble(ij1)
            clist(nn(j1))=clist(nn(j1))*fac
         END DO
      END IF
c     perform sum, normalize, and assign correct phase
      sum=ZERO
      !O j1=j1min,j1max,ONE
      DO ij1=nint(j1min),nint(j1max)
         j1=dble(ij1)
         sum=sum+clist(nn(j1))*clist(nn(j1))*(TWO*j1+ONE)
      END DO
      sum=sum*(TWO*l1+ONE)
      sum=sqrt(sum)
c     evaluate (-1)**(j2+j3+l2+l3)
      IF ( mod(nint(j2+j3+l2+l3),2) .eq. 0 ) THEN
         phase= ONE
      ELSE
         phase=-ONE
      END IF
      if((phase*clist(nn(j1max))).lt.ZERO) sum=-sum
!     DO j1=j1min,j1max,ONE
      DO ij1=nint(j1min),nint(j1max)
         j1=dble(ij1)
         clist(nn(j1))=clist(nn(j1))/sum
      END DO
      return
      end
