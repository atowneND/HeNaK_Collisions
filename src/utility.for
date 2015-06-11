
      function GetLine (lnum, string, nchar)

c     Reads a line from logical unit number lnum.
c
c     nchar   = number of characters read, less leading blanks
c     string  = the line as a character string
c     GetLine = nchar
c
c     usage:       integer*4   lnum,GetLine,nchar
c                  character   string*(maxchar)
c                  nchar = GetLine(lnum,string,nchar)

      include      'standard.aph'
      integer*4    GetLine,lnum,nchar
      character    string*(*)

      integer*4    len_trim
      character    strI4*16

      string = BLANK
      read (lnum,'(a)',end=97,err=99) string
      nchar = len_trim(string)
      if (nchar.eq.0) string = NULL
      GetLine =  nchar
      return

97    continue  !  end of file
      GetLine = -1
      return

99    continue  !  error
      GetLine = -1
      call putline (STDOUT,'error in reading unit '//stri4(lnum))
      end


      subroutine putline (lnum, string)

c     Outputs string to unit lnum.  Escape sequences can be used:
c
c     ^,     new line (CR/LF)
c     ^;     new page (CR/FF)
c       ^b     bell
c       ^t     time (hr:min:sec)
c       ^d     date (month-day-yr)
c       ^/     start sub or end super
c       ^\     end sub or start super
c       ^[     start greek font
c       ^]     end greek font
c       ^#     toggle skipping
c       ^_     toggle underlining
c
c       usage:     integer*4   lnum
c                  character   string*(maxchar)
c                  call putline (lnum,string)

      include      'standard.aph'
      integer*4    lnum,i,k,ln,ld,len_trim
      logical*1    ul_on,endline,escseq
      character    string*(*),buf*(MAXCHAR),dummy*16,ch,dummy2*16

      i = 1
      k = 0
      ch = NULL
      ul_on = .false.
      endline = .false.
      ln = len_trim(string)
      DO WHILE (i.le.ln)
         escseq = .true.
         IF (string(i:i).ne.ESCHAR) THEN
            escseq = .false.
            buf(k+1:) = string(i:i)
            k = k + 1
         ELSE IF (i.eq.ln) THEN
            !ignore trailing escape character
         ELSE IF (string(i+1:i+1).eq.SKIP) THEN
            !advance to end of portion to be skipped
            i = i + index(string(i+1:),ESCHAR//SKIP)
         ELSE IF (string(i+1:i+1).eq.';') THEN
            ch = NEWPAGE
            if (k.gt.0) endline = .true.
         ELSE IF (string(i+1:i+1).eq.',') THEN
            if (k.eq.0) ch = BLANK
            if (k.gt.0) endline = .true.
         ELSE IF (string(i+1:i+1).eq.'[') THEN
            buf(k+1:) = START GREEK
            k = k + len(START GREEK)
         ELSE IF (string(i+1:i+1).eq.']') THEN
            buf(k+1:) = END GREEK
            k = k + len(END GREEK)
         ELSE IF (string(i+1:i+1).eq.'/') THEN
            buf(k+1:) = START SUB
            k = k + len(START SUB)
         ELSE IF (string(i+1:i+1).eq.'\\') THEN
            buf(k+1:) = END SUB
            k = k + len(END SUB)
         ELSE IF (string(i+1:i+1).eq.'b') THEN
            buf(k+1:) = BELL
            k = k + 1
         ELSE IF (string(i+1:i+1).eq.'t') THEN
            call DateTime(dummy,dummy2)
            ld = len_trim(dummy2)
            buf(k+1:k+ld) = dummy2(1:ld)
            k = k + ld
         ELSE IF (string(i+1:i+1).eq.'d') THEN
            call DateTime(dummy,dummy2)
            ld = len_trim(dummy)
            buf(k+1:k+ld) = dummy(1:ld)
            k = k + ld
         ELSE IF (string(i+1:i+1).eq.'_') THEN
            IF (ul_on) THEN
               buf(k+1:) = end uline
               k = k + len(end uline)
            ELSE
               buf(k+1:) = start uline
               k = k + len(start uline)
            END IF
            ul_on = .not. ul_on
         ELSE
            buf(k+1:) = string(i+1:i+1)
            k = k + 1
         END IF
            IF (escseq) THEN
               i = i + 2
            ELSE
               i = i + 1
            END IF
         IF (i.gt.ln .or. endline) THEN
            write (lnum,*) buf(1:k)
            endline = .false.
            k = 0
         END IF
         IF (ch.ne.NULL) THEN
            write (lnum,'(1a)') ch
            ch = NULL
         END IF
      END DO
      end

      function fassign (lnum, filename, type)

c     Opens file 'filename' and assigns it a logical unit number (lnum).
c     Return value is fassign=lnum for successful return, -1 otherwise
c     Options for type are
c
c     READq   ==>  read, file must already exist
c     WRITEq  ==>  write, file need not exist
c     APPENDq ==>  append, to add to an existing file
c
c     usage:       integer*4   fassign,lnum
c                  character   filename*(maxchar)
c                  lnum = fassign(lnum,filename,readq)

      include      'standard.aph'
      integer*4    fassign, dassign, lnum, type, falloc, dalloc
      character    filename*(*)

      IF (falloc(lnum).le.0) THEN    ! allocate a logical unit number
         go to 98
      ELSE IF (type.eq.READq) THEN
         open (unit=lnum, file=filename, status='old', err=98)
      ELSE IF (type.eq.WRITEq) THEN
         open (unit=lnum, file=filename, err=98)
      ELSE IF (type.eq.APPENDq) THEN
         open (unit=lnum, file=filename, access='append', err=98)
      ELSE
         go to 98  !  type not recognized
      END IF
      fassign = lnum
      return

98    continue  !  error handling
         if (dalloc(lnum).gt.0) lnum = -1
         fassign = -1
      return


      ENTRY dassign (lnum)

c     deassigns logical unit number 'lnum' and closes associated file
c     return value is dassign = lnum (>0) if successful, -1 otherwise
c
c     usage:       lnum = dassign(lnum)

      IF (lnum.ge.MINFILE .and. lnum.le.MAXFILE) THEN
         close (unit=lnum, err=99)
         dassign = dalloc(lnum)
      END IF
      return

99    continue  !  error handling
         dassign = -1
      end

      subroutine error_handler

      include      'standard.aph'
      character    message(MAXERROR)*(MAXCHAR), string*(*)
      integer*4    count, file, out, i
      save         out, message, count
      data         count/0/

      ENTRY fileset(file)
      out = file
      return

      ENTRY warning (string)
      count = count + 1
      message(count) = string
      IF (count.lt.MAXERROR) THEN
         return
      ELSE
         call PutLine(out,'***** JOB TERMINATED :  too many warnings')
         DO i=count,1,-1
            call PutLine(out,message(i))
         END DO
         stop ' '
      END IF

      ENTRY reset (string)
      IF (string.ne.NULL) THEN
         call PutLine (out,string)
         DO i=count,1,-1
            call PutLine(out,message(i))
         END DO
      END IF
      count = 0
      return

      ENTRY abort (string)
      call PutLine(out,'***** JOB TERMINATED ^t^b')
      call PutLine (out,string)
      DO i=count,1,-1
         call PutLine(out,message(i))
      END DO
      stop ' '
      end


      function falloc (lnum)

c     Provides an available logical unit number to open a file.
c     Return value is falloc=lnum (>0) if successful, -1 otherwise
c
c     usage:       integer*4   falloc,lnum
c                  lnum = falloc(lnum)

      include      'standard.aph'
      integer*4    falloc,dalloc,lnum
      logical*1    isopen(MAXFILE)
      save         isopen
      data         isopen / MAXFILE*.false./ !units initially unassigned

      lnum=MINFILE
      DO WHILE (lnum.le.MAXFILE .and. isopen(lnum) )
         lnum = lnum + 1
      END DO
      IF (lnum.gt.MAXFILE) THEN
         lnum=-1
      ELSE
         isopen(lnum) = .true.
      END IF
      falloc = lnum
      return

      ENTRY dalloc (lnum)

c     Deallocates a logical unit number no longer needed.
c     Return value is always dalloc = lnum
c
c     usage:       integer*4   dalloc,lnum
c                  lnum = dalloc(lnum)


      if (lnum.ge.MINFILE .and. lnum.le.MAXFILE) isopen(lnum) = .false.
      dalloc = lnum

      end


      function countR8 (string,xarray,n)

c     reads and counts elements of an array from input line

      include      'standard.aph'
      integer*4    countR8,countI4,    ! number of values; -1 for error
     -             n,                  ! number of values read
     -             iarray(*)           ! array of integers
      real*8       xarray(*)           ! array of reals
      character    string*(*)          ! line to be read

      integer*4    ivalue,lp
      real*8       xvalue
      character    GetWord*(MAXCHAR),word*32
      logical*1    valR8,valI4,success

      lp = 1
      n = 0
      success = .true.
      DO WHILE (GetWord(string,word,lp).ne.NULL)
         IF (valR8(word,xvalue)) THEN
            n = n + 1
            xarray(n) = xvalue
         ELSE
            call PutLine(STDOUT,'cannot read '//word)
            call warning('cannot read '//word)
            success = .false.
         END IF
      END DO
      IF (success) THEN
         countR8 = n
      ELSE
         countR8 = -1
      END IF
      return

      ENTRY countI4(string,iarray,n)
      lp = 1
      n = 0
      success = .true.
      DO WHILE (GetWord(string,word,lp).ne.NULL)
         IF (valI4(word,ivalue)) THEN
            n = n + 1
            iarray(n) = ivalue
         ELSE
            call PutLine(STDOUT,'cannot read '//word)
            call warning('cannot read '//word)
            success = .false.
         END IF
      END DO
      IF (success) THEN
         countI4 = n
      ELSE
         countI4 = -1
      END IF
      end

      function valR8 (string, valueR8)

c     converts a string to a number (real*8, real*4, int*4, or int*2)

      include      'standard.aph'
      logical*1    valR8,valI4,valR4,valI2    !  .true. if successful
      character    string*(*)                 !  input string
      real*8       valueR8                    !  these are the outputs,
      real*4       valueR4                    !  depending on the
      integer*4    valueI4,len_trim           !  entry point
      integer*2    valueI2

      integer      flag,ln
      character    fmt*32

      IF (string.ne.NULL) THEN
         ln = len_trim(string)
         write(fmt,'(''(f'',i10,''.0)'')') ln
         read (string,fmt,iostat=flag) valueR8
      ELSE
         flag = -1
      END IF
      IF (flag.eq.OK) THEN
         valR8 = .true.
      ELSE
         valR8 = .false.
      END IF
      return

      ENTRY valR4 (string,valueR4)
      IF (string.ne.NULL) THEN
         ln = len_trim(string)
         write(fmt,'(''(f'',i10,''.0)'')') ln
         read (string,fmt,iostat=flag) valueR4
      ELSE
         flag = -1
      END IF
      IF (flag.eq.OK) THEN
         valR4 = .true.
      ELSE
         valR4 = .false.
      END IF
      return

      ENTRY valI4 (string,valueI4)
      IF (string.ne.NULL) THEN
         ln = len_trim(string)
         write(fmt,'(''(i'',i10,'')'')') ln
         read (string,fmt,iostat=flag) valueI4
      ELSE
         flag = -1
      END IF
      IF (flag.eq.OK) THEN
         valI4 = .true.
      ELSE
         valI4 = .false.
      END IF
      return


      ENTRY valI2 (string,valueI2)
      IF (string.ne.NULL) THEN
         ln = len_trim(string)
         write(fmt,'(''(i'',i10,'')'')') ln
         read (string,fmt,iostat=flag) valueI2
      ELSE
         flag = -1
      END IF
      IF (flag.eq.OK) THEN
         valI2 = .true.
      ELSE
         valI2 = .false.
      END IF
      end

      function GetWord (line,word,lp)

*     returns the next word from a line, starting at position lp

      include      'standard.aph'
      character    GetWord*(*),word*(*), !  GetWord = word = the next
     -             line*(*)              !  word in the string line(lp:)
      integer*4    lp,len_trim           !  lp is incremented on output


*     This routine considers a word to be a substring of the input
*     string line(lp:) delimited by characters from the following set:
*     blank ( ), comma(,), or equals (=).  A word can also be delimited
*     by quotation marks("..."), which allows embedding blanks or any
*     other character.
*     The routine returns the NULL string when no more words are in
*     the line.

      integer*4    lnth, wordbgn, wordend, endquote

      lnth = len_trim(line)
      DO WHILE ( (line(lp:lp).eq.BLANK
     -       .or. line(lp:lp).eq.','
     -       .or. line(lp:lp).eq.'=') .and. lp.lt.lnth)
         lp = lp + 1
      END DO
      IF (line(lp:lp).eq.DQUOTE) THEN
         wordbgn = lp + 1
         lp = wordbgn
         endquote = index(line(wordbgn:lnth),DQUOTE)
         IF (endquote.eq.0) THEN
            wordend = lnth
            lp = wordend + 1
         ELSE
            wordend = wordbgn + endquote - 2
            lp = wordend + 2
         END IF
      ELSE
         wordbgn = lp
         DO WHILE (  line(lp:lp).ne.BLANK
     -         .and. line(lp:lp).ne.','
     -         .and. line(lp:lp).ne.'='  .and. lp.le.lnth)
            lp = lp + 1
         END DO
         wordend = lp-1
      END IF
      IF (wordend.ge.wordbgn) THEN
         word = line(wordbgn:wordend)
      ELSE
         word = NULL
      END IF
      GetWord = word
      end

      subroutine sort(n,array)

c     sorts array into ascending order

      implicit     none
      integer*4    n
      real*8       array(*)

      integer*4    i,j
      real*8       a

      DO j=2,n
         a = array(j)
         DO i=j-1,1,-1
            if (array(i).le.a) go to 10
            array(i+1) = array(i)
         END DO
         i=0
10       array(i+1) = a
      END DO
      end

      subroutine sort2(n,array,iarray,jarray)

c     sorts array into ascending order, also moves corresponding
c     elements of iarray and jarray

      implicit     none
      integer*4    n,iarray(*),jarray(*)
      real*8       array(*)

      integer*2    i,j,nn,ia,ja
      real*8       a

      nn = n
      DO j=2,nn
         a = array(j)
         ia = iarray(j)
         ja = jarray(j)
         DO i=j-1,1,-1
            if (array(i).le.a) go to 10
            array(i+1) = array(i)
            iarray(i+1) = iarray(i)
            jarray(i+1) = jarray(i)
         END DO
         i=0
10       array(i+1) = a
         iarray(i+1) = ia
         jarray(i+1) = ja
      END DO
      end

      function fprompt(string,type)

c     prompts for a file name and opens the file

      include      'standard.aph'
      integer*4    fprompt,            !  file number assigned
     -             type                !  file type (read,write,append)
      character    string*(*)          !  prompt message

      integer*4    fassign,ln,len_trim
      logical*1    success
      character    name*(MAXCHAR)

10    CONTINUE
         call PutLine(STDOUT,string)
         read (STDIN,*) name
         fprompt = fassign(fprompt,name,type)
         ln = len_trim(name)
         IF (fprompt.gt.0) THEN
            write (STDOUT,*) 'file '//name(1:ln)//' opened'
            success = .true.
         ELSE
            write (STDOUT,*) 'cannot open file '//name(1:ln)
            success = .false.
         END IF
      IF (.not.success) GO TO 10
      end

      subroutine fxprint (lnum,A,neq,nd)

c     prints all elements of a matrix

      implicit     none
      integer*4    lnum,neq,nd
      real*8       A(nd,*)

      integer*2    i,j,ibgn,iend

      DO j = 1,neq
         ibgn = 1
         iend = min(neq,5)
         DO WHILE (ibgn.le.iend)
            IF (ibgn.eq.1) THEN
               write (lnum,'(1x,i2,2x,1p5G14.5)') j,(A(j,i),i=ibgn,iend)
            ELSE
               write (lnum,'(5x,1p5G14.5)') (A(j,i),i=ibgn,iend)
            END IF
            ibgn = ibgn + 5
            iend = min(neq,iend+5)
         END DO
      END DO
      end

      function strI2(i2)

c     writes a number to a string and marks trailing blanks
c     for later deletion in putline
c
c     usage:       character    stri4*(maxchar),strr8*(maxchar)
c                  integer*4    i,lnum
c                  real*8       x
c                  call putline (lnum,'i = '//stri4(i)//' and x = '//strr8(x))

      include      'standard.aph'
      character    strI2*(*),strI4*(*),strR4*(*),strR8*(*)
      character    buf*32,dzero*32
      integer*2    i2
      integer*4    i4,iflag
      real*4       r4
      real*8       r8

c     ENTRY strI2(i2)
      write (buf,'(i32)',iostat=iflag) i2
      if (iflag.ne.OK) buf = '?'
      call lshift(strI2,buf)
      return

      ENTRY strI4(i4)
      write (buf,'(i32)',iostat=iflag) i4
      if (iflag.ne.OK) buf = '?'
      call lshift(strI4,buf)
      return

      ENTRY strR4(r4)
      IF (abs(r4).ge.1000.) THEN
         write (buf,'(1pE32.5)',iostat=iflag) r4
      ELSE
         write (buf,'(1pG32.5)',iostat=iflag) r4
      END IF
      if (iflag.ne.OK) buf = '?'
      call lshift(strR4,dzero(buf))
      return

      ENTRY strR8(r8)
      IF (abs(r8).ge.1000.) THEN
         write (buf,'(1pE32.5)',iostat=iflag) r8
      ELSE
         write (buf,'(1pG32.5)',iostat=iflag) r8
      END IF
      if (iflag.ne.OK) buf = '?'
      call lshift(strR8,dzero(buf))
      return
      end

      subroutine lshift (string,buf)

c     deletes leading and trailing blanks from buf, loads to string,
c     and inserts escape code for putline
c
c     usage:       character    string*(maxchar),buf*(maxchar)
c                  call lshift (string,buf)

      include      'standard.aph'
      character    string*(*),buf*(*)
      integer*4    ln, ln buf, ln str, fnb, fnbc, len_trim

      ln = len_trim(buf)
      ln str = len(string)
      fnb = fnbc(buf,fnb) ! position of first non blank character
      ln buf = ln + 1 - fnb  ! length less leading and trailing blanks
      IF (ln buf .gt. ln str) THEN
         string = '?'
      ELSE IF (ln buf .le. ln str - 4) THEN
         string(1:) = buf(fnb:ln)//ESCHAR//SKIP
         string(ln str -1:) = ESCHAR//SKIP
      ELSE
         string(1:) = buf(fnb:ln)
      END IF
      end

      character*(*) function dzero (string)

c     deletes extra zeros from string representing a floating point
c
c     usage:       character    dzero*(MAXCHAR),string*(MAXCHAR)
c                  string = dzero(string)

      include      'standard.aph'
      character    string*(*)
      integer*4    len_trim
      integer*2    ln,lt

      IF (index(string,'E') .gt. 0) THEN
         ln = index(string,'E') - 1
      ELSE
         ln = len_trim (string)
      END IF
      lt = ln
      DO WHILE (lt.ge.3 .and. string(lt-1:lt).eq.'00')
         lt = lt - 1
      END DO
      dzero = string(1:lt-1)//string(ln:)
      end


      function fnbc (string,fnb)

c     returns first non blank character of a string

      include      'standard.aph'
      integer*4    fnbc,fnb,ln
      character    string*(*)

      fnb = 1
      ln = len(string)
      DO WHILE (fnb.lt.ln .and. string(fnb:fnb).eq.blank)
         fnb = fnb + 1
      END DO
      fnbc = fnb
      end

      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
*     spline routine from Numerical Recipes
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

      subroutine splint(xa,ya,y2a,n,x,y)

c     routine from Numerical Recipes, modified 6-18-90 by Peet Hickman
c     this version checks whether last khi and klo are still good.

      real*4       xa(n),ya(n),y2a(n)
      integer*4    klo, khi
      character    strR4*16
      save         klo,khi
      data         klo/1/, khi/2/

      external     abort


      IF (xa(klo).gt.x .or. x.gt.xa(khi)) THEN
         klo=1
         khi=n
         DO WHILE (khi-klo.gt.1)
            k=(khi+klo)/2
            IF (xa(k).gt.x) THEN
               khi=k
            ELSE
               klo=k
            ENDIF
         END DO
      END IF
      h=xa(khi)-xa(klo)
      if (h.eq.0.) stop 'Error utility.for:751, splint' !call abort('splint: xa = '//strR4(xa)//' is bad')
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+
     -      ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      end


!*     file x3j6j9j.for assembles the routines for calculating
!*     3j, 6j, and 9j coefficients
!
!      function x3j(el1,el2,el3,em1,em2,em3)
!      integer triangle
!      dimension clist(100)
!
!      if ((em1+em2+em3.gt.0.001).or.(triangle(el1,el2,el3).eq.0)) then
!          x3j=0.
!      else
!          call threej(el1,el2,el3,em1,clist,em2min,em2max,200)
!          m2=nint(em2-em2min+1.)
!          x3j=clist(m2)
!      end if
!
!      return
!      end

      integer function triangle (x,y,z)

c     check if arguments satisfy triangle inequality

c     triangle = 0 if triangle inequality not satisfied
c                 or any argument is negative
c                         or x+y+z is not an integer
c     triangle = 1 if triangle inequality is satisfied
c                 and every argument is nonnegative
c                 and x+y+z is an integer

      i=nint(2.*x)
      j=nint(2.*y)
      k=nint(2.*z)

      triangle=0

      if ( i.ge.0 .and. j.ge.0 .and. k.ge.0
     *     .and. iabs(i-j).le.k .and. k.le.(i+j)
     *     .and. mod(i+j+k,2).eq.0 )
     *     triangle = 1

      return
      end


!      SUBROUTINE THREEJ(EL1,EL2,EL3,EM1,CLIST,EM2MIN,EM2MAX,N)
!C
!C     THE 3J COEFFICIENT   ( EL1   EL2      EL3    ) IS PUT IN
!C                          ( EM1   EM2  -(EM1+EM2) )
!C
!C     VALUES ARE CALCULATED FOR ALL ALLOWED EM2
!C
!      DIMENSION CLIST(1)
!      C(EM2)=SQRT((EL2-EM2+1.0)*(EL2+EM2)*(EL3-EM1-EM2+1.0)*(EL3
!     2   +EM1+EM2))
!      D(EM2)=EL2*(EL2+1.0)+EL3*(EL3+1.0)-EL1*(EL1+1.0)+2.0*EM2*
!     2   (-EM1-EM2)
!      M2(EM2)=EM2-EM2MIN+1.00001
!      DO 5 I=1,N
!      CLIST(I)=0.0
!5     CONTINUE
!      EM2MIN=AMAX1(-EL2,-(EL3+EM1))
!      EM2MAX=AMIN1(EL2,(EL3-EM1))
!      EM2DIF=EM2MAX-EM2MIN
!      EM2MID=EM2MIN+EM2DIF/2.0
!      IF(EM2DIF.LE.3.00001)EM2MID=EM2MAX
!      M2MIN=M2(EM2MIN)
!      M2MID=M2(EM2MID)
!      M2MAX=M2(EM2MAX)
!10    FORMAT(1P5E14.5)
!C     FORWARD RECURSION FROM EM2MIN TO EM2MID
!      GA=1.0
!      CLIST(1)=GA
!      IF(EM2DIF.LT.0.00001) GO TO 100
!      GB=-D(EM2MIN)/C(EM2MIN+1.0)
!      CLIST(2)=GB
!      IF(EM2DIF.LE.1.00001) GO TO 100
!      EM2=EM2MIN+1.0
!25    GC=(D(EM2)*GB+C(EM2)*GA)/(-C(EM2+1.))
!      GA=GB
!      GB=GC
!      CLIST(M2(EM2+1.0))=GC
!      EM2=EM2+1.0
!      IF(EM2.LE.(EM2MID-0.99999)) GO TO 25
!      SAVE=GC
!      SAVE2=GA
!C     IF NECESSARY, BACKWARD RECURSION FROM EM2MAX TO EM2MID
!      IF(EM2DIF.LE.3.00001) GO TO 100
!C     ADJUST MATCHING POINT TO AVOID POSSIBLE ZERO
!      IF(ABS(SAVE).GE.ABS(SAVE2)) GO TO 35
!      EM2MID=EM2MID-1.0
!      M2MID=M2MID-1
!      SAVE=SAVE2
!35    CONTINUE
!      GA=1.0
!      GB=-D(EM2MAX)/C(EM2MAX)
!      CLIST(M2(EM2MAX))=GA
!      CLIST(M2(EM2MAX-1.0))=GB
!      EM2=EM2MAX-1.0
!      NUMB=M2MAX-M2MID-1
!      DO 40 I=1,NUMB
!      GC=-(C(EM2+1.0)*GA+D(EM2)*GB)/C(EM2)
!      GA=GB
!      GB=GC
!      CLIST(M2(EM2-1.0))=GC
!      EM2=EM2-1.0
!40    CONTINUE
!C     COMPARE AT EM2MID AND RESCALE
!      FAC=SAVE/CLIST(M2MID)
!      DO 60 I=M2MID,M2MAX
!      CLIST(I)=CLIST(I)*FAC
!60    CONTINUE
!C     PERFORM SUM, NORMALIZE, AND ASSIGN CORRECT PHASE
!100   CONTINUE
!      SUM=0.0
!      DO 150 I=M2MIN,M2MAX
!      SUM=SUM+CLIST(I)*CLIST(I)
!150   CONTINUE
!      SUM=SUM*(2.0*EL1+1.0)
!      SUM=SQRT(SUM)
!C     IPHASE=ABS(EL1-EL3-EM1)
!C     CHANGE MADE AUG 4, 1987 FOLLOWING BUG FIND BY WKB
!      IPHASE=ABS(EL2-EL3-EM1)
!      PHASE=(-1.0)**MOD(IPHASE,2)
!      IF((PHASE*CLIST(M2MAX)).LT.0.) SUM=-SUM
!      DO 200 I=M2MIN,M2MAX
!      CLIST(I)=CLIST(I)/SUM
!200   CONTINUE
!      RETURN
!      END


!      subroutine sixj(j2,j3,l1,l2,l3,clist,j1min,j1max,n)
!c
!c     the 6-j coefficient (j1 j2 j3)  is stored in clist(int(j1+1))
!c                         (l1 l2 l3)
!c
!c     values are calculated for all allowed j1, using recursion formulas
!c     from Schulten and Gordon, J. Math. Phys. _16_, 1961 (1975).
!c
!c     written by Peet Hickman.  version 6-30-89 includes half
!c     integral j, and least squares matching of forward and
!c     backward recursion at three points.  still no overflow protection.
!c     version 10-23-90 cleans up things a little more.
!
!      integer*4    triangle
!      real*4       clist(*),j1,j2,j3,l1,l2,l3,j1min,j1max,jdif,j1mid
!      e(j1)=sqrt((j1*j1-(j2-j3)*(j2-j3))*((j2+j3+1.)*(j2+j3+
!     -  1.)-j1*j1)*(j1*j1-(l2-l3)*(l2-l3))*((l2+l3+1.)*(l2+
!     -  l3+1.) -j1*j1))
!      f(j1)=(2.*j1+1.)*(j1*(j1+1.)*(-j1*(j1+1.)+j2*(j2+1.)+j3*
!     -  (j3+1.))+l2*(l2+1.)*(j1*(j1+1.)+j2*(j2+1.)-j3*(j3+1.))
!     -  +l3*(l3+1.)*(j1*(j1+1.)-j2*(j2+1.)+j3*(j3+1.))
!     -  -2.*j1*(j1+1.)*l1*(l1+1.))
!
!      DO i=1,n
!         clist(i)=0.
!      END DO
!      j1min=amax1(abs(j2-j3),abs(l2-l3))
!      j1max=amin1((j2+j3),(l2+l3))
!c     return all zeros if triangle inequality not satisfied
!      if(triangle(j1min,j2,j3).eq.0 .or. triangle(j1min,l2,l3).eq.0
!     * .or. triangle(j2,l1,l3).eq.0 .or. triangle(j3,l1,l2).eq.0) return
!      jdif=j1max-j1min
!      j1mid = j1min + float(nint(jdif/2.))
!      if (nint(jdif).le.3) j1mid=j1max
!c     start forward recursion from j1min to j1mid
!      ha=1.
!      clist(nint(j1min+1.))=ha
!c     if jdif.ge.1, do second term in recursion
!      IF (nint(jdif).ge.1) THEN
!         IF(nint(j1min).gt.0) THEN
!            hb=-f(j1min)/j1min/e(j1min+1.)
!         ELSE
!            xxj2=j2*(j2+1.)
!            xxl2=l2*(l2+1.)
!            hb=-(xxj2+xxl2-l1*(l1+1.))/sqrt(4.*xxj2*xxl2)
!         END IF
!         clist(nint(j1min+2.))=hb
!      END IF
!c     if jdif.ge.2, do third term through j1mid-th term
!      IF (nint(jdif).ge.2) THEN
!         DO j1=j1min+1.,j1mid-1.+.001,1.0
!            hc=(f(j1)*hb+(j1+1.)*e(j1)*ha)/(-j1*e(j1+1.))
!            ha=hb
!            hb=hc
!            clist(nint(j1+2.))=hc
!         END DO
!      END IF
!c     if jdif = 3, recursion is complete, because j1mid=j1max
!c     if jdif.ge.4, do backward recursion from j1max to j1mid
!      IF (nint(jdif).ge.4) THEN
!         save=hc
!         savem1=ha
!         savep1=
!     -    (f(j1mid)*hb+(j1mid+1.)*e(j1mid)*ha)/(-j1mid*e(j1mid+1.))
!         ha=1.
!         clist(nint(j1max+1.))=ha
!         hb=-f(j1max)/(j1max+1.)/e(j1max)
!         clist(nint(j1max))=hb
!         j1=j1max
!         DO i=1,nint(j1max-j1mid-1.)+1
!            j1=j1-1.
!            hc=-(j1*e(j1+1.)*ha+f(j1)*hb)/(j1+1.)/e(j1)
!            ha=hb
!            hb=hc
!            clist(nint(j1))=hc
!         END DO
!c        compare at j1mid-1, j1mid, j1mid+1 and rescale
!         fac= (savem1*clist(nint(j1mid)) + save *
!     -      clist(nint(j1mid+1.))+savep1*clist(nint(j1mid+2.)))
!     -      /(clist(nint(j1mid))**2 + clist(nint(j1mid+1.))**2
!     -      + clist(nint(j1mid+2.))**2)
!         DO j1=j1mid-1.,j1max+.001,1.0
!            clist(int(j1+1.001))=clist(int(j1+1.001))*fac
!         END DO
!      END IF
!c     perform sum, normalize, and assign correct phase
!      sum=0.
!      DO j1=j1min,j1max+.001,1.0
!         sum=sum+clist(int(j1+1.001))*clist(int(j1+1.001))*(2.*j1+1.)
!      END DO
!      sum=sum*(2.*l1+1.)
!      sum=sqrt(sum)
!c     evaluate (-1)**(j2+j3+l2+l3)
!      IF ( mod(nint(j2+j3+l2+l3),2) .eq. 0 ) THEN
!         phase= 1
!      ELSE
!         phase=-1
!      END IF
!      if((phase*clist(int(j1max+1.001))).lt.0.) sum=-sum
!      DO j1=j1min,j1max+.001,1.0
!         clist(int(j1+1.001))=clist(int(j1+1.001))/sum
!      END DO
!      return
!      end


!      real*4 function ninej(j1,j2,j3,j4,j5,j6,j7,j8,j9)
!
!c     calculates 9j symbol as a sum over 6j symbols
!
!c     version 6-30-89 by Peet Hickman
!
!      real *4 j1,j2,j3,j4,j5,j6,j7,j8,j9
!      dimension clist(200),dlist(200),elist(200)
!
!c     get the three strings of 6j symbols
!      call sixj(j1,j9,j3,j6,j2,clist,cmin,cmax,200)
!      call sixj(j2,j6,j5,j4,j8,dlist,dmin,dmax,200)
!      call sixj(j9,j1,j7,j4,j8,elist,emin,emax,200)
!
!c     limits for sum
!      gmin=amax1(cmin,dmin,emin)
!      gmax=amin1(cmax,dmax,emax)
!
!c     return zero if no terms in sum
!      ninej=0.
!      if (gmin.gt.gmax+.001) return
!
!c     otherwise evaluate the sum
!      do 100 g=gmin,gmax+.001,1.
!          index=int(g+1.001)
!          ninej=ninej+(2.*g+1.)*clist(index)*dlist(index)*elist(index)
! 100      continue
!      phase=1.
!      if ( abs(mod(2.*gmin,2.)-1.) .le. 0.001) phase=-1.
!      ninej=ninej*phase
!
!      return
!      end


      function ddiag(A,n,nd,nvec,E,U)

c     diagonalize symmetric matrix A and (optionally) get eigenvectors

      include      'standard.aph'
      integer*4    ddiag,       !   0 for normal return, -1 otherwise
     -             n,nd,        !   used and declared row dimension of A
     -             nvec         !   number of eigenvectors desired
      real*8       A(nd,*),     !   matrix to diagonalize (is destroyed)
     -             E(*),        !   returns eigenvalues
     -             U(nd,*)      !   matrix of eigenvectors


c     The eigenvalues are returned in ascending order.
c     If no eigenvectors are desired, set nvec=0.  In this case, the
c     array U is not referred to at all, so a construction such as
c
c     if (ddiag(A,n,nd,0,E,A).ne.0) call abort('problem in ddiag')
c
c     can be used to avoid using another matrix for U.
c     The maximum size of matrix that this routine will accept is
c     controlled by the parameter MAXSIZE, defined below.

      integer*4    MAXSIZE,djacobi
      parameter    (MAXSIZE=600)
      integer*4    ierr,iwk(MAXSIZE)
      real*8       wk(8*MAXSIZE)
      character    message*(MAXCHAR), strI4*16

      ddiag = 0
      IF (n.gt.nd .or. nvec.gt.nd .or. n.le.0 .or. nvec.lt.0) THEN
         ddiag = -1
         call warning ('ddiag: bad input')
      ELSE IF (n.gt.MAXSIZE) THEN
         ddiag = -1
         message='ddiag: n = '//stri4(n)//', MAXSIZE = '//strI4(MAXSIZE)
         call warning (message)
      ELSE IF (n.le.4) THEN
         ierr = djacobi(A,n,nd,nvec,U,E)
      ELSE ! (n.gt.4 .and. n.le.MAXSIZE)
         call rsm(nd,n,A,E,nvec,U,wk,iwk,ierr)
         if (ierr.ne.0) ddiag = -1
      END IF
      end


      INTEGER FUNCTION DJACOBI(H,nbf,ndcl,NVEC,U,X)
c     transferred from MPLVAX to 386 by Peet Hickman 12-28-89
C     DJCBI:  D. L. Huestis, SRI 04-22-86
C     JACOBI: D. L. HUESTIS, SRI 12-18-82
C     LAST MODIFICATION:  05-06-86
C
C     DIAGONALIZATION OF SMALL MATRICES
C     USING JACOBI METHOD
C     BASED ON HDIAG FROM CALTECH
C
*     changed by Peet Hickman 8/4/90 as follows:
*          if nvec=0, U is not used at all (even for n=1)
*          eigenvalues are returned in ascending order
*          return code changed:  djacobi=0 for success, -1 for failure
*          failure is nonconvergence after MAXITS iterations

      IMPLICIT REAL*8(A-H,O-Q,S-Z)
      IMPLICIT REAL*8(R)
      COMPLEX*16 DCMPLX
      REAL*8 RAP,HMAX,HDTEST,ABS,SQRT,DIF,ROOT
      parameter (MAXITS=50)
      DIMENSION H(ndcl,1),U(ndcl,1),X(1)
      EQUIVALENCE (R0,Z0),(RHALF,ZHALF),(R1,Z1),(R2,Z2)
      DATA Z0,ZHALF,Z1,Z2/0.0D0,0.5D0,1.0D0,2.0D0/
C     DATA RAP/1.E-10/
      DATA RAP/1.D-20/
C
C     INITIALIZE ----------------------------------------------------
C
      N=NBF
      N1=N-1
      NR=0
      IF (N.eq.1) THEN
         X(1)=H(1,1)
         if (nvec.ge.1) U(1,1)=Z1
         djacobi = 0
         return
      END IF
90    IF(NVEC.LE.0)GOTO 120
      DO 110 I=1,N1
      DO 100 J=I+1,N
      U(I,J)=Z0
100   U(J,I)=Z0
110   U(I,I)=Z1
      U(N,N)=Z1
120   HDTEST=R0
      DO 140 I=1,N1
      DO 130 J=I+1,N
130   HDTEST=HDTEST+ABS(H(I,J))**2
140   HDTEST=HDTEST+RHALF*ABS(H(I,I))**2
      HDTEST=RAP*(SQRT((R2*HDTEST+ABS(H(N,N))**2)/FLOAT(N))+R1)
C
C     SEARCH FOR LARGEST OFF-DIAGONAL ELEMENT ------------------------
C
200   HMAX=R0
      DO 210 I=1,N1
      DO 205 J=I+1,N
      IF(HMAX.GE.ABS(H(I,J)))GO TO 205
      HMAX=ABS(H(I,J))
      IPIV=I
      JPIV=J
205   CONTINUE
210   CONTINUE
      IF(HMAX.LE.HDTEST)GO TO 500
C
C     COMPUTE ROTATION SINE AND COSINE -----------------------------
C
      NR=NR+1
      IF (nr.gt.MAXITS) THEN
         djacobi = -1
         return
      END IF
      DIF=RHALF*(H(IPIV,IPIV)-H(JPIV,JPIV))
      ROOT=CDABS(DCMPLX(DIF,H(IPIV,JPIV)))
      COSINE=SQRT(RHALF*(ABS(DIF)+ROOT)/ROOT)
      SINE=SIGN(RHALF,DIF)*H(IPIV,JPIV)/(ROOT*COSINE)
      TANGEN=SINE/COSINE
      TAU=SINE/(Z1+COSINE)
C
C     TRANSFORM ELEMENTS OF H -------------------------------
C
      DO 390 I=1,N
      IF(I-IPIV)310,390,340
310   HTEMP=H(I,IPIV)
      H(I,IPIV)=HTEMP     + SINE*(H(I,JPIV)-TAU*HTEMP)
      GO TO 355
340   HTEMP=H(IPIV,I)
      IF(I-JPIV)350,390,370
350   H(IPIV,I)=HTEMP     + SINE*(H(I,JPIV)-TAU*HTEMP)
355   H(I,JPIV)=H(I,JPIV) - SINE*(HTEMP+TAU*H(I,JPIV))
      GO TO 390
370   H(IPIV,I)=HTEMP     + SINE*(H(JPIV,I)-TAU*HTEMP)
      H(JPIV,I)=H(JPIV,I) - SINE*(HTEMP+TAU*H(JPIV,I))
390   CONTINUE
      H(IPIV,IPIV)=H(IPIV,IPIV) + TANGEN*H(IPIV,JPIV)
      H(JPIV,JPIV)=H(JPIV,JPIV) - TANGEN*H(IPIV,JPIV)
      H(IPIV,JPIV)=Z0
C
C     TRANSFORM U ---------------------------------------
C
      IF(NVEC.LE.0)GO TO 200
      DO 490 I=1,N
      HTEMP=U(I,IPIV)
      U(I,IPIV)=HTEMP     + SINE*(U(I,JPIV)-TAU*HTEMP)
490   U(I,JPIV)=U(I,JPIV) - SINE*(HTEMP+TAU*U(I,JPIV))
C
      GO TO 200
C
C     ORDER EIGENVALUES ------------------------------------------
C
500   DO 510 K=1,N
510   X(K)=H(K,K)
520   IPIV=0
      DO 530 K=1,N1
      IF(X(K).le.X(K+1))GO TO 530
*       changed by APH 8/4/90 so eigenvalues are in ascending order
      IPIV=IPIV+1
      HTEMP=X(K)
      X(K)=X(K+1)
      X(K+1)=HTEMP
      IF(NVEC.LE.0)GO TO 530
      DO 525 I=1,N
      HTEMP=U(I,K)
      U(I,K)=U(I,K+1)
525   U(I,K+1)=HTEMP
530   CONTINUE
      IF(IPIV.GT.0)GO TO 520
C
C     ORTHONORMALIZE EIGENVECTORS ----------------------------------
C
      IF(NVEC.LE.0)GO TO 900
      DO 690 K=1,N
      IF(K.EQ.1)GO TO 650
      DO 640 J=1,K-1
      HTEMP=Z0
      DO 620 I=1,N
620   HTEMP=HTEMP+U(I,J)*U(I,K)
      DO 630 I=1,N
630   U(I,K)=U(I,K)-HTEMP*U(I,J)
640   CONTINUE
650   HMAX=R0
      DO 660 I=1,N
660   HMAX=HMAX+ABS(U(I,K))**2
      HTEMP=R1/SQRT(HMAX)
      DO 670 I=1,N
670   U(I,K)=HTEMP*U(I,K)
690   CONTINUE
C
900   djacobi=0
      RETURN
      END

      subroutine rsm(nm,n,a,w,m,z,fwork,iwork,ierr)
c
      integer n,nm,m,iwork(n),ierr
      integer k1,k2,k3,k4,k5,k6,k7
*     double precision a(nm,n),w(n),z(nm,m),fwork(1)
      double precision a(nm,n),w(n),z(nm,*),fwork(1)
*     August 3, 1990  change by APH: column dimension of z changed.
*     (m=0 to disable calculation of eigenvectors led to zero dimension)
*     note that if m=0, the array z is never used
c
c     this subroutine calls the recommended sequence of
c     subroutines from the eigensystem subroutine package (eispack)
c     to find all of the eigenvalues and some of the eigenvectors
c     of a real symmetric matrix.
c
c     on input
c
c        nm  must be set to the row dimension of the two-dimensional
c        array parameters as declared in the calling program
c        dimension statement.
c
c        n  is the order of the matrix  a.
c
c        a  contains the real symmetric matrix.
c
c        m  the eigenvectors corresponding to the first m eigenvalues
c           are to be computed.
c           if m = 0 then no eigenvectors are computed.
c           if m = n then all of the eigenvectors are computed.
c
c     on output
c
c        w  contains all n eigenvalues in ascending order.
c
c        z  contains the orthonormal eigenvectors associated with
c           the first m eigenvalues.
c
c        ierr  is an integer output variable set equal to an error
c           completion code described in the documentation for tqlrat,
c           imtqlv and tinvit.  the normal completion code is zero.
c
c        fwork  is a temporary storage array of dimension 8*n.
c
c        iwork  is an integer temporary storage array of dimension n.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
*     This code changed August 3, 1990 by APH to ensure that an error
*     return (ierr.ne.0) from any of the subroutines is reported.

      IF (n .gt. nm .or. m .gt. nm) THEN
         ierr = 10 * n
         return
      END IF
      k1 = 1
      k2 = k1 + n
      k3 = k2 + n
      k4 = k3 + n
      k5 = k4 + n
      k6 = k5 + n
      k7 = k6 + n
      k8 = k7 + n
      IF (m .eq. 0) THEN
*        find eigenvalues only
         call tred1(nm,n,a,w,fwork(k1),fwork(k2))
         call tqlrat(n,w,fwork(k2),ierr)
      ELSE
*        find all eigenvalues and m eigenvectors
         call tred1(nm,n,a,fwork(k1),fwork(k2),fwork(k3))
         call imtqlv(n,fwork(k1),fwork(k2),fwork(k3),w,iwork,
     -                ierr,fwork(k4))
         if (ierr.gt.0) return
         call tinvit(nm,n,fwork(k1),fwork(k2),fwork(k3),m,w,iwork,z,
     -        ierr,fwork(k4),fwork(k5),fwork(k6),fwork(k7),fwork(k8))
         if (ierr.lt.0) return
         call trbak1(nm,n,a,fwork(k2),m,z)
      END IF
      return
      end
      subroutine tinvit(nm,n,d,e,e2,m,w,ind,z,
     x                  ierr,rv1,rv2,rv3,rv4,rv6)
c
      integer i,j,m,n,p,q,r,s,ii,ip,jj,nm,its,tag,ierr,group
      double precision d(n),e(n),e2(n),w(m),z(nm,m),
     x       rv1(n),rv2(n),rv3(n),rv4(n),rv6(n)
      double precision u,v,uk,xu,x0,x1,eps2,eps3,eps4,norm,order,epslon,
     x       pythag
      integer ind(m)
c
c     this subroutine is a translation of the inverse iteration tech-
c     nique in the algol procedure tristurm by peters and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 418-439(1971).
c
c     this subroutine finds those eigenvectors of a tridiagonal
c     symmetric matrix corresponding to specified eigenvalues,
c     using inverse iteration.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c
c        e2 contains the squares of the corresponding elements of e,
c          with zeros corresponding to negligible elements of e.
c          e(i) is considered negligible if it is not larger than
c          the product of the relative machine precision and the sum
c          of the magnitudes of d(i) and d(i-1).  e2(1) must contain
c          0.0d0 if the eigenvalues are in ascending order, or 2.0d0
c          if the eigenvalues are in descending order.  if  bisect,
c          tridib, or  imtqlv  has been used to find the eigenvalues,
c          their output e2 array is exactly what is expected here.
c
c        m is the number of specified eigenvalues.
c
c        w contains the m eigenvalues in ascending or descending order.
c
c        ind contains in its first m positions the submatrix indices
c          associated with the corresponding eigenvalues in w --
c          1 for eigenvalues belonging to the first submatrix from
c          the top, 2 for those belonging to the second submatrix, etc.
c
c     on output
c
c        all input arrays are unaltered.
c
c        z contains the associated set of orthonormal eigenvectors.
c          any vector which fails to converge is set to zero.
c
c        ierr is set to
c          zero       for normal return,
c          -r         if the eigenvector corresponding to the r-th
c                     eigenvalue fails to converge in 5 iterations.
c
c        rv1, rv2, rv3, rv4, and rv6 are temporary storage arrays.
c
c     calls pythag for  dsqrt(a*a + b*b) .
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      if (m .eq. 0) go to 1001
      tag = 0
      order = 1.0d0 - e2(1)
      q = 0
c     .......... establish and process next submatrix ..........
  100 p = q + 1
c
      do 120 q = p, n
         if (q .eq. n) go to 140
         if (e2(q+1) .eq. 0.0d0) go to 140
  120 continue
c     .......... find vectors by inverse iteration ..........
  140 tag = tag + 1
      s = 0
c
      do 920 r = 1, m
         if (ind(r) .ne. tag) go to 920
         its = 1
         x1 = w(r)
         if (s .ne. 0) go to 510
c     .......... check for isolated root ..........
         xu = 1.0d0
         if (p .ne. q) go to 490
         rv6(p) = 1.0d0
         go to 870
  490    norm = dabs(d(p))
         ip = p + 1
c
         do 500 i = ip, q
  500    norm = dmax1(norm, dabs(d(i))+dabs(e(i)))
c     .......... eps2 is the criterion for grouping,
c                eps3 replaces zero pivots and equal
c                roots are modified by eps3,
c                eps4 is taken very small to avoid overflow ..........
         eps2 = 1.0d-3 * norm
         eps3 = epslon(norm)
         uk = q - p + 1
         eps4 = uk * eps3
         uk = eps4 / dsqrt(uk)
         s = p
  505    group = 0
         go to 520
c     .......... look for close or coincident roots ..........
  510    if (dabs(x1-x0) .ge. eps2) go to 505
         group = group + 1
         if (order * (x1 - x0) .le. 0.0d0) x1 = x0 + order * eps3
c     .......... elimination with interchanges and
c                initialization of vector ..........
  520    v = 0.0d0
c
         do 580 i = p, q
            rv6(i) = uk
            if (i .eq. p) go to 560
            if (dabs(e(i)) .lt. dabs(u)) go to 540
c     .......... warning -- a divide check may occur here if
c                e2 array has not been specified correctly ..........
            xu = u / e(i)
            rv4(i) = xu
            rv1(i-1) = e(i)
            rv2(i-1) = d(i) - x1
            rv3(i-1) = 0.0d0
            if (i .ne. q) rv3(i-1) = e(i+1)
            u = v - xu * rv2(i-1)
            v = -xu * rv3(i-1)
            go to 580
  540       xu = e(i) / u
            rv4(i) = xu
            rv1(i-1) = u
            rv2(i-1) = v
            rv3(i-1) = 0.0d0
  560       u = d(i) - x1 - xu * v
            if (i .ne. q) v = e(i+1)
  580    continue
c
         if (u .eq. 0.0d0) u = eps3
         rv1(q) = u
         rv2(q) = 0.0d0
         rv3(q) = 0.0d0
c     .......... back substitution
c                for i=q step -1 until p do -- ..........
  600    do 620 ii = p, q
            i = p + q - ii
            rv6(i) = (rv6(i) - u * rv2(i) - v * rv3(i)) / rv1(i)
            v = u
            u = rv6(i)
  620    continue
c     .......... orthogonalize with respect to previous
c                members of group ..........
         if (group .eq. 0) go to 700
         j = r
c
         do 680 jj = 1, group
  630       j = j - 1
            if (ind(j) .ne. tag) go to 630
            xu = 0.0d0
c
            do 640 i = p, q
  640       xu = xu + rv6(i) * z(i,j)
c
            do 660 i = p, q
  660       rv6(i) = rv6(i) - xu * z(i,j)
c
  680    continue
c
  700    norm = 0.0d0
c
         do 720 i = p, q
  720    norm = norm + dabs(rv6(i))
c
         if (norm .ge. 1.0d0) go to 840
c     .......... forward substitution ..........
         if (its .eq. 5) go to 830
         if (norm .ne. 0.0d0) go to 740
         rv6(s) = eps4
         s = s + 1
         if (s .gt. q) s = p
         go to 780
  740    xu = eps4 / norm
c
         do 760 i = p, q
  760    rv6(i) = rv6(i) * xu
c     .......... elimination operations on next vector
c                iterate ..........
  780    do 820 i = ip, q
            u = rv6(i)
c     .......... if rv1(i-1) .eq. e(i), a row interchange
c                was performed earlier in the
c                triangularization process ..........
            if (rv1(i-1) .ne. e(i)) go to 800
            u = rv6(i-1)
            rv6(i-1) = rv6(i)
  800       rv6(i) = u - rv4(i) * rv6(i-1)
  820    continue
c
         its = its + 1
         go to 600
c     .......... set error -- non-converged eigenvector ..........
  830    ierr = -r
         xu = 0.0d0
         go to 870
c     .......... normalize so that sum of squares is
c                1 and expand to full order ..........
  840    u = 0.0d0
c
         do 860 i = p, q
  860    u = pythag(u,rv6(i))
c
         xu = 1.0d0 / u
c
  870    do 880 i = 1, n
  880    z(i,r) = 0.0d0
c
         do 900 i = p, q
  900    z(i,r) = rv6(i) * xu
c
         x0 = x1
  920 continue
c
      if (q .lt. n) go to 100
 1001 return
      end
**** for old version, "send otqlrat from eispack"
** From dana!moler Tue, 1 Sep 87 10:15:40 PDT
** New TQLRAT
      SUBROUTINE TQLRAT(N,D,E2,IERR)
C
      INTEGER I,J,L,M,N,II,L1,MML,IERR
      DOUBLE PRECISION D(N),E2(N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,T,EPSLON,PYTHAG
C
C     This subroutine is a translation of the Algol procedure tqlrat,
C     Algorithm 464, Comm. ACM 16, 689(1973) by Reinsch.
C
C     This subroutine finds the eigenvalues of a symmetric
C     tridiagonal matrix by the rational QL method.
C
C     On input
C
C        N is the order of the matrix.
C
C        D contains the diagonal elements of the input matrix.
C
C        E2 contains the squares of the subdiagonal elements of the
C          input matrix in its last N-1 positions.  E2(1) is arbitrary.
C
C      On output
C
C        D contains the eigenvalues in ascending order.  If an
C          error exit is made, the eigenvalues are correct and
C          ordered for indices 1,2,...IERR-1, but may not be
C          the smallest eigenvalues.
C
C        E2 has been destroyed.
C
C        IERR is set to
C          zero       for normal return,
C          J          if the J-th eigenvalue has not been
C                     determined after 30 iterations.
C
C     Calls PYTHAG for  DSQRT(A*A + B*B) .
C
C     Questions and comments should be directed to Burton S. Garbow,
C     Mathematics and Computer Science Div, Argonne National Laboratory
C
C     This version dated August 1987.
C     Modified by C. Moler to fix underflow/overflow difficulties,
C     especially on the VAX and other machines where epslon(1.0d0)**2
C     nearly underflows.  See the loop involving statement 102 and
C     the two statements just before statement 200.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E2(I-1) = E2(I)
C
      F = 0.0D0
      T = 0.0D0
      E2(N) = 0.0D0
C
      DO 290 L = 1, N
         J = 0
         H = DABS(D(L)) + DSQRT(E2(L))
         IF (T .GT. H) GO TO 105
         T = H
         B = EPSLON(T)
         C = B * B
         if (c .ne. 0.0d0) go to 105
C        Spliting tolerance underflowed.  Look for larger value.
         do 102 i = l, n
            h = dabs(d(i)) + dsqrt(e2(i))
            if (h .gt. t) t = h
  102    continue
         b = epslon(t)
         c = b * b
C     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (E2(M) .LE. C) GO TO 120
C     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         S = DSQRT(E2(L))
         G = D(L)
         P = (D(L1) - G) / (2.0D0 * S)
         R = PYTHAG(P,1.0D0)
         D(L) = S / (P + DSIGN(R,P))
         H = G - D(L)
C
         DO 140 I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     .......... RATIONAL QL TRANSFORMATION ..........
         G = D(M)
         IF (G .EQ. 0.0D0) G = B
         H = G
         S = 0.0D0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
C           Avoid division by zero on next pass
            if (g .eq. 0.0d0) g = epslon(d(i))
            h = g * (p / r)
  200    CONTINUE
C
         E2(L) = S * G
         D(L) = H
C     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
         IF (H .EQ. 0.0D0) GO TO 210
         IF (DABS(E2(L)) .LE. DABS(C/H)) GO TO 210
         E2(L) = H * E2(L)
         IF (E2(L) .NE. 0.0D0) GO TO 130
  210    P = D(L) + F
C     .......... ORDER EIGENVALUES ..........
         IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
      subroutine trbak1(nm,n,a,e,m,z)
c
      integer i,j,k,l,m,n,nm
      double precision a(nm,n),e(n),z(nm,m)
      double precision s
c
c     this subroutine is a translation of the algol procedure trbak1,
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine forms the eigenvectors of a real symmetric
c     matrix by back transforming those of the corresponding
c     symmetric tridiagonal matrix determined by  tred1.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        a contains information about the orthogonal trans-
c          formations used in the reduction by  tred1
c          in its strict lower triangle.
c
c        e contains the subdiagonal elements of the tridiagonal
c          matrix in its last n-1 positions.  e(1) is arbitrary.
c
c        m is the number of eigenvectors to be back transformed.
c
c        z contains the eigenvectors to be back transformed
c          in its first m columns.
c
c     on output
c
c        z contains the transformed eigenvectors
c          in its first m columns.
c
c     note that trbak1 preserves vector euclidean norms.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      if (m .eq. 0) go to 200
      if (n .eq. 1) go to 200
c
      do 140 i = 2, n
         l = i - 1
         if (e(i) .eq. 0.0d0) go to 140
c
         do 130 j = 1, m
            s = 0.0d0
c
            do 110 k = 1, l
  110       s = s + a(i,k) * z(k,j)
c     .......... divisor below is negative of h formed in tred1.
c                double division avoids possible underflow ..........
            s = (s / a(i,l)) / e(i)
c
            do 120 k = 1, l
  120       z(k,j) = z(k,j) + s * a(i,k)
c
  130    continue
c
  140 continue
c
  200 return
      end
      subroutine tred1(nm,n,a,d,e,e2)
c
      integer i,j,k,l,n,ii,nm,jp1
      double precision a(nm,n),d(n),e(n),e2(n)
      double precision f,g,h,scale
c
c     this subroutine is a translation of the algol procedure tred1,
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine reduces a real symmetric matrix
c     to a symmetric tridiagonal matrix using
c     orthogonal similarity transformations.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        a contains the real symmetric input matrix.  only the
c          lower triangle of the matrix need be supplied.
c
c     on output
c
c        a contains information about the orthogonal trans-
c          formations used in the reduction in its strict lower
c          triangle.  the full upper triangle of a is unaltered.
c
c        d contains the diagonal elements of the tridiagonal matrix.
c
c        e contains the subdiagonal elements of the tridiagonal
c          matrix in its last n-1 positions.  e(1) is set to zero.
c
c        e2 contains the squares of the corresponding elements of e.
c          e2 may coincide with e if the squares are not needed.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      do 100 i = 1, n
         d(i) = a(n,i)
         a(n,i) = a(i,i)
  100 continue
c     .......... for i=n step -1 until 1 do -- ..........
      do 300 ii = 1, n
         i = n + 1 - ii
         l = i - 1
         h = 0.0d0
         scale = 0.0d0
         if (l .lt. 1) go to 130
c     .......... scale row (algol tol then not needed) ..........
         do 120 k = 1, l
  120    scale = scale + dabs(d(k))
c
         if (scale .ne. 0.0d0) go to 140
c
         do 125 j = 1, l
            d(j) = a(l,j)
            a(l,j) = a(i,j)
            a(i,j) = 0.0d0
  125    continue
c
  130    e(i) = 0.0d0
         e2(i) = 0.0d0
         go to 300
c
  140    do 150 k = 1, l
            d(k) = d(k) / scale
            h = h + d(k) * d(k)
  150    continue
c
         e2(i) = scale * scale * h
         f = d(l)
         g = -dsign(dsqrt(h),f)
         e(i) = scale * g
         h = h - f * g
         d(l) = f - g
         if (l .eq. 1) go to 285
c     .......... form a*u ..........
         do 170 j = 1, l
  170    e(j) = 0.0d0
c
         do 240 j = 1, l
            f = d(j)
            g = e(j) + a(j,j) * f
            jp1 = j + 1
            if (l .lt. jp1) go to 220
c
            do 200 k = jp1, l
               g = g + a(k,j) * d(k)
               e(k) = e(k) + a(k,j) * f
  200       continue
c
  220       e(j) = g
  240    continue
c     .......... form p ..........
         f = 0.0d0
c
         do 245 j = 1, l
            e(j) = e(j) / h
            f = f + e(j) * d(j)
  245    continue
c
         h = f / (h + h)
c     .......... form q ..........
         do 250 j = 1, l
  250    e(j) = e(j) - h * d(j)
c     .......... form reduced a ..........
         do 280 j = 1, l
            f = d(j)
            g = e(j)
c
            do 260 k = j, l
  260       a(k,j) = a(k,j) - f * e(k) - g * d(k)
c
  280    continue
c
  285    do 290 j = 1, l
            f = d(j)
            d(j) = a(l,j)
            a(l,j) = a(i,j)
            a(i,j) = f * scale
  290    continue
c
  300 continue
c
      return
      end
      double precision function epslon (x)
      double precision x
c
c     estimate unit roundoff in quantities of size x.
c
      double precision a,b,c,eps
c
c     this program should function properly on all systems
c     satisfying the following two assumptions,
c        1.  the base used in representing floating point
c            numbers is not a power of three.
c        2.  the quantity  a  in statement 10 is represented to
c            the accuracy used in floating point variables
c            that are stored in memory.
c     the statement number 10 and the go to 10 are intended to
c     force optimizing compilers to generate code satisfying
c     assumption 2.
c     under these assumptions, it should be true that,
c            a  is not exactly equal to four-thirds,
c            b  has a zero for its last bit or digit,
c            c  is not exactly equal to one,
c            eps  measures the separation of 1.0 from
c                 the next larger floating point number.
c     the developers of eispack would appreciate being informed
c     about any systems where these assumptions do not hold.
c
c     this version dated 4/6/83.
c
      a = 4.0d0/3.0d0
   10 b = a - 1.0d0
      c = b + b + b
      eps = dabs(c-1.0d0)
      if (eps .eq. 0.0d0) go to 10
      epslon = eps*dabs(x)
      return
      end
      double precision function pythag(a,b)
      double precision a,b
c
c     finds dsqrt(a**2+b**2) without overflow or destructive underflow
c
      double precision p,r,s,t,u
      p = dmax1(dabs(a),dabs(b))
      if (p .eq. 0.0d0) go to 20
      r = (dmin1(dabs(a),dabs(b))/p)**2
   10 continue
         t = 4.0d0 + r
         if (t .eq. 4.0d0) go to 20
         s = r/t
         u = 1.0d0 + 2.0d0*s
         p = u*p
         r = (s/u)**2 * r
      go to 10
   20 pythag = p
      return
      end
      subroutine imtqlv(n,d,e,e2,w,ind,ierr,rv1)
c
      integer i,j,k,l,m,n,ii,mml,tag,ierr
      double precision d(n),e(n),e2(n),w(n),rv1(n)
      double precision b,c,f,g,p,r,s,tst1,tst2,pythag
      integer ind(n)
c
c     this subroutine is a variant of  imtql1  which is a translation of
c     algol procedure imtql1, num. math. 12, 377-383(1968) by martin and
c     wilkinson, as modified in num. math. 15, 450(1970) by dubrulle.
c     handbook for auto. comp., vol.ii-linear algebra, 241-248(1971).
c
c     this subroutine finds the eigenvalues of a symmetric tridiagonal
c     matrix by the implicit ql method and associates with them
c     their corresponding submatrix indices.
c
c     on input
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c
c        e2 contains the squares of the corresponding elements of e.
c          e2(1) is arbitrary.
c
c     on output
c
c        d and e are unaltered.
c
c        elements of e2, corresponding to elements of e regarded
c          as negligible, have been replaced by zero causing the
c          matrix to split into a direct sum of submatrices.
c          e2(1) is also set to zero.
c
c        w contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct and
c          ordered for indices 1,2,...ierr-1, but may not be
c          the smallest eigenvalues.
c
c        ind contains the submatrix indices associated with the
c          corresponding eigenvalues in w -- 1 for eigenvalues
c          belonging to the first submatrix from the top,
c          2 for those belonging to the second submatrix, etc..
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
c
c        rv1 is a temporary storage array.
c
c     calls pythag for  dsqrt(a*a + b*b) .
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      k = 0
      tag = 0
c
      do 100 i = 1, n
         w(i) = d(i)
         if (i .ne. 1) rv1(i-1) = e(i)
  100 continue
c
      e2(1) = 0.0d0
      rv1(n) = 0.0d0
c
      do 290 l = 1, n
         j = 0
c     .......... look for small sub-diagonal element ..........
  105    do 110 m = l, n
            if (m .eq. n) go to 120
            tst1 = dabs(w(m)) + dabs(w(m+1))
            tst2 = tst1 + dabs(rv1(m))
            if (tst2 .eq. tst1) go to 120
c     .......... guard against underflowed element of e2 ..........
            if (e2(m+1) .eq. 0.0d0) go to 125
  110    continue
c
  120    if (m .le. k) go to 130
         if (m .ne. n) e2(m+1) = 0.0d0
  125    k = m
         tag = tag + 1
  130    p = w(l)
         if (m .eq. l) go to 215
         if (j .eq. 30) go to 1000
         j = j + 1
c     .......... form shift ..........
         g = (w(l+1) - p) / (2.0d0 * rv1(l))
         r = pythag(g,1.0d0)
         g = w(m) - p + rv1(l) / (g + dsign(r,g))
         s = 1.0d0
         c = 1.0d0
         p = 0.0d0
         mml = m - l
c     .......... for i=m-1 step -1 until l do -- ..........
         do 200 ii = 1, mml
            i = m - ii
            f = s * rv1(i)
            b = c * rv1(i)
            r = pythag(f,g)
            rv1(i+1) = r
            if (r .eq. 0.0d0) go to 210
            s = f / r
            c = g / r
            g = w(i+1) - p
            r = (w(i) - g) * s + 2.0d0 * c * b
            p = s * r
            w(i+1) = g + p
            g = c * r - b
  200    continue
c
         w(l) = w(l) - p
         rv1(l) = g
         rv1(m) = 0.0d0
         go to 105
c     .......... recover from underflow ..........
  210    w(i+1) = w(i+1) - p
         rv1(m) = 0.0d0
         go to 105
c     .......... order eigenvalues ..........
  215    if (l .eq. 1) go to 250
c     .......... for i=l step -1 until 2 do -- ..........
         do 230 ii = 2, l
            i = l + 2 - ii
            if (p .ge. w(i-1)) go to 270
            w(i) = w(i-1)
            ind(i) = ind(i-1)
  230    continue
c
  250    i = 1
  270    w(i) = p
         ind(i) = tag
  290 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end

      function daxb(A,B,na,nb,nd,flag)

*     solves linear equations AX=B.  The solution X overwrites B.

      implicit     none
      integer*4    daxb,            !  0 for success, -1 for singular
     -             na,nb,           !  number of columns of A,B
     -             nd,              !  declared row dimension of A,B
     -             flag             !  on input, set to:
                                    !   0 for general A,B
                                    !   1 for X known to be symmetric
                                    !   2 to reuse LU from previous call
                                    !     (X treated as nonsymmetric)
      real*8       A(nd,*),B(nd,*)

*     calls
      character    strI4*16

      character    line*128
      integer*4    MAXSIZE,i,j
      parameter    (MAXSIZE = 600)
      integer*4    indx(MAXSIZE),code
      real*8       wk(MAXSIZE),d
      common       /daxbsave/indx

      daxb = 0
      IF (na.gt.nd .or. nb.gt.nd .or. (flag.eq.1 .and. na.ne.nb)) THEN
         call warning ('daxb: bad input')
         daxb = -1
      ELSE IF (nd.gt.MAXSIZE) THEN
         line = 'daxb: nd = '//strI4(nd)//' is .gt. '//strI4(MAXSIZE)
         call warning (line)
         daxb = -1
      END IF
      if (daxb.eq.-1) return
      code = 0
      if (flag.ne.2) call ludcmp(A,na,nd,indx,d,wk,code)
      IF (code.eq.-1) THEN
         daxb = -1
      ELSE IF (flag.eq.0 .or. flag.eq.2) THEN
         DO j=1,nb
            call lubksb(A,na,nd,indx,B(1,j))   !  back substitution for
         END DO                                !  general X
         daxb = 0
      ELSE IF (flag.eq.1) THEN
         DO j=1,nb
            call lusym(A,na,nd,indx,B(1,j),j)  !  for symmetric X, get
            DO i=1,j-1                         !  the lower triangle and
               B(i,j) = B(j,i)                 !  fill in rest by symmetry
            END DO
         END DO
         daxb = 0
      ELSE
         call warning('daxb: bad flag')
         daxb = -1
      END IF
      end


      subroutine ludcmp(a,n,np,indx,d,wk,code)

c     LU decomposition of a matrix, from Numerical Recipes.
c     rendered double precision by APH 12-28-89

      implicit     none
      integer*4    i,j,k,n,np,imax,indx,code
      real*8       a,wk,aamax,sum,dum,d,TINY
      parameter    (TINY=1.0e-20)
      dimension    a(np,np),indx(n),wk(np)

      code = 0
      D=1.D0
      DO 12 I=1,N
        AAMAX=0.D0
        DO 11 J=1,N
          IF (DABS(A(I,J)).GT.AAMAX) AAMAX=DABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.D0) THEN
           code = -1                   !  modified by APH 12-22-90
           d = 0.                      !  returns error code for
           return                      !  singular matrix
        END IF
        wk(I)=1.D0/AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.D0
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=wk(I)*DABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          wk(IMAX)=wk(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.D0)A(J,J)=TINY
          DUM=1.D0/A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.D0)A(N,N)=TINY
      RETURN
      END


      subroutine lubksb(a,n,np,indx,b)

      implicit     none
      integer*4    i,j,n,np,ii,ll,indx(n)
      real*8       a(np,np),b(n),sum

      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END

      subroutine lusym(a,n,np,indx,b,row)

c     back substitution from Numerical Recipes
c     modified to calculate b(i) only for i.ge.row, for finding a
c     solution to Ax=b known to be symmetric.....APH 6-19-90

      implicit     none
      integer*4    i,j,n,np,ii,ll,indx(n),row
      real*8       a(np,np),b(n),sum

      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,row,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END
