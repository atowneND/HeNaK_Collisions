
      subroutine stdio (in,out)

*----------------------------------------------------------------------*
*     This routine is for use with the gnu f95 fortran compiler
*     May 3, 2010
*----------------------------------------------------------------------*

c     Set up input and output files, using command line arguments.
c     The input is read from the file specified as the first argument,
c     if that file exists, otherwise from the default, STDIN.  If a
c     second argument is given, the output goes to that file,
c     otherwise to the default, STDOUT.
c
c     usage:       integer*4   in,out
c                  call stdio (in,out)

      include      'standard.aph'
      integer*4    in,out,argc,fassign,i
      character    argv(MAXARG)*(MAXCHAR)

      argc = iargc()
      DO i = 1,argc
         call getarg(i,argv(i))
      END DO

      IF (argc.eq.0) THEN
         in = STDIN
         out = STDOUT
      ELSE IF (argc.eq.1) THEN
         if (fassign(in,argv(1),READq).lt.0) in = STDIN
         out = STDOUT
      ELSE IF (argc.ge.2) THEN
         if (fassign(in,argv(1),READq).lt.0) in = STDIN
         if (fassign(out,argv(2),WRITEq).lt.0) out = STDOUT
      END IF

      call fileset (out)

      end
