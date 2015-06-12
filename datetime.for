
      subroutine DateTime(datestr,timestr)

*     ------------------------------------------------------------------
*     This routine returns the date and time strings, using Fortran 90
*     intrinsic functions.
*     ------------------------------------------------------------------

      implicit none
      character datestr*(*),timestr*(*)

      character  ::  time*10,date*14,zone*10,day*2
      character*5, dimension(12) ::  months =
     -   (/'Jan. ','Feb. ','March','April','May  ','June ',
     -     'July ','Aug. ','Sept.','Oct. ','Nov. ','Dec. '/)
      integer, dimension(8) :: dt

      call date_and_time(date,time,zone,dt)

      IF (dt(3).le.9) THEN    ! drop leading zero from the day
         day = date(8:8)//' '
      ELSE
         day = date(7:8)
      END IF

      time = time(1:2)//':'//time(3:4)//':'//time(5:6)
      date = trim(months(dt(2)))//' '//trim(day)//', '//date(1:4)

      datestr=date
      timestr=time

      end subroutine DateTime
