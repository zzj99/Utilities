program test
   implicit none
   character(len=40) :: infile      ! length of file name should be <= 40
   character(len=1050) :: results   ! assume the results text is no longer than 15 lines
   real(8) :: eHF
   integer :: idHF

   infile="c2min-energy.log"   ! log file for test

   call g09results(infile, results)

   ! Now we have lines contains results, separated with '\'
   idHF=index(results,'HF=')
   if (idHF > 0) then
      read(results(idHF+3:idHF+14), *) eHF   ! length of number is fixed: 12
   else
      write(*,*) "can't find 'HF=' in results text"
      stop
   end if

   write(*,*) eHF

   stop
end program test

!-------------------------------------------------------------
! grab results from g09 log file
!
subroutine g09results(g09log, results)
   implicit none
   character(len=40) :: g09log      ! length of file name should be <= 40
   character(len=80) :: buffer
   integer :: ierr
   character(len=1050) :: results   ! assume the results text is no longer than 15 lines
   logical :: is_res         ! is current line in results text?

   open(1, file=g09log, status='old', iostat=ierr)  ! open log file
   if (ierr /= 0) then
      write(*,*) 'Error in g09results: can NOT find '//trim(g09log)
      stop
   end if

   ! detect results 
   results=''
   is_res = .false.
   do 
      read(1, '(a)', iostat=ierr) buffer   ! read line
      if (ierr /= 0) exit                 ! end of file or error

      if (index(buffer,'1\1\') /= 0)then  ! begin of results
         is_res = .true.
      end if

      if (is_res)then
         results=trim(results)//buffer(2:71)  ! text range: column 2 to 71
         if (buffer .eq. '') then         ! end of results
            is_res = .false.
         end if
      end if      
   end do
   close(1)   ! close log file

   return
end subroutine g09results
