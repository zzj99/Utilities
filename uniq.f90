program main
   integer,parameter :: n=20000
   real(8) :: a(n)
   integer :: b(n)
   integer :: c(n)

   integer :: nuniq
   real(8) :: uniq(n)
   integer :: i
   real(8) :: start, finish

   a = 1d0
   do i = 1,12
      a(10*(i-1)+1) = 10d0
      a(10*(i-1)+2) = 20d0
      a(10*(i-1)+3) = 30d0
      a(10*(i-1)+5) = 50d0
      a(10*(i-1)+6) = 60d0
      a(10*(i-1)+7) = 70d0
      a(10*(i-1)+8) = 80d0
   end do

   ! 
   call cpu_time(start)
   do i = 1, n
      call count_val(a, a(i), b(i))
   end do
   call cpu_time(finish)
   write(*,*) 'count val', finish-start

   call cpu_time(start)
   call count_dups(a, c)
   call cpu_time(finish)
   write(*,*) 'count dups', finish-start

   do i = 1, n
      if (b(i) - c(i) .ne. 0) write(*,*) i
   end do

   call uniq_val(a, uniq, nuniq)
   write(*,*) nuniq, uniq(1:nuniq)
contains
   !
   ! find the unique values and the number of them of an array
   !
   subroutine uniq_val(vals, uniq, nuniq)
      implicit none
      real(8) :: vals(:)
      real(8) :: uniq(size(vals))
      integer :: nuniq

      logical :: checked(size(vals))
      integer :: p, ipos, i, cnt

      checked = .false.
      nuniq = 0
      outer: do i = 1, n
         cnt = 1
         p = i         
         if (.not. checked(i)) then
            do 
               call index_val(vals(p+1:), vals(i), ipos)
               if (ipos == 0) then
                  nuniq = nuniq + 1
                  uniq(i) = vals(i)
                  cycle outer
               end if
               p = p + ipos
               checked(p) = .true.
            end do
         end if         
      end do outer

   end subroutine uniq_val

   !
   ! count occurrence of duplicate values in an array
   !
   subroutine count_dups(vals, cnts)
      implicit none
      real(8) :: vals(:)
      integer :: cnts(size(vals))

      logical :: checked(size(vals))
      integer :: ieqto(size(vals))
      integer :: p, ipos, i

      checked = .false.
      outer: do i = 1, n
         cnts(i) = 1
         p=i
         if (.not. checked(i)) then
            do
               call index_val(vals(p+1:), vals(i), ipos)
               if (ipos == 0) cycle outer
               cnts(i) = cnts(i) + 1
               p = p + ipos
               checked(p) = .true.
               ieqto(p) = i
            end do
         else
            cnts(i) = cnts(ieqto(i))
         end if
      end do outer

   end subroutine count_dups

   !
   ! count occurrence of given value in an array
   !
   subroutine count_val(vals, val, cnt)
      implicit none
      real(8) :: vals(:)
      real(8) :: val
      integer :: cnt

      integer :: p, ipos
      cnt = 0
      p = 1
      do
         call index_val(vals(p:), val, ipos)
         if (ipos == 0) return
         cnt = cnt + 1
         p = p + ipos
      end do

      return
   end subroutine count_val

   !
   ! find the index of given value in an array
   !
   subroutine index_val(vals, val, ipos)
      implicit none
      real(8) :: vals(:)
      real(8) :: val
      integer :: ipos

      integer :: i
      ipos = 0
      do i = 1, size(vals)
         if (vals(i) .eq. val) then
            ipos = i
            return
         end if
      end do
      return
   end subroutine index_val

end program main
