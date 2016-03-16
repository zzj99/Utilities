module distribution
contains
   !-- count the distribution of an given array, with given number of bins
   subroutine count_dist(x, nbin, dups, ndups, dupsize)
      implicit none
      real(8) :: x(:)
      integer :: nbin
      real(8) :: dups(size(x))
      integer :: ndups(size(x))
      integer :: dupsize

      real(8) :: rx(size(x))
      real(8) :: binwidth
      integer :: i, j, k
      real(8) :: xmin, xmax

      ! get min and max value
      call getminmax(x, xmin, xmax)

      ! calculate bin width from min and max value
      binwidth = (xmax - xmin)/dble(nbin)

      ! round the values
      do i = 1, size(x)
         call roundbin(x(i), binwidth, rx(i))
      end do

      ! count duplicates
      call count_dups(rx, dups, ndups, dupsize)

      return
   end subroutine count_dist

   !-- round x with given bin width
   subroutine roundbin(x, binwidth, rx)
      implicit none
      real(8) :: x, binwidth
      real(8) :: rx

      rx = binwidth * (floor(x/binwidth) + 0.5d0)

      return
   end subroutine roundbin

   !-- get min and max values of an array
   subroutine getminmax(x, xmin, xmax)
      implicit none
      real(8) :: x(:)
      real(8) :: xmin, xmax
      integer :: i

      xmin = 1.0d30    ! a very large number
      xmax = -1.0d30   ! a very small number

      do i = 1, size(x)
         if (xmin > x(i)) xmin = x(i)
         if (xmax < x(i)) xmax = x(i)
      end do

      return
   end subroutine getminmax

   !-- count duplicate elements of 
   subroutine count_dups(x, dups, ndups, k)
      implicit none
      real(8) :: x(:)
      real(8) :: dups(size(x))
      integer :: ndups(size(x)), k

      integer :: i, j

      dups = 0d0
      ndups = 0

      k = 1
      dups(1) = x(1)
      outer: do i=2,size(x)
         do j=1,k
            if (dups(j) == x(i)) then
               ! Found a match, duplicate numbers +1
               ndups(j) = ndups(j) + 1
               cycle outer ! start new matching
            end if
         end do
         ! No match 
         k = k + 1
         dups(k) = x(i)
      end do outer
      ndups(1:k)=ndups(1:k)+1

      return
   end subroutine count_dups

end module distribution
!
program main
   use distribution
   implicit none

   integer,parameter :: n = 100
   integer,parameter :: nbin = 10
   real(8) :: example(n), dups(n)
   integer :: ndups(n), dupsize
   integer :: i

   open(1, file="example.dat")
   open(2, file="dist.dat")
   call random_seed()
   do i = 1, n
      call random_number(example(i))
   end do
   call count_dist(example, nbin, dups, ndups, dupsize)

   write(*,*) '--- example ---'
   do i = 1, n
      write(1,'(E20.13)') example(i)
   end do
   write(*,*) '--- dist ---'
   do i = 1, dupsize
      write(2,'(E20.13, 2x, i3)') dups(i), ndups(i)
   end do

   close(1)
   close(2)

   stop
end program main
