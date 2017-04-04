! get histogram of a given array
!
!     h1     h2     h3     h4
!  +------+------+------+------+
! min  min+w  min+2w min+3w   max=(min+4w)
!
! hist(:,1) the hist values
! hist(:,2) number of hists
subroutine histogram(hist, x, n, nbin)
   implicit none
   integer :: n, nbin
   real(8) :: x(n)
   real(8) :: hist(nbin,2)

   integer :: i, j
   real(8) :: width, xmin, xmax

   ! 1. find the max and min value of the array (don't need to sort whole array)
   xmin = minval(x)
   xmax = maxval(x)

   ! 2. calculate bin width
   width=(xmax-xmin)/dble(nbin)

   ! 3. set hist value of each bin
   do j = 1, nbin
      hist(j,1) = xmin + width*(j-0.5d0)
   end do

   ! 4. count hists
   do i = 1, n
       j = min(int((x(i) - xmin)/width)+1,nbin)
      hist(j,2) = hist(j,2) + 1d0
   end do

   ! 5. normalize density
   hist(:,2) = hist(:,2) / (n * width)
end subroutine

program main
   implicit none
   character(len=80) :: f_in, f_out
   integer :: stat, i
   integer,parameter :: ndata = 100000
   integer,parameter :: nbin = 100
   real(8) :: x(ndata), hist(nbin,2)

   f_in = 'test-a.dat'
   f_out = 'test-f.dat'

   open(11, file=f_in)
   do i = 1, ndata
      read(11, *, iostat=stat) x(i)
   end do
   close(11)

   call histogram( hist, x, ndata, nbin)

   open(12, file=f_out)
   do i = 1, nbin
      write(12, '(E20.13,1x,A,1x,E20.13)') hist(i,1), ',', hist(i,2)
   end do
   close(12)

end program main
