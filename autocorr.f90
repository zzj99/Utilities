program main
   implicit none
   integer,parameter :: n = 100
   integer,parameter :: ncor = n-1 !n/2
   integer :: i
   real(8) :: A(n), c1(0:ncor), c2(0:ncor)

   call random_seed()

   do i = 1, n
      call random_number(A(i))
   end do

   call acf(c1, A, n, ncor)
   write(*,*) 'acf by FFT'
   write(*,*)  c1 

   call autocorr(c2, A, n, ncor)
   write(*,*) 'acf by direct'
   write(*,*)  c2 

   write(*,*) 'c2/c1'
   write(*,*) c2/c1

end program main

module fft
contains
   !______________________________________________________________________
   !
   ! Use subroutines in FFTW3 library to do fft calculation
   !
   subroutine fft_r2c(cfout, rfin, n)
      implicit none
      integer :: n
      real(8) :: rfin(n)
      complex(8) :: cfout(n/2+1)

      ! variables for calling fftw
      integer(8) :: plan_forward

      !
      ! Make sure you have this file in local directory, or include directory
      !
      include "fftw3.f"

      !
      !  Set up a plan, and execute the plan to transform the IN data to
      !  the OUT FFT coefficients.
      !
      call dfftw_plan_dft_r2c_1d_ ( plan_forward, n, rfin, cfout, FFTW_ESTIMATE )

      call dfftw_execute_ ( plan_forward )

      !
      !  Discard the information associated with the plans.
      !
      call dfftw_destroy_plan_ ( plan_forward )

   end subroutine fft_r2c

   !______________________________________________________________________
   !
   ! Use subroutines in FFTW3 library to do fft calculation
   !
   subroutine fft_c2r(rfout, cfin, n)
      implicit none
      integer :: n
      complex(8) :: cfin(n/2+1)
      real(8) :: rfout(n)

      ! variables for calling fftw
      integer(8) :: plan_backward

      !
      ! Make sure you have this file in local directory, or include directory
      !
      include "fftw3.f"

      !
      !  Set up a plan, and execute the plan to backtransform the
      !  complex FFT coefficients in OUT to real data.
      !
      call dfftw_plan_dft_c2r_1d_ ( plan_backward, n, cfin, rfout, FFTW_ESTIMATE )

      call dfftw_execute_ ( plan_backward )

      !
      !  Discard the information associated with the plans.
      !
      call dfftw_destroy_plan_ ( plan_backward )

   end subroutine fft_c2r

end module fft

!! calculate autocorrelation function with FFT
!!
subroutine acf(C, A, n, ncor)
   use fft
   implicit none
   integer :: n, ncor
   real(8) :: A(n), C(0:ncor)
   integer(8) :: plan_forward, plan_backward
   integer :: i, nfft
   real(8), allocatable, dimension(:) :: rfin, rfout
   complex(8),allocatable,dimension(:) :: cfin, cfout

   include "fftw3.f"

   !! size for FFT
   nfft = 2**(ceiling(log(2.0*n-1.0)/log(2.0)))

   allocate(rfin(nfft))
   allocate(rfout(nfft))
   allocate(cfin(nfft/2+1))
   allocate(cfout(nfft/2+1))

   !! zero padding
   rfin = 0.0d0
   rfin(1:n) = A

   !! FFT   
   call fft_r2c(cfout, rfin, nfft)

   !! modulas
   cfin = cfout*conjg(cfout)/dble(nfft)

   !! Inverse FFT
   call fft_c2r(rfout, cfin, nfft)

   !! normalization
   do i = 0, ncor
      c(i) = rfout(i+1) / dble(n-i) 
   end do

   deallocate(rfin)
   deallocate(rfout)
   deallocate(cfin)
   deallocate(cfout)

end subroutine


!----------------------------------------------------------------------------------
!> calculate auto correlation function of observable A
!!
!! @see [AT90] Allen and Tildesley, "Computer Simulation of Liquids", P.186
!!
!! @param [in]  A(1:iTtotal)  = values of observable A
!! @param [in]  iTtotal       = size of array A
!! @param [in]  iTcorr        = size-1 of correlation function
!! @param [out] acf(0:iTcorr) = normalized auto-correlation function
subroutine autocorr(acf, A, iTtotal, iTcorr)
   implicit none
   integer, intent(in)  ::  iTtotal       ! total number of A
   integer, intent(in)  ::  iTcorr        ! correlation length
   real(8), intent(in)  ::  A(1:iTtotal)  ! array of values of A
   real(8), intent(out) ::  acf(0:iTcorr) ! normalized auto-correlation function

   real(8) ::  nACF(0:iTcorr)             ! number of ACF, for normalization of ACF
   integer :: iT, iT0, iT0max, iTau       ! time index of t0+tau, t0, t0max and tau

   ! initialize all the auto-correlation functions to be calculated
   acf(0:iTcorr)  = 0d0
   nACF(0:iTcorr) = 0d0

   ! for each t0 from 1 to total, calculate auto-correlation function
   do iT0 = 1, iTtotal

      ! the max index of T0 must not exceed the last index
      iT0max = min(iTtotal, iT0+iTcorr)

      ! for all possible t0+tau, calculate a(t0)*a(t0+tau) and add them
      do iT = iT0, iT0max

         ! interval between t and t0
         iTau  = iT - iT0

         ! calculate auto-correlation function for current interval
         acf(iTau) = acf(iTau) + A(iT0) * A(iT)

         ! count the number of auto-correlation function
         nACF(iTau) = nACF(iTau) + 1d0
      end do

   end do

   ! normalize auto-correlation function
   do iT = 0, iTcorr
      acf(iT) = acf(iT) / nACF(iT)
   end do

   return
end subroutine autocorr
