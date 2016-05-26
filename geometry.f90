!=========================================================================
! calculate length 1-2, same unit as x1,x2
!-------------------------------------------------------------------------
! [in] 
!       x1,x2 : cartesian coordinates for points 1,2, respectively
! [out]
!          r  : the length 1-2, same unit as x1 and x2
!
subroutine bond_length(r, x1, x2)
   implicit none
   real(8) :: x1(3), x2(3)
   real(8) :: r
   real(8) :: vec(3)
   vec = x2 - x1
   r = norm2(vec)
end subroutine

!==========================================================================
! calculate angle 1-2-3, in radian
!--------------------------------------------------------------------------
! [in] 
!       x1,x2,x3 : cartesian coordinates for points 1,2,3, respectively
! [out]
!            a   : the angle 1-2-3, in radian
!
subroutine bond_angle(a, x1, x2, x3)
   implicit none
   real(8) :: x1(3), x2(3), x3(3)
   real(8) :: a

   real(8) :: cosa, v21(3), v23(3), r21, r23
   v21 = x1 - x2
   v23 = x3 - x2
   r21 = norm2(v21)
   r23 = norm2(v23)
   if ((r21 <= 0d0) .or. (r23 <= 0d0)) then
      print *, 'bondangle: Error bond length is 0'
      stop
   end if
   cosa = dot_product(v21, v23)/(r21*r23)
   if (cosa < -1d0) then
      cosa = -1d0
   end if
   if (cosa > 1d0) then
      cosa = 1d0
   end if
   a = acos(cosa)
end subroutine
!============================================================================
! calculate dihedral angle 1-2-3-4, in radian
!----------------------------------------------------------------------------
! [in] 
!     x1,x2,x3,x4 : cartesian coordinates for points 1,2,3,4, respectively
!        flag     : specified range for dihedral angle
!                   * -1  : -pi to pi (default)
!                   *  0  :   0 to pi
!                   *  1  :   0 to 2pi
! [out]
!         tau     : the dihedral angle 1-2-3-4
!
! Example code for test (output result should be -119.2 degrees, same as pymol)
!   x1 = [  1.0d0,  2.0d0,  4.0d0 ]
!   x2 = [ -1.0d0,  2.0d0,  3.0d0 ]
!   x3 = [  1.0d0, -2.0d0,  3.0d0 ]
!   x4 = [ -1.0d0,  2.0d0, -3.0d0 ]
!   call dihedral_angle(tau, x1, x2, x3, x4, -1)
!   print *, tau*180.0d0/acos(-1d0)  
!
subroutine dihedral_angle(tau, x1, x2, x3, x4, flag)
   implicit none
   real(8) :: x1(3), x2(3), x3(3), x4(3)
   real(8) :: tau
   integer,optional :: flag
   real(8),parameter :: pi = 3.14159265358979323846d0

   real(8) :: v21(3), v23(3), v34(3), n123(3), n234(3), n1234(3)
   real(8) :: cost, sint, signt

   v21 = x1 - x2
   v23 = x3 - x2
   v34 = x4 - x3

   call cross(n123, v21, v23)      ! v21 x v23
   call cross(n234, -v23, v34)     ! v32 x v34
   cost = dot_product(n123, n234)  ! omit /(r123*r234), reduced for tant

   call cross(n1234, n123, n234)
   signT = dot_product(n1234, v23) ! direction - same (+) or opposite (-)
   sint = sign(norm2(n1234),signT) ! omit /(r123*r234), reduced for tant

   tau = atan2(sint, cost)     ! -pi to pi
   ! specified range might be  
   if (present(flag)) then
      select case (flag)
      case (0)                 ! 0 to pi
         if (tau < 0) then
            tau = -tau
         end if
      case (1)                 ! 0 to 2pi
         if (tau < 0) then
            tau = 2d0*pi + tau
         end if
      case default ! flag = -1 or others
      end select
   end if
contains
   subroutine cross(c, a, b)
      implicit none
      real(8) :: a(3), b(3), c(3)
      c(1) = a(2)*b(3) - a(3)*b(2)
      c(2) = a(3)*b(1) - a(1)*b(3)
      c(3) = a(1)*b(2) - a(2)*b(1)
   end subroutine
end subroutine
