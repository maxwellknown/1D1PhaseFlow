subroutine Energy
  use ieee_arithmetic, only: ieee_is_nan
  use iso_c_binding
  use dat 
  use seuif97
  implicit none

  real * 8 dhdt  
  integer i,i1, nflag, delta
  logical :: is_nan

  nflag = NINT(time)
  ! delta = TW_bc(2,1) - TW_bc(1,1) 
  do i=31,60
    TW(i) = custom_interpolation(TW_bc(1:i_file_len, 1), TW_bc(1:i_file_len, i+1), time) 
  enddo
  do i=91,120
    TW(i) = custom_interpolation(TW_bc(1:i_file_len, 1), TW_bc(1:i_file_len, i+1), time)
  enddo
  call h_coef
  ! write(*,*)'Time',time,TW(31),TW(60),TW(91),TW(120)
  ! write(*,*)'Time',time,TW_bc(nflag/delta+1, 32),TW_bc(nflag/delta+1,61),TW_bc(nflag/delta+1,92),TW_bc(nflag/delta+1,121)
  ! write(*,*)'Time',time,ht_coef(31),ht_coef(60),ht_coef(91),ht_coef(120)
  do i=1,120
      i1 = i-1
      if (i.eq.1) i1 = 120
      if ((i.lt.61 .and. i .gt.30) .or. (i .lt. 121 .and. i .gt. 90)) then
          ! QL(i) = (TW(i) - T(i)) * 2000.0d0 * pi * D(i)
          QL(i) = (TW(i) - T(i)) * ht_coef(i) * pi * D(i)
      endif
      dhdt = (h(i1) * ro(i1) * u(i1) * Area(i1) - h(i) * ro(i) * u(i) * Area(i)) + QL(i) * dx(i) 
      h(i) = h(i) + dhdt * dt  
      is_nan = ieee_is_nan(h(i))
      if (is_nan) then
        write(*,*)'Energy error!',i,ht_coef(i),dhdt,dt,h(i)
        stop
      endif
      ! T(i) = TEMPERATURE((p(i)+p0)/1.0d6, h(i)/1.0d3) - 273.15d0     ! TEMPERATURE
      ! ro(i) = DENSITY((p(i)+p0)/1.0d6, T(i) + 273.15d0)              ! DENSITY
      T(i) = ph((p(i)+p0)/1.0d6,h(i)/1.0d3,1)
      ro(i) = ph((p(i)+p0)/1.0d6,h(i)/1.0d3,2)
  enddo
  ro_old = ro
  return
contains    
  function custom_interpolation(x, y, x_target) result(y_target)
      implicit none
      
      real*8, intent(in) :: x(:), y(:), x_target
      real*8 :: y_target
  
      integer :: k , n
      n = size(x)
      ! Check if x_target is within the known time range
      if (x_target <= x(1)) then
        y_target = y(1)  ! Use the first temperature for x_target <= x(1)
      elseif (x_target >= x(n)) then
        y_target = y(n)  ! Use the last temperature for x_target >= x(n)
      else
        ! Find the nearest neighbor interval containing x_target
        k = 1
        do while (x(k) < x_target)
          k = k + 1
        end do
  
        ! Linear interpolation in between the two nearest neighbors
        if (k > 1 .and. k < n) then
          y_target = y(k - 1) + (x_target - x(k - 1)) * (y(k) - y(k - 1)) / (x(k) - x(k - 1))
        end if
      end if
    end function custom_interpolation
end subroutine Energy


