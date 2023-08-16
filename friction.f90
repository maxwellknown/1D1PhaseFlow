subroutine friction
    use dat 
    use seuif97
    implicit none

    integer i
    real * 8 nu 

    do i = 1,120
        nu = ph((p(i)+p0)/1.0d6,h(i)/1.0d3,25) ! 运动粘度
        Re(i) = D(i) * u(i) / nu 
        if (Re(i) .lt. 64.0d0) f(i) = 1.0d0
        ! if ((Re(i) .lt. 2320.0d0) .and. Re(i) .ge. 64.0d0)  f(i) = 64.0/Re(i)        ! f>=0.0275862
        ! if ((Re(i) .ge. 2320.0d0) .and. (Re(i) .le. 1.0d5)) f(i) = 0.184d0/Re(i)**0.2d0 ! f<0.0390589    0.0184
        ! if (Re(i) .gt. 1.0d5) f(i) = 0.11d0 * (1.0d-5 + 68.0d0/Re(i))**0.25d0 ! f < 0.017828
        if ((Re(i) .lt. 1502.0d0) .and. Re(i) .ge. 64.0d0)  f(i) = 64.0/Re(i)        ! f>=0.0275862
        if ((Re(i) .ge. 1502.0d0) .and. (Re(i) .le. 5.0d5)) f(i) = 0.184d0/Re(i)**0.2d0 ! f<0.0390589    0.0184
        if (Re(i) .gt. 5.0d5) f(i) = 0.11d0 * (1.0d-5 + 68.0d0/Re(i))**0.25d0 ! f < 0.017828
    enddo
    return 
end subroutine friction