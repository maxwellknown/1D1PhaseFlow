real * 8 function callerr() result(er)
    use dat 
    implicit none

    integer  i,i1 
    ! real*8 er
    do i = 1,120 
        i1 = i - 1
        if (i .eq.1) i1 = 120
        er = max(er,abs(ro(i) * u(i) - ro(i1) * u(i1))/dx(i))
    enddo
    ! callerr = er
end function callerr