subroutine get_pressure

    use dat 
    implicit none

    integer i,i2,i1,num
    real *8 err
    err = 1.0d0
    num = 0
    do while(err .gt. p_err .and. num .lt. max_iter_num)
        err = 0.0d0 
        do i = 1,120
            i1 = i - 1 
            i2 = i + 1
            if (i.eq.1) i1 = 120
            if (i.eq.120) i2 = 1
            p(i) = (pd(i) - b(i1) * p(i1) - b(i2) * p(i2))/a(i)
            err = max(err,abs(p(i) - p_new(i))/(abs(p(i)) + 1.0d-10))
        enddo
        num = num + 1 
        p_new = p
    enddo
    ! write(*,*)'Pressure iter.no = ',num
    return 
    
end subroutine get_pressure