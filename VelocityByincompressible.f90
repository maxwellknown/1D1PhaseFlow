subroutine VelocityByincompressible

    use dat
    implicit none

    real * 8 err
    real * 8 :: callerr
    integer num 
    num = 0
    err = 1.0 
    dt = dt_min 
    do while(err .ge. u_err .and. num .lt. max_iter_num)
        call onestep_incompressible
        u_old = u 
        p_old = p 
        err = callerr()
        num = num + 1
    enddo
    max_err = err
    return

end subroutine VelocityByincompressible