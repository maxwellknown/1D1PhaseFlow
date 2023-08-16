subroutine onestep_incompressible

    use dat
    implicit none
    integer i,i1 
    a = dt/dx/dx * 2.0d0 
    b = -dt/dx/dx 
    do i = 1,120 
        i1 = i - 1
        if (i .eq. 1) i1 = 120
        pd(i) = -(ro(i) * u(i) + Ast(i) * dt - ro(i1) * u(i1) - Ast(i1) * dt) / dx(i)
    enddo
    ! write(*,*)'pd(1),ro(i),u(i),Ast(i),ro(i1),u(i1),Ast(i1)'&
    ! ,pd(1),ro(1),u(1),Ast(1),ro(120),u(120),Ast(120)
    call get_pressure
    call get_velocity
    return 
    
end subroutine onestep_incompressible