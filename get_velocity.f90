subroutine get_velocity

    use dat 
    implicit none

    integer i,i2,i1 
    do i = 1,120
        i1 = i - 1
        i2 = i + 1
        if (i .eq. 1) i1 = 120
        if (i .eq. 120) i2 = 1
        Ast(i)= -(ro_old(i2) * u_old(i2) * abs(u_old(i2))&
        -ro_old(i1) * u_old(i1) * abs(u_old(i1)))/dx(i)*0.5d0& 
        +ro_old(i) * g * theta(i) &
        -f(i) * dx(i) / D(i) * ro_old(i) * u_old(i) * abs(u_old(i))/2.0d0 
        u(i) = (ro_old(i) * u_old(i) + Ast(i) * dt &
          -dt / dx(i) * (p(i2)-p(i)))/ro(i)
        
        if(( u(i).gt.- 1.d-88 ) .and. ( u(i).lt.1.d-88 ))u(i)=0.0d0
    enddo
    call friction
    ! write(*,*)'Ast(i),u(i)',Ast(1),u(1)
    return
    
end subroutine get_velocity