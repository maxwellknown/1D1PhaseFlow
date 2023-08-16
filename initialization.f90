subroutine initialization

    use iso_c_binding
    use dat 
    use seuif97
    implicit none

    real * 8 h0
    integer i 
    time = 0.0d0
    p0 = P_init
    p = 0.0d0
    drdp = 4.5d-7
    T = T_f
    do i = 1,120
        h0 = pt((p(i)+p0)/1.0d6, T(i), 4) ! Need to change if T != const
        h(i) = h0 * 1.0d3 
        ro(i) = ph((p(i)+p0)/1.0d6,h0,2)
    enddo
    ! h0 = pt((p(i)+p0)/1.0d6, T(i), 4) ! Need to change if T != const
    ! h(i) = h0 * 1.0d3 
    ! ro = 1000.0d0
    r0 = ro(1)
    theta = 0.0d0
    u = 0.0d0
    D = 0.008d0
    Re = 1.0d0
    f = 1.0d0 
    dK = 0.0d0 
    dx = 0.1d0
    Area = pi * D * D /4.0d0
    theta(31:60) = -1.0d0
    theta(91:120) = 1.0d0 
    p_old = p 
    ro_old = ro 
    u_old = u 
    Ast = 0.0d0 
    QL = 0.0d0 
    TW = 25.0d0 
    call read_data
    TW(31:60) = T_eva
    TW(91:120) = T_cond
    ! TW(31:60) = TW_bc(1, 32:61)  
    ! TW(91:120) = TW_bc(1, 92:121)
    write(*,*)'Initialization finished!'
    return 
    
end subroutine initialization