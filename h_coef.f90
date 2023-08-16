subroutine h_coef
    ! use ieee_arithmetic, only: ieee_is_nan
    use dat 
    use seuif97
    implicit none

    integer i
    real * 8 nu, prantdlf, prantdlw, k, Gr, beta, tc
    ! logical :: is_nan

    do i = 1,120
        nu = pt((p(i)+p0)/1.0d6,T(i),25) ! 运动粘度
        prantdlf = pt((p(i)+p0)/1.0d6,T(i),28) ! 普朗特数
        prantdlw = pt((p(i)+p0)/1.0d6,TW(i),28) ! 普朗特数
        k = (prantdlf/prantdlw)**0.11d0
        Re(i) = max(D(i) * u(i) / nu, 64.0d0 )
        if (Re(i) .ge. 2300.0d0) then
            Nusselt(i) = (f(i)/8.0d0*(Re(i)-1000.0d0)*prantdlf)/(1.0d0 + 12.7d0*(prantdlf**0.666667-1.0d0)&
         *(f(i)/8.0d0)**0.5d0) * (1.0d0 + (D(i)/3.0d0)**0.666667)*k
            ! if (i == 31) then
            !     ! write(*,*)'Force',f(i),Re(i),prantdlf,k
            ! endif
        endif
        if (Re(i) .lt. 2300.0d0) then
            beta = pt((p(i)+p0)/1.0d6,T(i),17) ! 等压膨胀系数
            Gr = g * beta * max(abs(T(i) - TW(i)),1.0d-1) * D(i)**3/nu
            Nusselt(i) = 0.17 * Re(i)**0.33*prantdlf**0.43*(prantdlf/prantdlw)**0.25*Gr**0.1d0
            ! if (i == 31) then
            !     write(*,*)'NC',p(i),h(i),T(i),TW(i),beta,Gr,prantdlf,prantdlw,nu,Re(i)
                
            ! endif
            ! is_nan = ieee_is_nan(Nusselt(i))
            ! if (is_nan) then
            !     write(*,*)'Ht_coef error!',i,Nusselt(i)
            !     write(*,*)'NC',p(i),h(i),T(i),TW(i),beta,Gr,prantdlf,prantdlw,nu,Re(i)
            ! endif
        endif
        tc = pt((p(i)+p0)/1.0d6,T(i),26) ! Thermal conductivity
        ht_coef(i) = Nusselt(i) * tc / D(i)
    enddo
    return 
end subroutine h_coef


  