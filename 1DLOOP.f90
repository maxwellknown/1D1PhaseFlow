!  gfortran -fno-underscoring -o 1DLOOP5  1DLOOP5.f90 -L/usr/lib -lseuif97
!  gfortran  -fno-underscoring -c 1DLOOP seuif97.f90 -L/Users/maxwell/项目/数据同化/代码/1DLOOPnew/Src/True -lseuif97 -lm
!  gfortran  -fno-underscoring -o test test.f90  seuif97.f90 -L/Users/maxwell/项目/数据同化/代码/1D2PhaseFlow -lseuif97 -lm
!  gfortran  -fno-underscoring -o 1DLOOP *.f90  -L/Users/maxwell/项目/数据同化/代码/1D2PhaseFlow -lseuif97 -lm
! /Users/maxwell/项目/数据同化/代码/1D2PhaseFlow
program LOOP

    use dat
    implicit none

    integer j,step
    INTEGER :: start_time, end_time, elapsed_time, seconds
    character(len=60) ::temp


    call input
    ! /Users/maxwell/项目/数据同化/代码/1DLOOPnew/True
    temp = TRIM('/Users/maxwell/项目/数据同化/代码/1DLOOPnew/True/'//o_file)
    P_file = TRIM(temp)//'-Pressure.prn'
    V_file = TRIM(temp)//'-Velocity.prn'
    T_file = TRIM(temp)//'-Temperature.prn'
    RO_file = TRIM(temp)//'-Density.prn'
    H_file = TRIM(temp)//'-Enthalpy.prn'
    ! write(*,*)'o_file ',o_file,len(o_file),len_trim(o_file)
    ! write(*,*)'temp ',temp,len(temp),len_trim(temp)
    ! write(*,*)'P_file ',P_file,len(P_file),len_trim(P_file)


    OPEN(UNIT=22,FILE=TRIM(P_file))
    OPEN(UNIT=24,FILE=TRIM(V_file))
    OPEN(UNIT=26,FILE=TRIM(T_file))
    OPEN(UNIT=28,FILE=TRIM(RO_file))
    OPEN(UNIT=30,FILE=TRIM(H_file))

    call initialization
    step = 0
    ! 获取程序开始时间
    CALL SYSTEM_CLOCK(COUNT=start_time)

    do while( time .lt. time_end)

        call VelocityByincompressible
        ! dt = 1.0d-1 
        dt = dt_max
        time = time + dt 
        step = step + 1
        call Energy
        if (mod(step, n_interval) == 0) then
            ! 获取程序结束时间
            CALL SYSTEM_CLOCK(COUNT=end_time)

            ! 计算运行时长（以秒为单位）
            elapsed_time = end_time - start_time

            ! 将秒转换为时分秒格式
            seconds = elapsed_time/1000 ! ms->s
            write(22,100) time,(p(j),j=1,120)
            write(24,100) time,(u(j),j=1,120)
            write(26,100) time,(T(j),j=1,120)
            write(28,100) time,(ro(j),j=1,120)
            write(30,100) time,(h(j),j=1,120)
            write(*,*)'CPU.time=',seconds,'Prob.time=',time,'Max.err=',max_err
        end if

    enddo
    close(22)
    close(24)
    close(26)
    close(28)
    close(30)

100 format(121e18.8)

    
end program LOOP