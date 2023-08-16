subroutine input
    use dat 
    implicit none
    character user_input

    T_f = 120.d0
    T_eva = 200.d0
    T_cond = 40.d0
    P_init = 10.0d6
    ! 默认值
    time_end = 5000.0d0
    dt_max = 1.0d-3
    dt_min = 1.0d-5
    p_err = 1.0d-10
    u_err = 1.0d-3
    n_interval = 1000
    max_iter_num = 200
    i_file = 'Not Use'
    o_file = 'mac'

        ! 打印输入的数据
    write(*, *) "Defult settings:"
    write(*, *) "T_f = ", T_f
    write(*, *) "T_eva = ", T_eva
    write(*, *) "T_cond = ", T_cond 
    write(*, *) "P_init = ", P_init
    write(*, *) "time_end =", time_end
    write(*, *) "dt_max =", dt_max
    write(*, *) "dt_min =", dt_min
    write(*, *) "P_err =", p_err
    write(*, *) "u_err =", u_err
    write(*, *) "n_interval =", n_interval
    write(*, *) "max_iter_num =", max_iter_num
    write(*, *) "i_file =", i_file
    write(*, *) "o_file =", o_file

    write(*,*) 'Do you want the defult settings? Press Y/y to confirm or any other key to set new value!'
    read(*, *) user_input
    if (user_input /= 'Y' .and. user_input /= 'y') then
        write(*, *) "...................Set New Value....................."
        goto 100
    else 
        write(*, *) 'HELLO 1DLOOP!'
        return
    endif



100 call process_real_input(T_f,'T_f',T_f)
    call process_real_input(T_eva,'T_eva',T_eva)
    call process_real_input(T_cond,'T_cond',T_cond)
    call process_real_input(P_init,'P_init',P_init)
    call process_real_input(time_end,'time_end',time_end)
    call process_real_input(dt_max,'dt_max',dt_max)
    call process_real_input(dt_min,'dt_min',dt_min)
    call process_real_input(p_err,'p_err',p_err)
    call process_real_input(u_err,'u_err',u_err)
    call process_int_input(n_interval,'n_interval',n_interval)
    call process_int_input(max_iter_num,'max_iter_num',max_iter_num)
    call process_chara_input(i_file,'i_file',i_file)
    call process_chara_input(o_file,'o_file',o_file)

        ! 打印输入的数据
    write(*, *) "Defult settings:"
    write(*, *) "T_f = ", T_f
    write(*, *) "T_eva = ", T_eva
    write(*, *) "T_cond = ", T_cond 
    write(*, *) "P_init = ", P_init
    write(*, *) "time_end =", time_end
    write(*, *) "dt_max =", dt_max
    write(*, *) "dt_min =", dt_min
    write(*, *) "P_err =", p_err
    write(*, *) "u_err =", u_err
    write(*, *) "n_interval =", n_interval
    write(*, *) "max_iter_num =", max_iter_num
    write(*, *) "i_file =", i_file
    write(*, *) "o_file =", o_file

    write(*, *) 'Press Y/y to Confirm the values. Press any other key to quit the code! '
    read(*, *) user_input
    if (user_input /= 'Y' .and. user_input /= 'y') then
        stop
    else 
        write(*, *) 'HELLO 1DLOOP!'
    endif
    
end subroutine input

subroutine process_int_input(default_var, default_var_name,  new_var)
    integer, intent(in) :: default_var
    character(len=*), intent(in) :: default_var_name
    integer, intent(out) ::  new_var
  
    character(len=12) user_input
    character(len=120) output_string
  
    
    write(output_string, '(A, A, A, I0, A)') "Use the default value(",TRIM(default_var_name)&
    ,"=", default_var, ")? (Y/y for yes, any other character for no):"
    write(*, *) output_string
    read(*, *) user_input
    if (user_input /= 'Y' .and. user_input /= 'y') then
        write(output_string, '(A, A, A)') 'Input New ',TRIM(default_var_name),' value! '
        write(*, *) output_string
        read(*, *) new_var
    else
      new_var = default_var
    endif
  
  end subroutine process_int_input
  
  subroutine process_real_input(default_var, default_var_name,  new_var)
    real * 8, intent(in) :: default_var
    character(len=*), intent(in) :: default_var_name
    real * 8, intent(out) ::  new_var
  
    character(len=12) user_input
    character(len=120) output_string
  
    
    write(output_string, '(A, A, A, F18.8, A)') "Use the default value(",TRIM(default_var_name)&
    ,"=", default_var, ")? (Y/y for yes, any other character for no):"
    write(*, *) output_string
    read(*, *) user_input
    if (user_input /= 'Y' .and. user_input /= 'y') then
        write(output_string, '(A, A, A)') 'Input New ',TRIM(default_var_name),' value! '
        write(*, *) output_string
        read(*, *) new_var
    else
      new_var = default_var
    endif
  
  end subroutine process_real_input
  
  subroutine process_chara_input(default_var, default_var_name,  new_var)
    character(len=*), intent(in) :: default_var
    character(len=*), intent(in) :: default_var_name
    character(len=*), intent(out) ::  new_var
  
    character(len=12) user_input
    character(len=120) output_string
  
    
    write(output_string, '(A, A, A, A, A)') "Use the default value(",TRIM(default_var_name)&
    ,"=", default_var, ")? (Y/y for yes, any other character for no):"
    write(*, *) output_string
    read(*, *) user_input
    if (user_input /= 'Y' .and. user_input /= 'y') then
        write(output_string, '(A, A, A)') 'Input New ',TRIM(default_var_name),' value! '
        write(*, *) output_string
        read(*, *) new_var
        new_var = TRIM(new_var)
    else
      new_var = TRIM(default_var)
    endif
  
  end subroutine process_chara_input


