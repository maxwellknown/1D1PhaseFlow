module dat
    implicit none
    real * 8 ,parameter ::pi=3.1415926d0,g=9.81d0 
    real * 8 ,dimension (1:120)::ro,u,p,pd,h,ht_coef
    real * 8 ,dimension (1:120) :: ro_old,u_old,p_old, p_new, u_new
    real * 8 ,dimension (1:120):: T, dx, D, Area, dK, f, a, b, Re
    real * 8 ,dimension (1:120) :: theta, drdt, dudt, Ast, QL, TW, Nusselt 
    real * 8 :: time, dt, p0, r0, drdp
    real * 8 :: time_end, dt_max, dt_min, p_err, u_err, max_err
    real * 8 :: T_f, T_eva, T_cond, P_init
    real * 8 ,dimension (50000, 121) :: TW_bc
    
    integer :: n_interval, max_iter_num, i_file_len
    character(12) :: o_file, i_file
    character(120):: P_file,V_file,T_file,RO_file,H_file
end module dat