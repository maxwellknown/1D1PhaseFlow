subroutine read_data
    use dat 
        implicit none
        
        character(256) :: filename1  !, filename2, filename3, filename4
        integer :: i, j, num_rows, num_cols, num_lines, iunit
        ! real, allocatable :: data(:,:)
        character(len=100) :: line
        ! real*8, dimension(10000, 121) :: data
        
        ! 文件路径
        filename1 = TRIM(TRIM("/Users/maxwell/项目/数据同化/代码/1DLOOPnew/"//TRIM(i_file))//"-WallTemperature.prn")
        return 
        ! filename4 = TRIM(TRIM("/home/max/文档/1DLOOPnew/Measurements/"//TRIM(m_file))//"-Enthalpy.prn")   

        ! 读取文件数据
        iunit = 8
        open(unit=iunit, file=filename1, status='old', action='read')
        open(unit=10, file=filename1, status='old')
        ! open(unit=12, file=filename2, status='old')
        ! open(unit=14, file=filename3, status='old')
        ! open(unit=16, file=filename4, status='old')
        ! 获取数据最大行数和列数
        num_rows = 50000
        num_cols = 121
        
    ! Count the number of lines in the file
        num_lines = 0
        do
            read(iunit, '(A)', iostat=i) line
            if (i /= 0) exit
            num_lines = num_lines + 1
        end do
    
        ! Rewind the file to read it again for data extraction
        ! rewind(iunit)
        close(8)
    
        ! Allocate the array with the determined dimensions
        ! allocate(data(num_lines, 121)) ! Adjust '2' to the number of columns in your CSV
        
        ! 读取数据
        i_file_len = min(num_rows,num_lines)
        do i = 1, min(num_rows,num_lines)
            read(10, *) (TW_bc(i, j), j = 1, num_cols)
            ! read(12, *) (data_p_measurement(i, j), j = 1, num_cols)
            ! read(14, *) (data_t_measurement(i, j), j = 1, num_cols)
            ! read(16, *) (data_h_measurement(i, j), j = 1, num_cols)
        enddo
        
        ! 关闭文件
        close(10)
        ! close(12)
        ! close(14)
        ! close(16)
        ! do i = 1, min(num_rows,num_lines)
        !     write(*,*)  (data_u_measurement(i, j), j = 1, 3)
        ! enddo
        write(*,*)'Read data sucessfully!'
    
end subroutine read_data