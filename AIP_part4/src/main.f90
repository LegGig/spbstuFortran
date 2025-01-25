! 4.1г Протабулировать функции одной переменной: 
!      f(x) = sin(3x) - 3 sin(x)
!      2.1375 <= x <= 3.2875
!      dx = -0.25
program exercise_4_1_G
    use Environment
   
    implicit none
    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt", r_format = "(*(f0.4, 1x))"
    integer(I_)                 :: Out, In, i, n
    real(R_)                    :: max_x, min_x, dx, t
    real(R_), allocatable       :: X(:), F(:)

    open (file=input_file, newunit=In) 
        read (In, *) min_x, max_x, dx
    close (In)

    write (*, r_format) min_x, max_x, dx

    !Вычисление количества элементов в массиве
    n = int((max_x - min_x) / abs(dx)) + 1
    
    allocate(X(n), F(n))
    
    !Выбор x в зависимости + или - dx
    t = Merge(min_x, max_x, dx > 0) 
    X = [(t + i * dx , i = 0, n - 1)] 
    F = Sin(3 * X) - 3 * Sin(X)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, '(f0.4, T10, f0.4)') (X(i), F(i), i = 1, n)
    close (Out)

    deallocate(X, F)
end program exercise_4_1_G
