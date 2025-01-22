! 2.4 Даны значения трех вещественных переменных a, b и c, причем два из них одинаковы.
! Найти значение отличное от этих двух.
program exercise_2_4
    use Environment
   
    implicit none
    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                     :: In = 0, Out = 0
    real(R_)                    :: a, b, c, t

    open (file=input_file, newunit=In) 
       read (In, *) a, b, c  
    close (In)

    write(*, '( a, f0.2)') 'a = ', a, 'b = ', b, 'c = ', c  
    
    !Нахождение переменной.
    if (a == b) then
        t = c
    else if (a == c) then
        t = b
    else
        t = a
    end if

    !Запись ответа.
    open (file=output_file, encoding=E_, newunit=Out)
       write (Out, '(f0.2)') t
    close (Out)
end program exercise_2_4
