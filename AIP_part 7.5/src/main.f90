!7.39+() Сформировать одномерный массив из положительных (отрицательных) элементов двумерного массива B (10, 15), просматривая последний по строкам (столбцам).
program exercise_7_5
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0, j = 0, npadded = 0
   real(R_), allocatable   :: A(:, :), B_positive(:), B_negative(:), C_positive(:), C_negative(:)

   !DIR$ ATTRIBUTES ALIGN : 32 :: A, B_positive, B_negative, C_positive, C_negative

   open (file=input_file, newunit=In)
      read (In, *) N
      npadded = Merge(N + L - Mod(N, L), N, Mod(N, L) /= 0)
      allocate (A(npadded, N))
        read (In, *) ((A(i, j), i = 1, N), j = 1, N) !i - строки, j - столбцы
   close (In)

   ! Формируем одномерные массивы из положительных и отрицательных элементов всех строк массива A
   B_positive = Pack(A, A > 0)
   B_negative = Pack(A, A < 0)

   deallocate (A)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate(A(npadded, N))
         read (In, *) ((A(j, i), i = 1, N), j = 1, N) !j - строки i - столбцы
   close (In)

   ! Формируем одномерные массивы из положительных и отрицательных элементов всех столбцов массива A
   C_positive = Pack(A, A > 0)
   C_negative = Pack(A, A < 0)

   open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
   close (Out)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)
      write (Out, '(A)') 'Отрицательные элементы всех строк массива A:'
      write (Out, '('//Size(B_negative)//'f6.2)') B_negative
      write (Out, *)
      write (Out, '(A)') 'Положительные элементы всех строк массива A:'
      write (Out, '('//Size(B_positive)//'f6.2)') B_positive
      write (Out, *)
      write (Out, '(A)') 'Отрицательные элементы всех столбцов массива A:'
      write (Out, '('//Size(C_negative)//'f6.2)') C_negative
      write (Out, *)
      write (Out, '(A)') 'Положительные элементы всех столбцов массива A:'
      write (Out, '('//Size(C_positive)//'f6.2)') C_positive
      write (Out, *)
   close (Out)

end program exercise_7_5


