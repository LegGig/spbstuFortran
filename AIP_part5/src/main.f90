!5.3 У вектора X(10) вычислить сумму компонентов, предшествующих первому порядку компоненту со значением интервалов (0, 1). Если такого компонента нет, то просуммировать все компоненты вектора.

program exercise_5_3
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   integer                 :: S = 0
   integer, allocatable    :: Z(:)
   integer  indices

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)

   ! Ищем первый элемент в интервале (0, 1)
   indices = FindLoc(Z > 0 .and. Z < 1, value=.true., dim = 1)

   ! Если такого элемента нет, суммируем все элементы
   if (indices == 0) then
      S = Sum(Z)
      M = N
   else
      ! Иначе суммируем элементы до первого элемента в интервале (0, 1)
      S = Sum(Z(1:indices - 1))
      M = indices - 1
   end if

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") Z
      write (Out, '(/2(a, T12, "= ", i0/))') 'Полож.', M, "Сумма", S
   close (Out)

end program exercise_5_3
