!Вычислить с заданной абсолютной погрешности ABSERR значения элементарных функций при заданной значении аргумента x:
!г) arcsin(x) = x + ((x^3)/(2*3)) + ((1*3)/(2*4*5))x^5 + ((1*3*5)/(2*4*6*7))x^7 + ...; 
program paragraph_6_1G
   use Environment

   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", read_file = "read.txt", &
                                 output_file = "output.txt"
   integer                    :: In = 0, Out = 0
   real(R_)                  :: x, atan_x

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)

   if (abs(x) >= 1.0) error stop "[Ошибка]: |x| не должен быть < 1"

   open (file=read_file, newunit=Out)
      write (Out, "(e0.10)") x
   close (Out)

   atan_x = AtanX(x)

   open (file=output_file, newunit=Out)
      write (Out, "(4(a, T18, ': ', e0.10/))") 'x', x, "Atan(x)", atan_x, "Fortran Atan(x)", atan(x), &
          "Diff", atan_x - atan(x)
   close (Out)

contains
   real(R_) pure function AtanX(x)
      real(R_), intent(in) :: x
      real(R_), parameter :: double_PI = 8 * Atan(1._R_)
      real(R_) R(4), Numerators(4), Denominators(4), x_s, x_8 !, n_fact

      ! Вычисление остатка от деления x на 2*pi
      x_s = Mod(x, double_PI)

      ! Вычисление числителей ряда
      Numerators = x_s ** [3, 5, 7, 9]
      Numerators = Numerators * [-1, 1, -1, 1]

      !Вычисление последнего элемента ряда
      x_8 = Numerators(4) / x_s

      !Определение знаминателей рядв
      Denominators = [3, 5, 7, 9]

      !Вычисление элемента ряда
      R = Numerators / Denominators

      !Вычисление значение арктангеса
      AtanX = x_s + Sum(R)

      do while (AtanX + R(4) /= AtanX)

         Numerators = Numerators * x_8
         Denominators = Denominators + 8

         R = Numerators / Denominators

         AtanX = AtanX + Sum(R)
      end do
   end function AtanX

end program paragraph_6_1G
