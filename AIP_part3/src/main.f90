!ВНИМАНИЕ. В ДАННОЙ РАБОТЕ ИСПОЛЬЗУЕТСЯ ВЫРАВНИВАНИЕ И РАССПАЛЕЛИЗАЦИЯ. УБРАТЬ omp_lib И СТРОКИ С OMP и убрать DIR с npadded.

!3.3 Вычислить значения произведения y=П (^1) (n=1) ((n+1/n!)+n)

program exercise_3
   use Environment
   use omp_lib

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In, Out, N, i, j, npadded
   real(R_), allocatable   :: B(:), Fact(:)
   real(R_)                :: P

   !DIR$ ATTRIBUTES ALIGN : 32 :: B, Fact

   open (file=input_file, newunit=In)
      read (In, *) N
      npadded = Merge(N+L-Mod(N,L), N, Mod(N,L) /= 0)
      allocate (B(N))
      allocate (Fact(N-1))
      do i = 1, N
         read (In, *) B(i)
      end do
   close (In)

   ! Подсчет факториала
   Fact(1) = 1.0_R_
   !$OMP PARALLEL DO PRIVATE(i) SHARED(Fact)
   do i = 2, npadded
      !dir$ vector aligned
      Fact(i-1) = product([(j, j=2,i)])
   end do
   !$OMP END PARALLEL DO

   ! Вычислите произведение элементов в B
   P = product(B(2:N))

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "('Кол-во элементов = ', i0)") N
      write (Out, "("//N//"f6.2)") B
      write (Out, *)
      write (Out, "('Итог = ', f0.2)") P
   close (Out)

end program exercise_3

