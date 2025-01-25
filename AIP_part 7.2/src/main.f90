! 7.9 Вычислить нормы квадратной матрицы A, содержащей 100 элементов (N=10):
! a) ||A|| = sqrt(E (n)(i=1) E (n)(j=1) a^2 ij)
program exercise_7_2
   use Environment
   use omp_lib

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i, npadded
   real(R_), allocatable   :: A(:, :)
   real(R_)                :: A_ending

   !DIR$ ATTRIBUTES ALIGN : 32 :: A

   open (file=input_file, newunit=In)
      read (In, *) n
      npadded = Merge(N+L-Mod(N,L), N, Mod(N,L) /= 0)
      allocate (A(npadded, n))
      read (In, *) (A(1:N, i), i=1, N)
   close (In)

   do i = 1, N
      print "("//n//"f6.2)", A(:N, i)
      !write (Out, "("//n//"f6.2)") A(:N, i)
   end do

   close (Out)

   A_ending = Norm2(A)

   open (file=output_file, encoding=E_, status='replace', newunit=Out)
      write (Out, '("A = ", f10.2)') A_ending
   close (Out)
end program exercise_7_2
