! 7.29()+ Расположить строки матрицы А (10, 15) в порядке убывания (возрастания) их первых элементов aij. Вывести на печать прежнее номера строк в упорядоченной матрице.
program exercise_7_4_29
   use Environment
   use omp_lib

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, npadded = 0
   real(R_), allocatable   :: A(:, :), A_copy(:, :)
   integer, allocatable    :: N_ind(:)

   !DIR$ ATTRIBUTES ALIGN : 32 :: A, A_copy, N_ind

   open (file=input_file, newunit=In)
      read (In, *) N, M
      npadded = Merge(N + L - Mod(N, L), N, Mod(N, L) /= 0)
      allocate (A(M, N))
      read (In, *) (A(:, i), i = 1, N) !j = строки i = столбцы
   close (In)

   ! Создание копии матрицы A
   allocate(A_copy(size(A,1), size(A,2)))
   A_copy = A

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (A(:, i), i = 1, N)
   close (Out)

   allocate (N_ind(N))

   call SortMatrixDec(A, N, M, N_ind)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(/'//N//'('//M//'f6.2/))') (A(1:M, i), i = 1, N)
      write (Out, '("Предыдущий порядок строк ", '//N//'i3)') (N_ind(i), i = 1, N)

      call SortMatrixInc(A_copy, N, M, N_ind)

      write (Out, '(A, I3)') "Предыдущий порядок строк для второго массива: "
      write (Out, '(10(I3, " "))') (N_ind(i), i = 1, N)

   close (Out)

contains

   pure subroutine SortMatrixDec(A, N, M, N_ind)
      real(R_), intent(inout) :: A(:, :)
      integer, intent(inout)  :: N, M, N_ind(:)

      integer i, MaxInd, tmp_ind
      real(R_) tmp(M)

      N_ind = [((i), i = 1, N)]

      !OMP PARALLEL DO SIMD PRIVATE(MaxInd, tmp_ind, tmp)
      do i = 1, N
         MaxInd = MaxLoc(A(1,i:), 1) + i - 1
         if (i /= MaxInd) then
            tmp_ind = N_ind(i)
            tmp = A(:, i)

            N_ind(i) = N_ind(MaxInd)
            A(:, i) = A(:, MaxInd)

            N_ind(MaxInd) = tmp_ind
            A(:, MaxInd) = tmp
         end if
      end do
      !OMP END PARALLEL DO SIMD
   end subroutine SortMatrixDec

   pure subroutine SortMatrixInc(A_copy, N, M, N_ind)
      real(R_), intent(inout) :: A_copy(:, :)
      integer, intent(inout)  :: N, M, N_ind(:)

      integer i, MaxInd, tmp_ind
      real(R_) tmp(M)

      N_ind = [((i), i = 1, N)]

      !OMP PARALLEL DO SIMD PRIVATE(MaxInd, tmp_ind, tmp)
      do i = N, 1, -1
         MaxInd = MaxLoc(A_copy(1,:i), 1)
         if (i /= MaxInd) then
            tmp_ind = N_ind(i)
            tmp = A_copy(:, i)

            N_ind(i) = N_ind(MaxInd)
            A_copy(:, i) = A_copy(:, MaxInd)

            N_ind(MaxInd) = tmp_ind
            A_copy(:, MaxInd) = tmp
         end if
      end do
      !OMP END PARALLEL DO SIMD
   end subroutine SortMatrixInc

end program exercise_7_4_29



