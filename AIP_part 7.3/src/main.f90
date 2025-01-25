! 17) В массиве А (100, 50) найти элемент, являющийся: г) Наименьший по модулю.
program paragraph_7_3
   use Environment
   use omp_lib

   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", matrix_file = "matrix.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, npadded = 0, N, M, i, min_count
   real(R_)                   :: min_value
   real(R_), allocatable      :: A(:, :)
   integer, allocatable       :: Indexes(:, :), Min_Indexes(:, :)
   logical, allocatable       :: Mask(:)
   character(:), allocatable  :: fmt

   open (file=input_file, newunit=In)
      read (In, *) N, M
      npadded = Merge(N + L - Mod(N, L), N, Mod(N, L) /= 0)
      !$ omp allocate(A) align(32)
      allocate (A(N, M))
      read (In, *) (A(i, :), i = 1, N)
   close (In)

   fmt = "("//M//"f10.2, 3X)"
   open (file=matrix_file, newunit=Out)
      write (Out, fmt) (A(i, :), i = 1, N)
   close (Out)

   !$ omp allocate(Indexes,Mask) align(32)
   allocate (Indexes(N * M, 2))
   allocate (Mask(N * M), source=.false.)

   call MinAbsElement(A, Mask, Indexes, Min_Indexes, min_value, min_count)

   N = size(Min_Indexes, 1)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(a, f6.2)") "Минимальный по модулю элемент: ", min_value
      write (Out, "(a, I0)") "Количество минимальных по модулю элементов: ", min_count
      write (Out, "(a)") "Индексы минимальных по модулю элементов:"
      write (Out, "(I0, 2X, I0)") (Min_Indexes(i, :), i = 1, N)
   close (Out)


contains

   pure subroutine MinAbsElement(A, Mask, Indexes, Min_Indexes, min_value, min_count)
      real(R_), intent(in)              :: A(:, :)
      real(R_), intent(out)             :: min_value
      integer, intent(out)              :: Indexes(:, :), min_count
      integer, allocatable, intent(out) :: Min_Indexes(:, :)
      logical, intent(out)              :: Mask(:)
      integer i, j

      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)] ! Векторизация
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)] ! Векторизация

      min_value = minval(abs(A))
      Mask = [abs(A) == min_value] ! Векторизация
      min_count = count(Mask)

      !$ omp allocate(Min_Indexes) align(32)
      allocate(Min_Indexes(min_count, 2))

      Min_Indexes = reshape(pack(Indexes, spread(Mask, 2, 2)), [min_count, 2])

   end subroutine MinAbsElement

end program paragraph_7_3
