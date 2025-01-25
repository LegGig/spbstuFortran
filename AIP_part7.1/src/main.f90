!Преобразовать заданную числовую последовательность a1, a2, a3, ..., a75 так, чтобы: 
!a) элементы, удовлетворяющие условию ai<=0, располагались в начале последовательности и были упорядочены по возрастанию.
program exercise_7_1_2a
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0
   real(R_), allocatable   :: A(:)
   logical, allocatable    :: Pos(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f6.2)") A
   close (Out)

   allocate(Pos(M), source = .false.)

   call SortNegatives(A, Pos)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//M//"f6.2)") A
   close (Out)

contains
   pure subroutine SortNegatives(A, Pos)
      real(R_), intent(inout) :: A(:)
      logical, intent(inout)  :: Pos(:)

      real(R_) :: tmp
      integer  :: i, MinInd

      !Создаю маску для положительных элементов
      Pos = A > 0
      !Расставляем с использованием маски элементы
      A = [Pack(A, .not. Pos),Pack(A, Pos)]

      !Сортировка отрицательных элементов
      do i = 1, Count(.not. Pos)
         MinInd = MinLoc(A(i:), 1) + i - 1
         if (i /= MinInd) then
            tmp = A(i)
            A(i) = A(MinInd)
            A(MinInd) = tmp
         end if
      end do
   end subroutine SortNegatives
end program exercise_7_1_2a
