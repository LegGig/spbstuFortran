! 8.4) Составить процедуру упорядочения по возрастанию элементов одномерного массива А (М) с испольщованием её для упорядочения элементов строк заданной матрицы С (20, 15)
program pr_8_4
   use Environment
   use IO_Func
   use Calculate

   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(*), parameter    :: from_start = "rewind", cont = "append"
   real(R_), allocatable      :: A(:, :)
   integer                    :: padding = 0
   integer, parameter         :: AvxBytes = 32, Bytes = AvxBytes / R_

   integer :: M, N

   call Read2dArray(input_file, M, N, padding, Bytes, A)

   call Output2dArray(output_file, M, N, A, from_start)

   call Sort2dArrayByRows(A, UBound(A, 1), UBound(A, 2))

   call OutputEmptyLine(output_file)
   call Output2dArray(output_file, M, N, A, cont)
   call OutputEmptyLine(output_file)

end program pr_8_4
