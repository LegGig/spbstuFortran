module IO_Func
   use Environment

   implicit none
contains
   subroutine Read2dArray(input_file, M, N, padding, Bytes, Array)
      character(*), intent(in)   :: input_file
      real(R_), allocatable, intent(inout)    :: Array(:,:)
      integer, intent(inout)     :: M, N, padding
      integer, intent(in) :: Bytes
      integer :: In = 0, i

      open (file=input_file, newunit=In)
         read (In, *) N, M ! N - строки, M - столбцы (по умолчанию M-строки)
         padding = Merge(M - Mod(M, Bytes) + Bytes, M, Mod(M, Bytes) /= 0)

         allocate(Array(padding, N))
         Array = 0

         read(In, *) (Array(1:M, i), i=1, N)

      close (In)
   end subroutine Read2dArray

   subroutine Output2dArray(output_file, M,N, Array, position)
      character(*), intent(in)   :: output_file, position
      real(R_), intent(in)       :: Array(:,:)
      integer, intent(in)        :: N, M
      integer                    :: i

      integer :: Out = 0

      open (file=output_file, encoding=E_, newunit=Out, position=position)
      do i = 1, N
         write(Out, '('//M//'(f6.1))') Array(1:M, i)
      end do
      close (Out)
   end subroutine Output2dArray

   subroutine OutputEmptyLine(output_file)
      character(*), intent(in)   :: output_file
      integer :: Out = 0
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *)
      close (Out)
   end subroutine OutputEmptyLine

end module IO_Func
