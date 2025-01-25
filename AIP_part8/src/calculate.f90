module Calculate
   use Environment
   use omp_lib
   !implicit none
contains
 subroutine Sort2dArrayByRows(A, M, N)
      integer, intent(in)        :: M, N
      real(R_), intent(inout)    :: A(:,:)
      integer                    :: i

      do concurrent (i=1:N)
          A(:M, i) = SortArray(A(:M, i), M)
       end do
   end subroutine Sort2dArrayByRows
   pure function SortArray(A, Length) result(Sorted)
      real(R_), intent(in)       :: A(:)
      integer, intent(in)        :: Length
      real(R_)                   :: Sorted(Length)
      real                       :: Temp
      integer                    :: i, MinInd

      Sorted = A
      do i = 1, Length
         MinInd = CustomMinLoc(Sorted(i:)) + i - 1
         ! Поменять мин.знач с 1 несортированным числом
         Temp = Sorted(i)
         Sorted(i) = Sorted(MinInd)
         Sorted(MinInd) = Temp
       end do
   end function SortArray

   pure function CustomMinLoc(Array) result(MinInd)
      implicit none
      real, dimension(:), intent(in) :: Array
      integer :: MinInd
      integer :: i

      MinInd = 1
      do i = 2, UBound(Array, 1)
        if (Array(i) < Array(MinInd)) then
          MinInd = i
        end if
      end do
    end function CustomMinLoc
end module Calculate
