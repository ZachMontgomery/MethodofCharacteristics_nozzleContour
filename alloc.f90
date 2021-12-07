module alloc
implicit none
!***************************************************************************
contains
!***************************************************************************
subroutine set(matrix, nodes)
integer, intent(in) :: nodes
real, allocatable, dimension(:,:,:) :: matrix
allocate(matrix(0:nodes,nodes+2,4))
matrix = 0.
end subroutine set
!***************************************************************************
end module alloc
