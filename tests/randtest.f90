program randblock
! confirms random distribution of block types
use blocks, only: generate_next_type, init_random_seed
implicit none

character(*), parameter :: types = 'ITLJSZB'
character,allocatable :: b(:)
integer :: i,n
character(32) :: buf

N=1000000
call get_command_argument(1,buf,status=i)
if (i==0) read(buf,*) N

allocate(b(N))

call init_random_seed()

call generate_next_type(b)

do i=1,len(types)
  print *,types(i:i),countletters(b,types(i:i))
enddo

contains

  integer function countletters(str,let) result (c)
  
 
    character, intent(in) :: str(:)  ! array of single characters
    character, intent(in) :: let
  
    integer :: i
    
    c = 0
    
    do i = 1,size(str)
      if (str(i) == let) c=c+1
    enddo
  
  end function countletters


end program
