  subroutine X(this,x)
    class(sexp), intent(in), pointer :: this
    Y, allocatable, intent(out) :: x(:)
    class(list_t), pointer :: lst
    integer :: m,i

    if (.not. associated(this)) return
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          call this%err%set('Cannot get double array value from atom')
       type is (list_t)
          lst=>p
          m=0
          do
             if (.not. associated(lst)) exit
             lst=>lst%cdr
             m=m+1
          end do
          allocate(x(m))
          lst=>p
          do i=1,m
             call get_value(lst%car,x(i))
             if (erroneous(lst%car%err)) return
             lst=>lst%cdr
          end do
     end select
  end subroutine X
