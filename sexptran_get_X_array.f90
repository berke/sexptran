  subroutine X(this,x)
    class(sexp), intent(in), pointer :: this
    Y, allocatable, intent(out) :: x(:)
    class(list_t), pointer :: lst
    integer :: m,i

    call list_length(this,m,lst)
    if (.not. associated(lst)) then
       call this%err%set('Cannot get '//Z//' array')
       return
    end if
    allocate(x(m))
    do i=1,m
       call get_value(lst%car,x(i))
       if (erroneous(lst%car%err)) return
       lst=>lst%cdr
    end do
  end subroutine X
