  subroutine X(this,x)
    class(sexp), intent(in), pointer :: this
    Y, allocatable, intent(out) :: x(:,:)
    class(list_t), pointer :: lst,row
    integer :: m,n,n2,i,j
    character(len=100) :: buf

    if (.not. associated(this)) return
    call list_length(this,m,lst)
    if (.not. associated(lst)) then
       call this%err%set('Cannot get '//Z//' matrix')
       return
    end if
    if (m==0) then
       allocate(x(0,0))
       return
    end if

    call list_length(lst%car,n,row)
    if (.not. associated(row)) then
       write (buf,'("Cannot get first row of ",I0," row ",A," matrix")') m,Z
       goto 900
    end if
       
    allocate(x(m,n))

    do i=1,m
       call list_length(lst%car,n2,row)
       if (n /= n2) then
          write (buf,'("Row ",I0," has abnormal length ",I0," in ",I0," by ",I0," ",A," matrix")') &
               i,n2,m,n,Z
          goto 900
       end if
       do j=1,n
          call get_value(row%car,x(i,j))
          if (erroneous(lst%car%err)) then
             write (buf,'("Cannot parse row ",I0," column ",I0," of ",I0," by ",I0," ",A," matrix")') &
                  i,j,m,n,Z
             goto 900
          end if
          row=>row%cdr
       end do
       lst=>lst%cdr
    end do
    return

900 call this%err%set(trim(buf))
    deallocate(x)
  end subroutine X
