  subroutine X(this,x)
    class(sexp), pointer, intent(in) :: this
    Y, intent(out) :: x
    integer :: st

    if (.not. associated(this)) return
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          read (p%content,*,iostat=st) x
          if (st/=0) call this%err%set('Cannot read '//Z//' from '//p%content)
       type is (list_t)
          call this%err%set('Cannot get '//Z//' value from list')
     end select
  end subroutine X
