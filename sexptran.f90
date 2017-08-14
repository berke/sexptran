! SEXPTRAN
! Fortran library for reading and writing S-expressions
! 
! Copyright (c) 2017, Oguz Berke Antoine DURAK <berke.durak@gmail.com>
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:

! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.

! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module sexptran
  use input_wrapper_m
  implicit none

  integer, parameter :: dp=kind(0.0d0)

  type :: out_channel
     integer :: unit
     logical :: is_open=.false.
   contains
     procedure :: open_out=>out_channel_open, write=>out_channel_write
     final :: out_channel_cleanup
  end type out_channel

  type, extends(out_channel) :: line_wrapper
     integer :: col=1,col_max=80
   contains
     procedure :: write=>line_wrapper_write
  end type line_wrapper

  type :: error_status
     logical :: error=.false.
     character(len=:),allocatable :: message
   contains
     procedure :: set=>error_status_set,clear=>error_status_clear,check=>error_check
  end type error_status

  type(error_status), target :: default_error

  type, abstract :: sexp
     integer :: refcnt=0
     type(error_status), pointer :: err=>default_error
     contains
       procedure(write_intf), deferred :: write
       procedure(is_atom_intf), nopass, deferred :: is_atom
       procedure :: save=>sexp_save
       procedure :: ref=>sexp_ref
  end type sexp

  abstract interface
     subroutine write_intf(this,oc,start)
       import
       class(sexp), intent(in), target :: this
       class(out_channel) :: oc
       logical, intent(in), optional :: start
     end subroutine write_intf

     function is_atom_intf()
       import
       logical :: is_atom_intf
     end function is_atom_intf

     subroutine write_char_intf(oc,u)
       import
       class(out_channel) :: oc
       character(len=*), intent(in) :: u
     end subroutine write_char_intf
  end interface

  interface tuple
     module procedure tuple1,tuple2,tuple3,tuple4,tuple5
  end interface tuple

  interface get_value
     module procedure sexp_get_logical,sexp_get_double,sexp_get_float, &
          sexp_get_integer,sexp_get_string,sexp_get_double_array
  end interface

  interface atom
     module procedure atom_string,atom_integer,atom_float,atom_double, &
          atom_logical
  end interface atom

  interface list
     module procedure list_float_array,list_double_array
  end interface list

  interface deref
     module procedure deref_list,deref_sexp
  end interface deref

  type, extends(sexp) :: list_t
     class(sexp), pointer :: car=>null()
     class(list_t), pointer :: cdr=>null()
   contains
     procedure :: write=>list_write,set_car,set_cdr
     procedure, nopass :: is_atom=>list_is_atom
     final :: list_cleanup
  end type list_t

  type, extends(sexp) :: atom_t
     character(len=:), pointer :: content
   contains
     procedure :: write=>atom_write
     procedure, nopass :: is_atom=>atom_is_atom
     final :: atom_cleanup
  end type atom_t

  character(len=*), parameter :: &
       safe_set='abcdefghijklmnopqrstuvwxyz' // &
                'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                '0123456789' // &
                '/,.-+_''!@%^&*{}[]|:?<>'

  character, parameter, private :: &
       lf=achar(10),cr=achar(13),tab=achar(9),backslash=achar(92),bs=achar(8)

  class(sexp), pointer :: nil=>null()

  type :: position
     integer :: pos=1,row=1,row_begin=1
  end type position

  type(position) :: start_pos

  integer, parameter :: &
       tok_error=-1,tok_unspecified=0,tok_lpar=1,tok_rpar=2,tok_atom=3,tok_eof=4

  type :: lexer
     character(len=:), pointer :: u
     type(position) :: start,current
     integer :: kind
     character(len=:), pointer :: data=>null()
     type(error_status) :: error
   contains
     procedure :: init=>lexer_init
     procedure :: read_token
  end type lexer
contains
  subroutine error_check(err)
    class(error_status) :: err
    if (err%error) then
       write (error_unit,'("ERROR: ",A)') err%message
       stop 'Fatal error'
    end if
  end subroutine error_check

  function field(this,name) result(ptr)
    class(sexp), intent(in), pointer :: this
    class(sexp), pointer :: ptr
    class(list_t), pointer :: lst
    character(len=*), intent(in) ::name 

    ptr=>null()
    if (.not. associated(this)) return
    if (this%err%error) return
    ptr=>this
    select type(p=>this)
       type is (atom_t)
          call this%err%set('Expecting associative list, not atom')
       type is (list_t)
          lst=>p
          do
             if (.not. associated(lst)) then
                call this%err%set('Field '//name//' not found')
                return
             end if

             select type(q=>lst%car)
                type is (list_t)
                   select type(r=>q%car)
                        type is (atom_t)
                           if (r%content==name) then
                              if (associated(q%cdr)) then
                                 if (associated(q%cdr%cdr)) then
                                    call this%err%set('Key '//name//' is not a pair')
                                 else
                                    ptr=>q%cdr%car
                                 end if
                              else
                                 call this%err%set('Key '//name//' has no associated value')
                              end if
                              return
                           else
                              lst=>lst%cdr
                           end if

                        class default
                           call this%err%set('Not an associative list indexed by atoms')
                           return
                   end select
               class default
                  call this%err%set('Not an associative list')
                  return
             end select
          end do
     end select
  end function field

  function nth(this,i) result(ptr)
    class(sexp), intent(in), pointer :: this
    class(sexp), pointer :: ptr
    class(list_t), pointer :: lst
    integer, intent(in) :: i
    integer :: j

    if (.not. associated(this)) then
       ptr=>null()
       return
    end if
    ptr=>this
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          call this%err%set('Cannot index atom')
       type is (list_t)
          lst=>p
          j=1
          do
             if (.not. associated(lst)) then
                call this%err%set('Index out of bounds')
                return
             end if
             if (i==j) then
                ptr=>lst%car
                return
             end if
             lst=>lst%cdr
             j=j+1
          end do
     end select
  end function nth
  
  subroutine sexp_get_string(this,u)
    class(sexp), pointer, intent(in) :: this
    character(len=:), allocatable, intent(out) :: u

    if (.not. associated(this)) return
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          allocate(u,source=p%content)
       type is (list_t)
          call this%err%set('Cannot get string value from list')
     end select
  end subroutine sexp_get_string

  subroutine sexp_get_integer(this,x)
    class(sexp), pointer, intent(in) :: this
    integer, intent(out) :: x
    integer :: st

    if (.not. associated(this)) return
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          read (p%content,*,iostat=st) x
          if (st/=0) call this%err%set('Cannot read integer from '//p%content)
       type is (list_t)
          call this%err%set('Cannot get integer value from list')
     end select
  end subroutine sexp_get_integer

  subroutine sexp_get_float(this,x)
    class(sexp), pointer, intent(in) :: this
    real, intent(out) :: x
    integer :: st

    if (.not. associated(this)) return
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          read (p%content,*,iostat=st) x
          if (st/=0) call this%err%set('Cannot read float from '//p%content)
       type is (list_t)
          call this%err%set('Cannot get float value from list')
     end select
  end subroutine sexp_get_float

  subroutine sexp_get_double(this,x)
    class(sexp), pointer, intent(in) :: this
    real(dp), intent(out) :: x
    integer :: st

    if (.not. associated(this)) return
    if (this%err%error) return
    select type(p=>this)
       type is (atom_t)
          read (p%content,*,iostat=st) x
          if (st/=0) call this%err%set('Cannot read double from '//p%content)
       type is (list_t)
          call this%err%set('Cannot get double value from list')
     end select
  end subroutine sexp_get_double

  subroutine sexp_get_double_array(this,x)
    class(sexp), intent(in), pointer :: this
    real(dp), allocatable, intent(out) :: x(:)
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
  end subroutine sexp_get_double_array

  function erroneous(err)
    type(error_status), pointer :: err
    logical :: erroneous

    if (associated(err)) then
       erroneous=err%error
    else
       erroneous=.false.
    end if
  end function erroneous

  subroutine sexp_get_logical(this,x)
    class(sexp), intent(in) :: this
    logical, intent(out) :: x

    if (erroneous(this%err)) return
    select type(p=>this)
       type is (atom_t)
          select case(p%content)
            case ('true')  ; x=.true.
            case ('false') ; x=.false.
            case default   ; call this%err%set('Illegal logical value '//p%content)
          end select
       type is (list_t)
          call this%err%set('Cannot get logical value from list')
     end select
  end subroutine sexp_get_logical

  subroutine out_channel_open(oc,fn,err)
    class(out_channel) :: oc
    type(error_status), intent(out) :: err
    character(len=*), intent(in) :: fn
    integer :: st

    open(newunit=oc%unit,file=fn,action='write',status='replace', &
         form='unformatted',access='stream',iostat=st)
    if (st/=0) then
       call err%set('Cannot open output file '//fn)
    end if
    oc%is_open=.true.
  end subroutine out_channel_open

  subroutine out_channel_cleanup(oc)
    type(out_channel) :: oc
    if (oc%is_open) then
       close(oc%unit)
       oc%is_open=.false.
    end if
  end subroutine out_channel_cleanup

  subroutine out_channel_write(oc,u)
    class(out_channel) :: oc
    character(len=*), intent(in) :: u

    write (oc%unit) u
  end subroutine out_channel_write

  subroutine line_wrapper_write(oc,u)
    class(line_wrapper) :: oc
    character(len=*), intent(in) :: u
    integer :: i,j
    
    j=oc%col
    do i=1,len(u)
       if (u(i:i)==lf) then
          j=1
       else
          j=j+1
       end if
    end do
    write (oc%unit) u
    if (j>oc%col_max) then
       write (oc%unit) lf
       j=1
    end if
    oc%col=j
  end subroutine line_wrapper_write

  subroutine error_status_clear(this)
    class(error_status) :: this

    if (allocated(this%message)) deallocate(this%message)
    this%error=.false.
  end subroutine error_status_clear

  subroutine error_status_set(this,u)
    class(error_status) :: this
    character(len=*), intent(in) :: u

    call this%clear
    allocate(this%message,source=u)
    this%error=.true.
  end subroutine error_status_set

  subroutine lexer_init(this,u)
    class(lexer) :: this
    character(len=*),intent(in),target :: u
    this%u=>u
    this%start=start_pos
    this%current=start_pos
  end subroutine lexer_init

  function atom_is_atom()
    logical :: atom_is_atom
    atom_is_atom=.true.
  end function atom_is_atom

  function list_is_atom()
    logical :: list_is_atom
    list_is_atom=.false.
  end function list_is_atom

  subroutine atom_cleanup(this)
    type(atom_t) :: this

    if (associated(this%content)) deallocate(this%content)
  end subroutine atom_cleanup

  subroutine sexp_ref(this)
    class(sexp) :: this

    this%refcnt=this%refcnt+1
  end subroutine sexp_ref

  recursive subroutine deref_sexp(this)
    class(sexp), pointer :: this

    this%refcnt=this%refcnt-1
    if (this%refcnt==0) deallocate(this)
  end subroutine deref_sexp

  recursive subroutine deref_list(this)
    type(list_t), pointer :: this

    this%refcnt=this%refcnt-1
    if (this%refcnt==0) deallocate(this)
  end subroutine deref_list
  
  recursive subroutine list_cleanup(this)
    type(list_t) :: this

    call this%set_car(null())
    call this%set_cdr(null())
  end subroutine list_cleanup

  recursive subroutine set_car(cell,car)
    class(list_t) :: cell
    class(sexp), pointer :: car

    if (associated(cell%car)) call deref(cell%car)
    cell%car=>car
    if (associated(car)) call car%ref
  end subroutine set_car

  recursive subroutine set_cdr(cell,cdr)
    class(list_t) :: cell
    type(list_t), pointer :: cdr

    if (associated(cell%cdr)) call deref(cell%cdr)
    cell%cdr=>cdr
    if (associated(cdr)) call cdr%ref
  end subroutine set_cdr

  subroutine sexp_report_error(lex,error,unit)
    type(lexer), intent(in) :: lex
    type(error_status) :: error
    integer, intent(in), optional :: unit
    integer :: un

    if (present(unit)) then
       un=unit
    else
       un=error_unit
    end if

    if (lex%kind==tok_error) then
       write (un,'("LEXICAL ERROR reading S-expression: ",A)') lex%error%message
    else
       write (un,'("SYNTAX ERROR reading S-expression: ",A)') error%message
    end if
    write (un,'("Between positions ",I0,":",I0," and ",I0,":",I0)') &
         lex%start%row,lex%start%pos-lex%start%row_begin+1, &
         lex%current%row,lex%current%pos-lex%current%row_begin+1
    write (un,'(I4,1X,A,"<<HERE>>")') &
         lex%current%row, &
         lex%u(lex%current%row_begin:min(len(lex%u),max(1,lex%current%pos+1)))
  end subroutine sexp_report_error

  function token_to_string(k) result(u)
    integer, intent(in) :: k
    character(len=:), pointer :: u

    select case (k)
       case (tok_error)       ; call set('Error')
       case (tok_unspecified) ; call set('No token')
       case (tok_lpar)        ; call set('Left parenthesis')
       case (tok_rpar)        ; call set('Right parenthesis')
       case (tok_eof)         ; call set('End of file')
       case (tok_atom)        ; call set('Atom')
       case default           ; call set('Invalid token code')
    end select
  contains
    subroutine set(v)
      character(len=*), intent(in) :: v
      allocate(u,source=v)
    end subroutine set
  end function token_to_string

  function sexp_load(fn,err) result(this)
    character(len=*), intent(in) :: fn
    type(error_status), intent(out), target :: err
    type(error_status), pointer :: err_ptr
    class(sexp), pointer :: this
    character(len=:),allocatable,target :: buf
    type(lexer) :: lex

    err_ptr=>err
    this=>null()
    call slurp(fn,buf,err)
    if (err%error) return
    call lex%init(buf)
    this=>sexp_read(lex,err_ptr,finish=.true.)
    if (err%error) then
       call sexp_report_error(lex,err)
    end if
  end function sexp_load

  recursive function sexp_read(lex,err,next,finish) result(this)
    type(lexer) :: lex
    type(error_status), pointer :: err
    class(sexp), pointer :: this
    logical, optional :: next, finish
    
    this=>null()

    if (defval(.true.,next)) call lex%read_token()

    select case (lex%kind)
       case (tok_atom)
          this=>atom_get(lex,err)
       case (tok_lpar)
          this=>list_read(lex,err)
       case default
          call err%set('Unexpected token in S-expression: '//token_to_string(lex%kind))
    end select

    if (.not. err%error .and. defval(.false.,finish)) then
       call lex%read_token()
       if (lex%kind/=tok_eof) then
          call err%set('Trailing garbage')
          deallocate(this)
          this=>null()
       end if
    end if
  end function sexp_read

  recursive function list_read(lex,err) result(this)
    type(lexer) :: lex
    class(list_t), pointer :: this
    type(error_status), pointer :: err
    
    this=>null()
    
    call err%clear
    call lex%read_token()
    select case (lex%kind)
        case (tok_rpar)
           call err%clear
        case (tok_lpar,tok_atom)
           allocate(this)
           this%err=>err
           call this%set_car(sexp_read(lex,err,next=.false.))
           if (err%error) then
              deallocate(this)
              return
           end if
           call this%set_cdr(list_read(lex,err))
        case default
           call err%set('Unexpected token in list: '//token_to_string(lex%kind))
    end select
  end function list_read

  function atom_get(lex,err) result(this_ptr)
    type(lexer) :: lex
    type(error_status), pointer :: err
    class(atom_t), pointer :: this
    class(sexp), pointer :: this_ptr

    if (lex%kind /= tok_atom) then
       this_ptr=>null()
       call err%set('Token is not an atom')
       return
    else
       allocate(this)
       this%err=>err
       this%content=>lex%data
       lex%data=>null()
       this_ptr=>this
       call err%clear
    end if
  end function atom_get

  function atom_string(u) result(this_ptr)
    character(len=*), intent(in) :: u
    class(atom_t), pointer :: this
    class(sexp), pointer :: this_ptr

    allocate(this)
    allocate(this%content,source=u)
    this_ptr=>this
  end function atom_string

  function atom_integer(n) result(this_ptr)
    integer, intent(in) :: n
    character(len=32) :: buf
    class(sexp), pointer :: this_ptr

    write(buf,'(I0)') n
    this_ptr=>atom_string(trim(adjustl(buf)))
  end function atom_integer

  function atom_float(u) result(this_ptr)
    real, intent(in) :: u
    character(len=22) :: buf
    class(sexp), pointer :: this_ptr

    write(buf,'(G22.16)') u
    this_ptr=>atom_string(trim(adjustl(buf)))
  end function atom_float

  function atom_double(u) result(this_ptr)
    real(dp), intent(in) :: u
    character(len=22) :: buf
    class(sexp), pointer :: this_ptr

    write(buf,'(G22.16)') u
    this_ptr=>atom_string(trim(adjustl(buf)))
  end function atom_double

  function atom_logical(x) result(this_ptr)
    logical, intent(in) :: x
    class(sexp), pointer :: this_ptr

    if (x) then
       this_ptr=>atom_string('true')
    else
       this_ptr=>atom_string('false')
    end if
  end function atom_logical

  function is_eof(u,i)
    character(len=*), intent(in) :: u
    integer, intent(in) :: i
    logical :: is_eof

    is_eof=i>len(u)
  end function is_eof

  function readch(u,i,eof)
    character(len=*), intent(in) :: u
    integer, intent(inout) :: i
    logical, intent(out) :: eof
    character :: readch

    if (i>len(u)) then
       eof=.true.
    else
       readch=u(i:i)
       i=i+1
       eof=.false.
    end if
  end function readch

  function list_float_array(a) result(ptr)
    real, intent(in) :: a(:)
    integer :: m,i
    class(sexp), pointer :: ptr

    m=size(a)
    ptr=>nil
    do i=m,1,-1
       ptr=>cons(atom(a(i)),ptr)
    end do
  end function list_float_array

  function list_double_array(a) result(ptr)
    real(dp), intent(in) :: a(:)
    integer :: m,i
    class(sexp), pointer :: ptr

    m=size(a)
    ptr=>nil
    do i=m,1,-1
       ptr=>cons(atom(a(i)),ptr)
    end do
  end function list_double_array

  function tuple1(a) result(ptr)
    class(sexp), pointer, intent(in) :: a
    class(sexp), pointer :: ptr

    ptr=>cons(a,nil)
  end function tuple1

  function pair(k,v) result(ptr)
    character(len=*), intent(in) :: k
    class(sexp), pointer, intent(in) :: v
    class(sexp), pointer :: ptr

    ptr=>tuple(atom(k),v)
  end function pair

  function tuple2(a,b) result(ptr)
    class(sexp), pointer, intent(in) :: a,b
    class(sexp), pointer :: ptr

    ptr=>cons(a,cons(b,nil))
  end function tuple2

  function tuple3(a,b,c) result(ptr)
    class(sexp), pointer, intent(in) :: a,b,c
    class(sexp), pointer :: ptr

    ptr=>cons(a,cons(b,cons(c,nil)))
  end function tuple3

  function tuple4(a,b,c,d) result(ptr)
    class(sexp), pointer, intent(in) :: a,b,c,d
    class(sexp), pointer :: ptr

    ptr=>cons(a,cons(b,cons(c,cons(d,nil))))
  end function tuple4

  function tuple5(a,b,c,d,e) result(ptr)
    class(sexp), pointer, intent(in) :: a,b,c,d,e
    class(sexp), pointer :: ptr

    ptr=>cons(a,cons(b,cons(c,cons(d,cons(e,nil)))))
  end function tuple5

  function cons(car,cdr) result(this_ptr)
    class(sexp), pointer, intent(in) :: car
    class(sexp), pointer, intent(in), optional :: cdr
    class(list_t), pointer :: this
    class(sexp), pointer :: this_ptr

    allocate(this)
    call this%set_car(car)
    if (present(cdr)) then
       if (associated(cdr)) then
          select type(p=>cdr)
             class is (list_t)
                call this%set_cdr(p)
             class default
                stop 'CDR must be list'
          end select
       end if
    else
       call this%set_cdr(null())
    end if
    this_ptr=>this
  end function cons

  function defval(def,x)
    logical, intent(in) :: def
    logical :: defval
    logical, intent(in), optional :: x

    if (present(x)) then
       defval=x
    else
       defval=def
    end if
  end function defval

  subroutine sexp_save(this,fn,err,nowrap)
    class(sexp), intent(in) :: this
    character(len=*),intent(in) :: fn
    type(error_status), intent(out) :: err
    logical, intent(in), optional :: nowrap
    type(out_channel) :: oc
    type(line_wrapper) :: lw

    if (defval(.false.,nowrap)) then
      call oc%open_out(fn,err)
      call save(oc)
    else
      call lw%open_out(fn,err)
      call save(lw)
    end if
  contains
    subroutine save(oc)
      class(out_channel) :: oc

      if (err%error) return
      call this%write(oc)
      call oc%write(lf)
    end subroutine save
  end subroutine sexp_save

  recursive subroutine list_write(this,oc,start)
    class(out_channel) :: oc
    class(list_t), intent(in), target :: this
    logical, intent(in), optional :: start

    if (defval(.true.,start)) call oc%write('(')
    if (associated(this%car)) call this%car%write(oc,.true.)
    if (associated(this%cdr)) then
       if (this%cdr%is_atom()) then
          call oc%write('. ')
          call this%cdr%write(oc,.true.)
       else
          call oc%write(' ')
          call this%cdr%write(oc,.false.)
       end if
    end if
    if (defval(.true.,start)) call oc%write(')')
  end subroutine list_write

  subroutine atom_write(this,oc,start)
    class(out_channel) :: oc
    class(atom_t), intent(in), target :: this
    logical, intent(in), optional :: start

    call write_string(oc,this%content)
    if (.not. defval(.true.,start)) call oc%write(' ')
  end subroutine atom_write

  function safe_string(str) result(safe)
    character(len=*), intent(in) :: str
    logical :: safe
    safe=len(str)>0 .and. verify(str,safe_set)==0
  end function safe_string

  subroutine write_string(oc,str)
    class(out_channel) :: oc
    character(len=*), intent(in) :: str
    character :: c
    integer :: i,m

    m=len(str)
    
    if (safe_string(str)) then
       call oc%write(str)
    else
       call oc%write('"')
       do i=1,m
          c=str(i:i)
          select case (c)
             case ('"')
                call oc%write('\"')
             case (lf)
                call oc%write('\n')
             case (cr)
                call oc%write('\r')
             case (bs)
                call oc%write('\b')
             case (tab)
                call oc%write('\t')
             case default
                call oc%write(c)
          end select
       end do
       call oc%write('"')
    end if
  end subroutine write_string

  subroutine slurp(fn,u,err)
    character(len=*), intent(in) :: fn
    character(len=:), allocatable :: u
    type(error_status), intent(out) :: err
    integer :: unit,size,st

    open(newunit=unit,file=fn,action='read',form='unformatted', &
         status='old',access='stream',iostat=st)
    if (st /= 0) then
       call err%set('Cannot open file '//trim(fn)//' for reading')
       return
    end if
    inquire(unit,size=size)
    allocate(character(len=size) :: u)
    read(unit,iostat=st) u
    if (st /= 0) then
       call err%set('Error while reading from file '//trim(fn))
    end if
    close(unit)
  end subroutine slurp

  subroutine read_token(lex)
    class(lexer), intent(inout) :: lex
    character :: c
    integer, parameter :: q_init=1,q_comment=2,q_quoted=3, &
                          q_unquoted=4,q_backslash=5,q_decimal=6, &
                          q_continuation=7,q_continuation_cr=8
    integer :: i,q,m,d,x
    logical :: for_real,eof,back
    
    for_real=.false.
    lex%kind=tok_unspecified
    lex%start=lex%current
    lex%data=>null()

    call scan

    if (lex%kind==tok_atom) then
       lex%current=lex%start
       lex%kind=tok_unspecified
       for_real=.true.
       allocate(character(len=m) :: lex%data)
       call scan
    end if
  contains
    subroutine append_atom(c)
      character, intent(in) :: c
      m=m+1
      if (for_real) lex%data(m:m)=c
    end subroutine append_atom

    subroutine scan
      type(position) :: prev

      q=q_init
      m=0
      do
         if (lex%kind/=tok_unspecified) exit

         prev=lex%current
         i=lex%current%pos

         if (i<=len(lex%u)) then
            c=lex%u(i:i)
            i=i+1
            lex%current%pos=i
            eof=.false.
            if (c==lf) then
               lex%current%row_begin=i
               lex%current%row=lex%current%row+1
            end if
         else
            eof=.true.
         end if

         back=.false.
         select case(q)
            case (q_init)            ; call x_init
            case (q_comment)         ; call x_comment
            case (q_quoted)          ; call x_quoted
            case (q_unquoted)        ; call x_unquoted
            case (q_backslash)       ; call x_backslash
            case (q_decimal)         ; call x_decimal
            case (q_continuation)    ; call x_continuation
            case (q_continuation_cr) ; call x_continuation_cr
         end select

         if (back) lex%current=prev
      end do
    end subroutine scan

    subroutine x_init
      if (eof) then
         lex%kind=tok_eof
      else
         select case (c)
         case (' ',tab,lf,cr)
         case (';')           ; q=q_comment
         case ('(')           ; lex%kind=tok_lpar
         case (')')           ; lex%kind=tok_rpar
         case ('"')           ; q=q_quoted
         case default         ; q=q_unquoted; back=.true.
         end select
      end if
    end subroutine x_init

    subroutine x_comment
      if (eof) then
         lex%kind=tok_eof
      else
         select case (c)
            case (lf)            ; q=q_init
            case default
         end select
      end if
    end subroutine x_comment

    subroutine x_unquoted
      if (eof) then
         lex%kind=tok_atom
      else
         select case(c)
            case (' ',tab,lf,cr,'(',')',';','"')
               lex%kind=tok_atom
               back=.true.
            case default
               call append_atom(c)
         end select
      end if
    end subroutine x_unquoted

    subroutine x_quoted
      if (eof) then
         lex%kind=tok_error
         call lex%error%set('EOF in quoted string')
      else
         select case(c)
            case ('"')       ; lex%kind=tok_atom
            case (backslash) ; q=q_backslash
            case default     ; call append_atom(c)
         end select
      end if
    end subroutine x_quoted

    subroutine x_backslash
      if (eof) then
         lex%kind=tok_error
         call lex%error%set('EOF in escape sequence')
      else
         select case(c)
            case (cr)        ; q=q_continuation_cr
            case (lf)        ; q=q_continuation
            case ('"')       ; call append_atom('"')       ; q=q_quoted
            case (backslash) ; call append_atom(backslash) ; q=q_quoted
            case ('n')       ; call append_atom(lf)        ; q=q_quoted
            case ('t')       ; call append_atom(tab)       ; q=q_quoted
            case ('b')       ; call append_atom(bs)        ; q=q_quoted
            case ('0':'9')
               q=q_decimal
               d=0
               x=0
               back=.true.
            case default
               lex%kind=tok_error
               call lex%error%set('Invalid or unhandled escape sequence')
         end select
      end if
    end subroutine x_backslash

    subroutine x_continuation
      if (eof) then
         lex%kind=tok_error
         call lex%error%set('EOF in line continuation')
      else
         select case(c)
            case (' ',tab)
            case default
               q=q_quoted
               back=.true.
         end select
      end if
    end subroutine x_continuation

    subroutine x_continuation_cr
      if (eof) then
         lex%kind=tok_error
         call lex%error%set('EOF in line continuation with CR')
      else
         select case(c)
            case (lf)
               q=q_continuation
            case default
               lex%kind=tok_error
               call lex%error%set('Invalid character in line continuation with CR')
         end select
      end if
    end subroutine x_continuation_cr

    subroutine x_decimal
      if (eof) then
         lex%kind=tok_error
         call lex%error%set('EOF in decimal escape sequence')
      else
         select case(c)
            case ('0':'9')
               x=10*x+(ichar(c)-48)
               d=d+1
               if (d==3) then
                  call append_atom(achar(x))
                  q=q_quoted
               end if
            case default
               lex%kind=tok_error
               call lex%error%set('Invalid digit in decimal escape')
         end select
      end if
    end subroutine x_decimal
  end subroutine read_token
  
end module sexptran
