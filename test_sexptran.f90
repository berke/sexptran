! Test program for the SEXPTRAN library
! 
! Usage: test_sexptran config.sexp
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

program test_sexptran
  use sexptran
  use iso_fortran_env
  implicit none

  integer, parameter :: dp=kind(0.0d0)
  character(len=:),allocatable :: cfg_fn          ! Configuration file name
  integer :: m                                    ! Length of argument
  class(sexp),pointer :: cfg,grid,res             ! S-expressions for configuration, grid subexpression and result
  type(error_status) :: err                       ! Error handler
  character(len=:), allocatable :: out_fn         ! Output file name
  real(dp) :: x0,x1,x,y
  integer :: xi,nx,j
  real(dp), allocatable :: poly(:),xs(:),ys(:)

  write (*,'("SEXPTRAN test case")')

  ! Get the first command line argument, which gives the name of the
  ! configuation file
  call get_command_argument(1,length=m)  ! First call to get length
  if (m==0) stop 'Missing argument: Configuration file name'
  allocate(character(len=m) :: cfg_fn)   ! Allocate character string of proper length
  call get_command_argument(1,cfg_fn)    ! Second call to get the argument

  write (*,'("Configuration:",T20,A)') cfg_fn

  cfg=>sexp_load(cfg_fn,err)                ! Load an S-expression from the configuration file
  call err%check                            ! Stop on error - it will already have been printed

  ! Extract computation parameters
  grid=>field(cfg,'grid')                   ! Find the grid
  call err%check                            ! If not found, grid will be null so check error
  call get_value(field(grid,'x0'),x0)       ! Get field values via overloaded get_value() function
  call get_value(field(grid,'x1'),x1)
  call get_value(field(grid,'nx'),nx)
  call get_value(field(cfg,'polynomial'),poly) ! Get polynomial coefficients
  call get_value(field(cfg,'output'),out_fn)   ! Get output file - will be allocated by get_valu()
  call err%check                               ! Check for any errors so far

  ! Write summary of configuration
  write (*,'("Grid:",T20,EN20.12,1X,EN20.12,1X,I8)') x0,x1,nx
  write (*,'("Output file:",T20,A)') out_fn
  write (*,'("Polynomial:",T20,99EN20.12)') poly

  ! Perform computation.  It's a simple polynomial evaluation
  allocate(xs(nx),ys(nx))
  do xi=1,nx
     xs(xi)=x0+(xi-1)*(x1-x0)/(nx-1)
     x=1
     y=0
     do j=1,size(poly)
        y=y+x*poly(j)
        x=x*xs(xi)
     end do
     ys(xi)=y
  end do

  ! Create result S-expression using constructor functions
  res=>tuple( &
       pair('grid',grid), &
       pair('xs',list(xs)), &
       pair('ys',list(ys)), &
       pair('version',atom('1.46beta')))
  ! Save S-expression and check error
  call res%save(out_fn,err)
  call err%check

  deallocate(res,poly,xs,ys,cfg,cfg_fn,out_fn)
end program test_sexptran
