! Example program for the SEXPTRAN library
!
! Usage: simplest_example calendar.sexp
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

program simplest_example
  use sexptran
  implicit none

  class(sexp),pointer :: cal,rep
  type(error_status) :: err
  character(len=:), allocatable :: name
  integer, parameter :: month=11

  cal=>sexp_load('calendar.sexp',err)
  call get_value(nth(field(cal,'months'),month),name)
  call err%check
  print *,'Month name: ',name

  rep=>tuple(atom('calendar_analysis_report'), &
         tuple(pair('month',atom(month)), &
               pair('name',atom(name)), &
               pair('input_calendar',cal)))
  call rep%save('report.sexp',err)
  call err%check
  
  deallocate(cal,rep,name)
end program simplest_example
