      program im3d
      implicit real*8(a-h,o-z), integer*4(i-n)
      dimension a(9), b(9)
      open(1, file='info.dat')
c
      write(6, *) 'enter a(1)-a(9) values for inverse matrix calculation'
      read(5, *) (a(i), i=1,9)
      d1 = a(1) * a(5) * a(9) + a(2) * a(6) * a(7) + a(3) * a(4) * a(8)
      d2 = a(1) * a(6) * a(8) + a(2) * a(4) * a(9) + a(3) * a(5) * a(7)
      d = d1 - d2
c
      if(d.eq.0.0d0) then
      write(6, *) 'This matrix does not have inverse matrix'
      close(1)
      stop
      end if
c
      b(1) = a(5) * a(9) - a(6) * a(8)
      b(2) =- (a(2) * a(9) - a(3) * a(8))
      b(3) = a(2) * a(6) - a(3) * a(5)
      b(4) =- (a(4) * a(9) - a(6) * a(7))
      b(5) = a(1) * a(9) - a(3) * a(7)
      b(6) =- (a(1) * a(6) - a(3) * a(4))
      b(7) = a(4) * a(8) - a(5) * a(7)
      b(8) =- (a(1) * a(8) - a(2) * a(7))
      b(9) = a(1) * a(5) - a(2) * a(4)
c
      do 111 i = 1, 9
      b(i) = b(i) / d
  111 continue
c
      do 222 i = 1, 3
      write(1, 200) b(3 * i - 2), b(3 * i - 1), b(3 * i)
  200 format(3(3x, e24.16))
  222 continue
c
      write(6, *) 'The calculation has been completed'
      close(1)
      end
