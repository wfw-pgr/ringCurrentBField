module ellipticIMod
contains


  !******************************************************
  !* Complete elliptic integral of the first and second *
  !* kind. The input parameter is xk, which should be   *
  !* between 0 and 1. Technique uses Gauss' formula for *
  !* the arithmogeometrical mean. e is a measure of the * 
  !* convergence accuracy. The returned values are e1,  *
  !* the elliptic integral of the first kind, and e2,   *
  !* the elliptic integral of the second kind.          *
  !* -------------------------------------------------- *
  !* Reference: Ball, algorithms for RPN calculators.   *
  !******************************************************
  Subroutine CElliptic(e,xk,e1,e2,n)  
    ! Label: 100
    real*8 e,xk,e1,e2,pi
    real*8  A(0:99), B(0:99)
    integer j,m,n
    pi = 4.d0*datan(1.d0)
    A(0)=1.d0+xk ; B(0)=1.d0-xk
    n=0
    if (xk < 0.d0) return
    if (xk > 1.d0) return
    if (e <= 0.d0) return
100 n = n + 1
    ! Generate improved values
    A(n)=(A(n-1)+B(n-1))/2.d0
    B(n)=dsqrt(A(n-1)*B(n-1))
    if (dabs(A(n)-B(n)) > e) goto 100
    e1=pi/2.d0/A(n)
    e2=2.d0
    m=1
    do j = 1, n 
       e2=e2-m*(A(j)*A(j)-B(j)*B(j))
       m=m*2
    end do
    e2 = e2*e1/2.d0
    return
  end Subroutine CElliptic

end module ellipticIMod
