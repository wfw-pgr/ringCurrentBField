module bfd_ringIMod
contains

  subroutine calc__ringCurrentBField( BField, ring_xc, ring_yc, ring_zc, ring_r0, ring_I0, nBpt )
    use ellipticIMod
    implicit none
    integer         , intent(in)    :: nBpt
    double precision, intent(in)    :: ring_xc, ring_yc, ring_zc, ring_r0, ring_I0
    double precision, intent(inout) :: BField(6,nBpt)
    double precision, parameter     :: mu0         = 16.d0*atan(1.d0)*1.d-7
    double precision, parameter     :: mu0_over_pi = 4.d-7
    double precision, parameter     :: eps = 1.d-10
    integer         , parameter     :: xp_=1, yp_=2, zp_=3, bx_=4, by_=5, bz_=6
    integer                         :: iB, nellip
    double precision                :: elip1, elip2
    double precision                :: xr, yr, zr
    double precision                :: rho, dist, alpha, beta, ksq, coef, denom1, denom2

    ! ------------------------------------------------------ !
    ! --- [1] ringCurrentBField                          --- !
    ! ------------------------------------------------------ !
    do iB=1, nBpt
       ! -- [1-1] relative position -- !
       xr     = BField(xp_,iB) - ring_xc
       yr     = BField(yp_,iB) - ring_yc
       zr     = BField(zp_,iB) - ring_zc
       ! -- [1-2] Coefficients      -- !
       rho    = sqrt( xr**2 + yr**2         )
       dist   = sqrt( xr**2 + yr**2 + zr**2 )
       if ( ( rho.gt.0.d0 ).and.( dist.gt.0.d0 ) ) then
          alpha  = sqrt( ring_r0**2 + dist**2 - 2.d0 * ring_r0 * rho )
          beta   = sqrt( ring_r0**2 + dist**2 + 2.d0 * ring_r0 * rho )
          ksq    = 1 - alpha**2 / beta**2
          coef   = mu0_over_pi * ring_I0
          denom1 = 1.d0 / ( 2.d0 * alpha**2 * beta * rho**2 )
          denom2 = 1.d0 / ( 2.d0 * alpha**2 * beta          )
          ! -- [1-3] B-Field           -- !
          call CElliptic( eps, ksq, elip1, elip2, nellip )
          BField(bx_,iB) = coef * xr * zr * denom1 &
               &   * ( ( ring_r0**2 + dist**2 ) * elip1 - alpha**2 * elip2 )
          BField(by_,iB) = coef * yr * zr * denom1 &
               &   * ( ( ring_r0**2 + dist**2 ) * elip1 - alpha**2 * elip2 )
          BField(bz_,iB) = coef           * denom2 &
               &   * ( ( ring_r0**2 - dist**2 ) * elip1 + alpha**2 * elip2 )
       else
          ! -- [1-4] r=0 or rho=0 case -- !
          BField(bx_,iB ) = 0.d0
          BField(by_,iB ) = 0.d0
          BField(bz_,iB ) = mu0 * ring_I0 * ring_r0**2 &
               &    / ( 2.d0 * ( ring_r0**2 + zr**2 )**1.5d0 )
       endif
    enddo
    return
  end subroutine calc__ringCurrentBField


  function elip1( ksq )
    implicit none
    double precision, intent(in) :: ksq
    double precision             :: elip1
    elip1 = 1.d0
    return
  end function elip1

  function elip2( ksq )
    implicit none
    double precision, intent(in) :: ksq
    double precision             :: elip2
    elip2 = 1.d0
    return
  end function elip2


end module bfd_ringIMod
