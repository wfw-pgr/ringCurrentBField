module shimFieldMod
contains
  
  ! ====================================================== !
  ! === calculate shim iron piece magnetic field       === !
  ! ====================================================== !
  subroutine calc__shimField( bfield, mvec, r1, r2, p1, p2, z1, z2, LI, LJ, LK, nB )
    implicit none
    integer         , intent(in)  :: LI, LJ, LK, nB
    double precision, intent(in)  :: r1, r2, p1, p2, z1, z2, mvec(3)
    double precision, intent(out) :: bfield(6,nB)
    integer                       :: i, j, k, iB
    double precision              :: dr, dp, dz, vol, rpzpos(3), shmpos(3)
    integer         , parameter   :: xp_=1, yp_=2, zp_=3, bx_=4, by_=5, bz_=6
    integer         , parameter   :: rp_=1, pp_=2
    
    dr  = ( r2 - r1 ) / dble( LI )
    dp  = ( p2 - p1 ) / dble( LJ )
    dz  = ( z2 - z1 ) / dble( LK )
    do k=1, LK
       do j=1, LJ
          do i=1, LI
             do iB=1, nB
                rpzpos(rp_)        = ( ( i-1 ) + 0.5d0 ) * dr + r1
                rpzpos(pp_)        = ( ( j-1 ) + 0.5d0 ) * dp + p1
                rpzpos(zp_)        = ( ( k-1 ) + 0.5d0 ) * dz + z1
                shmpos(xp_)        = rpzpos(rp_) * cos( rpzpos(pp_) )
                shmpos(yp_)        = rpzpos(rp_) * sin( rpzpos(pp_) )
                shmpos(zp_)        = rpzpos(zp_)
                vol                = dr * ( rpzpos(rp_) * dp ) * dz
                bfield(bx_:bz_,iB) = bfield(bx_:bz_,iB) &
                     &  + vol * (   MagneticMoment( shmpos(xp_:zp_), bfield(xp_:zp_,iB), mvec ) &
                     &            + MagneticMoment( shmpos(xp_:zp_), bfield(xp_:zp_,iB), mvec ) )
             enddo
          enddo
       enddo
    enddo
    
    return
  end subroutine calc__shimField

  
  ! ====================================================== !
  ! === calculate MagnetiMoment from position          === !
  ! ====================================================== !
  function MagneticMoment( shmpos, bfdpos, mvec )
    implicit none
    double precision, intent(in) :: shmpos(3), bfdpos(3), mvec(3)
    double precision             :: mdotr, rvec3Inv, rvec(3)
    double precision             :: MagneticMoment(3)
    double precision, parameter  :: fourpi = 16.d0*atan(1.d0)
    
    rvec(:)           = bfdpos(:) - shmpos(:)
    rvec3Inv          = 1.d0 / ( fourpi * sqrt( rvec(1)**2 + rvec(2)**2 + rvec(3)**2 )**3 )
    mdotr             = mvec(1) * rvec(1) + mvec(2) * rvec(2) + mvec(3) * rvec(3)
    MagneticMoment(:) = rvec3Inv * ( 3.d0 * mdotr * rvec(:) - mvec )
    return
  end function MagneticMoment

  
end module shimFieldMod
