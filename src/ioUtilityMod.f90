module ioUtilityMod
contains


  ! ====================================================== !
  ! === load parameters from file                      === !
  ! ====================================================== !
  subroutine load__parameters
    use variablesMod
    implicit none
    character(cLen)             :: cmt

    ! ------------------------------------------------------ !
    ! --- [1] load parameter file                        --- !
    ! ------------------------------------------------------ !
    open(lun,file=trim(prmFile),form='formatted')
    read(lun,*) cmt, cmt, ring_xc
    read(lun,*) cmt, cmt, ring_yc
    read(lun,*) cmt, cmt, ring_zc
    read(lun,*) cmt, cmt, ring_r0
    read(lun,*) cmt, cmt, ring_I0
    close(lun)
    ! ------------------------------------------------------ !
    ! --- [2] check parameters                           --- !
    ! ------------------------------------------------------ !
    write(6,'(a)'                 ) '[load__parameters@ioUtilityMod] check parameters '
    write(6,'(6x,a12,a4,1x,f10.5)') 'ring_xc', ' :: ', ring_xc
    write(6,'(6x,a12,a4,1x,f10.5)') 'ring_yc', ' :: ', ring_yc
    write(6,'(6x,a12,a4,1x,f10.5)') 'ring_zc', ' :: ', ring_zc
    write(6,'(6x,a12,a4,1x,f10.5)') 'ring_r0', ' :: ', ring_r0
    write(6,'(6x,a12,a4,1x,f10.5)') 'ring_I0', ' :: ', ring_I0
    write(6,'(a)'                 ) '[load__parameters@ioUtilityMod] check parameters '    
    return
  end subroutine load__parameters
  

  ! ====================================================== !
  ! === load bfield position from file                 === !
  ! ====================================================== !
  subroutine load__bfieldposition
    use variablesMod
    implicit none
    integer :: iB

    ! ------------------------------------------------------ !
    ! --- [1] preparation                                --- !
    ! ------------------------------------------------------ !
    call countLines( trim(bptFile), nBpt )
    allocate( BField(6,nBpt) )
    BField(:,:) = 0.d0
    ! ------------------------------------------------------ !
    ! --- [2] load bfieldposition                        --- !
    ! ------------------------------------------------------ !
    open(lun,file=trim(bptFile),status='old',form='formatted')
    read(lun,*)
    do iB=1, nBpt
       read(lun,*) BField(1:3,iB)
    enddo
    close(lun)
    return
  end subroutine load__bfieldposition


  ! ====================================================== !
  ! === save calculated bfield results into file       === !
  ! ====================================================== !
  subroutine save__results
    use variablesMod
    implicit none
    integer         :: iB
    character(cLen) :: fmt = '(6(e12.5,1x))'
    
    open(lun,file=trim(outFile),form='formatted')
    write(lun,*) '# xp yp zp bx by bz'
    do iB=1, nBpt
       write(lun,trim(fmt)) BField(:,iB)
    enddo
    close(lun)
    write(6,*) '[save__results]  results saved in... ', trim(outFile)
    
    return
  end subroutine save__results


  ! ====================================================== !
  ! === count up Number of Lines in the file           === !
  ! ====================================================== !
  subroutine countLines( FileName, count )
    implicit none
    integer     , intent(out) :: count
    character(*), intent(in)  :: FileName
    integer                   :: ios     = 0
    integer     , parameter   :: lun     = 50
    integer     , parameter   :: cLenMax = 50000
    character(1), parameter   :: comment = '#'
    character(1)              :: topchar
    character(cLenMax)        :: buffer

    count = 0
    open(lun,file=trim(FileName),status='old')
    do
       read(lun,*,iostat=ios) buffer
       topchar = ( adjustL( buffer ) )
       if ( topchar.eq.comment ) cycle
       if ( ios.lt.0 ) exit
       count = count + 1
    enddo
    close(lun)
    return
  end subroutine countLines

  
end module ioUtilityMod
