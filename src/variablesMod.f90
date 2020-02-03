module variablesMod
  implicit none
  integer                       :: nBpt
  double precision              :: ring_xc, ring_yc, ring_zc, ring_r0, ring_I0, mu_0
  integer         , parameter   :: x_=1, y_=2, z_=3
  double precision, allocatable :: bfield(:,:)
  integer         , parameter   :: cLen    = 300
  integer         , parameter   ::  lun    =  50
  character(cLen)               :: prmFile = 'dat/parameter.conf'
  character(cLen)               :: outFile = 'dat/ringCurrentBField.dat'
  character(cLen)               :: bptFile = 'dat/bfieldCoordinate.dat'
  
end module variablesMod
