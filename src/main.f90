program main
  use variablesMod
  use ioUtilityMod, only : load__parameters, load__bfieldposition, save__results
  use bfd_ringIMod, only : calc__ringCurrentBField
  implicit none

  call load__parameters
  call load__bfieldposition
  call calc__ringCurrentBField( BField, ring_xc, ring_yc, ring_zc, ring_r0, ring_I0, nBpt )
  call save__results
  
end program main
