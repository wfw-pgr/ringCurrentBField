import numpy                   as np
import nkUtilities.LoadConst   as lcn
import genGrid.EquiSpaceGrid3D as esg

# ========================================================= #
# ===  generate Coordinate                              === #
# ========================================================= #
def genCoordinate():
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    outFile     = "dat/bfieldCoordinate.dat"
    prmFile     = "dat/bfieldcoordinate.conf"
    const       = lcn.LoadConst( inpFile=prmFile )
    # ------------------------------------------------- #
    # --- [2] generate Coordinate                   --- #
    # ------------------------------------------------- #
    x1MinMaxNum = [ const["x1Min"], const["x1Max"], const["L1"] ]
    x2MinMaxNum = [ const["x2Min"], const["x2Max"], const["L2"] ]
    x3MinMaxNum = [ const["x3Min"], const["x3Max"], const["L3"] ]
    xg1,xg2,xg3 = esg.EquiSpaceGrid3D( x1MinMaxNum=x1MinMaxNum, x2MinMaxNum=x2MinMaxNum, \
                                       x3MinMaxNum=x3MinMaxNum )
    wData       = np.zeros( (const["L1"]*const["L2"]*const["L3"],3) )
    wData[:,0]  = xg1.reshape( (-1,) )
    wData[:,1]  = xg2.reshape( (-1,) )
    wData[:,2]  = xg3.reshape( (-1,) )
    # ------------------------------------------------- #
    # --- [3] write Data into File                  --- #
    # ------------------------------------------------- #
    with open( outFile, "w" ) as f:
        f.write( "# xb yb zb\n" )
        np.savetxt( f, wData )


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    import myUtils.genArgs as gar
    args    = gar.genArgs()
    genCoordinate()
