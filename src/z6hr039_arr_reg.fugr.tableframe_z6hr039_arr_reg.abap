*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z6HR039_ARR_REG
*   generation date: 12.01.2013 at 12:47:10 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z6HR039_ARR_REG    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
