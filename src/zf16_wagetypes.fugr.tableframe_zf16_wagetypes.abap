*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZF16_WAGETYPES
*   generation date: 16.04.2011 at 10:38:52 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZF16_WAGETYPES     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
