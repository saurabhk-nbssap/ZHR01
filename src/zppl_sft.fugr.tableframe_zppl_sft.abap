*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPPL_SFT
*   generation date: 28.01.2013 at 11:41:06 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPPL_SFT           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
