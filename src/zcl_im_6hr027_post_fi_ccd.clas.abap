class ZCL_IM_6HR027_POST_FI_CCD definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6HR027_POST_FI_CCD
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_TRIP_POST_FI .
protected section.
*"* protected components of class ZCL_IM_6HR027_POST_FI_CCD
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6HR027_POST_FI_CCD
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6HR027_POST_FI_CCD IMPLEMENTATION.


  method IF_EX_TRIP_POST_FI~DETERMINE_SRFC.
  endmethod.


method IF_EX_TRIP_POST_FI~EXA706K.
endmethod.


method IF_EX_TRIP_POST_FI~EXB706K.
endmethod.


method IF_EX_TRIP_POST_FI~EX_CCARD.
endmethod.


method IF_EX_TRIP_POST_FI~EX_SGTXT.
endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP.
endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP_ACCOUNT1.

endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP_ACCOUNT2.

endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP_COMPLETE.
*  DATA : WA_ZWEP TYPE PTRV_ZWEP,
*         WA1_ZWEP TYPE PTRV_ZWEP,
*         WA2_ZWEP TYPE PTRV_ZWEP.
*  DATA : IT_ZWEP TYPE TAB_PTRV_ZWEP,
*         IK_ZWEP TYPE TAB_PTRV_ZWEP,
*         LV_TABIX TYPE SY-TABIX.
*  DATA : LV_LGART TYPE PA0008-LGA01.
*  DATA : WA_SETLEAF TYPE SETLEAF.
*  BREAK IBMABAP01.
*  loop at zwep into wa_zwep WHERE KOMOK_GEGEN EQ '31'.
*
*
*      SELECT SINGLE * FROM SETLEAF INTO WA_SETLEAF WHERE SETCLASS = '0000'
*                                      AND SUBCLASS = SPACE
*                                      AND SETNAME = 'ZLGART'
*                                      AND VALFROM = WA_ZWEP-LGART.
*      IF SY-SUBRC EQ 0.
*          WA_ZWEP-KOMOK_GEGEN = '39'.
*          MODIFY ZWEP FROM WA_ZWEP.
*          CLEAR WA_ZWEP.
*      ENDIF.
*
*  endloop.
**  LOOP AT ZWEP INTO WA_ZWEP .
**  LV_TABIX = SY-TABIX.
**    IF WA_ZWEP-KTOSL EQ 'HRP'.
**      APPEND WA_ZWEP TO IK_ZWEP.
**      DELETE ZWEP INDEX LV_TABIX.   .
**      CLEAR WA_ZWEP.
**    ELSE.
***      APPEND WA_ZWEP TO IT_ZWEP.
***      CLEAR WA_ZWEP.
**    ENDIF.
**  ENDLOOP.
**  LOOP AT IK_ZWEP INTO WA_ZWEP.
**      LOOP AT ZWEP INTO WA1_ZWEP WHERE REINR = WA_ZWEP-REINR
**                                   AND PERNR = WA_ZWEP-PERNR.
**        MOVE-CORRESPONDING WA_ZWEP TO WA2_ZWEP.
**        MOVE : WA1_ZWEP-BETRG TO WA2_ZWEP-BETRG.
***        WA2_ZWEP-BETRG = WA2_ZWEP-BETRG * -1.
**        APPEND WA2_ZWEP TO IT_ZWEP.
**        CLEAR WA2_ZWEP.
**        CLEAR WA1_ZWEP.
**
**      ENDLOOP.
**      CLEAR WA_ZWEP.
**  ENDLOOP.
**  CLEAR ZWEP.
**  APPEND LINES OF IT_ZWEP TO ZWEP.
**  APPEND LINES OF IK_ZWEP TO ZWEP.
**  LOOP AT ZWEP INTO WA_ZWEP.
**      WA_ZWEP-EP_LINE = SY-TABIX.
**      MODIFY ZWEP FROM WA_ZWEP.
**      CLEAR WA_ZWEP.
**  ENDLOOP.
**  SORT ZWEP BY EP_LINE.
endmethod.


method IF_EX_TRIP_POST_FI~FILL_EXT_RAWDATA.
endmethod.


method IF_EX_TRIP_POST_FI~MODIFY_PTRV_DOC_HD.

  ptrv_doc_hd-budat = sy-datum.
  ptrv_doc_hd-bldat = sy-datum.
endmethod.


method IF_EX_TRIP_POST_FI~MODIFY_PTRV_DOC_IT.
*  break-point.
endmethod.


method IF_EX_TRIP_POST_FI~MODIFY_PTRV_DOC_TAX.
endmethod.
ENDCLASS.
