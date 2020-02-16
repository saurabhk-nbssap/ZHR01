class ZCL_IM_6HR028B_SALARY_STMT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6HR028B_SALARY_STMT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_XSS_REM_INTERFACE .
protected section.
*"* protected components of class ZCL_IM_6HR028B_SALARY_STMT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6HR028B_SALARY_STMT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6HR028B_SALARY_STMT IMPLEMENTATION.


method IF_EX_XSS_REM_INTERFACE~DEFINE_PRINTBUTTON_STATUS.
endmethod.


method IF_EX_XSS_REM_INTERFACE~PROVIDE_DROPDOWN_ENTRIES.
endmethod.


method IF_EX_XSS_REM_INTERFACE~PROVIDE_FILTERED_RGDIR.
data : wa_pc261 type PC261.

 DATA: newest_date LIKE sy-datum.
 DATA : WA_PARAMS TYPE Z6MMA_PARAMS.
  ex_filtered_rgdir[] = im_in_rgdir[].
  CLEAR ex_message.

* remove all in-periods where not:
*  a) paydt is at the past
*  b) paydt is in the near future (3 days)
  newest_date = sy-datum + 3.
  DELETE ex_filtered_rgdir WHERE INPER LT '201007'.
  LOOP AT ex_filtered_rgdir INTO wa_pc261.

  SELECT SINGLE * FROM Z6MMA_PARAMS INTO WA_PARAMS WHERE PROGNAME EQ 'SALARY'
                                      AND  param1 eq wa_pc261-abkrs
                                      and PARAM2 EQ  WA_PC261-inper
                                      AND PARAMVAL eq SPACE.
  IF SY-SUBRC NE 0.
    DELETE EX_FILTERED_RGDIR INDEX SY-TABIX.
  ENDIF.

  ENDLOOP.
*loop at im_in_rgdir into wa_pc261.
*if wa_pc261-inper lt '201007'.
*  append wa_pc261 to ex_filtered_rgdir.
*  clear wa_pc261.
*
*endif.
*endloop.
endmethod.


method IF_EX_XSS_REM_INTERFACE~PROVIDE_OVERVIEWTAB_FIELDCAT.
endmethod.


method IF_EX_XSS_REM_INTERFACE~PROVIDE_OVERVIEWTAB_LINE.
endmethod.


method IF_EX_XSS_REM_INTERFACE~PROVIDE_SRVDESCRIPTION.
endmethod.
ENDCLASS.
