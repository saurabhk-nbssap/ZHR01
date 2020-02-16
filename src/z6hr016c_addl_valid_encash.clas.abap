class Z6HR016C_ADDL_VALID_ENCASH definition
  public
  final
  create public .

*"* public components of class Z6HR016C_ADDL_VALID_ENCASH
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_HRPBSIN_VALIDATE_REQ .
protected section.
*"* protected components of class Z6HR016C_ADDL_VALID_ENCASH
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR016C_ADDL_VALID_ENCASH
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR016C_ADDL_VALID_ENCASH IMPLEMENTATION.


method IF_HRPBSIN_VALIDATE_REQ~VALIDATE_REQ.
  TYPES : BEGIN OF S_2001 ,
         PERNR TYPE PERSNO,
         SUBTY TYPE SUBTY,
         BEGDA TYPE BEGDA,
         ENDDA TYPE ENDDA,
         KALTG TYPE KALTG,
         GJAHR TYPE GJAHR,
          END OF S_2001.
  DATA : LV_KALTG TYPE PS2001-KALTG.
  TYPES : BEGIN OF S_0416 ,
         PERNR TYPE PERSNO,
         SUBTY TYPE SUBTY,
         BEGDA TYPE BEGDA,
         ENDDA TYPE ENDDA,
         KALTG TYPE KALTG,
         GJAHR TYPE GJAHR,
          END OF S_0416.

  DATA : IT_2001 TYPE  TABLE OF S_2001.
  DATA : WA_2001 TYPE S_2001.
  DATA : IT_0416 TYPE  TABLE OF S_0416.
  DATA : WA_0416 TYPE S_0416.
  data : v_count type i.
  DATA : WA_RETURN TYPE BAPIRET2.
  data : ldays(15).
  data : lint(15), ldec(4).
  TYPES:  I0019 TYPE P0019.
  data : IT_0019 TYPE TABLE OF I0019.

  if leave_en_req-leave_type ne '01'.
    IS_VALIDATED = ' '.
        CLEAR WA_RETURN.
        WA_RETURN-TYPE = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        WA_RETURN-MESSAGE = 'Leave Encashment facility Allowed only for PL '.
        append wa_return to error_tab.
        clear wa_return.


  ENDIF.

  ldays = leave_en_req-ENCASHED_DAYS.
  condense ldays no-gaps.
  split ldays at '.' into lint ldec.

  if ldec+0(1) ne 0.
    IS_VALIDATED = ' '.
        CLEAR WA_RETURN.
        WA_RETURN-TYPE = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        WA_RETURN-MESSAGE = 'Decimals not Allowed'.
        append wa_return to error_tab.
        clear wa_return.

  endif.
  if leave_en_req-LEAVE_TYPE = '01'.
    if emp_grp eq 'M' OR emp_grp eq 'N' OR emp_grp eq 'D'.
      SELECT *  FROM PA2001 INTO CORRESPONDING FIELDS OF TABLE IT_2001
                     WHERE PERNR EQ PERNR
                       AND SUBTY EQ 'PL'
                       AND KALTG GE 4.

      LOOP AT IT_2001 INTO WA_2001.
        WA_2001-GJAHR = wa_2001-ENDDA+0(4).
        MODIFY IT_2001 FROM WA_2001.
        CLEAR WA_2001.
      ENDLOOP.
      READ TABLE IT_2001 INTO WA_2001 WITH KEY PERNR = PERNR
                                               SUBTY = 'PL'
                                               gjahr =  sy-datum+0(4).
      IF SY-SUBRC NE 0.

        IS_VALIDATED = ' '.
        CLEAR WA_RETURN.
        WA_RETURN-TYPE = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        WA_RETURN-MESSAGE = 'You are not Eligible for Leave Encashment'.
        append wa_return to error_tab.
        clear wa_return.
      ELSE.
        if leave_en_req-ENCASHED_DAYS ge 15 and leave_en_req-ENCASHED_DAYS le 30.
          SELECT *  FROM PA0416 INTO CORRESPONDING FIELDS OF TABLE IT_0416
                  WHERE PERNR EQ PERNR
                    AND SUBTY EQ '1000'.

          clear v_count .
          LOOP AT IT_0416 INTO WA_0416 where endda+0(4) eq sy-datum+0(4).
            v_count = v_count + 1.
          ENDLOOP.
*          if v_count gt 1.
*            CLEAR WA_RETURN.
*            WA_RETURN-TYPE = 'E'.
*            WA_RETURN-ID = 'ZHR01'.
*            WA_RETURN-NUMBER = '000'.
*            WA_RETURN-MESSAGE = 'Only One Leave Encashment Request in a Calendar Year'.
*            append wa_return to error_tab.
*            clear wa_return.
*          endif.
        else.
          CLEAR WA_RETURN.
          WA_RETURN-TYPE = 'E'.
          WA_RETURN-ID = 'ZHR01'.
          WA_RETURN-NUMBER = '000'.
          WA_RETURN-MESSAGE = 'Minimum Days of Encashment is 15 days and Maximum is 30 days'.
          append wa_return to error_tab.
          clear wa_return.
        endif.
      ENDIF.
    elseif emp_grp eq 'S' OR emp_grp eq 'W'  .
*
*      ELSE.
      if leave_en_req-ENCASHED_DAYS ge 12 and leave_en_req-ENCASHED_DAYS le 30.

        SELECT *  FROM PA2001 INTO CORRESPONDING FIELDS OF TABLE IT_2001
                WHERE PERNR EQ PERNR
                  AND SUBTY EQ 'PL'.
        CLEAR LV_KALTG.
        LOOP AT IT_2001 INTO WA_2001 WHERE ENDDA+0(4) EQ sy-datum+0(4).
          LV_KALTG = LV_KALTG + WA_2001-KALTG.
        ENDLOOP.
        IF LV_KALTG lt leave_en_req-ENCASHED_DAYS.

          IS_VALIDATED = ' '.
          CLEAR WA_RETURN.
          WA_RETURN-TYPE = 'E'.
          WA_RETURN-ID = 'ZHR01'.
          WA_RETURN-NUMBER = '000'.
          WA_RETURN-MESSAGE = 'You are not Eligible for Leave Encashment'.
          append wa_return to error_tab.
          clear wa_return.
        ELSE.
          SELECT *  FROM PA0416 INTO CORRESPONDING FIELDS OF TABLE IT_0416
                  WHERE PERNR EQ PERNR
                    AND SUBTY EQ '1000'.

          clear v_count .
          LOOP AT IT_0416 INTO WA_0416 where endda+0(4) eq sy-datum+0(4).
            v_count = v_count + 1.
          ENDLOOP.
          if v_count gt 1.
            CLEAR WA_RETURN.
            WA_RETURN-TYPE = 'E'.
            WA_RETURN-ID = 'ZHR01'.
            WA_RETURN-NUMBER = '000'.
            WA_RETURN-MESSAGE = 'Only One Leave Encashment Request in a Calendar Year'.
            append wa_return to error_tab.
            clear wa_return.
          endif.
        ENDIF.
      else.
        CLEAR WA_RETURN.
        WA_RETURN-TYPE = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        WA_RETURN-MESSAGE = 'Minimum Days of Encashment is 15 days and Maximum is 30 days'.
        append wa_return to error_tab.
        clear wa_return.
      endif.
    ENDIF.



  ELSE.
    CLEAR WA_RETURN.
    WA_RETURN-TYPE = 'E'.
    WA_RETURN-ID = 'ZHR01'.
    WA_RETURN-NUMBER = '000'.
    WA_RETURN-MESSAGE = 'Leave Encashment is possilbe only for Leave Type PL'.
    append wa_return to error_tab.
    clear wa_return.

  ENDIF.
endmethod.
ENDCLASS.
