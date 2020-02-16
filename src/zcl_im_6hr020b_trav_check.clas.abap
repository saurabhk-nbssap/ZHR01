class ZCL_IM_6HR020B_TRAV_CHECK definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6HR020B_TRAV_CHECK
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_TRIP_WEB_CHECK .
protected section.
*"* protected components of class ZCL_IM_6HR020B_TRAV_CHECK
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6HR020B_TRAV_CHECK
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6HR020B_TRAV_CHECK IMPLEMENTATION.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ADVANCES.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_CHANGES.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_DEDUCTIONS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_GENERAL_DATA.
  data : wa_return type BAPIRET2.
  data : lv_days type i.
  DATA : LAST_DATE TYPE SY-DATUM.
  data : lv_ergru type p0017-ergru.
  data : V_SUM_REIMBU  type ptrv_shdr-SUM_REIMBU.
  DATA : LV_MONATE TYPE KOMP-ANZ_MONATE.
*** Travel Request
  if general_data-schem EQ 'PL'.
      select single ergru from pa0017 into lv_ergru
                         where pernr eq employeenumber
                           and begda le sy-datum
                           and endda ge sy-datum.

      if lv_ergru eq '1'.
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'You are not authorized to submit this travel '.
        wa_return-message_v2 = 'Request'.
        append wa_return to return.
        clear wa_return.

      endif.
    if general_data-datv1 lt sy-datum.
      wa_return-type = 'E'.
      WA_RETURN-ID = 'ZHR01'.
      WA_RETURN-NUMBER = '000'.
      wa_return-message = 'You are not allowed to submit Past Travel Requests.'.
      wa_return-MESSAGE_V1 = 'You are not allowed to submit Past Travel Requests.'.
      append wa_return to return.
      clear wa_return.
    endif.
  Else.

*** Validations for Travel Expenses Report Claims
    if general_data-datb1 ge sy-datum.
      wa_return-type = 'E'.
      WA_RETURN-ID = 'ZHR01'.
      WA_RETURN-NUMBER = '000'.
      wa_return-message = 'You are not allowed to submit Future Travel expenses.'.
      wa_return-MESSAGE_V1 = 'You are not allowed to submit Future Travel'.
      wa_return-message_v2 = ' expenses.'.
      append wa_return to return.
      clear wa_return.
    endif.
    if general_data-datv1 ge sy-datum.
      wa_return-type = 'E'.
      WA_RETURN-ID = 'ZHR01'.
      WA_RETURN-NUMBER = '000'.
      wa_return-message = 'You are not allowed to submit Future Travel expenses.'.
      wa_return-MESSAGE_V1 = 'You are not allowed to submit Future Travel'.
      wa_return-message_v2 = ' expenses.'.
      append wa_return to return.
      clear wa_return.

    ELSE.

      CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES_NEW'
        EXPORTING
          I_DATUM_BIS       = SY-DATUM
          I_DATUM_VON       = general_data-datv1
          I_KZ_INCL_BIS     = ' '
          I_KZ_VOLLE_MONATE = ' '
        IMPORTING
          E_MONATE          = LV_MONATE.
      lv_days = SY-DATUM - general_data-datv1.
      IF lv_days GT 45.
*        wa_return-type = 'E'.
*        WA_RETURN-ID = 'ZHR01'.
*        WA_RETURN-NUMBER = '000'.
*        wa_return-MESSAGE_V1 = 'You are not allowed to submit this claim'.
*
*        append wa_return to return.
*        clear wa_return.
      ENDIF.
    endif.
    if  general_data-schem eq '01'.


      SELECT SINGLE SUM_REIMBU FROM PTRV_SHDR INTO V_SUM_REIMBU
                                    WHERE PERNR = GENERAL_DATA-PERNR
                                      AND REINR = GENERAL_DATA-REINR
                                      and perio = general_data-perio.
      if   V_SUM_REIMBU gt 20000.
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'Claim amount should not be more than Rs 20000'.

        append wa_return to return.
        clear wa_return.
      endif.

      select single ergru from pa0017 into lv_ergru
                         where pernr eq employeenumber
                           and begda le sy-datum
                           and endda ge sy-datum.

      if lv_ergru eq '1'.
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'You are not authorized to submit this expense'.

        append wa_return to return.
        clear wa_return.

      endif.
      CALL FUNCTION 'LAST_DAY_OF_MONTHS'
        EXPORTING
          DAY_IN                  = GENERAL_DATA-DATV1
       IMPORTING
          LAST_DAY_OF_MONTH       = LAST_DATE
*         EXCEPTIONS
*           DAY_IN_NO_DATE          = 1
*           OTHERS                  = 2
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      if general_data-datv1+6(2) eq '01'.

        concatenate general_data-datv1+0(6) '15' into general_data-datb1.
      endif.

      if general_data-datv1+6(2) eq '16'.
        concatenate general_data-datv1+0(6) last_date+6(2) into general_data-datb1.
      endif.
      if general_data-datv1+6(2) eq '01' and general_data-datb1+6(2) ne '15'.
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
        append wa_return to return.
        clear wa_return.
      elseif general_data-datv1+6(2) eq '16' and general_data-datb1+6(2) ne LAST_DATE+6(2).
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
        append wa_return to return.
        clear wa_return.
      elseif general_data-datv1+6(2) ne '01' and   general_data-datv1+6(2) ne '16'.
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
        append wa_return to return.
        clear wa_return.
      Elseif general_data-datB1+6(2) ne '15' and  general_data-datB1+6(2) ne LAST_DATE+6(2).
        wa_return-type = 'E'.
        WA_RETURN-ID = 'ZHR01'.
        WA_RETURN-NUMBER = '000'.
        wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
        append wa_return to return.
        clear wa_return.
      endif.
    elseif general_data-schem eq '02'.
      select single ergru from pa0017 into lv_ergru
                          where pernr eq employeenumber
                            and begda le sy-datum
                            and endda ge sy-datum.
      if lv_ergru eq '1'.
        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
EXPORTING
DAY_IN                  = GENERAL_DATA-DATV1
IMPORTING
LAST_DAY_OF_MONTH       = LAST_DATE
*         EXCEPTIONS
*           DAY_IN_NO_DATE          = 1
*           OTHERS                  = 2
.
        IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        if general_data-datv1+6(2) eq '01'.

          concatenate general_data-datv1+0(6) '15' into general_data-datb1.
        endif.

        if general_data-datv1+6(2) eq '16'.
          concatenate general_data-datv1+0(6) last_date+6(2) into general_data-datb1.
        endif.


        if general_data-datv1+6(2) eq '01' and general_data-datb1+6(2) ne '15'.
          wa_return-type = 'E'.
          WA_RETURN-ID = 'ZHR01'.
          WA_RETURN-NUMBER = '000'.
          wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
          append wa_return to return.
          clear wa_return.
        elseif general_data-datv1+6(2) eq '16' and general_data-datb1+6(2) ne LAST_DATE+6(2).
          wa_return-type = 'E'.
          WA_RETURN-ID = 'ZHR01'.
          WA_RETURN-NUMBER = '000'.
          wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
          append wa_return to return.
          clear wa_return.
        elseif general_data-datv1+6(2) ne '01' and   general_data-datv1+6(2) ne '16'.
          wa_return-type = 'E'.
          WA_RETURN-ID = 'ZHR01'.
          WA_RETURN-NUMBER = '000'.
          wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
          append wa_return to return.
          clear wa_return.
        Elseif general_data-datB1+6(2) ne '15' and  general_data-datB1+6(2) ne LAST_DATE+6(2).
          wa_return-type = 'E'.
          WA_RETURN-ID = 'ZHR01'.
          WA_RETURN-NUMBER = '000'.
          wa_return-MESSAGE_V1 = 'Only forthnight claims are allowed'.
          append wa_return to return.
          clear wa_return.
        endif.
      endif.
    endif.
  endif.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ITINERARY.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ITIN_COSTS_SPLIT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_ADVANCES.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_CCC_RECEIPT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_DEDUCTIONS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_ITINERARY.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_MILEAGE.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_RECEIPTS.
  data : wa_T706S_RECEIPT type T706S_RECEIPT.
  data  : wa_return type BAPIRET2.
  select single * from T706S_RECEIPT into wa_T706S_RECEIPT
                           where morei eq general_data-morei
                             and schem eq general_data-schem
                             and spkzl eq receipt-spkzl
                             and begda le receipt-bldat
                             and endda ge receipt-bldat.
  if sy-subrc ne 0.
      wa_return-type = 'E'.
      WA_RETURN-ID = 'ZHR01'.
      WA_RETURN-NUMBER = '001'.
      WA_RETURN-MESSAGE = 'This Expense Type is not allowed '.
      wa_return-message_v1 = 'This Expense Type is not allowed '.
      append wa_return to return.
      clear wa_Return.

  endif.
  if general_data-schem eq '01'.
     receipt-PAYOT = 'CC'.
  ELSEIF general_data-schem eq '02'.
      receipt-PAYOT = 'TC'.
  ENDIF.

endmethod.


  method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_TRANSPORT.
  endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILEAGE.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILE_COSTS_SPLIT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_RECEIPTS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_RECE_COSTS_SPLIT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TEXT.
endmethod.


  method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TRANSPORTS.
  endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TRIP_COSTS_SPLIT.
endmethod.
ENDCLASS.
