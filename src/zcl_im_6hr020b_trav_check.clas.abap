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

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_CHANGES.

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_DEDUCTIONS.
endmethod.


METHOD if_ex_trip_web_check~user_check_general_data.
  DATA : wa_return TYPE bapiret2.
  DATA : lv_days TYPE i.
  DATA : last_date TYPE sy-datum.
  DATA : lv_ergru TYPE p0017-ergru.
  DATA : v_sum_reimbu  TYPE ptrv_shdr-sum_reimbu.
  DATA : lv_monate TYPE komp-anz_monate.

  BREAK-POINT.

*** Travel Request
  IF general_data-schem EQ 'PL'.
    SELECT SINGLE ergru FROM pa0017 INTO lv_ergru
                       WHERE pernr EQ employeenumber
                         AND begda LE sy-datum
                         AND endda GE sy-datum.

    IF lv_ergru EQ '1'.
      wa_return-type = 'E'.
      wa_return-id = 'ZHR01'.
      wa_return-number = '000'.
      wa_return-message_v1 = 'You are not authorized to submit this travel '.
      wa_return-message_v2 = 'Request'.
      APPEND wa_return TO return.
      CLEAR wa_return.

    ENDIF.
    IF general_data-datv1 LT sy-datum.
      wa_return-type = 'E'.
      wa_return-id = 'ZHR01'.
      wa_return-number = '000'.
      wa_return-message = 'You are not allowed to submit Past Travel Requests.'.
      wa_return-message_v1 = 'You are not allowed to submit Past Travel Requests.'.
      APPEND wa_return TO return.
      CLEAR wa_return.
    ENDIF.
  ELSE.

*** Validations for Travel Expenses Report Claims
    IF general_data-datb1 GE sy-datum.
      wa_return-type = 'E'.
      wa_return-id = 'ZHR01'.
      wa_return-number = '000'.
      wa_return-message = 'You are not allowed to submit Future Travel expenses.'.
      wa_return-message_v1 = 'You are not allowed to submit Future Travel'.
      wa_return-message_v2 = ' expenses.'.
      APPEND wa_return TO return.
      CLEAR wa_return.
    ENDIF.
    IF general_data-datv1 GE sy-datum.
      wa_return-type = 'E'.
      wa_return-id = 'ZHR01'.
      wa_return-number = '000'.
      wa_return-message = 'You are not allowed to submit Future Travel expenses.'.
      wa_return-message_v1 = 'You are not allowed to submit Future Travel'.
      wa_return-message_v2 = ' expenses.'.
      APPEND wa_return TO return.
      CLEAR wa_return.

    ELSE.

      CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES_NEW'
        EXPORTING
          i_datum_bis       = sy-datum
          i_datum_von       = general_data-datv1
          i_kz_incl_bis     = ' '
          i_kz_volle_monate = ' '
        IMPORTING
          e_monate          = lv_monate.
      lv_days = sy-datum - general_data-datv1.
      IF lv_days GT 45.
*        wa_return-type = 'E'.
*        WA_RETURN-ID = 'ZHR01'.
*        WA_RETURN-NUMBER = '000'.
*        wa_return-MESSAGE_V1 = 'You are not allowed to submit this claim'.
*
*        append wa_return to return.
*        clear wa_return.
      ENDIF.
    ENDIF.
    IF  general_data-schem EQ '01'.


      SELECT SINGLE sum_reimbu FROM ptrv_shdr INTO v_sum_reimbu
                                    WHERE pernr = general_data-pernr
                                      AND reinr = general_data-reinr
                                      AND perio = general_data-perio.
      IF   v_sum_reimbu GT 20000.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'Claim amount should not be more than Rs 20000'.

        APPEND wa_return TO return.
        CLEAR wa_return.
      ENDIF.

      SELECT SINGLE ergru FROM pa0017 INTO lv_ergru
                         WHERE pernr EQ employeenumber
                           AND begda LE sy-datum
                           AND endda GE sy-datum.

      IF lv_ergru EQ '1'.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'You are not authorized to submit this expense'.

        APPEND wa_return TO return.
        CLEAR wa_return.

      ENDIF.
      CALL FUNCTION 'LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = general_data-datv1
        IMPORTING
          last_day_of_month = last_date
*         EXCEPTIONS
*         DAY_IN_NO_DATE    = 1
*         OTHERS            = 2
        .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF general_data-datv1+6(2) EQ '01'.

        CONCATENATE general_data-datv1+0(6) '15' INTO general_data-datb1.
      ENDIF.

      IF general_data-datv1+6(2) EQ '16'.
        CONCATENATE general_data-datv1+0(6) last_date+6(2) INTO general_data-datb1.
      ENDIF.
      IF general_data-datv1+6(2) EQ '01' AND general_data-datb1+6(2) NE '15'.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'Only forthnight claims are allowed'.
        APPEND wa_return TO return.
        CLEAR wa_return.
      ELSEIF general_data-datv1+6(2) EQ '16' AND general_data-datb1+6(2) NE last_date+6(2).
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'Only forthnight claims are allowed'.
        APPEND wa_return TO return.
        CLEAR wa_return.
      ELSEIF general_data-datv1+6(2) NE '01' AND   general_data-datv1+6(2) NE '16'.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'Only forthnight claims are allowed'.
        APPEND wa_return TO return.
        CLEAR wa_return.
      ELSEIF general_data-datb1+6(2) NE '15' AND  general_data-datb1+6(2) NE last_date+6(2).
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'Only forthnight claims are allowed'.
        APPEND wa_return TO return.
        CLEAR wa_return.
      ENDIF.
    ELSEIF general_data-schem EQ '02'.
      SELECT SINGLE ergru FROM pa0017 INTO lv_ergru
                          WHERE pernr EQ employeenumber
                            AND begda LE sy-datum
                            AND endda GE sy-datum.
      IF lv_ergru EQ '1'.
        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = general_data-datv1
          IMPORTING
            last_day_of_month = last_date
*         EXCEPTIONS
*           DAY_IN_NO_DATE    = 1
*           OTHERS            = 2
          .
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        IF general_data-datv1+6(2) EQ '01'.
          CONCATENATE general_data-datv1+0(6) '15' INTO general_data-datb1.
        ENDIF.

        IF general_data-datv1+6(2) EQ '16'.
          CONCATENATE general_data-datv1+0(6) last_date+6(2) INTO general_data-datb1.
        ENDIF.


        IF general_data-datv1+6(2) EQ '01' AND general_data-datb1+6(2) NE '15'.
          wa_return-type = 'E'.
          wa_return-id = 'ZHR01'.
          wa_return-number = '000'.
          wa_return-message_v1 = 'Only forthnight claims are allowed'.
          APPEND wa_return TO return.
          CLEAR wa_return.
        ELSEIF general_data-datv1+6(2) EQ '16' AND general_data-datb1+6(2) NE last_date+6(2).
          wa_return-type = 'E'.
          wa_return-id = 'ZHR01'.
          wa_return-number = '000'.
          wa_return-message_v1 = 'Only forthnight claims are allowed'.
          APPEND wa_return TO return.
          CLEAR wa_return.
        ELSEIF general_data-datv1+6(2) NE '01' AND   general_data-datv1+6(2) NE '16'.
          wa_return-type = 'E'.
          wa_return-id = 'ZHR01'.
          wa_return-number = '000'.
          wa_return-message_v1 = 'Only forthnight claims are allowed'.
          APPEND wa_return TO return.
          CLEAR wa_return.
        ELSEIF general_data-datb1+6(2) NE '15' AND  general_data-datb1+6(2) NE last_date+6(2).
          wa_return-type = 'E'.
          wa_return-id = 'ZHR01'.
          wa_return-number = '000'.
          wa_return-message_v1 = 'Only forthnight claims are allowed'.
          APPEND wa_return TO return.
          CLEAR wa_return.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*************************************************************************************************
******Remove duplicate trips within same period
********WO-896****Start of change-22nd Sept 2020

  DATA : gv_last_date TYPE sy-datum.

  CLEAR : gv_last_date.

  SELECT SINGLE ergru FROM  pa0017 INTO @DATA(gv_ergru)
                        WHERE pernr EQ @employeenumber
                          AND begda LE @sy-datum
                          AND endda GE @sy-datum.
  IF sy-subrc = 0.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = general_data-datv1
      IMPORTING
        last_day_of_month = gv_last_date
*         EXCEPTIONS
*       DAY_IN_NO_DATE    = 1
*       OTHERS            = 2
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

********check Travel schema and Reimbursement Group for Meals/Accomm.
    IF general_data-schem = '02' AND gv_ergru = '2'.


*****Get trip data for the Personnel number
      SELECT SINGLE *
        FROM ptrv_head
        INTO @DATA(ls_trip)
        WHERE pernr = @employeenumber
        AND   datv1 = @general_data-datv1
        AND   datb1 = @general_data-datb1.

***if found for any other trip between this range give an error
      IF sy-subrc = 0.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'You have already booked a trip in this period'.
        APPEND wa_return TO return.
        CLEAR wa_return.

*****Else check whetehr trip commences on the same date give and error
      ELSE.

        SELECT SINGLE *
        FROM ptrv_head
        INTO @ls_trip
        WHERE pernr = @employeenumber
        AND   datv1 = @general_data-datv1.

****if trip found throw an error
        IF sy-subrc = 0.
          wa_return-type = 'E'.
          wa_return-id = 'ZHR01'.
          wa_return-number = '000'.
          wa_return-message_v1 = 'You have already booked a trip starting from this date'.
          APPEND wa_return TO return.
          CLEAR wa_return.

***ELse finally check whether trip ends on the same day
        ELSE.

          SELECT SINGLE *
             FROM ptrv_head
             INTO @ls_trip
             WHERE pernr = @employeenumber
             AND   datb1 = @general_data-datb1.

****If trip found throw an error not to allow mutliple trips
          IF sy-subrc = 0.
            wa_return-type = 'E'.
            wa_return-id = 'ZHR01'.
            wa_return-number = '000'.
            wa_return-message_v1 = 'You have already booked a trip ending with this date'.
            APPEND wa_return TO return.
            CLEAR wa_return.
          ENDIF.
        ENDIF.

      ENDIF.

********Cash schema check

    ELSEIF general_data-schem = '01'  AND gv_ergru = '1'.

      IF general_data-datv1+6(2) EQ '01'.
        CONCATENATE general_data-datv1+0(6) '15' INTO general_data-datb1.
      ENDIF.

      IF general_data-datv1+6(2) EQ '16'.
        CONCATENATE general_data-datv1+0(6) gv_last_date+6(2) INTO general_data-datb1.
      ENDIF.

      IF general_data-datv1+6(2) EQ '01' AND general_data-datb1+6(2) NE '15'.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'You have already booked a trip ending with this date'.
        APPEND wa_return TO return.
        CLEAR wa_return.

      ELSEIF general_data-datv1+6(2) EQ '16' AND general_data-datb1+6(2) NE gv_last_date+6(2).
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '000'.
        wa_return-message_v1 = 'You have already booked a trip ending with this date'.
        APPEND wa_return TO return.
        CLEAR wa_return.

      ENDIF.

    ENDIF.

  ENDIF.

  CLEAR : gv_last_date.

ENDMETHOD.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ITINERARY.

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ITIN_COSTS_SPLIT.

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_ADVANCES.

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_CCC_RECEIPT.

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_DEDUCTIONS.

  BREAK-POINT.

endmethod.


METHOD if_ex_trip_web_check~user_check_line_of_itinerary.

  BREAK-POINT.

ENDMETHOD.


METHOD if_ex_trip_web_check~user_check_line_of_mileage.

  BREAK-POINT.

ENDMETHOD.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_RECEIPTS.

  BREAK-POINT.

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

    BREAK-POINT.

  endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILEAGE.
  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILE_COSTS_SPLIT.

  BREAK-POINT.

endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_RECEIPTS.

  BREAK-POINT.

endmethod.


METHOD if_ex_trip_web_check~user_check_rece_costs_split.

  BREAK-POINT.

*****23rd Sept20
******If expense is 0 validation- Expense should not be zero
  DATA : wa_return TYPE bapiret2,
         cs_rec    TYPE split_of_receipt.

  LOOP AT costdistribution_receipt INTO cs_rec.
    IF cs_rec-auftl_abs <= '0.00'.
      wa_return-type   = 'E'.
      wa_return-id     = 'ZHR01'.
      wa_return-number = '000'.
      wa_return-message_v1 = 'Expense reciept can not be Zero'.
      APPEND wa_return TO return.
      CLEAR wa_return.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TEXT.

  BREAK-POINT.

endmethod.


  method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TRANSPORTS.

    BREAK-POINT.

  endmethod.


METHOD if_ex_trip_web_check~user_check_trip_costs_split.


BREAK-POINT.

ENDMETHOD.
ENDCLASS.
