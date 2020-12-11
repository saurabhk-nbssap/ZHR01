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


METHOD if_ex_trip_web_check~user_check_general_data.
  DATA : wa_return TYPE bapiret2.
  DATA : lv_days TYPE i.
  DATA : last_date TYPE sy-datum.
  DATA : lv_ergru TYPE p0017-ergru.
  DATA : v_sum_reimbu  TYPE ptrv_shdr-sum_reimbu.
  DATA : lv_monate TYPE komp-anz_monate.

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
*-----CHanges 11.12.20202 ++++Sandeep
      IF lv_ergru EQ '1' OR lv_ergru EQ '2'.
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

*******1)
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
        wa_return-number = '002'.
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
          wa_return-number = '002'.
          wa_return-message_v1 = 'You have already booked a trip on same date'."starting from this date'.
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
            wa_return-number = '002'.
            wa_return-message_v1 = 'You have already booked a trip on same date'.
            APPEND wa_return TO return.
            CLEAR wa_return.
          ENDIF.
        ENDIF.

      ENDIF.

*****************
*****2) Travel schema- 01

    ELSEIF general_data-schem = '02' AND gv_ergru = '1'.

*****Get trip data for the Personnel number
      SELECT SINGLE *
        FROM ptrv_head
        INTO @ls_trip
        WHERE pernr = @employeenumber
        AND   datv1 = @general_data-datv1
        AND   datb1 = @general_data-datb1.

***if found for any other trip between this range give an error
      IF sy-subrc = 0.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '002'.
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
          wa_return-number = '002'.
          wa_return-message_v1 = 'You have already booked a trip on same date'."starting from this date'.
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
            wa_return-number = '002'.
            wa_return-message_v1 = 'You have already booked a trip on same date'.
            APPEND wa_return TO return.
            CLEAR wa_return.
          ENDIF.
        ENDIF.

      ENDIF.


************3)
********Cash schema check

    ELSEIF general_data-schem = '01'  AND gv_ergru = '1'.

*      IF general_data-datv1+6(2) EQ '01'.
*        CONCATENATE general_data-datv1+0(6) '15' INTO general_data-datb1.
*      ENDIF.
*
*      IF general_data-datv1+6(2) EQ '16'.
*        CONCATENATE general_data-datv1+0(6) gv_last_date+6(2) INTO general_data-datb1.
*      ENDIF.
*
*      IF general_data-datv1+6(2) EQ '01' AND general_data-datb1+6(2) NE '15'.
*        wa_return-type = 'E'.
*        wa_return-id = 'ZHR01'.
*        wa_return-number = '002'.
*        wa_return-message_v1 = 'You have already booked a trip on same date'.
*        APPEND wa_return TO return.
*        CLEAR wa_return.
*
*      ELSEIF general_data-datv1+6(2) EQ '16' AND general_data-datb1+6(2) NE gv_last_date+6(2).
*        wa_return-type = 'E'.
*        wa_return-id = 'ZHR01'.
*        wa_return-number = '002'.
*        wa_return-message_v1 = 'You have already booked a trip on same date'.
*        APPEND wa_return TO return.
*        CLEAR wa_return.
*
*      ENDIF.

****************
*      *****Get trip data for the Personnel number
      SELECT SINGLE *
        FROM ptrv_head
        INTO @ls_trip
        WHERE pernr = @employeenumber
        AND   datv1 = @general_data-datv1
        AND   datb1 = @general_data-datb1.

***if found for any other trip between this range give an error
      IF sy-subrc = 0.
        wa_return-type = 'E'.
        wa_return-id = 'ZHR01'.
        wa_return-number = '002'.
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
          wa_return-number = '002'.
          wa_return-message_v1 = 'You have already booked a trip on same date'."starting from this date'.
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
            wa_return-number = '002'.
            wa_return-message_v1 = 'You have already booked a trip on same date'.
            APPEND wa_return TO return.
            CLEAR wa_return.
          ENDIF.
        ENDIF.

      ENDIF.


**************

    ENDIF.

  ENDIF.

  CLEAR : gv_last_date.

ENDMETHOD.


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


METHOD if_ex_trip_web_check~user_check_line_of_itinerary.

ENDMETHOD.


METHOD if_ex_trip_web_check~user_check_line_of_mileage.


ENDMETHOD.


METHOD if_ex_trip_web_check~user_check_line_of_receipts.

  DATA : wa_t706s_receipt TYPE t706s_receipt.
  DATA  : wa_return TYPE bapiret2.
  SELECT SINGLE * FROM t706s_receipt INTO wa_t706s_receipt
                           WHERE morei EQ general_data-morei
                             AND schem EQ general_data-schem
                             AND spkzl EQ receipt-spkzl
                             AND begda LE receipt-bldat
                             AND endda GE receipt-bldat.
  IF sy-subrc NE 0.
    wa_return-type = 'E'.
    wa_return-id = 'ZHR01'.
    wa_return-number = '001'.
    wa_return-message = 'This Expense Type is not allowed '.
    wa_return-message_v1 = 'This Expense Type is not allowed '.
    APPEND wa_return TO return.
    CLEAR wa_return.

  ENDIF.
  IF general_data-schem EQ '01'.
    receipt-payot = 'CC'.
  ELSEIF general_data-schem EQ '02'.
    receipt-payot = 'TC'.
  ENDIF.




***************
*****23rd Sept20
******If expense is 0 validation- Expense should not be zero
  DATA : cs_rec    TYPE split_of_receipt.

*AUFTL_ABS

*BETRG
*pay_amount

  CLEAR wa_return.

  IF receipt-betrg <= '0.00' OR  receipt-betrg <= 0 .
    wa_return-type   = 'E'.
    wa_return-id     = 'ZHR01'.
    wa_return-number = '002'.
    wa_return-message_v1 = 'Expense reciept can not be Zero'.
    APPEND wa_return TO return.
    CLEAR wa_return.
  ELSEIF receipt-pay_amount <= '0.00' OR  receipt-pay_amount <= 0 .
    wa_return-type   = 'E'.
    wa_return-id     = 'ZHR01'.
    wa_return-number = '002'.
    wa_return-message_v1 = 'Expense reciept can not be Zero'.
    APPEND wa_return TO return.
    CLEAR wa_return.
  ENDIF.



ENDMETHOD.


  method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_TRANSPORT.

  endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILEAGE.


endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILE_COSTS_SPLIT.


endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_RECEIPTS.


endmethod.


METHOD if_ex_trip_web_check~user_check_rece_costs_split.


ENDMETHOD.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TEXT.


endmethod.


  method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TRANSPORTS.


  endmethod.


METHOD if_ex_trip_web_check~user_check_trip_costs_split.


ENDMETHOD.
ENDCLASS.
