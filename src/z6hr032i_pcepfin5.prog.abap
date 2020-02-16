*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN5                                                   *
*----------------------------------------------------------------------*
DATA: DISP_FLG_LOT TYPE I VALUE 1.
DATA: NEGTV TYPE I VALUE -1.

AT SELECTION-SCREEN OUTPUT.
  IF DISP_FLG_LOT GE 1.
    MOVE 'Customer Layout'(005) TO LOUT.
    PERFORM CHANGE_SCREEN_LOT USING ON OFF.
  ELSE.
    MOVE 'Standard Layout'(006) TO LOUT.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON.
  ENDIF.

AT SELECTION-SCREEN ON BLOCK BOX1.
  IF SSCRFIELDS-UCOMM = 'LOUT'.
    DISP_FLG_LOT = DISP_FLG_LOT * NEGTV.
  ENDIF.

AT SELECTION-SCREEN.
  IF DISP_FLG_LOT LT 1 AND SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM CHK_LAYOUT USING LAYOUT.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN_LOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->$PAR1  text
*      -->$PAR2  text
*----------------------------------------------------------------------*
FORM CHANGE_SCREEN_LOT USING   $PAR1 $PAR2.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'LOT'.
      SCREEN-INVISIBLE = $PAR1.
      SCREEN-INPUT = $PAR2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


ENDFORM.                               " CHANGE_SCREEN_LOT

*&---------------------------------------------------------------------*
*&      Form  CHK_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT  text
*      -->P_LANGU  text
*----------------------------------------------------------------------*
FORM CHK_LAYOUT USING    $LAYOUT.

  IF $LAYOUT IS INITIAL.
    SET CURSOR FIELD 'LAYOUT'.
    MESSAGE E110(HRPADIN01).
*   Please specify the layout name
  ENDIF.


ENDFORM.                               " CHK_LAYOUT
