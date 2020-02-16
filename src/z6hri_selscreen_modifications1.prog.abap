*&---------------------------------------------------------------------*
*&  Include           Z6HRI_SELSCREEN_MODIFICATIONS1
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZHRI_SELSCREEN_MODIFICATIONS1
*&---------------------------------------------------------------------*

*  IF pb_pernr IS NOT INITIAL .
*    LOOP AT SCREEN.
*      IF screen-name = 'PA_PERNR' .
**     if screen-group1 = 'G2' .
*        screen-input = 0.
**      screen-invisible = 1.
*        MODIFY SCREEN.
*      ENDIF.
**   endif.
*    ENDLOOP.
*    LOOP AT SCREEN.
*      IF screen-name = 'SM' .
**    if  screen-group1 = 'G1'.
*        screen-input = 0.
**     screen-invisible = 1.
*        MODIFY SCREEN.
*      ENDIF.
**   endif.
*    ENDLOOP.
*  ENDIF.

  IF pa_pernr IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'PB_PERNR' .
*    if  screen-group1 = 'G1'.
        screen-input = 0.
*     screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
*   endif.
    ENDLOOP.
    LOOP AT SCREEN.
      IF screen-name = 'SM' .
*    if  screen-group1 = 'G1'.
        screen-input = 1.
*     screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
*   endif.
    ENDLOOP.
  ENDIF.
