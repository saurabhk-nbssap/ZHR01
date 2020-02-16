*----------------------------------------------------------------------*
*   INCLUDE PCSANIN2                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  GET PERNR.

* Read Infotypes.
  PERFORM READ_INFOTYPES.

* Set Progress Indicator.
  PERFORM PROGRESS_INDICATOR USING TEXT-BEG.

* Read Cluster Results.
  PERFORM IMPORT_RESULTS.

* Fill Data
  PERFORM FILL_DISP_BODY.

END-OF-SELECTION.

* No valid records found for selection.
   DESCRIBE TABLE DISP_BODY LINES COUNT.
   IF COUNT = 0.
     CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = 'Process Discontinued'(029)
            TXT1  = 'No Payroll Results available for'(030)
            TXT2  = 'employee/employees selected for this criteria'(031)
          EXCEPTIONS
               OTHERS  = 1.
      if sy-batch = 'X'.
       write: TEXT-030,
              TEXT-031.
      endif.
     EXIT.
   ENDIF.

* Display Results.
  PERFORM DISPLAY.
