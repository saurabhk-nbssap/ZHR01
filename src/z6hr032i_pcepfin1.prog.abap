*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN1                                                   *
*----------------------------------------------------------------------*

INFOTYPES:            0587,            "Provident Fund
                      0002,            "Personal data
                      0021,            "family data
                      0023,            "Prev Service
                      0000.

TABLES: PERNR,                         "Std Selections PA Master Data
        T7INF1,                                             "
        T596F,                         "HR Subroutines
        T511K,
        ADDR1_DATA,
        SADR,
        SSCRFIELDS,
        pa0001, " IHDK902336

***** START OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****
        T500C.
***** END OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****
