*----------------------------------------------------------------------*
*   INCLUDE RHSBETOP                                                   *
*----------------------------------------------------------------------*

* name of hierarch. table for overview list
CONSTANTS: $O_WORKT_TAB  TYPE SLIS_TABNAME VALUE 'O_WORKT_TAB',
           $O_OBJECT_TAB TYPE SLIS_TABNAME VALUE 'O_OBJECT_TAB',
           $ORG_TAB      TYPE SLIS_TABNAME VALUE 'ORG_TAB'.
* name of keyinfo-field for overview list
CONSTANTS: $ORGUNIT     TYPE SLIS_FIELDNAME VALUE 'ORGUNIT'.

* constants for color
CONSTANTS: $COL_ORG(3)     TYPE C VALUE 'C10',  "hellblau
           $COL_POS(3)     TYPE C VALUE 'C30',  "hellgelb
           $COL_ROT(3)     TYPE C VALUE 'C60'.  "hellrot

* name of hierarch. table for output detail list
CONSTANTS: $FIRST_TAB   TYPE SLIS_TABNAME   VALUE 'FIRST_TAB',
           $SECOND_TAB  TYPE SLIS_TABNAME   VALUE 'SECOND_TAB',
           $DETAIL_TAB  TYPE SLIS_TABNAME   VALUE 'DETAIL_TAB',
           $LIST_OUT    TYPE SLIS_TABNAME   VALUE 'LIST_OUT'.
* name of keyinfo-field for detail list
CONSTANTS: $S_POS_ID    TYPE SLIS_FIELDNAME VALUE 'S_POS_ID'.
* name of fcode for changing view absolute <-> effective
CONSTANTS: $ABS         TYPE SLIS_EXTAB-FCODE VALUE 'ABS',
           $EFF         TYPE SLIS_EXTAB-FCODE VALUE 'EFF'.

* structure overview list
DATA:  BEGIN OF OVER,
             ORGUNIT(10)    TYPE C,    "Orgschlüssel
             STEXT          LIKE OBJEC-STEXT,   "Orgbezeichnung
             LEVEL          LIKE STRUC-LEVEL,   "Stufe
             DCT_POS(30)    TYPE C,    "info direct position
             DCT_OPOS(30)   TYPE C,
             SUM_DCT_POS    TYPE P,
             DCT_PER        TYPE P,    "info direct person
             DCT_OPER       TYPE P,
             SUM_DCT_PER    TYPE P,
             IND_POS(30)    TYPE C,    "info indirect pos.
             IND_OPOS(30)   TYPE C,
             SUM_IND_POS    TYPE P,
             IND_PER        TYPE P,    "info indirect per.
             IND_OPER       TYPE P,
             SUM_IND_PER    TYPE P,
             TOT_WRKT_POS   TYPE P DECIMALS 2,
             TOT_WRKT_PER   TYPE P DECIMALS 2,
             TOT_WRKT_DEV   TYPE P DECIMALS 2,  "Abweichung in Stunden
             TOT_WRKT_PRO   TYPE P DECIMALS 2.  "Abweichung in Prozent
DATA: END OF OVER.
* internal table for whole information -used only as help-table
DATA: O_VIEW LIKE OVER OCCURS 0.

* structure for item table -overview
DATA: BEGIN OF OVER_OBJ,
             ORGUNIT         LIKE OBJEC-OBJID,    "Orgid
             STEXT           LIKE OBJEC-STEXT,    "Orgbezeichnung
             LEVEL(10)       TYPE C,   "Stufe
             POS_SORT        TYPE I,   "field for sort
             POS_TEXT(30)    TYPE C,   "Text for statistic
             POS_COUNT(20)   TYPE C,   "statistic data position
             PER_COUNT(20)   TYPE C,   "statistic data person
             COL_I(3)        TYPE C.   "field for color
DATA: END OF OVER_OBJ.
* structure for header table -overview
DATA: BEGIN OF OVER_WKT,
             ORGUNIT          LIKE OBJEC-OBJID,    "Orgid
             SHORT            LIKE OBJEC-SHORT,    "Kürzel
             STEXT            LIKE OBJEC-STEXT,    "Langtext
             LEVEL            LIKE STRUC-LEVEL,    "Stufe
             TOT_WRKT_POS     TYPE P DECIMALS 2,   "Soll Arbeitszeit
             TOT_WRKT_PER     TYPE P DECIMALS 2,   "Ist Arbeitszeit
             TOT_WRKT_DEV     TYPE P DECIMALS 2,   "Abweichung/Stunden
             TOT_WRKT_PRO     TYPE P DECIMALS 2,   "Abweichung/Prozent
             COL(3)           TYPE C.
DATA: END OF OVER_WKT.
* internal tables for hierar. list -overview
DATA: O_WORKT_TAB  LIKE OVER_WKT OCCURS 0.
DATA: O_OBJECT_TAB LIKE OVER_OBJ OCCURS 0.
*************************
*** Overview as sequentiell list with ALV GRID
DATA: BEGIN OF OVER_STRUC,
             ORGUNIT         LIKE OBJEC-OBJID,    "Orgid
             SHORT            LIKE OBJEC-SHORT,    "Kürzel
             STEXT           LIKE OBJEC-STEXT,    "Orgbezeichnung
             LEVEL(10)       TYPE C,   "Stufe
             TOT_WRKT_POS     TYPE P DECIMALS 2,   "Soll Arbeitszeit
             TOT_WRKT_PER     TYPE P DECIMALS 2,   "Ist Arbeitszeit
             TOT_WRKT_DEV     TYPE P DECIMALS 2,   "Abweichung/Stunden
             TOT_WRKTPRO     TYPE P DECIMALS 2,   "Abweichung/Prozent
             POS_SORT        TYPE I,   "field for sort
             POS_TEXT(30)    TYPE C,   "Text for statistic
             POS_COUNT(20)   TYPE C,   "statistic data position
             PER_COUNT(20)   TYPE C.   "statistic data person
DATA: END OF OVER_STRUC.
DATA: O_GRID_TAB  LIKE OVER_STRUC OCCURS 0.

*  structure detail-information for hierarch. list
DATA:  BEGIN OF DETAIL_FIRST,
             S_POS_ID       LIKE OBJEC-OBJID,
             S_SHORT        LIKE OBJEC-SHORT,   "Planstellenkürzel
             S_TEXT         LIKE OBJEC-STEXT,   "Planstel.bezeichnung
             S_ODATE(22)    TYPE C,    "Objektzeitraum Planst.
             S_LEITER(4)    TYPE C,    "Leiterplanstellen Kennzeichen
*             S_LEADER       like P1000-STEXT, "Manager of position
             S_OBSOLET(4)   TYPE C,    "Obsolet Kennzeichen
             S_VACAN(22)    TYPE C,    "Vakanzzeitraum
             S_NOCCUP(10)   TYPE C,    "Datum 'unbesetzt seit'
             S_WORKT        TYPE P DECIMALS 2,  "Soll Arbeitszeit
             S_ADEV         TYPE P DECIMALS 2,  "Abweichung -absolut
             S_EDEV         TYPE P DECIMALS 2,  "Abweichung -effektiv
             COL_F(3)       TYPE C,    "Hilfsfeld für Zeilenfärbung
       END OF DETAIL_FIRST.

DATA: FIRST_TAB LIKE DETAIL_FIRST OCCURS 0.

DATA:  BEGIN OF DETAIL_SECOND,
             S_POS_ID      LIKE OBJEC-OBJID,
             V_DATE(22)    TYPE C,              "Verknüpfungszeitraum
             P_OTYPE       LIKE OBJEC-OTYPE,
             P_HOLDER(10)  TYPE C,               "Otype und Objid
*            p_short       like objec-short,    "Inhaberkürzel
             P_STEXT       LIKE OBJEC-STEXT,     "Name des Inhabers
             P_ODATE(22)   TYPE C,               "Objektzeitraum Inhaber
             P_WORKT       TYPE P DECIMALS 2,    "Ist Arbeitszeit -abs
             P_EWORK       TYPE P DECIMALS 2,    "Ist Arbeitszeit -eff
             P_ABES        TYPE P DECIMALS 2, "BesProzent-abs Note649340
             P_EBES        TYPE P DECIMALS 2, "BesProz.eff.VWMNOTE401990
             P_PERSG       LIKE T501T-PTEXT,     "Mitarbeitergruppe
             P_PERSK       LIKE T503T-PTEXT,     "Mitarbeiterkreis
             P_COL_F(3)    TYPE C,         "Hilfsfeld für Zeilenfärbung
        END OF DETAIL_SECOND.

DATA: SECOND_TAB LIKE DETAIL_SECOND OCCURS 0.

*** structure for staff assignment Detail View with ALV Grid
*** only sequentiell lists are provided
DATA:  BEGIN OF DETAIL_LIST,
             S_POS_ID       LIKE OBJEC-OBJID,
             S_SHORT        LIKE OBJEC-SHORT,   "Planstellenkürzel
             S_TEXT         LIKE OBJEC-STEXT,   "Planstel.bezeichnung
             S_ODATE(22)    TYPE C,    "Objektzeitraum Planst.
             S_LEITER(4)    TYPE C,    "Leiterplanstellen Kennzeichen
*             S_LEADER       like P1000-STEXT,  "Manager of position
*             S_OBSOLET(4)   TYPE C,    "Obsolet Kennzeichen
*             S_VACAN(22)    TYPE C,    "Vakanzzeitraum
*             S_NOCCUP(10)   TYPE C,    "Datum 'unbesetzt seit'
             S_BES_STAT(35)  Type c,    "Besetzungsstatus
             S_WORKT        TYPE P DECIMALS 2,  "Soll Arbeitszeit
             S_ADEV         TYPE P DECIMALS 2,  "Abweichung -absolut
             S_EDEV         TYPE P DECIMALS 2,  "Abweichung -effektiv
             V_DATE(22)    TYPE C,              "Verknüpfungszeitraum
             P_OTYPE       LIKE OBJEC-OTYPE,
             P_HOLDER(10)  TYPE C,               "Otype und Objid
*            p_short       like objec-short,    "Inhaberkürzel
             P_STEXT       LIKE OBJEC-STEXT,     "Name des Inhabers
             P_ODATE(22)   TYPE C,               "Objektzeitraum Inhaber
             P_WORKT       TYPE P DECIMALS 2,    "Ist Arbeitszeit -abs
             P_EWORK       TYPE P DECIMALS 2,    "Ist Arbeitszeit -eff
             P_ABES        TYPE P DECIMALS 2, "BesProzent-abs Note649340
             P_EBES        TYPE P DECIMALS 2, "BesProz.eff.VWMNOTE401990
             P_PERSG       LIKE T501T-PTEXT,     "Mitarbeitergruppe
             P_PERSK       LIKE T503T-PTEXT,     "Mitarbeiterkreis
        END OF DETAIL_LIST.

DATA: DETAIL_TAB LIKE DETAIL_LIST OCCURS 0.




DATA: LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: OLIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
data: LIST_END_OF_PAGE  type slis_T_Listheader.
DATA: BEGIN OF FIELDNAMES,
           SPALTEN_NAME(60),
           DDIC_NAME(10),
           DDIC_TABLE(10),
           LAYOUT,                     "I: invisible; F: fix; X:Key
         END OF FIELDNAMES.

DATA: UCOMM  LIKE SY-UCOMM.

* flag for changing between Listviewer and Standard
DATA: EXIT_FLAG LIKE PDBES-EXP_STAT.
DATA: V_EXIT TYPE SLIS_EXIT_BY_USER.
DATA: CHANGE_FLAG LIKE PDBES-EXP_STAT.
DATA: VIEW_FLAG LIKE PDBES-EXP_STAT.

* keyinformation for hierarch. list
*ata: d_keyinfo  type slis_keyinfo_alv,
DATA: O_KEYINFO  TYPE SLIS_KEYINFO_ALV.
DATA: GT_PRINT TYPE SLIS_PRINT_ALV.

* excluding table for status
DATA: D_EXTAB TYPE SLIS_EXTAB.

* table for changing layout in fieldcat
TYPES: BEGIN OF C_FIELD,
              FIELDNAME TYPE SLIS_FIELDNAME,
              TABNAME   TYPE SLIS_TABNAME,
         END OF C_FIELD.
TYPES: C_FIELD_TAB TYPE C_FIELD OCCURS 0.

* fieldcat data for changing layout
DATA: C_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

* global structure for header info in detail view
DATA: TOP LIKE OVER.
DATA: S_DAY(10) TYPE C.
DATA: ORGUNIT       LIKE OBJEC-OBJID,
      ORGTEXT       LIKE OBJEC-STEXT.

* internal table for fix list column
TYPES: BEGIN OF COL_STRUC,
       COL_POS         LIKE SY-CUCOL,
       FIELDNAME       TYPE SLIS_FIELDNAME,
       TABNAME         TYPE SLIS_TABNAME,
       END OF COL_STRUC.
TYPES: COL_QUEUE TYPE COL_STRUC OCCURS 1.
DATA: O_COL_QUEUE     TYPE COL_QUEUE WITH HEADER LINE.
DATA: D_COL_QUEUE     TYPE COL_QUEUE WITH HEADER LINE.

* data for changing view of list
DATA: BEGIN OF ACT_INFO,
               OKEY       TYPE C,
               TREL       TYPE C,
               SHOR       TYPE C,
               ODAT       TYPE C,
               VDAT       TYPE C,
               CHANGE_OKEY TYPE C,
               CHANGE_SHOR TYPE C,
               CHANGE_ODAT TYPE C,
               CHANGE_VDAT TYPE C,
               OKEY_FTEXT LIKE T77FD-FTEXT,
               TREL_FTEXT LIKE T77FD-FTEXT,
               SHOR_FTEXT LIKE T77FD-FTEXT,
               ODAT_FTEXT LIKE T77FD-FTEXT,
               VDAT_FTEXT LIKE T77FD-FTEXT.
DATA: END OF ACT_INFO.

DATA: D_ACT_OKEY TYPE C,
      D_ACT_SHOR TYPE C.

* structure for detail modus
DATA:  BEGIN OF LIST_TAB,
             O_ORG_ID       LIKE OBJEC-OBJID,
             O_SHORT        LIKE OBJEC-SHORT,
             O_STEXT        LIKE OBJEC-STEXT,
             O_OSTAT        LIKE OBJEC-ISTAT,
             O_ODATE(22)    TYPE C,
             S_POS_ID       LIKE OBJEC-OBJID,
             S_SHORT        LIKE OBJEC-SHORT,   "Planstellenkürzel
             S_TEXT         LIKE OBJEC-STEXT,   "Planstel.bezeichnung
             S_OSTAT        LIKE OBJEC-ISTAT,
             S_ODATE(22)    TYPE C,       "Objektzeitraum Planst.
*            S_LEITER(4)    TYPE C,       "Leiterplanstellen Kennzeichen
             S_LEiter       like P1000-STEXT, "Manager of position
*             S_OBSOLET      TYPE FLAG_X,  "Obsolet Kennzeichen
*             S_VACAN(22)    TYPE C,       "Vakanzzeitraum
             S_BES_STAT(35)  Type c,        "Besetzungsstatus
*            S_NOCCUP(10)   TYPE C,    "Datum 'unbesetzt seit'
             S_WORKT        TYPE P DECIMALS 2,  "Soll Arbeitszeit
             S_WORKS TYPE P DECIMALS 2,         "Soll*     "AP569860
             S_ADEV         TYPE P DECIMALS 2,  "Abweichung -absolut
             S_EDEV         TYPE P DECIMALS 2,  "Abweichung -effektiv
             V_DATE(22)    TYPE C,    "Verknüpfungszeitraum Inhaber
             P_ISTAT       LIKE STRUC-VISTAT,
             P_OTYPE       LIKE OBJEC-OTYPE,
*            P_HOLDER(10)  TYPE C,    "Otype und Objid    "SRON984148
             P_HOLDER(15)  TYPE C,    "Otype und Objid    "SRON984148
*            p_short       like objec-short,   "Inhaberkürzel
             P_STEXT       LIKE OBJEC-STEXT,   "Name des Inhabers
             P_OSTAT       LIKE OBJEC-ISTAT,
             P_ODATE(22)   TYPE C,    "Objektzeitraum Inhaber
             P_WORKT       TYPE P DECIMALS 2, "Ist Arbeitszeit -absolut
             P_EWORK       TYPE P DECIMALS 2,"Ist Arbeitszeit -effektiv
             P_ABES        type p decimals 2, "BesProzent-abs Note649340
             P_EBES        TYPE P DECIMALS 2, "BesProz.eff.VWMNOTE401990
             P_PERSG       LIKE T501T-PTEXT,   "Mitarbeitergruppe
             P_PERSK       LIKE T503T-PTEXT,   "Mitarbeiterkreis
             P_COL_F(3)    TYPE C,   "Hilfsfeld für Zeilenfärbung
             count type i,
             O_ORGID_MAIN      TYPE HRP1001-SOBID,
             O_SHORT_MAIN       LIKE OBJEC-SHORT,
             O_STEXT_MAIN       LIKE OBJEC-STEXT,
             tot_FIX TYPE P DECIMALS 2, "TOTAL FIXED PAY
             PERFORMANCE TYPE P DECIMALS 2," PERFORMANCE INCENTIVE
             CTC TYPE P DECIMALS 2," TOTAL CTC
        END OF LIST_TAB.

DATA: LIST_OUT LIKE LIST_TAB OCCURS 0.
