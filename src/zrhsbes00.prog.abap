* AP  20.12.2002 Note 583315
* AP   7.11.2002 Note 569533
* STRO 28.8.2002 Note 549290
* STRO 6.2.2002 Note 492950
* STRO note 0418541
* STRO 5.6.2001 note 97044
* STFO 14.11.01 note 0450293
REPORT ZRHSBES00 MESSAGE-ID 5A NO STANDARD PAGE HEADING.

****** ABAP LIST VIEWER *******
TYPE-POOLS: SLIS.
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
*******************************
TABLES: OBJEC, GDSTR, GDSET, T77WT, T77EO, T528T, SSCRFIELDS. "note583315

DATA : $CB    LIKE HRRHAS-CB_NR,                            "note97044
       HELP_PSTRU LIKE GDSTR-PSTRU.

INFOTYPES: 0001.

INCLUDE RHODAT00.
INCLUDE RHRDAT00.
INCLUDE RHSBESDT.

* data declaration for ABAP-Listviewer
INCLUDE ZRHSBETOP2.
*INCLUDE RHBESTOP.

DATA: $ORG_PATH   LIKE T778A-WEGID.
DATA: $OSP_PATH   LIKE T778A-WEGID.
DATA: $S_S_PATH   LIKE T778A-WEGID.
DATA: $OSBOS      LIKE PLOG-SUBTY.

* use functions
DATA: $RH_SBES_LI LIKE RS38L-NAME.
DATA: $RH_SBES_TP LIKE RS38L-NAME.
DATA: $RH_SBES_PD LIKE RS38L-NAME.

DATA: PERSG_INFTY LIKE PLOG-INFTY VALUE '1013'.
DATA: REDUN_INFTY LIKE PLOG-INFTY VALUE '1014'.
DATA: VACAN_INFTY LIKE PLOG-INFTY VALUE '1007'.
DATA: WORKT_INFTY LIKE PLOG-INFTY VALUE '1011'.

DATA: ACT_FCODE        LIKE T77FC-FCODE.
DATA: ACT_CSTRU        LIKE GDSTR-CSTRU.
DATA: BATCH_FLAG       LIKE SY-BATCH.
DATA: DEFAULT_POS      LIKE OBJEC-OBJID.
DATA: HEADER_FLAG      TYPE C.
DATA: OFF_CSTRU        LIKE GDSTR-CSTRU.
DATA: ORG_TAB_INDEX    LIKE SY-TABIX.
DATA: PERCK_TEXT       LIKE T77MT-MTEXT.
DATA: USED_PERCK       LIKE PDBES-FREQ."time period used as default
DATA: $MODE_TEXT       LIKE T77MT-MTEXT.
DATA: REP_NAME         LIKE TRDIR-NAME.
DATA: SUBRC            LIKE SY-SUBRC.
DATA: TOT_WRKT_DEV     TYPE P DECIMALS 2.      "work time deviation ab
DATA: TOT_WRKT_PRO     TYPE P DECIMALS 2.      "work time dev. precent


DATA: LT_ALV_CAT   TYPE  TABLE OF    LVC_S_FCAT .
DATA: wa_ALV_CAT   like line of    LT_ALV_CAT .

DATA      :   I_TABLE      TYPE  REF   TO    DATA .
FIELD-SYMBOLS : <TABLE>    TYPE  TABLE .     " Internal Table FOR zhrctc

FIELD-SYMBOLS: <dynline> TYPE ANY.
FIELD-SYMBOLS: <fld> TYPE ANY.


DATA: BEGIN OF ORG_TAB OCCURS 10,
        SEQNR            LIKE STRUC-SEQNR,
        ORG_OBJID        LIKE OBJEC-OBJID,
        PUP              LIKE STRUC-PUP,
        ISTAT            LIKE STRUC-VISTAT,      "status of org
        LEVEL            LIKE STRUC-LEVEL,       "level in structure
        SHORT            LIKE OBJEC-SHORT,
        STEXT            LIKE OBJEC-STEXT,                  "text
        DCT_POS_NO       TYPE P,       "no of direct positions
        DCT_OPOS_NO      TYPE P,       "no of direct obsolet positions
        DCT_BPOS_NO      TYPE P,       "no of occupied positions
        DCT_UPOS_NO      TYPE P,       "no of unoccupied positions
        DCT_BOPOS_NO     TYPE P,       "no of direct obsolet occ pos
        DCT_VPOS_NO      TYPE P,       "no of vakant positions
        DCT_PER_NO       TYPE P,       "no of direct holders
        DCT_OPER_NO      TYPE P,       "no of direct holders obs. pos
        IND_POS_NO       TYPE P,       "no of indirect pos.
        IND_BPOS_NO      TYPE P,       "no of ind. occupied positions
        IND_OPOS_NO      TYPE P,       "no of indirect obsolet position
        IND_BOPOS_NO     TYPE P,       "no of indirect obsolet occ pos
        IND_PER_NO       TYPE P,       "no of indirect holders
        IND_OPER_NO      TYPE P,       "no of indirect holders obs. pos
        TOT_WRKT_POS     TYPE P DECIMALS 2,      "tot work time of pos.
        TOT_WRKT_PER     TYPE P DECIMALS 2,      "tot work time of pers.
        BEGDA            LIKE STRUC-VBEGDA,
        ENDDA            LIKE STRUC-VENDDA,
      END   OF ORG_TAB.

DATA: BEGIN OF POS_STRUC OCCURS 100,
        ORG_OBJID LIKE OBJEC-OBJID,
        OTYPE     LIKE OBJEC-OTYPE,
        OBJID     LIKE OBJEC-OBJID,
        OSTAT     LIKE OBJEC-ISTAT,
        OBEG      LIKE OBJEC-BEGDA,
        OEND      LIKE OBJEC-ENDDA,
        LEVEL     LIKE STRUC-LEVEL,
        IST_MEM   TYPE N,              "status as member
        BEGDA     LIKE STRUC-VBEGDA,
        ENDDA     LIKE STRUC-VENDDA,
        IST_BOS   TYPE N,              "status as boss
        BEGDA_BOS LIKE STRUC-VBEGDA,
        ENDDA_BOS LIKE STRUC-VENDDA,
        SHORT     LIKE OBJEC-SHORT,
        STEXT     LIKE OBJEC-STEXT,
        WORKT     LIKE P1011-YRAVG,
        VACRC     LIKE SY-SUBRC,
        VACDT     LIKE P1007-BEGDA,
        FREDT     LIKE P1007-BEGDA,
        REDUN     LIKE P1014-REDUN,
        PNEXT     TYPE N,
        PDOWN     TYPE N,
        PUP       TYPE N,
      END   OF POS_STRUC.

DATA: BEGIN OF PER_STRUC OCCURS 100,
        POS_OTYPE LIKE OBJEC-OTYPE,
        POS_OBJID LIKE OBJEC-OBJID,
        OTYPE     LIKE OBJEC-OTYPE,
        OBJID     LIKE OBJEC-OBJID,
        OSTAT     LIKE OBJEC-ISTAT,
        OBEG      LIKE OBJEC-BEGDA,
        OEND      LIKE OBJEC-ENDDA,
        ISTAT     LIKE STRUC-VISTAT,
        BEGDA     LIKE STRUC-VBEGDA,
        ENDDA     LIKE STRUC-VENDDA,
        RSIGN     LIKE STRUC-VRSIGN,
        RELAT     LIKE STRUC-VRELAT,
        STEXT     LIKE OBJEC-STEXT,
        PROZT     LIKE STRUC-VPROZT,
        WORKT     LIKE P1011-YRAVG,
        PERSG     LIKE P0001-PERSG,
        PERSK     LIKE P0001-PERSK,
        REALO(12) TYPE C,
        ORG_OBJID LIKE OBJEC-OBJID, "STRO Note 549290
      END   OF PER_STRUC.
DATA: LAST_OBJID LIKE OBJEC-OBJID.  "AP Note 569860
DATA: LIN_NUM    TYPE I.            "AP Note 583315
DATA: BEGIN OF SEARCH_POS OCCURS 5,
        SEQNR LIKE STRUC-SEQNR,
        INDEX LIKE STRUC-SEQNR,
      END   OF SEARCH_POS.

DATA: BEGIN OF ORG_HEADER OCCURS 3.
        INCLUDE STRUCTURE OBJEC.
DATA: END   OF ORG_HEADER.

DATA: BEGIN OF ORG_OBJECT OCCURS 20.
        INCLUDE STRUCTURE OBJEC.
DATA: END   OF ORG_OBJECT.

DATA: BEGIN OF ORG_STRUCT OCCURS 20.
        INCLUDE STRUCTURE STRUC.
DATA: END   OF ORG_STRUCT.

DATA: BEGIN OF PER_OBJECT OCCURS 20.
        INCLUDE STRUCTURE OBJEC.
DATA: END   OF PER_OBJECT.

DATA: BEGIN OF POS_VACAN OCCURS  3.
        INCLUDE STRUCTURE P1007.
DATA: END   OF POS_VACAN.

DATA: BEGIN OF ALL_WORKT OCCURS 20.
        INCLUDE STRUCTURE P1011.
DATA: END   OF ALL_WORKT.

DATA: BEGIN OF POS_REDUN OCCURS  3.
        INCLUDE STRUCTURE P1014.
DATA: END   OF POS_REDUN.

DATA: BEGIN OF PER_GROUP OCCURS  3.
        INCLUDE STRUCTURE P0001.
DATA: END   OF PER_GROUP.

DATA: BEGIN OF POS_KEY,
        ORG_OBJID LIKE OBJEC-OBJID,
        OTYPE     LIKE OBJEC-OTYPE,
        OBJID     LIKE OBJEC-OBJID,
      END   OF POS_KEY.

DATA: BEGIN OF I77EO.
        INCLUDE STRUCTURE T77EO.
DATA: END   OF I77EO.

* table for persons groups
DATA: BEGIN OF WRKGP_TAB OCCURS 10,
       PERSG LIKE T77WT-PERSG,
       PERSK LIKE T77WT-PERSK,
       SUBTY LIKE T77WT-WTGRP,
      END OF WRKGP_TAB.

DATA: BEGIN OF WRKGP_TAB_KEY,
       PERSG LIKE T77WT-PERSG,
       PERSK LIKE T77WT-PERSK,
      END OF WRKGP_TAB_KEY.

* data-Definitions for get_worktime
DATA: RET_OBJID LIKE P1000-OBJID.      "OBJID for given worktime
DATA: RET_OTYPE LIKE P1000-OTYPE.      "OTYPE for given worktime
DATA: RET_OTEXT LIKE P1000-STEXT.      "OTEXT for given worktime
DATA: RET_OTYPETEXT LIKE T777O-OTEXT.  "OTYPETEXT for given worktime

DATA: POS_HIER  LIKE PDBES-POS_HIER  VALUE 'X'.

* selection block options
SELECTION-SCREEN BEGIN OF BLOCK LIST_2
                       WITH FRAME TITLE TEXT-LOS.
PARAMETERS: $PERCK   LIKE PDBES-FREQ.  "time period of work time
PARAMETERS: HOLDONLY LIKE PDBES-HOLDONLY.  "show only holder
PARAMETERS: ORG_P    LIKE PDBES-ORG_P. "show persons without position
SELECTION-SCREEN END OF BLOCK LIST_2.



INITIALIZATION.
  INCLUDE RHOINI00.
  INCLUDE RHRINI00.

  PERFORM INIT_SBES.

  PCHPLVAR = $PLVAR.
  PCHOTYPE = $ORGEH.
  PCHWEGID = $ORG_PATH.
  PCHTIMED = 'D'.
  RH-SEL-KEYDATE.                                           "YNKK120323

AT SELECTION-SCREEN.

  IF SSCRFIELDS-UCOMM = 'ONLI'.           "F8 = execute AP Note 583315
    DESCRIBE TABLE PCHOBJID LINES LIN_NUM.
    IF LIN_NUM IS INITIAL AND PCHSEARK IS INITIAL.
      MESSAGE W018(5A).
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON $PERCK.
  PERFORM CHECK_PERCK USING $PERCK PERCK_TEXT SUBRC.
  IF SUBRC > 0.
    MESSAGE E699 WITH
              'Diese Arbeitszeitbasis ist nicht erlaubt'(EPK).
  ENDIF.


START-OF-SELECTION.

  IF SY-BATCH <> SPACE.
*   report is excuted as batch run
    INCLUDE RHOINI00.
    INCLUDE RHRINI00.

    PERFORM INIT_SBES.
  ENDIF.

  PERFORM CHECK_SBES.

  CLEAR: OFF_CSTRU, ACT_CSTRU.

* object time period is selected
  IF NOT PCHOBEG IS INITIAL.
    PCHBEGDA = PCHOBEG.
    PCHENDDA = PCHOEND.
  ENDIF.

GET OBJEC.
  IF PC-WEGID IS INITIAL.
*   sequentiell access -> initialize variable which show structure info
    CLEAR: GDSTR, STRUC.
    STRUC-LEVEL = 1.
    STRUC-SEQNR = 1.
    PC-DEPTH = 1.
    PC-SVECT = '1'.
    OFF_CSTRU = OFF_CSTRU + ACT_CSTRU.
    ACT_CSTRU = 1.
  ELSEIF STRUC-LEVEL = 1.
*   increase offset pointer with info of the structure before
    OFF_CSTRU = OFF_CSTRU + ACT_CSTRU.
    ACT_CSTRU = GDSTR-CSTRU.
  ENDIF.
  STRUC-SEQNR = STRUC-SEQNR + OFF_CSTRU.
  IF STRUC-PUP > 0.
    STRUC-PUP   = STRUC-PUP + OFF_CSTRU.
  ENDIF.

* treat only org units
  IF OBJEC-OTYPE = $ORGEH.
    CLEAR ORG_TAB.
    ORG_TAB-SEQNR = STRUC-SEQNR.
    ORG_TAB-ORG_OBJID = OBJEC-OBJID.
    ORG_TAB-SHORT  = OBJEC-SHORT.
    IF OBJEC-STEXT IS INITIAL.
      ORG_TAB-STEXT = OBJEC-SHORT.
    ELSE.
      ORG_TAB-STEXT = OBJEC-STEXT.
    ENDIF.
    IF STRUC-PUP > 0.
      ORG_TAB-PUP   = STRUC-PUP.
    ENDIF.
    ORG_TAB-LEVEL = STRUC-LEVEL.
    ORG_TAB-ISTAT = STRUC-VISTAT.
    ORG_TAB-BEGDA = STRUC-VBEGDA.
    ORG_TAB-ENDDA = STRUC-VENDDA.
    APPEND ORG_TAB.

    PERFORM PROCESS_ORGEH TABLES ORG_TAB  SEARCH_POS
                          USING  OBJEC    STRUC
                                 PC-BEGDA PC-ISTAT
                                 PC-SVECT PC-ACTIV
                                 $PERCK   POS_HIER.
  ENDIF.

END-OF-SELECTION.

  PERFORM GET_OVER_DATA TABLES ORG_TAB O_VIEW
                               O_OBJECT_TAB O_WORKT_TAB O_GRID_TAB
                        USING  PERCK_TEXT.

* Anzeigemodus 'Detail View of Staffing assignment'
  PERFORM DETAIL_LIST_VIEWER TABLES ORG_TAB POS_STRUC PER_STRUC
                                       O_VIEW
                                USING  V_EXIT D_EXTAB PERCK_TEXT
                                       ' '.


*---------------------------------------------------------------------*
*       FORM CHECK_SBES                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CHECK_SBES.
  PERFORM CHECK_PERCK USING $PERCK PERCK_TEXT SUBRC.
  IF SUBRC > 0.
    MESSAGE E699 WITH
              'Diese Arbeitszeitbasis ist nicht erlaubt'(EPK).
  ENDIF.


  IF HOLDONLY = SPACE.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'P_EXT'
                                   $OSP_PATH SUBRC.
    IF SUBRC > 0.
      MESSAGE E001 WITH 'T77S0' 'SBES ' 'P_EXT' SPACE.
    ENDIF.
  ELSE.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'P_NOR'
                                   $OSP_PATH SUBRC.
    IF SUBRC > 0.
      MESSAGE E001 WITH 'T77S0' 'SBES ' 'P_NOR' SPACE.
    ENDIF.
  ENDIF.

  IF ORG_P <> SPACE.
    PERFORM RE77S0(MSTT77S0)
              USING 'PLOGI' 'PRELI' DEFAULT_POS SUBRC.
    IF SUBRC > 0.
      MESSAGE E001 WITH 'T77S0' 'PRELI' DEFAULT_POS SPACE.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_SBES

*---------------------------------------------------------------------*
*       FORM INIT_SBES                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM INIT_SBES.
  DATA: TAB_ENTRIES TYPE P.

  IF $ORG_PATH IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'P_O-O'
                                   $ORG_PATH SUBRC.
    IF SUBRC > 0.
      MESSAGE E001 WITH 'T77S0' 'SBES ' 'P_O-O' SPACE.
    ENDIF.
  ENDIF.

  IF $S_S_PATH IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'P_S-S'
                                   $S_S_PATH SUBRC.
    IF SUBRC > 0.
      MESSAGE E001 WITH 'T77S0' 'SBES ' 'P_S-S' SPACE.
    ENDIF.
  ENDIF.

  IF $RH_SBES_LI IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'LIST '
                                   $RH_SBES_LI SUBRC.
    IF SUBRC > 0.
      MESSAGE S001 WITH 'T77S0' 'SBES ' 'LIST ' SPACE.
      $RH_SBES_LI = 'RH_SBES_LI'.
    ENDIF.
  ENDIF.

  IF $RH_SBES_TP IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'TOPL '
                                   $RH_SBES_TP SUBRC.
    IF SUBRC > 0.
      MESSAGE S001 WITH 'T77S0' 'SBES ' 'TOPL ' SPACE.
      $RH_SBES_TP = 'RH_SBES_TP'.
    ENDIF.
  ENDIF.

  IF $RH_SBES_PD IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'SBES ' 'PERS '
                                   $RH_SBES_PD SUBRC.
    IF SUBRC > 0.
      MESSAGE S001 WITH 'T77S0' 'SBES ' 'PERS ' SPACE.
      $RH_SBES_PD = 'RH_SBES_PD'.
    ENDIF.
  ENDIF.

  IF $PERCK IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'WORKT' 'PERCK' $PERCK SUBRC.
    IF SUBRC > 0.
      MESSAGE S001 WITH 'T77S0' 'WORKT' 'PERCK' SPACE.
      $PERCK = MONTHLY.
    ENDIF.
    USED_PERCK = $PERCK.
  ENDIF.

  IF $OSBOS IS INITIAL.
    PERFORM RE77S0(MSTT77S0) USING 'PPREL' 'LEADA' $OSBOS SUBRC.
    IF SUBRC > 0.
      MESSAGE W001 WITH 'T77S0' 'PPREL' 'LEADA' SPACE.
      $OSBOS = 'A012'.
    ENDIF.
    PERFORM $TRANSFORM_RELAT(SAPFH5AN) USING $OSBOS $OSBOS.
  ENDIF.

  SELECT SINGLE * FROM T77EO WHERE OTYPE = $PERNR.
  IF SY-SUBRC > 0.
    MESSAGE E001 WITH 'T77EO' $PERNR SPACE SPACE.
  ENDIF.
  I77EO = T77EO.

*  IF ABS_BEG IS INITIAL.
*    ABS_BEG = SY-DATUM.
*    ABS_BEG+4 = '0101'.
*  ENDIF.

  DESCRIBE TABLE WRKGP_TAB LINES TAB_ENTRIES.
  IF TAB_ENTRIES = 0.
* entry not found -> read atab table
    SELECT * FROM T77WT.
* entry found -> append entry
      CLEAR WRKGP_TAB.
      WRKGP_TAB-PERSG = T77WT-PERSG.
      WRKGP_TAB-PERSK = T77WT-PERSK.
      WRKGP_TAB-SUBTY = T77WT-WTGRP.
      APPEND WRKGP_TAB.
    ENDSELECT.
  ENDIF.

ENDFORM.                    "INIT_SBES

*---------------------------------------------------------------------*
*       FORM PROCESS_ORGEH                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ORGEH                                                         *
*  -->  POS_IND                                                       *
*  -->  VALUE(ORG_OBJEC)                                              *
*  -->  VALUE(ORG_STR)                                                *
*  -->  VALUE(PAR_DATE)                                               *
*  -->  VALUE(PAR_ISTAT)                                              *
*  -->  VALUE(PAR_SVECT)                                              *
*  -->  VALUE(PAR_ACTIV)                                              *
*  -->  VALUE(PAR_PERCK)                                              *
*  -->  VALUE(PAR_S_HIER)                                             *
*---------------------------------------------------------------------*
FORM PROCESS_ORGEH TABLES ORGEH   STRUCTURE ORG_TAB
                          POS_IND STRUCTURE SEARCH_POS
                   USING  VALUE(ORG_OBJEC) STRUCTURE OBJEC
                          VALUE(ORG_STR) STRUCTURE STRUC
                          VALUE(PAR_DATE)  VALUE(PAR_ISTAT)
                          VALUE(PAR_SVECT) VALUE(PAR_ACTIV)
                          VALUE(PAR_PERCK) VALUE(PAR_S_HIER).

  DATA: BEGIN OF LOC_STRUC OCCURS 20.
          INCLUDE STRUCTURE POS_STRUC.
  DATA: END   OF LOC_STRUC.

  DATA: DCT_POS_NO   LIKE ORG_TAB-DCT_POS_NO,   "no of positions
        DCT_PER_NO   LIKE ORG_TAB-DCT_PER_NO,   "no of holders
        DCT_OPOS_NO  LIKE ORG_TAB-DCT_OPOS_NO,  "no of obs. pos
        DCT_BOPOS_NO LIKE ORG_TAB-DCT_BOPOS_NO, "no of occ obs. pos
        DCT_BPOS_NO  LIKE ORG_TAB-DCT_BPOS_NO,              "no occ pos
        DCT_VPOS_NO  LIKE ORG_TAB-DCT_VPOS_NO,  "no of vacant pos
        DCT_OPER_NO  LIKE ORG_TAB-DCT_OPER_NO,  "no of hold. obs. pos
        SUM_WRKT_PER LIKE ORG_TAB-TOT_WRKT_PER,
        TOT_WRKT_POS LIKE ORG_TAB-TOT_WRKT_POS, "tot work time of pos.
        TOT_WRKT_PER LIKE ORG_TAB-TOT_WRKT_PER. "tot work time of pers.

  DATA: ACT_LEVEL    LIKE STRUC-LEVEL.
  DATA: ACT_INDEX    LIKE SY-TABIX.
  DATA: ACT_INDEX_B  LIKE SY-TABIX.
  DATA: ACT_INDEX_L  LIKE SY-TABIX.
  DATA: TAB_ENTRIES  LIKE SY-TABIX.

  DATA: BEGIN OF ORG_WORKT OCCURS 3.
          INCLUDE STRUCTURE P1011.
  DATA: END   OF ORG_WORKT VALID BETWEEN BEGDA AND ENDDA.

  LOCAL: OBJEC, GDSTR, STRUC.

  CHECK ORG_OBJEC-OTYPE = $ORGEH.

* check if orgeh was already proccessed
  REFRESH: POS_IND.
  READ TABLE POS_STRUC WITH KEY ORG_OBJEC-OBJID.
  IF SY-SUBRC > 0.
*   orgeh was not processed -> get now all informations
*   now read data concerning org unit
*   read work time defined for org unit


*    PERFORM GET_WORKTIME(SAPFH5AM)
*              TABLES ORG_WORKT
*              USING  ORG_OBJEC-PLVAR ORG_OBJEC-OTYPE
*                     ORG_OBJEC-OBJID PAR_ISTAT
*                     PAR_DATE        PAR_DATE   SUBRC
*                     RET_OBJID RET_OTYPE RET_OTEXT
*                     RET_OTYPETEXT ' '.

* find related positions etc
*   perform e05_clear_cb(sapdbpch) using '11'.
*    perform e01_insert_stru(sapdbpch)
*              using org_objec-plvar  org_objec-otype
*                    org_objec-objid  $osp_path
*                    par_svect        par_date
*                    par_date         'X'         'X'
*                    par_activ        '0'.
*    clear struc-subrc.
*   NOW GET ELEMENTS
*    while struc-subrc = 0.
*      perform e04_get_stru(sapdbpch)
*                using gdstr-pstru 0 '11'.
*
*      if struc-subrc = 0.
    IF $CB IS INITIAL.
      CALL FUNCTION 'CONTROL_BLOCK_RESERVE'
        IMPORTING
          CB_NR  = $CB
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.

    CALL FUNCTION 'STRUCTURE_BUILD'
         EXPORTING
              PLVAR            = ORG_OBJEC-PLVAR
              OTYPE            = ORG_OBJEC-OTYPE
              OBJID            = ORG_OBJEC-OBJID
              WEGID            = $OSP_PATH
              SVECT            = PAR_SVECT
              SBEGD            = PAR_DATE
              SENDD            = PAR_DATE
              TFLAG            = 'X'
              VFLAG            = 'X'
              ACTIV            = PAR_ACTIV
*              tdepth           = 0
*              sflag            = 'X'
*              recurs           = ' '
*              77aw_int         = ' '
*              authy            = 'X'
*              authy_base       = '$'
               CBFLAG           = ' '
*              text_buffer_fill = ' '
         IMPORTING
              OBJECT           = OBJEC
              ROOT             = GDSTR
              ENTRY            = STRUC
*             cb_nr            = $cb
*         tables
*              check_tab_desc   =
          EXCEPTIONS
              ROOT_NOT_FOUND   = 1
              WEGID_NOT_FOUND  = 2
              OTHERS           = 3.

    IF SY-SUBRC = 0.
      CLEAR STRUC-SUBRC.
      WHILE STRUC-SUBRC = 0.
        CALL FUNCTION 'STRUCTURE_ENTRY_GET'
          EXPORTING
            INDEX  = GDSTR-PSTRU
            DEPTH  = 0
            CB_NR  = $CB
          IMPORTING
            OBJECT = OBJEC
            ENTRY  = STRUC.
*       get positions and persons
        CASE OBJEC-OTYPE.

          WHEN $PLSTE.
*           position -> store all relevant data
            PERFORM PROCESS_POSITION
                       TABLES LOC_STRUC ORG_WORKT  POS_IND
                       USING  OBJEC
                              STRUC
                              PAR_DATE
                              PAR_ISTAT
                              PAR_PERCK
                              ORG_OBJEC-OBJID.

          WHEN $WORKP.
*           treat work place like position (if eval. way is changed)
            PERFORM PROCESS_POSITION
                       TABLES LOC_STRUC ORG_WORKT  POS_IND
                       USING  OBJEC
                              STRUC
                              PAR_DATE
                              PAR_ISTAT
                              PAR_PERCK
                              ORG_OBJEC-OBJID.

          WHEN $PERNR.
            REFRESH P0001.   "STRO note 0418541
            PERFORM READ_INFTY(SAPDBPCH)
                TABLES P0001
                USING  OBJEC-PLVAR OBJEC-OTYPE
                       OBJEC-OBJID  '0001'
                       '1'.


*           store persons info
            PERFORM PROCESS_PERSON
                       TABLES PER_STRUC LOC_STRUC POS_IND
                              P0001
                       USING  OBJEC
                              STRUC
                              PAR_DATE
                              PAR_PERCK
                              SUBRC.

          WHEN $USER.
*           store persons info
            PERFORM PROCESS_USER
                       TABLES PER_STRUC LOC_STRUC POS_IND
                       USING  OBJEC
                              STRUC.

        ENDCASE.
*      endif.
      ENDWHILE.
    ENDIF.
*   clean up
*    perform e07_refresh_last_stru(sapdbpch).
    CALL FUNCTION 'CONTROL_BLOCK_DELETE'
      EXPORTING
        CB_NR = $CB.

    CALL FUNCTION 'STRUCTURE_DELETE'
      EXPORTING
        PSTRU = HELP_PSTRU.
    CLEAR: OBJEC, STRUC.

    IF NOT ORG_P IS INITIAL. "STRO note 492950
*     find all persons without position but linked to org unit
      CALL FUNCTION 'RH_GET_PERNR_WITHOUT_POSITION'
        EXPORTING
          ORG_OBJID              = ORG_OBJEC-OBJID
          PAR_BEGDA              = PAR_DATE
          PAR_ENDDA              = PAR_DATE
          PAR_PLVAR              = PC-PLVAR
        TABLES
          PER_OBJEC              = PER_OBJECT
        EXCEPTIONS
          DEFAULT_POS_NOT_FOUND  = 1
          PERSON_OTYPE_NOT_FOUND = 2.

      IF SY-SUBRC > 0.
        CLEAR ORG_P.
        REFRESH PER_OBJECT.
      ENDIF.

    ELSE.                  "STRO note 492950
      REFRESH PER_OBJECT.  "STRO note 492950
    ENDIF.                 "STRO note 492950


    DESCRIBE TABLE PER_OBJECT LINES TAB_ENTRIES.
    IF TAB_ENTRIES > 0.
*       insert default position
      PERFORM INSERT_DEFAULT_POS
                TABLES LOC_STRUC POS_IND
                USING  PAR_DATE
                       ORG_OBJEC-OBJID.
    ENDIF.

*     now treat these persons which are not linked to a position
    STRUC-PUP = '99999'.             "key for default pos
    STRUC-VISTAT = '1'.
    STRUC-VBEGDA = PAR_DATE.
    STRUC-VENDDA = PAR_DATE.
    STRUC-VRSIGN = $SPREL+0(1).
    STRUC-VRELAT = $SPREL+1(3).
    STRUC-VPROZT = 100.

    LOOP AT PER_OBJECT.
      REFRESH P0001.
      PERFORM READ_INFTY(SAPDBPCH)
                TABLES P0001
                USING  PC-PLVAR          $PERNR
                       PER_OBJECT-OBJID  '0001'
                       '1'.

      PERFORM PROCESS_PERSON
                 TABLES PER_STRUC  LOC_STRUC POS_IND
                        P0001
                 USING  PER_OBJECT STRUC
                        PAR_DATE   PAR_PERCK
                        SUBRC.
    ENDLOOP.
  ENDIF.

*   get hierachie informations
  ACT_LEVEL = 1.
  CLEAR ACT_INDEX.

  IF PAR_S_HIER <> SPACE.
*  take position hierarchie into account -> loop downwards in hierarchie
    DESCRIBE TABLE POS_STRUC LINES ACT_INDEX_L.
    ACT_INDEX_L = ACT_INDEX_L + 1.

*     copy roots to table pos_struc
    LOOP AT LOC_STRUC WHERE ORG_OBJID = ORG_OBJEC-OBJID
                        AND LEVEL     = 1.
      POS_STRUC = LOC_STRUC.
      APPEND POS_STRUC.
    ENDLOOP.

    WHILE SY-SUBRC = 0.
      LOOP AT POS_STRUC FROM ACT_INDEX_L
                        WHERE ORG_OBJID = ORG_OBJEC-OBJID
                          AND LEVEL     = ACT_LEVEL.

        ACT_INDEX = SY-TABIX.
        ACT_INDEX_B = SY-TABIX.

        PERFORM E05_CLEAR_CB(SAPDBPCH) USING '12'.
        PERFORM E01_INSERT_STRU(SAPDBPCH)
                  USING ORG_OBJEC-PLVAR  POS_STRUC-OTYPE
                        POS_STRUC-OBJID  $S_S_PATH
                        PAR_SVECT        PAR_DATE
                        PAR_DATE         'X'         'X'
                        PAR_ACTIV        '2'.
        CLEAR STRUC-SUBRC.
        WHILE STRUC-SUBRC = 0.
          PERFORM E04_GET_STRU(SAPDBPCH)
                    USING GDSTR-PSTRU 0 '12'.

          IF STRUC-SUBRC = 0.
*             read table pos_struc and copy enty to pos_struc
            POS_KEY-ORG_OBJID = ORG_OBJEC-OBJID.
            POS_KEY-OTYPE = OBJEC-OTYPE.
            POS_KEY-OBJID = OBJEC-OBJID.

            READ TABLE LOC_STRUC WITH KEY POS_KEY.
            IF SY-SUBRC        = 0 AND
               LOC_STRUC-LEVEL = 0.
*      mark member with actual level and move member to correct position
              LOC_STRUC-LEVEL = ACT_LEVEL + 1.
              MODIFY LOC_STRUC INDEX SY-TABIX.

*               now modify pointer of entry before
              READ TABLE POS_STRUC INDEX ACT_INDEX.
              IF POS_STRUC-LEVEL = LOC_STRUC-LEVEL.
                POS_STRUC-PNEXT = 1.
                MODIFY POS_STRUC INDEX ACT_INDEX.
              ELSEIF POS_STRUC-LEVEL = ACT_LEVEL.
                POS_STRUC-PDOWN = 1.
                MODIFY POS_STRUC INDEX ACT_INDEX.
              ENDIF.

              POS_STRUC = LOC_STRUC.
              ACT_INDEX = ACT_INDEX + 1.
              INSERT POS_STRUC INDEX ACT_INDEX.
            ENDIF.
          ENDIF.
        ENDWHILE.
        PERFORM E07_REFRESH_LAST_STRU(SAPDBPCH).
      ENDLOOP.
      IF SY-SUBRC = 0.
        ACT_LEVEL = ACT_LEVEL + 1.
        ACT_INDEX = ACT_INDEX_B.
      ENDIF.

    ENDWHILE.
  ELSE.
*     take only boss hierarchie into account
    LOOP AT LOC_STRUC WHERE ORG_OBJID = ORG_OBJEC-OBJID
                        AND LEVEL     = 1.
      POS_STRUC = LOC_STRUC.
      APPEND POS_STRUC.
    ENDLOOP.
    IF SY-SUBRC = 0.
*       boss exists -> rest of positions are one level lower
      ACT_LEVEL = ACT_LEVEL + 1.
      DESCRIBE TABLE POS_STRUC LINES ACT_INDEX.
    ENDIF.
  ENDIF.

*   now loop over all positions in table which are not in hierarchie
*   -> these positions have the lowest level
  LOOP AT LOC_STRUC WHERE ORG_OBJID = ORG_OBJEC-OBJID
                      AND LEVEL     = 0.

    POS_STRUC = LOC_STRUC.
    POS_STRUC-LEVEL = ACT_LEVEL.
    POS_STRUC-PUP   = 1.
    APPEND POS_STRUC.

* now modify pointer of entries before
    IF ACT_INDEX > 0.
      READ TABLE POS_STRUC INDEX ACT_INDEX.
      IF POS_STRUC-LEVEL = ACT_LEVEL.
        POS_STRUC-PNEXT = 1.
        MODIFY POS_STRUC INDEX ACT_INDEX.
      ELSEIF POS_STRUC-LEVEL < ACT_LEVEL.
        POS_STRUC-PDOWN = 1.
        MODIFY POS_STRUC INDEX ACT_INDEX.
      ENDIF.
      ACT_INDEX = ACT_INDEX + 1.
    ENDIF.

  ENDLOOP.
*endif.

  CLEAR: DCT_POS_NO, DCT_PER_NO,
         TOT_WRKT_POS, TOT_WRKT_PER.
  CLEAR: DCT_BPOS_NO, DCT_VPOS_NO.

* now update orgeh information table
* important: redundant positions are not calculate when head
*            count and work time will be summed up

* LOOP AT POS_STRUC.
  LOOP AT POS_STRUC WHERE ORG_OBJID = ORG_OBJEC-OBJID.
*   first: get total numbers of all positions within this org unit
    IF POS_STRUC-OTYPE     =  $PLSTE AND
       POS_STRUC-IST_MEM   <  9. "   and  "only memebers of org unit
      IF POS_STRUC-REDUN     =  SPACE. "no redundant position

        IF POS_STRUC-OBJID <> DEFAULT_POS.
*         do not count default position
          DCT_POS_NO = DCT_POS_NO + 1.
          TOT_WRKT_POS = TOT_WRKT_POS + POS_STRUC-WORKT.
*         count vacant position
          IF POS_STRUC-VACRC = 0.
            DCT_VPOS_NO = DCT_VPOS_NO + 1.
          ENDIF.
        ENDIF.

        CLEAR SUM_WRKT_PER.

*       get total number of all persons of direct position
        LOOP AT PER_STRUC WHERE POS_OTYPE = POS_STRUC-OTYPE
                            AND POS_OBJID = POS_STRUC-OBJID
                            AND RELAT     = $SPREL+1(3).

          DCT_PER_NO = DCT_PER_NO + 1.
          SUM_WRKT_PER = SUM_WRKT_PER +
                         PER_STRUC-WORKT *
                         PER_STRUC-PROZT / 100.
        ENDLOOP.
*       count occupied direct positions
        IF SY-SUBRC = 0.
          DCT_BPOS_NO = DCT_BPOS_NO + 1.
        ENDIF.

* check if deviation is less then tolerance
        CALL FUNCTION 'RH_COMPARE_WITH_TOLERANCE'
          EXPORTING
            VALUE_A           = POS_STRUC-WORKT
            VALUE_B           = SUM_WRKT_PER
          IMPORTING
            CORRECTED_VALUE_B = SUM_WRKT_PER
          EXCEPTIONS
            OTHERS            = 1.

        TOT_WRKT_PER = TOT_WRKT_PER + SUM_WRKT_PER.
      ELSE.
        IF POS_STRUC-OBJID <> DEFAULT_POS.
*         do not count default position
          DCT_OPOS_NO = DCT_OPOS_NO + 1.
        ENDIF.

*       clear sum_wrkt_per.
*       get total number of all persons of direct obsolet position
        LOOP AT PER_STRUC WHERE POS_OTYPE = POS_STRUC-OTYPE
                            AND POS_OBJID = POS_STRUC-OBJID
                            AND RELAT     = $SPREL+1(3).

          DCT_OPER_NO = DCT_OPER_NO + 1.
        ENDLOOP.
*       count occupied direct obsolet positions
        IF SY-SUBRC = 0.
          DCT_BOPOS_NO = DCT_BOPOS_NO + 1.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

* now update org table
  READ TABLE ORGEH WITH KEY ORG_STR-SEQNR.
  IF SY-SUBRC = 0.
    ORGEH-DCT_POS_NO   = DCT_POS_NO.
    ORGEH-DCT_BPOS_NO  = DCT_BPOS_NO.
    ORGEH-DCT_PER_NO   = DCT_PER_NO.
    ORGEH-DCT_OPOS_NO  = DCT_OPOS_NO.
    ORGEH-DCT_VPOS_NO  = DCT_VPOS_NO.
    ORGEH-DCT_BOPOS_NO = DCT_BOPOS_NO.
    ORGEH-DCT_OPER_NO  = DCT_OPER_NO.
    ORGEH-TOT_WRKT_POS = TOT_WRKT_POS.
    ORGEH-TOT_WRKT_PER = TOT_WRKT_PER.
    ORGEH-DCT_UPOS_NO  = ( DCT_POS_NO + DCT_OPOS_NO ) - DCT_BPOS_NO.
    MODIFY ORGEH INDEX SY-TABIX.

*   loop upwards in org structure
    IF PC-DEPTH = 0.
*     count indirect positions/persons only in case of unlimited depth
      WHILE SY-SUBRC = 0.
        READ TABLE ORGEH WITH KEY ORGEH-PUP.
        IF SY-SUBRC = 0.
*         update org unit with indirect position informations
          ORGEH-IND_POS_NO = ORGEH-IND_POS_NO +
                             DCT_POS_NO.
          ORGEH-IND_BPOS_NO = ORGEH-IND_BPOS_NO +
                              DCT_BPOS_NO.
          ORGEH-IND_PER_NO = ORGEH-IND_PER_NO +
                             DCT_PER_NO.
          ORGEH-IND_OPOS_NO = ORGEH-IND_OPOS_NO +
                             DCT_OPOS_NO.
          ORGEH-IND_BOPOS_NO = ORGEH-IND_BOPOS_NO +
                             DCT_BOPOS_NO.
          ORGEH-IND_OPER_NO = ORGEH-IND_OPER_NO +
                             DCT_OPER_NO.
          MODIFY ORGEH INDEX SY-TABIX.
        ENDIF.
      ENDWHILE.
    ENDIF.
  ENDIF.

  FREE: PER_OBJECT.

ENDFORM.                    "PROCESS_ORGEH

*---------------------------------------------------------------------*
*       FORM PROCESS_POSITION                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  POS_INFO                                                      *
*  -->  ORG_WORKT                                                     *
*  -->  POS_TAB                                                       *
*  -->  VALUE(POS_OBJEC)                                              *
*  -->  VALUE(POS_STRUC)                                              *
*  -->  VALUE(PAR_DATE)                                               *
*  -->  VALUE(PAR_ISTAT)                                              *
*  -->  VALUE(PAR_PERCK)                                              *
*  -->  VALUE(ORG_OBJID)                                              *
*---------------------------------------------------------------------*
FORM PROCESS_POSITION TABLES POS_INFO  STRUCTURE POS_STRUC
                             ORG_WORKT STRUCTURE P1011
                             POS_TAB   STRUCTURE SEARCH_POS
                      USING  VALUE(POS_OBJEC) STRUCTURE OBJEC
                             VALUE(POS_STRUC) STRUCTURE STRUC
                             VALUE(PAR_DATE)  VALUE(PAR_ISTAT)
                             VALUE(PAR_PERCK)
                             VALUE(ORG_OBJID).

  DATA: BEGIN OF POS_WORKT OCCURS 3.
          INCLUDE STRUCTURE P1011.
  DATA: END   OF POS_WORKT VALID BETWEEN BEGDA AND ENDDA.

  DATA: BEGIN OF POS_PERSG OCCURS 3.
          INCLUDE STRUCTURE P1013.
  DATA: END   OF POS_PERSG VALID BETWEEN BEGDA AND ENDDA.

  DATA: BEGIN OF POS_REDUN OCCURS 3.
          INCLUDE STRUCTURE P1014.
  DATA: END   OF POS_REDUN VALID BETWEEN BEGDA AND ENDDA.

  DATA: ACT_ENTRY  LIKE SY-TABIX.

  LOCAL: OBJEC, GDSTR.

* check if position was already handled
  POS_KEY-ORG_OBJID = ORG_OBJID.
  POS_KEY-OTYPE = POS_OBJEC-OTYPE.
  POS_KEY-OBJID = POS_OBJEC-OBJID.

  READ TABLE POS_INFO WITH KEY POS_KEY.
  IF SY-SUBRC = 0.
*   entry already inserted -> save table entry
    ACT_ENTRY = SY-TABIX.
  ELSE.
*   no entry found -> fill pos_info
    CLEAR: POS_INFO, ACT_ENTRY.
    POS_INFO-ORG_OBJID = ORG_OBJID.
    POS_INFO-OTYPE = POS_OBJEC-OTYPE.
    POS_INFO-OBJID = POS_OBJEC-OBJID.
    POS_INFO-OSTAT = POS_OBJEC-ISTAT.
    POS_INFO-OBEG  = POS_OBJEC-BEGDA.
    POS_INFO-OEND  = POS_OBJEC-ENDDA.
    POS_INFO-LEVEL = 0.
*   status 9 means: not evaluated
    POS_INFO-IST_MEM = 9.
    POS_INFO-IST_BOS = 9.
    POS_INFO-SHORT = POS_OBJEC-SHORT.
    IF POS_OBJEC-STEXT IS INITIAL.
      POS_INFO-STEXT = POS_OBJEC-SHORT.
    ELSE.
      POS_INFO-STEXT = POS_OBJEC-STEXT.
    ENDIF.

*  check if position is redundant
*  important: redundant positions are not calculate when head
*             count and work time will be summed up

    PERFORM READ_INFTY_EXTENDED(SAPDBPCH)
               TABLES POS_REDUN
               USING  POS_OBJEC-PLVAR POS_OBJEC-OTYPE
                      POS_OBJEC-OBJID
                      REDUN_INFTY     SPACE     PAR_ISTAT
                      PAR_DATE        PAR_DATE.

    LOOP AT POS_REDUN WHERE ISTAT =< POS_OBJEC-ISTAT
                        AND REDUN <> SPACE.               "#EC PORTABLE
      POS_INFO-REDUN = 'X'.
      EXIT.
    ENDLOOP.

    IF SY-SUBRC > 0.
*     position is not redundant -> check if position is vacant
      CALL FUNCTION 'RH_GET_VACANCY'
        EXPORTING
          VAC_PLVAR       = POS_OBJEC-PLVAR
          VAC_OTYPE       = POS_OBJEC-OTYPE
          VAC_OBJID       = POS_OBJEC-OBJID
          VAC_OBJEC_BEGDA = POS_OBJEC-BEGDA
          VAC_ISTAT       = PAR_ISTAT
          VAC_BEGDA       = PAR_DATE
          VAC_ENDDA       = PAR_DATE
          VAC_SWITCH      = ' '
        IMPORTING
          VAC_SUBRC       = POS_INFO-VACRC
          VACANCY_BEGDA   = POS_INFO-VACDT
          FREE_BEGDA      = POS_INFO-FREDT.

      IF POS_INFO-VACRC > 4.
*       position is not vacant
        CLEAR POS_INFO-VACDT.
      ENDIF.
      IF POS_INFO-FREDT > PAR_DATE.
* position is not vacant
        CLEAR POS_INFO-FREDT.
      ENDIF.

*     now determine work time of position

*     first read work time infty at position
      PERFORM READ_INFTY_EXTENDED(SAPDBPCH)
                 TABLES POS_WORKT
                 USING  POS_OBJEC-PLVAR POS_OBJEC-OTYPE
                        POS_OBJEC-OBJID
                        WORKT_INFTY     SPACE     PAR_ISTAT
                        PAR_DATE        PAR_DATE.

      LOOP AT POS_WORKT WHERE ISTAT =< POS_OBJEC-ISTAT.   "#EC PORTABLE
*       now get work time
        PERFORM GET_WORKTIME_IN_PERIOD
                  USING PAR_PERCK       POS_WORKT-DYAVG
                        POS_WORKT-WKAVG POS_WORKT-MOAVG
                        POS_WORKT-YRAVG POS_INFO-WORKT.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC > 0.
*     position has no direct work time definition -> use org definition

*       now check if position has persg and persk definition
        PERFORM READ_INFTY_EXTENDED(SAPDBPCH)
                   TABLES POS_PERSG
                   USING  POS_OBJEC-PLVAR POS_OBJEC-OTYPE
                          POS_OBJEC-OBJID
                          PERSG_INFTY     SPACE     PAR_ISTAT
                          PAR_DATE        PAR_DATE.

        CLEAR WRKGP_TAB.
        LOOP AT POS_PERSG WHERE ISTAT =< POS_OBJEC-ISTAT. "#EC PORTABLE
          CLEAR WRKGP_TAB_KEY.
          WRKGP_TAB_KEY-PERSG = POS_PERSG-PERSG.
          WRKGP_TAB_KEY-PERSK = POS_PERSG-PERSK.
          READ TABLE WRKGP_TAB WITH KEY WRKGP_TAB_KEY.
          IF SY-SUBRC > 0.
*           entry not found -> read with persk '**'
            WRKGP_TAB_KEY-PERSK = '**'.
            READ TABLE WRKGP_TAB WITH KEY WRKGP_TAB_KEY.
            IF SY-SUBRC > 0.
*             entry not found -> read with persg '*'
              WRKGP_TAB_KEY-PERSG = '*'.
              READ TABLE WRKGP_TAB WITH KEY WRKGP_TAB_KEY.
              IF SY-SUBRC > 0.
                CLEAR WRKGP_TAB.
              ENDIF.
            ENDIF.
          ENDIF.
          IF WRKGP_TAB-SUBTY <> SPACE.
            EXIT.
          ENDIF.
        ENDLOOP.

*       now read relevant work time group
        IF WRKGP_TAB-SUBTY <> SPACE.
          LOOP AT ORG_WORKT WHERE SUBTY =  WRKGP_TAB-SUBTY
                              AND BEGDA =< PAR_DATE
                              AND ENDDA >= PAR_DATE.
*           now get work time
            PERFORM GET_WORKTIME_IN_PERIOD
                      USING PAR_PERCK       ORG_WORKT-DYAVG
                            ORG_WORKT-WKAVG ORG_WORKT-MOAVG
                            ORG_WORKT-YRAVG POS_INFO-WORKT.
            EXIT.
          ENDLOOP.
          IF SY-SUBRC > 0.
*           if no work time has been found - read default value
            LOOP AT ORG_WORKT WHERE DEFPS <> SPACE
                                AND BEGDA =< PAR_DATE
                                AND ENDDA >= PAR_DATE.
*             now get work time
              PERFORM GET_WORKTIME_IN_PERIOD
                    USING PAR_PERCK
                          ORG_WORKT-DYAVG
                          ORG_WORKT-WKAVG
                          ORG_WORKT-MOAVG
                          ORG_WORKT-YRAVG
                          POS_INFO-WORKT.
              EXIT.
            ENDLOOP.
          ENDIF.
        ELSE.
*         if no work time has been found - read default value
          LOOP AT ORG_WORKT WHERE DEFPS <> SPACE
                              AND BEGDA =< PAR_DATE
                              AND ENDDA >= PAR_DATE.
*           now get work time
            PERFORM GET_WORKTIME_IN_PERIOD
                  USING PAR_PERCK
                        ORG_WORKT-DYAVG
                        ORG_WORKT-WKAVG
                        ORG_WORKT-MOAVG
                        ORG_WORKT-YRAVG
                        POS_INFO-WORKT.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


* now take kind of relation into account
  CASE POS_STRUC-VRELAT.

    WHEN $OSBOS+1(3).
*     position is boss of org unit
      POS_INFO-LEVEL = 1.
      IF POS_INFO-IST_BOS > POS_STRUC-VISTAT.             "#EC PORTABLE
        POS_INFO-IST_BOS = POS_STRUC-VISTAT.
        POS_INFO-BEGDA_BOS = POS_STRUC-VBEGDA.
        POS_INFO-ENDDA_BOS = POS_STRUC-VENDDA.
      ENDIF.

    WHEN $OSREL+1(3).
*     position is member of org unit
      IF POS_INFO-IST_MEM > POS_STRUC-VISTAT.             "#EC PORTABLE
        POS_INFO-IST_MEM = POS_STRUC-VISTAT.
        POS_INFO-BEGDA = POS_STRUC-VBEGDA.
        POS_INFO-ENDDA = POS_STRUC-VENDDA.
      ENDIF.

  ENDCASE.

  IF ACT_ENTRY IS INITIAL.
*   append information since entry is new
    APPEND POS_INFO.
    DESCRIBE TABLE POS_INFO LINES ACT_ENTRY.
  ELSE.
*   modify entry and append entry in pos_tab
    MODIFY POS_INFO INDEX ACT_ENTRY.
  ENDIF.

  POS_TAB-SEQNR = POS_STRUC-SEQNR.
  POS_TAB-INDEX = ACT_ENTRY.
  APPEND POS_TAB.
ENDFORM.                    "PROCESS_POSITION

*---------------------------------------------------------------------*
*       FORM PROCESS_PERSON                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PER_INFO                                                      *
*  -->  POS_INFO                                                      *
*  -->  POS_IND                                                       *
*  -->  I0001                                                         *
*  -->  VALUE(PER_OBJEC)                                              *
*  -->  VALUE(PER_STRUC)                                              *
*  -->  PAR_DATE                                                      *
*  -->  PAR_PERCK                                                     *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
FORM PROCESS_PERSON TABLES PER_INFO STRUCTURE PER_STRUC
                           POS_INFO STRUCTURE POS_STRUC
                           POS_IND  STRUCTURE SEARCH_POS
                           I0001    STRUCTURE P0001
                    USING  VALUE(PER_OBJEC) STRUCTURE OBJEC
                           VALUE(PER_STRUC) STRUCTURE STRUC
                           PAR_DATE     PAR_PERCK
                           SUBRC.

  DATA: PER_PROZT LIKE P1001-PROZT,
        PER_DYHRS LIKE P1011-DYAVG,
        PER_WKHRS LIKE P1011-WKAVG,
        PER_MOHRS LIKE P1011-MOAVG,
        PER_YRHRS LIKE P1011-YRAVG.

  DATA: LOC_BEGDA LIKE OBJEC-BEGDA.
  DATA: LOC_ENDDA LIKE OBJEC-ENDDA.

* get corresponding position
  READ TABLE POS_IND WITH KEY PER_STRUC-PUP.
  READ TABLE POS_INFO INDEX POS_IND-INDEX.

* check if per_info exists already
  LOOP AT PER_INFO WHERE POS_OTYPE = POS_INFO-OTYPE
                     AND POS_OBJID = POS_INFO-OBJID
                     AND OTYPE     = PER_OBJEC-OTYPE
                     AND OBJID     = PER_OBJEC-OBJID
                     AND ISTAT     = PER_STRUC-VISTAT
                     AND RSIGN     = PER_STRUC-VRSIGN
                     AND RELAT     = PER_STRUC-VRELAT.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC > 0.
*   new entry
    CLEAR: SUBRC.
*   get work time of person (employee)
    PERFORM GET_BSGRD_FIRST IN PROGRAM (I77EO-PROG)
              USING PER_OBJEC-OBJID
                    PAR_DATE        PAR_DATE
                    LOC_BEGDA       LOC_ENDDA
                    PER_PROZT       PER_DYHRS
                    PER_WKHRS       PER_MOHRS
                    PER_YRHRS       SUBRC.

    IF SUBRC = 0.
*     fill per_info
      CLEAR PER_INFO.
      PER_INFO-POS_OTYPE = POS_INFO-OTYPE.
      PER_INFO-POS_OBJID = POS_INFO-OBJID.
      PER_INFO-OTYPE = PER_OBJEC-OTYPE.
      PER_INFO-OBJID = PER_OBJEC-OBJID.
      PER_INFO-OSTAT = PER_OBJEC-ISTAT.
      PER_INFO-OBEG  = PER_OBJEC-BEGDA.
      PER_INFO-OEND  = PER_OBJEC-ENDDA.
      PER_INFO-ISTAT = PER_STRUC-VISTAT.
      PER_INFO-RSIGN = PER_STRUC-VRSIGN.
      PER_INFO-RELAT = PER_STRUC-VRELAT.
      PER_INFO-PROZT = PER_STRUC-VPROZT.
      PER_INFO-BEGDA = PER_STRUC-VBEGDA.
      PER_INFO-ENDDA = PER_STRUC-VENDDA.
      PER_INFO-REALO = PER_OBJEC-OBJID.
      IF PER_OBJEC-STEXT IS INITIAL.
        PER_INFO-STEXT = PER_OBJEC-SHORT.
      ELSE.
        PER_INFO-STEXT = PER_OBJEC-STEXT.
      ENDIF.

*     persons group and sub group
      LOOP AT I0001 WHERE BEGDA =< PAR_DATE
                      AND ENDDA >= PAR_DATE.
        PER_INFO-PERSG = I0001-PERSG.
        PER_INFO-PERSK = I0001-PERSK.
        PER_INFO-ORG_OBJID = I0001-ORGEH. "STRO Note 549290
        EXIT.
      ENDLOOP.

*    delete all worktime info except the one which is use for percentage
*     check -> otherwise there exists inconsistences
      CASE USED_PERCK.
        WHEN DAILY.
          CLEAR: PER_WKHRS, PER_MOHRS, PER_YRHRS.
        WHEN WEEKLY.
          CLEAR: PER_DYHRS, PER_MOHRS, PER_YRHRS.
        WHEN MONTHLY.
          CLEAR: PER_DYHRS, PER_WKHRS, PER_YRHRS.
        WHEN YEARLY.
          CLEAR: PER_DYHRS, PER_WKHRS, PER_MOHRS.
      ENDCASE.

*     now get work time
      PERFORM GET_WORKTIME_IN_PERIOD
                USING PAR_PERCK PER_DYHRS PER_WKHRS
                      PER_MOHRS PER_YRHRS PER_INFO-WORKT.
*     donnot (!) weight work time with holder percentage
*     PER_INFO-WORKT = PER_INFO-WORKT *
*                      PER_STRUC-VPROZT / 100.
      APPEND PER_INFO.
    ENDIF.
  ENDIF.
ENDFORM.                    "PROCESS_PERSON

*---------------------------------------------------------------------*
*       FORM GET_WORKTIME_IN_PERIOD                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(PAR_PERCK)                                              *
*  -->  DYHRS                                                         *
*  -->  WKHRS                                                         *
*  -->  MOHRS                                                         *
*  -->  YRHRS                                                         *
*  -->  WORKT                                                         *
*---------------------------------------------------------------------*
FORM GET_WORKTIME_IN_PERIOD USING VALUE(PAR_PERCK)
                                  DYHRS WKHRS MOHRS YRHRS
                                  WORKT.

  DATA WRKT LIKE P1011.                "YBHP30K036762

* get proper work time
  CASE PAR_PERCK.
    WHEN DAILY.
      WORKT = DYHRS.
    WHEN WEEKLY.
      WORKT = WKHRS.
    WHEN MONTHLY.
      WORKT = MOHRS.
    WHEN YEARLY.
      WORKT = YRHRS.
  ENDCASE.
  IF WORKT IS INITIAL.
*  infotype 1011 initial for selected basic working hours
    WRKT-DYAVG = DYHRS.
    WRKT-WKAVG = WKHRS.
    WRKT-MOAVG = MOHRS.
    WRKT-YRAVG = YRHRS.
    CALL FUNCTION 'RH_WORKTIME_HOURS_CALCULATE'
      EXPORTING
        ACT_P1011             = WRKT
      IMPORTING
        ACT_P1011             = WRKT
      EXCEPTIONS
        T77S0_ENTRY_NOT_FOUND = 1
        OTHERS                = 2.
    IF SY-SUBRC EQ 0.
*     get proper work time
      CASE PAR_PERCK.
        WHEN DAILY.
          WORKT = WRKT-DYAVG.
        WHEN WEEKLY.
          WORKT = WRKT-WKAVG.
        WHEN MONTHLY.
          WORKT = WRKT-MOAVG.
        WHEN YEARLY.
          WORKT = WRKT-YRAVG.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.                    "GET_WORKTIME_IN_PERIOD

*       FORM PRINT_ORG_UNITS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ORG_TAB                                                       *
*  -->  POS_STRUC                                                     *
*  -->  PER_STRUC                                                     *
*  -->  ACT_FCODE                                                     *
*  -->  VALUE(ALL_ORGS)                                               *
*  -->  VALUE(ORG_TAB_INDEX)                                          *

*---------------------------------------------------------------------*
*       FORM GET_ORG_DATA                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ALL_WORKT                                                     *
*  -->  ORG_HEADER                                                    *
*  -->  ORG_OBJECT                                                    *
*  -->  ORG_STRUCT                                                    *
*  -->  POS_REDUN                                                     *
*  -->  POS_VACAN                                                     *
*  -->  PER_GROUP                                                     *
*  -->  ORG_TAB_INDEX                                                 *
*  -->  DCT_PER_NO                                                    *
*  -->  DCT_POS_NO                                                    *
*  -->  DCT_BPOS_NO                                                   *
*  -->  DCT_OPER_NO                                                   *
*  -->  DCT_OPOS_NO                                                   *
*  -->  DCT_BOPOS_NO                                                  *
*  -->  IND_PER_NO                                                    *
*  -->  IND_POS_NO                                                    *
*  -->  IND_BPOS_NO                                                   *
*  -->  IND_OPER_NO                                                   *
*  -->  IND_OPOS_NO                                                   *
*  -->  IND_BOPOS_NO                                                  *
*  -->  ORG_LEVEL                                                     *
*  -->  TOT_WRKT_PER                                                  *
*  -->  TOT_WRKT_POS                                                  *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
FORM GET_ORG_DATA TABLES ALL_WORKT   STRUCTURE P1011
                         ORG_HEADER  STRUCTURE OBJEC
                         ORG_OBJECT  STRUCTURE OBJEC
                         ORG_STRUCT  STRUCTURE STRUC
                         POS_REDUN   STRUCTURE P1014
                         POS_VACAN   STRUCTURE P1007
                         PER_GROUP   STRUCTURE P0001
                  USING  ORG_TAB_INDEX
                         DCT_PER_NO
                         DCT_POS_NO
                         DCT_OPER_NO
                         DCT_OPOS_NO
                         IND_PER_NO
                         IND_POS_NO
                         IND_OPER_NO
                         IND_OPOS_NO
                         ORG_LEVEL
                         TOT_WRKT_PER
                         TOT_WRKT_POS
                         SUBRC
                         DCT_BPOS_NO
                         DCT_BOPOS_NO
                         IND_BPOS_NO
                         IND_BOPOS_NO.

  DATA: ACT_ENTRY  LIKE SY-TABIX.
  DATA: ENTRY_BEFORE LIKE SY-TABIX.

  READ TABLE ORG_TAB INDEX ORG_TAB_INDEX.
  IF SY-SUBRC = 0.
    DCT_PER_NO = ORG_TAB-DCT_PER_NO.
    DCT_POS_NO = ORG_TAB-DCT_POS_NO.
    DCT_OPER_NO = ORG_TAB-DCT_OPER_NO.
    DCT_OPOS_NO = ORG_TAB-DCT_OPOS_NO.
    DCT_BPOS_NO = ORG_TAB-DCT_BPOS_NO.
    DCT_BOPOS_NO = ORG_TAB-DCT_BOPOS_NO.
    IND_BPOS_NO = ORG_TAB-IND_BPOS_NO.
    IND_BOPOS_NO = ORG_TAB-IND_BOPOS_NO.
    IND_PER_NO = ORG_TAB-IND_PER_NO.
    IND_POS_NO = ORG_TAB-IND_POS_NO.
    IND_OPER_NO = ORG_TAB-IND_OPER_NO.
    IND_OPOS_NO = ORG_TAB-IND_OPOS_NO.
    TOT_WRKT_PER = ORG_TAB-TOT_WRKT_PER.
    TOT_WRKT_POS = ORG_TAB-TOT_WRKT_POS.

*   prepare org unit for printing
    REFRESH: ORG_HEADER,
             ORG_OBJECT, ORG_STRUCT, PER_GROUP,
             POS_VACAN,  POS_REDUN,  ALL_WORKT.

*   generate header -> org unit data
    PERFORM FILL_OBJEC
              TABLES ORG_HEADER
              USING  PC-PLVAR           $ORGEH
                     ORG_TAB-ORG_OBJID  ORG_TAB-BEGDA
                     ORG_TAB-ENDDA      ORG_TAB-ISTAT
                     ORG_TAB-STEXT      ORG_TAB-ORG_OBJID.

*   generate header -> boss data
    LOOP AT POS_STRUC WHERE ORG_OBJID = ORG_TAB-ORG_OBJID
                        AND IST_BOS   < 9.
      LOOP AT PER_STRUC WHERE POS_OTYPE = POS_STRUC-OTYPE
                          AND POS_OBJID = POS_STRUC-OBJID
                          AND RSIGN     = $SPREL+0(1)
                          AND RELAT     = $SPREL+1(3).
*       get boss
        PERFORM FILL_OBJEC
               TABLES ORG_HEADER
               USING  PC-PLVAR        PER_STRUC-OTYPE
                      PER_STRUC-OBJID PER_STRUC-OBEG
                      PER_STRUC-OEND  PER_STRUC-OSTAT
                      PER_STRUC-STEXT PER_STRUC-REALO.
      ENDLOOP.
      IF SY-SUBRC > 0.
*       no holder found -> use position
        PERFORM FILL_OBJEC
               TABLES ORG_HEADER
               USING  PC-PLVAR            POS_STRUC-OTYPE
                      POS_STRUC-OBJID     POS_STRUC-OBEG
                      POS_STRUC-OEND      POS_STRUC-OSTAT
                      POS_STRUC-STEXT     POS_STRUC-OBJID.
      ENDIF.
    ENDLOOP.

*   now get org unit objects including structure
    CLEAR: ACT_ENTRY, ENTRY_BEFORE.
    LOOP AT POS_STRUC WHERE ORG_OBJID = ORG_TAB-ORG_OBJID
                        AND IST_MEM   < 9.

      ACT_ENTRY = ACT_ENTRY + 1.
*     fill object data
      PERFORM FILL_OBJEC
             TABLES ORG_OBJECT
             USING  PC-PLVAR        POS_STRUC-OTYPE
                    POS_STRUC-OBJID POS_STRUC-OBEG
                    POS_STRUC-OEND  POS_STRUC-OSTAT
                    POS_STRUC-STEXT POS_STRUC-OBJID.
*     fill structural data
      CLEAR ORG_STRUCT.
      ORG_STRUCT-SEQNR  = ACT_ENTRY.
      ORG_STRUCT-LEVEL  = POS_STRUC-LEVEL.
      ORG_STRUCT-OTYPE  = POS_STRUC-OTYPE.
      ORG_STRUCT-OBJID  = POS_STRUC-OBJID.
      ORG_STRUCT-VRSIGN = $OSREL+0(1).
      ORG_STRUCT-VRELAT = $OSREL+1(3).
      ORG_STRUCT-VBEGDA = POS_STRUC-BEGDA.
      ORG_STRUCT-VENDDA = POS_STRUC-ENDDA.
      ORG_STRUCT-VISTAT = POS_STRUC-IST_MEM.
      ORG_STRUCT-PNEXT  = POS_STRUC-PNEXT.
      ORG_STRUCT-PDOWN  = POS_STRUC-PDOWN.
*     pointer pup means here not related position
      ORG_STRUCT-PUP    = POS_STRUC-PUP.
      APPEND ORG_STRUCT.

*     for grafical respresentation modify data of object before
      IF ENTRY_BEFORE > 0.
        READ TABLE ORG_STRUCT INDEX ENTRY_BEFORE.
*       object before is one level higher
        IF ORG_STRUCT-PNEXT > 0.
          ORG_STRUCT-PNEXT = ACT_ENTRY.
        ENDIF.

*       attention pointer prev is used for the index of the next
*       position
        ORG_STRUCT-PPREV = ACT_ENTRY.

        MODIFY ORG_STRUCT INDEX ENTRY_BEFORE.
      ENDIF.
      ENTRY_BEFORE = ACT_ENTRY.

*     check if position is redundant
      IF POS_STRUC-REDUN <> SPACE.
        CLEAR POS_REDUN.
        POS_REDUN-PLVAR = PC-PLVAR.
        POS_REDUN-OTYPE = POS_STRUC-OTYPE.
        POS_REDUN-OBJID = POS_STRUC-OBJID.
        POS_REDUN-INFTY = REDUN_INFTY.
        POS_REDUN-BEGDA = PC-BEGDA.
        POS_REDUN-ENDDA = PC-BEGDA.
        POS_REDUN-ISTAT = POS_STRUC-IST_MEM.
        POS_REDUN-REDUN = POS_STRUC-REDUN.
        APPEND POS_REDUN.
      ELSE.
*       check if position is vacant or free
        IF ( NOT POS_STRUC-VACDT IS INITIAL ) OR
           ( NOT POS_STRUC-FREDT IS INITIAL ).
          CLEAR POS_VACAN.
          POS_VACAN-PLVAR = PC-PLVAR.
          POS_VACAN-OTYPE = POS_STRUC-OTYPE.
          POS_VACAN-OBJID = POS_STRUC-OBJID.
          POS_VACAN-INFTY = VACAN_INFTY.
          IF POS_STRUC-VACRC =< 4.
*           position is vacant
            POS_VACAN-BEGDA = POS_STRUC-VACDT.
            POS_VACAN-VACAN = 'X'.
          ELSE.
*           position is not vacant but free.
            POS_VACAN-BEGDA = POS_STRUC-FREDT.
          ENDIF.
          POS_VACAN-ENDDA = '99991231'.
          POS_VACAN-ISTAT = POS_STRUC-IST_MEM.
          APPEND POS_VACAN.
        ENDIF.
*       save work time
        CLEAR ALL_WORKT.
        ALL_WORKT-PLVAR = PC-PLVAR.
        ALL_WORKT-OTYPE = POS_STRUC-OTYPE.
        ALL_WORKT-OBJID = POS_STRUC-OBJID.
        ALL_WORKT-INFTY = WORKT_INFTY.
        ALL_WORKT-BEGDA = PC-BEGDA.
        ALL_WORKT-ENDDA = PC-BEGDA.
        ALL_WORKT-ISTAT = POS_STRUC-IST_MEM.
        ALL_WORKT-YRAVG = POS_STRUC-WORKT.
        APPEND ALL_WORKT.
      ENDIF.

*     now read persons to given position
      LOOP AT PER_STRUC WHERE POS_OTYPE = POS_STRUC-OTYPE
                          AND POS_OBJID = POS_STRUC-OBJID.
        ACT_ENTRY = ACT_ENTRY + 1.
*       fill object data
        PERFORM FILL_OBJEC
               TABLES ORG_OBJECT
               USING  PC-PLVAR        PER_STRUC-OTYPE
                      PER_STRUC-OBJID PER_STRUC-OBEG
                      PER_STRUC-OEND  PER_STRUC-OSTAT
                      PER_STRUC-STEXT PER_STRUC-REALO.
*       fill structural data
        CLEAR ORG_STRUCT.
        ORG_STRUCT-SEQNR  = ACT_ENTRY.
        ORG_STRUCT-LEVEL  = POS_STRUC-LEVEL + 1.
        ORG_STRUCT-OTYPE  = PER_STRUC-OTYPE.
        ORG_STRUCT-OBJID  = PER_STRUC-OBJID.
        ORG_STRUCT-PUP    = ENTRY_BEFORE.
        ORG_STRUCT-VRSIGN = PER_STRUC-RSIGN.
        ORG_STRUCT-VRELAT = PER_STRUC-RELAT.
        ORG_STRUCT-VBEGDA = PER_STRUC-BEGDA.
        ORG_STRUCT-VENDDA = PER_STRUC-ENDDA.
        ORG_STRUCT-VISTAT = PER_STRUC-ISTAT.
        ORG_STRUCT-VPROZT = PER_STRUC-PROZT.
        APPEND ORG_STRUCT.
*       save work time
        CLEAR ALL_WORKT.
        ALL_WORKT-PLVAR = PC-PLVAR.
        ALL_WORKT-OTYPE = PER_STRUC-OTYPE.
        ALL_WORKT-OBJID = PER_STRUC-OBJID.
        ALL_WORKT-INFTY = WORKT_INFTY.
        ALL_WORKT-BEGDA = PC-BEGDA.
        ALL_WORKT-ENDDA = PC-BEGDA.
        ALL_WORKT-ISTAT = PER_STRUC-ISTAT.
        ALL_WORKT-YRAVG = PER_STRUC-WORKT.

        IF PER_STRUC-OBJID = 0.
*         in case of external objects use UNAME for REALO
          ALL_WORKT-UNAME = PER_STRUC-REALO.
        ENDIF.

        APPEND ALL_WORKT.
*       save person's group
        CLEAR PER_GROUP.
        IF POS_STRUC-REDUN IS INITIAL.
          PER_GROUP-PERNR = PER_STRUC-OBJID.
          PER_GROUP-INFTY = '0001'.
          PER_GROUP-BEGDA = PC-BEGDA.
          PER_GROUP-ENDDA = PC-BEGDA.
          PER_GROUP-PERSG = PER_STRUC-PERSG.
          PER_GROUP-PERSK = PER_STRUC-PERSK.
          APPEND PER_GROUP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    CLEAR SUBRC.
  ELSE.
    SUBRC = 4.
  ENDIF.
ENDFORM.                    "GET_ORG_DATA

*---------------------------------------------------------------------*
*       FORM FILL_OBJEC                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJECT                                                        *
*  -->  PLVAR                                                         *
*  -->  OTYPE                                                         *
*  -->  OBJID                                                         *
*  -->  BEGDA                                                         *
*  -->  ENDDA                                                         *
*  -->  ISTAT                                                         *
*  -->  STEXT                                                         *
*  -->  REALO                                                         *
*---------------------------------------------------------------------*
FORM FILL_OBJEC TABLES OBJECT STRUCTURE OBJEC
                USING  VALUE(PLVAR)  LIKE PLOG-PLVAR
                       VALUE(OTYPE)  LIKE OBJEC-OTYPE
                       VALUE(OBJID)  LIKE OBJEC-OBJID
                       VALUE(BEGDA)  LIKE OBJEC-BEGDA
                       VALUE(ENDDA)  LIKE OBJEC-ENDDA
                       VALUE(ISTAT)  LIKE STRUC-VISTAT
                       VALUE(STEXT)  LIKE OBJEC-STEXT
                       VALUE(REALO)  TYPE ANY.
  CLEAR OBJECT.
  OBJECT-PLVAR = PLVAR.
  OBJECT-OTYPE = OTYPE.
  OBJECT-OBJID = OBJID.
  OBJECT-BEGDA = BEGDA.
  OBJECT-ENDDA = ENDDA.
  OBJECT-ISTAT = ISTAT.
  OBJECT-SHORT = STEXT.
  OBJECT-STEXT = STEXT.
  OBJECT-REALO = REALO.
  APPEND OBJECT.
ENDFORM.                    "FILL_OBJEC

*---------------------------------------------------------------------*
*       FORM INSERT_DEFAULT_POS                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  POS_INFO                                                      *
*  -->  POS_TAB                                                       *
*  -->  VALUE(PAR_DATE)                                               *
*  -->  VALUE(ORG_OBJID)                                              *
*---------------------------------------------------------------------*
FORM INSERT_DEFAULT_POS TABLES POS_INFO  STRUCTURE POS_STRUC
                               POS_TAB   STRUCTURE SEARCH_POS
                        USING  VALUE(PAR_DATE)
                               VALUE(ORG_OBJID).

  DATA: ACT_ENTRY  LIKE SY-TABIX.

  SELECT * FROM T528T WHERE SPRSL =  SY-LANGU
                        AND PLANS =  DEFAULT_POS
                        AND ENDDA GE PAR_DATE
                        AND BEGDA LE PAR_DATE.
    EXIT.
  ENDSELECT.
  IF SY-SUBRC > 0.
    CLEAR T528T.
    T528T-OTYPE = $PLSTE.
    T528T-PLANS = DEFAULT_POS.
    T528T-ENDDA = PAR_DATE.
    T528T-BEGDA = PAR_DATE.
    T528T-PLSTX = 'Default Planstelle'(INT).
  ENDIF.

* check if position was already handled
  POS_KEY-ORG_OBJID = ORG_OBJID.
  POS_KEY-OTYPE = T528T-OTYPE.
  POS_KEY-OBJID = T528T-PLANS.

  READ TABLE POS_INFO WITH KEY POS_KEY.
  IF SY-SUBRC > 0.
*   no entry found -> fill pos_info
    CLEAR: POS_INFO, ACT_ENTRY.
    POS_INFO-ORG_OBJID = ORG_OBJID.
    POS_INFO-OTYPE = T528T-OTYPE.
    POS_INFO-OBJID = T528T-PLANS.
    POS_INFO-OSTAT = '1'.
    POS_INFO-OBEG  = T528T-BEGDA.
    POS_INFO-OEND  = T528T-ENDDA.
    POS_INFO-LEVEL = 0.
    POS_INFO-IST_MEM = '1'.
    POS_INFO-IST_BOS = '9'.
    POS_INFO-STEXT = T528T-PLSTX.
*   append information since entry is new
    APPEND POS_INFO.
    DESCRIBE TABLE POS_INFO LINES ACT_ENTRY.

    POS_TAB-SEQNR = '99999'.
    POS_TAB-INDEX = ACT_ENTRY.
    APPEND POS_TAB.
  ENDIF.
ENDFORM.                    "INSERT_DEFAULT_POS

*---------------------------------------------------------------------*
*       FORM WRITE_OVERVIEW_HEADER                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_OVERVIEW_HEADER.
  SUMMARY.

  SET TITLEBAR 'TIO'.

  FORMAT COLOR COL_HEADING.
  WRITE: /(55)  'Überblick über Organisationseinheiten'(OVL),
        60(8) 'Stichtag'(RDY), PCHBEGDA DD/MM/YYYY, 80(1) ' '.
  ULINE.
  FORMAT COLOR OFF.
ENDFORM.                    "WRITE_OVERVIEW_HEADER

*---------------------------------------------------------------------*
*       FORM PRINT_OVERVIEW                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_OVERVIEW.

  DATA:  SUM_DIR_POS LIKE ORG_TAB-DCT_POS_NO,
         SUM_DIR_PER LIKE ORG_TAB-DCT_PER_NO,
         SUM_IND_POS LIKE ORG_TAB-IND_POS_NO,
         SUM_IND_PER LIKE ORG_TAB-IND_PER_NO.

* show overview
  NEW-PAGE LINE-SIZE 80.

  SET PF-STATUS 'INIT'.
  HEADER_FLAG = 'S'.
  DETAIL.

* print total results
  LOOP AT ORG_TAB.
    ORG_TAB_INDEX = SY-TABIX.
    RESERVE 10 LINES.

    ULINE  /1(79).
    WRITE: /1(1)  SY-VLINE.
    WRITE:  3(25) 'Organisationseinheit'(ORG).
    WRITE: 29(1)  SY-VLINE.
    FORMAT COLOR COL_HEADING.
    WRITE: 30(40) ORG_TAB-STEXT.
    HIDE ORG_TAB_INDEX.
    FORMAT COLOR OFF.
    WRITE: 79(1)  SY-VLINE.

    ULINE  /1(79).
    WRITE: /1(1)  SY-VLINE.
    WRITE:  3(25) 'Stufe'(STP).
    WRITE: 29(1)  SY-VLINE.
    FORMAT COLOR COL_GROUP INTENSIFIED.
    WRITE:  30    ORG_TAB-LEVEL.
    HIDE ORG_TAB_INDEX.
    FORMAT COLOR OFF INTENSIFIED OFF.
    WRITE: 79(1)  SY-VLINE.

    ULINE  /1(79).
    WRITE: /1(1)  SY-VLINE.
    WRITE:  3(25) 'Direkte Planstellen'(DCS).
    WRITE: 29(1)  SY-VLINE.
    FORMAT COLOR COL_TOTAL.
    WRITE:  30    ORG_TAB-DCT_POS_NO.
    HIDE ORG_TAB_INDEX.
    FORMAT COLOR OFF.
    WRITE: 47(15) 'davon besetzt:'(BES).
    FORMAT COLOR COL_TOTAL.
    WRITE: 63     ORG_TAB-DCT_BPOS_NO.
    HIDE ORG_TAB_INDEX.
    FORMAT COLOR OFF.
    WRITE: 79(1)  SY-VLINE.

*   show direct obsolet information only when filled
    IF ORG_TAB-DCT_OPOS_NO > 0 .
      WRITE: /1(1)  SY-VLINE.
      WRITE:  3(25) 'obsolet'(OBS).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    ORG_TAB-DCT_OPOS_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 47(15) 'davon besetzt:'(BES).
      FORMAT COLOR COL_TOTAL.
      WRITE: 63     ORG_TAB-DCT_BOPOS_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 29(1)  SY-VLINE.


      WRITE: 30(49) SY-ULINE.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 15(13) 'Summe'(SUM).
      WRITE: 29(1)  SY-VLINE.
      SUM_DIR_POS = ORG_TAB-DCT_POS_NO +
                    ORG_TAB-DCT_OPOS_NO.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    SUM_DIR_POS.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
    ENDIF.

    ULINE  /1(79).
    WRITE: /1(1)  SY-VLINE.
    WRITE:  3(25) 'Direkte Mitarbeiter'(DCP).
    WRITE: 29(1)  SY-VLINE.
    FORMAT COLOR COL_TOTAL.
    WRITE:  30    ORG_TAB-DCT_PER_NO.
    HIDE ORG_TAB_INDEX.
    FORMAT COLOR OFF.
    WRITE: 79(1)  SY-VLINE.

    IF ORG_TAB-DCT_OPER_NO > 0.
      WRITE: /1(1)  SY-VLINE.
      WRITE:  3(25) 'auf obsoleten Planstellen'(AOP).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    ORG_TAB-DCT_OPER_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 29(1)  SY-VLINE.
      WRITE: 30(49) SY-ULINE.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 15(13) 'Summe'(SUM).
      WRITE: 29(1)  SY-VLINE.
      SUM_DIR_PER = ORG_TAB-DCT_PER_NO +
                    ORG_TAB-DCT_OPER_NO.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    SUM_DIR_PER.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
    ENDIF.

*   show indirect informations only when filled
    IF ORG_TAB-IND_POS_NO > 0 OR
       ORG_TAB-IND_OPOS_NO > 0.
      ULINE  /1(79).
      WRITE: /1(1)  SY-VLINE.
      WRITE:  3(25) 'Indirekte Planstellen'(INS).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    ORG_TAB-IND_POS_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 47(15) 'davon besetzt:'(BES).
      FORMAT COLOR COL_TOTAL.
      WRITE: 63     ORG_TAB-IND_BPOS_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
    ENDIF.

*   show indirect obsolet information when filled
    IF ORG_TAB-IND_OPOS_NO > 0.
      WRITE: /1(1)  SY-VLINE.
      WRITE:  3(25) 'obsolet'(OBS).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    ORG_TAB-IND_OPOS_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 47(15) 'davon besetzt:'(BES).
      FORMAT COLOR COL_TOTAL.
      WRITE: 63     ORG_TAB-IND_BOPOS_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 29(1)  SY-VLINE.
      WRITE: 30(49) SY-ULINE.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 15(13) 'Summe'(SUM).
      WRITE: 29(1)  SY-VLINE.
      SUM_IND_POS = ORG_TAB-IND_POS_NO +
                    ORG_TAB-IND_OPOS_NO.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    SUM_IND_POS.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
    ENDIF.

    IF ORG_TAB-IND_PER_NO > 0 OR
     ( ORG_TAB-IND_OPER_NO > 0 ).
      ULINE  /1(79).
      WRITE: /1(1)  SY-VLINE.
      WRITE:  3(25) 'Indirekte Mitarbeiter'(INP).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    ORG_TAB-IND_PER_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
    ENDIF.

    IF ORG_TAB-IND_OPER_NO > 0.
      WRITE: /1(1)  SY-VLINE.
      WRITE:  3(25) 'auf obsoleten Planstellen'(AOP).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    ORG_TAB-IND_OPER_NO.
      HIDE ORG_TAB_INDEX.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 29(1)  SY-VLINE.
      WRITE: 30(49) SY-ULINE.
      WRITE: 79(1)  SY-VLINE.
      NEW-LINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE: 15(13) 'Summe'(SUM).
      WRITE: 29(1)  SY-VLINE.
      SUM_IND_PER = ORG_TAB-IND_PER_NO +
                    ORG_TAB-IND_OPER_NO.
      FORMAT COLOR COL_TOTAL.
      WRITE:  30    SUM_IND_PER.
      FORMAT COLOR OFF.
      WRITE: 79(1)  SY-VLINE.
    ENDIF.

    ULINE  /1(79).
    WRITE: /1(1) SY-VLINE.
    WRITE: 3(25) 'Soll Arbeitszeit'(SWT).
    WRITE: 29(1) SY-VLINE.
    FORMAT COLOR COL_TOTAL.
    WRITE: 30(18)  ORG_TAB-TOT_WRKT_POS NO-SIGN,
             (10) 'Stunden'(HRS), (20) PERCK_TEXT.
    FORMAT COLOR OFF.
    WRITE: 79(1) SY-VLINE.

    ULINE  /1(79).
    WRITE: /1(1) SY-VLINE.
    WRITE:   3(25) 'Ist Arbeitszeit'(IWT).
    WRITE: 29(1) SY-VLINE.
    FORMAT COLOR COL_TOTAL.
    WRITE: 30(18)  ORG_TAB-TOT_WRKT_PER NO-SIGN,
             (10)  'Stunden'(HRS), (20) PERCK_TEXT.
    FORMAT COLOR OFF.
    WRITE: 79(1) SY-VLINE.

*   calculate deviations
    TOT_WRKT_DEV = ORG_TAB-TOT_WRKT_PER -
                   ORG_TAB-TOT_WRKT_POS.

    IF ORG_TAB-TOT_WRKT_POS <> 0.
      TOT_WRKT_PRO =   TOT_WRKT_DEV /
                     ORG_TAB-TOT_WRKT_POS * 100.
    ELSE.
      TOT_WRKT_PRO = 0.
    ENDIF.

    IF TOT_WRKT_DEV > 0.
      ULINE  /1(79).
      WRITE: /1(1) SY-VLINE.
      WRITE: 3(25) 'Überdeckung'(OVR).
      WRITE: 29(1) SY-VLINE.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WRITE: 30(18) TOT_WRKT_DEV NO-SIGN,
               (10) 'Stunden'(HRS), (20) PERCK_TEXT.
      FORMAT COLOR OFF INTENSIFIED OFF.
      WRITE: 79(1) SY-VLINE.

      WRITE: /1(1) SY-VLINE.
      WRITE:       ' '.
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WRITE:  30(18) TOT_WRKT_PRO NO-SIGN,
                (10) 'Prozent'(PCT).
      FORMAT COLOR OFF INTENSIFIED OFF.
      WRITE: 79(1)  SY-VLINE.
      ULINE  /1(79).
    ELSE.
      ULINE  /1(79).
      WRITE: /1(1)  SY-VLINE.
      WRITE: 3(25)  'Unterdeckung'(BLL).
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WRITE: 30(18) TOT_WRKT_DEV NO-SIGN,
               (10) 'Stunden'(HRS), (20) PERCK_TEXT.
      FORMAT COLOR OFF INTENSIFIED OFF.
      WRITE: 79(1)  SY-VLINE.
      WRITE: /1(1)  SY-VLINE.
      WRITE:  ' '.
      WRITE: 29(1)  SY-VLINE.
      FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WRITE:  30(18) TOT_WRKT_PRO NO-SIGN,
                (15) 'Prozent'(PCT).
      FORMAT COLOR OFF INTENSIFIED OFF.
      WRITE: 79(1)  SY-VLINE.
      ULINE  /1(79).
    ENDIF.

    SKIP 5.
  ENDLOOP.
  CLEAR: ORG_TAB, ORG_TAB_INDEX, HEADER_FLAG.
  SKIP 3.
ENDFORM.                    "PRINT_OVERVIEW


*---------------------------------------------------------------------*
*       FORM MAINTAIN_OBJECT                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJEC                                                         *
*---------------------------------------------------------------------*
FORM MAINTAIN_OBJECT USING OBJEC STRUCTURE OBJEC.

* check if object is external
  SELECT SINGLE * FROM T77EO WHERE OTYPE = OBJEC-OTYPE.
  IF SY-SUBRC > 0.
*   internal object call transaction PP01 via cluster controller
    SET PARAMETER ID 'POD' FIELD OBJEC-ISTAT.
    SET PARAMETER ID 'BEG' FIELD OBJEC-BEGDA.
    SET PARAMETER ID 'END' FIELD OBJEC-ENDDA.

    PERFORM CTL(RHCLUC00) USING
          PC-PLVAR 'ORGP' OBJEC-OTYPE OBJEC-OBJID 'MAIN'.
  ELSE.
*   external object
    PERFORM SET_TRANSACTION IN PROGRAM (T77EO-PROG)
              USING 'AEND' OBJEC-REALO.
  ENDIF.
ENDFORM.                    "MAINTAIN_OBJECT

*---------------------------------------------------------------------*
*       FORM DESCRIBE_OBJECT                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJEC                                                         *
*---------------------------------------------------------------------*
FORM DESCRIBE_OBJECT USING OBJEC STRUCTURE OBJEC.
  SUBMIT RHDESC00 WITH PCHPLVAR EQ PC-PLVAR
                  WITH PCHOTYPE EQ OBJEC-OTYPE
                  WITH PCHOBJID EQ OBJEC-OBJID
                  WITH PCHOBEG  EQ OBJEC-BEGDA
                  WITH PCHOEND  EQ OBJEC-ENDDA
                  WITH PCHBEGDA EQ PC-BEGDA
                  WITH PCHENDDA EQ PC-BEGDA
                  WITH ANZ_ONLY EQ 'X'
                                           AND RETURN.
ENDFORM.                    "DESCRIBE_OBJECT

**---------------------------------------------------------------------*
**       FORM CHECK_MODE                                               *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
**  -->  MODE                                                          *
**  -->  MODE_TEXT                                                     *
**  -->  SUBRC                                                         *
**---------------------------------------------------------------------*
*FORM CHECK_MODE USING MODE MODE_TEXT SUBRC.
*  CLEAR: MODE_TEXT, SUBRC.
*  CASE MODE.
*    WHEN MODE_BATCH.
*      MODE_TEXT = 'Batchlauf'(MBT).
*    WHEN MODE_DETAIL.
*      MODE_TEXT = 'Detailanzeige'(MDT).
*    WHEN MODE_OVERVIEW.
*      MODE_TEXT = 'Übersicht'(MOV).
*    WHEN OTHERS.
*      SUBRC = 4.
*  ENDCASE.
*ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_PERCK                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PERCK                                                         *
*  -->  PERCK_TEXT                                                    *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
FORM CHECK_PERCK USING PERCK PERCK_TEXT SUBRC.
  CASE PERCK.
    WHEN DAILY.
      PERCK_TEXT = 'täglich'(DAY).
      CLEAR SUBRC.
    WHEN WEEKLY.
      PERCK_TEXT = 'wöchentlich'(WEK).
      CLEAR SUBRC.
    WHEN MONTHLY.
      PERCK_TEXT = 'monatlich'(MTH).
      CLEAR SUBRC.
    WHEN YEARLY.
      PERCK_TEXT = 'jährlich'(YER).
      CLEAR SUBRC.
    WHEN OTHERS.
      CLEAR PERCK_TEXT.
      SUBRC = 4.
  ENDCASE.
ENDFORM.                    "CHECK_PERCK

*---------------------------------------------------------------------*
*       FORM PROCESS_USER                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PER_INFO                                                      *
*  -->  POS_INFO                                                      *
*  -->  POS_IND                                                       *
*  -->  VALUE(PER_OBJEC)                                              *
*  -->  VALUE(PER_STRUC)                                              *
*---------------------------------------------------------------------*
FORM PROCESS_USER TABLES PER_INFO STRUCTURE PER_STRUC
                         POS_INFO STRUCTURE POS_STRUC
                         POS_IND  STRUCTURE SEARCH_POS
                  USING  VALUE(PER_OBJEC) STRUCTURE OBJEC
                         VALUE(PER_STRUC) STRUCTURE STRUC.

* get corresponding position
  READ TABLE POS_IND WITH KEY PER_STRUC-PUP.
  READ TABLE POS_INFO INDEX POS_IND-INDEX.

* check if per_info exists already
  LOOP AT PER_INFO WHERE POS_OTYPE = POS_INFO-OTYPE
                     AND POS_OBJID = POS_INFO-OBJID
                     AND OTYPE     = PER_OBJEC-OTYPE
                     AND ISTAT     = PER_STRUC-VISTAT
                     AND RSIGN     = PER_STRUC-VRSIGN
                     AND RELAT     = PER_STRUC-VRELAT
                     AND REALO     = PER_OBJEC-REALO.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC > 0.
*   new entry -> fill per_info
    CLEAR PER_INFO.
    PER_INFO-POS_OTYPE = POS_INFO-OTYPE.
    PER_INFO-POS_OBJID = POS_INFO-OBJID.
    PER_INFO-OTYPE = PER_OBJEC-OTYPE.
*   use realo instead of objid
    PER_INFO-REALO = PER_OBJEC-REALO.
    PER_INFO-OSTAT = PER_OBJEC-ISTAT.
    PER_INFO-OBEG  = PER_OBJEC-BEGDA.
    PER_INFO-OEND  = PER_OBJEC-ENDDA.
    PER_INFO-ISTAT = PER_STRUC-VISTAT.
    PER_INFO-RSIGN = PER_STRUC-VRSIGN.
    PER_INFO-RELAT = PER_STRUC-VRELAT.
    PER_INFO-PROZT = PER_STRUC-VPROZT.
    PER_INFO-BEGDA = PER_STRUC-VBEGDA.
    PER_INFO-ENDDA = PER_STRUC-VENDDA.
    IF PER_OBJEC-STEXT IS INITIAL.
      PER_INFO-STEXT = PER_OBJEC-SHORT.
    ELSE.
      PER_INFO-STEXT = PER_OBJEC-STEXT.
    ENDIF.

*   position work time is equal holder work time (assumption)
    PER_INFO-WORKT = POS_INFO-WORKT.
    APPEND PER_INFO.
  ENDIF.
ENDFORM.                    "PROCESS_USER

*---------------------------------------------------------------------*
*       FORM DETAIL_LIST_VIEWER                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ORG_TAB                                                       *
*  -->  POS_STRUC                                                     *
*  -->  PER_STRUC                                                     *
*  -->  O_VIEW                                                        *
*  -->  V_EXIT                                                        *
*  -->  D_EXTAB                                                       *
*  -->  VALUE(PERCK_TEXT)                                             *
*  -->  VALUE(INDEX)                                                  *
*---------------------------------------------------------------------*
FORM DETAIL_LIST_VIEWER TABLES ORG_TAB         STRUCTURE ORG_TAB
                               POS_STRUC       STRUCTURE POS_STRUC
                               PER_STRUC       STRUCTURE PER_STRUC
                               O_VIEW          STRUCTURE OVER
                        USING  V_EXIT        TYPE SLIS_EXIT_BY_USER
                               D_EXTAB       TYPE SLIS_EXTAB
                               VALUE(PERCK_TEXT) LIKE T77MT-MTEXT
                               VALUE(INDEX) LIKE SY-TABIX.
*
  DATA: LIST_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: LIST_LAYOUT   TYPE SLIS_LAYOUT_ALV.
  DATA: LIST_EVENT    TYPE SLIS_T_EVENT.
  DATA: D_SAVE(1)     TYPE C.
  DATA: D_VARIANT     TYPE DISVARIANT.
  DATA: DS_VARIANT    TYPE DISVARIANT.
  DATA: REPID         LIKE SY-REPID.
  DATA: D_SP_GROUP    TYPE SLIS_T_SP_GROUP_ALV.
  DATA: D_SORT          TYPE SLIS_T_SORTINFO_ALV.
  DATA: D_EVENT_EXIT    TYPE SLIS_T_EVENT_EXIT.
  DATA: NUMBER_LIST    TYPE I.
*
  CLEAR: TOP, S_DAY.
  CLEAR: ORGUNIT, ORGTEXT.
* initialize excluding fcode 'absolut'
  D_EXTAB-FCODE = $ABS.

  WRITE PC-BEGDA DD/MM/YYYY TO S_DAY.
  REPID = SY-REPID.

* get header information
  READ TABLE O_VIEW INDEX INDEX INTO TOP.
* ..Standard- und benutzerspezifische Speicherung
  D_SAVE = 'A'.

* get table entrys for lists in detail modus
  PERFORM GET_TAB_INFORMATION TABLES DETAIL_TAB LIST_OUT
                                     ORG_TAB  POS_STRUC PER_STRUC
                              USING  INDEX SUBRC
                                     ORGUNIT ORGTEXT PERCK_TEXT.

  DESCRIBE TABLE LIST_OUT LINES NUMBER_LIST.
  IF NOT NUMBER_LIST EQ 0.
*   get fieldcat for sequentiell list
    PERFORM FILL_SEQU_FIELDCAT USING LIST_FIELDCAT[] D_COL_QUEUE[].
*   activate variants
    PERFORM VARIANT_INIT USING DS_VARIANT REPID 'DLI2'.
*   fill table of events
    PERFORM GET_LIST_EVENT_D   USING LIST_EVENT.
*   layout for list
    PERFORM BUILD_LAYOUT_D     USING LIST_LAYOUT.
*   commentary for event top of page
    PERFORM COMMENT_BUILD_D    USING LIST_TOP_OF_PAGE S_DAY PERCK_TEXT.
*   sort
    PERFORM D_SEQ_SORT          USING D_SORT[].
*   special groups
    PERFORM D_SP_GROUP_BUILD   USING D_SP_GROUP[].
*   event for 'get back processing' before/after ALV functioncodes
    PERFORM GET_EVENT_EXIT   USING D_EVENT_EXIT[].


* ABAP LISTVIEWER EINSTUFIG
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*         I_INTERFACE_CHECK           = ' '
             I_CALLBACK_PROGRAM          = REPID
             I_CALLBACK_PF_STATUS_SET    = 'LIST_SET_PF_STATUS'
             I_CALLBACK_USER_COMMAND     = 'USER_COMMAND_D'
*         I_CALLBACK_TOP_OF_PAGE      = ' '
*         I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*         I_STRUCTURE_NAME            =
*         I_BACKGROUND_ID             = ' '
*         I_GRID_TITLE                =
             IS_LAYOUT                   = LIST_LAYOUT
             IT_FIELDCAT                 = LIST_FIELDCAT[]
*         IT_EXCLUDING                =
          IT_SPECIAL_GROUPS           = D_SP_GROUP[]
* because of note 0450293 d_sort will be cleared. So the order of "STFO
* positions is like priority and not like names.                  "STFO
*         IT_SORT                     = d_sort[]                  "STFO
*         IT_FILTER                   =
*         IS_SEL_HIDE                 =
*         I_DEFAULT                   = 'X'
          I_SAVE                      = D_SAVE
          IS_VARIANT                  = DS_VARIANT
          IT_EVENTS                    = LIST_EVENT[]
*         IT_EVENT_EXIT               = D_EVENT_EXIT[]
*    IMPORTING
*         E_EXIT_CAUSED_BY_CALLER     =
*         ES_EXIT_CAUSED_BY_USER      =
         TABLES
              T_OUTTAB                    = LIST_OUT
     EXCEPTIONS
          PROGRAM_ERROR               = 1
          OTHERS                      = 2
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    MESSAGE I115(5W).                  "Keine Daten vorhanden
  ENDIF.

ENDFORM.                               " DETAIL_LIST_VIEWER

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Layoutangaben der Detailliste im Overview-Modus
*----------------------------------------------------------------------*
*      -->P_LIST_LAYOUT  text                                          *
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT USING  P_LIST_LAYOUT TYPE SLIS_LAYOUT_ALV.

  DATA: D_HEADER(20) TYPE C,
        D_ITEM(20)   TYPE C.

  MOVE 'Planstelle'(POS) TO D_HEADER.
  MOVE 'Inhaber'(HLD)    TO D_ITEM.

*** Detailinfo auf Doppelklick ***
*  P_LIST_LAYOUT-F2CODE = 'INFO'.
*** optimiert Spaltenbr., s.d. alle Inhalte vollständig angezeigt werden
  P_LIST_LAYOUT-COLWIDTH_OPTIMIZE  = 'X'.
*** Ausgabe mit Streifenmuster (bei großen Listen)
*  p_list_layout-zebra              = 'X'.
*** totals for numc-fields possible
  P_LIST_LAYOUT-NUMC_SUM = 'X'.
*** show only totals
  P_LIST_LAYOUT-TOTALS_TEXT = 'Summe'(SUM).
*  p_list_layout-totals_only = 'X'.

ENDFORM.                               " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  GET_LIST_EVENT
*&---------------------------------------------------------------------*
*       Tabelle für die Ereignisse der Detailsicht im Overview-Modus
*----------------------------------------------------------------------*
*      -->LIST_EVENT  text                                             *
*----------------------------------------------------------------------*
FORM GET_LIST_EVENT USING  LIST_EVENT TYPE SLIS_T_EVENT.

  DATA: LS_EVENT TYPE SLIS_T_EVENT WITH HEADER LINE.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE     = 1
    IMPORTING
      ET_EVENTS       = LIST_EVENT
    EXCEPTIONS
      LIST_TYPE_WRONG = 1
      OTHERS          = 2.
*
  CLEAR LS_EVENT.
  READ TABLE LIST_EVENT WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                        INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE 'TOP_OF_PAGE' TO LS_EVENT-FORM.
    APPEND LS_EVENT TO LIST_EVENT.
  ENDIF.

  CLEAR LS_EVENT.
  READ TABLE LIST_EVENT WITH KEY NAME = SLIS_EV_END_OF_LIST
                        INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE 'END_OF_LIST_D' TO LS_EVENT-FORM.
    APPEND LS_EVENT TO LIST_EVENT.
  ENDIF.


ENDFORM.                               " GET_LIST_EVENT

*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       Text für Listkopf oder Listende                               *
*---------------------------------------------------------------------*
*  -->  P_LIST_TOP_OF_PAGE                                            *
*  -->  S_DAY                                                         *
*  -->  ORGUNIT                                                       *
*  -->  ORGTEXT                                                       *
*  -->  VALUE(TOP)                                                    *
*---------------------------------------------------------------------*
FORM COMMENT_BUILD USING    P_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER
                            S_DAY
                            ORGUNIT  TYPE OBJEC-OBJID
                            ORGTEXT  TYPE OBJEC-STEXT
                            VALUE(TOP) STRUCTURE OVER.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: P_OUT(10) TYPE C.
  DATA: P_BASIS(40) TYPE C.

  REFRESH P_LIST_TOP_OF_PAGE.

* Kopfinfo: Typ S
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Stichtag'(RDY).
  LS_LINE-INFO = S_DAY.
  APPEND LS_LINE TO P_LIST_TOP_OF_PAGE.
*
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Organisationseinheit'(ORG).
  CONCATENATE ORGUNIT ORGTEXT INTO LS_LINE-INFO SEPARATED BY SPACE.
  APPEND LS_LINE TO P_LIST_TOP_OF_PAGE.
*
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Arbeitszeitbasis'(WTB).
  CONCATENATE 'Stunden'(HRS) PERCK_TEXT INTO P_BASIS SEPARATED BY SPACE.
  LS_LINE-INFO = P_BASIS.
  APPEND LS_LINE TO P_LIST_TOP_OF_PAGE.

ENDFORM.                               " COMMENT_BUILD

*----------------------------------------------------------------------*
*  FORM TOP_OF_PAGE                                                    *
*----------------------------------------------------------------------*
*  Callback-routine für Listenkopfaufbereitung                         *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIST_TOP_OF_PAGE.

ENDFORM.                               "TOP_OF_PAGE
*----------------------------------------------------------------------*
*  FORM END_OF_LIST_D                                                  *
*----------------------------------------------------------------------*
*  Callback-routine für Listenkopfaufbereitung                         *
*----------------------------------------------------------------------*
FORM END_OF_LIST_D.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LIST_END_OF_PAGE
*           I_LOGO             =
            I_END_OF_LIST_GRID = 'X'.


ENDFORM.                               "END_OF_LIST_D


*----------------------------------------------------------------------*
*  LIST_SET_PF_STATUS                                                  *
*----------------------------------------------------------------------*
*  Callback-routine für Listenstatus                                   *
*----------------------------------------------------------------------*
FORM LIST_SET_PF_STATUS USING  VT_EXTAB TYPE SLIS_T_EXTAB.
*
  DATA: P_EXTAB TYPE SLIS_T_EXTAB WITH HEADER LINE.

  P_EXTAB[] = VT_EXTAB[].

  CALL FUNCTION 'RH_USER_VIEW_PARAMETER'
    IMPORTING
      OBJECT_KEY_FTEXT    = ACT_INFO-OKEY_FTEXT
      SHORT_FTEXT         = ACT_INFO-SHOR_FTEXT
      OBJECT_DATE_FTEXT   = ACT_INFO-ODAT_FTEXT
      RELATION_DATE_FTEXT = ACT_INFO-VDAT_FTEXT.

* insert fcode absolute or effectiv in p_extab
  IF D_EXTAB-FCODE EQ 'ABS'.
    READ TABLE P_EXTAB WITH KEY FCODE = 'EFF'.
    IF SY-SUBRC EQ 0.
      DELETE P_EXTAB INDEX SY-INDEX.
    ENDIF.
  ELSEIF D_EXTAB-FCODE EQ 'EFF'.
    READ TABLE P_EXTAB WITH KEY FCODE = 'ABS'.
    IF SY-SUBRC EQ 0.
      DELETE P_EXTAB INDEX SY-INDEX.
    ENDIF.
  ENDIF.
  APPEND D_EXTAB TO P_EXTAB.
  SET PF-STATUS 'STANDARD' EXCLUDING P_EXTAB.
  SET TITLEBAR 'STANDARD_D'.
*
ENDFORM.                               "LIST_SET_PF_STATUS


*---------------------------------------------------------------------*
*       FORM GET_OVER_DATA                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ORG_TAB                                                       *
*  -->  O_VIEW                                                        *
*  -->  O_OBJECT_TAB                                                  *
*  -->  O_WORKT_TAB                                                   *
*  -->  VALUE(PERCK_TEXT)                                             *
*---------------------------------------------------------------------*
FORM GET_OVER_DATA TABLES   ORG_TAB STRUCTURE ORG_TAB
                            O_VIEW  STRUCTURE OVER
                            O_OBJECT_TAB STRUCTURE OVER_OBJ
                            O_WORKT_TAB  STRUCTURE OVER_WKT
                            O_GRID_TAB   STRUCTURE OVER_STRUC
                   USING    VALUE(PERCK_TEXT) LIKE T77MT-MTEXT.

  DATA:  SUM_DIR_POS  LIKE ORG_TAB-DCT_POS_NO,
         SUM_DIR_BPOS LIKE ORG_TAB-DCT_BPOS_NO,
         SUM_DIR_PER  LIKE ORG_TAB-DCT_PER_NO,
         SUM_IND_POS  LIKE ORG_TAB-IND_POS_NO,
         SUM_IND_BPOS LIKE ORG_TAB-IND_BPOS_NO,
         SUM_IND_PER  LIKE ORG_TAB-IND_PER_NO,
         P_PERCK_TXT(15) TYPE C,
         P_WRKT_DEV  TYPE P DECIMALS 2,
         P_WRKT_PRO  TYPE P DECIMALS 2.
  DATA: COUNT_SORT TYPE I.
  DATA: BEGIN OF P_POS,
            COUNT(8) TYPE C,
            REST(22) TYPE C.
  DATA: END OF P_POS.

  DATA:  H_PACK_1(8) TYPE C, H_PACK_2(8) TYPE C.
  CLEAR: H_PACK_1, H_PACK_2.
  REFRESH O_VIEW.
  CLEAR COUNT_SORT.
  MOVE PERCK_TEXT TO P_PERCK_TXT.
* get data for list output
  LOOP AT ORG_TAB.
    CLEAR COUNT_SORT.
*   Id orgunit
    O_VIEW-ORGUNIT       = ORG_TAB-ORG_OBJID.
    O_OBJECT_TAB-ORGUNIT = ORG_TAB-ORG_OBJID.
    O_WORKT_TAB-ORGUNIT  = ORG_TAB-ORG_OBJID.
    O_GRID_TAB-ORGUNIT   = ORG_TAB-ORG_OBJID.
*   text and level
    O_VIEW-STEXT        = ORG_TAB-STEXT.
    O_VIEW-LEVEL        = ORG_TAB-LEVEL.
    O_OBJECT_TAB-STEXT  = ORG_TAB-STEXT.
    MOVE ORG_TAB-LEVEL TO O_OBJECT_TAB-LEVEL.
    O_WORKT_TAB-SHORT   = ORG_TAB-SHORT.
    O_WORKT_TAB-STEXT   = ORG_TAB-STEXT.
    O_WORKT_TAB-LEVEL   = ORG_TAB-LEVEL.
    O_GRID_TAB-SHORT   = ORG_TAB-SHORT.
    O_GRID_TAB-STEXT   = ORG_TAB-STEXT.
    O_GRID_TAB-LEVEL   = ORG_TAB-LEVEL.
*** row with 'DIRECT' information
    MOVE 'Direkte'(DRK) TO O_OBJECT_TAB-POS_TEXT.
*** sum information (all direct position inclusive obsolet)
    SUM_DIR_POS = ORG_TAB-DCT_POS_NO + ORG_TAB-DCT_OPOS_NO.
    O_VIEW-SUM_DCT_POS  = SUM_DIR_POS.
    CLEAR P_POS-COUNT.
    MOVE SUM_DIR_POS TO O_OBJECT_TAB-POS_COUNT.
    MOVE SUM_DIR_POS TO O_GRID_TAB-POS_COUNT.
*   sum direct person
    SUM_DIR_PER = ORG_TAB-DCT_PER_NO + ORG_TAB-DCT_OPER_NO.
    O_VIEW-SUM_DCT_PER  = SUM_DIR_PER.
    MOVE SUM_DIR_PER TO O_OBJECT_TAB-PER_COUNT.
    MOVE SUM_DIR_PER TO O_GRID_TAB-PER_COUNT.
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
    O_GRID_TAB-POS_SORT = COUNT_SORT.
    APPEND O_OBJECT_TAB.
*   position
    MOVE ORG_TAB-DCT_POS_NO  TO H_PACK_1.
    MOVE ORG_TAB-DCT_BPOS_NO TO H_PACK_2.
    CONCATENATE H_PACK_1 TEXT-OCC H_PACK_2
           INTO O_VIEW-DCT_POS SEPARATED BY SPACE.
    CLEAR: H_PACK_1, H_PACK_2.
*** row with occupied direct position
    MOVE 'besetzt'(OCC) TO O_OBJECT_TAB-POS_TEXT.
    SUM_DIR_BPOS = ORG_TAB-DCT_BPOS_NO + ORG_TAB-DCT_BOPOS_NO.
    MOVE SUM_DIR_BPOS TO O_OBJECT_TAB-POS_COUNT.
    MOVE SUM_DIR_BPOS TO  O_GRID_TAB-POS_COUNT.
    MOVE SPACE TO O_OBJECT_TAB-PER_COUNT.
    MOVE SPACE TO O_GRID_TAB-PER_COUNT.
*   person
    O_VIEW-DCT_PER = ORG_TAB-DCT_PER_NO.
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
    O_GRID_TAB-POS_SORT = COUNT_SORT.
    APPEND O_OBJECT_TAB.
    APPEND  O_GRID_TAB.
*** row with obsolet information
*   direct obsolet position
    MOVE TEXT-OBS TO O_OBJECT_TAB-POS_TEXT.
    MOVE ORG_TAB-DCT_OPOS_NO TO O_OBJECT_TAB-POS_COUNT.
    MOVE ORG_TAB-DCT_OPOS_NO  TO H_PACK_1.
    MOVE ORG_TAB-DCT_BOPOS_NO TO H_PACK_2.
    CONCATENATE H_PACK_1 TEXT-OCC H_PACK_2
           INTO O_VIEW-DCT_OPOS SEPARATED BY SPACE.
    CLEAR: H_PACK_1, H_PACK_2.
*   person on direct obsolet position
    O_VIEW-DCT_OPER = ORG_TAB-DCT_OPER_NO.
    MOVE ORG_TAB-DCT_OPER_NO TO O_OBJECT_TAB-PER_COUNT.
    MOVE ORG_TAB-DCT_OPER_NO TO  O_GRID_TAB-PER_COUNT.
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
    APPEND O_OBJECT_TAB.
    APPEND  O_GRID_TAB.
*** row indirect information
    MOVE 'Indirekte'(IND) TO O_OBJECT_TAB-POS_TEXT.
*   sum indirect position
    SUM_IND_POS = ORG_TAB-IND_POS_NO + ORG_TAB-IND_OPOS_NO.
    O_VIEW-SUM_IND_POS = SUM_IND_POS.
    MOVE SUM_IND_POS TO O_OBJECT_TAB-POS_COUNT.
*   sum indirect person
    SUM_IND_PER = ORG_TAB-IND_PER_NO + ORG_TAB-IND_OPER_NO.
    O_VIEW-SUM_IND_PER = SUM_IND_PER.
    MOVE SUM_IND_PER TO O_OBJECT_TAB-PER_COUNT.
    MOVE ORG_TAB-IND_POS_NO  TO H_PACK_1.
    MOVE ORG_TAB-IND_BPOS_NO TO H_PACK_2.
    CONCATENATE H_PACK_1 TEXT-OCC H_PACK_2
          INTO  O_VIEW-IND_POS SEPARATED BY SPACE.
    CLEAR: H_PACK_1, H_PACK_2.
*   person on indirect position
    O_VIEW-IND_PER      = ORG_TAB-IND_PER_NO.
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
    APPEND O_OBJECT_TAB.
*** row - sum of occupied indirect position
    MOVE 'besetzt'(OCC) TO O_OBJECT_TAB-POS_TEXT.
    SUM_IND_BPOS = ORG_TAB-IND_BPOS_NO + ORG_TAB-IND_BOPOS_NO.
    MOVE SUM_IND_BPOS TO O_OBJECT_TAB-POS_COUNT.
    MOVE SPACE TO O_OBJECT_TAB-PER_COUNT.
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
    APPEND O_OBJECT_TAB.
**+ row indirect obsolet information
*   indirect obsolet position
    MOVE 'obsolet'(OBS)       TO O_OBJECT_TAB-POS_TEXT.
    MOVE ORG_TAB-IND_OPOS_NO  TO H_PACK_1.
    MOVE ORG_TAB-IND_BOPOS_NO TO H_PACK_2.
    CONCATENATE H_PACK_1 TEXT-OCC H_PACK_2
          INTO  O_VIEW-IND_OPOS SEPARATED BY SPACE.
    CLEAR: H_PACK_1, H_PACK_2.
    MOVE ORG_TAB-IND_OPOS_NO TO O_OBJECT_TAB-POS_COUNT.
*   person on indirect obsolet position
    O_VIEW-IND_OPER     = ORG_TAB-IND_OPER_NO.
    MOVE ORG_TAB-IND_OPER_NO TO O_OBJECT_TAB-PER_COUNT.
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
    APPEND O_OBJECT_TAB.

*   worktime in hours
    O_VIEW-TOT_WRKT_POS      = ORG_TAB-TOT_WRKT_POS.
    O_WORKT_TAB-TOT_WRKT_POS = ORG_TAB-TOT_WRKT_POS.
*   worktime in percent
    O_VIEW-TOT_WRKT_PER      = ORG_TAB-TOT_WRKT_PER.
    O_WORKT_TAB-TOT_WRKT_PER = ORG_TAB-TOT_WRKT_PER.
*   calculate devicit
    P_WRKT_DEV = ORG_TAB-TOT_WRKT_PER -
                 ORG_TAB-TOT_WRKT_POS.
    IF ORG_TAB-TOT_WRKT_POS <> 0.
      P_WRKT_PRO = P_WRKT_DEV /
                   ORG_TAB-TOT_WRKT_POS * 100.
    ELSE.
      P_WRKT_PRO = 0.
    ENDIF.
*   devicit in hours
    O_VIEW-TOT_WRKT_DEV      = P_WRKT_DEV.
    O_WORKT_TAB-TOT_WRKT_DEV = P_WRKT_DEV.
*   devicit in percent
    O_VIEW-TOT_WRKT_PRO      = P_WRKT_PRO.
    O_WORKT_TAB-TOT_WRKT_PRO = P_WRKT_PRO.
*
    COUNT_SORT = COUNT_SORT + 1.
    O_OBJECT_TAB-POS_SORT = COUNT_SORT.
*
    O_WORKT_TAB-COL = $COL_ORG.
    APPEND O_VIEW.
    APPEND O_WORKT_TAB.

  ENDLOOP.

ENDFORM.                               " GET_OVER_DATA


*&---------------------------------------------------------------------*
*&      Module  LIST_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIST_DISPLAY OUTPUT.

  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM PRINT_OVERVIEW.
  LEAVE SCREEN.

ENDMODULE.                             " LIST_DISPLAY  OUTPUT

*----------------------------------------------------------------------*
* USER_COMMAND_D                                                       *
*----------------------------------------------------------------------*
* Callback-routine zur Behandlung anwendungsspezifischer Funktionen    *
*----------------------------------------------------------------------*
FORM USER_COMMAND_D USING R_UCOMM      LIKE SY-UCOMM
                          RS_SELFIELD  TYPE SLIS_SELFIELD.

  DATA: P_OBJEC     LIKE OBJEC.
  DATA: D_FIELD_TAB TYPE C_FIELD_TAB WITH HEADER LINE.
  DATA: FIRST       TYPE SLIS_TABNAME, SECOND TYPE SLIS_TABNAME.


  CASE R_UCOMM.

    WHEN '&OLX' OR '&OL0'.
*     get list layout info after processing variant
      PERFORM GET_LIST_LAYOUT_INFO USING C_FIELDCAT[].
*     set new col_pos for fieldcat
      PERFORM SET_NEW_COL_POS TABLES D_COL_QUEUE
                                     C_FIELDCAT.
*     set changed list_layout_info
      PERFORM SET_LIST_LAYOUT_INFO USING C_FIELDCAT[].

*     refresh list
      RS_SELFIELD-REFRESH = 'X'.

  ENDCASE.

ENDFORM.                               "USER_COMMAND_O



*---------------------------------------------------------------------*
*       FORM GET_DETAIL_INFO                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  POS_STRUC                                                     *
*  -->  PER_STRUC                                                     *
*  -->  FIRST_TAB                                                     *
*  -->  SECOND_TAB                                                    *
*  -->  RS_SELFIELD                                                   *
*  -->  P_OBJEC                                                       *
*---------------------------------------------------------------------*
FORM GET_DETAIL_INFO TABLES   POS_STRUC  STRUCTURE POS_STRUC
                              PER_STRUC  STRUCTURE PER_STRUC
                              DETAIL_TAB  STRUCTURE DETAIL_LIST
                     USING    RS_SELFIELD TYPE SLIS_SELFIELD
                              P_OBJEC       LIKE OBJEC.

  DATA: BEGIN OF H_HOLDER,
              TYP  LIKE OBJEC-OTYPE,
              ID   LIKE OBJEC-OBJID.
  DATA: END OF H_HOLDER.
  DATA: H_REALO     LIKE OBJEC-REALO.
  DATA: H_PER_REALO LIKE PER_STRUC-REALO.

* selection position detail info
  IF ( RS_SELFIELD-FIELDNAME = 'S_POS_ID' ) OR
     ( RS_SELFIELD-FIELDNAME = 'S_SHORT' ) OR
     ( RS_SELFIELD-FIELDNAME = 'S_TEXT' ).
    READ TABLE DETAIL_TAB INDEX RS_SELFIELD-TABINDEX.
    IF SY-SUBRC EQ 0.
      READ TABLE POS_STRUC WITH KEY OTYPE = $PLSTE
                                    OBJID = DETAIL_TAB-S_POS_ID.
      IF SY-SUBRC = 0.
        PERFORM FILL_P_OBJEC USING  P_OBJEC
                                    PC-PLVAR
                                    POS_STRUC-OTYPE
                                    POS_STRUC-OBJID
                                    POS_STRUC-OBEG
                                    POS_STRUC-OEND
                                    POS_STRUC-OSTAT
                                    POS_STRUC-SHORT
                                    POS_STRUC-STEXT
                                    ' '.
      ENDIF.
    ENDIF.
* detail information holder
  ELSEIF ( RS_SELFIELD-FIELDNAME = 'P_HOLDER' ) OR
         ( RS_SELFIELD-FIELDNAME = 'P_STEXT' ).
    READ TABLE DETAIL_TAB INDEX RS_SELFIELD-TABINDEX.
    IF SY-SUBRC EQ 0.
      CLEAR H_HOLDER.
      SPLIT DETAIL_TAB-P_HOLDER AT SPACE INTO H_HOLDER-TYP H_HOLDER-ID.
      IF H_HOLDER-TYP = 'P'.
        READ TABLE PER_STRUC WITH KEY OTYPE = H_HOLDER-TYP
                                      OBJID = H_HOLDER-ID.
      ELSE.
        CLEAR H_PER_REALO.
        MOVE H_HOLDER-ID TO H_PER_REALO.
        READ TABLE PER_STRUC WITH KEY OTYPE = H_HOLDER-TYP
                                      REALO = H_PER_REALO.
      ENDIF.
      IF SY-SUBRC = 0.
        CLEAR H_REALO.
        MOVE PER_STRUC-REALO TO H_REALO.
        PERFORM FILL_P_OBJEC USING  P_OBJEC
                                    PC-PLVAR
                                    PER_STRUC-OTYPE
                                    PER_STRUC-OBJID
                                    PER_STRUC-OBEG
                                    PER_STRUC-OEND
                                    PER_STRUC-OSTAT
                                    ' '
                                    PER_STRUC-STEXT
                                    H_REALO.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE S085(5A).    "Bitte den Cursor richtig positionieren
  ENDIF.

ENDFORM.                               " GET_DETAIL_INFO

*---------------------------------------------------------------------*
*       FORM VARIANT_INIT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  G_VARIANT                                                     *
*  -->  VALUE(G_REPID)                                                *
*---------------------------------------------------------------------*
FORM VARIANT_INIT USING G_VARIANT       LIKE DISVARIANT
                        VALUE(G_REPID)  LIKE SY-REPID
                        VALUE(G_HANDLE) TYPE SLIS_HANDL.

  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.
  G_VARIANT-HANDLE = G_HANDLE.

ENDFORM.                               " VARIANT_INIT

*---------------------------------------------------------------------*
*       FORM D_SP_GROUP_BUILD                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_D_SP_GROUP                                                  *
*---------------------------------------------------------------------*
FORM D_SP_GROUP_BUILD USING  P_D_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV.

  DATA: LS_SP_GROUP TYPE SLIS_SP_GROUP_ALV.
*
  CLEAR  LS_SP_GROUP.
  LS_SP_GROUP-SP_GROUP = 'A'.
  LS_SP_GROUP-TEXT     = 'ORGUNIT'.
  APPEND LS_SP_GROUP TO P_D_SP_GROUP.
*
  CLEAR LS_SP_GROUP.
  LS_SP_GROUP-SP_GROUP = 'B'.
  LS_SP_GROUP-TEXT     = 'POSITION'.
  APPEND LS_SP_GROUP TO P_D_SP_GROUP.
*
  CLEAR LS_SP_GROUP.
  LS_SP_GROUP-SP_GROUP = 'C'.
  LS_SP_GROUP-TEXT     = 'PERSON'.
  APPEND LS_SP_GROUP TO P_D_SP_GROUP.

ENDFORM.                               " d_SP_GROUP_BUILD

*---------------------------------------------------------------------*
*       FORM GET_HIERARCH_TAB                                         *
*---------------------------------------------------------------------*
*   Sammeln der Dateninformation für die Ausgabe mit dem              *
*   Listviewer; Ausgabe der Detail-Listen ist abhängig vom            *
*   selektierten Anzeigemodus.                                        *
*   Overview: Hierach.Liste: FIRST_TAB; SECOND_TAB                    *
*   Detailview: Einstuf. Liste; LIST_OUT                              *
*---------------------------------------------------------------------*
*  -->  FIRST_TAB           -Headerliste für hierarch. Ausgabe        *
*  -->  SECOND_TAB          -Itemliste für hierarch. Ausgabe          *
*  -->  LIST_OUT            -Einstufige Liste                        *
*  -->  ORG_TAB                                                       *
*  -->  POS_STRUC                                                     *
*  -->  PER_STRUC                                                     *
*  -->  VALUE(TAB_INDEX)                                              *
*  -->  SUBRC                                                         *
*  -->  P_ORGUNIT                                                     *
*  -->  P_ORGTEXT                                                     *
*  -->  VALUE(PERCK_TEXT)                                             *
*---------------------------------------------------------------------*
FORM GET_TAB_INFORMATION TABLES   DETAIL_TAB  STRUCTURE DETAIL_LIST
                                  LIST_OUT   STRUCTURE LIST_TAB
                                  ORG_TAB    STRUCTURE ORG_TAB
                                  POS_STRUC  STRUCTURE POS_STRUC
                                  PER_STRUC  STRUCTURE PER_STRUC
                         USING    VALUE(TAB_INDEX) LIKE SY-TABIX
                                  SUBRC          LIKE SY-SUBRC
                                  P_ORGUNIT      LIKE ORG_TAB-ORG_OBJID
                                  P_ORGTEXT      LIKE ORG_TAB-STEXT
                                  VALUE(PERCK_TEXT) LIKE T77MT-MTEXT.

  DATA: BEGIN(10) TYPE C,
        END(10)   TYPE C.
  DATA: P_PERSG_TEXT  LIKE T501T-PTEXT,
        P_PERSK_TEXT  LIKE T503T-PTEXT.

  DATA: EFF_WORKT  TYPE P DECIMALS 2,
        EFF_VPROZT TYPE P DECIMALS 2.                "VWMNOTE401990

  DATA: P_ORG_TAB LIKE ORG_TAB OCCURS 0 WITH HEADER LINE.

  DATA: P_PERCK_TXT(15) TYPE C.
  DATA: L_TEXT LIKE P1000-STEXT.

  DATA: RSUBRC LIKE SY-SUBRC. "STRO Note 549290

  REFRESH: DETAIL_TAB, LIST_OUT, P_ORG_TAB.
  CLEAR:   ORG_TAB.

  MOVE PERCK_TEXT TO P_PERCK_TXT.

*  IMPORT LT_ALV_CAT = LT_ALV_CAT FROM MEMORY ID 'ZFCAT'.
  perform fill_LT_ALV_CAT.
  PERFORM CREATE_DYNAMIC_TABLE.


*   detail modus
  P_ORG_TAB[] = ORG_TAB[].

  LOOP AT P_ORG_TAB.
    LOOP AT POS_STRUC WHERE ORG_OBJID = P_ORG_TAB-ORG_OBJID
                      AND IST_MEM   < 9.
      CLEAR:  LIST_OUT.
*     orgunit ID and short and long text
      LIST_OUT-O_ORG_ID = P_ORG_TAB-ORG_OBJID.
      LIST_OUT-O_SHORT  = P_ORG_TAB-SHORT.
      LIST_OUT-O_STEXT  = P_ORG_TAB-STEXT.
      LIST_OUT-COUNT = 1.

*      O_ORGID_MAIN      TYPE HRP1001-SOBID,
*             O_SHORT_MAIN       LIKE OBJEC-SHORT,
*             O_STEXT_MAIN       LIKE OBJEC-STEXT,
data: PRIOX type hrp1001-PRIOX.
clear: PRIOX.

      SELECT SINGLE SOBID PRIOX
        FROM HRP1001 INTO (LIST_OUT-O_ORGID_MAIN  , PRIOX )
        WHERE SCLAS = 'O'
        AND OTYPE = 'O'
        AND RSIGN = 'A'
        AND OBJID = LIST_OUT-O_ORG_ID
        AND BEGDA LE SY-DATUM
        AND ENDDA GE SY-DATUM.
      IF SY-SUBRC <> 0 .
        LIST_OUT-O_ORGID_MAIN = LIST_OUT-O_ORG_ID.
      ENDIF.

      SELECT SINGLE MC_SHORT MC_STEXT
        FROM HRP1000
        INTO (LIST_OUT-O_SHORT_MAIN , LIST_OUT-O_STEXT_MAIN)
        WHERE OBJID = LIST_OUT-O_ORGID_MAIN
        AND OTYPE = 'O'
        AND LANGU = SY-LANGU
        AND BEGDA LE SY-DATUM
        AND ENDDA GE SY-DATUM.




*     IF SY-SUBRC = 0 .
*
*      SELECT SINGLE SOBID
*        FROM HRP1001 INTO LIST_OUT-O_ORGID_MAIN
*        WHERE SCLAS = 'O'
*        AND OTYPE = 'O'
*        AND RSIGN = 'A'
*        AND OBJID = LIST_OUT-O_ORGID_MAIN
*        AND begda LE sy-datum
*        AND endda GE sy-datum.
*       IF SY-SUBRC = 0 .
*
*       ELSE.
*           LIST_OUT-O_ORGID_MAIN = LIST_OUT-O_ORG_ID.
*       ENDIF.
*     ELSE.
*        LIST_OUT-O_ORGID_MAIN = LIST_OUT-O_ORG_ID.
*     ENDIF.

*     key position
      LIST_OUT-S_POS_ID   = POS_STRUC-OBJID.
*     short and long text of position
      LIST_OUT-S_SHORT  = POS_STRUC-SHORT.
      LIST_OUT-S_TEXT   = POS_STRUC-STEXT.
*     status of position
      LIST_OUT-S_OSTAT  = POS_STRUC-OSTAT.
*     object period of position
      CLEAR: BEGIN, END.
      WRITE: POS_STRUC-OBEG DD/MM/YYYY TO BEGIN,
             POS_STRUC-OEND DD/MM/YYYY TO END.
      CONCATENATE BEGIN END
             INTO LIST_OUT-S_ODATE SEPARATED BY '-'.
*     hours position
      MOVE POS_STRUC-WORKT TO LIST_OUT-S_WORKT.
*     Soll*                                        "AP Note 569860
      MOVE POS_STRUC-WORKT TO LIST_OUT-S_WORKS.
*     check if position = boss
      IF POS_STRUC-BEGDA_BOS <> 0.
        LIST_OUT-S_LEITER  = 'Leiter'(MNG).
      ELSE.
        MOVE SPACE TO LIST_OUT-S_LEITER.
      ENDIF.
*     check if position is obsolet
      IF POS_STRUC-REDUN <> SPACE.
        LIST_OUT-S_BES_STAT  = 'obsolet'(OBS).
      ELSE.
*       check if position is vacant or free
        IF ( NOT POS_STRUC-VACDT IS INITIAL ) OR
           ( NOT POS_STRUC-FREDT IS INITIAL ).
*         position is vacant
          IF POS_STRUC-VACRC =< 4.
            WRITE: POS_STRUC-VACDT DD/MM/YYYY TO BEGIN.
            CONCATENATE TEXT-VVB BEGIN
              INTO LIST_OUT-S_BES_STAT SEPARATED BY SPACE.
          ELSE.
*           position is not vacant but free
            WRITE: POS_STRUC-FREDT DD/MM/YYYY TO BEGIN.
            CONCATENATE TEXT-NOO BEGIN
              INTO LIST_OUT-S_BES_STAT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDIF.
*
      READ TABLE PER_STRUC WITH KEY POS_OTYPE = POS_STRUC-OTYPE
                                    POS_OBJID = POS_STRUC-OBJID.
      IF SY-SUBRC <> 0.  "STRO Note 549290
        RSUBRC = 4.
      ELSE.
        IF PER_STRUC-POS_OBJID = '99999999'.           "AP Note 569533
          IF PER_STRUC-ORG_OBJID = POS_STRUC-ORG_OBJID.
            RSUBRC = 0.
          ELSE.
            RSUBRC = 7.
          ENDIF.
        ELSE.
          RSUBRC = 0.
        ENDIF.
      ENDIF.


      IF RSUBRC = 4.
        APPEND LIST_OUT.
      ELSEIF RSUBRC = 0.
*       read person to given position "STRO Note 549290
        LOOP AT PER_STRUC WHERE POS_OTYPE = POS_STRUC-OTYPE
                          AND POS_OBJID = POS_STRUC-OBJID.
*                and org_objid = pos_struc-org_objid. "AP Note 569533
          IF PER_STRUC-POS_OBJID = '99999999'           "AP Note 569533
            AND PER_STRUC-ORG_OBJID <> POS_STRUC-ORG_OBJID.
            EXIT.
          ENDIF.



*         holder type
          LIST_OUT-P_OTYPE    = PER_STRUC-OTYPE.
*         holder key
          IF PER_STRUC-OTYPE = 'P'.
            MOVE PER_STRUC-OBJID TO LIST_OUT-P_HOLDER.
          ELSE.
            CONCATENATE PER_STRUC-OTYPE PER_STRUC-REALO
                        INTO LIST_OUT-P_HOLDER SEPARATED BY SPACE.
          ENDIF.

          IF LIST_OUT-P_HOLDER IS NOT INITIAL.

            submit Z6HR026R_CTC_DETAILS_2
             with PNPTIMR1 = 'X'
             with PNPPERNR-LOW = LIST_OUT-P_HOLDER
             exporting list to memory and return.

             IMPORT <TABLE> = <TABLE> FROM MEMORY ID 'ZCOUNT'.
*             tot_FIX TYPE P DECIMALS 2, "TOTAL FIXED PAY
*             PERFORMANCE TYPE P DECIMALS 2," PERFORMANCE INCENTIVE
*             CTC TYPE P DECIMALS 2," TOTAL CTC

              LOOP AT <TABLE> ASSIGNING <DYNLINE>.

              ASSIGN COMPONENT 47 OF STRUCTURE <DYNLINE> TO <FLD>.
              LIST_OUT-tot_FIX = <FLD>.
              ASSIGN COMPONENT 48 OF STRUCTURE <DYNLINE> TO <FLD>.
              LIST_OUT-PERFORMANCE = <FLD>.
              ASSIGN COMPONENT 49 OF STRUCTURE <DYNLINE> TO <FLD>.
              LIST_OUT-CTC = <FLD>.
              ENDLOOP.

*              MOVE <TABLE>-CTC1 TO LIST_OUT-tot_FIX.


          ENDIF.
*         relationship period
          CLEAR: BEGIN, END.
          WRITE: PER_STRUC-BEGDA TO BEGIN,
                 PER_STRUC-ENDDA TO END.
          CONCATENATE BEGIN END
                      INTO LIST_OUT-V_DATE SEPARATED BY '-'.
*         status relationship
          LIST_OUT-P_ISTAT = PER_STRUC-ISTAT.
*         name of holder
          LIST_OUT-P_STEXT   = PER_STRUC-STEXT.
*         status of holder
          LIST_OUT-P_ISTAT   = PER_STRUC-OSTAT.
*         object period holder
          CLEAR: BEGIN, END.
          WRITE: PER_STRUC-OBEG DD/MM/YYYY TO BEGIN,
                 PER_STRUC-OEND DD/MM/YYYY TO END.
          CONCATENATE BEGIN END
                 INTO LIST_OUT-P_ODATE SEPARATED BY '-'.
*         hours holder
          MOVE PER_STRUC-WORKT TO LIST_OUT-P_WORKT.
*         Soll*                                        "AP Note 569860
          IF LAST_OBJID <> 0 AND
                        PER_STRUC-POS_OBJID = LAST_OBJID.
            CLEAR LIST_OUT-S_WORKS.
          ENDIF.
*         BesProzent -absolut
          MOVE PER_STRUC-PROZT TO LIST_OUT-P_ABES.
*         effektive Arbeitszeit
          EFF_WORKT = PER_STRUC-WORKT * PER_STRUC-PROZT / 100.
          LIST_OUT-P_EWORK   = EFF_WORKT.
*         effektiver Besetzungsprozentsatz
          IF POS_STRUC-WORKT > 0.
            EFF_VPROZT = PER_STRUC-PROZT * PER_STRUC-WORKT
                         / POS_STRUC-WORKT.
          ENDIF.
          LIST_OUT-P_EBES   = EFF_VPROZT.
*         Mitarbeitergruppe und -kreis
*         Nachfolger oder Stellvertreter
          IF PER_STRUC-RSIGN = 'A' AND PER_STRUC-RELAT = '008'.
            IF PER_STRUC-OTYPE = 'P'.
              PERFORM RE_PERSG_PERSK(SAPLRHSD) USING PER_STRUC-PERSG
                                              PER_STRUC-PERSK
                                              P_PERSG_TEXT P_PERSK_TEXT.
              LIST_OUT-P_PERSG   = P_PERSG_TEXT.
              LIST_OUT-P_PERSK   = P_PERSK_TEXT.
            ELSE.
              LIST_OUT-P_PERSG   = 'Benutzer'(USR).
              LIST_OUT-P_PERSK   = SPACE.
            ENDIF.
          ELSE.
            IF PER_STRUC-RSIGN = 'A' AND PER_STRUC-RELAT = '009'.
              LIST_OUT-P_PERSG   = 'Nachfolger'(XXX).
              LIST_OUT-P_PERSK   = SPACE.
            ELSEIF PER_STRUC-RSIGN = 'A' AND PER_STRUC-RELAT = '010'.
              LIST_OUT-P_PERSG   = 'Stellvertreter'(YYY).
              LIST_OUT-P_PERSK   = SPACE.
            ENDIF.
          ENDIF.
          LAST_OBJID = PER_STRUC-POS_OBJID.  "AP Note 569860

          APPEND LIST_OUT.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

DELETE LIST_OUT WHERE S_BES_STAT IS NOT INITIAL.
SORT LIST_OUT BY P_HOLDER S_POS_ID.
DELETE ADJACENT DUPLICATES FROM  LIST_OUT.
ENDFORM.                               "GET_TAB_INFORMATION

*---------------------------------------------------------------------*
*       FORM FILL_HIER_FIELDCAT                                       *
*---------------------------------------------------------------------*
*       Definition des Feldkataloges für hierarchische Listausgabe    *
*---------------------------------------------------------------------*
*  -->  LIST_FIELDCAT                                                 *
*  -->  D_COL_QUEUE                                                   *
*  -->  VALUE(P_ACT_INFO)                                             *
*  -->  VALUE(D_TABNAME_HEADER)                                       *
*  -->  VALUE(D_TABNAME_ITEM)                                         *
*---------------------------------------------------------------------*
FORM FILL_HIER_FIELDCAT USING LIST_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV
                              D_COL_QUEUE   TYPE COL_QUEUE.

  DATA: ACT_COL TYPE I.

  CLEAR ACT_COL.
  REFRESH LIST_FIELDCAT.

* position id
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'S_POS_ID'         "fieldname
                                         12                 "outputlen
                                         'ID'(IDO)          "seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         'X'               "no_out
                                         'A'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* position: short text
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'S_SHORT'          "fieldname
                                         20                 "outputlen
                                         'Kuerzel'(SHO)     "seltext_l
                                        'DETAIL_TAB'        "tabname
                                         ' '                "icon
                                         'X'               "no_out
                                         'A'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         ' '.               "datatype

* position: long text
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'S_TEXT'           "fieldname
                                         20                 "outputlen
                                         'Planstelle'(POS)  "seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         ' '               "no_out
                                         'A'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* position object period
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                         'S_ODATE'          "fieldname
                                         24                 "outputlen
                                         'Objektzeitraum'(VAL)"seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         'X'                  "no_out
                                         'A'                  "sp_group
                                         ' '                  "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* position is boss
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL             "col_pos
                                         ' '                 "row_pos
                                         'S_LEITER'         "fieldname
                                         3                  "outputlen
*                                      'Vorgesetzte'(VOR)     "seltext_l
                                       'Leiter'(LVB)
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         ' '                 "no_out
                                         'A'                 "sp_group
                                         ' '                 "do_sum
                                         'C'                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* position is obsolet/vacant/unoccupied since
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL             "col_pos
                                         ' '                 "row_pos
                                         'S_BES_STAT'       "fieldname
                                         12                 "outputlen
                                     'Besetzungsstatus'(SAS)"seltext_l
                                        'DETAIL_TAB'        "tabname
                                         ' '                "icon
                                         ' '                 "no_out
                                         'A'                 "sp_group
                                         ' '                 "do_sum
                                         'C'                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* relationsship period between postion and holder
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                         'V_DATE'           "fieldname
                                          24                "outputlen
                                  'Verknüpfungszeitraum'(ASH)"seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                        'X'                  "no_out
                                        ' '                   "sp_group
                                        ' '                   "do_sum
                                        ' '                 "just
                                        ' '                 "emphasize
                                        ' '.                "datatype
* holder object id
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL          "col_pos
                                         ' '              "row_pos
                                         'P_HOLDER'         "fieldname
                                         12                 "outputlen
                                         'Personalnr'(PNR)  "seltext_l
                                        'DETAIL_TAB'        "tabname
                                         ' '                "icon
                                         'X'              "no_out
                                         'B'              "sp_group
                                         ' '              "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* holder name
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL             "col_pos
                                         ' '                 "row_pos
                                         'P_STEXT'          "fieldname
                                         20                 "outputlen
                                         'Mitarbeiter'(PER) "seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         ' '                 "no_out
                                         'B'                 "sp_group
                                         ' '                 "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* holder object period
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                USING  ACT_COL                "col_pos
                                       ' '                    "row_pos
                                       'P_ODATE'            "fieldname
                                       24                   "outputlen
                                       'Objektzeitraum'(VAL)"seltext_l
                                       'DETAIL_TAB'         "tabname
                                       ' '                  "icon
                                       'X'                   "no_out
                                       'B'                    "sp_group
                                       ' '                    "do_sum
                                       ' '                  "just
                                       ' '                  "emphasize
                                       ' '.                 "datatype
* work time of position
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'S_WORKT'             "fieldname
                                      6                     "outputlen
                                      'Soll Arbeitszeit'(SWT)"seltext_l
                                     'DETAIL_TAB'           "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      'A'                     "sp_group
                                      ' '                     "do_sum
                                      'R'                   "just
                                      ' '                   "emphasize
                                      'DEC'.                "datatype
* work time-> Soll*                                    "AP Note 569860
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'S_WORKS'             "fieldname
                                      6                     "outputlen
            'Soll Arbeitszeit zur Summierung'(SWS)          "seltext_l
                                     'DETAIL_TAB'           "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      'A'                     "sp_group
                                      ' '                     "do_sum
                                      'R'                   "just
                                      ' '                   "emphasize
                                      'DEC'.                "datatype

* work time of holder
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                USING  ACT_COL                "col_pos
                                       ' '                    "row_pos
                                       'P_WORKT'            "fieldname
                                       6                    "outputlen
                                       'Ist Arbeitszeit'(IWT)"seltext_l
                                       'DETAIL_TAB'         "tabname
                                       ' '                  "icon
                                       ' '                    "no_out
                                       'A'                    "sp_group
                                       ' '                    "do_sum
                                       'R'                  "just
                                       ' '                  "emphasize
                                       'DEC'.               "datatype
* Besetzungsprozentsatz
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL            "col_pos
                                         ' '                "row_pos
                                         'P_ABES'           "fieldname
                                         6                  "outputlen
                                         'BesProzent'(BPZ)  "seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         ' '                "no_out
                                         'A'                "sp_group
                                         ' '                "do_sum
                                         'C'                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* work time of holder
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                USING  ACT_COL                "col_pos
                                       ' '                    "row_pos
                                       'P_EWORK'            "fieldname
                                       6                    "outputlen
                                       'eff.Arbeitszeit'(EWT)"seltext_l
                                      'DETAIL_TAB'          "tabname
                                       ' '                  "icon
                                       'X'                    "no_out
                                       'C'                    "sp_group
                                       ' '                    "do_sum
                                       'C'                  "just
                                       ' '                  "emphasize
                                       'DEC'.               "datatype
* Besetzungsprozentsatz
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL            "col_pos
                                         ' '                "row_pos
                                         'P_EBES'           "fieldname
                                         6                  "outputlen
                                         'eff.BesProz'(EBP) "seltext_l
                                         'DETAIL_TAB'       "tabname
                                         ' '                "icon
                                         'X'                "no_out
                                         'C'                "sp_group
                                         ' '                "do_sum
                                         'C'                "just
                                         ' '                "emphasize
                                         ' '.               "datatype
* holder employee group
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'P_PERSG'             "fieldname
                                      15                    "outputlen
                                      'Mitarbeitergruppe'(MGR)"seltext_l
                                     'DETAIL_TAB'           "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      ' '                     "sp_group
                                      ' '                     "do_sum
                                      'C'                   "just
                                      ' '                   "emphasize
                                      ' '.                  "datatype
* holder EE subgroup
  PERFORM FILL_HIER_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'P_PERSK'             "fieldname
                                      15                    "outputlen
                                      'Mitarbeiterkreis'(MKR)"seltext_l
                                      'DETAIL_TAB'          "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      ' '                     "sp_group
                                      ' '                     "do_sum
                                      'C'                   "just
                                      ' '                   "emphasize
                                      ' '.                  "datatype


*
PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'COUNT'             "fieldname
                                      20                    "outputlen
                                      'count'(CNT) "seltext_l
                                      'LIST_OUT'            "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      'C'                     "sp_group
                                      ' '                     "do_sum
                                      'C'                   "just
                                      ' '                   "emphasize
                                      'NUMC'                "datatype
                                      'I'                   "inttype
                                      ' '                   "lowercase
                                      ' '
                                      ' '.

*      O_ORGID_MAIN      TYPE HRP1001-SOBID,
*             O_SHORT_MAIN       LIKE OBJEC-SHORT,
*             O_STEXT_MAIN       LIKE OBJEC-STEXT,

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'O_ORGID_MAIN'             "fieldname
                                        20                    "outputlen
                                        'MAIN ORG.UNIT'(MOU) "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'CHAR'                "datatype
                                        'C'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.
*
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'O_SHORT_MAIN'             "fieldname
                                        20                    "outputlen
                                        'Main ORG.Unit txt'(MOT) "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'CHAR'                "datatype
                                        'C'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'O_STEXT_MAIN'             "fieldname
                                        20                    "outputlen
                                        'Main ORG.Unit txt'(MOT) "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'CHAR'                "datatype
                                        'C'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'TOT_FIX'             "fieldname
                                        20                    "outputlen
                                        'TOTAL FIXED PAY'      "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'NUMC'                "datatype
                                        'I'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'PERFORMANCE'             "fieldname
                                        20                    "outputlen
                                        'PERFORMANCE INCENTIVE'      "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'NUMC'                "datatype
                                        'I'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'CTC'             "fieldname
                                        20                    "outputlen
                                        'TOTAL CTC'      "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'NUMC'                "datatype
                                        'I'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.
*

ENDFORM.                               " FILL_HIER_FIELDCAT

*---------------------------------------------------------------------*
*       FORM FILL_HIER_FIELDCAT_LINE                                  *
*---------------------------------------------------------------------*
*      Unterroutine zum Füllen des Feldkataloges                      *
*---------------------------------------------------------------------*
*  -->  LIST_FIELDCAT                                                 *
*  -->  P_COL_QUEUE      Text                                         *
*  -->  ACT_COL          Position of the column                       *
*  -->  VALUE(ROW_POS)   Output in row                                *
*  -->  VALUE(FIELDNAME) Feldname der internen Tabelle                *
*  -->  VALUE(OUTPUTLEN) Ausgabelänge                                 *
*  -->  VALUE(SELTEXT_L) long key word                                *
*  -->  VALUE(TABNAME)   name der internen Tabelle                    *
*  -->  VALUE(ICON)        'X': Spaltenausgabe als Icon               *
*  -->  VALUE(NO_OUT)      'X': inaktive Spalte                       *
*  -->  VALUE(SP_GROUP)                                               *
*  -->  VALUE(DO_SUM)                                                 *
*  -->  VALUE(JUST)      Ausgabejustierung (R)ight,(L)eft,(C)enter    *
*---------------------------------------------------------------------*
FORM FILL_HIER_FIELDCAT_LINE
              TABLES LIST_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV
                     P_COL_QUEUE   TYPE COL_QUEUE
              USING  ACT_COL          TYPE I
                     VALUE(ROW_POS)   TYPE SY-CUROW
                     VALUE(FIELDNAME) TYPE SLIS_FIELDCAT_ALV-FIELDNAME
                     VALUE(OUTPUTLEN) TYPE SLIS_FIELDCAT_ALV-OUTPUTLEN
                     VALUE(SELTEXT_L) TYPE ANY
                     VALUE(TABNAME)   TYPE SLIS_FIELDCAT_ALV-TABNAME
                     VALUE(ICON)      TYPE SLIS_FIELDCAT_ALV-ICON
                     VALUE(NO_OUT)    TYPE SLIS_FIELDCAT_ALV-NO_OUT
                     VALUE(SP_GROUP)  TYPE SLIS_FIELDCAT_ALV-SP_GROUP
                     VALUE(DO_SUM)    TYPE SLIS_FIELDCAT_ALV-DO_SUM
                     VALUE(JUST)      TYPE SLIS_FIELDCAT_ALV-JUST
                     VALUE(EMPHASIZE) TYPE SLIS_FIELDCAT_ALV-EMPHASIZE
                     VALUE(DATATYPE)  TYPE SLIS_FIELDCAT_ALV-DATATYPE.

  DATA: FIELD_HEAD_STRUC TYPE SLIS_FIELDCAT_ALV.
  DATA: P_COL_FIELD      TYPE COL_STRUC.

  ACT_COL = ACT_COL + 1.

  CLEAR FIELD_HEAD_STRUC. CLEAR P_COL_FIELD.

  FIELD_HEAD_STRUC-COL_POS    =  ACT_COL.
  P_COL_FIELD-COL_POS         =  ACT_COL.
  FIELD_HEAD_STRUC-ROW_POS    =  ROW_POS.
  FIELD_HEAD_STRUC-FIELDNAME  =  FIELDNAME.
  P_COL_FIELD-FIELDNAME       =  FIELDNAME.
  FIELD_HEAD_STRUC-OUTPUTLEN  =  OUTPUTLEN.
  FIELD_HEAD_STRUC-SELTEXT_L  =  SELTEXT_L.
  FIELD_HEAD_STRUC-TABNAME    =  TABNAME.
  P_COL_FIELD-TABNAME         =  TABNAME.
  FIELD_HEAD_STRUC-ICON       =  ICON.
  FIELD_HEAD_STRUC-NO_OUT     =  NO_OUT.
  FIELD_HEAD_STRUC-SP_GROUP   =  SP_GROUP.
  FIELD_HEAD_STRUC-DO_SUM     =  DO_SUM.
  FIELD_HEAD_STRUC-JUST       =  JUST.
  FIELD_HEAD_STRUC-EMPHASIZE  =  EMPHASIZE.
  FIELD_HEAD_STRUC-DATATYPE   =  DATATYPE.
  APPEND FIELD_HEAD_STRUC TO LIST_FIELDCAT.
  APPEND P_COL_FIELD TO P_COL_QUEUE.

ENDFORM.                               " FILL_HIER_FIELDCAT_LINE
*---------------------------------------------------------------------*
*       FORM FILL_SEQ_FIELDCAT_LINE                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LIST_FIELDCAT                                                 *
*  -->  P_COL_QUEUE                                                   *
*  -->  ACT_COL                                                       *
*  -->  VALUE(ROW_POS)                                                *
*  -->  VALUE(FIELDNAME)                                              *
*  -->  VALUE(OUTPUTLEN)                                              *
*  -->  VALUE(SELTEXT_L)                                              *
*  -->  VALUE(TABNAME)                                                *
*  -->  VALUE(ICON)                                                   *
*  -->  VALUE(NO_OUT)                                                 *
*  -->  VALUE(SP_GROUP)                                               *
*  -->  VALUE(DO_SUM)                                                 *
*  -->  VALUE(JUST)                                                   *
*  -->  VALUE(EMPHASIZE)                                              *
*  -->  VALUE(DATATYPE)                                               *
*---------------------------------------------------------------------*
FORM FILL_SEQ_FIELDCAT_LINE
              TABLES LIST_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV
                     P_COL_QUEUE   TYPE COL_QUEUE
              USING  ACT_COL          TYPE I
                     VALUE(ROW_POS)   TYPE SY-CUROW
                     VALUE(FIELDNAME) TYPE SLIS_FIELDCAT_ALV-FIELDNAME
                     VALUE(OUTPUTLEN) TYPE SLIS_FIELDCAT_ALV-OUTPUTLEN
                     VALUE(SELTEXT_L) TYPE ANY
                     VALUE(TABNAME)   TYPE SLIS_FIELDCAT_ALV-TABNAME
                     VALUE(ICON)      TYPE SLIS_FIELDCAT_ALV-ICON
                     VALUE(NO_OUT)    TYPE SLIS_FIELDCAT_ALV-NO_OUT
                     VALUE(SP_GROUP)  TYPE SLIS_FIELDCAT_ALV-SP_GROUP
                     VALUE(DO_SUM)    TYPE SLIS_FIELDCAT_ALV-DO_SUM
                     VALUE(JUST)      TYPE SLIS_FIELDCAT_ALV-JUST
                     VALUE(EMPHASIZE) TYPE SLIS_FIELDCAT_ALV-EMPHASIZE
                     VALUE(DATATYPE)  TYPE SLIS_FIELDCAT_ALV-DATATYPE
                     VALUE(INTTYPE)   TYPE SLIS_FIELDCAT_ALV-INTTYPE
                     VALUE(LOWERCASE) TYPE SLIS_FIELDCAT_ALV-LOWERCASE
             VALUE(REF_FIELDNAME) TYPE SLIS_FIELDCAT_ALV-REF_FIELDNAME
             VALUE(REF_TABNAME)   TYPE SLIS_FIELDCAT_ALV-REF_TABNAME.

  DATA: FIELD_HEAD_STRUC TYPE SLIS_FIELDCAT_ALV.
  DATA: P_COL_FIELD      TYPE COL_STRUC.

  ACT_COL = ACT_COL + 1.

  CLEAR FIELD_HEAD_STRUC. CLEAR P_COL_FIELD.

  FIELD_HEAD_STRUC-COL_POS    =  ACT_COL.
  P_COL_FIELD-COL_POS         =  ACT_COL.
  FIELD_HEAD_STRUC-ROW_POS    =  ROW_POS.
  FIELD_HEAD_STRUC-FIELDNAME  =  FIELDNAME.
  P_COL_FIELD-FIELDNAME       =  FIELDNAME.
  FIELD_HEAD_STRUC-OUTPUTLEN  =  OUTPUTLEN.
  FIELD_HEAD_STRUC-SELTEXT_L  =  SELTEXT_L.
  FIELD_HEAD_STRUC-TABNAME    =  TABNAME.
  P_COL_FIELD-TABNAME         =  TABNAME.
  FIELD_HEAD_STRUC-ICON       =  ICON.
  FIELD_HEAD_STRUC-NO_OUT     =  NO_OUT.
  FIELD_HEAD_STRUC-SP_GROUP   =  SP_GROUP.
  FIELD_HEAD_STRUC-DO_SUM     =  DO_SUM.
  FIELD_HEAD_STRUC-JUST       =  JUST.
  FIELD_HEAD_STRUC-EMPHASIZE  =  EMPHASIZE.
  FIELD_HEAD_STRUC-DATATYPE   =  DATATYPE.
  FIELD_HEAD_STRUC-INTTYPE    =  INTTYPE.
  FIELD_HEAD_STRUC-LOWERCASE  =  LOWERCASE.
  FIELD_HEAD_STRUC-REF_FIELDNAME = REF_FIELDNAME.
  FIELD_HEAD_STRUC-REF_TABNAME   = REF_TABNAME.

  APPEND FIELD_HEAD_STRUC TO LIST_FIELDCAT.
  APPEND P_COL_FIELD TO P_COL_QUEUE.

ENDFORM.                               " FILL_SEQ_FIELDCAT_LINE


*---------------------------------------------------------------------*
*       FORM D_GET_SORT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  D_SORT                                                        *
*  -->  VALUE(D_TABNAME_HEADER)                                       *
*  -->  VALUE(D_TABNAME_ITEM)                                         *
*---------------------------------------------------------------------*
FORM D_GET_SORT USING    D_SORT TYPE SLIS_T_SORTINFO_ALV
                         VALUE(D_TABNAME_HEADER) TYPE SLIS_TABNAME
                         VALUE(D_TABNAME_ITEM)   TYPE SLIS_TABNAME.

  DATA: SORTINFO TYPE SLIS_SORTINFO_ALV.

  CLEAR SORTINFO.

  SORTINFO-FIELDNAME = 'S_POS_ID'.
  SORTINFO-TABNAME   = D_TABNAME_HEADER.
  SORTINFO-UP        = 'X'.
*  sortinfo-spos     = 1.
*  sortinfo-subtot    = 'X'.
  APPEND SORTINFO TO D_SORT.

*  clear sortinfo.
*  sortinfo-spos =
*   sortinfo-fieldname = 'P_HOLDER'.
*   sortinfo-tabname   = d_tabname_item.
*   sortinfo-subtot    = 'X'.
*   append sortinfo to d_sort.


ENDFORM.                               " D_GET_SORT
*---------------------------------------------------------------------*
*       FORM d_seq_sort                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  d_sort                                                        *
*---------------------------------------------------------------------*
FORM D_SEQ_SORT       USING D_SORT TYPE SLIS_T_SORTINFO_ALV.
  DATA: SORTINFO TYPE SLIS_SORTINFO_ALV.

  CLEAR SORTINFO.

  SORTINFO-FIELDNAME = 'O_ORG_ID'.
  SORTINFO-UP        = 'X'.
  SORTINFO-SPOS     = 1.
  APPEND SORTINFO TO D_SORT.
*
  CLEAR SORTINFO.
  SORTINFO-FIELDNAME = 'O_SHORT'.
  SORTINFO-UP        = 'X'.
  SORTINFO-SPOS     = 2.
  APPEND SORTINFO TO D_SORT.
*
  CLEAR SORTINFO.
  SORTINFO-FIELDNAME = 'S_SHORT'.
  SORTINFO-UP        = 'X'.
  SORTINFO-SPOS      = 3.
  APPEND SORTINFO TO D_SORT.
*
  CLEAR SORTINFO.
  SORTINFO-FIELDNAME = 'P_STEXT'.
  SORTINFO-UP        = 'X'.
  SORTINFO-SPOS     = 4.
  APPEND SORTINFO TO D_SORT.

ENDFORM.                    "d_seq_sort

*&---------------------------------------------------------------------*
*&      Form  D_GET_EVENT_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_D_EVENT_EXIT[]  text
*----------------------------------------------------------------------*
FORM GET_EVENT_EXIT USING    P_EVENT_EXIT TYPE SLIS_T_EVENT_EXIT.

  DATA: P_EVENT_STRUC TYPE SLIS_EVENT_EXIT.

  CLEAR P_EVENT_STRUC.
  P_EVENT_STRUC-UCOMM = '&OL0'.
  P_EVENT_STRUC-AFTER = 'X'.
  APPEND P_EVENT_STRUC TO P_EVENT_EXIT.

  CLEAR P_EVENT_STRUC.
  P_EVENT_STRUC-UCOMM = '&OLX'.
  P_EVENT_STRUC-AFTER = 'X'.
  APPEND P_EVENT_STRUC TO P_EVENT_EXIT.


ENDFORM.                               " D_GET_EVENT_EXIT
*&---------------------------------------------------------------------*
*&      Form  GET_LIST_LAYOUT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM GET_LIST_LAYOUT_INFO USING P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

*  DATA: P_ES_VARIANT  LIKE DISVARIANT,
*        P_ES_LAYOUT   TYPE SLIS_LAYOUT_ALV,
*        P_ET_SORT     TYPE SLIS_T_SORTINFO_ALV,
*        P_ET_FILTER   TYPE SLIS_T_FILTER_ALV,
*        P_LIST_SCROLL TYPE SLIS_LIST_SCROLL.


  CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
       IMPORTING
*            ES_LAYOUT      = P_ES_LAYOUT
            ET_FIELDCAT    = P_FIELDCAT[]
*            ET_SORT        = P_ET_SORT[]
*            ET_FILTER      = P_ET_FILTER[]
*            ES_LIST_SCROLL = P_LIST_SCROLL
*            ES_VARIANT     = P_ES_VARIANT
       EXCEPTIONS
            NO_INFOS       = 1
            PROGRAM_ERROR  = 2
            OTHERS         = 3.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " GET_LIST_LAYOUT_INFO

*&---------------------------------------------------------------------*
*&      Form  SET_LIST_LAYOUT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM SET_LIST_LAYOUT_INFO USING P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
      EXPORTING
*              IS_LAYOUT      =
            IT_FIELDCAT    = P_FIELDCAT[].
*              IT_SORT        =
*              IT_FILTER      =
*              IS_LIST_SCROLL =.

ENDFORM.                               " SET_LIST_LAYOUT_INFO

*---------------------------------------------------------------------*
*       FORM GET_CHANGED_FIELD_INFO                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ACT_INFO                                                      *
*---------------------------------------------------------------------*
FORM GET_CHANGED_FIELD_INFO.

  DATA: C_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

* get fieldcat after processing variant
  PERFORM GET_LIST_LAYOUT_INFO USING C_FIELDCAT[].

* set new col_pos for fieldcat
  PERFORM SET_NEW_COL_POS TABLES O_COL_QUEUE
                                 C_FIELDCAT.

* set changeg list_layout_info
  PERFORM SET_LIST_LAYOUT_INFO USING C_FIELDCAT[].

ENDFORM.                               " GET_CHANGED_FIELD_INFO
*---------------------------------------------------------------------*
*       FORM SET_NEW_COL_POS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_COL_QUEUE                                                   *
*  -->  C_FIELDCAT                                                    *
*---------------------------------------------------------------------*
FORM SET_NEW_COL_POS TABLES   P_COL_QUEUE TYPE COL_QUEUE
                              C_FIELDCAT  TYPE SLIS_T_FIELDCAT_ALV.

  LOOP AT P_COL_QUEUE.
    READ TABLE C_FIELDCAT WITH KEY FIELDNAME = P_COL_QUEUE-FIELDNAME
                                   TABNAME   = P_COL_QUEUE-TABNAME.
    IF SY-SUBRC EQ 0.
      C_FIELDCAT-COL_POS = P_COL_QUEUE-COL_POS.
      MODIFY C_FIELDCAT INDEX SY-TABIX TRANSPORTING COL_POS.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " SET_NEW_COL_POS
*---------------------------------------------------------------------*
*       FORM CHANGE_MENU_TEXT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  C_FIELDCAT                                                    *
*  -->  VALUE(C_FIELDNAME)                                            *
*  -->  VALUE(C_TABNAME)                                              *
*  -->  VALUE(P_ACT_INFO)                                             *
*  -->  VALUE(VIEW_STATUS_FLAG)                                       *
*---------------------------------------------------------------------*
FORM CHANGE_MENU_TEXT TABLES C_FIELDCAT         TYPE SLIS_T_FIELDCAT_ALV
                      USING  VALUE(C_FIELDNAME) TYPE SLIS_FIELDNAME
                             VALUE(C_TABNAME)   TYPE SLIS_TABNAME
                             VALUE(P_ACT_INFO)  LIKE ACT_INFO
                             VALUE(VIEW_STATUS_FLAG) TYPE C.

  READ TABLE C_FIELDCAT WITH KEY FIELDNAME = C_FIELDNAME
                                 TABNAME   = C_TABNAME.
  IF SY-SUBRC EQ 0.
    IF  ( C_FIELDCAT-NO_OUT IS INITIAL AND VIEW_STATUS_FLAG IS INITIAL )
                               OR ( NOT C_FIELDCAT-NO_OUT IS INITIAL AND
                                      NOT VIEW_STATUS_FLAG  IS INITIAL ).
      CALL FUNCTION 'RH_USER_VIEW_PARAMETER'
        EXPORTING
          CHANGE_OBJECT_KEY    = P_ACT_INFO-CHANGE_OKEY
          CHANGE_SHORT         = P_ACT_INFO-CHANGE_SHOR
          CHANGE_OBJECT_DATE   = P_ACT_INFO-CHANGE_ODAT
          CHANGE_RELATION_DATE = P_ACT_INFO-CHANGE_VDAT.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHANGE_MENU_TEXT
*---------------------------------------------------------------------*
*       FORM ALL_CHANGE_MENU_TEXT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  C_FIELDCAT                                                    *
*  -->  VALUE(C_FIELDNAME)                                            *
*  -->  VALUE(C_TABNAME)                                              *
*  -->  VALUE(P_ACT_INFO)                                             *
*  -->  VALUE(VIEW_STATUS_FLAG)                                       *
*  -->  P_NO_OUT                                                      *
*  -->  P_FLAG                                                        *
*---------------------------------------------------------------------*
FORM ALL_CHANGE_MENU_TEXT
                   TABLES C_FIELDCAT           TYPE SLIS_T_FIELDCAT_ALV
                   USING  VALUE(C_FIELDNAME)   TYPE SLIS_FIELDNAME
                          VALUE(C_TABNAME)     TYPE SLIS_TABNAME
                          VALUE(P_ACT_INFO)    LIKE ACT_INFO
                          VALUE(VIEW_STATUS_FLAG) TYPE C
                          P_NO_OUT                TYPE C
                          P_FLAG                  TYPE C.

  READ TABLE C_FIELDCAT WITH KEY FIELDNAME = C_FIELDNAME
                                 TABNAME   = C_TABNAME.
  IF SY-SUBRC EQ 0.
    IF  ( C_FIELDCAT-NO_OUT IS INITIAL AND VIEW_STATUS_FLAG IS INITIAL )
                               OR ( NOT C_FIELDCAT-NO_OUT IS INITIAL AND
                                      NOT VIEW_STATUS_FLAG  IS INITIAL ).
      P_NO_OUT = C_FIELDCAT-NO_OUT.
      P_FLAG = 'X'.
      CALL FUNCTION 'RH_USER_VIEW_PARAMETER'
        EXPORTING
          CHANGE_OBJECT_KEY    = P_ACT_INFO-CHANGE_OKEY
          CHANGE_SHORT         = P_ACT_INFO-CHANGE_SHOR
          CHANGE_OBJECT_DATE   = P_ACT_INFO-CHANGE_ODAT
          CHANGE_RELATION_DATE = P_ACT_INFO-CHANGE_VDAT.
    ENDIF.
  ENDIF.

ENDFORM.                               " ALL_CHANGE_MENU_TEXT
*---------------------------------------------------------------------*
*       FORM MODIFY_FIELDCAT                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  C_FIELDCAT                                                    *
*  -->  VALUE(C_FIELDNAME)                                            *
*  -->  VALUE(C_TABNAME)                                              *
*  -->  VALUE(P_NO_OUT)                                               *
*---------------------------------------------------------------------*
FORM MODIFY_FIELDCAT TABLES C_FIELDCAT         TYPE SLIS_T_FIELDCAT_ALV
                     USING  VALUE(C_FIELDNAME) TYPE SLIS_FIELDNAME
                            VALUE(C_TABNAME)   TYPE SLIS_TABNAME
                            VALUE(P_NO_OUT)    TYPE C.

  READ TABLE C_FIELDCAT WITH KEY FIELDNAME = C_FIELDNAME
                                 TABNAME   = C_TABNAME.
  IF SY-SUBRC EQ 0.
    IF NOT C_FIELDCAT-NO_OUT EQ P_NO_OUT.
      C_FIELDCAT-NO_OUT = P_NO_OUT.
      MODIFY C_FIELDCAT INDEX SY-TABIX TRANSPORTING NO_OUT.
*      message s899 with 'Schluesselfelder'(008)
*                        'nur gemeinsam ein/ausblendbar'(007).
    ENDIF.
  ENDIF.



ENDFORM.                               " MODIFY_FIELDCAT
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD_D                                          *
*---------------------------------------------------------------------*
*       Text für Listkopf/Listende der einstufigen Liste (Detailmodus)*
*---------------------------------------------------------------------*
*  -->  P_LIST_TOP_OF_PAGE                                            *
*  -->  VALUE(S_DAY)                                                  *
*  -->  VALUE(PERCK_TEXT)                                             *
*---------------------------------------------------------------------*
FORM COMMENT_BUILD_D USING  P_LIST_TOP_OF_PAGE  TYPE SLIS_T_LISTHEADER
                            VALUE(S_DAY)      TYPE ANY
                            VALUE(PERCK_TEXT) LIKE T77MT-MTEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: P_BASIS(40) TYPE C.

  REFRESH P_LIST_TOP_OF_PAGE.

* Listenüberschrift: Typ H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-INFO  = 'Besetzungsplan'(BZP).
  APPEND LS_LINE TO P_LIST_TOP_OF_PAGE.
* Kopfinfo: Typ S
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Stichtag'(RDY).
  LS_LINE-INFO = S_DAY.
  APPEND LS_LINE TO P_LIST_TOP_OF_PAGE.
*
*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Arbeitszeitbasis'(WTB).
* CONCATENATE 'Stunden'(HRS) PERCK_TEXT INTO P_BASIS SEPARATED BY SPACE.
*  LS_LINE-INFO = P_BASIS.
**  ls_line-info = perck_text.
*  APPEND LS_LINE TO P_LIST_TOP_OF_PAGE.

ENDFORM.                               " COMMENT_BUILD_D
*---------------------------------------------------------------------*
*       FORM BUILD_LAYOUT_D                                           *
*---------------------------------------------------------------------*
*       Layoutangaben für einstufige Liste im Modus Detail            *
*---------------------------------------------------------------------*
*  -->  P_LIST_LAYOUT                                                 *
*---------------------------------------------------------------------*
FORM BUILD_LAYOUT_D USING    P_LIST_LAYOUT TYPE SLIS_LAYOUT_ALV.

*** Detailinfo auf Doppelklick ***
*  P_LIST_LAYOUT-F2CODE = 'INFO'.
*** optimiert Spaltenbr., s.d. alle Inhalte vollständig angezeigt werden
  P_LIST_LAYOUT-COLWIDTH_OPTIMIZE  = 'X'.
*** Sortierkriterium für Aufbereitungsoption ***
  P_LIST_LAYOUT-GROUP_CHANGE_EDIT = 'X'.
*** Ausgabe mit Zeilenfärbung
*  P_LIST_LAYOUT-INFO_FIELDNAME = 'P_COL_F'.
*** totals for numc-fields possible
  P_LIST_LAYOUT-NUMC_SUM = 'X'.
*** show only totals
  P_LIST_LAYOUT-TOTALS_TEXT = 'Summe'(SUM).
*  P_LIST_LAYOUT-TOTALS_ONLY = 'X'.


ENDFORM.                               " BUILD_LAYOUT_D
*---------------------------------------------------------------------*
*       FORM FILL_SEQU_FIELDCAT                                       *
*---------------------------------------------------------------------*
*       Feldkatalog für einstufige Liste                              *
*---------------------------------------------------------------------*
*  -->  LIST_FIELDCAT                                                 *
*  -->  D_COL_QUEUE                                                   *
*  -->  VALUE(P_ACT_INFO)                                             *
*---------------------------------------------------------------------*
FORM FILL_SEQU_FIELDCAT USING LIST_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV
                               D_COL_QUEUE     TYPE COL_QUEUE.

  DATA: ACT_COL TYPE I.

*
  CLEAR ACT_COL.
  REFRESH LIST_FIELDCAT.

* OrgUnit Id
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL          "col_pos
                                         ' '              "row_pos
                                         'O_ORG_ID'         "fieldname
                                         12                 "outputlen
                                         'OrgID'(OID)       "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'              "no_out
                                         'A'              "sp_group
                                         ' '              "do_sum
                                         ' '                "just
                                         'C10'              "emphasize
                                         'NUMC'             "datatype
                                         'N'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
* OrgUnit short text
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'O_SHORT'          "fieldname
                                         12                 "outputlen
                                         'OrgEinheit'(ORU)  "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         ' '               "no_out
                                         'A'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.

* OrgUnit long text                       "VWMORGLONG Note 399383

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'O_STEXT'          "fieldname
                                         40                 "outputlen
                                  'Organisationseinheit'(ORG) "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'               "no_out
                                         'A'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.
* position id
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                         'S_POS_ID'         "fieldname
                                         12                 "outputlen
                                         'PlanstellenID'(PID)"seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'                   "no_out
                                         'B'                  "sp_group
                                         ' '                  "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'NUMC'             "datatype
                                         'N'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
* position: short text
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'S_SHORT'          "fieldname
                                         12                 "outputlen
                                        'Planst.kuerzel'(PSH)"seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'               "no_out
                                         'B'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.
  .
*  position: long text
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL           "col_pos
                                         ' '               "row_pos
                                         'S_TEXT'           "fieldname
                                         40                 "outputlen
                                       'Planstellen'(PLA)   "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         ' '               "no_out
                                         'B'               "sp_group
                                         ' '               "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.
* holder name
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL             "col_pos
                                         ' '                 "row_pos
                                         'P_STEXT'          "fieldname
                                          40                "outputlen
                                         'Mitarbeiter'(PER) "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         ' '                 "no_out
                                         'C'                 "sp_group
                                         ' '                 "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.

* position object period
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                         'S_ODATE'          "fieldname
                                         24                 "outputlen
                                    'Planst.Objektzeit.'(PVA)"seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'                  "no_out
                                         'B'                  "sp_group
                                         ' '                  "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
* position is boss
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL             "col_pos
                                         ' '                 "row_pos
                                         'S_LEITER'         "fieldname
                                         3                  "outputlen
*                                        'Vorgesetzte'(VOR)  "seltext_l
                                         'Leiter'(LVB)
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         ' '                 "no_out
                                         'B'                 "sp_group
                                         ' '                 "do_sum
                                         'C'                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.
* new Column
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL             "col_pos
                                         ' '                 "row_pos
                                         'S_BES_STAT'       "fieldname
                                         35                 "outputlen
                                   'Besetzungsstatus'(SAS)  "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         ' '                 "no_out
                                         'B'                 "sp_group
                                         ' '                 "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         'X'                "lowercase
                                         ' '
                                         ' '.


* relationsship period between postion and holder
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL            "col_pos
                                         ' '                "row_pos
                                         'V_DATE'           "fieldname
                                         24                 "outputlen
               'Verknüpfungszeitraum Planst.-Inhaber'(ASS)  "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'                "no_out
                                         'C'                "sp_group
                                         ' '                "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
* holder object id
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL          "col_pos
                                         ' '              "row_pos
                                         'P_HOLDER'         "fieldname
                                         12                 "outputlen
                                         'Personalnr'(PNR)  "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'              "no_out
                                         'C'              "sp_group
                                         ' '              "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
** holder name
*  PERFORM FILL_seq_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
*                                  USING  ACT_COL             "col_pos
*                                         ' '                 "row_pos
*                                         'P_STEXT'           "fieldname
*                                         20                  "outputlen
*                                         'Mitarbeiter'(PER)  "seltext_l
*                                         'LIST_OUT'          "tabname
*                                         ' '                 "icon
*                                         ' '                 "no_out
*                                         'C'                 "sp_group
*                                         ' '                 "do_sum
*                                         ' '                 "just
*                                         'C50'               "emphasize
*                                         'CHAR'           "datatype
*                                         'C'              "inttype
*                                         'X'              "lowercase
*                                         ' '
*                                         ' '.
* holder object period
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                         'P_ODATE'          "fieldname
                                         24                 "outputlen
                                    'InhaberObjektzeit.'(IVA)"seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'                  "no_out
                                         'C'                  "sp_group
                                         ' '                  "do_sum
                                         ' '                "just
                                         ' '                "emphasize
                                         'CHAR'             "datatype
                                         'C'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
* work time of holder
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                USING  ACT_COL                "col_pos
                                       ' '                    "row_pos
                                       'P_WORKT'            "fieldname
                                       6                    "outputlen
                                       'Ist Arbeitszeit'(IWT)"seltext_l
                                       'LIST_OUT'           "tabname
                                       ' '                  "icon
                                       ' '                    "no_out
                                       'C'                    "sp_group
                                       ' '                    "do_sum
                                       'R'                  "just
                                       ' '                  "emphasize
                                       'DEC'                "datatype
                                       'P'                  "inttype
                                       ' '                  "lowercase
                                       ' '
                                       ' '.
* work time of position
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                        'S_WORKT'           "fieldname
                                         6                  "outputlen
                                      'Soll Arbeitszeit'(SWT)"seltext_l
                                        'LIST_OUT'          "tabname
                                        ' '                 "icon
                                        ' '                   "no_out
                                        'B'                   "sp_group
                                        ' '                   "do_sum
                                        'R'                 "just
                                        ' '                 "emphasize
                                        'DEC'               "datatype
                                        'P'                 "inttype
                                        ' '                 "lowercase
                                        ' '
                                        ' '.
* work time-> Soll*                                  "AP Note 569860
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL              "col_pos
                                         ' '                  "row_pos
                                        'S_WORKS'           "fieldname
                                         6                  "outputlen
              'Soll Arbeitszeit zur Summierung'(SWS)        "seltext_l
                                        'LIST_OUT'          "tabname
                                        ' '                 "icon
                                        ' '                   "no_out
                                        'B'                   "sp_group
                                        ' '                   "do_sum
                                        'R'                 "just
                                        ' '                 "emphasize
                                        'DEC'               "datatype
                                        'P'                 "inttype
                                        ' '                 "lowercase
                                        ' '
                                        ' '.

* Besetzungsprozentsatz
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL            "col_pos
                                         ' '                "row_pos
                                         'P_ABES'           "fieldname
                                         6                  "outputlen
                                         'BesProzent'(BPZ)  "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         ' '                "no_out
                                         'B'                "sp_group
                                         ' '                "do_sum
                                         'R'                "just
                                         ' '                "emphasize
                                         'DEC'              "datatype
                                         'P'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.
* work time of holder
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                USING  ACT_COL                "col_pos
                                       ' '                    "row_pos
                                       'P_EWORK'            "fieldname
                                       6                    "outputlen
                                       'eff.Arbeitszeit'(EWT)"seltext_l
                                       'LIST_OUT'           "tabname
                                       ' '                  "icon
                                       'X'                    "no_out
                                       'C'                    "sp_group
                                       ' '                    "do_sum
                                       'R'                  "just
                                       ' '                  "emphasize
                                       'DEC'                "datatype
                                       'P'                  "inttype
                                       ' '                  "lowercase
                                       ' '
                                       ' '.
* Besetzungsprozentsatz
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                  USING  ACT_COL            "col_pos
                                         ' '                "row_pos
                                         'P_EBES'           "fieldname
                                         6                  "outputlen
                                         'eff.BesProz'(EBP) "seltext_l
                                         'LIST_OUT'         "tabname
                                         ' '                "icon
                                         'X'                "no_out
                                         'B'                "sp_group
                                         ' '                "do_sum
                                         'R'                "just
                                         ' '                "emphasize
                                         'DEC'              "datatype
                                         'P'                "inttype
                                         ' '                "lowercase
                                         ' '
                                         ' '.

* holder employee group
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'P_PERSG'             "fieldname
                                      20                    "outputlen
                                      'Mitarbeitergruppe'(MGR)"seltext_l
                                      'LIST_OUT'            "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      'C'                     "sp_group
                                      ' '                     "do_sum
                                      'C'                   "just
                                      ' '                   "emphasize
                                      'CHAR'                "datatype
                                      'C'                   "inttype
                                      'X'                   "lowercase
                                      ' '
                                      ' '.
* holder EE subgroup
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'P_PERSK'             "fieldname
                                      20                    "outputlen
                                      'Employee subgroup'(MKR)"seltext_l
                                      'LIST_OUT'            "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      'C'                     "sp_group
                                      ' '                     "do_sum
                                      'C'                   "just
                                      ' '                   "emphasize
                                      'CHAR'                "datatype
                                      'C'                   "inttype
                                      'X'                   "lowercase
                                      ' '
                                      ' '.
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                               USING  ACT_COL                 "col_pos
                                      ' '                     "row_pos
                                      'COUNT'             "fieldname
                                      20                    "outputlen
                                      'count'(CNT) "seltext_l
                                      'LIST_OUT'            "tabname
                                      ' '                   "icon
                                      ' '                     "no_out
                                      'C'                     "sp_group
                                      ' '                     "do_sum
                                      'C'                   "just
                                      ' '                   "emphasize
                                      'NUMC'                "datatype
                                      'I'                   "inttype
                                      ' '                   "lowercase
                                      ' '
                                      ' '.

*      O_ORGID_MAIN      TYPE HRP1001-SOBID,
*             O_SHORT_MAIN       LIKE OBJEC-SHORT,
*             O_STEXT_MAIN       LIKE OBJEC-STEXT,

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'O_ORGID_MAIN'             "fieldname
                                        20                    "outputlen
                                        'MAIN ORG.UNIT'(MOU) "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'CHAR'                "datatype
                                        'C'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.
*
  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'O_SHORT_MAIN'             "fieldname
                                        20                    "outputlen
                                        'Main ORG.Unit txt'(MOT) "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'CHAR'                "datatype
                                        'C'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'O_STEXT_MAIN'             "fieldname
                                        20                    "outputlen
                                        'Main ORG.Unit txt'(MOT) "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'CHAR'                "datatype
                                        'C'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

  PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'TOT_FIX'             "fieldname
                                        20                    "outputlen
                                        'TOTAL FIXED PAY'      "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'NUMC'                "datatype
                                        'I'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'PERFORMANCE'             "fieldname
                                        20                    "outputlen
                                        'PERFORMANCE INCENTIVE'      "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'NUMC'                "datatype
                                        'I'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.

PERFORM FILL_SEQ_FIELDCAT_LINE TABLES LIST_FIELDCAT D_COL_QUEUE
                                 USING  ACT_COL                 "col_pos
                                        ' '                     "row_pos
                                        'CTC'             "fieldname
                                        20                    "outputlen
                                        'TOTAL CTC'      "seltext_l
                                        'LIST_OUT'            "tabname
                                        ' '                   "icon
                                        ' '                     "no_out
                                        'C'                     "sp_group
                                        ' '                     "do_sum
                                        'C'                   "just
                                        ' '                   "emphasize
                                        'NUMC'                "datatype
                                        'I'                   "inttype
                                        ' '                   "lowercase
                                        ' '
                                        ' '.
*               TYPE P DECIMALS 2, "
*             PERFORMANCE TYPE P DECIMALS 2," PERFORMANCE INCENTIVE
*             CTC TYPE P DECIMALS 2," TOTAL CTC



ENDFORM.                               " FILL_SEQU_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GET_LIST_EVENT_D
*&---------------------------------------------------------------------*
*       Ereignis für einstufige Liste im Modus Detail
*----------------------------------------------------------------------*
*      -->LIST_EVENT  text
*----------------------------------------------------------------------*
FORM GET_LIST_EVENT_D USING  LIST_EVENT  TYPE SLIS_T_EVENT.

  DATA: LS_EVENT TYPE SLIS_T_EVENT WITH HEADER LINE.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE     = 1
    IMPORTING
      ET_EVENTS       = LIST_EVENT
    EXCEPTIONS
      LIST_TYPE_WRONG = 1
      OTHERS          = 2.
*
  CLEAR LS_EVENT.
  READ TABLE LIST_EVENT WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                        INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE 'TOP_OF_PAGE' TO LS_EVENT-FORM.
    APPEND LS_EVENT TO LIST_EVENT.
  ENDIF.


ENDFORM.                               " GET_LIST_EVENT_D
*---------------------------------------------------------------------*
*       FORM CHANGE_ID_VAR                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  C_FIELDCAT                                                    *
*  -->  VALUE(FIRST)                                                  *
*  -->  VALUE(SECOND)                                                 *
*  -->  VALUE(D_ACT_INFO)                                             *
*---------------------------------------------------------------------*
FORM CHANGE_ID_VAR TABLES   C_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV
                   USING    VALUE(FIRST)      TYPE SLIS_TABNAME
                            VALUE(SECOND)     TYPE SLIS_TABNAME
                            VALUE(D_ACT_INFO) LIKE ACT_INFO.

  DATA: P_NO_OUT   TYPE C.
  DATA: MOD_FLAG   TYPE C,
        P_FLAG     TYPE C.

  CLEAR: P_NO_OUT, MOD_FLAG, P_FLAG.

* overtake Id from header table
  PERFORM ALL_CHANGE_MENU_TEXT  TABLES  C_FIELDCAT
                                USING  'S_POS_ID'  FIRST
                                        D_ACT_INFO  ACT_INFO-OKEY
                                        P_NO_OUT    P_FLAG.
  IF P_FLAG = 'X'. MOD_FLAG = '1'. ENDIF.
  CLEAR P_FLAG.
* overtake ID from item table
  PERFORM ALL_CHANGE_MENU_TEXT  TABLES  C_FIELDCAT
                                USING   'P_HOLDER'  SECOND
                                        D_ACT_INFO  ACT_INFO-OKEY
                                        P_NO_OUT    P_FLAG.
  IF P_FLAG = 'X'. MOD_FLAG = '2'. ENDIF.

  CASE MOD_FLAG.
    WHEN '1'.
*     modify ID-field for item table
      PERFORM MODIFY_FIELDCAT TABLES  C_FIELDCAT
                              USING   'P_HOLDER'  SECOND
                                      P_NO_OUT.

    WHEN '2'.
*     modify ID-field for header table
      PERFORM MODIFY_FIELDCAT TABLES  C_FIELDCAT
                              USING   'S_POS_ID'  FIRST
                                      P_NO_OUT.
  ENDCASE.

ENDFORM.                               " CHANGE_ID_VAR

*---------------------------------------------------------------------*
*       FORM CHANGE_ODAT_VAR                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  C_FIELDCAT                                                    *
*  -->  VALUE(FIRST)                                                  *
*  -->  VALUE(SECOND)                                                 *
*  -->  VALUE(D_ACT_INFO)                                             *
*---------------------------------------------------------------------*
FORM CHANGE_ODAT_VAR TABLES   C_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV
                     USING    VALUE(FIRST)      TYPE SLIS_TABNAME
                              VALUE(SECOND)     TYPE SLIS_TABNAME
                              VALUE(D_ACT_INFO) LIKE ACT_INFO.

  DATA: P_NO_OUT   TYPE C.
  DATA: MOD_FLAG   TYPE C,
        P_FLAG     TYPE C.

  CLEAR: P_NO_OUT, MOD_FLAG, P_FLAG.

* overtake Object_date from header table
  PERFORM ALL_CHANGE_MENU_TEXT  TABLES  C_FIELDCAT
                                USING   'S_ODATE'   FIRST
                                         D_ACT_INFO ACT_INFO-ODAT
                                         P_NO_OUT   P_FLAG.
  IF P_FLAG = 'X'. MOD_FLAG = '1'. ENDIF.
  CLEAR P_FLAG.
* overtake Object_date from item table
  PERFORM ALL_CHANGE_MENU_TEXT  TABLES  C_FIELDCAT
                                USING   'P_ODATE' SECOND
                                        D_ACT_INFO ACT_INFO-ODAT
                                        P_NO_OUT   P_FLAG.
  IF P_FLAG = 'X'. MOD_FLAG = '2'. ENDIF.

  CASE MOD_FLAG.
    WHEN '1'.
*     modify object date -field for item table
      PERFORM MODIFY_FIELDCAT  TABLES  C_FIELDCAT
                               USING   'P_ODATE'  SECOND
                                       P_NO_OUT.

    WHEN '2'.
*     modify object_date-field for header table
      PERFORM MODIFY_FIELDCAT  TABLES  C_FIELDCAT
                               USING   'S_ODATE'  FIRST
                                       P_NO_OUT.
  ENDCASE.

ENDFORM.                               " CHANGE_ODAT_VAR
*---------------------------------------------------------------------*
*       FORM FILL_P_OBJEC                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_OBJEC                                                       *
*  -->  VALUE(P_PLVAR)                                                *
*  -->  VALUE(P_OTYPE)                                                *
*  -->  VALUE(P_OBJID)                                                *
*  -->  VALUE(P_OBEG)                                                 *
*  -->  VALUE(P_OEND)                                                 *
*  -->  VALUE(P_OSTAT)                                                *
*  -->  VALUE(P_SHORT)                                                *
*  -->  VALUE(P_STEXT)                                                *
*  -->  VALUE(P_H_OBJID)                                              *
*---------------------------------------------------------------------*
FORM FILL_P_OBJEC USING    P_OBJEC             LIKE OBJEC
                           VALUE(P_PLVAR)      LIKE OBJEC-PLVAR
                           VALUE(P_OTYPE)      LIKE OBJEC-OTYPE
                           VALUE(P_OBJID)      LIKE OBJEC-OBJID
                           VALUE(P_OBEG)       LIKE OBJEC-BEGDA
                           VALUE(P_OEND)       LIKE OBJEC-ENDDA
                           VALUE(P_OSTAT)      LIKE OBJEC-ISTAT
                           VALUE(P_SHORT)      LIKE OBJEC-SHORT
                           VALUE(P_STEXT)      LIKE OBJEC-STEXT
                           VALUE(P_H_OBJID)    LIKE OBJEC-REALO.

  CLEAR P_OBJEC.
  P_OBJEC-PLVAR = P_PLVAR.
  P_OBJEC-OTYPE = P_OTYPE.
  P_OBJEC-OBJID = P_OBJID.
  P_OBJEC-BEGDA = P_OBEG.
  P_OBJEC-ENDDA = P_OEND.
  P_OBJEC-ISTAT = P_OSTAT.
  P_OBJEC-SHORT = P_STEXT.
  P_OBJEC-STEXT = P_STEXT.
  P_OBJEC-REALO = P_H_OBJID.

ENDFORM.                               " FILL_P_OBJEC
*---------------------------------------------------------------------*
*       FORM GET_LIST_INFO                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  POS_STRUC                                                     *
*  -->  PER_STRUC                                                     *
*  -->  LIST_OUT                                                      *
*  -->  RS_SELFIELD                                                   *
*  -->  P_OBJEC                                                       *
*---------------------------------------------------------------------*
FORM GET_LIST_INFO TABLES   POS_STRUC  STRUCTURE POS_STRUC
                            PER_STRUC  STRUCTURE PER_STRUC
                            LIST_OUT   STRUCTURE LIST_TAB
                   USING    RS_SELFIELD TYPE SLIS_SELFIELD
                            P_OBJEC       LIKE OBJEC.

  DATA: P_OTYPE     LIKE OBJEC-OTYPE.
  DATA: P_OBJID     LIKE OBJEC-OBJID.
  DATA: H_REALO     LIKE OBJEC-REALO.
  DATA: H_PER_REALO LIKE PER_STRUC-REALO.

  READ TABLE LIST_OUT INDEX RS_SELFIELD-TABINDEX.

  CASE RS_SELFIELD-SEL_TAB_FIELD.

    WHEN 'LIST_OUT-O_ORG_ID' OR 'LIST_OUT-O_SHORT' OR 'LIST_OUT-O_STEXT'.
      READ TABLE ORG_TAB WITH KEY ORG_OBJID = LIST_OUT-O_ORG_ID.
      IF SY-SUBRC = 0.
        PERFORM FILL_P_OBJEC USING  P_OBJEC
                                    PC-PLVAR
                                    $ORGEH
                                    ORG_TAB-ORG_OBJID
                                    ORG_TAB-BEGDA
                                    ORG_TAB-ENDDA
                                    ORG_TAB-ISTAT
                                    ORG_TAB-SHORT
                                    ORG_TAB-STEXT
                                    ' '.
      ENDIF.

    WHEN 'LIST_OUT-S_POS_ID' OR 'LIST_OUT-S_SHORT' OR 'LIST_OUT-S_TEXT'.
      READ TABLE POS_STRUC WITH KEY OTYPE = $PLSTE
                                    OBJID = LIST_OUT-S_POS_ID.
      IF SY-SUBRC = 0.
        PERFORM FILL_P_OBJEC USING  P_OBJEC
                                    PC-PLVAR
                                    POS_STRUC-OTYPE
                                    POS_STRUC-OBJID
                                    POS_STRUC-OBEG
                                    POS_STRUC-OEND
                                    POS_STRUC-OSTAT
                                    POS_STRUC-SHORT
                                    POS_STRUC-STEXT
                                    ' '.
      ENDIF.

    WHEN 'LIST_OUT-P_HOLDER' OR 'LIST_OUT-P_STEXT'.
*    holder is person
      IF LIST_OUT-P_OTYPE = 'P'.
        CLEAR P_OBJID.
        MOVE LIST_OUT-P_HOLDER TO P_OBJID.
        READ TABLE PER_STRUC WITH KEY OTYPE = LIST_OUT-P_OTYPE
                                      OBJID = P_OBJID.
*    holder is user
      ELSE.
        CLEAR H_PER_REALO.
        SPLIT LIST_OUT-P_HOLDER AT SPACE INTO P_OTYPE H_PER_REALO.
        READ TABLE PER_STRUC WITH KEY OTYPE = LIST_OUT-P_OTYPE
                                      REALO = H_PER_REALO.
      ENDIF.
      IF SY-SUBRC = 0.
        CLEAR H_REALO.
        MOVE PER_STRUC-REALO TO H_REALO.
        PERFORM FILL_P_OBJEC USING  P_OBJEC
                                    PC-PLVAR
                                    PER_STRUC-OTYPE
                                    PER_STRUC-OBJID
                                    PER_STRUC-OBEG
                                    PER_STRUC-OEND
                                    PER_STRUC-OSTAT
                                    ' '
                                    PER_STRUC-STEXT
                                    H_REALO.
      ENDIF.

  ENDCASE.


ENDFORM.                               " GET_LIST_INFO
*&---------------------------------------------------------------------*
*&      Form  end_commend
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIST_END_OF_PAGE  text
*----------------------------------------------------------------------*
FORM END_COMMEND USING    P_LIST_END_OF_PAGE TYPE SLIS_T_LISTHEADER
                          VALUE(TOP) STRUCTURE OVER.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: A_BESPROZ          TYPE P DECIMALS 2.

  REFRESH P_LIST_END_OF_PAGE.

* Kopfinfo: Typ S
*
* calculate mittlerer Besetzungsprozentsatz
  IF NOT TOP-TOT_WRKT_POS IS INITIAL.
    A_BESPROZ = TOP-TOT_WRKT_PER / TOP-TOT_WRKT_POS * 100.
  ELSE.
    A_BESPROZ = 0.
  ENDIF.
  CLEAR LS_LINE.   LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Mittlerer BesProz'(MBP).
  LS_LINE-INFO =  A_BESPROZ.
  APPEND LS_LINE TO P_LIST_END_OF_PAGE.
**
*  clear ls_line.   LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Soll Arbeitszeit'(SWT).
*  LS_LINE-INFO =  TOP-TOT_WRKT_POS.
*  APPEND LS_LINE TO P_LIST_END_OF_PAGE.
**
*  clear ls_line.   LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Ist Arbeitszeit'(IWT).
*  LS_LINE-INFO =  TOP-TOT_WRKT_PER.
*  APPEND LS_LINE TO P_LIST_END_OF_PAGE.
**
  CLEAR LS_LINE.   LS_LINE-TYP  = 'S'.
  IF TOP-TOT_WRKT_DEV GT 0.
    LS_LINE-KEY  = 'Überdeckung'(OVR).
    LS_LINE-INFO =  TOP-TOT_WRKT_DEV.
  ELSEIF TOP-TOT_WRKT_DEV LT 0.
    LS_LINE-KEY  = 'Unterdeckung'(BLL).
    LS_LINE-INFO =  TOP-TOT_WRKT_DEV.
  ENDIF.
  APPEND LS_LINE TO P_LIST_END_OF_PAGE.

ENDFORM.                               " end_commend
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DYNAMIC_TABLE .

  IF NOT LT_ALV_CAT[] IS INITIAL .
* Create Dynamic Table -> i_table
    CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
      EXPORTING
        IT_FIELDCATALOG = LT_ALV_CAT
      IMPORTING
        EP_TABLE        = I_TABLE.
    ASSIGN I_TABLE->* TO <TABLE> .
* Create dynamic work area and assign to FS

*    CREATE DATA I_STRUCT LIKE LINE OF <TABLE>.
*    ASSIGN I_STRUCT->* TO <STRUC>.


  ENDIF.

ENDFORM.                    " CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  FILL_LT_ALV_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_LT_ALV_CAT .


wa_ALV_CAT-COL_POS  = 1.
wa_ALV_CAT-FIELDNAME  = 'PERNR'.
wa_ALV_CAT-SELTEXT  = 'Employee No'.
wa_ALV_CAT-REF_FIELD  = 'PERNR'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 2.
wa_ALV_CAT-FIELDNAME  = 'ENAME'.
wa_ALV_CAT-SELTEXT  = 'Employee Name'.
wa_ALV_CAT-REF_FIELD  = 'ENAME'.
wa_ALV_CAT-REF_TABLE  = 'PA0001'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.


wa_ALV_CAT-COL_POS  = 3.
wa_ALV_CAT-FIELDNAME  = 'STEXT'.
wa_ALV_CAT-SELTEXT  = 'Designation'.
wa_ALV_CAT-REF_FIELD  = 'STEXT'.
wa_ALV_CAT-REF_TABLE  = 'HRP1000'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 4.
wa_ALV_CAT-FIELDNAME  = 'PERSK'.
wa_ALV_CAT-SELTEXT  = 'Grade'.
wa_ALV_CAT-REF_FIELD  = 'PERSK'.
wa_ALV_CAT-REF_TABLE  = 'PA0001'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 5.
wa_ALV_CAT-FIELDNAME  = 'ORGEH'.
wa_ALV_CAT-SELTEXT  = 'Org.Unit'.
wa_ALV_CAT-REF_FIELD  = 'ORGEH'.
wa_ALV_CAT-REF_TABLE  = 'PA0001'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 6.
wa_ALV_CAT-FIELDNAME  = 'ORGTX'.
wa_ALV_CAT-SELTEXT  = 'Org.Unit Txt'.
wa_ALV_CAT-REF_FIELD  = 'ORGTX'.
wa_ALV_CAT-REF_TABLE  = 'T527X'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 7.
wa_ALV_CAT-FIELDNAME  = 'WERKS'.
wa_ALV_CAT-SELTEXT  = 'Pers.area'.
wa_ALV_CAT-REF_FIELD  = 'WERKS'.
wa_ALV_CAT-REF_TABLE  = 'PA0001'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 8.
wa_ALV_CAT-FIELDNAME  = 'NAME1'.
wa_ALV_CAT-SELTEXT  = 'Pers.area Txt'.
wa_ALV_CAT-REF_FIELD  = 'NAME1'.
wa_ALV_CAT-REF_TABLE  = 'T500P'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 9.
wa_ALV_CAT-FIELDNAME  = 'KOSTL'.
wa_ALV_CAT-SELTEXT  = 'Cost Cntr.CODE'.
wa_ALV_CAT-REF_FIELD  = 'KOSTL'.
wa_ALV_CAT-REF_TABLE  = 'PA0001'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 10.
wa_ALV_CAT-FIELDNAME  = 'KTEXT'.
wa_ALV_CAT-SELTEXT  = 'Cost Cntr.'.
wa_ALV_CAT-REF_FIELD  = 'KTEXT'.
wa_ALV_CAT-REF_TABLE  = 'CSKT'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 11.
wa_ALV_CAT-FIELDNAME  = 'ZZLOC_TEXT'.
wa_ALV_CAT-SELTEXT  = 'Location'.
wa_ALV_CAT-REF_FIELD  = 'ZZLOC_TEXT'.
wa_ALV_CAT-REF_TABLE  = 'PA0001'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 12.
wa_ALV_CAT-FIELDNAME  = 'GBDAT'.
wa_ALV_CAT-SELTEXT  = 'DOB'.
wa_ALV_CAT-REF_FIELD  = 'GBDAT'.
wa_ALV_CAT-REF_TABLE  = 'PA0002'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 13.
wa_ALV_CAT-FIELDNAME  = 'BEGDA'.
wa_ALV_CAT-SELTEXT  = 'DOJ'.
wa_ALV_CAT-REF_FIELD  = 'BEGDA'.
wa_ALV_CAT-REF_TABLE  = 'PA0000'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 14.
wa_ALV_CAT-FIELDNAME  = '1001'.
wa_ALV_CAT-SELTEXT  = 'Basic Pay'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 15.
wa_ALV_CAT-FIELDNAME  = 'M001'.
wa_ALV_CAT-SELTEXT  = 'Monthly Basic'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 16.
wa_ALV_CAT-FIELDNAME  = '1004'.
wa_ALV_CAT-SELTEXT  = 'House Rent Allowance.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 17.
wa_ALV_CAT-FIELDNAME  = 'M004'.
wa_ALV_CAT-SELTEXT  = 'Monthly HRA'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 18.
wa_ALV_CAT-FIELDNAME  = '1007'.
wa_ALV_CAT-SELTEXT  = 'Special Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 19.
wa_ALV_CAT-FIELDNAME  = 'M007'.
wa_ALV_CAT-SELTEXT  = 'Monthly SPL.allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 20.
wa_ALV_CAT-FIELDNAME  = '1013'.
wa_ALV_CAT-SELTEXT  = 'Other Allowance.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 21.
wa_ALV_CAT-FIELDNAME  = 'M013'.
wa_ALV_CAT-SELTEXT  = 'Monthly Other Allowance.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 22.
wa_ALV_CAT-FIELDNAME  = '1005'.
wa_ALV_CAT-SELTEXT  = 'Educational Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 23.
wa_ALV_CAT-FIELDNAME  = 'M005'.
wa_ALV_CAT-SELTEXT  = 'Monthly Edu. Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 24.
wa_ALV_CAT-FIELDNAME  = '1006'.
wa_ALV_CAT-SELTEXT  = 'Conveyance Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 25.
wa_ALV_CAT-FIELDNAME  = 'M006'.
wa_ALV_CAT-SELTEXT  = 'Monthly Conveyance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 26.
wa_ALV_CAT-FIELDNAME  = 'M019'.
wa_ALV_CAT-SELTEXT  = 'Monthly.Telephone Reimb.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 27.
wa_ALV_CAT-FIELDNAME  = '1019'.
wa_ALV_CAT-SELTEXT  =  'Telephone Reimbursement.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 28.
wa_ALV_CAT-FIELDNAME  = 'M020'.
wa_ALV_CAT-SELTEXT  = 'Monthly.Ent. Allowance.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 29.
wa_ALV_CAT-FIELDNAME  = '1020'.
wa_ALV_CAT-SELTEXT  = 'Entertainment. Allowance.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 30.
wa_ALV_CAT-FIELDNAME  = 'M008'.
wa_ALV_CAT-SELTEXT  = 'Monthly Car Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 31.
wa_ALV_CAT-FIELDNAME  = '1008'.
wa_ALV_CAT-SELTEXT  = 'Car Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 32.
wa_ALV_CAT-FIELDNAME  = 'MON1'.
wa_ALV_CAT-SELTEXT  = 'MONTHLY'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 33.
wa_ALV_CAT-FIELDNAME  = 'ANNM'.
wa_ALV_CAT-SELTEXT  = 'P.A.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 34.
wa_ALV_CAT-FIELDNAME  = 'C3G1'.
wa_ALV_CAT-SELTEXT  = 'Bonus'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 35.
wa_ALV_CAT-FIELDNAME  = '1102'.
wa_ALV_CAT-SELTEXT  = 'Medical Reimbursement.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 36.
wa_ALV_CAT-FIELDNAME  = '1015'.
wa_ALV_CAT-SELTEXT  = 'Lease Value'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 37.
wa_ALV_CAT-FIELDNAME  = '1016'.
wa_ALV_CAT-SELTEXT  = 'Comp Acc.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 38.
wa_ALV_CAT-FIELDNAME  = 'M015'.
wa_ALV_CAT-SELTEXT  = 'Lease Value Monthlty'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 39.
wa_ALV_CAT-FIELDNAME  = 'M016'.
wa_ALV_CAT-SELTEXT  = 'Comp Acc.Monthlty'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 40.
wa_ALV_CAT-FIELDNAME  = '1101'.
wa_ALV_CAT-SELTEXT  = 'Leave Travel Allowance'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 41.
wa_ALV_CAT-FIELDNAME  = '1018'.
wa_ALV_CAT-SELTEXT  = 'Hard Furnishing'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 42.
wa_ALV_CAT-FIELDNAME  = 'TOT1'.
wa_ALV_CAT-SELTEXT  = 'TOTAL 1'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 43.
wa_ALV_CAT-FIELDNAME  = 'C3G2'.
wa_ALV_CAT-SELTEXT  = 'PF'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 44.
wa_ALV_CAT-FIELDNAME  = 'C3G4'.
wa_ALV_CAT-SELTEXT  = 'Super Annuation'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 45.
wa_ALV_CAT-FIELDNAME  = 'C3G3'.
wa_ALV_CAT-SELTEXT  = 'GRATUITY'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 46.
wa_ALV_CAT-FIELDNAME  = 'TOT2'.
wa_ALV_CAT-SELTEXT  = 'TOTAL 2'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 47.
wa_ALV_CAT-FIELDNAME  = 'CTC1'.
wa_ALV_CAT-SELTEXT  = 'Total Fixed Pay'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 48.
wa_ALV_CAT-FIELDNAME  = '1106'.
wa_ALV_CAT-SELTEXT  = 'Performance Incentive'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 49.
wa_ALV_CAT-FIELDNAME  = 'TCTC'.
wa_ALV_CAT-SELTEXT  = 'CTC'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 50.
wa_ALV_CAT-FIELDNAME  = 'M116'.
wa_ALV_CAT-SELTEXT  = 'IT 14 Month.Tel.Reimb.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.

wa_ALV_CAT-COL_POS  = 51.
wa_ALV_CAT-FIELDNAME  = '1116'.
wa_ALV_CAT-SELTEXT  = 'IT 14 Tel.Reimb.'.
wa_ALV_CAT-REF_FIELD  = 'BET01'.
wa_ALV_CAT-REF_TABLE  = 'PA0008'.
APPEND WA_ALV_CAT TO LT_ALV_CAT.
CLEAR: wa_ALV_CAT.


ENDFORM.                    " FILL_LT_ALV_CAT
