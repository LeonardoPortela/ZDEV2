*&---------------------------------------------------------------------*
*& Report  ZAA04
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZAA04.
TYPE-POOLS: SLIS.
*report AQZZ/SAPQUERY/AM28============.
INCLUDE <SYMBOL>.
INCLUDE <ICON>.

CONSTANTS:
  BEGIN OF %IQID,"type aqliqid
    WORKSPACE TYPE AQL_WSID   VALUE 'G',
    USERGROUP TYPE AQL_UGROUP VALUE '/SAPQUERY/AM',
    QUERY     TYPE AQL_QUERY  VALUE '28',
    LID       TYPE AQL_LID    VALUE 'G00',
    STRUCT    TYPE AQL_TNAME  VALUE 'ZIQG000000000004',
    INFOSET   TYPE AQL_ISET   VALUE '/SAPQUERY/AM27',
  END OF %IQID.

DATA %RUNMODE TYPE AQLIMODE.

DATA %SELOPTIONS TYPE TABLE OF RSPARAMS WITH HEADER LINE.

FIELD-SYMBOLS <%SELOPT> TYPE RSPARAMS_TT.


TABLES T093B.
DATA: GD_T093C LIKE T093C.
DATA: GD_T093  LIKE T093.
DATA: TL_CAT TYPE SLIS_T_FIELDCAT_ALV,
      WL_CAT LIKE LINE OF TL_CAT.
DATA: WL_LAYOUT TYPE  SLIS_LAYOUT_ALV.

DATA TEXT_ANLP_ANLN1 LIKE ANLH-ANLHTXT.
DATA TEXT_ANLP_ANLN2 LIKE ANLA-LETXT.
DATA TEXT_ANLP_BUKRS LIKE T001-BUTXT.
DATA TEXT_ANLP_CAUFN LIKE AUFK-KTEXT.
DATA TEXT_ANLP_GSBER LIKE TGSBT-GTEXT.
DATA TEXT_ANLP_KTOGR LIKE T095T-KTGRTX.
DATA TEXT_ANLP_PERAF LIKE DD07D-DDTEXT.
* TXT4
DATA %Z_0001(004) TYPE C .
* TXT12
DATA %Z_0005(012) TYPE C .
TABLES ANLP.
*include /1BCDWB/IQG000000000004DAT.
*include /1BCDWB/IQG000000000005DAT.
DATA %DTAB TYPE STANDARD TABLE OF ZIQG000000000004 WITH HEADER LINE.

DATA %SUBRC TYPE SY-SUBRC.
DATA: TG_T095 TYPE TABLE OF T095 WITH HEADER LINE.

*include /1BCDWB/IQG000000000004SSCR.
*
*include /1BCDWB/IQG000000000004SSCRAT.
*include /1BCDWB/IQG000000000005SSCR.
SELECTION-SCREEN BEGIN OF BLOCK ISSEL
                          WITH FRAME TITLE TEXT-S01.
SELECT-OPTIONS BUKRS FOR ANLP-BUKRS MEMORY ID BUK .
SELECT-OPTIONS ANLAGE FOR ANLP-ANLN1 .
SELECT-OPTIONS UNTNR FOR ANLP-ANLN2 . SELECTION-SCREEN SKIP .
SELECT-OPTIONS SO_KOSTL FOR ANLP-KOSTL MEMORY ID KOS. SELECTION-SCREEN SKIP .
SELECT-OPTIONS GSBER FOR ANLP-GSBER. SELECTION-SCREEN SKIP .
SELECT-OPTIONS AFABE FOR ANLP-AFABER MEMORY ID AFB NO-EXTENSION NO INTERVALS. .
SELECT-OPTIONS GJAHR FOR ANLP-GJAHR NO-EXTENSION NO INTERVALS. .
SELECT-OPTIONS AFBPLE FOR ANLP-PERAF .SELECTION-SCREEN SKIP .
PARAMETERS RECAP AS CHECKBOX.
PARAMETERS DEPRE AS CHECKBOX.
PARAMETERS DEPREAC AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK ISSEL.
SELECTION-SCREEN BEGIN OF BLOCK STDSEL WITH FRAME TITLE TEXT-S03.
PARAMETERS %LAYOUT TYPE SLIS_VARI MODIF ID LAY.
SELECTION-SCREEN END OF BLOCK STDSEL.

*include /1BCDWB/IQG000000000005SSCRAT.
AT SELECTION-SCREEN.
  TABLES SSCRFIELDS.
  CALL FUNCTION 'RSAQRT_LAYOUT_CHECK'
    EXPORTING
      VARIANT = %LAYOUT
    CHANGING
      RTMODE  = %RUNMODE.
  CALL FUNCTION 'RSAQRT_SSCR_TEST'
    EXPORTING
      SSCRUC = SSCRFIELDS-UCOMM
    TABLES
      SELOPT = <%SELOPT>
    CHANGING
      RTMODE = %RUNMODE
    EXCEPTIONS
      OTHERS = 1.
  IF SY-SUBRC <> 0.
    %RUNMODE-EXTR_ON = SPACE.
    %RUNMODE-SHOW_ON = SPACE.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR %LAYOUT.
  CALL FUNCTION 'RSAQRT_LAYOUT_VALUE_REQUEST'
    CHANGING
      RTMODE  = %RUNMODE
      VARIANT = %LAYOUT.

AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'RSAQRT_SSCR_OUTPUT'
    CHANGING
      RTMODE = %RUNMODE.

INITIALIZATION.
  ASSIGN %SELOPTIONS[] TO <%SELOPT>.
  CALL FUNCTION 'RSAQRT_INITIALIZATION'
    EXPORTING
      IQID   = %IQID
    IMPORTING
      RTMODE = %RUNMODE.
  PERFORM %INITIALIZATION.

*---------------------------------------------------------------*
*       FORM %initialization                                    *
*---------------------------------------------------------------*

FORM %INITIALIZATION.


ENDFORM.                    "%initialization

START-OF-SELECTION.
  IF %RUNMODE-EXTR_ON <> SPACE.
    CALL FUNCTION 'ZIQG000000000004EXTR'
      TABLES
        %SELOPT = %SELOPTIONS
        DTAB    = %DTAB
      CHANGING
        %RTMODE = %RUNMODE
      EXCEPTIONS
        NO_DATA = 1
        OTHERS  = 2.
    %SUBRC = SY-SUBRC.
    CALL FUNCTION 'RSAQRT_CHECK_EXTR'
      EXPORTING
        EXTR_SUBRC = %SUBRC
      TABLES
        DTAB       = %DTAB
      CHANGING
        RTMODE     = %RUNMODE.

  ENDIF.


END-OF-SELECTION.

  IF %RUNMODE-SHOW_ON <> SPACE.
*    call function '/1BCDWB/IQG000000000004SHOW'
*         tables   %dtab   = %dtab
*         changing %rtmode = %runmode.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME       = 'ZIQG000000000004'
      CHANGING
        CT_FIELDCAT            = TL_CAT
      EXCEPTIONS
        INCONSISTENT_INTERFACE = 1
        PROGRAM_ERROR          = 2
        OTHERS                 = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT TL_CAT INTO WL_CAT.
      IF WL_CAT-FIELDNAME EQ 'KTOGR'.
        WL_CAT-SELTEXT_S     = 'Classe imob.'.
        WL_CAT-SELTEXT_M     = 'Classe imob.'.
        WL_CAT-SELTEXT_L     = 'Classe imob.'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'KTANSW'.
        WL_CAT-SELTEXT_S     = 'Cont.Razão'.
        WL_CAT-SELTEXT_M     = 'Cont.Razão'.
        WL_CAT-SELTEXT_L     = 'Cont.Razão'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'AKTIV'.
        WL_CAT-SELTEXT_S     = 'Dt. incorporação'.
        WL_CAT-SELTEXT_M     = 'Dt. incorporação'.
        WL_CAT-SELTEXT_L     = 'Dt. incorporação'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'ZUGDT'.
        WL_CAT-SELTEXT_S     = 'Dt. aquisição'.
        WL_CAT-SELTEXT_M     = 'Dt. aquisição'.
        WL_CAT-SELTEXT_L     = 'Dt. aquisição'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'ORD43'.
        WL_CAT-SELTEXT_S     = 'RECAP'.
        WL_CAT-SELTEXT_M     = 'RECAP'.
        WL_CAT-SELTEXT_L     = 'RECAP'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'ORD42'.
        WL_CAT-SELTEXT_S     = 'Depre. Ince.'.
        WL_CAT-SELTEXT_M     = 'Depreciação Incen.'.
        WL_CAT-SELTEXT_L     = 'Depreciação Incentivada'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'ORD41'.
        WL_CAT-SELTEXT_S     = 'CC.1'.
        WL_CAT-SELTEXT_M     = 'CC.1'.
        WL_CAT-SELTEXT_L     = 'CC.1'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'ORD44'.
        WL_CAT-SELTEXT_S     = 'CC.4'.
        WL_CAT-SELTEXT_M     = 'CC.4'.
        WL_CAT-SELTEXT_L     = 'CC.4'.
        MODIFY TL_CAT FROM WL_CAT.
      ELSEIF WL_CAT-FIELDNAME EQ 'GDLGRP'.
        WL_CAT-SELTEXT_S     = 'CC.5'.
        WL_CAT-SELTEXT_M     = 'CC.5'.
        WL_CAT-SELTEXT_L     = 'CC.5'.
        MODIFY TL_CAT FROM WL_CAT.
      ENDIF.
    ENDLOOP.
    WL_LAYOUT-HEADER_TEXT = 'Depreciações lançadas por imobilizado e período contábil'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = SY-REPID
        IS_LAYOUT          = WL_LAYOUT
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        IT_FIELDCAT        = TL_CAT
        I_SAVE             = 'A'
*       IT_EVENTS          = EVENTS
*       IS_PRINT           = T_PRINT
      TABLES
        T_OUTTAB           = %DTAB[].
*  call function 'RSAQRT_ALV_DISPLAY'
*       tables     dtab         = %dtab[]
*       changing   rtmode       = %runmode.
  ENDIF.


*----------------------------------------------------------------
*    special code for old API and BW extractor calls
*----------------------------------------------------------------

FORM %SET_DATA CHANGING P_LINES TYPE I.

  IMPORT LDATA TO %DTAB FROM MEMORY ID 'AQLISTDATA'.
  DESCRIBE TABLE %DTAB LINES P_LINES.
  FREE MEMORY ID 'AQLISTDATA'.

ENDFORM.                    "%set_data

*&---------------------------------------------------------------------*
*&      Form  %get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DTAB     text
*      -->P_FIRST    text
*      -->P_LAST     text
*----------------------------------------------------------------------*
FORM %GET_DATA TABLES P_DTAB  STRUCTURE %DTAB
               USING  P_FIRST TYPE I
                      P_LAST  TYPE I.

  APPEND LINES OF %DTAB FROM P_FIRST TO P_LAST TO P_DTAB.

ENDFORM.                    "%get_data

*&---------------------------------------------------------------------*
*&      Form  %get_ref_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LID      text
*      -->P_REF      text
*----------------------------------------------------------------------*
FORM %GET_REF_TO_TABLE USING P_LID   TYPE AQL_LID
                             P_REF   TYPE REF TO DATA
                             P_SUBRC TYPE I.

  IF P_LID = %IQID-LID.
    CREATE DATA P_REF LIKE %DTAB[].
    P_SUBRC = 0.
  ELSE.
    P_SUBRC = 4.
  ENDIF.

ENDFORM.                    "%get_ref_to_table
