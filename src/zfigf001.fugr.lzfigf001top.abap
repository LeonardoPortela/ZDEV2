FUNCTION-POOL ZFIGF001.                     "MESSAGE-ID ..
*---------------------------------------------------------------------*
*       CLASS lcl_cale DEFINITION
*---------------------------------------------------------------------*
*       calendar buffering - new with YEKP9CK151906
*---------------------------------------------------------------------*
*CLASS LCL_CALE DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS: CHECK_WORK_DATE
*                     IMPORTING
*                       IM_PERSONNEL_NUMBER TYPE PERNR_D
*                       IM_CHECK_DATE       TYPE D
*                     EXPORTING
*                       EX_WORK_DAY TYPE    XFELD
*                       EX_NEXT_WORK_DATE   TYPE D
*                       EX_DAY_ATTRIBUTES   TYPE CASDAYATTR,
*
*                   GET_CALENDAR_IDS
*                     IMPORTING
*                       IM_PERSONNEL_NUMBER TYPE PERNR_D
*                       IM_VDATE            TYPE D
*                     EXPORTING
*                       EX_FCAL_ID          TYPE FABKL
*                       EX_HCAL_ID          TYPE HIDENT,
*
*                   GET_HOLIDAYS
*                     IMPORTING
*                       IM_PERSONNEL_NUMBER TYPE PERNR_D
*                       IM_BEGIN_DATE       TYPE D
*                       IM_END_DATE         TYPE D
*                       IM_GET_WEEKEND_DAYS TYPE XFELD
*                      EXPORTING
*                        EX_HOLIDAYS        TYPE CATSXT_ISCAL_DAY_ITAB,
*
*                  GET_DAY_INFO
*                    IMPORTING
*                       IM_FCAL_ID          TYPE FABKL
*                       IM_HCAL_ID          TYPE HIDENT
*                       IM_DATE             TYPE D
*                    EXPORTING
*                       EX_DAY_INFO         TYPE ISCAL_DAY.
*
*    METHODS:       CONSTRUCTOR
*                     IMPORTING
*                       IM_FCAL_ID TYPE FABKL
*                       IM_HCAL_ID TYPE HIDENT.
*
*  PRIVATE SECTION.
*    TYPES:   BEGIN OF  TY_T001W_WA.
*    INCLUDE TYPE T001W.
*    TYPES:     HOCID   TYPE TFACD-HOCID,
*             END OF TY_T001W_WA.
*
*    CLASS-DATA:
*          T001W_WA      TYPE TY_T001W_WA,
*          I0315         TYPE CATS_0315,
*          I0315_TAB     TYPE STANDARD TABLE OF CATS_0315
*                             INITIAL SIZE 10,
*          OLD_PERNR     TYPE PERNR_D,
*          DBTAB(6)      TYPE C VALUE 'PA0315',
*
*          BEGIN OF ITAB_LN,
*            FCAL_ID TYPE FABKL,
*            HCAL_ID TYPE HIDENT,
*            OBJREF  TYPE REF TO LCL_CALE,
*          END OF ITAB_LN,
*
*          ITAB LIKE HASHED TABLE OF ITAB_LN
*                      WITH UNIQUE KEY FCAL_ID HCAL_ID
*                      INITIAL SIZE 10.
*
*    DATA: FCAL_ID   TYPE FABKL,
*          HCAL_ID   TYPE HIDENT,
*          HTAB      TYPE HASHED TABLE OF ISCAL_DAY WITH UNIQUE KEY DATE.
*
*    METHODS:      GET_DAY_DETAILS
*                    IMPORTING
*                       IM_DATE             TYPE D
*                    EXPORTING
*                       EX_DAY_INFO         TYPE ISCAL_DAY.
*
*ENDCLASS.                    "lcl_cale DEFINITION

**---------------------------------------------------------------------*
**       CLASS lcl_ddic_info DEFINITION
**---------------------------------------------------------------------*
**       DDIC buffering
**---------------------------------------------------------------------*
*CLASS: LCL_DDIC_INFO DEFINITION.
*  PUBLIC SECTION.
*
*    TYPES:        BEGIN OF TY_STRUC_LN,
*                    NAME      TYPE STRING,
*                    DDIC_INFO TYPE DDFIELDS,
*                  END OF TY_STRUC_LN.
*
*    CLASS-DATA:    STRUC_TAB TYPE HASHED TABLE OF TY_STRUC_LN
*                     WITH UNIQUE KEY NAME,
*
*                   STRUC_LN  LIKE LINE OF STRUC_TAB.
*
*    CLASS-METHODS: GET_DATA
*                     IMPORTING
*                       IM_STRUCTURE_NAME TYPE C
*                     EXPORTING
*                       EX_DDIC_INFO      TYPE DDFIELDS.
*
*ENDCLASS.                    "lcl_ddic_info DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_longtext DEFINITION
*---------------------------------------------------------------------*
*       longtext display/change handling - new with YEKP9CK117331
*---------------------------------------------------------------------*
CLASS LCL_LONGTEXT DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:   CO_X  TYPE XFELD VALUE 'X'.
* begin XNSAL0K005520
*                 co_text_separator type txline
*                         value '*** Internal Memo ***'.

    TYPES: BEGIN OF TY_MEMO_TAB,
             TMP_KEY TYPE CATSCOUNTE,
           END OF TY_MEMO_TAB.
* end XNSAL0K005520

    DATA: CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER
                                                    READ-ONLY,
          DISPLAY_MODE TYPE XFELD                   READ-ONLY,
          EDITOR       TYPE REF TO CL_GUI_TEXTEDIT  READ-ONLY,
          FIRST_REC    TYPE I                       READ-ONLY,
          LAST_REC     TYPE I                       READ-ONLY,
          LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB    READ-ONLY,
*         Begin YEKAL0K010641
          OLD_PERNR    TYPE PERNR_D                 READ-ONLY,
*         End YEKAL0K010641
          TABIX        TYPE SYTABIX                 READ-ONLY,
          RCOUNT       TYPE I                       READ-ONLY,
          ACT_TAB      TYPE CATSXT_WA_ITAB          READ-ONLY,
          ACT_LN       LIKE LINE OF ACT_TAB         READ-ONLY,
          WORKDATE(10) TYPE C                       READ-ONLY,
* begin XNSAL0K005520
          LTXT_SPLIT   TYPE XFELD VALUE ' ',
          MEMO_TAB     TYPE HASHED TABLE OF TY_MEMO_TAB READ-ONLY
                            WITH UNIQUE KEY TMP_KEY .
* end XNSAL0K005520

    METHODS: CONSTRUCTOR
                IMPORTING
                  IM_DISPLAY_MODE  TYPE XFELD
                  IM_TABIX         TYPE I
                  IM_RECORD_COUNT  TYPE I
                  IM_ACTIVITY_DATA TYPE CATSXT_WA_ITAB,

             FREE,

             GET_TEXT
* begin XNSAL0K005520
               IMPORTING IM_LONGTEXT_ID TYPE CATSXT_LONGTEXT_ID,
* end XNSAL0K005520

             GET_ACTIVITIES
               RETURNING VALUE(RE_ACTIVITY_DATA) TYPE CATSXT_WA_ITAB,

             INTERNAL_MEMO,

             NEXT,

             CHANGE_LONGTEXT_ID,                          "XNSAL0K005520

             PREVIOUS,

             READ_ACT_TAB,

             START,

             SET_TEXT,

             SET_TITLE.

*   begin XNSAL0K010641
    CLASS-METHODS: SPLIT_LONGTEXT
                     IMPORTING
                       IM_CATSXT_LONGTEXT   TYPE CATSXT_LONGTEXT_ITAB
                     EXPORTING
                       EX_LONGTEXT_EXTERNAL TYPE CATSXT_LONGTEXT_ITAB
                       EX_LONGTEXT_INTERNAL TYPE CATSXT_LONGTEXT_ITAB.
*   end XNSAL0K010641

ENDCLASS.                    "lcl_longtext DEFINITION

* Begin YEKAL0K026011
CLASS LCL_SIMPLE_TEXT_EDITOR DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:   CO_X  TYPE XFELD VALUE 'X'.

    DATA: CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER
                                                    READ-ONLY,
          DISPLAY_MODE TYPE XFELD                   READ-ONLY,
          EDITOR       TYPE REF TO CL_GUI_TEXTEDIT  READ-ONLY,
          LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB    READ-ONLY,
          TITLE        TYPE SYTITLE                 READ-ONLY.

    METHODS: CONSTRUCTOR
               IMPORTING
                 IM_TITLE        TYPE SYTITLE
                 IM_LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB
                 IM_DISPLAY_MODE TYPE XFELD,

             FREE,

             GET_TEXT
               RETURNING
                 VALUE(RE_LONGTEXT) TYPE CATSXT_LONGTEXT_ITAB,

             START.

ENDCLASS.                    "lcl_simple_text_editor DEFINITION
* End YEKAL0K026011

*CLASS LCL_SF_RECEIVER DEFINITION.
*  PUBLIC SECTION.
*    METHODS: CHECK
*               IMPORTING
*                 IM_RECEIVER_NAME TYPE C
*               RETURNING
*                 VALUE(RE_RECEIVERS) TYPE SWFBORPTAB,
*
*             CHECK_DISTRIBUTION_LIST
*               IMPORTING
*                 IM_LIST_NAME TYPE C
*                 IM_LIST_TYPE TYPE SO_ESCAPE
*               RETURNING
*                 VALUE(RE_RECEIVERS) TYPE SWFBORPTAB
*               EXCEPTIONS
*                 FAILED.
*
*ENDCLASS.                    "lcl_sf_receiver DEFINITION


* Types, tables and data definitions
TYPE-POOLS: CXTAB,
            ICON,                                          "VWLP9C117331
            SZADR.

TABLES:     CATSXT_USER_DFLT_SCR,
            CATSXT_USER_DEFAULT,
            CATSXT_LTXT,                                  "XNSAL0K005520
            CATSXT_SF.

*           CATSXT constant definitions
INCLUDE:    RCATSXT_CONSTANTS,
*           BOR object processing
           <CNTN01>.

DATA:       ENAME               TYPE EMNAM,
*           Begin YEKP9CK117331
*           longtext_container  type ref to cl_gui_custom_container,
*           longtext_editor     type ref to cl_gui_textedit,
*           longtext_tab        type catsxt_longtext_itab,
            LONGTEXT            TYPE REF TO LCL_LONGTEXT,
            CB_EXIT_ON_SAVE     TYPE XFELD,
            FCODE_TAB           TYPE STANDARD TABLE OF SYTCODE,
*           End YEKP9CK117331
            OK_CODE             LIKE SY-TCODE,
            GF_PERNR            TYPE PERNR_D,
            RB_MONTHS           TYPE CHAR01,
            RB_YEAR             TYPE CHAR01,
            TMP_KEY             TYPE CATSCOUNTE,
            D0100_VIEW_MODE     TYPE CHAR1,
*           begin XNSAL0K005520
            OLD_LONGTEXT_ID     TYPE CATSXT_LONGTEXT_ID,
            INT_LONGTEXT_TAB    TYPE CATSXT_LONGTEXT_ITAB,
            EXT_LONGTEXT_TAB    TYPE CATSXT_LONGTEXT_ITAB,
*           end XNSAL0K005520

*           Begin YEKAL0K026011
            STE                 TYPE REF TO LCL_SIMPLE_TEXT_EDITOR,
*           End YEKAL0K026011
            WG_REPROC,
            BEGIN OF LONGTEXT_TITLE,
              TASKTYPE  TYPE CATSXT_TASKTYPE_TEXT,
              TASKLEVEL TYPE CATSXT_TASKLEVEL_TEXT,
              WORKDATE(10),
            END OF LONGTEXT_TITLE,

*           Begin YEKP9CK117331
            BEGIN OF RB_PROP,
              METHOD1 TYPE XFELD,
              METHOD2 TYPE XFELD,
              METHOD3 TYPE XFELD,
              METHOD4 TYPE XFELD,
            END OF RB_PROP.
*           End YEKP9CK117331

CONSTANTS:  CO_03 TYPE CATSXT_RELDATE VALUE '03'.

*           Load interface definition because of syntax check problems
INTERFACE:  IF_EX_CATSXT_EVENT LOAD.
  CLASS LCL_LONGTEXT IMPLEMENTATION.
  METHOD CONSTRUCTOR.

    DATA: LF_ACT_TABIX TYPE SYTABIX.

    FIELD-SYMBOLS: <LS_ACT>  LIKE LINE OF ACT_TAB.

    ACT_TAB      = IM_ACTIVITY_DATA.
    RCOUNT       = IM_RECORD_COUNT.
    DISPLAY_MODE = IM_DISPLAY_MODE.
    TABIX        = IM_TABIX.

    IF IM_TABIX > RCOUNT.
      TABIX = 1.
    ENDIF.

    CHECK NOT DISPLAY_MODE IS INITIAL.
*   Find out the first and last record that
*   contain longtext info -> to enable/disable
*   the paging buttons
    LOOP AT ACT_TAB ASSIGNING <LS_ACT>.
      LF_ACT_TABIX = SY-TABIX.

      IF NOT <LS_ACT>-LONGTEXT_TAB IS INITIAL.
        IF FIRST_REC IS INITIAL.
          FIRST_REC = LF_ACT_TABIX.
        ENDIF.

        LAST_REC = LF_ACT_TABIX.
      ELSE.
        LOOP AT <LS_ACT>-COMP_TAB TRANSPORTING NO FIELDS
          WHERE NOT LONGTEXT IS INITIAL.

          IF FIRST_REC IS INITIAL.
            FIRST_REC = LF_ACT_TABIX.
          ENDIF.

          LAST_REC = LF_ACT_TABIX.

          EXIT.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF TABIX < FIRST_REC OR TABIX > LAST_REC.
      TABIX = FIRST_REC.
    ENDIF.

  ENDMETHOD.                    "lcl_longtext

  METHOD FREE.

    CALL METHOD:EDITOR->FREE,
      CONTAINER->FREE.

  ENDMETHOD.                    "lcl_longtext

  METHOD GET_ACTIVITIES.

    RE_ACTIVITY_DATA = ACT_TAB.

  ENDMETHOD.                    "lcl_longtext

  METHOD INTERNAL_MEMO.
    DATA: LS_LTXT  LIKE LINE OF LONGTEXT_TAB,
          LF_TABIX TYPE I,
          LF_DATUM(10) TYPE C,
          LF_ENAME     TYPE EMNAM,
          LF_PERNR     TYPE PERNR_D,
          LF_UZEIT(8)  TYPE C,

          BEGIN OF IM,
            F1(68)    TYPE C,
            F2(1)     TYPE C,
            F3(3)     TYPE C VALUE '***',
          END OF IM.

    CALL METHOD GET_TEXT                          "XNSAL0K005520
      EXPORTING                                   "XNSAL0K005520
        IM_LONGTEXT_ID = CATSXT_LTXT-ID.          "XNSAL0K005520

    IF NOT LONGTEXT_TAB IS INITIAL.
      APPEND INITIAL LINE TO LONGTEXT_TAB.
    ENDIF.

    LF_PERNR = CL_TIME_SHEET_CATSXT=>GET_PERNR_OF_UNAME( ).

    CALL FUNCTION 'CATS_GET_EMPLOYEE_NAME'
      EXPORTING
        PERNR  = LF_PERNR
      IMPORTING
        NAME   = LF_ENAME
      EXCEPTIONS
        OTHERS = 1.

    IF LF_ENAME IS INITIAL.
      LF_ENAME = SY-UNAME.
    ENDIF.

    WRITE SY-UZEIT TO LF_UZEIT USING EDIT MASK '__:__:__'.
    WRITE SY-DATUM TO LF_DATUM DD/MM/YYYY.

    CONCATENATE '***'
                'Internal Memo'(007)
                 LF_ENAME
                 LF_DATUM
                 LF_UZEIT
                 INTO IM-F1 SEPARATED BY SPACE.

    LS_LTXT = IM.

    APPEND LS_LTXT TO LONGTEXT_TAB.
    LF_TABIX = SY-TABIX.

    APPEND INITIAL LINE TO LONGTEXT_TAB.

    CALL METHOD SET_TEXT.

    ACT_LN-LONGTEXT_TAB = LONGTEXT_TAB.

    MODIFY ACT_TAB FROM ACT_LN INDEX TABIX
      TRANSPORTING LONGTEXT_TAB.

  ENDMETHOD.                    "lcl_longtext

  METHOD NEXT.
    DATA: LF_SAVE_TABIX TYPE SYTABIX,
          LF_FOUND      TYPE XFELD.

    CHECK TABIX < RCOUNT.

    LF_SAVE_TABIX = TABIX.

    IF DISPLAY_MODE IS INITIAL.
*     Get text from control
      CALL METHOD GET_TEXT                      "XNSAL0K005520
        EXPORTING                               "XNSAL0K005520
          IM_LONGTEXT_ID = CATSXT_LTXT-ID.      "XNSAL0K005520

*     Get next record
      ADD 1 TO TABIX.

*     ... and send it to the control
      CALL METHOD:READ_ACT_TAB,
                  SET_TEXT.
    ELSE.
*     Display mode is on -> find next record with existing text
      WHILE TABIX < RCOUNT.
        ADD 1 TO TABIX.

        CALL METHOD READ_ACT_TAB.
        IF NOT LONGTEXT_TAB IS INITIAL.
          LF_FOUND = CO_X.
          EXIT.
        ENDIF.
      ENDWHILE.

      IF LF_FOUND IS INITIAL.
*       No further texts exist -> restore values
        TABIX = LF_SAVE_TABIX.
        CALL METHOD READ_ACT_TAB.
*       send message -> no more texts exist
      ELSE.
*       Next text found -> send it to the control
        CALL METHOD:SET_TEXT.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "lcl_longtext

  METHOD PREVIOUS.
    DATA: LF_SAVE_TABIX TYPE SYTABIX,
          LF_FOUND      TYPE XFELD.

    CHECK TABIX > 1.

    IF DISPLAY_MODE IS INITIAL.
*     Get text from control
      CALL METHOD GET_TEXT                        "XNSAL0K005520
        EXPORTING                                 "XNSAL0K005520
          IM_LONGTEXT_ID = CATSXT_LTXT-ID.        "XNSAL0K005520

      SUBTRACT 1 FROM TABIX.

      CALL METHOD:READ_ACT_TAB,
        SET_TEXT.

    ELSE.
*     Display mode is on -> find previous record with existing text
      WHILE TABIX > 1.
        SUBTRACT 1 FROM TABIX.

        CALL METHOD READ_ACT_TAB.
        IF NOT LONGTEXT_TAB IS INITIAL.
          LF_FOUND = CO_X.
          EXIT.
        ENDIF.
      ENDWHILE.

      IF LF_FOUND IS INITIAL.
*       No further texts exist -> restore values
        TABIX = LF_SAVE_TABIX.
        CALL METHOD READ_ACT_TAB.
*       send message -> no more texts exist
      ELSE.
*       Next text found -> send it to the control
        CALL METHOD:SET_TEXT.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "lcl_longtext

  METHOD START.
    DATA: LF_PERNR_OF_UNAME TYPE PERNR_D.                 "XNSAL0K005520

    CALL METHOD:READ_ACT_TAB.

    CREATE OBJECT CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'LONGTEXT'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CHECK SY-SUBRC IS INITIAL.

    CREATE OBJECT EDITOR
      EXPORTING
        PARENT                 =  CONTAINER
        WORDWRAP_MODE          = '2'
        WORDWRAP_POSITION      = '72'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

    IF NOT DISPLAY_MODE IS INITIAL.
*     Set control to display only
      CALL METHOD EDITOR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR->TRUE.
    ENDIF.

* begin XNSAL0K005520
    IF LONGTEXT->LTXT_SPLIT = CO_X.
      REFRESH MEMO_TAB.
      LF_PERNR_OF_UNAME = CL_TIME_SHEET_CATSXT=>GET_PERNR_OF_UNAME( ).
*     Set default longtext id
      IF CATSXT_LTXT-ID IS INITIAL.
        IF ACT_LN-PERNR <> LF_PERNR_OF_UNAME AND
           NOT INT_LONGTEXT_TAB IS INITIAL.
          CATSXT_LTXT-ID = 'INTT'.
        ELSE.
          CATSXT_LTXT-ID = 'EXTT'.
        ENDIF.
      ENDIF.
      OLD_LONGTEXT_ID = CATSXT_LTXT-ID.
    ELSE.
      CLEAR CATSXT_LTXT-ID.
    ENDIF.
* end XNSAL0K005520

    CALL METHOD SET_TEXT.

  ENDMETHOD.                    "lcl_longtext

  METHOD READ_ACT_TAB.
    DATA: LF_GET_LONGTEXT TYPE XFELD,               "XNSAL0K005520
          LF_TABIX1       TYPE SYTABIX,             "XNSAL0K005520
          LF_TABIX2       TYPE SYTABIX,             "XNSAL0K005520
          LF_COUNT        TYPE SYTABIX.             "XNSAL0K005520

    DATA: LS_CC           TYPE TCATX_CC. "...........YEKAL0K010641

    FIELD-SYMBOLS: <LFS_TEXT> TYPE TXLINE.          "XNSAL0K005520

    REFRESH: LONGTEXT_TAB,                          "XNSAL0K005520
             EXT_LONGTEXT_TAB,                      "XNSAL0K005520
             INT_LONGTEXT_TAB.                      "XNSAL0K005520

    READ TABLE ACT_TAB INDEX TABIX INTO ACT_LN.
    IF NOT SY-SUBRC IS INITIAL.
      CLEAR ACT_LN.
      EXIT.
    ENDIF.

*   Begin YEKAL0K010641
    IF OLD_PERNR <> ACT_LN-PERNR.
      OLD_PERNR = ACT_LN-PERNR.

      CALL FUNCTION 'CATSXT_GET_CENTRAL_CONTROL'
        EXPORTING
          IM_PERSONNEL_NUMBER       = ACT_LN-PERNR
        IMPORTING
          EX_CENTRAL_CONTROL        = LS_CC
        EXCEPTIONS
          EMP_OBJ_AND_PERNR_INITIAL = 1
          CENTRAL_CONTROL_NOT_FOUND = 2
          OTHERS                    = 3.

      IF SY-SUBRC IS INITIAL.
        IF NOT ( LS_CC-LTXT_COMP  IS INITIAL AND
                 LS_CC-LTXT_SPLIT IS INITIAL ).
          LTXT_SPLIT = LS_CC-LTXT_SPLIT.
        ENDIF.
      ENDIF.
    ENDIF.
*   End YEKAL0K010641

*   Always call enrich method because new records might
*   not have the longtext plus type/level texts set
    IF ACT_LN-LONGTEXT_TAB IS INITIAL AND           "XNSAL0K005520
       ACT_LN-LONGTEXT_ICON CS ICON_ANNOTATION(3).  "XNSAL0K005520
      LF_GET_LONGTEXT = CO_X.
    ENDIF.

    CALL METHOD CL_TIME_SHEET_CATSXT=>ENRICH_ACTIVITY
      EXPORTING
        IM_GET_LONG_TEXT = LF_GET_LONGTEXT
      CHANGING
        CH_ACTIVITY      = ACT_LN.

    LONGTEXT_TAB = ACT_LN-LONGTEXT_TAB.

*   begin XNSAL0K005520
    IF LONGTEXT->LTXT_SPLIT = CO_X.
*     begin XNSAL0K010641
*      loop at longtext_tab assigning <lfs_text>
*              where table_line cs co_text_separator.
*        lf_tabix1 = sy-tabix - 1.
*        lf_tabix2 = sy-tabix + 1.
*        exit.
*      endloop.
*      if sy-subrc is initial.
*        if lf_tabix1 > 0.
*append lines of longtext_tab to lf_tabix1 to ext_longtext_tab.
*        endif.
*        describe table longtext_tab lines lf_count.
*        if lf_count >= lf_tabix2.
*append lines of longtext_tab from lf_tabix2 to int_longtext_tab.
*        endif.
*      elseif not longtext_tab is initial.
*        ext_longtext_tab = longtext_tab.
*      endif.
      CALL METHOD SPLIT_LONGTEXT
        EXPORTING
          IM_CATSXT_LONGTEXT   = LONGTEXT_TAB
        IMPORTING
          EX_LONGTEXT_EXTERNAL = EXT_LONGTEXT_TAB
          EX_LONGTEXT_INTERNAL = INT_LONGTEXT_TAB.
*     end XNSAL0K010641
    ENDIF.
*   end XNSAL0K005520

    CALL METHOD SET_TITLE.

  ENDMETHOD.                    "lcl_longtext

  METHOD SET_TEXT.
*   begin XNSAL0K005520
*    data: lf_ename type pernr_list_structure-ename.
    DATA: LT_LONGTEXT  TYPE CATSXT_LONGTEXT_ITAB,
          LS_LTXT      TYPE TXLINE,
          LS_MEMO      LIKE LINE OF MEMO_TAB,
          LF_DATUM(10) TYPE C,
          LF_ENAME     TYPE EMNAM,
          LF_UZEIT(8)  TYPE C,
          LF_COUNT     TYPE SYTABIX,
          LF_TABIX     TYPE SYTABIX,
          LF_PERNR     TYPE PERNR_D,

          BEGIN OF IM,
            F1(68)    TYPE C,
            F2(1)     TYPE C,
            F3(3)     TYPE C VALUE '***',
          END OF IM.

*   begin XNSAL0K098521
    IF NOT DISPLAY_MODE IS INITIAL.
      IF CATSXT_LTXT-ID = 'EXTT' AND EXT_LONGTEXT_TAB IS INITIAL.
        CATSXT_LTXT-ID = 'INTT'.
      ELSEIF CATSXT_LTXT-ID = 'INTT' AND INT_LONGTEXT_TAB IS INITIAL.
        CATSXT_LTXT-ID = 'EXTT'.
      ENDIF.
    ENDIF.
*   end XNSAL0K098521

    CASE CATSXT_LTXT-ID.
*     external long text
      WHEN 'EXTT'.
        LT_LONGTEXT = EXT_LONGTEXT_TAB.
*     internal long text
      WHEN 'INTT'.
        LT_LONGTEXT = INT_LONGTEXT_TAB.
        IF DISPLAY_MODE IS INITIAL.
          READ TABLE MEMO_TAB WITH TABLE KEY TMP_KEY = ACT_LN-TMP_KEY
                                   TRANSPORTING NO FIELDS.
          IF NOT SY-SUBRC IS INITIAL.
            LF_PERNR = CL_TIME_SHEET_CATSXT=>GET_PERNR_OF_UNAME( ).
            CALL FUNCTION 'CATS_GET_EMPLOYEE_NAME'
              EXPORTING
                PERNR  = LF_PERNR
              IMPORTING
                NAME   = LF_ENAME
              EXCEPTIONS
                OTHERS = 1.
            IF LF_ENAME IS INITIAL.
              LF_ENAME = SY-UNAME.
            ENDIF.
            WRITE SY-UZEIT TO LF_UZEIT USING EDIT MASK '__:__:__'.
            WRITE SY-DATUM TO LF_DATUM DD/MM/YYYY.
            CONCATENATE '***'
                         LF_ENAME
                         LF_DATUM
                         LF_UZEIT
                         INTO IM-F1 SEPARATED BY SPACE.
            LS_LTXT = IM.
            APPEND LS_LTXT TO LT_LONGTEXT.
            MOVE ACT_LN-TMP_KEY TO LS_MEMO-TMP_KEY.
            INSERT LS_MEMO INTO TABLE MEMO_TAB.
          ENDIF.
          APPEND INITIAL LINE TO LT_LONGTEXT.
          LF_COUNT = SY-TABIX.
        ENDIF.
*     no longtext split
      WHEN OTHERS.
        LT_LONGTEXT = LONGTEXT_TAB.
    ENDCASE.
*   end XNSAL0K005520

    CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
      EXPORTING
*        table           = longtext_tab                 "XNSAL0K005520
        TABLE           = LT_LONGTEXT                   "XNSAL0K005520
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

*   begin XNSAL0K005520
    IF LONGTEXT->LTXT_SPLIT =  CO_X    AND
       NOT LF_COUNT         IS INITIAL AND
       DISPLAY_MODE         IS INITIAL.

      CALL METHOD EDITOR->GO_TO_LINE
        EXPORTING
          LINE                   = LF_COUNT
        EXCEPTIONS
          ERROR_CNTL_CALL_METHOD = 1.

      LOOP AT LT_LONGTEXT TRANSPORTING NO FIELDS
              WHERE TABLE_LINE CP '#*#*#* *#*#*#*'.
        LF_TABIX = SY-TABIX.
        CALL METHOD EDITOR->PROTECT_LINES
          EXPORTING
            FROM_LINE              = LF_TABIX
            TO_LINE                = LF_TABIX
          EXCEPTIONS
            ERROR_CNTL_CALL_METHOD = 1
            INVALID_PARAMETER      = 2.
      ENDLOOP.
    ENDIF.
*   end XNSAL0K005520

    CALL FUNCTION 'CATS_GET_EMPLOYEE_NAME'
      EXPORTING
        PERNR           = ACT_LN-PERNR
      IMPORTING
        NAME            = LF_ENAME
      EXCEPTIONS
        PERNR_NOT_FOUND = 1
        OTHERS          = 2.

    CHECK NOT LF_ENAME IS INITIAL.

    CALL METHOD EDITOR->SET_STATUS_TEXT
      EXPORTING
        STATUS_TEXT            = LF_ENAME
      EXCEPTIONS
        ERROR_CNTL_CALL_METHOD = 1
        OTHERS                 = 2.

*   Begin YEKP6BK036557
    IF DISPLAY_MODE IS INITIAL.
      CALL METHOD EDITOR->SET_FOCUS
        EXPORTING
          CONTROL           = EDITOR
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
    ENDIF.
*   End YEKP6BK036557

  ENDMETHOD.                    "lcl_longtext

  METHOD SET_TITLE.

    WRITE ACT_LN-WORKDATE TO WORKDATE DD/MM/YYYY.

  ENDMETHOD.                    "lcl_longtext

  METHOD GET_TEXT.
    DATA: LT_TEXT TYPE CATSXT_LONGTEXT_ITAB,
*         begin XNSAL0K005520
          LT_INT_TEXT            TYPE CATSXT_LONGTEXT_ITAB,
          LT_EXT_TEXT            TYPE CATSXT_LONGTEXT_ITAB,
          LF_DEL_CNT             TYPE SYTABIX,
          LF_COUNT               TYPE SYTABIX.
*         end XNSAL0K005520

    FIELD-SYMBOLS: <LS_COMP> LIKE LINE OF ACT_LN-COMP_TAB.

    CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE           = LT_TEXT
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

    CALL METHOD CL_GUI_CFW=>FLUSH.

*   begin XNSAL0K005520
    DESCRIBE TABLE LT_TEXT LINES LF_COUNT.

    DO.
      DELETE LT_TEXT FROM  LF_COUNT
                     WHERE TABLE_LINE IS INITIAL.
      IF SY-SUBRC IS INITIAL AND LF_COUNT > 1.
        SUBTRACT 1 FROM LF_COUNT.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CASE IM_LONGTEXT_ID.
      WHEN 'EXTT'.
        EXT_LONGTEXT_TAB = LT_EXT_TEXT = LT_TEXT.
        IF NOT INT_LONGTEXT_TAB IS INITIAL.
          APPEND CO_TEXT_SEPARATOR TO LT_TEXT.
          APPEND LINES OF INT_LONGTEXT_TAB TO LT_TEXT.
        ENDIF.
      WHEN 'INTT'.
        DELETE LT_TEXT FROM  LF_COUNT
                       WHERE TABLE_LINE CP '#*#*#* *#*#*#*'.
        IF SY-SUBRC IS INITIAL.
          DELETE MEMO_TAB WHERE TMP_KEY = ACT_LN-TMP_KEY.
          DESCRIBE TABLE LT_TEXT LINES LF_COUNT.
          LOOP AT LT_TEXT TRANSPORTING NO FIELDS
                    WHERE TABLE_LINE IS INITIAL OR
                          TABLE_LINE CP '#*#*#* *#*#*#*'.
            ADD 1 TO LF_DEL_CNT.
          ENDLOOP.
          IF LF_COUNT = LF_DEL_CNT.
            REFRESH LT_TEXT.
          ENDIF.
        ENDIF.
        INT_LONGTEXT_TAB = LT_INT_TEXT = LT_TEXT.
        IF NOT LT_INT_TEXT IS INITIAL.
          INSERT CO_TEXT_SEPARATOR INTO LT_TEXT INDEX 1.
        ENDIF.
        INSERT LINES OF EXT_LONGTEXT_TAB INTO LT_TEXT INDEX 1.
    ENDCASE.
*   end XNSAL0K005520

    IF DISPLAY_MODE IS INITIAL.
      ACT_LN-LONGTEXT_TAB = LT_TEXT.

      IF LT_TEXT <> LONGTEXT_TAB.
*       Text was changed -> set pflag
        READ TABLE ACT_LN-COMP_TAB WITH KEY LONGTEXT = CO_X
                                   ASSIGNING <LS_COMP>.

        IF SY-SUBRC IS INITIAL AND <LS_COMP>-PFLAG <> CO_DELETE.
          IF <LS_COMP>-PFLAG IS INITIAL.
            <LS_COMP>-PFLAG = CO_CHANGE.
          ENDIF.
        ELSE.
          LOOP AT ACT_LN-COMP_TAB ASSIGNING <LS_COMP>
            WHERE PFLAG <> CO_DELETE
            AND   NO_CHANGE IS INITIAL.

            IF <LS_COMP>-PFLAG IS INITIAL.
              <LS_COMP>-PFLAG = CO_CHANGE.
            ENDIF.

            EXIT.
          ENDLOOP.
        ENDIF.

        MODIFY ACT_TAB FROM ACT_LN INDEX TABIX
          TRANSPORTING COMP_TAB.

      ENDIF.

*     begin XNSAL0K005520
      IF ACT_LN-LONGTEXT_TAB IS INITIAL.
        ACT_LN-LONGTEXT_ICON = ICON_SPACE.
      ENDIF.
*     end XNSAL0K005520

      MODIFY ACT_TAB FROM ACT_LN INDEX TABIX
         TRANSPORTING LONGTEXT_TAB
                      LONGTEXT_ICON.                      "XNSAL0K005520

    ENDIF.

    LONGTEXT_TAB = LT_TEXT.
  ENDMETHOD.                    "lcl_longtext

* begin XNSAL0K005520
  METHOD CHANGE_LONGTEXT_ID.

    IF CATSXT_LTXT-ID IS INITIAL.
      CATSXT_LTXT-ID = 'EXTT'.
    ENDIF.

    CHECK CATSXT_LTXT-ID <> OLD_LONGTEXT_ID.

    CALL METHOD GET_TEXT
      EXPORTING
        IM_LONGTEXT_ID = OLD_LONGTEXT_ID.

    CALL METHOD SET_TEXT.

    OLD_LONGTEXT_ID = CATSXT_LTXT-ID.

  ENDMETHOD.                    "lcl_longtext
* end XNSAL0K005520

* begin XNSAL0K010641
  METHOD SPLIT_LONGTEXT.

    DATA:          LF_COUNT          TYPE SYTABIX,
                   LF_TABIX1         TYPE SYTABIX,
                   LF_TABIX2         TYPE SYTABIX.

    FIELD-SYMBOLS: <LFS_TEXT>        TYPE TXLINE.

    LOOP AT IM_CATSXT_LONGTEXT ASSIGNING <LFS_TEXT>
            WHERE TABLE_LINE CS CO_TEXT_SEPARATOR.
      LF_TABIX1 = SY-TABIX - 1.
      LF_TABIX2 = SY-TABIX + 1.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC IS INITIAL.
      IF LF_TABIX1 > 0.
        APPEND LINES OF IM_CATSXT_LONGTEXT TO LF_TABIX1
                     TO EX_LONGTEXT_EXTERNAL.
      ENDIF.
      DESCRIBE TABLE IM_CATSXT_LONGTEXT LINES LF_COUNT.
      IF LF_COUNT >= LF_TABIX2.
        APPEND LINES OF IM_CATSXT_LONGTEXT FROM LF_TABIX2
                     TO EX_LONGTEXT_INTERNAL.
      ENDIF.
    ELSEIF NOT IM_CATSXT_LONGTEXT IS INITIAL.
      EX_LONGTEXT_EXTERNAL = IM_CATSXT_LONGTEXT.
    ENDIF.

  ENDMETHOD.                    "lcl_longtext
* end XNSAL0K010641

ENDCLASS.                    "lcl_longtext IMPLEMENTATION

* Begin YEKAL0K026011
CLASS LCL_SIMPLE_TEXT_EDITOR IMPLEMENTATION.
  METHOD CONSTRUCTOR.

    TITLE        = IM_TITLE.
    LONGTEXT_TAB = IM_LONGTEXT_TAB.
    DISPLAY_MODE = IM_DISPLAY_MODE.

  ENDMETHOD.                    "constructor

  METHOD FREE.
    CALL METHOD: EDITOR->FREE,
                 CONTAINER->FREE.
  ENDMETHOD.                    "free

  METHOD GET_TEXT.
    DATA: LF_COUNT TYPE SYTABIX.

    CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

    CALL METHOD CL_GUI_CFW=>FLUSH.

    LF_COUNT = LINES( LONGTEXT_TAB ).

    DO.
      DELETE LONGTEXT_TAB FROM  LF_COUNT
                     WHERE TABLE_LINE IS INITIAL.
      IF SY-SUBRC IS INITIAL AND LF_COUNT > 1.
        SUBTRACT 1 FROM LF_COUNT.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    RE_LONGTEXT = LONGTEXT_TAB.

  ENDMETHOD.                    "get_text

  METHOD START.
    CREATE OBJECT CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'LONGTEXT'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CHECK SY-SUBRC IS INITIAL.

    CREATE OBJECT EDITOR
      EXPORTING
        PARENT                 =  CONTAINER
        WORDWRAP_MODE          = '2'
        WORDWRAP_POSITION      = '72'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

    IF NOT DISPLAY_MODE IS INITIAL.
*     Set control to display only
      CALL METHOD EDITOR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR->TRUE.
    ENDIF.

    CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
      EXPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

  ENDMETHOD.                    "start

ENDCLASS.                    "lcl_simple_text_editor IMPLEMENTATION
