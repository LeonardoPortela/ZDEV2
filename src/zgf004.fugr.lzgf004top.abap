FUNCTION-POOL ZGF004.                       "MESSAGE-ID ..
*---------------------------------------------------------------------*
*       CLASS lcl_longtext DEFINITION
*---------------------------------------------------------------------*
*       longtext display/change handling - new with YEKP9CK117331
*---------------------------------------------------------------------*
*CLASS LCL_LONGTEXT DEFINITION.
*  PUBLIC SECTION.
*    CONSTANTS:   CO_X  TYPE XFELD VALUE 'X'.
** begin XNSAL0K005520
**                 co_text_separator type txline
**                         value '*** Internal Memo ***'.
*
*    TYPES: BEGIN OF TY_MEMO_TAB,
*             TMP_KEY TYPE CATSCOUNTE,
*           END OF TY_MEMO_TAB.
** end XNSAL0K005520
*
*    DATA: CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER
*                                                    READ-ONLY,
*          DISPLAY_MODE TYPE XFELD                   READ-ONLY,
*          EDITOR       TYPE REF TO CL_GUI_TEXTEDIT  READ-ONLY,
*          FIRST_REC    TYPE I                       READ-ONLY,
*          LAST_REC     TYPE I                       READ-ONLY,
*          LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB    READ-ONLY,
**         Begin YEKAL0K010641
*          OLD_PERNR    TYPE PERNR_D                 READ-ONLY,
**         End YEKAL0K010641
*          TABIX        TYPE SYTABIX                 READ-ONLY,
*          RCOUNT       TYPE I                       READ-ONLY,
*          ACT_TAB      TYPE CATSXT_WA_ITAB          READ-ONLY,
*          ACT_LN       LIKE LINE OF ACT_TAB         READ-ONLY,
*          WORKDATE(10) TYPE C                       READ-ONLY,
** begin XNSAL0K005520
*          LTXT_SPLIT   TYPE XFELD VALUE ' ',
*          MEMO_TAB     TYPE HASHED TABLE OF TY_MEMO_TAB READ-ONLY
*                            WITH UNIQUE KEY TMP_KEY .
** end XNSAL0K005520
*
*    METHODS: CONSTRUCTOR
*                IMPORTING
*                  IM_DISPLAY_MODE  TYPE XFELD
*                  IM_TABIX         TYPE I
*                  IM_RECORD_COUNT  TYPE I
*                  IM_ACTIVITY_DATA TYPE CATSXT_WA_ITAB,
*
*             FREE,
*
*             GET_TEXT
** begin XNSAL0K005520
*               IMPORTING IM_LONGTEXT_ID TYPE CATSXT_LONGTEXT_ID,
** end XNSAL0K005520
*
*             GET_ACTIVITIES
*               RETURNING VALUE(RE_ACTIVITY_DATA) TYPE CATSXT_WA_ITAB,
*
*             INTERNAL_MEMO,
*
*             NEXT,
*
*             CHANGE_LONGTEXT_ID,                          "XNSAL0K005520
*
*             PREVIOUS,
*
*             READ_ACT_TAB,
*
*             START,
*
*             SET_TEXT,
*
*             SET_TITLE.
*
**   begin XNSAL0K010641
*    CLASS-METHODS: SPLIT_LONGTEXT
*                     IMPORTING
*                       IM_CATSXT_LONGTEXT   TYPE CATSXT_LONGTEXT_ITAB
*                     EXPORTING
*                       EX_LONGTEXT_EXTERNAL TYPE CATSXT_LONGTEXT_ITAB
*                       EX_LONGTEXT_INTERNAL TYPE CATSXT_LONGTEXT_ITAB.
**   end XNSAL0K010641
*
*ENDCLASS.                    "lcl_longtext DEFINITION

* Begin YEKAL0K026011
CLASS LCL_SIMPLE_TEXT_EDITOR DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:   CO_X  TYPE XFELD VALUE 'X'.

    DATA: CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER
                                                    READ-ONLY,
          DISPLAY_MODE TYPE XFELD                   READ-ONLY,
          EDITOR       TYPE REF TO CL_GUI_TEXTEDIT  READ-ONLY,
          LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB    READ-ONLY,
          TITLE        TYPE SYTITLE                 READ-ONLY,
          proteger     TYPE i                       READ-ONLY.

    METHODS: CONSTRUCTOR
               IMPORTING
                 IM_TITLE        TYPE SYTITLE
                 IM_LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB
                 IM_DISPLAY_MODE TYPE XFELD
                 im_proteger     TYPE i OPTIONAL,

             FREE,

             GET_TEXT
               RETURNING
                 VALUE(RE_LONGTEXT) TYPE CATSXT_LONGTEXT_ITAB,

             START.

ENDCLASS.                    "lcl_simple_text_editor DEFINITION
* Types, tables and data definitions
TYPE-POOLS: CXTAB,
            ICON.                                          "VWLP9C117331

            TABLES:     CATSXT_USER_DFLT_SCR,
            CATSXT_USER_DEFAULT,
            CATSXT_LTXT,                                  "XNSAL0K005520
            CATSXT_SF.

            INCLUDE:    RCATSXT_CONSTANTS,
*           BOR object processing
           <CNTN01>.

DATA:       ENAME               TYPE EMNAM,
*           Begin YEKP9CK117331
*           longtext_container  type ref to cl_gui_custom_container,
*           longtext_editor     type ref to cl_gui_textedit,
*           longtext_tab        type catsxt_longtext_itab,
*            LONGTEXT            TYPE REF TO LCL_LONGTEXT,
            CB_EXIT_ON_SAVE     TYPE XFELD,
            FCODE_TAB           TYPE STANDARD TABLE OF SYTCODE,
*           End YEKP9CK117331
            OK_CODE             LIKE SY-TCODE,
*            GF_PERNR            TYPE PERNR_D,
*            RB_MONTHS           TYPE CHAR01,
*            RB_YEAR             TYPE CHAR01,
*            TMP_KEY             TYPE CATSCOUNTE,
*            D0100_VIEW_MODE     TYPE CHAR1,
*           begin XNSAL0K005520
            OLD_LONGTEXT_ID     TYPE CATSXT_LONGTEXT_ID,
            INT_LONGTEXT_TAB    TYPE CATSXT_LONGTEXT_ITAB,
            EXT_LONGTEXT_TAB    TYPE CATSXT_LONGTEXT_ITAB,
*           end XNSAL0K005520

*           Begin YEKAL0K026011
            STE                 TYPE REF TO LCL_SIMPLE_TEXT_EDITOR,
*           End YEKAL0K026011

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
