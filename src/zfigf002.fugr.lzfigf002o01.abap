*----------------------------------------------------------------------*
***INCLUDE LZFIGF002O01 .
*----------------------------------------------------------------------*
* 4.7
* XNSAL0K098521 02/01/2002 - suppress longtext id in display mode if
*                            either int. or ext. longtext is initial
*----------------------------------------------------------------------*
*       Module  D0100_INIT  OUTPUT
*----------------------------------------------------------------------*
*       Init user default value screen
*----------------------------------------------------------------------*
MODULE D0100_INIT OUTPUT.

  CLEAR: OK_CODE.

  SET: PF-STATUS 'D0100'.

  IF D0100_VIEW_MODE IS INITIAL.
    SET TITLEBAR  'D0100' WITH ENAME.
  ELSE.
    SET TITLEBAR  'D0100_DISP' WITH ENAME.
  ENDIF.

  IF RB_YEAR IS INITIAL AND CATSXT_USER_DEFAULT-RELDATE = CO_99.
    CATSXT_USER_DEFAULT-RELDATE = CO_03.
  ENDIF.

* Begin YEKP9CK117331
  IF CATSXT_USER_DEFAULT-EXIT_ON_SAVE = CO_YES.
    CB_EXIT_ON_SAVE = CO_X.
  ELSE.
    CLEAR: CB_EXIT_ON_SAVE.
  ENDIF.

  CLEAR RB_PROP.

  CASE CATSXT_USER_DEFAULT-PROP_METH.
    WHEN CL_TIME_SHEET_CATSXT=>CO_PROP_METH_1.
      RB_PROP-METHOD1 = CO_X.
    WHEN CL_TIME_SHEET_CATSXT=>CO_PROP_METH_2.
      RB_PROP-METHOD2 = CO_X.
    WHEN CL_TIME_SHEET_CATSXT=>CO_PROP_METH_3.
      RB_PROP-METHOD3 = CO_X.
    WHEN CL_TIME_SHEET_CATSXT=>CO_PROP_METH_4.
      RB_PROP-METHOD4 = CO_X.
  ENDCASE.
* End YEKP9CK117331

* begin XNSP9CK153934
  IF CATSXT_USER_DEFAULT-FREEATSAVE_DFLT = CO_YES.
    CATSXT_USER_DEFAULT-FREEATSAVE_DFLT = CO_X.
  ELSE.
    CLEAR CATSXT_USER_DEFAULT-FREEATSAVE_DFLT.
  ENDIF.
* end XNSP9CK153934

ENDMODULE.                             " D0100_INIT  OUTPUT

*----------------------------------------------------------------------*
*       Module  d0100_modif  OUTPUT
*----------------------------------------------------------------------*
*       Set user default screen fields to output only
*----------------------------------------------------------------------*
MODULE D0100_MODIF OUTPUT.

  IF D0100_VIEW_MODE = CO_X.
    LOOP AT SCREEN.
      CHECK SCREEN-GROUP1 <> 'CDT'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDLOOP.
    EXIT.
  ENDIF.

  IF GF_PERNR IS INITIAL.
    LOOP AT SCREEN.
      CHECK SCREEN-GROUP1 = 'CDT'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  CHECK NOT RB_YEAR IS INITIAL.

* Disable input in months field if year is selected
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'MON'.
    SCREEN-ACTIVE = '0'.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                             " d0100_modif  OUTPUT
*----------------------------------------------------------------------*
*       Module  d0200_init  OUTPUT
*----------------------------------------------------------------------*
*       Init longtext display screen
*----------------------------------------------------------------------*
MODULE D0200_INIT OUTPUT.
* Module completely revised with YEKP9CK117331 (see version management)
  CLEAR: OK_CODE.

  REFRESH FCODE_TAB.

  IF LONGTEXT->CONTAINER IS INITIAL.
    CALL METHOD LONGTEXT->START.
  ENDIF.

  IF ( LONGTEXT->TABIX  = 1 ) OR
     ( LONGTEXT->RCOUNT = 1 ) OR
     ( LONGTEXT->TABIX  = LONGTEXT->FIRST_REC ).
    APPEND CO_OK-PREV_COMP TO FCODE_TAB.
  ENDIF.

  IF ( LONGTEXT->TABIX  = LONGTEXT->RCOUNT ) OR
     ( LONGTEXT->RCOUNT = 1 )                OR
     ( LONGTEXT->TABIX  = LONGTEXT->LAST_REC ).
    APPEND CO_OK-NEXT_COMP TO FCODE_TAB.
  ENDIF.

  IF NOT LONGTEXT->DISPLAY_MODE IS INITIAL.
    APPEND 'CX_NEW' TO FCODE_TAB.
  ENDIF.

  SET: PF-STATUS 'D0200' EXCLUDING FCODE_TAB,
       TITLEBAR  'D0200' WITH LONGTEXT->WORKDATE
                              LONGTEXT->ACT_LN-TASKTYPE_TEXT
                              LONGTEXT->ACT_LN-TASKLEVEL_TEXT.

ENDMODULE.                             " d0200_init  OUTPUT

* begin XNSAL0K005520
*----------------------------------------------------------------------*
*       Module  d0200_modif  OUTPUT
*----------------------------------------------------------------------*
*       Modify screen 0200
*----------------------------------------------------------------------*
MODULE D0200_MODIF OUTPUT.

  IF LONGTEXT->LTXT_SPLIT IS INITIAL OR          "........XNSAL0K098521
     ( LONGTEXT->DISPLAY_MODE IS NOT INITIAL AND "........XNSAL0K098521
       ( EXT_LONGTEXT_TAB IS INITIAL OR          "........XNSAL0K098521
         INT_LONGTEXT_TAB IS INITIAL ) ).        "........XNSAL0K098521
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'TID'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT     = '0'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " d0200_modif  OUTPUT
* end XNSAL0K005520
*----------------------------------------------------------------------*
*       Module  d0400_init  OUTPUT - new with YEKAL0K026011
*----------------------------------------------------------------------*
*       Init simple text editor screen
*----------------------------------------------------------------------*
MODULE D0400_INIT OUTPUT.

  CLEAR: OK_CODE.

  IF STE->CONTAINER IS INITIAL.
    CALL METHOD STE->START.
  ENDIF.

  SET: PF-STATUS 'D0400',
       TITLEBAR  'D0400' WITH STE->TITLE.

ENDMODULE.                             " d0400_init  OUTPUT
*----------------------------------------------------------------------*
*       Module  d0300_init  OUTPUT
*----------------------------------------------------------------------*
*       Initialize Smart Form Dynpro
*----------------------------------------------------------------------*
MODULE D0300_INIT OUTPUT.
  SET: PF-STATUS 'D0300',
       TITLEBAR  'D0300'.
ENDMODULE.                 " d0300_init  OUTPUT
