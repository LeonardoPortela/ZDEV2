*----------------------------------------------------------------------*
***INCLUDE LZFIGF001I01 .
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Module  D0100_EXIT  INPUT
*----------------------------------------------------------------------*
*       Leave user default value screen
*----------------------------------------------------------------------*
MODULE D0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                             " D0100_EXIT  INPUT

*----------------------------------------------------------------------*
*       Module  D0200_EXIT  INPUT
*----------------------------------------------------------------------*
*       Leave longtext display screen
*-------------------------------------------------------------------*
MODULE D0200_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                             " D0200_EXIT  INPUT
*----------------------------------------------------------------------*
*       Module  D0300_EXIT  INPUT
*----------------------------------------------------------------------*
*       Leave smart form selection screen
*-------------------------------------------------------------------*
MODULE D0300_EXIT INPUT.
**  CLEAR: CATSXT_SF-FORMNAME.
**  LEAVE TO SCREEN 0.
ENDMODULE.                             " D0300_EXIT  INPUT
*----------------------------------------------------------------------*
*       Module  D0300_CHECK_FORMNAME  INPUT
*----------------------------------------------------------------------*
*       Check smart form name
*-------------------------------------------------------------------*
MODULE D0300_CHECK_FORM_NAME INPUT.
*  PERFORM CHECK_SMARTFORM_NAME.
ENDMODULE.                             " D0300_CHECK_FORM_NAME  INPUT
*----------------------------------------------------------------------*
*       Module  D0300_CHECK_MAIL_RECEIVER  INPUT
*----------------------------------------------------------------------*
*       Check email recipient for smart form
*-------------------------------------------------------------------*
MODULE D0300_CHECK_MAIL_RECEIVER INPUT.
*  PERFORM CHECK_SMARTFORM_MAIL_RECEIVER.
ENDMODULE.                             " D0300_CHECK_MAIL_RECEIER  INPUT
*----------------------------------------------------------------------*
*       Module  D0300_F4_EMAIL  INPUT
*----------------------------------------------------------------------*
*       F4 help for email recipient for smart form
*-------------------------------------------------------------------*
MODULE D0300_F4_EMAIL INPUT.
*  PERFORM F4_EMAIL_RECIPIENT.
ENDMODULE.                             " D0300_F4_EMAIL  INPUT

*----------------------------------------------------------------------*
*       Module  D0100_OK_CODE  INPUT
*----------------------------------------------------------------------*
*       Function code user default screen
*----------------------------------------------------------------------*
MODULE D0100_OK_CODE INPUT.
*  CASE OK_CODE.
*    WHEN CO_OK-SAVE.
*      LEAVE TO SCREEN 0.
*    WHEN 'CX_CUSTDTL'.
*      PERFORM SHOW_CUST_DFLT_VALUES.
*    WHEN 'CX_INFO'.
*      PERFORM SHOW_CUSTOMIZING_DOCU.
*  ENDCASE.
ENDMODULE.                             " D0100_OK_CODE  INPUT
*----------------------------------------------------------------------*
*       Module  d0200_ok_code  INPUT
*----------------------------------------------------------------------*
*       Function codes for longtext screen - YEKP9CK117331
*----------------------------------------------------------------------
MODULE D0200_OK_CODE INPUT.

  CASE OK_CODE.
    WHEN 'CX_CONT'.
      CALL METHOD LONGTEXT->GET_TEXT                      "XNSAL0K005520
        EXPORTING                                         "XNSAL0K005520
          IM_LONGTEXT_ID = CATSXT_LTXT-ID.                "XNSAL0K005520
      LEAVE TO SCREEN 0.
    WHEN 'CX_NEW'.
      CALL METHOD LONGTEXT->INTERNAL_MEMO.
    WHEN 'CX_LONGTEXT_ID'.                                "XNSAL0K005520
      CALL METHOD LONGTEXT->CHANGE_LONGTEXT_ID.
    WHEN CO_OK-NEXT_COMP.
      CALL METHOD LONGTEXT->NEXT.
    WHEN CO_OK-PREV_COMP.
      CALL METHOD LONGTEXT->PREVIOUS.
  ENDCASE.

ENDMODULE.                 " d0200_ok_code  INPUT
*----------------------------------------------------------------------*
*       Module  d0100_exit_on_save  INPUT
*----------------------------------------------------------------------*
*       Set exit_on_save indicator - new with YEKP9CK117331
*----------------------------------------------------------------------*
MODULE D0100_EXIT_ON_SAVE INPUT.

  IF CB_EXIT_ON_SAVE IS INITIAL.
    CATSXT_USER_DEFAULT-EXIT_ON_SAVE = CO_NO.
  ELSE.
    CATSXT_USER_DEFAULT-EXIT_ON_SAVE = CO_YES.
  ENDIF.

ENDMODULE.                 " d0100_exit_on_save  INPUT
*----------------------------------------------------------------------*
*       Module  d0100_proposal_method  INPUT
*----------------------------------------------------------------------*
*       Set value for proposal method  - new with YEKP9CK117331
*----------------------------------------------------------------------*
MODULE D0100_PROPOSAL_METHOD INPUT.
  DATA: LR_TS TYPE REF TO CL_TIME_SHEET_CATSXT.

  IF NOT RB_PROP-METHOD1 IS INITIAL.
    CATSXT_USER_DEFAULT-PROP_METH = CL_TIME_SHEET_CATSXT=>CO_PROP_METH_1.
  ELSEIF NOT RB_PROP-METHOD2 IS INITIAL.
    CATSXT_USER_DEFAULT-PROP_METH = CL_TIME_SHEET_CATSXT=>CO_PROP_METH_2.
  ELSEIF NOT RB_PROP-METHOD3 IS INITIAL.
    CATSXT_USER_DEFAULT-PROP_METH = CL_TIME_SHEET_CATSXT=>CO_PROP_METH_3.
  ELSEIF NOT RB_PROP-METHOD4 IS INITIAL.
    CATSXT_USER_DEFAULT-PROP_METH = CL_TIME_SHEET_CATSXT=>CO_PROP_METH_4.
  ENDIF.

ENDMODULE.                 " d0100_proposal_method  INPUT
*----------------------------------------------------------------------*
*       Module  d0100_freeatsave_dflt  INPUT
*----------------------------------------------------------------------*
*       Set freeatsave_dflt - new with XNSP9CK153934
*----------------------------------------------------------------------*
MODULE D0100_FREEATSAVE_DFLT INPUT.

  IF CATSXT_USER_DEFAULT-FREEATSAVE_DFLT = CO_X.
    CATSXT_USER_DEFAULT-FREEATSAVE_DFLT = CO_YES.
  ELSE.
    CATSXT_USER_DEFAULT-FREEATSAVE_DFLT = CO_NO.
  ENDIF.

ENDMODULE.                 " d0100_freeatsave_dflt  INPUT
*----------------------------------------------------------------------*
*       Module  d0400_ok_code  INPUT
*----------------------------------------------------------------------*
*       Function codes for simple text editor screen - YEKAL0K026011
*----------------------------------------------------------------------
MODULE D0400_OK_CODE INPUT.

  CASE OK_CODE.
    WHEN 'CX_REPROC'.
      WG_REPROC = 'X'.
       LEAVE TO SCREEN 0.
    WHEN 'CX_CONT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " d0400_ok_code  INPUT
*----------------------------------------------------------------------*
*       Module  d0300_ok_code  INPUT
*----------------------------------------------------------------------*
*       Function codes for smart form selection
*----------------------------------------------------------------------
MODULE D0300_OK_CODE INPUT.

  CASE OK_CODE.
    WHEN 'CX_CONT'.
      LEAVE TO SCREEN 0.
    WHEN 'CX_SF_INFO'.
      CALL FUNCTION 'POPUP_DISPLAY_TEXT'
        EXPORTING
          POPUP_TITLE    = 'Smart Forms in CATS'(008)
          TEXT_OBJECT    = 'CATSXT_SMARTFORMS'
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
  ENDCASE.

ENDMODULE.                 " d0300_ok_code  INPUT
