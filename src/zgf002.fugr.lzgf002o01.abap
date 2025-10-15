*----------------------------------------------------------------------*
***INCLUDE LZGF002O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'Z001'.
*  SET TITLEBAR 'xxx'.

  IF OBG_EDITCONTAINER IS INITIAL.
    CREATE OBJECT OBG_EDITCONTAINER
      EXPORTING
        CONTAINER_NAME = 'SCR'.
*        REPID          = SY-REPID
*        DYNNR          = '0200'.


    CREATE OBJECT OBG_EDITOR
      EXPORTING
        PARENT            = OBG_EDITCONTAINER
        WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION = 256
        MAX_NUMBER_CHARS  = 100000.

    CALL METHOD OBG_EDITOR->SET_TEXT_AS_R3TABLE
      EXPORTING
        TABLE = TG_CODE_SHOW[].

    DATA: WL_CONT TYPE SY-TABIX.
    DESCRIBE TABLE TG_CODE_SHOW LINES WL_CONT.

    OBG_EDITOR->SET_READONLY_MODE( 1 ).
    OBG_EDITOR->SET_AUTOINDENT_MODE( 1 ).
    OBG_EDITOR->INDENT_LINES( FROM_LINE = 1 ).
  ENDIF.
ENDMODULE.                 " STATUS_0200  OUTPUT
