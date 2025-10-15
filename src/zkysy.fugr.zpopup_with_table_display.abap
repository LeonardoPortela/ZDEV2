FUNCTION zpopup_with_table_display.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(ENDPOS_COL) TYPE  INT4
*"     VALUE(ENDPOS_ROW) TYPE  INT4
*"     VALUE(STARTPOS_COL) TYPE  INT4
*"     VALUE(STARTPOS_ROW) TYPE  INT4
*"     VALUE(TITLETEXT) TYPE  CHAR80
*"     REFERENCE(TAMANHO_AREA) TYPE  INT4 DEFAULT 200
*"  EXPORTING
*"     VALUE(CHOISE) LIKE  SY-TABIX
*"  TABLES
*"      VALUETAB
*"  EXCEPTIONS
*"      BREAK_OFF
*"----------------------------------------------------------------------
  REFRESH wa_listtab. CLEAR wa_listtab.

  FIELD-SYMBOLS: <l_valuetab> TYPE x,
                 <l_listtab> TYPE x.
  ASSIGN: wa_listtab TO <l_listtab> TYPE 'X',
          valuetab TO <l_valuetab> TYPE 'X'.

  LOOP AT valuetab.
    <l_listtab> = <l_valuetab>.
    APPEND wa_listtab.
  ENDLOOP.

  title_text = titletext.

  p_tamanho_area = tamanho_area.

  CALL SCREEN 0100 STARTING AT startpos_col startpos_row
                   ENDING   AT endpos_col endpos_row.
  choise = hline.
ENDFUNCTION.


AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'SLCT'.
      read line sy-tabix.
      GET CURSOR FIELD hfield LINE hline.
      IF sy-subrc = 0.
        SET SCREEN 0.
        LEAVE SCREEN.
      ENDIF.
    WHEN 'ABR'.
      SET SCREEN 0.
      RAISE break_off.
      LEAVE SCREEN.
  ENDCASE.

*---------------------------------------------------------------------*
*       MODULE SET_TITLE                                              *
*---------------------------------------------------------------------*
*       Set the title given by calling program                        *
*---------------------------------------------------------------------*

MODULE set_title OUTPUT.

  SET TITLEBAR '001' WITH title_text.

ENDMODULE.                    "set_title OUTPUT

*---------------------------------------------------------------------*
*       MODULE LISTPROCESSING OUTPUT                                  *
*---------------------------------------------------------------------*
*       Switchs to list-processing and displays internal table data   *
*---------------------------------------------------------------------*
MODULE listprocessing OUTPUT.

  "NEW-PAGE LINE-COUNT 10 LINE-SIZE p_tamanho_area.

  SET PF-STATUS 'PICK'.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 100.
  LOOP AT wa_listtab.
    WRITE: / wa_listtab(p_tamanho_area).
    hide: wa_listtab(p_tamanho_area).
  ENDLOOP.
  LEAVE SCREEN.

ENDMODULE.                    "listprocessing OUTPUT
