*&---------------------------------------------------------------------*
*& Report ZSDI_AJUSTA_REMESSA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdi_ajusta_remessa.

TABLES: likp.

DATA: t_likp  TYPE TABLE OF likp.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_remes FOR likp-vbeln.
SELECTION-SCREEN END   OF BLOCK a1.

START-OF-SELECTION.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

  SELECT *
    FROM likp
    INTO TABLE t_likp
   WHERE vbeln IN p_remes.

  LOOP AT t_likp INTO DATA(w_likp).

    SELECT SINGLE *
      FROM zsdt0023_temp
      INTO @DATA(_0023)
     WHERE vbeln = @w_likp-vbeln.

    IF sy-subrc = 0.
      IF  _0023-mblnr_s IS NOT INITIAL OR _0023-mblnr_e IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    SUBMIT zsdi_ajusta_movto WITH p_remes = w_likp-vbeln
                              AND RETURN.
  ENDLOOP.

  MESSAGE s024(sd) WITH 'Execução Finalidaza!'.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
