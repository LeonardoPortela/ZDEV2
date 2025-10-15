*----------------------------------------------------------------------*
***INCLUDE ZLESR0165_STATUS_0100O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: t_ucomm, ok_code, zlest0025-deschvid.

  IF lv_tela = 'N'.
    APPEND '&NOVO'     TO t_ucomm.
  ELSE.
    APPEND '&CONSULTA' TO t_ucomm.
    APPEND 'SALVAR'    TO t_ucomm.
  ENDIF.

  SET PF-STATUS 'ZLESR0165' EXCLUDING t_ucomm.
  SET TITLEBAR 'ZLESR0165'.

  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      IF lv_tela = 'C'.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  SELECT SINGLE    deschvid
    INTO zlest0025-deschvid
    FROM zlest0025
   WHERE chvid = zlest0025-chvid.

  PERFORM f_init_alv.

ENDMODULE.
