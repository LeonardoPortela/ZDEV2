"Name: \PR:SAPLCOKO1\FO:_I_CHECK_ORDER_TYPE\SE:END\EI
ENHANCEMENT 0 ZENH_TIPO_ORDEM_MAT.
*186237 BUG solto - Implementar bloqueio tipo de ordem PP x Material - RGA - 31.07.2025

  DATA: lv_matnr TYPE marc-matnr,
        lv_sfcpf TYPE marc-sfcpf,
        lv_auart TYPE v_tco43-auart.


  DATA: ls_marc   TYPE marc,
        ls_tco43  TYPE tco43,
        ls_aufpar TYPE aufpar.


  lv_matnr = caufvd-matnr.

  IF lv_matnr IS INITIAL.
    EXIT.
  ENDIF.


  SELECT SINGLE sfcpf
    FROM marc
    INTO lv_sfcpf
    WHERE matnr = lv_matnr.

  IF sy-subrc <> 0 OR lv_sfcpf IS INITIAL.

    EXIT.
  ENDIF.


  SELECT SINGLE auart
    FROM tco43
    INTO lv_auart
    WHERE co_prodprf = lv_sfcpf.

  IF sy-subrc <> 0 OR lv_auart IS INITIAL.
    MESSAGE 'Tipo de ordem informada diverge do material selecionado' TYPE 'E'.
  ENDIF.

  IF lv_auart <> aufpar-pp_aufart.
    MESSAGE 'Tipo de ordem informada diverge do material selecionado' TYPE 'E'.
  ENDIF.


ENDENHANCEMENT.
