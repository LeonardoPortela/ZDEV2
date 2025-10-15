FUNCTION zles_cadastro_avseg.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_FILTRO) TYPE  ZDE_ZLEST0143_FILTRO
*"----------------------------------------------------------------------

  DATA: objeto                TYPE REF TO zcl_averbacao_seguro,
        it_zlest0143_r        TYPE zde_zlest0143_alv_t,
        it_zlest0143_s        TYPE zde_zlest0143_alv_sint_t.
*        it_zlest0143_alv_sint TYPE zde_zlest0143_alv_sint_t,
*        ws_zlest0143_alv_sint TYPE zde_zlest0143_alv_sint.

  CREATE OBJECT objeto.

  CLEAR: vg_tp_rel.
  READ TABLE i_filtro-tp_relatorio INTO DATA(ws_tp_rel) INDEX 1.
  IF sy-subrc EQ 0.
    vg_tp_rel = ws_tp_rel-low.
  ENDIF.

  objeto->zif_pesquisa~pesquisar( EXPORTING i_filtros = i_filtro IMPORTING e_registros = it_zlest0143_r  e_registros_s = it_zlest0143_s ).

  IF vg_tp_rel EQ 'A'.
    LOOP AT it_zlest0143_r INTO DATA(wa_zlest0143_r).
      APPEND wa_zlest0143_r TO it_zlest0143.
    ENDLOOP.
  ELSE.

    MOVE-CORRESPONDING it_zlest0143_s TO it_zlest0143_alv_sint.
  ENDIF.
  lc_filtro_43 = i_filtro.

  CALL SCREEN 0001.

  CLEAR: objeto.

ENDFUNCTION.
