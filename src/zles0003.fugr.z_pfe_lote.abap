FUNCTION z_pfe_lote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_NM_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(P_EDITAR) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     VALUE(P_LOTE_RET) TYPE  ZPFE_LOTE
*"     VALUE(P_LOTE_ALV_RET) TYPE  ZPFE_LOTE_ALV
*"----------------------------------------------------------------------

  DATA: pnrlote      TYPE lxhme_range_c10_t WITH HEADER LINE,
        it_lotes     TYPE TABLE OF zpfe_lote,
        it_lotes_alv TYPE TABLE OF zpfe_lote_alv.

  CLEAR: vg_alterado, vg_editar.

  pnrlote-sign    = 'I'.
  pnrlote-option  = 'EQ'.
  pnrlote-low     = p_nm_lote.
  pnrlote-high    = p_nm_lote.
  APPEND pnrlote.

  CALL FUNCTION 'Z_PFE_PSQ_LOTE'
    EXPORTING
      pnrlote      = pnrlote[]
    TABLES
      it_lotes     = it_lotes
      it_lotes_alv = it_lotes_alv.

  READ TABLE it_lotes INTO zpfe_lote INDEX 1.
  READ TABLE it_lotes_alv INTO p_lote_alv_ret INDEX 1.

  MOVE-CORRESPONDING p_lote_alv_ret TO wa_lote_alv.

  IF ( p_editar IS NOT INITIAL )
    AND ( zpfe_lote-belnr IS INITIAL )
    AND ( ( zpfe_lote-status EQ c_i ) OR ( zpfe_lote-status EQ c_e ) ).
    vg_editar = p_editar.
  ENDIF.

  CALL SCREEN 0001 STARTING AT 10 05 ENDING AT 85 10.

  IF NOT vg_alterado IS INITIAL.
    MODIFY zpfe_lote.
  ENDIF.

  CALL FUNCTION 'Z_PFE_PSQ_LOTE'
    EXPORTING
      pnrlote      = pnrlote[]
    TABLES
      it_lotes     = it_lotes
      it_lotes_alv = it_lotes_alv.

  READ TABLE it_lotes INTO p_lote_ret INDEX 1.
  READ TABLE it_lotes_alv INTO p_lote_alv_ret INDEX 1.


ENDFUNCTION.
