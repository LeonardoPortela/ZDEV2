*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0285_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0285_exit.

FORM f_exit_zsdt0285_0001 CHANGING p_registro_manter TYPE any.

*  DATA: wl_zsdt0285 TYPE zsdt0285.
*
*  CLEAR: wl_zsdt0285.
*
*  MOVE-CORRESPONDING wl_zsdt0285 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0285_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

*  DATA: wl_zsdt0285 TYPE zsdt0285.
*
*  CLEAR: wl_zsdt0285,
*         p_error.

ENDFORM.

FORM f_exit_zsdt0285_0003 CHANGING p_registro_manter TYPE any.

*  DATA: wl_zsdt0285 TYPE zsdt0285.
*
*  CLEAR: wl_zsdt0285.
*
*  MOVE-CORRESPONDING wl_zsdt0285 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0285_0004 CHANGING p_saida TYPE any.

*  DATA: wl_zsdt0285_out TYPE zsdt0285_out.
*
*  CLEAR: wl_zsdt0285_out.
*
*  MOVE-CORRESPONDING wl_zsdt0285_out TO p_saida.

ENDFORM.

FORM f_exit_zsdt0285_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_ref_tabname = 'ZSDT0285_OUT'.
    CASE p_field.
      WHEN 'GRUPO_TRIB'.
        p_scrtext_l    = 'Grupo Tributação'.
        p_outputlen    = 30.
      WHEN'CAMPO'.
        p_scrtext_l    = 'Nome Campo'.
        p_outputlen    = 20.
      WHEN'CODIGO'.
        p_scrtext_l    = 'Código'.
        p_outputlen    = 10.
      WHEN 'DESCRICAO'.
        p_scrtext_l    = 'Descrição'.
        p_outputlen    = 120.
    ENDCASE.
  ENDIF.

ENDFORM.
