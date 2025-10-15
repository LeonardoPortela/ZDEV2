*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIWRT2003_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfiwrt2003_exit.

FORM f_exit_zfiwrt2003_0001  CHANGING p_registro_manter TYPE any.

  DATA: wl_zfiwrt2003 TYPE zfiwrt2003.

  CLEAR: wl_zfiwrt2003.
  wl_zfiwrt2003-usuario = sy-uname.
  wl_zfiwrt2003-data    = sy-datum.
  wl_zfiwrt2003-hora    = sy-uzeit.

  MOVE-CORRESPONDING wl_zfiwrt2003 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfiwrt2003_0002     USING p_registro_manter TYPE any
                             CHANGING p_error.

  DATA: wl_zfiwrt2003 TYPE zfiwrt2003.

  CLEAR p_error.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt2003.

  SELECT SINGLE werks
    INTO @DATA(l_werks)
    FROM t001w
   WHERE werks = @wl_zfiwrt2003-cen_emissor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-cen_emissor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro Emissor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE werks
    INTO @l_werks
    FROM t001w
   WHERE werks = @wl_zfiwrt2003-cen_receptor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-cen_receptor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro Receptor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE operacao
    INTO @DATA(l_operacao)
    FROM zfiwrt0001
   WHERE operacao = @wl_zfiwrt2003-ope_saida_credor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-ope_saida_credor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cod.Operação Saída Credor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr
    INTO @DATA(l_matnr)
    FROM mara
   WHERE matnr = @wl_zfiwrt2003-mat_saida_credor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-mat_saida_credor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cod.Material Saída Credor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE operacao
    INTO @l_operacao
    FROM zfiwrt0001
   WHERE operacao = @wl_zfiwrt2003-ope_saida_devedor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-ope_saida_devedor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cod.Operação Saída Devedor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr
    INTO @l_matnr
    FROM mara
   WHERE matnr = @wl_zfiwrt2003-mat_saida_devedor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-mat_saida_devedor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cod.Material Saída Devedor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE operacao
    INTO @l_operacao
    FROM zfiwrt0001
   WHERE operacao = @wl_zfiwrt2003-ope_entrada_credor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-ope_entrada_credor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cod.Operação Entrada Credor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr
    INTO @l_matnr
    FROM mara
   WHERE matnr = @wl_zfiwrt2003-mat_entrada_credor.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Cod.Material Entrada Credor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE operacao
    INTO @l_operacao
    FROM zfiwrt0001
   WHERE operacao = @wl_zfiwrt2003-ope_entrada_devedor.

  IF sy-subrc <> 0 OR wl_zfiwrt2003-ope_entrada_devedor IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cod.Operação Entrada Devedor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr
    INTO @l_matnr
    FROM mara
   WHERE matnr = @wl_zfiwrt2003-mat_entrada_devedor.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Cod.Material Entrada Devedor está Incorreto.' TYPE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zfiwrt2003_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZFIWRT2003_OUT'.
    CASE p_field.
      WHEN 'CEN_EMISSOR'.
        p_scrtext_l    = 'Centro Emissor'.
        p_outputlen    = 15.

      WHEN 'CEN_RECEPTOR'.
        p_scrtext_l    = 'Centro Receptor'.
        p_outputlen    = 15.

      WHEN 'OPE_SAIDA_CREDOR'.
        p_scrtext_l    = 'Ope.Saída Credor'.
        p_outputlen    = 18.

      WHEN 'MAT_SAIDA_CREDOR'.
        p_scrtext_l    = 'Material Saída Credor'.
        p_outputlen    = 22.

      WHEN 'OPE_SAIDA_DEVEDOR'.
        p_scrtext_l    = 'Ope.Saída Devedor'.
        p_outputlen    = 18.

      WHEN 'MAT_SAIDA_DEVEDOR'.
        p_scrtext_l    = 'Material Saída Devedor'.
        p_outputlen    = 22.

      WHEN 'OPE_ENTRADA_CREDOR'.
        p_scrtext_l    = 'Ope.Entrada Credor'.
        p_outputlen    = 18.

      WHEN 'MAT_ENTRADA_CREDOR'.
        p_scrtext_l    = 'Material Entrada Credor'.
        p_outputlen    = 22.

      WHEN 'OPE_ENTRADA_DEVEDOR'.
        p_scrtext_l    = 'Ope.Entrada Devedor'.
        p_outputlen    = 18.

      WHEN 'MAT_ENTRADA_DEVEDOR'.
        p_scrtext_l    = 'Material Entrada Devedor'.
        p_outputlen    = 22.

    ENDCASE.
  ENDIF.

ENDFORM.
