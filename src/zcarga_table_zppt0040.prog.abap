*&---------------------------------------------------------------------*
*& Report ZCARGA_TABLE_ZPPT0040
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcarga_table_zppt0040.

DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
      wl_planilha LIKE alsmex_tabline,
      vl_dt_temp  TYPE sydatum.

DATA: p_file  TYPE rlgrap-filename.

DATA: lit_zppt0040 TYPE TABLE OF zppt0040,
      lwa_zppt0040 TYPE zppt0040.


START-OF-SELECTION.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = |Importando Dados|.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 55
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF gt_planilha[] IS INITIAL.
    MESSAGE |Planilha vazia! Arquivo: { p_file } | TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: lwa_zppt0040.

      lwa_zppt0040-carga       = abap_true.
      lwa_zppt0040-dt_registro = sy-datum.
      lwa_zppt0040-hr_registro = sy-uzeit.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        lwa_zppt0040-werks = wl_planilha-value.
      WHEN 2.
        lwa_zppt0040-lgort = wl_planilha-value.
      WHEN 3.
        lwa_zppt0040-safra = wl_planilha-value.
      WHEN 4.
        lwa_zppt0040-qtd_fardinhos = wl_planilha-value.

      WHEN 5.
        lwa_zppt0040-capacidade_bloco = wl_planilha-value.
      WHEN 6.
        lwa_zppt0040-tipo_fardo = wl_planilha-value.
      WHEN 7.
        lwa_zppt0040-cd_classificacao = wl_planilha-value.
      WHEN 8.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        lwa_zppt0040-peso_liquido = wl_planilha-value.
    ENDCASE.

    AT END OF row.
      APPEND lwa_zppt0040 TO lit_zppt0040.
    ENDAT.

  ENDLOOP.

  IF lit_zppt0040[] IS INITIAL.
    MESSAGE 'Nenhum dado encontrado para importação!' TYPE 'I'.
    RETURN.
  ENDIF.

  MODIFY zppt0040 FROM TABLE lit_zppt0040.
  IF sy-subrc EQ 0.
    DATA(linhas) = lines( lit_zppt0040 ).
    MESSAGE |Foram importados { linhas } registros!| TYPE 'I'.
    COMMIT WORK.
  ELSE.
    MESSAGE |Houve um erro na importação dos registros!| TYPE 'I'.
  ENDIF.
