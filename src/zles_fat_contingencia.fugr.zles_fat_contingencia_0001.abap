FUNCTION zles_fat_contingencia_0001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  DATA: lva_destination TYPE char40.

  DATA: lit_zsdt0001_ecc      TYPE TABLE OF zlese0161,
        lit_zsdt0001ovro_ecc  TYPE TABLE OF zsdt0001ovro,
        lit_zsdt0001ov_ecc    TYPE TABLE OF zsdt0001ov,
        lit_zsdt0001rs_ecc    TYPE TABLE OF zsdt0001rs,
        lit_zsdt0001cl_ecc    TYPE TABLE OF zsdt0001cl,
        lit_zsdt0001cg_ecc    TYPE TABLE OF zlese0162,
        lit_zsdt0001_item_ecc TYPE TABLE OF zlese0163.

  DATA: lwa_zsdt0001_ecc      TYPE zsdt0001,
        lwa_zsdt0001cg_ecc    TYPE zsdt0001cg,
        lwa_zsdt0001_item_ecc TYPE zsdt0001_item.

  DATA: lit_zsdt0001_s4 TYPE TABLE OF zsdt0001.

  CASE sy-sysid.
    WHEN 'DEV'.
      lva_destination =  'DEV_ECC'.
    WHEN 'QAS'.
      lva_destination =  'QAS_ECC'.
    WHEN 'PRD'.
      lva_destination =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CHECK lva_destination IS NOT INITIAL.

  "Função que Le o XML no GRC
  CALL FUNCTION 'ZLES_FATURAMENTO_CONTINGENCIA' DESTINATION lva_destination
    EXPORTING
      i_operacao         = '01'
    TABLES
      t_out_zsdt0001      = lit_zsdt0001_ecc
      t_out_zsdt0001ovro  = lit_zsdt0001ovro_ecc
      t_out_zsdt0001ov    = lit_zsdt0001ov_ecc
      t_out_zsdt0001rs    = lit_zsdt0001rs_ecc
      t_out_zsdt0001cl    = lit_zsdt0001cl_ecc
      t_out_zsdt0001cg    = lit_zsdt0001cg_ecc
      t_out_zsdt0001_item = lit_zsdt0001_item_ecc.

  IF lit_zsdt0001_ecc[] IS INITIAL.
    MESSAGE 'Nenhum romaneio a ser importado!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT *
    FROM zsdt0001 INTO TABLE lit_zsdt0001_s4
    FOR ALL ENTRIES IN lit_zsdt0001_ecc
    WHERE ch_referencia = lit_zsdt0001_ecc-ch_referencia.

  SORT lit_zsdt0001_s4 BY ch_referencia.

  DATA(lva_romaneio_importado) = abap_false.

  LOOP AT lit_zsdt0001_ecc INTO DATA(lwa_zsdt0001).

    DATA(lva_exists) = abap_false.

    READ TABLE lit_zsdt0001_s4 INTO DATA(lwa_zsdt0001_s4) WITH KEY ch_referencia = lwa_zsdt0001-ch_referencia BINARY SEARCH.

    IF sy-subrc eq 0.
      lva_exists = abap_true.
    ENDIF.

    CLEAR: lwa_zsdt0001_ecc.
    MOVE-CORRESPONDING lwa_zsdt0001 TO lwa_zsdt0001_ecc.

    IF lva_exists eq abap_true.
      IF lwa_zsdt0001_ecc eq lwa_zsdt0001_s4.
        CONTINUE.
      ENDIF.
    ELSE.
      CLEAR: lwa_zsdt0001_ecc-status, lwa_zsdt0001_ecc-st_proc,lwa_zsdt0001_ecc-doc_rem,lwa_zsdt0001_ecc-fatura_prod,
             lwa_zsdt0001_ecc-nro_nf_prod,lwa_zsdt0001_ecc-doc_transp,lwa_zsdt0001_ecc-tknum,lwa_zsdt0001_ecc-fknum,
             lwa_zsdt0001_ecc-ov_frete,lwa_zsdt0001_ecc-fatura_frete,lwa_zsdt0001_ecc-nro_nf_frete,
             lwa_zsdt0001_ecc-kbetr,lwa_zsdt0001_ecc-agente_frete,lwa_zsdt0001_ecc-seq_lcto,
             lwa_zsdt0001_ecc-nro_nf_rem, lwa_zsdt0001_ecc-doc_aviso, lwa_zsdt0001_ecc-doc_material_e,
             lwa_zsdt0001_ecc-ano_material_e, lwa_zsdt0001_ecc-doc_material,lwa_zsdt0001_ecc-ano_material,
             lwa_zsdt0001_ecc-docnum_aquav.
    ENDIF.

    lwa_zsdt0001_ecc-doc_rem_ecc = lwa_zsdt0001_ecc-doc_rem.
    lwa_zsdt0001_ecc-fat_contingencia_ecc = abap_true.

    MODIFY zsdt0001 FROM lwa_zsdt0001_ecc.

    LOOP AT lit_zsdt0001_item_ecc INTO DATA(lwa_zsdt0001_item) WHERE ch_referencia = lwa_zsdt0001_ecc-ch_referencia.
      CLEAR: lwa_zsdt0001_item_ecc.
      MOVE-CORRESPONDING lwa_zsdt0001_item TO lwa_zsdt0001_item_ecc.
      MODIFY zsdt0001_item FROM lwa_zsdt0001_item_ecc.
    ENDLOOP.

    READ TABLE lit_zsdt0001ovro_ecc INTO DATA(lwa_zsdt0001ovro_ecc) WITH KEY ch_referencia_sai = lwa_zsdt0001_ecc-ch_referencia.
    IF sy-subrc EQ 0.
      DATA(lva_id_carga) = lwa_zsdt0001ovro_ecc-id_carga.

      LOOP AT lit_zsdt0001cg_ecc INTO DATA(lwa_zsdt0001cg) WHERE id_carga = lva_id_carga.
        CLEAR: lwa_zsdt0001cg_ecc.
        MOVE-CORRESPONDING lwa_zsdt0001cg TO lwa_zsdt0001cg_ecc.
        MODIFY zsdt0001cg FROM lwa_zsdt0001cg_ecc.
      ENDLOOP.

      LOOP AT lit_zsdt0001ovro_ecc INTO lwa_zsdt0001ovro_ecc WHERE id_carga = lva_id_carga.
        MODIFY zsdt0001ovro FROM lwa_zsdt0001ovro_ecc.
      ENDLOOP.

      LOOP AT lit_zsdt0001ov_ecc INTO DATA(lwa_zsdt0001ov_ecc) WHERE id_carga = lva_id_carga.
        MODIFY zsdt0001ov FROM lwa_zsdt0001ov_ecc.
      ENDLOOP.

      LOOP AT lit_zsdt0001rs_ecc INTO DATA(lwa_zsdt0001rs_ecc) WHERE id_carga = lva_id_carga.
        MODIFY zsdt0001rs FROM lwa_zsdt0001rs_ecc.
      ENDLOOP.

      LOOP AT lit_zsdt0001cl_ecc INTO DATA(lwa_zsdt0001cl_ecc) WHERE id_carga = lva_id_carga.
        MODIFY zsdt0001cl FROM lwa_zsdt0001cl_ecc.
      ENDLOOP.

    ENDIF.

    lva_romaneio_importado = abap_true.

  ENDLOOP.

  IF lva_romaneio_importado EQ ABAP_TRUE.
    MESSAGE 'Romaneios Importados com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'Todos os romaneios romaneios já foram importados!' TYPE 'S'.
  ENDIF.

ENDFUNCTION.
