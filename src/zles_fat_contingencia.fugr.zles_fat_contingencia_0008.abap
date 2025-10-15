FUNCTION zles_fat_contingencia_0008.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_ERROR)
*"----------------------------------------------------------------------

  RANGES: lra_vbeln FOR likp-vbeln.

  DATA: lva_destination TYPE char40.

  DATA: lwa_zlest0108_bapi TYPE zlest0108,
        lwa_zlest0109_bapi TYPE zlest0109,
        lwa_zlest0110_bapi TYPE zlest0110,
        lit_zlest0109_bapi TYPE TABLE OF zlest0109.

  DATA: lit_zlest0108_ecc TYPE TABLE OF zlest0108,
        lit_zlest0109_ecc TYPE TABLE OF zlest0109_tmp,
        lit_zlest0110_ecc TYPE TABLE OF zlest0110_tmp.

  DATA: lwa_zlest0108_ecc TYPE zlest0108.
  DATA: lwa_docs_gerados TYPE ZMMT_EE_ZGR_DOCS.


  DATA: lit_zlest0108_s4 TYPE TABLE OF zlest0108.

*  CLEAR: LRA_VBELN[].
*
*  IF i_vbeln IS NOT INITIAL.
*    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = I_VBELN ) TO lra_vbeln.
*  ENDIF.

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
      i_operacao      = '04'
      i_vbeln         = i_vbeln
    TABLES
      t_out_zlest0108 = lit_zlest0108_ecc
      t_out_zlest0109 = lit_zlest0109_ecc
      t_out_zlest0110 = lit_zlest0110_ecc.


  IF lit_zlest0108_ecc[] IS INITIAL.
    MESSAGE 'Nenhum frete a ser importado!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT *
    FROM zlest0108 INTO TABLE lit_zlest0108_s4
    FOR ALL ENTRIES IN lit_zlest0108_ecc
    WHERE vbeln_ecc = lit_zlest0108_ecc-vbeln.

  SORT lit_zlest0108_s4 BY vbeln.

  DATA(lva_registro_importado) = abap_false.

  LOOP AT lit_zlest0108_ecc INTO lwa_zlest0108_ecc.

    DATA(lva_exists) = abap_false.

    READ TABLE lit_zlest0108_s4 INTO DATA(lwa_zlest0108_s4) WITH KEY vbeln_ecc = lwa_zlest0108_ecc-vbeln.

    CHECK sy-subrc NE 0.

    CLEAR: lwa_zlest0108_bapi, lwa_zlest0110_bapi, lit_zlest0109_bapi[].

    MOVE-CORRESPONDING lwa_zlest0108_ecc TO lwa_zlest0108_bapi.
    CLEAR:  lwa_zlest0108_bapi-vbeln,
            lwa_zlest0108_bapi-ST_PROC,
            lwa_zlest0108_bapi-DOC_TRANSP,
            lwa_zlest0108_bapi-FKNUM,
            lwa_zlest0108_bapi-OV_FRETE,
            lwa_zlest0108_bapi-FATURA_FRETE,
            lwa_zlest0108_bapi-NRO_NF_FRETE,
            lwa_zlest0108_bapi-KBETR.


    lwa_zlest0108_bapi-vbeln_ecc            = lwa_zlest0108_ecc-vbeln.
    lwa_zlest0108_bapi-fat_contingencia_ecc = abap_true.


    READ TABLE lit_zlest0110_ecc INTO DATA(lwa_zlest0110_ecc) WITH KEY vbeln = lwa_zlest0108_ecc-vbeln.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lwa_zlest0110_ecc TO lwa_zlest0110_bapi.
      CLEAR: lwa_zlest0110_bapi-vbeln.
    ENDIF.

    LOOP AT lit_zlest0109_ecc INTO DATA(lwa_zlest0109_ecc) WHERE vbeln = lwa_zlest0108_ecc-vbeln.
      MOVE-CORRESPONDING lwa_zlest0109_ecc TO lwa_zlest0109_bapi.
      CLEAR: lwa_zlest0109_bapi-vbeln.
      APPEND lwa_zlest0109_bapi TO lit_zlest0109_bapi.
    ENDLOOP.

    DATA: vl_data_aviso TYPE datum.

    vl_data_aviso = lwa_zlest0108_bapi-data_aviso.

    CALL FUNCTION 'Z_MM_CRIAR_AVISO'
      EXPORTING
        wa_zlest0108       = lwa_zlest0108_bapi
        wa_zlest0110       = lwa_zlest0110_bapi
        data_aviso         = vl_data_aviso
        gerar_apenas_aviso = 'X'
        frete_ent_terc     = 'X'
      IMPORTING
        doc_gerados        = lwa_docs_gerados
      TABLES
        it_zlest0109       = lit_zlest0109_bapi
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    lva_registro_importado = abap_true.

  ENDLOOP.

  IF lva_registro_importado EQ abap_true.
    MESSAGE 'Registros Importados com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'Todos os registros já foram importados!' TYPE 'S'.
  ENDIF.

ENDFUNCTION.
