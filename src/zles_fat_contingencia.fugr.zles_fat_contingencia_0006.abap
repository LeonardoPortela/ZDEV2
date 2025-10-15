FUNCTION ZLES_FAT_CONTINGENCIA_0006.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  DATA: lva_destination TYPE char40.

  "RANGES: lra_object FOR nriv-object.

  DATA: lit_zmmt_ee_zgr_ecc TYPE TABLE OF zmmt_ee_zgr_tmp,
        lit_zmmt_ee_zgr_s4  TYPE TABLE OF zmmt_ee_zgr.

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
      i_operacao = '06'
    TABLES
      t_out_zmmt_ee_zgr = lit_zmmt_ee_zgr_ecc.

  IF lit_zmmt_ee_zgr_ecc[] IS INITIAL.
    MESSAGE 'Nenhuma entrada estoque a ser importada!' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT *
     FROM zmmt_ee_zgr INTO TABLE lit_zmmt_ee_zgr_s4
    FOR ALL ENTRIES IN lit_zmmt_ee_zgr_ecc
    WHERE obj_key = lit_zmmt_ee_zgr_ecc-obj_key.

  SORT lit_zmmt_ee_zgr_s4  by obj_key.
  SORT lit_zmmt_ee_zgr_ecc by obj_key.

  DATA(_registros_importados) = abap_false.
  LOOP AT lit_zmmt_ee_zgr_ecc INTO DATA(lwa_zmmt_ee_zgr_ecc).

    READ TABLE lit_zmmt_ee_zgr_s4 INTO DATA(lwa_zmmt_ee_zgr_s4) WITH KEY  obj_key = lwa_zmmt_ee_zgr_ecc-obj_key BINARY SEARCH.

    CHECK sy-subrc ne 0.

    CLEAR: lwa_zmmt_ee_zgr_s4.

    MOVE-CORRESPONDING lwa_zmmt_ee_zgr_ecc to lwa_zmmt_ee_zgr_s4.

    lwa_zmmt_ee_zgr_s4-zrg_atlz             = '1'.
    lwa_zmmt_ee_zgr_s4-fat_contingencia_ecc = abap_true.

    MODIFY zmmt_ee_zgr FROM lwa_zmmt_ee_zgr_s4.
    COMMIT WORK AND WAIT.

    _registros_importados = abap_true.
  ENDLOOP.

  IF _registros_importados EQ abap_true.
    MESSAGE 'Entrada Estoque importada com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'Todas as entradas de estoque ja foram importadas!' TYPE 'I'.
  ENDIF.

ENDFUNCTION.
