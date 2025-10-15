FUNCTION zles_fat_contingencia_0005.
*"--------------------------------------------------------------------
*"*"Interface local:
*"--------------------------------------------------------------------

  DATA: lva_destination TYPE char40.

  RANGES: lra_object FOR nriv-object.

  DATA: lit_nriv_ecc TYPE TABLE OF nriv,
        lit_nriv_s4  TYPE TABLE OF nriv.

  CLEAR: lra_object[].

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
      i_operacao = '05'
    TABLES
      t_out_nriv = lit_nriv_ecc.

  IF lit_nriv_ecc[] IS INITIAL.
    MESSAGE 'Nenhuma numeração a ser importada!' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_snun_equalize)
   WHERE name EQ 'SNUM_EQUALIZE_CONTINGENCIA'.

  LOOP AT lit_tvarvc_snun_equalize INTO DATA(lwa_tvarvc).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_tvarvc-low ) TO lra_object.
  ENDLOOP.

  IF lra_object[] IS INITIAL.
    MESSAGE 'Snum não configurada stvarv SNUM_EQUALIZE_CONTINGENCIA!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT * FROM nriv INTO TABLE lit_nriv_s4.

  DELETE lit_nriv_s4  WHERE object NOT IN lra_object.
  DELETE lit_nriv_ecc WHERE object NOT IN lra_object.

  DATA(_snum_equalizada) = abap_false.
  LOOP AT lit_nriv_ecc INTO DATA(lwa_nriv_ecc).

    READ TABLE lit_nriv_s4 INTO DATA(lwa_nriv_s4) WITH KEY  client     = lwa_nriv_ecc-client
                                                            object     = lwa_nriv_ecc-object
                                                            subobject  = lwa_nriv_ecc-subobject
                                                            nrrangenr  = lwa_nriv_ecc-nrrangenr
                                                            toyear     = lwa_nriv_ecc-toyear.

    if sy-subrc ne 0.
      MESSAGE |SNUM Object: {  lwa_nriv_ecc-object } SubOject: { lwa_nriv_ecc-subobject } nao encontrada no S4 !| TYPE 'I'.
      CONTINUE.
    endif.

    CHECK lwa_nriv_ecc-nrlevel > lwa_nriv_s4-nrlevel.

    lwa_nriv_s4-nrlevel = lwa_nriv_ecc-nrlevel.

    MODIFY nriv FROM lwa_nriv_s4.
    COMMIT WORK AND WAIT.

    _snum_equalizada = abap_true.
  ENDLOOP.

  IF _snum_equalizada EQ abap_true.
    MESSAGE 'SNUM equalizada com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'SNUM não foi equalizada!' TYPE 'I'.
  ENDIF.

ENDFUNCTION.
