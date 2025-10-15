FUNCTION zimp_rfc_sonda_guia.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(FLAG) LIKE  /PWSATI/ZSATI009-IOS_TP_CARGA
*"     VALUE(RECORDS) LIKE  SOID-ACCNT
*"  EXPORTING
*"     VALUE(TOTAL) LIKE  SOID-ACCNT
*"     VALUE(VERSAO) TYPE  CHAR5
*"  TABLES
*"      LF_ZIMP_GUIA_SONDA STRUCTURE  ZIMP_GUIA_SONDA
*"----------------------------------------------------------------------

  IF records = 0.
    records = 10.
  ENDIF.

  versao = '1.0'.

  IF flag = 'S'.

    SELECT *
       APPENDING TABLE  lf_zimp_guia_sonda
       FROM zimp_guia_sonda  UP TO records ROWS
       WHERE igu_cod_matriz   NE space AND
             igu_cod_filial   NE space AND
             igu_num_doc_arr  NE space AND
             igu_cod_receita  NE space AND
             igu_referencia   NE space.

  ELSEIF flag = 'D'.

    LOOP AT lf_zimp_guia_sonda.

      DELETE FROM zimp_guia_sonda WHERE
      igu_cod_matriz   = lf_zimp_guia_sonda-igu_cod_matriz   AND
      igu_cod_filial   = lf_zimp_guia_sonda-igu_cod_filial   AND
      igu_num_doc_arr  = lf_zimp_guia_sonda-igu_num_doc_arr  AND
      igu_cod_receita  = lf_zimp_guia_sonda-igu_cod_receita  AND
      igu_referencia   = lf_zimp_guia_sonda-igu_referencia.

    ENDLOOP.
    COMMIT WORK.
    SELECT COUNT(*) INTO total FROM zimp_guia_sonda.

  ENDIF.





ENDFUNCTION.
