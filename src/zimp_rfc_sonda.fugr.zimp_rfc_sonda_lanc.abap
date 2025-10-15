FUNCTION zimp_rfc_sonda_lanc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(FLAG) LIKE  /PWSATI/ZSATI009-IOS_TP_CARGA
*"     VALUE(RECORDS) LIKE  SOID-ACCNT
*"  EXPORTING
*"     VALUE(TOTAL) LIKE  SOID-ACCNT
*"     VALUE(VERSAO) TYPE  CHAR5
*"  TABLES
*"      LF_ZIMP_LANC_SONDA STRUCTURE  ZIMP_LANC_SONDA
*"----------------------------------------------------------------------

  IF records = 0.
    records = 10.
  ENDIF.

  versao = '1.0'.


  IF flag = 'S'.

    SELECT *
       APPENDING TABLE  lf_zimp_lanc_sonda
       FROM zimp_lanc_sonda  UP TO records ROWS
       WHERE iaj_cod_matriz   NE space AND
             iaj_cod_filial   NE space AND
             iaj_nr_doc_arrec NE space AND
             iaj_dt_lancto    NE space.

  ELSEIF flag = 'D'.

    LOOP AT lf_zimp_lanc_sonda.

      DELETE FROM zimp_lanc_sonda WHERE
      iaj_cod_matriz = lf_zimp_lanc_sonda-iaj_cod_matriz     AND
      iaj_cod_filial = lf_zimp_lanc_sonda-iaj_cod_filial     AND
      iaj_nr_doc_arrec = lf_zimp_lanc_sonda-iaj_nr_doc_arrec AND
      iaj_dt_lancto    = lf_zimp_lanc_sonda-iaj_dt_lancto.

    ENDLOOP.
    COMMIT WORK.
    SELECT COUNT(*) INTO total FROM zimp_lanc_sonda.

  ENDIF.


ENDFUNCTION.
