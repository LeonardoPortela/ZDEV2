FUNCTION zsd_get_data_nomeacao_transp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ID_NOMEACAO_TRAN STRUCTURE  ZENOMEACAO_TRAN
*"      T_SAIDA_ZNOM_TRANSPO STRUCTURE  ZNOM_TRANSPORTE
*"      T_SAIDA_ZNOM_PROGRAMACAO STRUCTURE  ZNOM_PROGRAMACAO
*"      T_SAIDA_ZNOM_PROG_REME STRUCTURE  ZTNOM_PROG_REME
*"      T_SAIDA_ZREG_EXPORTACAO STRUCTURE  ZREG_EXPORTACAO
*"----------------------------------------------------------------------

  DATA:
        lra_id_nomeacao_tran TYPE RANGE OF zid_nomeacao.


  CHECK t_id_nomeacao_tran[] is NOT INITIAL.  "*-Equalização RISE x PRD - 19.07.2023 - JT

  LOOP AT t_id_nomeacao_tran ASSIGNING FIELD-SYMBOL(<fs_id_nomeacao>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_id_nomeacao> ) TO lra_id_nomeacao_tran.
  ENDLOOP.


  SELECT *
  FROM znom_transporte
  INTO TABLE t_saida_znom_transpo
  WHERE  id_nomeacao_tran IN lra_id_nomeacao_tran.



  IF t_saida_znom_transpo[] IS NOT INITIAL.

    SELECT *
    FROM znom_programacao
    INTO TABLE t_saida_znom_programacao
    FOR ALL ENTRIES IN t_saida_znom_transpo
    WHERE id_nomeacao_tran = t_saida_znom_transpo-id_nomeacao_tran.

    SELECT  rm~*,
            lp~NTGEW
    FROM znom_prog_reme as rm
    LEFT OUTER join likp as lp on rm~ID_REMESSA eq  lp~vbeln
    INTO TABLE @t_saida_znom_prog_reme
    FOR ALL ENTRIES IN @t_saida_znom_transpo
    WHERE id_nomeacao_tran = @t_saida_znom_transpo-id_nomeacao_tran.

    IF t_saida_znom_prog_reme[] IS NOT INITIAL.

      SELECT  *
           FROM zreg_exportacao
        INTO TABLE t_saida_zreg_exportacao
        FOR ALL ENTRIES IN t_saida_znom_prog_reme
          WHERE id_registro_expo = t_saida_znom_prog_reme-id_registro_expo.
    ENDIF.

  ENDIF.


ENDFUNCTION.
