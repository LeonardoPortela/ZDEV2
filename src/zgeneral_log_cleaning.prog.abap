*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report  ZGENERAL_LOG_CLEANING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zgeneral_log_cleaning.


IF sy-batch EQ abap_true.
  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.


*--------------------------------------------------------------------------------*
* Tabelas Gerais
*--------------------------------------------------------------------------------*

PERFORM f_clear_geral.

*--------------------------------------------------------------------------------*
* Tabelas MM
*--------------------------------------------------------------------------------*

PERFORM f_clear_zib_cte_dist_log. "Tabela de InBound de CT-e Distribuida
PERFORM f_clear_zmmt0087.
*--------------------------------------------------------------------------------*
* Tabelas SD
*--------------------------------------------------------------------------------*
PERFORM f_clear_zsdt0327.  "Log de Registros Integração SAP x Trace Cotton "#147275-30.07.2024-JT

*--------------------------------------------------------------------------------*
* Tabelas LES
*--------------------------------------------------------------------------------*


*--------------------------------------------------------------------------------*
* Tabelas FI
*--------------------------------------------------------------------------------*



* Fim |--------------------------------------------------------------------------------------------------------------------------------------------------------------------*

*-#147275-30.07.2024-JT-inicio
FORM f_clear_zsdt0327.

  TYPES: BEGIN OF ty_table_key,
           mandt        TYPE zsdt0327-mandt,
           zseq_inst    TYPE zsdt0327-zseq_inst,
           objek        TYPE zsdt0327-objek,
           objecttable  TYPE zsdt0327-objecttable,
           id_contrato  TYPE zsdt0327-id_contrato,
           nro_sol_ov   TYPE zsdt0327-nro_sol_ov,
           posnr        TYPE zsdt0327-posnr,
           id_carga     TYPE zsdt0327-id_carga,
           matnr        TYPE zsdt0327-matnr,
           werks        TYPE zsdt0327-werks,
           lgort        TYPE zsdt0327-lgort,
           acharg       TYPE zsdt0327-acharg,
           safra        TYPE zsdt0327-safra,
           seq          TYPE zsdt0327-seq,
           tipo_integra TYPE zsdt0327-tipo_integra.
  TYPES END OF ty_table_key.

  DATA: lit_table_key TYPE TABLE OF ty_table_key.

  DATA: lra_tipo_integra_update TYPE RANGE OF zsdt0327-tipo_integra.

  DATA: lva_dt_registro TYPE zsdt0327-data.

  APPEND VALUE #(  sign = 'I' option = 'EQ' low = 'CO' ) TO lra_tipo_integra_update.
  APPEND VALUE #(  sign = 'I' option = 'EQ' low = 'IN' ) TO lra_tipo_integra_update.
  APPEND VALUE #(  sign = 'I' option = 'EQ' low = 'OV' ) TO lra_tipo_integra_update.

  MESSAGE 'Iniciando Limpeza Logs tabela ZSDT0327...' TYPE 'S'.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(tvarvc)
   WHERE name EQ 'ZSDT0327_DIAS_RETENCAO'.

  IF sy-subrc EQ 0 AND tvarvc-low IS NOT INITIAL.
    lva_dt_registro = sy-datum - tvarvc-low.
  ELSE.
    lva_dt_registro = sy-datum - 90.
  ENDIF.

  DATA(_stop) = abap_false.

  WHILE _stop EQ abap_false.

    CLEAR: lit_table_key[].

    SELECT mandt    zseq_inst   objek        objecttable  id_contrato  nro_sol_ov
           posnr    id_carga    matnr        werks        lgort        acharg
           safra    seq         tipo_integra
      FROM zsdt0327
      INTO TABLE lit_table_key
        UP TO 20000 ROWS
     WHERE data         < lva_dt_registro
       AND marcado_elim = abap_off.

    IF sy-subrc NE 0.
      _stop = abap_true.
    ELSE.
      LOOP AT lit_table_key INTO DATA(liw_table_key).
        IF liw_table_key-tipo_integra IN lra_tipo_integra_update .
          UPDATE zsdt0327 SET mensagem     = abap_off
                              marcado_elim = abap_true
                        WHERE zseq_inst    = liw_table_key-zseq_inst
                          AND objek        = liw_table_key-objek
                          AND objecttable  = liw_table_key-objecttable
                          AND id_contrato  = liw_table_key-id_contrato
                          AND nro_sol_ov   = liw_table_key-nro_sol_ov
                          AND posnr        = liw_table_key-posnr
                          AND id_carga     = liw_table_key-id_carga
                          AND matnr        = liw_table_key-matnr
                          AND werks        = liw_table_key-werks
                          AND lgort        = liw_table_key-lgort
                          AND acharg       = liw_table_key-acharg
                          AND safra        = liw_table_key-safra
                          AND seq          = liw_table_key-seq.

        ELSE.
          DELETE zsdt0327 FROM liw_table_key.
        ENDIF.
      ENDLOOP.

      COMMIT WORK.
    ENDIF.

  ENDWHILE.

ENDFORM.
*-#147275-30.07.2024-JT-fim

FORM f_clear_geral.

  PERFORM f_clear_zsmt0001.     "Tabela de Registro de Impressão de SmartForms
  PERFORM f_clear_zob_mensagem. "Tabela de Mensagem de Processos SAP x Legados
  PERFORM f_clear_zjob0003.     "Tabelas de Escalonamento de Jobs

ENDFORM.

FORM f_clear_zsmt0001.

  TYPES: BEGIN OF ty_table_key,
           mandt           TYPE zsmt0001-mandt,
           cd_autenticacao TYPE zsmt0001-cd_autenticacao,
         END OF ty_table_key.

  DATA: lit_table_key TYPE TABLE OF ty_table_key.

  DATA: lva_dt_registro TYPE zsmt0001-dt_registro.

  MESSAGE 'Iniciando Limpeza Logs tabela ZSMT0001...' TYPE 'S'.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(tvarvc)
   WHERE name EQ 'ZSMT0001_DIAS_RETENCAO'.

  IF sy-subrc EQ 0 AND tvarvc-low IS NOT INITIAL.
    lva_dt_registro = sy-datum - tvarvc-low.
  ELSE.
    lva_dt_registro = sy-datum - 180.
  ENDIF.

  DATA(_stop) = abap_false.

  WHILE _stop EQ abap_false.

    CLEAR: lit_table_key[].

    SELECT mandt cd_autenticacao
      FROM zsmt0001 INTO TABLE lit_table_key
       UP TO 20000 ROWS
     WHERE dt_registro LT lva_dt_registro.

    IF sy-subrc NE 0.
      _stop = abap_true.
    ELSE.
      DELETE zsmt0001 FROM TABLE lit_table_key.
      COMMIT WORK.
    ENDIF.

  ENDWHILE.


ENDFORM.


FORM f_clear_zib_cte_dist_log .


  TYPES: BEGIN OF ty_table_key,
           mandt          TYPE zib_cte_dist_log-mandt,
           cd_chave_cte   TYPE zib_cte_dist_log-cd_chave_cte,
           dt_atualizacao TYPE zib_cte_dist_log-dt_atualizacao,
           hr_atualizacao TYPE zib_cte_dist_log-hr_atualizacao,
           nr_sequencia   TYPE zib_cte_dist_log-nr_sequencia,
         END OF ty_table_key.

  DATA: lit_table_key TYPE TABLE OF ty_table_key.

  DATA: lva_dt_registro TYPE zib_cte_dist_log-dt_atualizacao.

  MESSAGE 'Iniciando Limpeza Logs tabela ZIB_CTE_DIST_LOG...' TYPE 'S'.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(tvarvc)
   WHERE name EQ 'ZIB_CTE_DIST_LOG_DIAS_RETENCAO'.

  IF sy-subrc EQ 0 AND tvarvc-low IS NOT INITIAL.
    lva_dt_registro = sy-datum - tvarvc-low.
  ELSE.
    lva_dt_registro = sy-datum - 10.
  ENDIF.

  DATA(_stop) = abap_false.

  WHILE _stop EQ abap_false.

    CLEAR: lit_table_key[].

    SELECT mandt cd_chave_cte dt_atualizacao hr_atualizacao nr_sequencia
      FROM zib_cte_dist_log INTO TABLE lit_table_key
       UP TO 20000 ROWS
     WHERE dt_atualizacao LT lva_dt_registro.

    IF sy-subrc NE 0.
      _stop = abap_true.
    ELSE.
      DELETE zib_cte_dist_log FROM TABLE lit_table_key.
      COMMIT WORK.
    ENDIF.

  ENDWHILE.

ENDFORM.


FORM f_clear_zob_mensagem.

  TYPES: BEGIN OF ty_table_key,
           mandt        TYPE zob_mensagem-mandt,
           seq_registro TYPE zob_mensagem-seq_registro,
         END OF ty_table_key.

  DATA: lit_table_key TYPE TABLE OF ty_table_key.

  DATA: lva_dt_registro TYPE zob_mensagem-dt_atualizacao.

  MESSAGE 'Iniciando Limpeza Logs ZOB_MENSAGEM...' TYPE 'S'.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(tvarvc)
   WHERE name EQ 'ZOB_MENSAGEM_DIAS_RETENCAO'.

  IF sy-subrc EQ 0 AND tvarvc-low IS NOT INITIAL.
    lva_dt_registro = sy-datum - tvarvc-low.
  ELSE.
    lva_dt_registro = sy-datum - 30.
  ENDIF.

  DATA(_stop) = abap_false.

  WHILE _stop EQ abap_false.

    CLEAR: lit_table_key[].

    SELECT mandt seq_registro
      FROM zob_mensagem INTO TABLE lit_table_key
        UP TO 20000 ROWS
     WHERE dt_atualizacao LT lva_dt_registro.

    IF sy-subrc NE 0.
      _stop = abap_true.
    ELSE.
      DELETE zob_mensagem FROM TABLE lit_table_key.
      COMMIT WORK.
    ENDIF.

  ENDWHILE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_clear_zmmt0087
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_clear_zmmt0087 .
  TYPES: BEGIN OF ty_table_key,
           mandt            TYPE zmmt0087-mandt,
           rsnum            TYPE zmmt0087-rsnum,
           rspos            TYPE zmmt0087-rspos,
           matnr            TYPE zmmt0087-matnr,
           mblnr            TYPE zmmt0087-mblnr,
           operacao        TYPE zmmt0087-operacao,
           count_devolucao  TYPE zmmt0087-count_devolucao,
         END OF ty_table_key.
  DATA: lit_table_key TYPE TABLE OF ty_table_key.

  DATA: lva_data TYPE zmmt0087-data.

  MESSAGE 'Iniciando Limpeza Logs tabela ZMMT0087...' TYPE 'S'.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(tvarvc)
   WHERE name EQ 'ZMMT0087_DIAS_RETENCAO'.

  IF sy-subrc EQ 0 AND tvarvc-low IS NOT INITIAL.
    lva_data = sy-datum - tvarvc-low.
  ELSE.
    lva_data = sy-datum - 720.
  ENDIF.

  DATA(_stop) = abap_false.

  WHILE _stop EQ abap_false.

    CLEAR: lit_table_key[].

    SELECT mandt rsnum rspos matnr mblnr operacao count_devolucao
      FROM zmmt0087 INTO TABLE lit_table_key
       UP TO 20000 ROWS
     WHERE data LT lva_data
     AND   NOT EXISTS ( SELECT * FROM zmmt0081
                        WHERE zmmt0081~rsnum = zmmt0087~rsnum ).
    IF sy-subrc NE 0.
      _stop = abap_true.
    ELSE.
      DELETE zmmt0087 FROM TABLE lit_table_key.
      COMMIT WORK.
    ENDIF.

  ENDWHILE.

ENDFORM.

FORM f_clear_zjob0003 .

  DATA: lva_data TYPE zmmt0087-data.

  MESSAGE 'Iniciando Limpeza Logs tabela ZJOB0003 e ZJOB0004...' TYPE 'S'.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(tvarvc)
   WHERE name EQ 'ZJOB0003_DIAS_RETENCAO'.

  IF sy-subrc EQ 0 AND tvarvc-low IS NOT INITIAL.
    lva_data = sy-datum - tvarvc-low.
  ELSE.
    lva_data = sy-datum - 2.
  ENDIF.

  DELETE FROM zjob0003 WHERE date_create < lva_data.
  DELETE FROM zjob0004
   WHERE NOT EXISTS ( SELECT id_fila_job
                        FROM zjob0003
                       WHERE zjob0003~id_fila_job = zjob0004~id_fila_job ).

ENDFORM.
