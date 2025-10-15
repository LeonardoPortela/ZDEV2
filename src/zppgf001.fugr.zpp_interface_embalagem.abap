FUNCTION zpp_interface_embalagem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(EMBALAGENS) TYPE  ZPPET006
*"  EXPORTING
*"     REFERENCE(_MSG) TYPE  ZPPET010
*"----------------------------------------------------------------------

  " 05.11.2024 - 152607 - RAMON -->

  DATA: number        TYPE tbtcjob-jobcount,
        name          TYPE tbtcjob-jobname,
        t_rsparams    TYPE rsparams_tt,
        w_rsparams    TYPE rsparams,
        "lv_full_dados TYPE string,
        lv_dados_json TYPE string.
  "lv_erro_json  TYPE string.

  IF embalagens-nr_ordem = 'null'.
    CONCATENATE 'JOB_DOC_CTB' embalagens-empresa embalagens-centro INTO name SEPARATED BY '_'.
  ELSE.
    CONCATENATE 'JOB_DOC_CTB' embalagens-nr_ordem INTO name SEPARATED BY '_'.
  ENDIF.


  lv_dados_json = /ui2/cl_json=>serialize( data = embalagens ).

  "solicitar e Aguardar execução do job
  DATA(lv_json_ret) = zcl_job=>insert_job_fila_escalonamento( EXPORTING
                                           i_nome_job      = name
                                           i_report        = 'ZPPR027'
                                           i_user_job      = sy-uname
                                           i_processar_retorno = abap_true
                                           i_dados_processar = lv_dados_json
                                           i_wait_schedule = abap_true
                                           i_wait_finish   = abap_true ).

  "SPLIT lv_full_dados AT '###' INTO lv_dados_json lv_erro_json.

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_json_ret
    CHANGING
      data = _msg.

*  CALL METHOD /ui2/cl_json=>deserialize
*    EXPORTING
*      json = lv_erro_json
*    CHANGING
*      data = e_erros.

  " 05.11.2024 - 152607 - RAMON <--























*************  DATA: it_zppt0015       TYPE TABLE OF zppt0015,
*************        vl_tipo_movimento TYPE bwart,
*************        _matnr            TYPE matnr,
*************        _lgort            TYPE lgort_d,
*************        _charg            TYPE charg_d.
*************
*************  DATA(obj_create) = NEW zcl_pm_ordem( ).
*************
*************  DATA(tp_mov) = |{ embalagens-acao CASE = LOWER }|.
*************  zcl_emb=>set_bloco( ).
*************
*************  DATA(produtos) = embalagens-produtos.
*************  SORT produtos BY lote_individual.
*************  DELETE ADJACENT DUPLICATES FROM produtos COMPARING lote_individual.
*************
*************  ASSIGN _msg TO FIELD-SYMBOL(<fs_msg>).
*************  <fs_msg>-empresa = embalagens-empresa.
*************  <fs_msg>-filial  = embalagens-centro.
*************
*************  zcl_emb=>check_qtd( embalagens ).
*************  <fs_msg>-resultado = return.
*************  CHECK lv_qtd_invalido IS INITIAL.
*************  zcl_emb=>add_19( embalagens ).
*************
*************  IF tp_mov IS INITIAL.
*************    APPEND VALUE #( type = 'E' message = 'Tipo de Movimento em Branco.' ) TO return.
*************  ENDIF.
*************
*************  vl_tipo_movimento = SWITCH #( tp_mov
*************                                   WHEN 'entrada'                   THEN '309'
*************                                   WHEN 'transferenciadeposito'     THEN '309' "USER STORY 152607 / AOENNING
*************                                   WHEN 'recebimento'               THEN '301'
*************                                   ELSE '311'
*************                              ).
*************
*************  LOOP AT produtos ASSIGNING FIELD-SYMBOL(<fs_produtos>).
*************
*************    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*************      EXPORTING
*************        input        = <fs_produtos>-codigo
*************      IMPORTING
*************        output       = <fs_produtos>-codigo
*************      EXCEPTIONS
*************        length_error = 1
*************        OTHERS       = 2.
*************
*************  ENDLOOP.
*************
*************  it_zppt0015 = VALUE #( FOR ls1 IN produtos
*************                         (
*************                           bloco             = zcl_emb=>get_bloco( )
*************                           dt_registro       = sy-datum
*************                           hr_registro       = sy-uzeit
*************                           empresa           = embalagens-empresa
*************                           centro            = embalagens-centro
*************                           material          = ls1-codigo
*************                           lote_fabricante   = |{ ls1-lote_fabricante CASE = UPPER }|
*************                           deposito_saida    = |{ ls1-deposito_saida  CASE = UPPER }|
*************                           lote_individual   = |{ ls1-lote_individual CASE = UPPER }|
*************                           deposito_receptor = ls1-deposito_receptor
*************                           data              = ls1-data
*************                           quantidade        = ls1-quantidade
*************                           centro_receptor   = ls1-centro_receptor
*************                           lote_receptor     = ls1-lote_receptor
*************                           unidade_medida    = ls1-unidade_medida
*************                           tipo_movimento    = vl_tipo_movimento
*************                           lote_excluido     = ls1-lote_excluido
*************                           nr_ordem          = embalagens-nr_ordem
*************                           id_movimentacao   = |{ ls1-id_movimentacao ALPHA = OUT }|
**************                           VENCIMENTO        = ZCL_EMB=>GET_VENCIMENTO(                                                                         VL_MATNR = |{ LS1-CODIGO ALPHA = IN }|
**************                                                                          WERKS = EMBALAGENS-CENTRO
**************                                                                          LGORT = LS1-DEPOSITO_SAIDA
**************                                                                          VL_CHARG = COND #( WHEN LS1-LOTE_FABRICANTE IS INITIAL
**************                                                                                                      THEN LS1-LOTE_INDIVIDUAL
**************                                                                                                      ELSE LS1-LOTE_FABRICANTE
**************                                                                                           )
**************                                                                      )
*************                           )
*************).
*************
*************  SORT it_zppt0015 BY vencimento lote_individual lote_fabricante.
*************  CHECK it_zppt0015 IS NOT INITIAL.
*************
*************  data = zcl_emb=>get_data( ).
*************  time = zcl_emb=>get_time( ).
*************
*************  MODIFY zppt0015 FROM TABLE it_zppt0015.
*************  COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024
*************
**************  IF lv_qtd_invalido IS INITIAL.
*************  zcl_emb=>processa_lotes( EXPORTING acao = tp_mov CHANGING input = it_zppt0015 ).
**************  ENDIF.
*************
*************  MODIFY zppt0015 FROM TABLE it_zppt0015.
*************  COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024
*************
*************  <fs_msg>-resultado = return.
*************
**************  _msg =  VALUE #(
**************                   empresa = embalagens-empresa
**************                   filial  = embalagens-centro
**************                   resultado = return
**************                 ).
*************
**************   "// Processa os Dados que foi Enviado pelo Mobile
*************  CALL METHOD obj_create->call_report
*************    EXPORTING
*************      i_sequen   = CONV #( |{ zcl_emb=>get_bloco( ) ALPHA = OUT }| )
*************      t_zppt0015 = it_zppt0015
*************      i_report   = 'ZPPR015'.

ENDFUNCTION.
