*&---------------------------------------------------------------------*
*& Report ZPPR027
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr027.

DATA: it_zppt0015       TYPE TABLE OF zppt0015,
      vl_tipo_movimento TYPE bwart,
      _matnr            TYPE matnr,
      _lgort            TYPE lgort_d,
      _charg            TYPE charg_d.

DATA embalagens TYPE zppet006.

DATA _msg TYPE zppet010.

PARAMETERS p_idfila TYPE zde_id_fila_job OBLIGATORY.

START-OF-SELECTION.

  DATA(obj_create) = NEW zcl_pm_ordem( ).

  SELECT SINGLE *
    FROM zjob0003 INTO @DATA(lv_dados_processar)
    WHERE id_fila_job EQ @p_idfila.

  CHECK sy-subrc EQ 0.

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_dados_processar-dados_processar
    CHANGING
      data = embalagens.

  DATA(tp_mov) = |{ embalagens-acao CASE = LOWER }|.

  zcl_emb=>set_bloco( ).

  DATA(produtos) = embalagens-produtos.

  SORT produtos BY lote_individual.

  DELETE ADJACENT DUPLICATES FROM produtos COMPARING lote_individual.

  ASSIGN _msg TO FIELD-SYMBOL(<fs_msg>).
  <fs_msg>-empresa = embalagens-empresa.
  <fs_msg>-filial  = embalagens-centro.

  zcl_emb=>check_qtd( embalagens ).
  <fs_msg>-resultado = zcl_emb=>return.

  IF zcl_emb=>lv_qtd_invalido IS INITIAL. "Adiconado condição #IR211179 /AOENNING

    zcl_emb=>add_19( embalagens ).

    IF tp_mov IS INITIAL.
      APPEND VALUE #( type = 'E' message = 'Tipo de Movimento em Branco.' ) TO zcl_emb=>return.
    ENDIF.

    vl_tipo_movimento = SWITCH #( tp_mov
                                     WHEN 'entrada'                   THEN '309'
                                     WHEN 'transferenciadeposito'     THEN '309' "USER STORY 152607 / AOENNING
                                     WHEN 'recebimento'               THEN '301'
                                     ELSE '311'
                                ).

    LOOP AT produtos ASSIGNING FIELD-SYMBOL(<fs_produtos>).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <fs_produtos>-codigo
        IMPORTING
          output       = <fs_produtos>-codigo
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
    ENDLOOP.

    READ TABLE produtos INTO DATA(ls_prod) INDEX 1. "154380 CS2024000909 Mel. integração SAP vs Amaggi Pack PSA
    IF sy-subrc = 0.
      SELECT SINGLE * FROM zppt0016 INTO @DATA(ls_zppt0016) where matnr = @ls_prod-codigo and werks = @ls_prod-centro_receptor and lgort = @ls_prod-deposito_saida and ZLICHA = @ls_prod-LOTE_FABRICANTE. "BUG 181752 - BG
    ENDIF.

    "CHECK ls_zppt0016 IS NOT INITIAL.
    "verificar na entrada não encontrou nada da zppt0016

    it_zppt0015 = VALUE #( FOR ls1 IN produtos
                           (
                             bloco             = zcl_emb=>get_bloco( )
                             dt_registro       = sy-datum
                             hr_registro       = sy-uzeit
                             empresa           = embalagens-empresa
                             centro            = embalagens-centro
                             material          = ls1-codigo
                             lote_fabricante   = |{ ls1-lote_fabricante CASE = UPPER }|
                             deposito_saida    = |{ ls1-deposito_saida  CASE = UPPER }|
                             lote_individual   = |{ ls1-lote_individual CASE = UPPER }| "ls_zppt0016-charg- 154380 CS2024000909 Mel. integração SAP vs Amaggi Pack PSA
                             deposito_receptor = ls1-deposito_receptor
                             data              = ls1-data
                             quantidade        = ls1-quantidade
                             centro_receptor   = ls1-centro_receptor
                             lote_receptor     = ls1-lote_receptor
                             unidade_medida    = ls1-unidade_medida
                             tipo_movimento    = vl_tipo_movimento
                             lote_excluido     = ls1-lote_excluido
                             nr_ordem          = embalagens-nr_ordem
                             id_movimentacao   = |{ ls1-id_movimentacao ALPHA = OUT }|
*                           VENCIMENTO        = ZCL_EMB=>GET_VENCIMENTO(                                                                         VL_MATNR = |{ LS1-CODIGO ALPHA = IN }|
*                                                                          WERKS = EMBALAGENS-CENTRO
*                                                                          LGORT = LS1-DEPOSITO_SAIDA
*                                                                          VL_CHARG = COND #( WHEN LS1-LOTE_FABRICANTE IS INITIAL
*                                                                                                      THEN LS1-LOTE_INDIVIDUAL
*                                                                                                      ELSE LS1-LOTE_FABRICANTE
*                                                                                           )
*                                                                      )
                             )
  ).

    SORT it_zppt0015 BY vencimento lote_individual lote_fabricante.
    CHECK it_zppt0015 IS NOT INITIAL.

    zcl_emb=>data = zcl_emb=>get_data( ).
    zcl_emb=>time = zcl_emb=>get_time( ).

    MODIFY zppt0015 FROM TABLE it_zppt0015.
    COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

*  IF lv_qtd_invalido IS INITIAL.
    zcl_emb=>processa_lotes( EXPORTING acao = tp_mov CHANGING input = it_zppt0015 ).
*  ENDIF.

    MODIFY zppt0015 FROM TABLE it_zppt0015.
    COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

    <fs_msg>-resultado = zcl_emb=>return.

*  _msg =  VALUE #(
*                   empresa = embalagens-empresa
*                   filial  = embalagens-centro
*                   resultado = return
*                 ).

*   "// Processa os Dados que foi Enviado pelo Mobile
    CALL METHOD obj_create->call_report
      EXPORTING
        i_sequen   = CONV #( |{ zcl_emb=>get_bloco( ) ALPHA = OUT }| )
        t_zppt0015 = it_zppt0015
        i_report   = 'ZPPR015'.

  ENDIF. "Adiconado condição #IR211179 /AOENNING

  DATA lv_dados_json TYPE string.

  lv_dados_json = /ui2/cl_json=>serialize( data = <fs_msg> ).

*  IF lv_dados_processar-jobcount IS INITIAL. "154380 CS2024000909 Mel. integração SAP vs Amaggi Pack PSA
*"Buscar o último jobcount para o job e usuário informados
*    SELECT SINGLE jobcount
*    FROM tbtco
*    WHERE jobname   = @lv_dados_processar-jobname
*    AND sdluname   = @lv_dados_processar-user_job
*    INTO @DATA(_job_count).
*
*    IF sy-subrc = 0.
*      UPDATE zjob0003 SET jobcount = _job_count
*      WHERE id_fila_job = p_idfila.
*
*      COMMIT WORK AND WAIT.
*    ENDIF.
*
*  ENDIF.

  UPDATE zjob0003 SET dados_processados = lv_dados_json
    WHERE id_fila_job = p_idfila.

  COMMIT WORK AND WAIT.
