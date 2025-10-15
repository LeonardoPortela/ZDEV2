*----------------------------------------------------------------------*
***INCLUDE ZIMP61_PIX.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form zf_monta_arquivo_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_monta_arquivo_pix USING p_bncemp TYPE any
                                p_emp TYPE any.

  DATA: passei_aqui TYPE char1.
  CLEAR: v_tot_tit.
  REFRESH t_arquivo.

*------------------------------
*-verifica qual segmento
*------------------------------
  CLEAR v_segmento.

  READ TABLE t_zimp_cabecalho_pix ASSIGNING <f_zimp_cabecalho> INDEX 1.

  v_segmento = 'J'.

  SORT t_zimp_detalhe BY doc_imposto cod_abertura bukrs.

  lote_servico = 1.
  sequencial_reg_lote = 1.

  PERFORM prenche_header_arquivo_pix USING p_bncemp p_emp .
  PERFORM preenche_header_lote_pix   USING p_bncemp p_emp .

  APPEND w_header_arquivo TO t_arquivo1.

  IF p_bncemp = 'BBD'.
    sequencial_reg_lote = 0.
    MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.
    APPEND w_header_lote_bbd    TO t_arquivo1.
  ELSE.
    APPEND w_header_lote    TO t_arquivo1.
  ENDIF.

  LOOP AT t_zimp_cabecalho_pix ASSIGNING <f_zimp_cabecalho>.

    PERFORM zf_monta_tp_pix.

    APPEND w_arquivo TO t_arquivo.

    v_regs_proc = v_regs_proc + 1.

    PERFORM f_tbatch.

    PERFORM preenche_segmento_j_pix   USING p_bncemp p_emp .
    PERFORM preenche_segmento_j52_pix USING p_bncemp p_emp .

    v_tot_tit = v_tot_tit + v_tit.

    sequencial_reg_lote = sequencial_reg_lote + 1.

  ENDLOOP.

  PERFORM preenche_trailer_lote USING 'X' .
  APPEND w_trailer_lote    TO t_arquivo1.

  PERFORM preenche_trailer_arquivo.
  APPEND w_trailer_arquivo TO t_arquivo1.

  "Adicionar mais uma linha em branco quando for banco Bradesco, em branco.
  IF p_bncemp EQ 'BBD'.
    CLEAR: w_trailer_arquivo.
    APPEND w_trailer_arquivo TO t_arquivo1.
  ENDIF.

*-PBI 71420 - 12.01.2022 - JT - inicio
  DESCRIBE TABLE t_arquivo1 LINES DATA(l_lines_arq).
  LOOP AT t_arquivo1  INTO DATA(w_arq).
    IF p_bncemp = 'BBD'.
      CHECK sy-tabix < l_lines_arq.
    ENDIF.
    w_arq+240(2)         = cl_abap_char_utilities=>cr_lf.
    MODIFY t_arquivo1 FROM w_arq INDEX sy-tabix.
  ENDLOOP.
*-PBI 71420 - 12.01.2022 - JT - fim

  PERFORM zf_grava_arquivo.
  lote_servico = lote_servico + 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prenche_header_arquivo_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prenche_header_arquivo_pix USING p_bncemp TYPE any
                                      p_emp TYPE any .
  DATA: vl_number         TYPE i,
        proximo(6)        TYPE c,
        agencia(8)        TYPE c,
        conta(9)          TYPE c,
        conta_simples(12) TYPE c,
        anterior          TYPE c,
        digito_conta      TYPE c,
        dv_agencia        TYPE c,
        nome_banco        TYPE bnka-banka,
        conv_banco_bbd    TYPE char6.


  PERFORM zf_codigo_banco USING codigo_banco.
  PERFORM zf_banco_agencia  USING agencia
                                  conta
                                  nome_banco
                                  dv_agencia.

  w_header_arquivo-cod_banco              = codigo_banco.
  w_header_arquivo-lote_servico           = '0000'.
  w_header_arquivo-tipo_registro          = '0'.
  w_header_arquivo-cnab                   = '         '.
  w_header_arquivo-tipo_inscricao_empresa = '2'.
  w_header_arquivo-num_inscricao_empresa  = identificador.
  PERFORM add_zero_esquerda    USING w_header_arquivo-num_inscricao_empresa.

  IF p_bncemp = 'BBD'.
    conv_banco_bbd = conv_banco.

    READ TABLE t_tvarvc INTO DATA(ls_tvarvc) WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBD'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      conv_banco_bbd = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

* Verifica se tem stvarv para empresa
    LOOP AT t_tvarvc INTO ls_tvarvc WHERE name CS 'MAGGI_ZIMP62_CONVENIO_BBD'.
      IF ls_tvarvc IS NOT INITIAL.
        DATA(lva_emp_cod) = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          conv_banco_bbd = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_tvarvc,  lva_emp_cod.

    PERFORM add_zero_esquerda    USING conv_banco_bbd.
    w_header_arquivo-cod_convenio_banco     = conv_banco_bbd.
  ELSE.
    w_header_arquivo-cod_convenio_banco     = conv_banco.
    PERFORM add_zero_esquerda    USING w_header_arquivo-cod_convenio_banco.
  ENDIF.

  IF p_bncemp = 'BBRA'.
    w_header_arquivo-cod_convenio_banco2  = '0126'.

    READ TABLE t_tvarvc INTO ls_tvarvc WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBRA'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      w_header_arquivo-cod_convenio_banco2 = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

    LOOP AT t_tvarvc   INTO ls_tvarvc WHERE name CS  'MAGGI_ZIMP62_CONVENIO_BBRA'.
      IF ls_tvarvc IS NOT INITIAL.
        lva_emp_cod = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          w_header_arquivo-cod_convenio_banco2 = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR:ls_tvarvc, lva_emp_cod.
  ENDIF.

  w_header_arquivo-ag_mantenedora_conta   = agencia+4(4).

  IF p_bncemp = 'BBD'.
    w_header_arquivo-d_verificador_agencia  = ' '.
  ELSE.
    w_header_arquivo-d_verificador_agencia  = dv_agencia.
  ENDIF.

  PERFORM zf_nome_cliente USING  w_header_arquivo-num_inscricao_empresa.
  PERFORM f_obtem_proximo_arquivo USING proximo.
  PERFORM add_zero_esquerda USING w_header_arquivo-ag_mantenedora_conta.

  WHILE conta(1) NE ' '.
    IF conta(1) NE '-'.
      CONCATENATE  conta_simples conta(1) INTO conta_simples.
    ELSE.
      anterior = conta(1).
    ENDIF.
    IF anterior EQ '-'.
      digito_conta = conta+1(1).
      SHIFT conta.
    ENDIF.
    SHIFT conta.
  ENDWHILE.

  PERFORM add_zero_esquerda USING conta_simples.
  PERFORM add_zero_esquerda USING proximo.

  w_header_arquivo-num_conta_corrente      = conta_simples.
  w_header_arquivo-dg_verificador_conta    = digito_conta.

  IF p_bncemp = 'BBD'.
    w_header_arquivo-dg_verificador_ag_conta = ' '.
  ELSE.
    w_header_arquivo-dg_verificador_ag_conta = '0'.
  ENDIF.
  w_header_arquivo-nome_empresa            = v_butxt.
  w_header_arquivo-nome_banco              = nome_banco.
  w_header_arquivo-cnab1                   = '          '.
  w_header_arquivo-cod_remessa_retorno     = '1'.
  w_header_arquivo-hora_geracao_arquivo    = sy-uzeit.
  w_header_arquivo-num_sequencial_arquivo = proximo.
  CONCATENATE  sy-datum+6(2)  sy-datum+4(2)  sy-datum+0(4) INTO w_header_arquivo-data_geracao_arquivo.

  IF p_bncemp = 'BBRA'.
    w_header_arquivo-num_vs_layout_arquivo = '103'.
  ELSEIF p_bncemp = 'BBD'.
    w_header_arquivo-num_vs_layout_arquivo = '089'.
  ELSEIF p_bncemp(2) = 'IT'.
    w_header_arquivo-num_vs_layout_arquivo = '080'.
  ENDIF.

  IF p_bncemp = 'BBD'.
    w_header_arquivo-densidade_gravacao_arquivo = '01600'.
  ELSE.
    w_header_arquivo-densidade_gravacao_arquivo = '00000'.
  ENDIF.

  CONCATENATE 'PIX' w_header_arquivo-reservado_banco INTO w_header_arquivo-reservado_banco.

  PERFORM z_monta_espaco USING w_header_arquivo-reservado_banco.
  PERFORM z_monta_espaco USING w_header_arquivo-reservado_empresa.
  PERFORM z_monta_espaco USING w_header_arquivo-cnab2.
  PERFORM z_format       USING w_header_arquivo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form preenche_header_lote_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM preenche_header_lote_pix USING p_bncemp TYPE any
                                      p_emp TYPE any .

  DATA: codigo_banco(3)   TYPE c,
        agencia(8)        TYPE c,
        conta(9)          TYPE c,
        conta_simples(12) TYPE c,
        anterior          TYPE c,
        digito_conta      TYPE c,
        dv_agencia        TYPE c,
        nome_banco        TYPE bnka-banka,
        conv_banco_bbd    TYPE char6.

  PERFORM zf_codigo_banco      USING codigo_banco.
  PERFORM f_obtem_proximo_lote USING proximo.
  PERFORM add_zero_esquerda    USING proximo.
  PERFORM zf_banco_agencia     USING agencia
                                     conta
                                     nome_banco
                                     dv_agencia.


  w_header_lote-lote_servico  = lote_servico."PROXIMO.
  PERFORM add_zero_esquerda    USING w_header_lote-lote_servico.
  w_header_lote-tipo_registro = '1'.
  w_header_lote-tipo_operacao = 'C'.
  w_header_lote-tipo_servico  = '20'.

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho> INDEX 1.

  w_header_lote-forma_lancamento = '47'. "PIX
  w_header_lote-num_vs_layout = '040'.
  w_header_lote-cnab                   = ' '.
  w_header_lote-cod_banco_compensacao  = codigo_banco.
  w_header_lote-tipo_inscricao_empresa = '2'.
  w_header_lote-num_inscricao_empresa  = identificador.
  PERFORM add_zero_esquerda    USING w_header_lote-num_inscricao_empresa.

*-BUG 71594 - 14.01.2022 - JT - inicio
  IF p_bncemp = 'BBD'.
    conv_banco_bbd = conv_banco.
**  Begin of CS2022000833 #97834 FF   05.12.2022
* BUG - 107478 - CBRAND -  Inicio

    READ TABLE t_tvarvc INTO DATA(ls_tvarvc) WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBD'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      conv_banco_bbd = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

    LOOP AT t_tvarvc INTO ls_tvarvc WHERE name CS 'MAGGI_ZIMP62_CONVENIO_BBD'.
      IF ls_tvarvc IS NOT INITIAL.
        DATA(lva_emp_cod) = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          conv_banco_bbd = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_tvarvc,  lva_emp_cod.
    PERFORM add_zero_esquerda    USING conv_banco_bbd.
    w_header_lote-cod_convenio_banco     = conv_banco_bbd.
  ELSE.
    w_header_lote-cod_convenio_banco     = conv_banco.
    PERFORM add_zero_esquerda    USING w_header_lote-cod_convenio_banco.
  ENDIF.

  IF p_bncemp = 'BBRA'.
    w_header_lote-cod_convenio_banco2  = '0126'.

    READ TABLE t_tvarvc INTO ls_tvarvc WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBRA'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      w_header_lote-cod_convenio_banco2 = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

    LOOP AT t_tvarvc   INTO ls_tvarvc WHERE name CS  'MAGGI_ZIMP62_CONVENIO_BBRA'.
      IF ls_tvarvc IS NOT INITIAL.
        lva_emp_cod = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          w_header_lote-cod_convenio_banco2 = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_tvarvc,  lva_emp_cod.
  ENDIF.


  w_header_lote-agencia_convenio       = agencia+4(4).

  IF p_bncemp = 'BBD'.
    w_header_lote-dig_verif_agencia      = ' '.
  ELSE.
    w_header_lote-dig_verif_agencia      = dv_agencia.
  ENDIF.
  PERFORM add_zero_esquerda USING w_header_lote-agencia_convenio.


  WHILE conta(1) NE ' '.
    IF conta(1) NE '-'.
      CONCATENATE  conta_simples conta(1) INTO conta_simples.
    ELSE.
      anterior = conta(1).
    ENDIF.
    IF anterior EQ '-'.
      digito_conta = conta+1(1).
      SHIFT conta.
    ENDIF.

    SHIFT conta.
  ENDWHILE.

  PERFORM zf_nome_cliente USING  w_header_lote-num_inscricao_empresa.

  PERFORM add_zero_esquerda USING conta_simples.

  w_header_lote-num_conta_corrrente = conta_simples.
  w_header_lote-dig_verif_conta     = digito_conta.

  IF p_bncemp = 'BBD'.
    w_header_lote-dig_verif_ag_conta  = ' '.
  ELSE.
    w_header_lote-dig_verif_ag_conta  = '0'.
  ENDIF.
  w_header_lote-nome_empresa        = v_butxt.
  w_header_lote-mensagem            = '                                        '.
  w_header_lote-logradouro          = w_adrc-street.

  w_header_lote-numero_local        = w_adrc-house_num1.

  PERFORM add_zero_esquerda USING w_header_lote-numero_local.

  w_header_lote-complemento         = w_adrc-city2.
  w_header_lote-cidade              = w_adrc-city1.
  w_header_lote-cep                 = |{ w_adrc-post_code1(5) }|.
  w_header_lote-complemento_cep     = |{ w_adrc-post_code1+6(3) }|.
  w_header_lote-estado              = w_adrc-region.

  IF p_bncemp = 'BBD'.
    w_header_lote-indic_forma_pag = '01'.
  ELSE .
    PERFORM z_monta_espaco USING w_header_lote-cnab_2.
  ENDIF.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_header_lote-cnab_2.
    PERFORM z_monta_espaco USING w_header_lote-cod_ocorrencia_retorno.
  ELSE.
    w_header_lote-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.

  PERFORM z_format       USING w_header_lote.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form preenche_segmento_j_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM preenche_segmento_j_pix USING p_bncemp TYPE any
                                   p_emp TYPE any .

  ASSIGN w_segmento_j TO <wa_data>.

  DATA: codigo_banco(3)       TYPE c.
  DATA: w_block_1(9)        TYPE c,
        w_block_2(10)       TYPE c,
        w_block_3(10)       TYPE c,
        w_block_4(14)       TYPE c,
        w_block_5(10)       TYPE c,
        w_codigo_barras(44) TYPE c,
        w_dv_1(1)           TYPE c,
        w_dv_2(1)           TYPE c,
        w_dv_3(1)           TYPE c,
        w_dv_4(2)           TYPE c,
        lv_part1(11)        TYPE c,
        lv_part2(11)        TYPE c,
        lv_part3(11)        TYPE c,
        lv_part4(11)        TYPE c.

  DATA: lv_date1          TYPE  d VALUE '19971007',
        lv_date2          TYPE  d,
        lv_datediff       TYPE  p,
        lv_datedifftxt(4) TYPE  c,
        lv_timediff       TYPE  p,
        lv_earliest       TYPE  c.

  DATA: lv_fator_mult  TYPE i.
  DATA: lv_pos  TYPE i.
  DATA: lv_calc  TYPE i.
  DATA: lv_tot  TYPE i.
  DATA: lv_rest  TYPE i.

  IF sequencial_reg_lote IS INITIAL.
    sequencial_reg_lote =  sequencial_reg_lote + 1.
  ENDIF.

  w_segmento_j-cod_segmento         = 'J'.
  w_segmento_j-valor_principal      = '0'.
  w_segmento_j-valor_desconto       = '0'.
  w_segmento_j-valor_juros_encargos = '0'.
  w_segmento_j-valor_pagamento      = '0'.
  w_segmento_j-quantidade_moeda     = '000000000000000'.
  w_segmento_j-cod_moeda            = '09'.
  w_segmento_j-num_doc_empresa      = <f_zimp_cabecalho>-doc_imposto.
  w_segmento_j-num_doc_banco        = '                    '.
*
  PERFORM preenche_valor.
*
  PERFORM add_zero_esquerda USING w_segmento_j-valor_principal.
  PERFORM add_zero_esquerda USING w_segmento_j-valor_desconto.
  PERFORM add_zero_esquerda USING w_segmento_j-valor_juros_encargos.
  PERFORM add_zero_esquerda USING w_segmento_j-valor_pagamento.

*---------------------------------------------------------------------
  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.

  w_segmento_j-cod_banco                      = codigo_banco.
  w_segmento_j-lote_servico                   = w_header_lote-lote_servico.  "proximo."LOTE_SERVICO. " iNFORMAR O NUMERO DO LOTE AO QUAL PERTENCE O REGISTRO. DEVE SER IGUAL AO NÚMERO INFORMADO NO HEADER DO LOTE
  PERFORM add_zero_esquerda USING w_segmento_j-lote_servico.
  w_segmento_j-registro_detalhe_lote          = '3'.
  w_segmento_j-sequencial_reg_lote            = sequencial_reg_lote.
  PERFORM add_zero_esquerda USING w_segmento_j-sequencial_reg_lote.
  w_segmento_j-tipo_movimento                 = '0'.
  w_segmento_j-cod_instrucao_movimento        = '09'. " Alterado de 00 para 09 no dia 12.03.2024

  lv_date2 = <f_zimp_cabecalho>-dt_venc.

  CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
    EXPORTING
      date1            = lv_date1
      date2            = lv_date2
    IMPORTING
      datediff         = lv_datediff
      timediff         = lv_timediff
      earliest         = lv_earliest
    EXCEPTIONS
      invalid_datetime = 1
      OTHERS           = 2.

  IF lv_datediff >= 10000.
    lv_datediff = lv_datediff - 9000.
  ENDIF.

  lv_datedifftxt = lv_datediff.

  " w_segmento_j-cod_barras .


  IF vlifnr IS NOT INITIAL.
    SELECT SINGLE name1 INTO w_segmento_j-nome_concessionaria
      FROM lfa1 WHERE lifnr = vlifnr.
  ELSE.
    w_segmento_j-nome_concessionaria            = '                              '.
  ENDIF.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_j-data_vencimento.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_j-data_pagamento.
*---------------------------------------------------------------------
*
  PERFORM z_monta_espaco USING w_segmento_j-cnab.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_segmento_j-cod_ocorrencia_retorno.
  ELSE.
    w_segmento_j-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.

  APPEND    w_segmento_j TO t_arquivo1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form preenche_segmento_j52_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM preenche_segmento_j52_pix USING p_bncemp TYPE any
                                     p_emp TYPE any .
  UNASSIGN <wa_data>.
  ASSIGN w_segmento_j52_pix TO <wa_data>.

  DATA: codigo_banco(3) TYPE c,
        l_name1         TYPE lfa1-name1,
        l_stcd1         TYPE lfa1-stcd1.

  DATA: lv_fator_mult  TYPE i.
  DATA: lv_pos  TYPE i.
  DATA: lv_calc  TYPE i.
  DATA: lv_tot  TYPE i.
  DATA: lv_rest  TYPE i.

  DATA: lva_00(2)    TYPE c,
        lva_count(2) TYPE c,
        lva_26(2)    TYPE c,
        lva_52(2)    TYPE c,
        lva_53(2)    TYPE c,
        lva_58(2)    TYPE c,
        lva_59(2)    TYPE c,
        lva_60(2)    TYPE c,
        lva_62(2)    TYPE c,
        lva_63(2)    TYPE c.


  CLEAR: w_segmento_j52_pix,
         l_name1.


  l_stcd1 = '0'.
  PERFORM add_zero_esquerda USING l_stcd1.

  sequencial_reg_lote = sequencial_reg_lote + 1.

  IF vlifnr IS NOT INITIAL.
    SELECT SINGLE     name1    stcd1
             INTO ( l_name1, l_stcd1 )
      FROM lfa1
     WHERE lifnr = vlifnr.
  ENDIF.

  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.

  w_segmento_j52_pix-cod_banco             = codigo_banco.
  w_segmento_j52_pix-lote_servico          = w_header_lote-lote_servico.
  w_segmento_j52_pix-registro_detalhe_lote = '3'.
  w_segmento_j52_pix-sequencial_reg_lote   = sequencial_reg_lote.
  w_segmento_j52_pix-cod_segmento          = 'J'.
  w_segmento_j52_pix-cnab                  = '  '.
  w_segmento_j52_pix-cod_mov_rem           = '00'.
  w_segmento_j52_pix-ident_reg_opcio       = '52'.

** BUG - 130040 - Inicio - CBRAND
  IF l_stcd1 IS INITIAL.
    w_segmento_j52_pix-tip_inscricao_pag     = '0'.
  ELSE.
    w_segmento_j52_pix-tip_inscricao_pag     = '2'.
  ENDIF.
** BUG - 130040 - Fim - CBRAND

  w_segmento_j52_pix-num_inscricao_pag     =  w_header_arquivo-num_inscricao_empresa.
  w_segmento_j52_pix-nome_pag              =  w_header_arquivo-nome_empresa.

  w_segmento_j52_pix-tip_inscricao_ben     = '2'.
  w_segmento_j52_pix-num_inscricao_ben     = l_stcd1.
  w_segmento_j52_pix-nome_ben              = l_name1.


  PERFORM zf_get_chaves_pix.

  PERFORM add_zero_esquerda USING w_segmento_j52_pix-lote_servico.
  PERFORM add_zero_esquerda USING w_segmento_j52_pix-sequencial_reg_lote.
  PERFORM add_zero_esquerda USING w_segmento_j52_pix-num_inscricao_pag.
  PERFORM add_zero_esquerda USING w_segmento_j52_pix-num_inscricao_ben.


  " PERFORM z_monta_espaco    USING w_segmento_j52-cnab2.

  APPEND    w_segmento_j52_pix     TO t_arquivo1.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_monta_tp_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_monta_tp_pix .

  CLEAR w_pagamento_pix.

  w_pagamento_pix-cod_receita          = <f_zimp_cabecalho>-cod_pgto.
  w_pagamento_pix-tipo_ident_contrib   = '01'.
  w_pagamento_pix-identif_contribuinte = identificador.
  PERFORM add_zero_esquerda USING w_pagamento_pix-identif_contribuinte.
  w_pagamento_pix-cod_identif_tribut   = <f_zimp_cabecalho>-tp_imposto.
  w_pagamento_pix-ano_base             = <f_zimp_cabecalho>-ano_apuracao.
  w_pagamento_pix-cod_renavam          = '000000000'.
  w_pagamento_pix-cod_municipio        = '00000'.
  w_pagamento_pix-unid_federacao       = '  '.
  w_pagamento_pix-placa_veiculo        = '       '.
  w_pagamento_pix-opcao_pagamento      = ' '.
  w_pagamento_pix-opcao_retirada_crvl  = ' '.

  PERFORM z_monta_espaco USING w_pagamento_pix-cnab.

  w_arquivo = w_pagamento_pix.

  PERFORM z_format       USING w_arquivo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_get_chaves_pix
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_get_chaves_pix .
  DATA: lva_qrcode  TYPE string,
        lva_size    TYPE i,
        lva_size_26 TYPE i,
        lva_total   TYPE i,
        lva_fix     TYPE i,
        lva_fix_26  TYPE i,
        lva_tam     TYPE i,
        lva_tam_26  TYPE i,
        lva_s       TYPE i,
        lva_id(2)   TYPE c,
        lva_s_26(2) TYPE c,
        lva_v_26    TYPE string,
        lva_v_00    TYPE string.
*        lva_s_00(2) TYPE c,
*        lva_v_01    TYPE string,
*        lva_s_01(2) TYPE c,
*        lva_v_02    TYPE string,
*        lva_s_02(2) TYPE c,
*        lva_v_05    TYPE string,
*        lva_s_05(2) TYPE c,
*        lva_s_26(2) TYPE c,
*        lva_v_26    TYPE string,
*        lva_s_52(2) TYPE c,
*        lva_v_52    TYPE string,
*        lva_s_53(2) TYPE c,
*        lva_v_53    TYPE string,
*        lva_s_54(2) TYPE c,
*        lva_v_54    TYPE string,
*        lva_s_58(2) TYPE c,
*        lva_v_58    TYPE string,
*        lva_s_59(2) TYPE c,
*        lva_v_59    TYPE string,
*        lva_s_60(2) TYPE c,
*        lva_v_60    TYPE string,
*        lva_s_62(2) TYPE c,
*        lva_v_62    TYPE string,
*        lva_s_63(2) TYPE c,
*        lva_v_63    TYPE string.

  lva_qrcode = <f_zimp_cabecalho>-qrcode.

  lva_size = strlen( lva_qrcode ).
  lva_s = 2.
  lva_id = lva_qrcode+0(2).

  "lva_qrcode = '00020126880014br.gov.bcb.pix01364afe2e54-02fb-44e8-81cb-d8eb849588740226Venc: 06.12.2023 R$ 423,275204000053039865406423.275802BR5925ENERGISA MATO GROSSO - DI6015COXIPO DA PONTE62270523BOLETO116757205102590376304680D'.
  "lva_qrcode = '00020126450014br.gov.bcb.pix0114+55659964498660205Teste520400005303986540510.005802BR5925AMAURY HENRIQUE SANTOS E 6006CUIABA62290525OPWtlSp3G4GBOc69gvgMqPScB630414A4'.
  "lva_qrcode = '00020101021226900014BR.GOV.BCB.PIX2568PIX-QRCODE.CAIXA.GOV.BR/API/V2/COBV/BA81EDFE41024D8288883078B85BDB3B5204000053039865802BR5923CAIXA ECONOMICA FEDERAL6008BRASILIA62070503***6304A2BE'.

  WHILE lva_size > 0.

    lva_id = lva_qrcode+lva_fix(lva_s).
    lva_fix = lva_fix + 2.
    lva_tam = lva_qrcode+lva_fix(lva_s).
    lva_fix = lva_fix + 2 .
    lva_v_00 = lva_qrcode+lva_fix(lva_tam).
    lva_fix = lva_fix + lva_tam.

    CASE lva_id.
      WHEN '26'.
        lva_fix_26 = 0.
        lva_s_26 = 2.
        lva_size_26 = strlen( lva_v_00 ).

        WHILE lva_size_26 > 0.

          lva_id = lva_v_00+lva_fix_26(lva_s).
          lva_fix_26 = lva_fix_26 + 2.
          lva_tam_26 = lva_v_00+lva_fix_26(lva_s).
          lva_fix_26 = lva_fix_26 + 2 .
          lva_v_26 = lva_v_00+lva_fix_26(lva_tam_26).
          lva_fix_26 = lva_fix_26 + lva_tam_26.

          IF lva_size_26 = lva_fix_26.
            w_segmento_j52_pix-url_chave_pix = lva_v_26.
            CONDENSE w_segmento_j52_pix-url_chave_pix NO-GAPS.
            EXIT.
          ENDIF.

        ENDWHILE.
    ENDCASE.

    IF lva_size = lva_fix.
      EXIT.
    ENDIF.

  ENDWHILE.


*** 05.03.2024 - Comentado - CSB - Inicio.
*  lva_size = strlen( lva_qrcode ).
*  lva_total = lva_size.
*  lva_fix = 2.
*  lva_s = 2.
*  lva_id = lva_qrcode+0(2).
*
*  lva_sum_size = 2.
*
*  CHECK lva_id = '00'.

*  WHILE lva_size > 0.
*    CASE lva_id.
*      WHEN '00'.
*        lva_s_00 = lva_qrcode+lva_fix(lva_s).
*        lva_s = lva_s + lva_s_00.
*        lva_v_00 = lva_qrcode+lva_s(lva_s_00).
*        lva_s = lva_s + lva_s_00.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '01'.
*        lva_s = lva_s + 2.
*        lva_s_01 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_01 = lva_qrcode+lva_s(lva_s_01).
*
*        lva_s = lva_s + lva_s_01.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '26'.
*        lva_s = lva_s + 2.
*        lva_s_26 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_26 = lva_qrcode+lva_s(lva_s_26).
*
*        lva_s = lva_s + lva_s_26.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '52'.
*        lva_s = lva_s + 2.
*        lva_s_52 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_52 = lva_qrcode+lva_s(lva_s_52).
*
*        lva_s = lva_s + lva_s_52.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '53'.
*        lva_s = lva_s + 2.
*        lva_s_53 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_53 = lva_qrcode+lva_s(lva_s_53).
*
*        lva_s = lva_s + lva_s_53.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '54'.
*        lva_s = lva_s + 2.
*        lva_s_54 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_54 = lva_qrcode+lva_s(lva_s_54).
*
*        lva_s = lva_s + lva_s_54.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '58'.
*        lva_s = lva_s + 2.
*        lva_s_58 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_58 = lva_qrcode+lva_s(lva_s_58).
*
*        lva_s = lva_s + lva_s_58.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*      WHEN '59'.
*        lva_s = lva_s + 2.
*        lva_s_59 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_59 = lva_qrcode+lva_s(lva_s_59).
*
*        lva_s = lva_s + lva_s_59.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '60'.
*        lva_s = lva_s + 2.
*        lva_s_60 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_60 = lva_qrcode+lva_s(lva_s_60).
*
*        lva_s = lva_s + lva_s_60.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '62'.
*        lva_s = lva_s + 2.
*        lva_s_62 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_62 = lva_qrcode+lva_s(lva_s_62).
*
*        lva_s = lva_s + lva_s_62.
*        lva_id = lva_qrcode+lva_s(lva_fix).
*
*      WHEN '63'.
*        lva_s = lva_s + 2.
*        lva_s_63 = lva_qrcode+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_63 = lva_qrcode+lva_s(lva_s_63).
*
*        lva_s = lva_s + lva_s_63.
*      WHEN OTHERS.
*        EXIT.
*    ENDCASE.
*    lva_size = lva_total - lva_s.
*  ENDWHILE.
*
**** Pegar o valor do campo 26
*  CLEAR: lva_size, lva_total, lva_id, lva_s_00, lva_v_00, lva_s .
*
*  lva_size = strlen( lva_v_26 ).
*  lva_total = lva_size.
*  lva_s = 2.
*  lva_id = lva_v_26+0(2).
*  WHILE lva_size > 0.
*    CASE lva_id.
*      WHEN '00'.
*        lva_s_00 = lva_v_26+lva_fix(lva_s).
*        lva_s = lva_s + 2.
*        lva_v_00 = lva_v_26+lva_s(lva_s_00).
*        lva_s = lva_s + lva_s_00.
*        lva_id = lva_v_26+lva_s(lva_fix).
*      WHEN '01'.
*        lva_s = lva_s + 2.
*        lva_s_01 = lva_v_26+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_01 = lva_v_26+lva_s(lva_s_01).
*        "w_segmento_j52_pix-url_chave_pix  =  lva_v_01.
*
*        CONCATENATE lva_v_00 lva_v_01 INTO  w_segmento_j52_pix-url_chave_pix.
*
*        CONDENSE w_segmento_j52_pix-url_chave_pix NO-GAPS.
*
*
*        lva_s = lva_s + lva_s_01.
*        lva_id = lva_v_26+lva_s(lva_fix).
*      WHEN '02'.
*        lva_s = lva_s + 2.
*        lva_s_02 = lva_v_26+lva_s(lva_fix).
*        lva_s = lva_s + 2.
*        lva_v_02 = lva_v_26+lva_s(lva_s_02).
*
*        lva_s = lva_s + lva_s_02.
*      WHEN OTHERS.
*        EXIT.
*    ENDCASE.
*    lva_size = lva_total - lva_s.
*  ENDWHILE.
**** Pegar Valor do Campo 62
*  CLEAR: lva_size, lva_total, lva_id, lva_s_05, lva_v_05, lva_s .
*  lva_size = strlen( lva_v_62 ).
*  lva_total = lva_size.
*  lva_s = 2.
*  lva_id = lva_v_62+0(2).
*  WHILE lva_size > 0.
*    CASE lva_id.
*      WHEN '05'.
*        lva_s_05 = lva_v_62+lva_fix(lva_s).
*        lva_s = lva_s + 2.
*        lva_v_05 = lva_v_62+lva_s(lva_s_05).
*        lva_s = lva_s + lva_s_05.
*        w_segmento_j52_pix-cod_qrcode     = lva_v_05.
*      WHEN OTHERS.
*        EXIT.
*    ENDCASE.
*    lva_size = lva_total - lva_s.
*  ENDWHILE.
*** Comentado - CSB - Fim
ENDFORM.
