*&--------------------------------------------------------------------------------&*
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 10.01.2025                                                          &*
*& Descrição: Ajustes de Viagem Rodoviaria - Strada Bank                          &*
*&--------------------------------------------------------------------------------&*
REPORT zlesr0165.

***********************************************************************************
* TABELAS
***********************************************************************************
TABLES: zcte_ciot, zcte_trans, zlest0025, zcarga_cte_tip.

***********************************************************************************
* TYPES
***********************************************************************************
TYPES: BEGIN OF ty_detalhe,
         docnum_dacte  TYPE zcte_ciot-docnum,
         nr_dacte      TYPE char15,
         docnum_danfe  TYPE zcte_info_nota-docnum_nf,
         nr_danfe      TYPE char15,
         peso_saida    TYPE zcte_info_nota-quantidade,
         valor_nf      TYPE zcte_info_nota-vl_nota_fiscal,
         cod_produto   TYPE zcte_info_nota-material,
         descr_produto TYPE makt-maktx.
TYPES: END OF ty_detalhe.

TYPES: BEGIN OF ty_ajustes.
         INCLUDE TYPE zcte_ajustes.
TYPES:   status TYPE icon-id.
TYPES:   go_tela  TYPE icon-id.
TYPES:   reenvia  TYPE icon-id.
TYPES:   valor TYPE zcte_ajustes-vlr_abon_quebra_frete.
TYPES:   deschvid TYPE zdeschvid.
TYPES: END   OF ty_ajustes.

TYPES: BEGIN OF ty_tl_0110,
         novo                  TYPE char01,
         deschvid              TYPE zlest0025-deschvid,
         quebra_frete          TYPE zcte_ajustes-quebra_frete,
         perda_merc            TYPE zcte_ajustes-perda_merc,
         peso_saida            TYPE zcte_ajustes-peso_saida,
         peso_chegada          TYPE zcte_ajustes-peso_chegada,
         peso_abonado          TYPE zcte_ajustes-peso_abonado,
         vlr_abon_quebra_frete TYPE zcte_ajustes-vlr_abon_quebra_frete,
         vlr_abon_perda_merc   TYPE zcte_ajustes-vlr_abon_perda_merc,
         obs_integracao        TYPE zcte_ajustes-obs_integracao,
         obs_ger1              TYPE zcte_ajustes-obs_gerencial,
         obs_ger2              TYPE zcte_ajustes-obs_gerencial,
         obs_ger3              TYPE zcte_ajustes-obs_gerencial.
TYPES: END   OF ty_tl_0110.

***********************************************************************************
* VARIAVEIS
***********************************************************************************
DATA: lv_tela                TYPE char01,
      lv_ponto_coleta        TYPE char10,
      lv_local_entrega       TYPE char10,
      lv_ponto_coleta_desc   TYPE j_1btxjurt-text,
      lv_local_entrega_desc  TYPE j_1btxjurt-text,
      lv_pesotransb          TYPE zlest0039-pesotransb,
      lv_pesochegada         TYPE zlest0039-pesochegada,
      lv_pesosaida           TYPE zlest0039-pesosaida,
      lv_peso_descarga       TYPE zlest0039-pesotransb,
      lv_adiantamento        TYPE zcte_ciot-vlr_adiantamento,
      lv_quebra              TYPE zcte_ciot-vlr_adiantamento,
      lv_perda               TYPE zcte_ciot-vlr_adiantamento,
      lv_seguro              TYPE zcte_ciot-vlr_adiantamento,
      lv_impostos            TYPE zcte_ciot-vlr_adiantamento,
      lv_subtotal            TYPE zcte_ciot-vlr_adiantamento,
      lv_ajustes_pagar       TYPE zcte_ciot-vlr_adiantamento,
      lv_ajustes_descontar   TYPE zcte_ciot-vlr_adiantamento,
      lv_saldo_pagar         TYPE zcte_ciot-vlr_adiantamento,
      lv_nucontrato_ant      TYPE zcte_ciot-nucontrato,
      lv_chvid_ant           TYPE zlest0025-chvid,
      lv_dif_peso            TYPE zlest0039-pesotransb,
      lv_dif_peso2           TYPE zlest0039-pesotransb,
      lv_erro                TYPE char01,
      tl_0110                TYPE ty_tl_0110,
      tl_0110_cop            TYPE ty_tl_0110,
*
      t_detalhe              TYPE TABLE OF ty_detalhe,
      t_ajustes              TYPE TABLE OF ty_ajustes,
      t_zlest0039            TYPE TABLE OF zlest0039,
      t_zlest0025            TYPE TABLE OF zlest0025,
      t_zcte_ajustes         TYPE TABLE OF zcte_ajustes,
      t_zcte_info_nota       TYPE TABLE OF zcte_info_nota,
      t_makt                 TYPE TABLE OF makt,
      w_j_1bnfdoc            TYPE j_1bnfdoc,
      w_detalhe              TYPE ty_detalhe,
      w_ajustes              TYPE ty_ajustes,
      w_zlest0039            TYPE zlest0039,
      w_zlest0025            TYPE zlest0025,
      w_zcte_ajustes         TYPE zcte_ajustes,
      w_grava_ajustes        TYPE zcte_ajustes,
      w_zcte_info_nota       TYPE zcte_info_nota,
      w_makt                 TYPE makt,
*
      t_fieldcat             TYPE lvc_t_fcat,
      w_fieldcat             TYPE lvc_s_fcat,
      t_sort                 TYPE lvc_t_sort,
      w_sort                 TYPE lvc_s_sort,
      t_color                TYPE lvc_t_scol,
      w_color                TYPE lvc_s_scol,
      t_ucomm                TYPE TABLE OF syst_ucomm,
      t_ucomm_0110           TYPE TABLE OF syst_ucomm,
      w_ucomm                TYPE syst_ucomm,
      t_exctab               TYPE slis_t_extab,
      w_exctab               TYPE slis_extab,
      w_layout               TYPE lvc_s_layo,
      w_stable               TYPE lvc_s_stbl    VALUE 'XX',
      t_style                TYPE lvc_t_styl,
      w_style                TYPE lvc_s_styl,
      t_rows                 TYPE lvc_t_row,
      w_rows                 TYPE lvc_s_row,
      lc_viagem              TYPE REF TO zcl_ciot_viagem,
*
      ok_code                TYPE syst_ucomm,
      ok_code_0110           TYPE syst_ucomm,
      g_grid_det             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_det TYPE REF TO cl_gui_custom_container,
      g_grid_aju             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_aju TYPE REF TO cl_gui_custom_container.

***********************************************************************************
* INCLUDES
***********************************************************************************
INCLUDE zlesr0165_status_0100o01.
INCLUDE zlesr0165_status_0110o01.
INCLUDE zlesr0165_user_command_0100i01.
INCLUDE zlesr0165_user_command_0110i01.
INCLUDE zlesr0165_classe.

***********************************************************************************
* START
***********************************************************************************
START-OF-SELECTION.

  lv_tela = 'C'. "Consultar

  CALL SCREEN 0100.

***********************************************************************************
* LIMPAR WORKS
***********************************************************************************
FORM f_clear_dados.

  FREE: lv_ponto_coleta,  lv_local_entrega ,  lv_ponto_coleta_desc, lv_local_entrega_desc,
        lv_adiantamento,  lv_quebra,          lv_perda,             lv_seguro,
        lv_peso_descarga, lv_nucontrato_ant,  lv_chvid_ant,
        lv_impostos,      lv_subtotal,        lv_ajustes_pagar,     lv_ajustes_descontar,
        lv_saldo_pagar,   zcte_ciot,          zcte_trans,           zlest0025,
        t_zcte_info_nota, t_zcte_ajustes,     t_makt,               w_j_1bnfdoc,
        t_detalhe,        t_ajustes,          t_zlest0039,          w_grava_ajustes,
        tl_0110,          tl_0110_cop.

ENDFORM.

***********************************************************************************
* SELECAO DADOS
***********************************************************************************
FORM f_selecao_dados CHANGING p_erro.

  FREE: p_erro.

  SELECT *
    INTO zcte_ciot
    FROM zcte_ciot
      UP TO 1 ROWS
   WHERE nucontrato = zcarga_cte_tip-nucontrato
     AND st_ciot   IN ('5','6')
     AND cancelado  = abap_off.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH TEXT-001 DISPLAY LIKE 'E'.
    p_erro  = abap_true.
    lv_tela = 'C'.
    EXIT.
  ENDIF.

  lv_ponto_coleta  = zcte_ciot-uf_origem && | | && zcte_ciot-municipio_origem.
  lv_local_entrega = zcte_ciot-uf_termin && | | && zcte_ciot-municipio_termin.

  SELECT *
    FROM zcte_ajustes
    INTO TABLE t_zcte_ajustes
   WHERE nucontrato = zcte_ciot-nucontrato.

  SELECT SINGLE text
    INTO lv_ponto_coleta_desc
    FROM j_1btxjurt
   WHERE country    = 'BR'
     AND taxjurcode = lv_ponto_coleta.

  SELECT SINGLE text
    INTO lv_local_entrega_desc
    FROM j_1btxjurt
   WHERE country    = 'BR'
     AND taxjurcode = lv_local_entrega.

  SELECT *
    INTO zcte_trans
    FROM zcte_trans
      UP TO 1 ROWS
   WHERE docnum     = zcte_ciot-docnum
     AND tp_veiculo = '0'.
  ENDSELECT.

  SELECT *
    INTO TABLE t_zcte_info_nota
    FROM zcte_info_nota
   WHERE docnum = zcte_ciot-docnum.

  IF t_zcte_info_nota[] IS NOT INITIAL.
    SELECT *
      INTO TABLE t_makt
      FROM makt
       FOR ALL ENTRIES IN t_zcte_info_nota
     WHERE matnr = t_zcte_info_nota-material
       AND spras = sy-langu.

    SELECT *
      FROM zlest0039
      INTO TABLE t_zlest0039
       FOR ALL ENTRIES IN t_zcte_info_nota
     WHERE docnum = t_zcte_info_nota-docnum_nf.
  ENDIF.

  SELECT SINGLE *
    INTO w_j_1bnfdoc
    FROM j_1bnfdoc
   WHERE docnum = zcte_ciot-docnum.

  SELECT *
    INTO TABLE t_zlest0025
    FROM zlest0025.

ENDFORM.

***********************************************************************************
* MONTAR DADOS
***********************************************************************************
FORM f_montar_dados.

  FREE: t_detalhe, t_ajustes, lv_pesotransb, lv_pesochegada, lv_pesosaida.

  LOOP AT t_zcte_info_nota INTO w_zcte_info_nota.

    FREE: w_detalhe, w_makt.
    READ TABLE t_makt      INTO w_makt WITH KEY matnr = w_zcte_info_nota-material.

    w_detalhe-docnum_dacte    = zcte_ciot-docnum.
    w_detalhe-nr_dacte        = w_j_1bnfdoc-nfenum      && '-' && w_j_1bnfdoc-series.
    w_detalhe-docnum_danfe    = w_zcte_info_nota-docnum_nf.
    w_detalhe-nr_danfe        = w_zcte_info_nota-numero && '-' && w_zcte_info_nota-serie.
    w_detalhe-peso_saida      = w_zcte_info_nota-quantidade.
    w_detalhe-valor_nf        = w_zcte_info_nota-vl_nota_fiscal.
    w_detalhe-cod_produto     = w_zcte_info_nota-material.
    w_detalhe-descr_produto   = w_makt-maktx.
    lv_pesosaida              = lv_pesosaida + w_zcte_info_nota-quantidade.

    APPEND w_detalhe         TO t_detalhe.
  ENDLOOP.

  LOOP AT t_zlest0039 INTO w_zlest0039.
    lv_pesotransb        = lv_pesotransb  + w_zlest0039-pesotransb.
    lv_pesochegada       = lv_pesochegada + w_zlest0039-pesochegada.
  ENDLOOP.

  lv_peso_descarga = COND #( WHEN lv_pesotransb IS NOT INITIAL THEN lv_pesotransb
                                                               ELSE lv_pesochegada ).

  PERFORM f_calcula_frete   USING lv_peso_descarga.

  LOOP AT t_zcte_ajustes             INTO w_zcte_ajustes.
    FREE: w_ajustes, w_zlest0025.
    READ TABLE t_zlest0025           INTO w_zlest0025 WITH KEY chvid = w_zcte_ajustes-chvid.

    MOVE-CORRESPONDING w_zcte_ajustes  TO w_ajustes.

    w_ajustes-status                    = COND #( WHEN w_zcte_ajustes-status_integracao = 'S' THEN icon_system_okay
                                                                                              ELSE icon_message_error_small ).
    w_ajustes-go_tela                   = icon_display_more.
    w_ajustes-reenvia                   = icon_transfer.
    w_ajustes-nucontrato                = w_zcte_ajustes-nucontrato.
    w_ajustes-chvid                     = w_zcte_ajustes-chvid.
    w_ajustes-deschvid                  = w_zlest0025-deschvid.
    w_ajustes-valor                     = w_zcte_ajustes-vlr_abon_quebra_frete + w_zcte_ajustes-vlr_abon_perda_merc.
    w_ajustes-vlr_abon_quebra_frete     = w_zcte_ajustes-vlr_abon_quebra_frete.
    w_ajustes-vlr_abon_perda_merc       = w_zcte_ajustes-vlr_abon_perda_merc.
    w_ajustes-msg_integracao            = w_zcte_ajustes-msg_integracao.
    APPEND w_ajustes                   TO t_ajustes.
  ENDLOOP.

ENDFORM.

***********************************************************************************
* CALCULA_PESO_DESCARGA
***********************************************************************************
FORM f_calcula_frete USING p_peso_descarga.

  DATA: lv_quant_toler TYPE zcte_ciot-quantidade,
        lv_quant_desc  TYPE zcte_ciot-quantidade.

  lv_adiantamento      = zcte_ciot-vlr_adiantamento.
  lv_seguro            = zcte_ciot-vlr_seguro + zcte_ciot-vlr_iof.
  lv_impostos          = zcte_ciot-vlr_inss   + zcte_ciot-vlr_iss + zcte_ciot-vlr_sest + zcte_ciot-vlr_irpf.

  IF p_peso_descarga IS INITIAL.
    CLEAR: lv_quebra, lv_perda, lv_ajustes_pagar, lv_ajustes_descontar.
    lv_subtotal        = zcte_ciot-vlr_frete  - ( lv_adiantamento + lv_quebra + lv_perda + lv_seguro + lv_impostos ).
    EXIT.
  ENDIF.

  lv_dif_peso  = ( zcte_ciot-quantidade - p_peso_descarga ).
  lv_dif_peso2 = abs( lv_dif_peso ).

  IF lv_dif_peso > 0.
    IF zcte_ciot-unid_vlr_frete = 'TO'.
      lv_quebra = zcte_ciot-vlr_frete - ( p_peso_descarga * zcte_ciot-vlr_unit_frete / 1000 ).
    ELSE.
      lv_quebra = zcte_ciot-vlr_frete - ( p_peso_descarga * zcte_ciot-vlr_unit_frete ).
    ENDIF.
  ELSE.
    lv_quebra = 0.
  ENDIF.

  IF zcte_ciot-perc_tolerancia <> 0.
    lv_quant_toler = round( val = zcte_ciot-quantidade * zcte_ciot-perc_tolerancia  / 100 prec = 8 ) .
    lv_quant_desc  = lv_dif_peso -  lv_quant_toler.

    IF lv_quant_desc > 0.
      lv_perda = lv_quant_desc * zcte_ciot-vlr_unit_merc.
    ELSE.
      lv_perda = 0.
    ENDIF.
  ENDIF.

  lv_subtotal          = zcte_ciot-vlr_frete  - ( lv_adiantamento + lv_quebra + lv_perda + lv_seguro + lv_impostos ).
  lv_ajustes_pagar     = zcte_ciot-vlr_frete.
  lv_ajustes_descontar = lv_adiantamento      + lv_quebra + lv_perda + lv_seguro + lv_impostos.

ENDFORM.

***********************************************************************************
* CHECK CONTRATO
***********************************************************************************
FORM f_check_contrato.

* IF lv_tela = 'N'.
*   PERFORM f_lock_contrato USING 'B' zcarga_cte_tip-nucontrato.
* ELSE.
*   PERFORM f_lock_contrato USING 'D' zcarga_cte_tip-nucontrato.
* ENDIF.

  IF lv_nucontrato_ant <> zcarga_cte_tip-nucontrato.
    lv_tela = 'C'.
    PERFORM f_clear_dados.
    PERFORM f_selecao_dados CHANGING lv_erro.
    IF lv_erro = abap_false.
      PERFORM f_montar_dados.
      lv_nucontrato_ant = zcarga_cte_tip-nucontrato.
    ENDIF.
  ENDIF.

ENDFORM.

***********************************************************************************
* CHECK AJUSTE
***********************************************************************************
FORM f_check_ajuste.

  CHECK zlest0025-chvid IS NOT INITIAL.

  IF lv_chvid_ant <> zlest0025-chvid.
    SELECT SINGLE chvid
      INTO @DATA(_chvid)
      FROM zlest0025
     WHERE chvid       = @zlest0025-chvid
       AND ctlgchavid  = 'AP'
       AND bl          = '2'.

    IF sy-subrc <> 0.
      MESSAGE e024(sd) WITH 'Parametro para Ajuste Incorreto!'. " DISPLAY LIKE 'E'.
    ENDIF.

    READ TABLE t_zcte_ajustes INTO w_zcte_ajustes WITH KEY chvid = zlest0025-chvid.
    IF sy-subrc = 0.
      MESSAGE e024(sd) WITH 'Parametro ja utilizado para este Contrato!'. " DISPLAY LIKE 'E'.
    ENDIF.

    PERFORM f_executa_tela USING zlest0025-chvid.
    lv_chvid_ant = zlest0025-chvid.
  ENDIF.

ENDFORM.

***********************************************************************************
* CHECK PESO DESCARGA
***********************************************************************************
FORM f_check_peso_descarga.

  IF lv_tela = 'C'.
    CLEAR lv_peso_descarga..
  ENDIF.

  PERFORM f_calcula_frete   USING lv_peso_descarga.

ENDFORM.

***********************************************************************************
* chamar tela expscifica
***********************************************************************************
FORM f_executa_tela USING p_chvid.

  IF p_chvid IS INITIAL.
    MESSAGE s024(sd) WITH 'Informar o Tipo de Ajuste!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CASE p_chvid.
    WHEN '13'.
      PERFORM f_monta_tela_0110 USING p_chvid.
      CALL SCREEN 0110 STARTING AT 40  03
                         ENDING AT 153 15.
*JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
    WHEN OTHERS.
      IF sy-uname = 'JTASSONI'.
        PERFORM f_monta_tela_0110 USING p_chvid.
        CALL SCREEN 0110 STARTING AT 40  03
                           ENDING AT 153 15.
      ENDIF.
*JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ

  ENDCASE.

ENDFORM.

***********************************************************************************
* validar dados
***********************************************************************************
FORM f_validar_dados    USING p_chvid
                     CHANGING p_erro.

  FREE: p_erro.

  CASE p_chvid.
    WHEN '13'.
      PERFORM f_valida_tela_0110 CHANGING p_erro.

  ENDCASE.

ENDFORM.

***********************************************************************************
* Gravar dados
***********************************************************************************
FORM f_gravar_dados CHANGING p_erro.

  FREE: p_erro.

  IF w_grava_ajustes IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não há informações a Salvar!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MODIFY zcte_ajustes FROM w_grava_ajustes.

  COMMIT WORK AND WAIT.

*---------------------------
*-enviar API
*---------------------------
  PERFORM f_enviar_dados_tip USING w_grava_ajustes-nucontrato
                                   w_grava_ajustes-chvid.

ENDFORM.

***********************************************************************************
* enviar dados a TIP
***********************************************************************************
FORM f_enviar_dados_tip USING p_nucontrato
                              p_chvid..

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Enviando Informações...'.

  CREATE OBJECT lc_viagem.

  lc_viagem->ajustar_viagem( EXPORTING i_nucontrato = p_nucontrato
                                       i_chvid      = p_chvid ).

ENDFORM.

***********************************************************************************
* montar tela 0110
***********************************************************************************
FORM f_monta_tela_0110 USING p_chvid.

  CLEAR: w_zcte_ajustes, w_zlest0025.

  READ TABLE t_zlest0025       INTO w_zlest0025    WITH KEY chvid = p_chvid.
  READ TABLE t_zcte_ajustes    INTO w_zcte_ajustes WITH KEY chvid = p_chvid.

  IF sy-subrc = 0.
    tl_0110-novo                  = abap_false.
    tl_0110-deschvid              = w_zlest0025-deschvid.
    tl_0110-quebra_frete          = w_zcte_ajustes-quebra_frete.
    tl_0110-perda_merc            = w_zcte_ajustes-perda_merc.
    tl_0110-peso_saida            = w_zcte_ajustes-peso_saida.
    tl_0110-peso_chegada          = w_zcte_ajustes-peso_chegada.
    tl_0110-peso_abonado          = w_zcte_ajustes-peso_abonado.
    tl_0110-vlr_abon_quebra_frete = w_zcte_ajustes-vlr_abon_quebra_frete.
    tl_0110-vlr_abon_perda_merc   = w_zcte_ajustes-vlr_abon_perda_merc.
    tl_0110-obs_integracao        = w_zcte_ajustes-obs_integracao.
    SPLIT w_zcte_ajustes-obs_gerencial AT '|' INTO tl_0110-obs_ger1 tl_0110-obs_ger2 tl_0110-obs_ger3.
    EXIT.
  ENDIF.

*-cadastro novo ---------------------
  tl_0110                         = tl_0110_cop.

  IF tl_0110 IS NOT INITIAL.
    tl_0110-peso_chegada          = lv_peso_descarga.
    EXIT.
  ENDIF.

  tl_0110-novo                    = abap_true.
  tl_0110-deschvid                = w_zlest0025-deschvid.
  tl_0110-peso_saida              = lv_pesosaida.
  tl_0110-peso_chegada            = lv_peso_descarga.
  tl_0110-peso_abonado            = lv_dif_peso2.
  tl_0110-vlr_abon_quebra_frete   = lv_quebra. "tl_0110-peso_abonado * zcte_ciot-vlr_unit_frete.
  tl_0110-vlr_abon_perda_merc     = lv_perda.  "tl_0110-peso_abonado * zcte_ciot-vlr_unit_merc.

ENDFORM.

***********************************************************************************
* VALIDA TELA 110
***********************************************************************************
FORM f_valida_tela_0110  CHANGING p_erro.

  DATA: lv_bonif_quebra_frete TYPE zcte_ajustes-vlr_abon_quebra_frete,
        lv_bonif_perda_merc   TYPE zcte_ajustes-vlr_abon_perda_merc.

  p_erro = abap_false.

  lv_bonif_quebra_frete  = lv_dif_peso2 * zcte_ciot-vlr_unit_frete.
  lv_bonif_perda_merc    = lv_dif_peso2 * zcte_ciot-vlr_unit_merc.

  IF ( tl_0110-obs_integracao IS INITIAL ) OR ( strlen( tl_0110-obs_integracao ) < 10 ).
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar a Observação da Integração!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF tl_0110-peso_abonado > lv_dif_peso2.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Peso Abonado superior à diferença de Peso!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF tl_0110-vlr_abon_quebra_frete > lv_bonif_quebra_frete.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Vlr.Abono Quebra Frete superior ' 'ao valor Calculado!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF tl_0110-vlr_abon_perda_merc > lv_bonif_perda_merc.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Vlr.Abono Perda Mercadoria superior ' 'ao valor Calculado!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

***********************************************************************************
* CONFIRMA TELA 110
***********************************************************************************
FORM f_confirma_tela_0110.

  CHECK tl_0110-novo = abap_true.

  w_grava_ajustes-mandt                 = sy-mandt.
  w_grava_ajustes-nucontrato            = zcarga_cte_tip-nucontrato.
  w_grava_ajustes-chvid                 = zlest0025-chvid.
  w_grava_ajustes-quebra_frete          = tl_0110-quebra_frete.
  w_grava_ajustes-perda_merc            = tl_0110-perda_merc.
  w_grava_ajustes-peso_saida            = tl_0110-peso_saida.
  w_grava_ajustes-peso_chegada          = tl_0110-peso_chegada.
  w_grava_ajustes-peso_abonado          = tl_0110-peso_abonado.
  w_grava_ajustes-vlr_abon_quebra_frete = tl_0110-vlr_abon_quebra_frete.
  w_grava_ajustes-vlr_abon_perda_merc   = tl_0110-vlr_abon_perda_merc.
  w_grava_ajustes-obs_integracao        = tl_0110-obs_integracao.
  w_grava_ajustes-obs_gerencial         = tl_0110-obs_ger1 && '|' && tl_0110-obs_ger2 && '|' && tl_0110-obs_ger3.
  w_grava_ajustes-usuario               = sy-uname.
  w_grava_ajustes-data_registro         = sy-datum.
  w_grava_ajustes-hora_registro         = sy-uzeit.

ENDFORM.

***********************************************************************************
* lock dados
***********************************************************************************
FORM f_lock_contrato USING p_status
                           p_nucontrato.

  CASE p_status.
    WHEN 'B'. "Bloqueio
      CALL FUNCTION 'ENQUEUE_EZCTE_AJUSTES'
        EXPORTING
          nucontrato     = p_nucontrato
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        lv_tela = 'C'.
        MESSAGE e024(sd) WITH |Contrato bloqueado por { sy-msgv1 }!|.
      ENDIF.

    WHEN 'D'. "Desbloqueio
      CALL FUNCTION 'DEQUEUE_EZCTE_AJUSTES'
        EXPORTING
          nucontrato = p_nucontrato.
  ENDCASE.

ENDFORM.

***********************************************************************************
* INICIA ALV
***********************************************************************************
FORM f_init_alv.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_true.
* w_layout-stylefname   = 'CELLSTYLES'.
* w_layout-ctab_fname   = 'CELLCOLOR'.
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-no_rowmark   = abap_true.

  IF g_grid_det IS INITIAL.
    PERFORM f_fieldcatalog USING 'DETALHE'.

    CREATE OBJECT g_custom_container_det
      EXPORTING
        container_name = 'CC_DETALHAMENTO'.

    CREATE OBJECT g_grid_det
      EXPORTING
        i_parent          = g_custom_container_det
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

*    CREATE OBJECT g_custom_container_det
*      EXPORTING
*        container_name = 'CC_DETALHAMENTO'.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_det,
                 lcl_event_handler=>on_data_changed  FOR g_grid_det,
                 lcl_event_handler=>user_command     FOR g_grid_det,
                 lcl_event_handler=>toolbar          FOR g_grid_det.

    CALL METHOD g_grid_det->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_detalhe
*       it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_det->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF g_grid_aju IS INITIAL.
    PERFORM f_fieldcatalog USING 'AJUSTE'.

    CREATE OBJECT g_custom_container_aju
      EXPORTING
        container_name = 'CC_AJUSTES_REALIZADOS'.

    CREATE OBJECT g_grid_aju
      EXPORTING
        i_parent          = g_custom_container_aju
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_aju,
                 lcl_event_handler=>on_data_changed  FOR g_grid_aju,
                 lcl_event_handler=>user_command     FOR g_grid_aju,
                 lcl_event_handler=>toolbar          FOR g_grid_aju.

    CALL METHOD g_grid_aju->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_ajustes
*       it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_aju->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog USING p_tipo.

  FREE t_fieldcat.

  CASE p_tipo.
    WHEN 'DETALHE'.
      PERFORM f_estrutura_alv USING:
        01  'J_1BNFDOC'  'DOCNUM'   'T_DETALHE' 'DOCNUM_DACTE'           'Docnum Dacte'         '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        02  ''           ''         'T_DETALHE' 'NR_DACTE'               'Nr.Dacte'             '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  'J_1BNFDOC'  'DOCNUM'   'T_DETALHE' 'DOCNUM_DANFE'           'Docnum Danfe'         '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''           ''         'T_DETALHE' 'NR_DANFE'               'Nr.Danfe'             '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''           ''         'T_DETALHE' 'PESO_SAIDA'             'Peso Saida(Kg)'       '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''           ''         'T_DETALHE' 'VALOR_NF'               'Valor NF'             '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  'MARA'       'MATNR'    'T_DETALHE' 'COD_PRODUTO'            'Cód.Produto'          '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''           ''         'T_DETALHE' 'DESCR_PRODUTO'          'Descr.Produto'        '65'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN 'AJUSTE'.
      PERFORM f_estrutura_alv USING:
        01  ''           ''         'T_AJUSTES' 'STATUS'                 'Status'               '06'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''           ''         'T_AJUSTES' 'GO_TELA'                'Detalhe'              '07'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        03  ''           ''         'T_AJUSTES' 'REENVIA'                'Reenvia'              '07'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        04  ''           ''         'T_AJUSTES' 'DESCHVID'               'Ajuste'               '11'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''           ''         'T_AJUSTES' 'VALOR'                  'Valor'                '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.
*       06  ''           ''         'T_AJUSTES' 'MSG_INTEGRACAO'         'Mesg.Integracao'      '80'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

  ENDCASE.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

***********************************************************************************
***********************************************************************************
