*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  PERFORM f_get_set.

  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* obter SETs
**********************************************************************
FORM f_get_set.

  DATA: valor         TYPE p DECIMALS 2,
        text_out(255) TYPE c.

  FREE: r_matkl_sel.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_tvarvc)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_tvarvc  INTO DATA(w_tvarvc).
    r_matkl_sel-sign   = w_tvarvc-sign.
    r_matkl_sel-option = w_tvarvc-opti.
    r_matkl_sel-low    = w_tvarvc-low.
    APPEND r_matkl_sel.
  ENDLOOP.


  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'ZSDR0108_DT_FATURA'
      class         = '0000'
    TABLES
      set_values    = it_value
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  CHECK it_value IS NOT INITIAL.

  CLEAR r_dt_fatura.

  LOOP AT it_value.
    r_dt_fatura-sign   = 'I'.
    r_dt_fatura-option = 'GE'.
    r_dt_fatura-low    = it_value-from.
    APPEND r_dt_fatura.
  ENDLOOP.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'MAGGI_ZSDT0158_VLR'
      class         = '0000'
    TABLES
      set_values    = it_value_01
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  CHECK it_value_01 IS NOT INITIAL.

  CLEAR: r_vlr_brl, v_vlr_brl.

  LOOP AT it_value_01.
    r_vlr_brl-sign    = 'I'.
    r_vlr_brl-option  = 'EQ'.

    text_out = it_value_01-from.
    DO 5  TIMES.
      REPLACE  ',00' WITH ' ' INTO text_out.
    ENDDO.

    CONDENSE text_out NO-GAPS.
    MOVE text_out TO valor.
    r_vlr_brl-low = valor.
    v_vlr_brl = r_vlr_brl-low.
    CONDENSE v_vlr_brl NO-GAPS.
    APPEND r_vlr_brl.
  ENDLOOP.

ENDFORM.

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados    USING p_lote
                     CHANGING p_erro.

  FREE: t_saida, t_zsdt0306, t_zsdt0225, r_dtfat, p_erro.

*------------------------------
* para procurar nao faturados
*------------------------------
  r_dtfat-sign   = 'I'.
  r_dtfat-option = 'EQ'.
  r_dtfat-low    = '00000000'.
  APPEND r_dtfat.

  IF   s_lote[]  IS INITIAL AND
     ( s_bukrs[] IS INITIAL ).
    p_erro = abap_true.
    MESSAGE s024(sd) WITH TEXT-300 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF p_nfatu = abap_on.
    SELECT *
      FROM zsdt0306
      INTO TABLE t_zsdt0306
     WHERE id_seq        IN s_lote
       AND ebeln         IN s_ebeln
       AND bukrs         IN s_bukrs
       AND werks         IN s_werks
       AND data_registro IN s_data.

    CHECK t_zsdt0306[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0225
      INTO TABLE t_zsdt0225
       FOR ALL ENTRIES IN t_zsdt0306
     WHERE id_seq     = t_zsdt0306-id_seq
       AND dt_fatura IN r_dtfat
       AND nr_ov      = abap_off.
  ELSE.
    SELECT *
      FROM zsdt0306
      INTO TABLE t_zsdt0306
     WHERE id_seq IN s_lote
       AND ebeln  IN s_ebeln
       AND bukrs  IN s_bukrs
       AND werks  IN s_werks.

    CHECK t_zsdt0306[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0225
      INTO TABLE t_zsdt0225
       FOR ALL ENTRIES IN t_zsdt0306
     WHERE id_seq     = t_zsdt0306-id_seq
       AND dt_fatura IN s_data
       AND nr_ov     <> abap_off.
  ENDIF.

  CHECK t_zsdt0225[] IS NOT INITIAL.

  SELECT *
    FROM mara
    INTO TABLE t_mara
     FOR ALL ENTRIES IN t_zsdt0225
   WHERE matnr = t_zsdt0225-cod_material.

  SELECT *
    FROM makt
    INTO TABLE t_makt
     FOR ALL ENTRIES IN t_zsdt0225
   WHERE matnr = t_zsdt0225-cod_material
     AND spras = sy-langu.

  SORT t_mara BY matnr.
  SORT t_makt BY matnr.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados USING p_sort.

  DATA: l_vlr_brl TYPE zsdt0225-vlr_brl,
        l_vlr_usd TYPE zsdt0225-vlr_usd.

  FREE: t_saida.

  LOOP AT t_zsdt0306 INTO w_zsdt0306.

    CLEAR: w_saida, w_mara, w_makt, l_vlr_brl,  l_vlr_usd.

    LOOP AT t_zsdt0225 INTO w_zsdt0225 WHERE id_seq       = w_zsdt0306-id_seq.
*                                        AND bukrs        = w_zsdt0306-bukrs
*                                        AND werks        = w_zsdt0306-werks
*                                        AND cod_material = w_zsdt0306-matnr.
      l_vlr_brl = l_vlr_brl + w_zsdt0225-vlr_brl.
      l_vlr_usd = l_vlr_usd + w_zsdt0225-vlr_usd.
    ENDLOOP.

    READ TABLE t_zsdt0225 INTO w_zsdt0225 WITH KEY id_seq       = w_zsdt0306-id_seq.
*                                                  bukrs        = w_zsdt0306-bukrs_fat
*                                                  werks        = w_zsdt0306-werks_fat
*                                                  cod_material = w_zsdt0306-matnr.
    CHECK sy-subrc = 0.

    READ TABLE t_mara INTO w_mara WITH KEY matnr = w_zsdt0225-cod_material
                                  BINARY SEARCH.
    READ TABLE t_makt INTO w_makt WITH KEY matnr = w_zsdt0225-cod_material
                                  BINARY SEARCH.

    CHECK w_mara-matkl IN r_matkl_sel[].

    w_saida-id_seq        = w_zsdt0225-id_seq.
    w_saida-bukrs         = w_zsdt0306-bukrs.
    w_saida-bukrs_fat     = w_zsdt0306-bukrs_fat.
    w_saida-werks         = w_zsdt0306-werks.
    w_saida-matnr         = w_zsdt0225-cod_material.
    w_saida-maktx         = w_makt-maktx.
    w_saida-matkl         = w_mara-matkl.
    w_saida-cl_codigo     = w_zsdt0225-cl_codigo.
    w_saida-kunnr         = w_zsdt0225-cl_codigo.
    w_saida-kunnr_ori     = w_zsdt0225-cl_codigo.
    w_saida-emp_fat_serv  = w_zsdt0225-bukrs_serv.
    w_saida-dt_fatura     = w_zsdt0225-dt_fatura.
    w_saida-auart         = w_zsdt0225-auart.
    w_saida-waerk         = w_zsdt0225-waerk.
    w_saida-netpr         = w_zsdt0225-netpr.
    w_saida-tax_dolar     = w_zsdt0225-tax_dolar.
    w_saida-vlr_usd       = COND #( WHEN w_zsdt0306-vlr_usd IS INITIAL THEN l_vlr_usd
                                                                       ELSE w_zsdt0306-vlr_usd ).
    w_saida-vlr_brl       = COND #( WHEN w_zsdt0306-vlr_brl IS INITIAL THEN l_vlr_brl
                                                                       ELSE w_zsdt0306-vlr_brl ).
    w_saida-waerk_fatura  = w_zsdt0225-waerk_fatura.
    w_saida-nr_ov         = w_zsdt0225-nr_ov.
    w_saida-fatura        = w_zsdt0225-fatura.
    w_saida-docnum        = w_zsdt0225-docnum.
    w_saida-navio         = w_zsdt0225-navio.
    w_saida-local_operacao = w_zsdt0225-local_operacao.
    w_saida-ebeln         = w_zsdt0306-ebeln.
    w_saida-ebelp         = w_zsdt0306-ebelp.
    w_saida-menge         = w_zsdt0306-quantidade.

    IF w_saida-fatura IS INITIAL AND w_saida-docnum IS INITIAL AND w_saida-nr_ov IS INITIAL.
      w_saida-status      = icon_led_yellow.
      t_style             = VALUE #( ( fieldname = 'NAVIO'          style = cl_gui_alv_grid=>mc_style_enabled  )
                                     ( fieldname = 'LOCAL_OPERACAO' style = cl_gui_alv_grid=>mc_style_enabled  )
                                     ( fieldname = 'DT_FATURA'      style = cl_gui_alv_grid=>mc_style_enabled  )
                                     ( fieldname = 'AUART'          style = cl_gui_alv_grid=>mc_style_enabled  )
                                     ( fieldname = 'WAERK'          style = cl_gui_alv_grid=>mc_style_enabled  ) ).
    ELSE.
      w_saida-status      = icon_led_green.
      t_style             = VALUE #( ( fieldname = 'NAVIO'          style = cl_gui_alv_grid=>mc_style_disabled  )
                                     ( fieldname = 'LOCAL_OPERACAO' style = cl_gui_alv_grid=>mc_style_disabled  )
                                     ( fieldname = 'DT_FATURA'      style = cl_gui_alv_grid=>mc_style_disabled )
                                     ( fieldname = 'AUART'          style = cl_gui_alv_grid=>mc_style_disabled )
                                     ( fieldname = 'WAERK'          style = cl_gui_alv_grid=>mc_style_disabled ) ).
    ENDIF.

    w_saida-celltab[]     = t_style[].

    APPEND w_saida       TO t_saida.
  ENDLOOP.

ENDFORM.

**********************************************************************
* gerar OV
**********************************************************************
FORM f_gerar_ov.

  DATA: wa_headerdata   TYPE bapisdhd1,
        wa_itemdata     TYPE bapisditm,
        wa_condition    TYPE bapicond,
        wa_partner      TYPE bapiparnr,
        wa_return       TYPE bapiret2,
        vbeln_ov        TYPE bapivbeln-vbeln,
        vbeln_fatura    TYPE c LENGTH 10,
        wa_j_1bnflin    TYPE j_1bnflin,
        wl_lifnr        TYPE lfa1-lifnr,
        l_itm_number    TYPE posnr_va,
        t_success       TYPE TABLE OF bapivbrksuccess,
        t_billing       TYPE TABLE OF bapivbrk,
        t_return        TYPE TABLE OF bapireturn1,
        vl_lifnr        TYPE lfa1-lifnr,
        vlr_icms        TYPE zlest0061-vlr_usd,
        vlr_pis         TYPE zlest0061-vlr_usd,
        vlr_cofins      TYPE zlest0061-vlr_usd,
        vlr_iss         TYPE zlest0061-vlr_usd,
        vlr_liquido     TYPE zlest0061-vlr_usd,
        vl_validto      TYPE j_1btxiss-validto,
        vl_validfrom    TYPE j_1btxiss-validfrom,
        vl_data         TYPE c LENGTH 10,
        l_id_seq        TYPE zsdt0225-id_seq,
        qtd_rows        TYPE sy-tabix,
        l_vlr_brl       TYPE zsdt0225-vlr_brl,
        l_vlr_usd       TYPE zsdt0225-vlr_usd,
        vpurch_no_s     TYPE char35,
        wg_mensagem(30),
        wl_erro(1),
        msg(150),
        _dia            TYPE i,
        r_auart_ztrg    TYPE RANGE OF auart.

*  DATA: lc_dados   TYPE zsde0183,    "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        lc_retorno TYPE zsdt0370_t,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        wc_retorno TYPE zsdt0370.    "*-CS2025000025-#164218-27.01.2025-JT-inicio

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  DESCRIBE TABLE t_rows LINES qtd_rows.

  IF     qtd_rows = 0.
    MESSAGE s024(sd) WITH 'Selecionar uma Linha!' DISPLAY LIKE 'W'.
    EXIT.
  ELSEIF qtd_rows > 1.
    MESSAGE s024(sd) WITH 'Selecionar apenas uma Linha para gerar OV!' DISPLAY LIKE 'W'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Gerando Ordem de Venda / Fatura...'.

  CLEAR: w_zsdt0225, w_zsdt0307.

  FREE: wl_header_in,
        wl_header_inx,
        it_itemdata,
        it_items_inx,
        tl_schedules_in,
        it_condition,
        it_partner,
        it_return.

*---------------------------------
*- validacoes
*---------------------------------
  READ TABLE t_rows  INTO w_rows  INDEX 1.
  READ TABLE t_saida INTO w_saida INDEX w_rows-index.

  l_id_seq = w_saida-id_seq.

  IF w_saida-waerk_fatura IS INITIAL.
    MESSAGE s024(sd) WITH 'Moeda Fatura não encontrada!'
                          ' Parametrizar na transação ZLES0075.' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_saida-navio IS INITIAL.
    MESSAGE s024(sd) WITH 'Informar o Navio!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_saida-local_operacao IS INITIAL.
    MESSAGE s024(sd) WITH 'Informar o Local de Operação!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0225
    INTO @DATA(wa_0225)
   WHERE id_seq   EQ @w_saida-id_seq
     AND bukrs    EQ @w_saida-bukrs
*    AND werks    EQ @w_saida-werks
     AND origem   EQ 'PI'
     AND nr_ov    NE ' '.

  IF sy-subrc = 0.
    MESSAGE s024(sd) WITH 'O.V de faturamento já gerada' 'Atualizar dados da tela!'  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0307
    INTO w_zsdt0307
   WHERE emp_pedido   = w_saida-bukrs
     AND emp_fat_serv = w_saida-emp_fat_serv.

  IF sy-subrc = 0.
    wl_lifnr = |{ w_zsdt0307-centro_fat_serv ALPHA = IN }|.
  ENDIF.

  FREE: r_matkl.
  t_matkl-sign   = 'I'.
  t_matkl-option = 'CP'.
  t_matkl-low    = '*' && w_saida-matkl && '*'.
  APPEND t_matkl TO r_matkl.

  SELECT *
    FROM zlest0055
    INTO TABLE @DATA(it_zlest0055)
   WHERE kunnr    EQ @w_saida-kunnr
     AND auart    EQ @w_saida-auart
     AND matkl    IN @r_matkl
     AND dt_fim   >= @w_saida-dt_fatura
     AND waerk    >= @w_saida-waerk
     AND status   EQ '1'
     AND vkorg    EQ @w_zsdt0307-emp_fat_serv.

  READ TABLE it_zlest0055 INTO DATA(wa_zlest0055) INDEX 1.

  LOOP AT t_saida  INTO w_saida WHERE id_seq = l_id_seq.
    w_saida-vkorg     = wa_zlest0055-vkorg.
    w_saida-vtweg     = wa_zlest0055-vtweg.
    w_saida-spart     = wa_zlest0055-spart.
    w_saida-netpr     = wa_zlest0055-netpr.
    MODIFY t_saida FROM w_saida INDEX sy-tabix.
  ENDLOOP.

  CLEAR _dia.

  _dia = w_saida-dt_fatura+6(2).
  IF ( _dia >= 01 ) AND  (  _dia <= 15 ).
    w_saida-zterm    = wa_zlest0055-zterm.
  ELSEIF ( _dia >= 16 ) AND  (  _dia <= 31 ).
    w_saida-zterm    = wa_zlest0055-zterm2.
  ENDIF.

  SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1) WHERE lifnr EQ @wl_lifnr.
  SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1) WHERE kunnr EQ @w_saida-cl_codigo.

*----------------------
* pedido recebedor mercadoria
*----------------------
  vpurch_no_s              = w_saida-id_seq.

*----------------------
* monta cabecalho
*----------------------
  wl_header_in-sales_org   = w_saida-vkorg. "(org. de venda)
  wl_header_in-distr_chan  = w_saida-vtweg. "(canal distr.)
  wl_header_in-currency    = w_saida-waerk_fatura. "(moeda.) " gw_saida_ger_ov-waerk 12.12.16
  wl_header_in-pymt_meth   = 'P'.
  wl_header_in-division    = w_saida-spart. "(setor atividade)
  wl_header_in-doc_type    = w_saida-auart. "(tipo de ordem)
  wl_header_in-pmnttrms    = w_saida-zterm. "ZTERM.
  wl_header_in-exrate_fi   = w_saida-tax_dolar."(taxa dolar)
  wl_header_in-bill_date   = w_saida-dt_fatura.
  wl_header_in-purch_no_c  = vpurch_no_s. "'.'.
  wl_header_in-purch_no_s  = vpurch_no_s.
  wl_header_in-fix_val_dy  = w_saida-dt_fatura. "VALDT Data efetiva fixa
  wl_header_in-pymt_meth   = ''. "ZLSCH. Forma de pagamento
  wl_header_in-dlvschduse  = wa_zlest0055-vkaus. "VKAUS. Código de utilização
  wl_header_in-incoterms1  = 'SRV'.
  wl_header_in-incoterms2  = 'Serviço'.

*----------------------
* Set utilizado por odens ZTRG criadas pela transação ZSDT0158
*----------------------
  SELECT *
    FROM setleaf
    INTO TABLE @DATA(it_set_ztrg)
   WHERE setname = 'MAGGI_VF01_TP_OV'.

  LOOP AT it_set_ztrg INTO DATA(wa_set_ztrg).
    APPEND VALUE rsdsselopt( option = wa_set_ztrg-valoption  sign = wa_set_ztrg-valsign low = wa_set_ztrg-valfrom )
              TO r_auart_ztrg.
  ENDLOOP.

  IF w_saida-auart IN r_auart_ztrg.
    wl_header_in-req_date_h = w_saida-dt_fatura.
    wl_header_in-price_date  =  w_saida-dt_fatura.
  ENDIF.

*------------------------------------------------
* itens da OV
*------------------------------------------------
  FREE: t_saida_ov,
        l_pedidos,
        l_erro.
*       lc_dados.

  LOOP AT t_saida INTO w_saida WHERE id_seq = l_id_seq.

    l_tabix = sy-tabix.

    SELECT SINGLE *
      FROM zlest0059
      INTO @DATA(wl_zlest0059)
     WHERE bukrs       EQ @w_saida-emp_fat_serv
       AND auart       EQ @w_saida-auart
       AND po_embarque EQ @abap_off
       AND po_destino  EQ @abap_off.

    IF sy-subrc <> 0.
      l_erro = abap_true.
      MESSAGE s024(sd) WITH 'Material não encontrado para Tipo Venda'  DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      w_saida-matnr_ov     = wl_zlest0059-matnr.
    ENDIF.

    MODIFY t_saida      FROM w_saida INDEX l_tabix.

    w_saida_ov-id_seq      = w_saida-id_seq.
    w_saida_ov-matnr_ov    = w_saida-matnr_ov.
    w_saida_ov-menge       = w_saida-menge.
    w_saida_ov-vlr_brl     = w_saida-vlr_brl.
    w_saida_ov-vlr_usd     = w_saida-vlr_usd.
    COLLECT w_saida_ov  INTO t_saida_ov.
  ENDLOOP.

  CHECK l_erro = abap_false.

  t_saida_ori[] = t_saida[].

  LOOP AT t_saida_ov   INTO w_saida_ov.

    FREE: l_pedidos.

    LOOP AT t_saida    INTO w_saida WHERE id_seq   = w_saida_ov-id_seq
                                      AND matnr_ov = w_saida_ov-matnr_ov.
      w_saida-menge       = w_saida_ov-menge.
      w_saida-vlr_usd     = w_saida_ov-vlr_usd.
      w_saida-vlr_brl     = w_saida_ov-vlr_brl.
      l_pedidos           = l_pedidos && w_saida-ebeln && '/' && w_saida-ebelp && '-'.
      MODIFY t_saida   FROM w_saida INDEX sy-tabix.
    ENDLOOP.

    CLEAR: vlr_icms, vlr_pis, vlr_cofins, vlr_iss, vlr_liquido.

    IF w_saida-vlr_brl > r_vlr_brl-low.
      l_erro = abap_true.
      t_saida[] = t_saida_ori[].
      msg    = 'Valor BRL não pode ser maior que R$'.
      CONCATENATE msg v_vlr_brl INTO  msg SEPARATED BY space.
      MESSAGE s024(sd) WITH msg DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*-CS2025000025-#164218-27.01.2025-JT-inicio
    SELECT *
      FROM zsdt0008
      INTO TABLE @DATA(it_zsdt0008)
     WHERE auart      EQ @w_saida-auart
       AND vkaus      EQ @wa_zlest0055-vkaus
       AND uf_centro  EQ @wa_lfa1-regio
       AND uf_cliente EQ @wa_kna1-regio
       AND mwsk1      EQ 'SD'
       AND ownpr      NE 'X'.

*    lc_dados-auart-valor      = w_saida-auart.
*    lc_dados-vkaus-valor      = wa_zlest0055-vkaus.
*    lc_dados-mwsk1-valor      = 'SD'.
*    lc_dados-uf_centro-valor  = wa_lfa1-regio.
*    lc_dados-uf_cliente-valor = wa_kna1-regio.
*    lc_dados-ownpr-regra      = 'NE'.
*    lc_dados-ownpr-valor      = 'X'.
*    lc_dados-bukrs_emit-valor = w_saida-bukrs_fat.
**   lc_dados-bukrs_toma-valor = w_saida-bukrs_fat.
*    lc_dados-kunnr-valor      = w_saida-cl_codigo.
*    lc_dados-werks-valor      = w_saida-werks.
*    lc_dados-matnr-valor      = w_saida-matnr.
*
*    lc_retorno = zcl_impostos=>get_tax_imposto( i_dados = lc_dados i_todos = abap_true ).
*
*    READ TABLE lc_retorno INTO wc_retorno INDEX 1.
*-CS2025000025-#164218-27.01.2025-JT-fim

   IF sy-subrc EQ 0.              "*-CS2025000025-#164218-27.01.2025-JT
*    IF wc_retorno IS NOT INITIAL.  "*-CS2025000025-#164218-27.01.2025-JT
*      SELECT *
*        FROM j_1btxsdc
*        INTO TABLE @DATA(it_j_1btxsdc)
*         FOR ALL ENTRIES IN  @lc_retorno               "@it_zsdt0008 "*-CS2025000025-#164218-27.01.2025-JT
*       WHERE taxcode   EQ @lc_retorno-j_1btxsdc        "@it_zsdt0008-j_1btxsdc "*-CS2025000025-#164218-27.01.2025-JT
*         AND custusage EQ 1.
     SELECT *
        FROM j_1btxsdc
        INTO TABLE @DATA(it_j_1btxsdc)
         FOR ALL ENTRIES IN  @it_zsdt0008 "*-CS2025000025-#164218-27.01.2025-JT
       WHERE taxcode   EQ @it_zsdt0008-j_1btxsdc "*-CS2025000025-#164218-27.01.2025-JT
         AND custusage EQ 1.

      IF sy-subrc EQ 0.
        LOOP AT it_j_1btxsdc INTO DATA(wa_j_1btxsdc).

          CLEAR: vlr_icms, vlr_pis, vlr_cofins.

          IF wa_j_1btxsdc-icms EQ 'X'.
            SELECT SINGLE *
              FROM j_1btxic1
              INTO @DATA(wa_j_1btxic1)
             WHERE land1    EQ 'BR'
               AND shipfrom EQ @wa_lfa1-regio
               AND shipto   EQ @wa_kna1-regio.

            CASE w_saida-waerk_fatura.
              WHEN 'BRL'.
                vlr_icms = ( w_saida-vlr_brl * wa_j_1btxic1-rate  ) / 100.
              WHEN 'USD'.
                vlr_icms = ( w_saida-vlr_usd * wa_j_1btxic1-rate  ) / 100.
            ENDCASE.
          ENDIF.

          IF wa_j_1btxsdc-pis EQ 'X'.
            SELECT SINGLE *
              FROM j_1btxpis
              INTO @DATA(wa_j_1btxpis)
             WHERE country    EQ 'BR'
               AND gruop      EQ '72'
               AND value      EQ @w_saida-werks
               AND validto    <= @w_saida-dt_fatura
               AND validfrom  >= @w_saida-dt_fatura.

            CASE w_saida-waerk_fatura.
              WHEN 'BRL'.
                vlr_pis = ( w_saida-vlr_brl *  wa_j_1btxpis-rate  ) / 100.
              WHEN 'USD'.
                vlr_pis = ( w_saida-vlr_usd *  wa_j_1btxpis-rate  ) / 100.
            ENDCASE.
          ENDIF.

          IF ( wa_j_1btxsdc-cofins EQ 'X' ).
            SELECT SINGLE *
              FROM j_1btxcof
              INTO @DATA(wa_j_1btxcof)
             WHERE country   EQ 'BR'
               AND gruop     EQ '71'
               AND value     EQ @w_saida-werks
               AND validto   <= @w_saida-dt_fatura
               AND validfrom >= @w_saida-dt_fatura.

            CASE w_saida-waerk_fatura.
              WHEN: 'BRL'.
                vlr_cofins  = ( w_saida-vlr_brl * wa_j_1btxcof-rate ) / 100.
              WHEN: 'USD'.
                vlr_cofins  = ( w_saida-vlr_usd * wa_j_1btxcof-rate ) / 100.
            ENDCASE.
          ENDIF.

          CASE w_saida-waerk_fatura.
            WHEN: 'BRL'.
              vlr_liquido = w_saida-vlr_brl - vlr_pis - vlr_cofins - vlr_icms.
            WHEN: 'USD'.
              vlr_liquido = w_saida-vlr_usd - vlr_pis - vlr_cofins - vlr_icms.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ELSE.
      FREE: it_j_1btxsdc.

      SELECT *
        FROM j_1bsdica
        INTO TABLE @DATA(it_j_1bsdica)
       WHERE auart EQ @w_saida-auart.

      SELECT *
        FROM j_1btxsdc
        INTO TABLE  it_j_1btxsdc
         FOR ALL ENTRIES IN it_j_1bsdica
       WHERE taxcode  EQ it_j_1bsdica-txsdc
         AND custusage EQ 1.

      IF sy-subrc EQ 0.
        CLEAR: wa_j_1btxpis, wa_j_1btxcof, vl_data, vl_validto, vl_validfrom.

        CONCATENATE w_saida-dt_fatura+6(2) '.'  w_saida-dt_fatura+4(2) '.' w_saida-dt_fatura(4) INTO vl_data.

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            input  = vl_data
          IMPORTING
            output = vl_validto.

        vl_validfrom  = vl_validto.

        LOOP AT it_j_1btxsdc INTO wa_j_1btxsdc.

          IF wa_j_1btxsdc-iss EQ 'X'.
            SELECT SINGLE *
              FROM j_1btxiss
              INTO @DATA(wa_j_1btxiss)
             WHERE country    EQ 'BR'
               AND gruop      EQ '73'
               AND taxjurcode EQ @wa_kna1-txjcd
               AND value      EQ @wa_lfa1-txjcd
               AND validto    <= @vl_validto
               AND validfrom  >= @vl_validfrom.

            CASE w_saida-waerk_fatura.
              WHEN: 'BRL'.
                vlr_iss   = (  w_saida-vlr_brl * wa_j_1btxiss-rate ) / 100.
              WHEN: 'USD'.
                vlr_iss    = ( w_saida-vlr_usd * wa_j_1btxiss-rate ) / 100.
            ENDCASE.
          ENDIF.

          IF wa_j_1btxsdc-pis EQ 'X'.
            SELECT SINGLE *
              FROM j_1btxpis
              INTO wa_j_1btxpis
             WHERE country   EQ 'BR'
               AND gruop     EQ '72'
               AND value     EQ w_saida-werks
               AND validto   <= vl_validto
               AND validfrom >= vl_validfrom.

            CASE w_saida-waerk_fatura.
              WHEN: 'BRL'.
                vlr_pis     = ( w_saida-vlr_brl * wa_j_1btxpis-rate ) / 100.
              WHEN: 'USD'.
                vlr_pis     = ( w_saida-vlr_usd * wa_j_1btxpis-rate ) / 100.
            ENDCASE.
          ENDIF.

          IF wa_j_1btxsdc-cofins EQ 'X'.
            SELECT SINGLE *
              FROM j_1btxcof
              INTO wa_j_1btxcof
             WHERE country    EQ 'BR'
               AND gruop      EQ '71'
               AND value      EQ w_saida-werks
               AND validto    <= vl_validto
               AND validfrom  >= vl_validfrom.

            CASE w_saida-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
              WHEN: 'BRL'.
                vlr_cofins  = ( w_saida-vlr_brl * wa_j_1btxcof-rate ) / 100.
              WHEN: 'USD'.
                vlr_cofins  = ( w_saida-vlr_usd * wa_j_1btxcof-rate ) / 100.
            ENDCASE.
          ENDIF.
        ENDLOOP.

        CASE w_saida-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
          WHEN: 'BRL'.
            vlr_liquido = w_saida-vlr_brl - vlr_iss - vlr_pis - vlr_cofins.
          WHEN: 'USD'.
            vlr_liquido = w_saida-vlr_usd - vlr_iss - vlr_pis - vlr_cofins.
        ENDCASE.
      ENDIF.
    ENDIF.


    CLEAR: wa_itemdata, wl_items_inx, wl_schedules_in, wa_condition.

*    SELECT SINGLE *
*      FROM zlest0059
*      INTO @DATA(wl_zlest0059)
*     WHERE bukrs       EQ @w_saida-emp_fat_serv
*       AND auart       EQ @w_saida-auart
*       AND po_embarque EQ @abap_off
*       AND po_destino  EQ @abap_off.
*
*    IF sy-subrc <> 0.
*      l_erro = abap_true.
*      MESSAGE s024(sd) WITH 'Material não encontrado para Tipo Venda'  DISPLAY LIKE 'E'.
*      EXIT.
*    ELSE.
*      w_saida-matnr_ov = wl_zlest0059-matnr.
*    ENDIF.

    SELECT SINGLE *
      FROM mara
      INTO @DATA(wmara)
     WHERE matnr EQ @w_saida-matnr_ov.

    IF sy-subrc <> 0.
      CLEAR wmara.
    ENDIF.

    l_itm_number            = l_itm_number + 10.

    wa_itemdata-itm_number  = l_itm_number.
    wa_itemdata-material    = w_saida-matnr_ov.
    wa_itemdata-plant       = w_zsdt0307-centro_fat_serv.
    wa_itemdata-division    = w_saida-spart. "(setor atividade)
    wa_itemdata-target_qty  = 1.
    wa_itemdata-target_qu   = wmara-meins.
    wa_itemdata-sales_unit  = wmara-meins.
    wa_itemdata-gross_wght  = w_saida-menge.
    wa_itemdata-net_weight  = w_saida-menge.
    wa_itemdata-untof_wght  = wmara-gewei.
    wa_itemdata-fix_val_dy  = w_saida-dt_fatura.
    wa_itemdata-price_date  = w_saida-dt_fatura.
    wa_itemdata-ex_rate_fi  = w_saida-tax_dolar.
    wa_itemdata-dlvschduse  = wa_zlest0055-vkaus.
    wa_itemdata-incoterms1  = 'SRV'.
    wa_itemdata-incoterms2  = 'Serviço'.
    wa_itemdata-purch_no_c  = l_pedidos. "w_saida-ebeln && '/' && w_saida-ebelp.

    SELECT SINGLE *
      FROM knvv
      INTO @DATA(wa_knvv)
     WHERE kunnr  EQ @w_saida-kunnr
       AND vkorg  EQ @w_saida-vkorg
       AND vtweg  EQ @w_saida-vtweg
       AND spart  EQ @w_saida-spart.

    IF sy-subrc <> 0.
      CLEAR wa_knvv.
    ENDIF.

    wa_knvv-kdgrp = |{ wa_knvv-kdgrp ALPHA = IN }|.

    SELECT SINGLE *
      FROM marc
      INTO @DATA(wa_marc)
     WHERE matnr EQ @w_saida-matnr_ov
       AND werks EQ @w_zsdt0307-centro_fat_serv.

    SELECT SINGLE *
      FROM mbew
      INTO @DATA(wa_mbew)
     WHERE matnr EQ @w_saida-matnr_ov
       AND bwkey EQ @w_zsdt0307-centro_fat_serv.

    IF wa_lfa1-regio EQ wa_kna1-regio.
      SELECT SINGLE *
        FROM j_1bapn
        INTO @DATA(wa_1bapn)
       WHERE direct EQ '2'
         AND dstcat EQ '0'
         AND indus3 EQ @wa_marc-indus
         AND itmtyp EQ 'ZH'
         AND ownpro EQ ' '
         AND matuse EQ @wa_mbew-mtuse
         AND indus1 EQ ' '.
    ELSE.
      SELECT SINGLE *
        FROM j_1bapn
        INTO wa_1bapn
       WHERE direct EQ '2'
         AND dstcat EQ '1'
         AND indus3 EQ wa_marc-indus
         AND itmtyp EQ 'ZH'
         AND ownpro EQ ' '
         AND matuse EQ wa_mbew-mtuse
         AND indus1 EQ ' '.
    ENDIF.

    wa_itemdata-cfop_long      = wa_1bapn-cfop.
    APPEND wa_itemdata        TO it_itemdata.

    wl_items_inx-itm_number    = l_itm_number.
    wl_items_inx-target_qty    = 'X'.
    APPEND wl_items_inx       TO it_items_inx.

    wl_schedules_in-itm_number = l_itm_number.
    wl_schedules_in-req_qty    = 1.
    APPEND wl_schedules_in    TO  tl_schedules_in.

    wa_condition-itm_number    = l_itm_number.
    wa_condition-cond_type     = 'PR00'.

    IF vlr_liquido IS NOT INITIAL.
      wa_condition-cond_value  =  vlr_liquido.
    ELSE.
      IF w_saida-waerk_fatura = 'BRL'.
        wa_condition-cond_value = w_saida-vlr_brl.
      ELSE.
        wa_condition-cond_value = w_saida-vlr_usd.
      ENDIF.
    ENDIF.

    wa_condition-currency      = w_saida-waerk_fatura.
    wa_condition-cond_unit     = wmara-meins.
    APPEND  wa_condition      TO it_condition.
  ENDLOOP.

  CHECK l_erro = abap_false.

*----------------------
* parceiros
*----------------------
  wa_partner-partn_role = 'AG'.
  wa_partner-partn_numb = w_saida-cl_codigo.
  APPEND wa_partner TO it_partner.

  wa_partner-partn_role = 'RE'.
  wa_partner-partn_numb = w_saida-cl_codigo.
  APPEND wa_partner TO it_partner.

  wa_partner-partn_role = 'RG'.
  wa_partner-partn_numb = w_saida-cl_codigo.
  APPEND wa_partner TO it_partner.

  wa_partner-partn_role = 'WE'.
  wa_partner-partn_numb = w_saida-cl_codigo.
  APPEND wa_partner TO it_partner.

*-------------------------------------------
* Criar OV
*-------------------------------------------
  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in     = wl_header_in
      sales_header_inx    = wl_header_inx
    IMPORTING
      salesdocument_ex    = vbeln_ov
    TABLES
      return              = it_return
      sales_items_in      = it_itemdata
      sales_items_inx     = it_items_inx
      sales_partners      = it_partner
      sales_schedules_in  = tl_schedules_in
      sales_conditions_in = it_condition.

  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    FREE : tg_msg_ret, wa_return, wl_msg_ret.

    LOOP AT it_return INTO wa_return WHERE type <> 'S'.
      wl_msg_ret-msg = wa_return-message.
      APPEND wl_msg_ret TO tg_msg_ret.
    ENDLOOP.

    CHECK tg_msg_ret[] IS NOT INITIAL.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = 'X'
        i_repid       = sy-repid
        i_pressed_tab = 'TABSTRIP-ACTIVETAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

    LOOP AT t_saida  INTO w_saida WHERE id_seq = l_id_seq.
      w_saida-status    = icon_led_red.
      MODIFY t_saida FROM w_saida INDEX sy-tabix TRANSPORTING status.
    ENDLOOP.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    SELECT *
      FROM vbuv
      INTO TABLE tl_vbuv
     WHERE vbeln EQ vbeln_ov.

    IF sy-subrc IS INITIAL.
      LOOP AT  tl_vbuv INTO wl_vbuv.
        CLEAR: wl_fieldname, wl_text.

        wl_fieldname = wl_vbuv-fdnam.

        CALL FUNCTION 'RM_DDIC_TEXTS_GET'
          EXPORTING
            i_name                = wl_fieldname
            i_type                = 'DTEL'
            i_langu               = sy-langu
          IMPORTING
            e_ddtxt               = wl_text
          EXCEPTIONS
            objtype_not_supported = 1
            illegal_input         = 2
            OTHERS                = 3.

        IF sy-subrc <> 0.
          CONCATENATE 'Dados incompletos na O.V: ' wl_vbuv-fdnam INTO wl_msg_ret-msg SEPARATED BY space.
        ELSE.
          CONCATENATE 'Dados incompletos na O.V: ' wl_text INTO wl_msg_ret-msg SEPARATED BY space.
        ENDIF.
        APPEND wl_msg_ret TO tg_msg_ret.
      ENDLOOP.

      CHECK tg_msg_ret[] IS NOT INITIAL.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = 'X'
          i_repid       = sy-repid
          i_pressed_tab = 'TABSTRIP-ACTIVETAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

      wl_erro = 'X'.
    ENDIF.

*-------------------------------------------
*-- Elimina OV
*-------------------------------------------
    CASE wl_erro.

      WHEN 'X'.
        REFRESH it_return.
        CLEAR: wl_header_in.
        wl_header_inx2-updateflag = 'D'.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            salesdocument    = vbeln_ov
            order_header_inx = wl_header_inx2
          TABLES
            return           = it_return.

        READ TABLE it_return INTO wa_return WITH KEY type = 'S'.

        IF sy-subrc = 0.
          REFRESH t_rows.

          CALL METHOD g_grid->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.

          CLEAR: w_saida, w_rows.
          READ TABLE t_rows   INTO w_rows  INDEX 1.
          READ TABLE t_saida  INTO w_saida INDEX w_rows-index.

          LOOP AT t_saida INTO w_saida WHERE id_seq = l_id_seq.
            CLEAR: w_saida-auart,
                   w_saida-tax_dolar,
                   w_saida-vlr_usd,
                   w_saida-vlr_brl,
                   w_saida-waerk,
                   w_saida-waerk_fatura.
            w_saida-status    = icon_led_red.
            MODIFY t_saida FROM w_saida INDEX sy-tabix TRANSPORTING status auart tax_dolar vlr_usd vlr_brl waerk waerk_fatura.
          ENDLOOP.

          CALL METHOD g_grid->refresh_table_display
            EXPORTING
              is_stable = w_stable.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.

      WHEN OTHERS.
        vbeln_fatura = vbeln_ov.

        "Set utilizado por odens ZTRG criadas pela transação ZSDT0158
        IF w_saida-auart IN r_auart_ztrg.
          t_billing = VALUE #( (  ref_doc     = vbeln_fatura
                                  ref_doc_ca  = 'C'
                                  bill_date   = w_saida-dt_fatura  )  ). "sy-datum  )  ).

        ELSE.
          t_billing = VALUE #( ( ref_doc      = vbeln_fatura
                                 ref_doc_ca   = 'C'
                                 bill_date    = sy-datum  )  ).
        ENDIF.

*-------------------------------------------
*------ Criar fatura
*-------------------------------------------
        CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
          TABLES
            billingdatain = t_billing
            return        = t_return
            success       = t_success.

        IF t_success IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          TRY.
              vbeln_fatura = t_success[ 1 ]-bill_doc.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          wl_erro =  abap_true.
        ENDIF.

        WAIT UP TO 12 SECONDS.

        CLEAR: wa_j_1bnflin.

        SELECT SINGLE *
          FROM j_1bnflin
          INTO wa_j_1bnflin
         WHERE refkey EQ vbeln_fatura.

        IF sy-subrc EQ 0.
          SELECT *
            FROM zsdt0306
            INTO TABLE @DATA(t_0306)
           WHERE id_seq = @w_saida-id_seq.

          SELECT *
            FROM zsdt0225
            INTO TABLE @DATA(t_0225)
           WHERE id_seq = @w_saida-id_seq.

          LOOP AT t_0306      INTO DATA(w_0306).
            READ TABLE t_saida_ori INTO w_saida WITH KEY id_seq = w_0306-id_seq
                                                         ebeln  = w_0306-ebeln
                                                         ebelp  = w_0306-ebelp.
            CHECK sy-subrc = 0.
            w_0306-bukrs_fat   = w_zsdt0307-emp_fat_serv.
            w_0306-werks_fat   = w_zsdt0307-centro_fat_serv.
            w_0306-vlr_brl     = w_saida-vlr_brl.
            w_0306-vlr_usd     = w_saida-vlr_usd.
            MODIFY zsdt0306 FROM w_0306.
          ENDLOOP.

          LOOP AT t_0225      INTO DATA(w_0225).

            CLEAR: w_saida, l_vlr_usd, l_vlr_brl.
            LOOP AT t_saida_ori INTO w_saida WHERE id_seq = w_0225-id_seq
                                               AND matnr  = w_0225-cod_material.
              l_vlr_brl = l_vlr_brl + w_saida-vlr_brl.
              l_vlr_usd = l_vlr_usd + w_saida-vlr_usd.
            ENDLOOP.

            w_0225-dt_fatura      = w_saida-dt_fatura.
            w_0225-auart          = w_saida-auart.
            w_0225-bukrs_serv     = w_zsdt0307-emp_fat_serv.
            w_0225-werks_serv     = w_zsdt0307-centro_fat_serv.
            w_0225-waerk          = w_saida-waerk.
            w_0225-netpr          = w_saida-netpr.
            w_0225-tax_dolar      = w_saida-tax_dolar.
            w_0225-vlr_usd        = l_vlr_usd.
            w_0225-vlr_brl        = l_vlr_brl.
            w_0225-vkorg          = w_saida-vkorg.
            w_0225-vtweg          = w_saida-vtweg.
            w_0225-spart          = w_saida-spart.
            w_0225-matnr_ov       = w_saida-matnr_ov.
            w_0225-zterm          = w_saida-zterm.
            w_0225-nr_ov          = vbeln_ov.
            w_0225-fatura         = vbeln_fatura.
            w_0225-docnum         = wa_j_1bnflin-docnum.
            w_0225-waerk_fatura   = w_saida-waerk_fatura.
            w_0225-navio          = w_saida-navio.
            w_0225-local_operacao = w_saida-local_operacao.
            w_0225-usuario        = sy-uname.
            w_0225-data_registro  = sy-datum.
            w_0225-hora_registro  = sy-uzeit.
            MODIFY zsdt0225    FROM w_0225.
          ENDLOOP.

          LOOP AT t_saida     INTO w_saida WHERE id_seq = l_id_seq.
            l_tabix = sy-tabix.

            READ TABLE t_saida_ori INTO w_saida_ori WITH KEY id_seq = w_saida-id_seq
                                                             matnr  = w_saida-matnr
                                                             ebeln  = w_saida-ebeln
                                                             ebelp  = w_saida-ebelp.
            w_saida-status       = icon_led_green.
            w_saida-nr_ov        = vbeln_ov.
            w_saida-fatura       = vbeln_fatura.
            w_saida-docnum       = wa_j_1bnflin-docnum.
            w_saida-menge        = w_saida_ori-menge.
            w_saida-tax_dolar    = w_saida_ori-tax_dolar.
            w_saida-vlr_usd      = w_saida_ori-vlr_usd.
            w_saida-vlr_brl      = w_saida_ori-vlr_brl.
            t_style              = VALUE #( ( fieldname = 'NAVIO'          style = cl_gui_alv_grid=>mc_style_disabled  )
                                            ( fieldname = 'LOCAL_OPERACAO' style = cl_gui_alv_grid=>mc_style_disabled  )
                                            ( fieldname = 'DT_FATURA'      style = cl_gui_alv_grid=>mc_style_disabled )
                                            ( fieldname = 'AUART'          style = cl_gui_alv_grid=>mc_style_disabled )
                                            ( fieldname = 'WAERK'          style = cl_gui_alv_grid=>mc_style_disabled ) ).
            w_saida-celltab[]    = t_style[].
            MODIFY t_saida    FROM w_saida INDEX l_tabix.
          ENDLOOP.

          COMMIT WORK.

          MESSAGE s024(sd) WITH 'Ordem de Venda Gerada com Sucesso!'.

          CALL METHOD g_grid->refresh_table_display
            EXPORTING
              is_stable = w_stable.
        ENDIF.
    ENDCASE.

  ENDIF.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

**********************************************************************
* gerar OV
**********************************************************************
FORM f_estornar_doctos.

  DATA: tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
        wl_msg_ret      TYPE zfiwrs0002,
        tg_estorno      TYPE TABLE OF zsdt0225,
        wg_estorno      TYPE zsdt0225,
        t_success       TYPE TABLE OF bapivbrksuccess,
        t_return        TYPE TABLE OF bapireturn1,
        vdocnum_est     TYPE j_1bdocnum,
        is_cancelled    TYPE bapivbrkout, " BUG 133411 - IR169490 -  DUMP em estorno lançamento  NFPS - transação ZSDT0158  - Fertilizantes - BG
        estornado       TYPE c,
        txt_bstkd       TYPE vbkd-bstkd,
        data_ov         TYPE c LENGTH 10,
        wg_mensagem(30),
        tl_msg          TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        wl_msg          TYPE bdcmsgcoll,
        w_erro          TYPE char1,
        w_bukrs         TYPE t001-bukrs,  "Empresa
        w_desc_bukrs    TYPE t001-butxt,  "Descrição da Empresa
        w_werks         TYPE t001w-werks, "Centro Emissor
        w_desc_werks    TYPE t001w-name1, "Descrição do Centro
        w_ano           TYPE zlest0056-ano_viagem, "Ano da Viagem
        w_viagem        TYPE zlest0058-nr_viagem,
        l_mode          TYPE char1,
        l_id_seq        TYPE zsdt0225-id_seq.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  IF     l_lines = 0.
    MESSAGE s024(sd) WITH 'Selecionar uma Linha!' DISPLAY LIKE 'W'.
    EXIT.
  ELSEIF l_lines > 1.
    MESSAGE s024(sd) WITH 'Selecionar apenas uma Linha para Estorno!' DISPLAY LIKE 'W'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Cancelando Fatura...'.

  FREE: t_return, t_success, w_erro, estornado.

*---------------------------------
*- validacoes
*---------------------------------
  READ TABLE t_rows  INTO w_rows  INDEX 1.
  READ TABLE t_saida INTO w_saida INDEX w_rows-index.

  l_id_seq = w_saida-id_seq.
  w_erro   = abap_false.

*---> S4 MIGRATION 07/07/2023 - MA
*  CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*    EXPORTING
*      billingdoc_number       = w_saida-fatura
*    IMPORTING
*      billingdoc_is_cancelled = is_cancelled.

  DATA: vl_bill_doc TYPE bapivbrksuccess-bill_doc.

  vl_bill_doc = CONV #( w_saida-fatura ).

  CALL FUNCTION 'BAPI_BILLINGDOC_GETDETAIL'
    EXPORTING
      billingdocument       = vl_bill_doc
    IMPORTING
      billingdocumentdetail = is_cancelled
*     RETURN                =
    .
*<--- S4 MIGRATION 07/07/2023 - MA

  IF is_cancelled-cancelled IS INITIAL.
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        billingdocument = w_saida-fatura
      TABLES
        return          = t_return
        success         = t_success.
  ENDIF.

  IF t_success[] IS NOT INITIAL OR  is_cancelled IS NOT INITIAL.

    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO @DATA(_docnum)
     WHERE docnum EQ @w_saida-docnum.

    SELECT SINGLE candat
      FROM j_1bnfdoc
      INTO @DATA(_vcandat)
     WHERE docnum EQ @_docnum.

    IF _vcandat IS INITIAL.
      SELECT SINGLE docnum
        FROM j_1bnfe_active
        INTO @DATA(v_docnum)
       WHERE docnum EQ @_docnum
         AND docsta EQ '1'
         AND cancel EQ ''.

      IF sy-subrc NE 0.
        CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
          EXPORTING
            doc_number               = _docnum
            ref_type                 = space
            ref_key                  = space
            can_dat                  = sy-datum
          IMPORTING
            doc_number               = vdocnum_est
          EXCEPTIONS
            document_not_found       = 1
            cancel_not_possible      = 2
            nf_cancel_type_not_found = 3
            database_problem         = 4
            docum_lock               = 5
            nfe_cancel_simulation    = 6
            OTHERS                   = 7.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          w_erro = abap_true.
        ENDIF.
      ELSE.
        w_erro = abap_true.
      ENDIF.

      IF w_erro IS NOT INITIAL.
        APPEND VALUE #( msg = |Impossível estorno de fatura { w_saida-fatura }. Danfe, não estornada| ) TO tg_msg_ret.
      ENDIF.
    ENDIF.
  ELSE.
    w_erro = abap_true.
  ENDIF.

  WAIT UP TO 2 SECONDS.

  DELETE t_return WHERE type NE 'E' .

  IF t_return[] IS INITIAL AND w_erro IS NOT INITIAL.

    LOOP AT t_return INTO DATA(wa).
      SELECT *
        FROM t100
        INTO TABLE @DATA(it_t100)
       WHERE sprsl EQ 'PT'
         AND arbgb EQ @wa-id
         AND msgnr EQ @wa-number.

      CHECK it_t100 IS NOT  INITIAL.

      LOOP AT it_t100 INTO DATA(wa_t100).
        CONCATENATE w_saida-fatura '-' wa_t100-text INTO wl_msg_ret-msg SEPARATED BY space.
        APPEND wl_msg_ret TO tg_msg_ret.
      ENDLOOP.

      IF NOT ( tg_msg_ret[] IS INITIAL ).
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = 'X'
            i_repid       = sy-repid
            i_pressed_tab = 'TABSTRIP-ACTIVETAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ELSE.
        estornado = abap_true.
      ENDIF.
    ENDLOOP.
  ELSE.
    estornado = abap_true.
  ENDIF.

  CASE estornado.
    WHEN abap_true.
      FREE: w_header_in, w_header_inx, it_return.

*----------------------------
*---- bloquear OV
*----------------------------
      w_header_in-bill_block  = '03'.
      w_header_inx-bill_block = abap_true.
      w_header_inx-updateflag = 'U'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          salesdocument    = w_saida-nr_ov
          order_header_in  = w_header_in
          order_header_inx = w_header_inx
        TABLES
          return           = it_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      SELECT *
        FROM zsdt0306
        INTO TABLE @DATA(t_0306)
       WHERE id_seq = @l_id_seq.

      LOOP AT t_0306      INTO DATA(w_0306).
        CLEAR: w_0306-vlr_brl, w_0306-vlr_usd.
        MODIFY zsdt0306   FROM w_0306.
      ENDLOOP.

      SELECT *
        FROM zsdt0225
        INTO TABLE @DATA(t_0225)
       WHERE id_seq = @l_id_seq.

      LOOP AT t_0225      INTO DATA(w_0225).
        CLEAR: w_0225-dt_fatura, w_0225-auart,  w_0225-bukrs_serv, w_0225-werks_serv,
               w_0225-waerk,     w_0225-netpr,  w_0225-tax_dolar,  w_0225-vlr_usd,    w_0225-vlr_brl,
               w_0225-vkorg,     w_0225-vtweg,  w_0225-spart,      w_0225-matnr_ov,   w_0225-zterm,
               w_0225-nr_ov,     w_0225-fatura, w_0225-docnum,     w_0225-waerk_fatura,
               w_0225-navio,     w_0225-local_operacao.
        MODIFY zsdt0225   FROM w_0225.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      LOOP AT t_saida     INTO w_saida WHERE id_seq = l_id_seq.
        CLEAR: w_saida-dt_fatura,
               w_saida-vlr_usd,
               w_saida-vlr_brl,
               w_saida-netpr,
               w_saida-waerk,
               w_saida-emp_fat_serv,
               w_saida-auart,
               w_saida-waerk_fatura,
               w_saida-zterm,
               w_saida-tax_dolar.

        w_saida-status       = icon_led_yellow.
        w_saida-nr_ov        = abap_off.
        w_saida-fatura       = abap_off.
        w_saida-docnum       = 0.
        t_style              = VALUE #( ( fieldname = 'NAVIO'          style = cl_gui_alv_grid=>mc_style_disabled  )
                                        ( fieldname = 'LOCAL_OPERACAO' style = cl_gui_alv_grid=>mc_style_disabled  )
                                        ( fieldname = 'DT_FATURA'      style = cl_gui_alv_grid=>mc_style_disabled )
                                        ( fieldname = 'AUART'          style = cl_gui_alv_grid=>mc_style_disabled )
                                        ( fieldname = 'WAERK'          style = cl_gui_alv_grid=>mc_style_disabled ) ).
        w_saida-celltab[]    = t_style[].
        MODIFY t_saida    FROM w_saida INDEX sy-tabix.
      ENDLOOP.
  ENDCASE.

  IF estornado = abap_true.
    MESSAGE s024(sd) WITH 'Documentos foram Estornados!'.
  ELSE.
    MESSAGE w024(sd) WITH 'Não há documentos a Estornar!'.
  ENDIF.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

**********************************************************************
* preenche shdb
**********************************************************************
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.

  CLEAR: wl_bdc.

  IF l_start = 'X'.
    MOVE: l_name  TO wl_bdc-program,
          l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE: l_name  TO wl_bdc-fnam,
          l_value TO wl_bdc-fval.
  ENDIF.

  APPEND wl_bdc   TO tl_bdc.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  FREE: lt_f401.

  wl_f401-fieldname  = 'AUART'.
  wl_f401-register   = 'X'.
  wl_f401-getbefore  = 'X'.
  APPEND wl_f401 TO  lt_f401.
  CLEAR wl_f401.

  wl_f401-fieldname  = 'WAERK'.
  wl_f401-register   = 'X'.
  wl_f401-getbefore  = 'X'.
  APPEND wl_f401 TO  lt_f401.
  CLEAR wl_f401.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-zebra        = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLTAB'.
* w_layout-ctab_fname   = 'CELLTAB'.

  PERFORM toolbar_alv.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid,
                 lcl_event_handler=>on_data_changed  FOR g_grid,
                 lcl_event_handler=>user_command     FOR g_grid,
                 lcl_event_handler=>toolbar          FOR g_grid,
                 lcl_event_handler=>on_f4            FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = t_exclude
      CHANGING
        it_outtab                     = t_saida[]
        it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f401[].

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: t_function.

  w_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND w_function TO t_function.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

  w_sort-fieldname = 'ID_SEQ'.
  w_sort-subtot    = 'X'.
  w_sort-spos      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.
*
*  w_sort-fieldname = 'NRO_CGD'.
** w_sort-subtot    = 'X'.
*  w_sort-spos      = 2.
*  w_sort-up        = 'X'.
*  APPEND w_sort   TO t_sort.
*
*  w_sort-fieldname = 'NR_ROMANEIO'.
** w_sort-subtot    = 'X'.
*  w_sort-spos      = 3.
*  w_sort-up        = 'X'.
*  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''          ''       'T_ALV' 'STATUS'              'Status'                   '05'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    02  ''          ''       'T_ALV' 'ID_SEQ'              'Lote'                     '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    03  ''          ''       'T_ALV' 'BUKRS'               'Empresa Ped.'             '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    04  ''          ''       'T_ALV' 'WERKS'               'Filial Ped.'              '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    05  ''          ''       'T_ALV' 'EBELN'               'Nr.Pedido'                '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    06  ''          ''       'T_ALV' 'EBELP'               'Item Ped.'                '09'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    07  'MARA'      'MATNR'  'T_ALV' 'MATNR'               'Cód.Material Ped.'        '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
    08  ''          ''       'T_ALV' 'MAKTX'               'Descr.Material Ped.'      '30'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''          ''       'T_ALV' 'MATKL'               'Grp.Merc Ped.'            '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''          ''       'T_ALV' 'MENGE'               'Quantidade Ped.'          '15'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    11  'KNA1'      'KUNNR'  'T_ALV' 'KUNNR'               'Cliente OV'               '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    12  ''          ''       'T_ALV' 'NAVIO'               'Navio'                    '40'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    13  ''          ''       'T_ALV' 'LOCAL_OPERACAO'      'Local de Operação'        '40'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    14  'VBAK'      'AEDAT'  'T_ALV' 'DT_FATURA'           'Dt.Fatura'                '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    15  ''          ''       'T_ALV' 'AUART'               'Tp.OV'                    '10'  ' ' ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ',
    16  ''          ''       'T_ALV' 'WAERK'               'Moeda Ct.'                '09'  ' ' ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ',
    17  ''          ''       'T_ALV' 'NETPR'               'Preço Unit.'              '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    18  ''          ''       'T_ALV' 'TAX_DOLAR'           'Tx.OV'                    '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    19  ''          ''       'T_ALV' 'EMP_FAT_SERV'        'Emp.Fat.Serv'             '12'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    20  ''          ''       'T_ALV' 'VLR_USD'             'Vlr.USD'                  '12'  ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    21  ''          ''       'T_ALV' 'VLR_BRL'             'Vlr.BRL'                  '12'  ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    22  ''          ''       'T_ALV' 'WAERK_FATURA'        'Moeda Fat'                '09'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
    23  'VBAK'      'VBELN'  'T_ALV' 'NR_OV'               'Nr.OV'                    '12'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
    24  'VBRK'      'VBELN'  'T_ALV' 'FATURA'              'Fatura'                   '12'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
    25  'J_1BNFDOC' 'DOCNUM' 'T_ALV' 'DOCNUM'              'Nro.NF'                   '10'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' '.

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

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv.

  FREE: t_exclude.
* APPEND cl_gui_alv_grid=>mc_mb_export TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_sort TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO t_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_filter TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_find TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_find_more TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_average TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_subtot TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_sum TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_mb_view TO t_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_graph TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_info TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_detail TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_check TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_call_abc TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO t_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_mb_paste TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO T_EXCLUDE.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO T_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO t_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO t_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO t_exclude.

ENDFORM.

**********************************************************************
* salvar variante
**********************************************************************
FORM f_save_variant.

  DATA: ft_params    TYPE TABLE OF rsparams,
        ft_sscr      TYPE TABLE OF rsscr,
        fs_sscr      TYPE rsscr,
        fr_selopt    TYPE RANGE OF rsscr-name,
        fs_selopt    LIKE LINE OF fr_selopt,
        f_vari_desc  TYPE varid,
        ft_vari_text TYPE TABLE OF varit,
        fs_vari_text TYPE varit,
        ft_vscreens  TYPE TABLE OF rsdynnr,
        fs_vscreens  TYPE rsdynnr,
        f_retcode    TYPE c,
        l_dynnr      TYPE sy-dynnr,
        l_varname    TYPE rsvar-variant,
        l_vartext    TYPE varit-vtext.

  CALL SCREEN 0200 STARTING AT 10 02
                     ENDING AT 57 04.

  CHECK rsvar-variant IS NOT INITIAL.
  l_dynnr   = '0100'.
  l_varname = rsvar-variant.
  l_vartext = rsvar-vtext.

*get all selection screen parameters and values
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = sy-repid
* IMPORTING
*     SP              = SP
    TABLES
      selection_table = ft_params
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
  ENDIF.

*get fields of our subscreen
  CALL FUNCTION 'RS_ISOLATE_1_SELSCREEN'
    EXPORTING
      program     = sy-repid
      dynnr       = '0101' "l_dynnr
*     TABIX       = 0
*   IMPORTING
*     LAST_TABIX  = LAST_TABIX
    TABLES
      screen_sscr = ft_sscr
*     GLOBAL_SSCR = GLOBAL_SSCR
    EXCEPTIONS
      no_objects  = 1
      OTHERS      = 2.
  IF sy-subrc EQ 0.
    LOOP AT ft_sscr INTO fs_sscr.
      fs_selopt-sign = 'I'.
      fs_selopt-option = 'EQ'.
      fs_selopt-low = fs_sscr-name.
      APPEND fs_selopt TO fr_selopt.
    ENDLOOP.
*delete parameters and values not used on our subscreen
    DELETE ft_params WHERE selname NOT IN fr_selopt.
  ENDIF.

*set variant global data
  MOVE sy-mandt             TO f_vari_desc-mandt.
  MOVE sy-repid             TO f_vari_desc-report.
  MOVE l_varname            TO f_vari_desc-variant.
  MOVE sy-uname             TO f_vari_desc-ename.
  MOVE sy-datum             TO f_vari_desc-edat .
  MOVE sy-uzeit             TO f_vari_desc-etime.
  MOVE 'X'                  TO f_vari_desc-environmnt.

*set description of variant
  MOVE sy-mandt             TO fs_vari_text-mandt.
  MOVE sy-langu             TO fs_vari_text-langu.
  MOVE sy-repid             TO fs_vari_text-report .
  MOVE l_varname            TO fs_vari_text-variant.
  MOVE l_vartext            TO fs_vari_text-vtext.
  APPEND  fs_vari_text TO ft_vari_text.

*set subscreen number
  fs_vscreens-dynnr = l_dynnr.
  fs_vscreens-kind = ''.
  APPEND fs_vscreens TO ft_vscreens.
*create variant
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      curr_report               = sy-repid
      curr_variant              = l_varname
      vari_desc                 = f_vari_desc
    TABLES
      vari_contents             = ft_params
      vari_text                 = ft_vari_text
      vscreens                  = ft_vscreens
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_exists            = 7
      variant_locked            = 8
      OTHERS                    = 9.
  IF sy-subrc EQ 7.
*variant already exists so ask if user wants to overwrite it
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1  = 'Variant exists, do you want to overwrite it?'
        textline1      = ' '
        titel          = 'Variant exists, do you want to overwrite it?'
        cancel_display = space
      IMPORTING
        answer         = f_retcode
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    IF f_retcode EQ 'J'.
*user agreed so change existing variant
      CALL FUNCTION 'RS_CHANGE_CREATED_VARIANT'
        EXPORTING
          curr_report               = sy-repid
          curr_variant              = l_varname
          vari_desc                 = f_vari_desc
*         ONLY_CONTENTS             = ONLY_CONTENTS
        TABLES
          vari_contents             = ft_params
          vari_text                 = ft_vari_text
*         VARI_SEL_DESC             = VARI_SEL_DESC
*         OBJECTS                   = OBJECTS
        EXCEPTIONS
          illegal_report_or_variant = 1
          illegal_variantname       = 2
          not_authorized            = 3
          not_executed              = 4
          report_not_existent       = 5
          report_not_supplied       = 6
          variant_doesnt_exist      = 7
          variant_locked            = 8
          selections_no_match       = 9
          OTHERS                    = 10.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* ler variante
**********************************************************************
FORM f_get_variant.

  DATA: ft_params    TYPE TABLE OF rsparams,
        f_variant    TYPE rsvar-variant,
        f_text       TYPE rsvar-vtext,
        fs_params    TYPE rsparams,
        fs_paramsscr TYPE rsparams,
        f_fieldname  TYPE fieldname,
        ft_sscr      TYPE TABLE OF rsscr,
        l_dynnr      TYPE sy-dynnr.

  FIELD-SYMBOLS <any_selopt_itab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <any_selopt> TYPE any.
  FIELD-SYMBOLS <any_field> TYPE any.

  l_dynnr = '0100'.

  "now I will display pop-up with variants for one subscreen
  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
*     NEW_TITLE            = ' '
      dynnr                = l_dynnr
*     INTERNAL_CALL        = ' '
*     MASKED               = 'X'
*     VARIANT              = ' '
      pop_up               = 'X'
    IMPORTING
      sel_variant          = f_variant
      sel_variant_text     = f_text
*   TABLES
*     BELONGING_DYNNR      = BELONGING_DYNNR
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

  IF sy-subrc EQ 0 .
    rsvar-variant = f_variant.
    rsvar-vtext   = f_text.

    "if variant was supplied then I read its content
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = sy-repid
        variant              = f_variant
        move_or_write        = 'M'
*       NO_IMPORT            = ' '
*       EXECUTE_DIRECT       = ' '
*   IMPORTING
*       SP                   = SP
      TABLES
*       L_PARAMS             = L_PARAMS
*       L_PARAMS_NONV        = L_PARAMS_NONV
*       L_SELOP              = L_SELOP
*       L_SELOP_NONV         = L_SELOP_NONV
        valutab              = ft_params
*       OBJECTS              = OBJECTS
*       FREE_SELECTIONS_DESC = FREE_SELECTIONS_DESC
*       FREE_SELECTIONS_VALUE       = FREE_SELECTIONS_VALUE
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc EQ 0.
      "let's see what fields are on this subscreen
      CALL FUNCTION 'RS_ISOLATE_1_SELSCREEN'
        EXPORTING
          program     = sy-repid
          dynnr       = '0101'  "l_dynnr
*         TABIX       = 0
*   IMPORTING
*         LAST_TABIX  = LAST_TABIX
        TABLES
          screen_sscr = ft_sscr
*         GLOBAL_SSCR = GLOBAL_SSCR
        EXCEPTIONS
          no_objects  = 1
          OTHERS      = 2.
      IF sy-subrc EQ 0.
        SORT ft_sscr BY name ASCENDING.

        " clear current content of selection fields
        LOOP AT ft_params INTO fs_params.
          READ TABLE ft_sscr WITH KEY name = fs_params-selname BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CONCATENATE '(' sy-repid ')' fs_params-selname INTO f_fieldname.
          CASE fs_params-kind.
            WHEN 'S'.
              CONCATENATE f_fieldname '[]' INTO f_fieldname.
              ASSIGN (f_fieldname) TO <any_selopt_itab>.
              IF sy-subrc EQ 0.
                REFRESH <any_selopt_itab>.
              ENDIF.
            WHEN 'P'.
              ASSIGN (f_fieldname) TO <any_field>.
              IF sy-subrc EQ 0.
                CLEAR <any_field>.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        "add values from saved variant to selection fields
        LOOP AT ft_params INTO fs_params.
          READ TABLE ft_sscr WITH KEY name = fs_params-selname BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CONCATENATE '(' sy-repid ')' fs_params-selname INTO f_fieldname.
          CASE fs_params-kind.
            WHEN 'S'. "select-options
              CONCATENATE f_fieldname '[]' INTO f_fieldname.
              ASSIGN (f_fieldname) TO <any_selopt_itab>.
              IF sy-subrc EQ 0.
                "firstly append initial line to be able to assign components
                APPEND INITIAL LINE TO <any_selopt_itab> ASSIGNING <any_selopt>.
                "now fill each component separately
                ASSIGN COMPONENT 'SIGN' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-sign.
                ENDIF.

                ASSIGN COMPONENT 'OPTION' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-option.
                ENDIF.

                ASSIGN COMPONENT 'LOW' OF STRUCTURE <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-low.
                ENDIF.

                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-high.
                ENDIF.

                "just to be sure that select options are filled
                ASSIGN COMPONENT 'SIGN' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc NE 0.
                  DELETE TABLE <any_selopt_itab> FROM <any_selopt>.
                ELSEIF <any_field> IS INITIAL.
                  DELETE TABLE <any_selopt_itab> FROM <any_selopt>.
                ENDIF.

              ENDIF.
            WHEN 'P'. "parameters
              ASSIGN (f_fieldname) TO <any_field>.
              IF sy-subrc EQ 0.
                CLEAR <any_field>.
                <any_field> = fs_params-low.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
**********************************************************************
