*----------------------------------------------------------------------*
***INCLUDE ZFIR0066_FORM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
DATA: v_data_init TYPE sy-datum,
      v_data_fina TYPE sy-datum.

*---> 25.07.2023 16:33:17 - Migração S4 - DL
v_data_init = sy-datum.
v_data_fina = sy-datum + 30.
*<--- 25.07.2023 16:33:17 - Migração S4 - DL

FORM seleciona_dados .

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
        wl_x001         TYPE x001,
        vl_gdatu        TYPE gdatu_inv,
        vl_ent_sai      TYPE c,
        vl_wrbtr        TYPE bsik-wrbtr,
        vl_lifnr_int    TYPE i,
        vl_obj_key_prev TYPE zfit0079-obj_key_prev,
        vl_tx_ndf       TYPE zfit0096-tx_ob08,
        vl_lifnr_str    TYPE string,
        v_dt_tmp        TYPE sy-datum,
        v_dt_tmp_01     TYPE sy-datum,
        v_dia_ant_util  TYPE sy-datum.

  DATA: wl_zles0145_0100_out TYPE zde_zles0145_0100_out.

  PERFORM inicia_variaveis.

*----------------------------------------------------------------------*
* Busca Parâmetros
*----------------------------------------------------------------------*
  SELECT SINGLE *
    INTO tg_0109
    FROM zfit0109.

  IF sy-subrc NE 0.
    vg_err_consulta = 'X'.
    MESSAGE 'Não encontrado Parametros de Fluxo de Caixa Previsto(ZFIT0109)' TYPE 'S'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* Busca Taxa Cambio
*----------------------------------------------------------------------*
  CREATE OBJECT obj_zcl_util_sd.

  MOVE sy-datum TO vl_gdatu.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_usd_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_usd_brl = 0.
    vg_err_consulta = 'X'.
    MESSAGE 'Taxa de Câmbio não encontrada(USD/BRL)!' TYPE 'S'.
    EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('ARS').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_usd_ars = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_usd_ars = 0.
    vg_err_consulta = 'X'.
    MESSAGE 'Taxa de Câmbio não encontrada(USD/ARS)!' TYPE 'S'.
    EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('EURX').
  obj_zcl_util_sd->set_waerk('EUR').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_eur_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_eur_brl = 0.
    vg_err_consulta = 'X'.
    MESSAGE 'Taxa de Câmbio não encontrada(EUR/BRL)!' TYPE 'S'.
    EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('EUR').
  obj_zcl_util_sd->set_tcurr('USD').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_eur_usd = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_eur_usd = 0.
    vg_err_consulta = 'X'.
    MESSAGE 'Taxa de Câmbio não encontrada(EUR/USD)!' TYPE 'S'.
    EXIT.
  ENDIF.


*----------------------------------------------------------------------*
* Busca Empresas
*----------------------------------------------------------------------*
  REFRESH: tg_bukrs.
  SELECT bukrs land1
    INTO TABLE tg_bukrs
    FROM t001
   WHERE bukrs IN s_bukrs.

  IF tg_bukrs[] IS INITIAL.
    vg_err_consulta = 'X'.
    MESSAGE 'Empresa(s) não encontrada(s)!' TYPE 'S'.
    EXIT.
  ELSE.
    SORT tg_bukrs BY bukrs.
  ENDIF.

  "Ini CS2017001994
  SELECT *
    FROM t001 INTO CORRESPONDING FIELDS OF TABLE tg_curr_inf
   WHERE bukrs IN s_bukrs.

  LOOP AT tg_curr_inf.
    CLEAR: wl_x001.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = tg_curr_inf-bukrs
      IMPORTING
        e_x001  = wl_x001.

    tg_curr_inf-hwae2 = wl_x001-hwae2.
    tg_curr_inf-hwae3 = wl_x001-hwae3.

    MODIFY tg_curr_inf.
  ENDLOOP.
  SORT tg_curr_inf BY bukrs.
  "Fim CS2017001994

  LOOP AT tg_bukrs.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = tg_bukrs-bukrs ) TO rg_bukrs_proc.
  ENDLOOP.


*----------------------------------------------------------------------*
* Busca dados VBSEGK
*----------------------------------------------------------------------*
  PERFORM load_vbsegk.

  LOOP AT tg_vbsegk.
    tg_vbsegk-venci = tg_vbsegk-zfbdt + tg_vbsegk-zbd1t.
    IF tg_vbsegk-venci IN it_dt_venc.

      IF tg_vbsegk-swaer IS INITIAL.
        tg_vbsegk-swaer = c_brl.
      ENDIF.

      PERFORM f_atrib_valor USING tg_vbsegk-bukrs
                                  tg_vbsegk-swaer
                                  tg_vbsegk-dmbtr
                                  tg_vbsegk-dmbe2
                                  tg_vbsegk-wrbtr
                                  0
                         CHANGING tg_vbsegk-xvlr
                                  tg_vbsegk-xvlu.

      tg_vbsegk-sistema_orig = 'SAP'.

      CLEAR: vl_lifnr_int, vl_lifnr_str.
      vl_lifnr_int = tg_vbsegk-lifnr.
      vl_lifnr_str = vl_lifnr_int.

      CONCATENATE 'PE' vl_lifnr_str INTO tg_vbsegk-processo_esp.

      MODIFY tg_vbsegk.

    ELSE.
      DELETE tg_vbsegk.
    ENDIF.
  ENDLOOP.


  IF tg_vbsegk[] IS NOT INITIAL.
    "Relaciona BKPF
    SELECT bukrs belnr gjahr usnam
      FROM bkpf APPENDING TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_vbsegk
     WHERE bukrs EQ tg_vbsegk-bukrs
       AND belnr EQ tg_vbsegk-belnr
       AND gjahr EQ tg_vbsegk-gjahr.
  ENDIF.

*----------------------------------------------------------------------*
* Busca dados BSIK
*----------------------------------------------------------------------*
  PERFORM load_bsik.

  LOOP AT tg_bsik.
    tg_bsik-venci = tg_bsik-zfbdt + tg_bsik-zbd1t.
    IF tg_bsik-venci IN it_dt_venc.

      PERFORM f_atrib_valor USING tg_bsik-bukrs
                                  tg_bsik-waers
                                  tg_bsik-dmbtr
                                  tg_bsik-dmbe2
                                  tg_bsik-wrbtr
                                  0
                         CHANGING tg_bsik-xvlr
                                  tg_bsik-xvlu.

      MODIFY tg_bsik.

    ELSE.
      DELETE tg_bsik.
    ENDIF.
  ENDLOOP.

  IF tg_bsik[] IS NOT INITIAL.

    "Relaciona BKPK
    SELECT bukrs belnr gjahr usnam
      FROM bkpf APPENDING TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_bsik
     WHERE bukrs EQ tg_bsik-bukrs
       AND belnr EQ tg_bsik-belnr
       AND gjahr EQ tg_bsik-gjahr.

    "Relaciona EKKO
    SELECT ebeln bsart
      FROM ekko APPENDING TABLE tg_ekko
       FOR ALL ENTRIES IN tg_bsik
     WHERE ebeln EQ tg_bsik-ebeln.

  ENDIF.

  "xblnr
  "ebelp
  "shkzg
  "ZLSCH
  "zlspr
  "hkont
  "sgtxt
  "hbkid
  "zfbdt
  "zbd1t
  "dmbtr
  "dmbe2
  "wrbtr
  "vbund
*----------------------------------------------------------------------*
* Busca dados BSAK
*----------------------------------------------------------------------*
  SELECT *
    FROM bsak
    INTO CORRESPONDING FIELDS OF TABLE tg_bsak
   WHERE bukrs IN s_bukrs
     AND shkzg EQ 'H'
     AND augdt >= sy-datum.
  IF sy-subrc IS INITIAL.
    DATA(lt_bsak) = tg_bsak[].
    SORT lt_bsak BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_bsak COMPARING lifnr.

    IF lt_bsak IS NOT INITIAL.

      SELECT lifnr land1
        FROM lfa1
        APPENDING TABLE t_lfa1
        FOR ALL ENTRIES IN lt_bsak
        WHERE lifnr = lt_bsak-lifnr.
      IF sy-subrc IS INITIAL.
        SORT t_lfa1 BY lifnr.
      ENDIF.

    ENDIF.
  ENDIF.
  LOOP AT tg_bsak.
    tg_bsak-venci = tg_bsak-zfbdt + tg_bsak-zbd1t.
    IF ( tg_bsak-augbl(2) EQ '15' ) OR ( tg_bsak-augbl(2) EQ '20' ).

      PERFORM f_atrib_valor USING tg_bsak-bukrs
                                  tg_bsak-waers
                                  tg_bsak-dmbtr
                                  tg_bsak-dmbe2
                                  tg_bsak-wrbtr
                                  0
                         CHANGING tg_bsak-xvlr
                                  tg_bsak-xvlu.
      MODIFY tg_bsak.

    ELSE.
      DELETE tg_bsak.
    ENDIF.
  ENDLOOP.

  IF tg_bsak[] IS NOT INITIAL.
    "Relaciona BKPK
    SELECT bukrs belnr gjahr usnam
      FROM bkpf APPENDING TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_bsak
     WHERE bukrs EQ tg_bsak-bukrs
       AND belnr EQ tg_bsak-belnr
       AND gjahr EQ tg_bsak-gjahr.

    "Relaciona EKKO
    SELECT ebeln bsart
      FROM ekko APPENDING TABLE tg_ekko
       FOR ALL ENTRIES IN tg_bsak
     WHERE ebeln EQ tg_bsak-ebeln.
  ENDIF.

*----------------------------------------------------------------------*
* Busca dados BSID
*----------------------------------------------------------------------*

  PERFORM load_bsid.

  LOOP AT tg_bsid.
    tg_bsid-venci = tg_bsid-zfbdt + tg_bsid-zbd1t.
    IF tg_bsid-venci IN it_dt_venc.

      PERFORM f_atrib_valor USING tg_bsid-bukrs
                                  tg_bsid-waers
                                  tg_bsid-dmbtr
                                  tg_bsid-dmbe2
                                  tg_bsid-wrbtr
                                  0
                         CHANGING tg_bsid-xvlr
                                  tg_bsid-xvlu.

      MODIFY tg_bsid.

    ELSE.
      DELETE tg_bsid.
    ENDIF.
  ENDLOOP.

  IF tg_bsid[] IS NOT INITIAL.

    "Relaciona BKPK
    SELECT bukrs belnr gjahr usnam
      FROM bkpf APPENDING TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_bsid
     WHERE bukrs EQ tg_bsid-bukrs
       AND belnr EQ tg_bsid-belnr
       AND gjahr EQ tg_bsid-gjahr.

    "Relaciona VBAK
    SELECT vbeln auart
      FROM vbak APPENDING TABLE tg_vbak
       FOR ALL ENTRIES IN tg_bsid
     WHERE vbeln EQ tg_bsid-vbel2.

  ENDIF.

*----------------------------------------------------------------------*
* Busca dados ZIMP Pagamentos
*----------------------------------------------------------------------*

  SELECT status_lote lote bukrs
    FROM zimp_cad_lote INTO TABLE tg_zimp_cad_lote
   WHERE status_lote  EQ 'L'
     AND bukrs        IN s_bukrs.

  IF tg_zimp_cad_lote[] IS NOT INITIAL.

    SELECT lote bukrs doc_imposto dt_venc waers usuario
      FROM zimp_lanc_impost INTO TABLE tg_zimp_lanc_impost
      FOR ALL ENTRIES IN tg_zimp_cad_lote
    WHERE bukrs	  EQ tg_zimp_cad_lote-bukrs
      AND lote    EQ tg_zimp_cad_lote-lote
      AND dt_venc IN it_dt_venc
      AND loekz   EQ space.

    IF tg_zimp_lanc_impost[] IS NOT INITIAL.

      SELECT doc_imposto bukrs seqitem bschl lifnr valor_imp valor_for gsber
        INTO TABLE tg_zimp_lanc_imp_ct
        FROM zimp_lanc_imp_ct
        FOR ALL ENTRIES IN tg_zimp_lanc_impost
       WHERE doc_imposto EQ	tg_zimp_lanc_impost-doc_imposto
         AND bukrs       EQ tg_zimp_lanc_impost-bukrs
         AND bschl       EQ  '31'.

    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* Buscar dados programação de pagamento do SIGAM
*----------------------------------------------------------------------*

  SELECT bukrs planilha planilha_itm lifnr waers dt_vcto gsber dmbtr dmbe2 tipo
         id_invoice ds_porto kursf processo_esp  sistema_orig
    INTO TABLE tg_0110
    FROM zfit0110
   WHERE bukrs   IN s_bukrs
     AND dt_vcto IN it_dt_venc.

  LOOP AT tg_0110.

    IF ( ( tg_0110-sistema_orig IS INITIAL ) OR
         ( tg_0110-processo_esp IS INITIAL ) ) AND ( tg_0110-tipo IS NOT INITIAL ).
      tg_0110-sistema_orig = 'SIGAM'.

      IF ( tg_0110-tipo = 'C' ).
        tg_0110-processo_esp = 'PG-COMPRA'.
      ELSEIF ( tg_0110-tipo = 'A' ).
        tg_0110-processo_esp = 'PG-ADTO'.
      ENDIF.
    ENDIF.

    IF ( tg_0110-sistema_orig IS INITIAL ) OR
       ( tg_0110-processo_esp IS INITIAL ).

      DELETE tg_0110.

    ELSE.

      "FI - Correção Programações SIGAM Bug Solto #165732 - WPP - Ini
      DATA(_converte_valor) = abap_true.

      IF ( tg_0110-sistema_orig = 'SIGAM' ) AND
         ( tg_0110-processo_esp EQ 'PG-COMPRA' OR tg_0110-processo_esp EQ 'PG-ADTO' ) AND
         ( tg_0110-dmbtr IS NOT INITIAL AND tg_0110-dmbe2 IS NOT INITIAL ).
        _converte_valor = abap_false. "Adiantamento/Pagamentos Produtores originados no Sigam na moeda USD , já vem convertido os valores de BRL/USD
      ENDIF.

      IF _converte_valor EQ abap_false.
        tg_0110-xvlr  = tg_0110-dmbtr.
        tg_0110-xvlu  = tg_0110-dmbe2.

        MODIFY tg_0110.
        CONTINUE.
      ENDIF.
      "FI - Correção Programações SIGAM Bug Solto #165732 - WPP - Fim

      PERFORM f_atrib_valor USING tg_0110-bukrs
                                  tg_0110-waers
                                  tg_0110-dmbtr
                                  tg_0110-dmbe2
                                  0
                                  tg_0110-kursf
                         CHANGING tg_0110-xvlr
                                  tg_0110-xvlu.

      MODIFY tg_0110.

    ENDIF.

  ENDLOOP.

*----------------------------------------------------------------------*
* Busca de Dados NDF
*----------------------------------------------------------------------*

  SELECT bukrs date_period_1 trade_id amount_dealt counter_amount
         cont_part_deal_c exch_rat_period1 side tx_ob08 aj_bruto_brl
    FROM zfit0096 INTO CORRESPONDING FIELDS OF TABLE tg_0096
   WHERE bukrs         IN s_bukrs
     AND date_period_1 IN it_dt_venc.

  SELECT bukrs date_period_1 trade_id amount_dealt counter_amount
         cont_part_deal_c exch_rat_period1 currency_1 side rev_trade
    INTO TABLE tg_0083
    FROM zfit0083
   WHERE bukrs IN s_bukrs
     AND date_period_1 IN it_dt_venc
     AND deal_type EQ '4'.

  "Check Estorno

  FIELD-SYMBOLS: <tg_0083>     LIKE tg_0083,
                 <tg_0083_aux> LIKE tg_0083.

  LOOP AT tg_0083 ASSIGNING <tg_0083>.
    IF <tg_0083>-rev_trade CS 'E'.

      READ TABLE tg_0083 ASSIGNING <tg_0083_aux> WITH KEY rev_trade = <tg_0083>-rev_trade+1(1)
                                                          trade_id  = <tg_0083>-trade_id.
      IF sy-subrc = 0.
        <tg_0083_aux>-rev_trade  = 'W'.
      ENDIF.

      <tg_0083>-rev_trade = 'W'.

    ENDIF.
  ENDLOOP.
  DELETE tg_0083 WHERE rev_trade EQ 'W'.

  LOOP AT tg_0096 WHERE currency_1 IS INITIAL.
    READ TABLE tg_0083 WITH KEY bukrs    = tg_0096-bukrs
                                trade_id = tg_0096-trade_id.
    IF sy-subrc EQ 0.
      tg_0096-currency_1 = tg_0083-currency_1.
      MODIFY tg_0096.
    ELSE.
      DELETE tg_0096.
    ENDIF.
  ENDLOOP.

  LOOP AT tg_0096.
    DELETE tg_0083 WHERE bukrs    EQ tg_0096-bukrs
                     AND trade_id EQ tg_0096-trade_id.
  ENDLOOP.

  LOOP AT tg_0083.

    CLEAR: vl_tx_ndf.

    READ TABLE tg_curr_inf WITH KEY bukrs = tg_0083-bukrs BINARY SEARCH.

    CHECK sy-subrc = 0.

    IF ( tg_curr_inf-waers = c_brl ) AND ( tg_curr_inf-hwae2 = c_usd ). " 1ª BRL / 2ª USD
      vl_tx_ndf = vg_tx_usd_brl.
    ELSEIF ( tg_curr_inf-waers = c_ars ) AND ( tg_curr_inf-hwae2 = c_usd ) . " 1ª ARS / 2ª USD
      vl_tx_ndf = vg_tx_usd_ars.
    ELSE.
      CONTINUE.
    ENDIF.

    CASE tg_0083-side.
      WHEN 'S'.
        tg_0083-xv1 = tg_0083-amount_dealt * ( tg_0083-exch_rat_period1 - vl_tx_ndf ).   "Valor R$  ->
      WHEN 'B'.
        tg_0083-xv1 = tg_0083-amount_dealt * ( vl_tx_ndf - tg_0083-exch_rat_period1 ).   "Valor R$  ->
    ENDCASE.

    "TG_0083-XV2 = TG_0083-COUNTER_AMOUNT - TG_0083-XV1.  "Valor R$
    "TG_0083-XV3 = TG_0083-XV2 / VG_TX_CAMBIO.            "Valor US

    IF ( tg_0083-xv1 > 0 ) AND ( tg_0083-bukrs NE '0100' ).
      tg_0083-xv4 = tg_0083-xv1 * '0.00005'.               "Valor R$
    ELSE.
      tg_0083-xv4 = 0.
    ENDIF.

    tg_0083-xv5 = tg_0083-xv1 - tg_0083-xv4.             "Valor R$ liquido
    tg_0083-xv6 = tg_0083-xv5 / vl_tx_ndf.                 "Valor US liquido

    IF tg_0083-xv6 >= 0.
      tg_0083-processo_esp = 'NDF-E'.
      tg_0083-sistema_orig = 'SAP'.
    ELSE.
      tg_0083-processo_esp = 'NDF-S'.
      tg_0083-sistema_orig = 'SAP'.
    ENDIF.

    MODIFY tg_0083.

  ENDLOOP.

  LOOP AT tg_0096.

    CLEAR: vl_tx_ndf.

    READ TABLE tg_curr_inf WITH KEY bukrs = tg_0096-bukrs BINARY SEARCH.

    CHECK sy-subrc = 0.

    IF ( tg_curr_inf-waers = c_brl ) AND ( tg_curr_inf-hwae2 = c_usd ). " 1ª BRL / 2ª USD
      vl_tx_ndf = vg_tx_usd_brl.
    ELSEIF ( tg_curr_inf-waers = c_ars ) AND ( tg_curr_inf-hwae2 = c_usd ) . " 1ª ARS / 2ª USD
      "VL_TX_NDF = VG_TX_USD_ARS.
      vl_tx_ndf = tg_0096-tx_ob08.
    ELSE.
      CONTINUE.
    ENDIF.

    CHECK vl_tx_ndf NE 0.

    CASE tg_0096-side.
      WHEN 'S'.
        tg_0096-xv1 = tg_0096-amount_dealt * ( tg_0096-exch_rat_period1 - vl_tx_ndf ).   "Valor R$  ->
      WHEN 'B'.
        tg_0096-xv1 = tg_0096-amount_dealt * ( vl_tx_ndf - tg_0096-exch_rat_period1 ).   "Valor R$  ->
    ENDCASE.

    IF tg_0096-bukrs = '0100'.
      tg_0096-xv1 = tg_0096-aj_bruto_brl.
    ENDIF.

    "TG_0096-XV2 = TG_0096-COUNTER_AMOUNT - TG_0096-XV1.  "Valor R$
    "TG_0096-XV3 = TG_0096-XV2 / VG_TX_CAMBIO.            "Valor US

    IF ( tg_0096-xv1 > 0 ) AND ( tg_0096-bukrs NE '0100' ).
      tg_0096-xv4 = tg_0096-xv1 * '0.00005'.               "Valor R$
    ELSE.
      tg_0096-xv4 = 0.
    ENDIF.

    tg_0096-xv5 = tg_0096-xv1 - tg_0096-xv4.             "Valor R$ liquido
    tg_0096-xv6 = tg_0096-xv5 / vl_tx_ndf.                 "Valor US liquido

    IF tg_0096-xv6 >= 0.
      tg_0096-processo_esp = 'NDF-E'.
      tg_0096-sistema_orig = 'SAP'.
    ELSE.
      tg_0096-processo_esp = 'NDF-S'.
      tg_0096-sistema_orig = 'SAP'.
    ENDIF.

    MODIFY tg_0096.

  ENDLOOP.

  SELECT bukrs banco_bloom kunnr lifnr hkont
    INTO TABLE tg_0094
    FROM zfit0094
   WHERE bukrs IN s_bukrs.

  LOOP AT tg_0094.
    tg_0094-bco_bloom_1 = tg_0094-banco_bloom(4).
    MODIFY tg_0094.
  ENDLOOP.

*----------------------------------------------------------------------*
* Buscar Lctos Previsões Manuais
*----------------------------------------------------------------------*
  SELECT *
    INTO TABLE tg_0115
    FROM zfit0115
   WHERE bukrs   IN s_bukrs
     AND dt_vcto IN it_dt_venc.

  LOOP AT tg_0115.

    PERFORM f_atrib_valor USING tg_0115-bukrs
                                tg_0115-waers
                                tg_0115-dmbtr
                                tg_0115-dmbe2
                                0
                                0
                       CHANGING tg_0115-dmbtr
                                tg_0115-dmbe2.

    MODIFY tg_0115.

  ENDLOOP.

*----------------------------------------------------------------------*
* Buscar Lctos XRT
*----------------------------------------------------------------------*

  SELECT bukrs opr_numero con_codigo data_vencimento mdo_codigo
         par_tipo mdo_tipo dmbtr dmbe2 waers
         bukrs_opr agente regra_val
    INTO TABLE tg_0112
    FROM zfit0112
   WHERE bukrs           IN s_bukrs
     AND data_vencimento IN it_dt_venc
     AND mdo_tipo        NE 'S'.

  LOOP AT tg_0112.

    tg_0112-sistema_orig = 'XRT'.

    IF tg_0112-dmbtr NE 0.
      vl_wrbtr = tg_0112-dmbtr.
    ELSEIF tg_0112-dmbe2 NE 0 .
      vl_wrbtr = tg_0112-dmbe2.
    ELSE.
      vl_wrbtr = 0.
    ENDIF.

    PERFORM f_atrib_valor USING tg_0112-bukrs
                                tg_0112-waers
                                tg_0112-dmbtr
                                tg_0112-dmbe2
                                vl_wrbtr
                                0
                       CHANGING tg_0112-xvlr
                                tg_0112-xvlu.

    IF tg_0112-xvlr >= 0.
      vl_ent_sai = 'E'.
    ELSE.
      vl_ent_sai = 'S'.
    ENDIF.

    tg_0112-xvlr = abs( tg_0112-xvlr ).
    tg_0112-xvlu = abs( tg_0112-xvlu ).

    CONCATENATE 'XRT-' tg_0112-mdo_tipo '-' tg_0112-waers  INTO tg_0112-processo_esp.
    CONCATENATE 'XRT-' tg_0112-mdo_tipo '-' vl_ent_sai     INTO tg_0112-processo_esp02.
    CONCATENATE 'XRT-' tg_0112-mdo_tipo '-' tg_0112-waers '-' vl_ent_sai
           INTO tg_0112-processo_esp03.
    CONCATENATE 'XRT-' tg_0112-mdo_tipo INTO tg_0112-processo_esp04.

    MODIFY tg_0112.

  ENDLOOP.

  "Resgate Aplicações - Sobra Caixa.
  SELECT bukrs opr_numero con_codigo data_vencimento mdo_codigo
         par_tipo mdo_tipo dmbtr dmbe2 waers
         bukrs_opr agente regra_val
    INTO TABLE tg_0112_sld_aplic
    FROM zfit0112
   WHERE bukrs           IN s_bukrs
     AND mdo_tipo        EQ 'S'.

  LOOP AT tg_0112_sld_aplic.

    tg_0112_sld_aplic-sistema_orig = 'XRT'.

    CONCATENATE 'XRT-' tg_0112_sld_aplic-mdo_tipo INTO tg_0112_sld_aplic-processo_esp.

    IF tg_0112_sld_aplic-dmbtr NE 0.
      vl_wrbtr = tg_0112_sld_aplic-dmbtr.
    ELSEIF tg_0112_sld_aplic-dmbe2 NE 0 .
      vl_wrbtr = tg_0112_sld_aplic-dmbe2.
    ELSE.
      vl_wrbtr = 0.
    ENDIF.

    PERFORM f_atrib_valor USING tg_0112_sld_aplic-bukrs
                                tg_0112_sld_aplic-waers
                                tg_0112_sld_aplic-dmbtr
                                tg_0112_sld_aplic-dmbe2
                                vl_wrbtr
                                0
                       CHANGING tg_0112_sld_aplic-xvlr
                                tg_0112_sld_aplic-xvlu.

    tg_0112_sld_aplic-xvlr  = abs( tg_0112_sld_aplic-xvlr ).
    tg_0112_sld_aplic-xvlu  = abs( tg_0112_sld_aplic-xvlu ).

    MODIFY tg_0112_sld_aplic.

  ENDLOOP.

*----------------------------------------------------------------------*
* Buscar Lctos Adiantamento
*----------------------------------------------------------------------*
  SELECT *
    INTO TABLE tg_0045
    FROM zfit0045
   WHERE bukrs       IN s_bukrs
     AND dt_pgto     IN it_dt_venc
     AND status      NE 'A'.

  IF tg_0045[] IS NOT INITIAL.

    SELECT nro_sol ebeln ebelp vlr_adiantamento
      INTO TABLE tg_0046
      FROM zfit0046
      FOR ALL ENTRIES IN tg_0045
     WHERE nro_sol  EQ tg_0045-nro_sol.

    LOOP AT tg_0046.

      READ TABLE tg_0045 WITH KEY nro_sol = tg_0046-nro_sol.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      tg_0046-sistema_orig = 'SAP'.
      tg_0046-processo_esp = 'ADTO'.

*---> 25/05/2023 - Migração S4 - JS
      DATA: vlr_adiantamento TYPE dmbtr.

      vlr_adiantamento = CONV #( tg_0046-vlr_adiantamento ).
*<--- 25/05/2023 - Migração S4 - JS

      PERFORM f_atrib_valor USING tg_0045-bukrs
                                  tg_0045-moeda_pgto
*---> 25/05/2023 - Migração S4 - JS
*                                  tg_0046-vlr_adiantamento
*                                  tg_0046-vlr_adiantamento
*                                  tg_0046-vlr_adiantamento
                                   vlr_adiantamento
                                   vlr_adiantamento
                                   vlr_adiantamento
*<--- 25/05/2023 - Migração S4 - JS

                                  0
                         CHANGING tg_0046-xvlr
                                  tg_0046-xvlu.

      MODIFY tg_0046.

    ENDLOOP.

  ENDIF.

*----------------------------------------------------------------------*
* Buscar Lctos Adiantamento Frete
*----------------------------------------------------------------------*

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE tg_zlest0140
    FROM zlest0140
   WHERE bukrs       IN s_bukrs
     AND data        IN it_dt_venc.

  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE tg_zlest0141
   FROM zlest0141
  WHERE bukrs       IN s_bukrs
    AND data        IN it_dt_venc.

  LOOP AT tg_zlest0140.

    v_dt_tmp = tg_zlest0140-data.

    PERFORM f_proximo_dia_util CHANGING v_dt_tmp.
    IF v_dt_tmp NE tg_zlest0140-data. "Não é dia Util
      CONTINUE.
    ENDIF.

    DATA(_adto_gerado) = abap_false.

    IF tg_zlest0140-data = sy-datum.

      READ TABLE tg_zlest0141 WITH KEY bukrs = tg_zlest0140-bukrs
                                       data  = tg_zlest0140-data.

      IF ( sy-subrc EQ 0 ).

        CLEAR: wl_zles0145_0100_out.

        IF ( tg_zlest0141-lote         IS INITIAL AND  "Não gerou adiantamento
             tg_zlest0141-sem_sld_pgto IS INITIAL ).   "Possui saldo Pagar

          CLEAR: wl_zles0145_0100_out.

          wl_zles0145_0100_out-bukrs             =  tg_zlest0141-bukrs.
          wl_zles0145_0100_out-lifnr             =  tg_zlest0141-lifnr.
          wl_zles0145_0100_out-data              =  tg_zlest0141-data.
          wl_zles0145_0100_out-vlr_mov           =  tg_zlest0141-vlr_mov.
          wl_zles0145_0100_out-saldo_frete       =  tg_zlest0141-saldo_frete.
          wl_zles0145_0100_out-saldo_estorno     =  tg_zlest0141-saldo_estorno.
          wl_zles0145_0100_out-saldo_outros     =  tg_zlest0141-saldo_outros.
          wl_zles0145_0100_out-sem_sld_pgto      =  tg_zlest0141-sem_sld_pgto.
          wl_zles0145_0100_out-usuario           =  tg_zlest0141-usuario.
          wl_zles0145_0100_out-vlr_prev_adt      =  tg_zlest0140-valor.
          wl_zles0145_0100_out-lifnr              = tg_zlest0140-lifnr.

          PERFORM f_calc_valores_pgto IN PROGRAM zlesr0109 IF FOUND
               USING ' '
            CHANGING wl_zles0145_0100_out.

          tg_zlest0140-valor = wl_zles0145_0100_out-vlr_pgto_adt.
          _adto_gerado = abap_true.

        ELSE.
          tg_zlest0140-valor = tg_zlest0141-vlr_pgto_adt.
          _adto_gerado = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.

    tg_zlest0140-xvlr  =  tg_zlest0140-valor.

    IF _adto_gerado = abap_false.
      "Adicionar Previsoes proximos dias não uteis
      DATA(_ok) = abap_false.
      CLEAR: v_dt_tmp, v_dt_tmp_01.
      WHILE _ok EQ abap_false.

        IF v_dt_tmp IS INITIAL.
          v_dt_tmp = tg_zlest0140-data.
        ENDIF.

        ADD 1 TO v_dt_tmp.
        v_dt_tmp_01 = v_dt_tmp.

        PERFORM f_proximo_dia_util CHANGING v_dt_tmp_01.
        IF v_dt_tmp_01 <> v_dt_tmp.
          SELECT SINGLE *
            FROM zlest0140 INTO @DATA(_wl_0140_tmp)
           WHERE bukrs = @tg_zlest0140-bukrs
             AND data  = @v_dt_tmp.
          IF sy-subrc NE 0.
            _ok = abap_true.
          ELSE.
            ADD _wl_0140_tmp-valor TO tg_zlest0140-xvlr.
          ENDIF.
        ELSE.
          _ok = abap_true.
        ENDIF.
      ENDWHILE.
    ENDIF.

    tg_zlest0140-xvlu  =  tg_zlest0140-xvlr / vg_tx_usd_brl.

    tg_zlest0140-processo_esp = 'ADTO-FRETE'.
    tg_zlest0140-sistema_orig = 'SAP'.

    MODIFY tg_zlest0140.

  ENDLOOP.

*----------------------------------------------------------------------*
* Buscar Lctos Saldo Frete
*----------------------------------------------------------------------*

  SELECT *
    FROM zlest0141_resumo INTO CORRESPONDING FIELDS OF TABLE tg_0141_sld_fre
   WHERE bukrs          IN s_bukrs
     AND dt_saldo_frete IN it_dt_venc.

  LOOP AT tg_0141_sld_fre.
    CLEAR: tg_0141_sld_fre_grp.

    MOVE-CORRESPONDING tg_0141_sld_fre TO tg_0141_sld_fre_grp.
    APPEND tg_0141_sld_fre_grp.
  ENDLOOP.

  SORT tg_0141_sld_fre_grp BY bukrs dt_saldo_frete lifnr.
  DELETE ADJACENT DUPLICATES FROM tg_0141_sld_fre_grp COMPARING bukrs dt_saldo_frete lifnr.

  LOOP AT tg_0141_sld_fre_grp.
    CLEAR: tg_0141_sld_fre_grp-saldo_frete.

    LOOP AT tg_0141_sld_fre WHERE bukrs          =  tg_0141_sld_fre_grp-bukrs
                              AND dt_saldo_frete =  tg_0141_sld_fre_grp-dt_saldo_frete
                              AND lifnr          =  tg_0141_sld_fre_grp-lifnr.
      ADD tg_0141_sld_fre-saldo_frete TO tg_0141_sld_fre_grp-saldo_frete.
    ENDLOOP.

    tg_0141_sld_fre_grp-xvlr  = tg_0141_sld_fre_grp-saldo_frete.

    IF tg_0141_sld_fre_grp-xvlr > 0.
      tg_0141_sld_fre_grp-xvlu  =  tg_0141_sld_fre_grp-xvlr / vg_tx_usd_brl.
    ENDIF.

    tg_0141_sld_fre_grp-processo_esp = 'SALDO-FRETE'.
    tg_0141_sld_fre_grp-sistema_orig = 'SAP'.

    MODIFY tg_0141_sld_fre_grp.
  ENDLOOP.


*----------------------------------------------------------------------*
* Buscar Lctos Frete - Outros
*----------------------------------------------------------------------*
  SELECT *
    FROM zlest0141_lote AS zlote
    LEFT JOIN zlest0141_l_item AS zitem
    ON zlote~nm_lote = zitem~nm_lote AND
      zlote~nr_lote_adm = zitem~nr_lote_adm
    INTO CORRESPONDING FIELDS OF TABLE @tg_0141_outros
    WHERE zlote~bukrs   IN @s_bukrs
      AND zitem~dt_util IN @it_dt_venc.

  LOOP AT tg_0141_outros.
    CLEAR: tg_0141_outros_grp.

    MOVE-CORRESPONDING tg_0141_outros TO tg_0141_outros_grp.
    APPEND tg_0141_outros_grp.
  ENDLOOP.

  SORT tg_0141_outros_grp BY bukrs dt_util.
  DELETE ADJACENT DUPLICATES FROM tg_0141_outros_grp COMPARING bukrs dt_util.

  LOOP AT tg_0141_outros_grp.
    CLEAR: tg_0141_outros_grp-vl_pago_lote.

    LOOP AT tg_0141_outros WHERE bukrs    =  tg_0141_outros_grp-bukrs
                              AND dt_util =  tg_0141_outros_grp-dt_util.

      ADD tg_0141_outros-vl_pago_lote TO tg_0141_outros_grp-vl_pago_lote.
    ENDLOOP.

    tg_0141_outros_grp-xvlr  = tg_0141_outros_grp-vl_pago_lote.

    IF tg_0141_outros_grp-xvlr > 0.
      tg_0141_outros_grp-xvlu  =  tg_0141_outros_grp-xvlr / vg_tx_usd_brl.
    ENDIF.

    tg_0141_outros_grp-processo_esp = 'FRETE-OUTROS'.
    tg_0141_outros_grp-sistema_orig = 'SAP'.

    MODIFY tg_0141_outros_grp.
  ENDLOOP.

*--------------------------------------------------------------------------*
* Previsões de Recebimento a partir de Lançamentos de Pagamento
*--------------------------------------------------------------------------*
  PERFORM load_bsik_for_bsid.

  LOOP AT tg_bsik_for_bsid.
    tg_bsik_for_bsid-venci = tg_bsik_for_bsid-zfbdt + tg_bsik_for_bsid-zbd1t.

    IF tg_bsik_for_bsid-venci IN it_dt_venc.

      PERFORM f_atrib_valor USING tg_bsik_for_bsid-bukrs
                                  tg_bsik_for_bsid-waers
                                  tg_bsik_for_bsid-dmbtr
                                  tg_bsik_for_bsid-dmbe2
                                  tg_bsik_for_bsid-wrbtr
                                  0
                         CHANGING tg_bsik_for_bsid-xvlr
                                  tg_bsik_for_bsid-xvlu.

      tg_bsik_for_bsid-processo_esp = 'REC_X_PGTO_AUTO'.
      tg_bsik_for_bsid-sistema_orig = 'SAP'.

      MODIFY tg_bsik_for_bsid.

    ELSE.
      DELETE tg_bsik_for_bsid.
    ENDIF.
  ENDLOOP.



ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  DATA: vl_found_par     TYPE c,
        vl_codigo        TYPE zfit0109-codigo,
        vl_clas_flx      TYPE zfit0109-clas_flx,
        vl_seq           TYPE zfit0109-seq,
        vl_tipo_doc      TYPE string,
        vl_tipo_ped      TYPE string,
        vl_tipo_ov       TYPE string,
        vl_forma_pgto    TYPE dzlsch,
        vl_bloq_pgto     TYPE string,
        vl_processo_esp  TYPE zfit0109-processo_esp,
        vl_sistema_orig  TYPE zfit0109-sistema_orig,
        vl_proc_esp_xrt  TYPE zfit0109-processo_esp,
        vl_obj_key_prev  TYPE zfit0079-obj_key_prev,
        vl_seq_item_prev TYPE zfit0079-seqitem.

*----------------------------------------------------------------------*
* Processamento dados BSIK
*----------------------------------------------------------------------*
  LOOP AT tg_bsik.

    CLEAR: vl_found_par,
           vl_tipo_doc,
           vl_tipo_ped,
           vl_bloq_pgto,
           vl_forma_pgto,
           tg_ekko,
           tg_0079.

    READ TABLE tg_ekko WITH KEY ebeln = tg_bsik-ebeln.

    IF ( sy-subrc = 0 ) AND ( tg_ekko-bsart IS NOT INITIAL ).
      CONCATENATE '%\' tg_ekko-bsart '\%' INTO vl_tipo_ped.
    ENDIF.

    IF tg_bsik-blart IS NOT INITIAL.
      CONCATENATE '%\' tg_bsik-blart '\%' INTO vl_tipo_doc.
    ENDIF.

    IF tg_bsik-zlspr IS NOT INITIAL.
      CONCATENATE '%\' tg_bsik-zlspr '\%' INTO vl_bloq_pgto.
    ENDIF.

    IF tg_bsik-zlsch IS NOT INITIAL.
      vl_forma_pgto = tg_bsik-zlsch.
    ENDIF.

    PERFORM get_par_flx  USING  vl_tipo_doc
                                vl_tipo_ped
                                ''
                                vl_bloq_pgto
                                vl_forma_pgto
                                tg_bsik-zuonr
                                tg_bsik-vbund  "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                abap_true      "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                'F'
                       CHANGING tg_0109
                                vl_found_par.

    IF ( vl_found_par IS INITIAL ).
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    IF ( ( tg_0109-bco_empresa IS NOT INITIAL ) AND ( tg_bsik-hbkid IS INITIAL ) )
        OR
       ( tg_0109-bco_empresa IS INITIAL ) AND ( tg_bsik-hbkid IS NOT INITIAL ) ..
      CONTINUE.
    ENDIF.

    READ TABLE tg_bkpf WITH KEY bukrs = tg_bsik-bukrs
                                gjahr = tg_bsik-gjahr
                                belnr = tg_bsik-belnr.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>)
    WITH KEY bukrs = tg_bsik-bukrs
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE t_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
      WITH KEY lifnr = tg_bsik-lifnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF <fs_lfa1>-land1 <> <fs_bukrs>-land1.
          tg_0079-cx_internacional = 'X'.
        ENDIF.

      ENDIF.

    ENDIF.

    MOVE:  tg_bsik-bukrs             TO tg_0079-bukrs,
           tg_bsik-lifnr             TO tg_0079-lifnr,
           tg_bsik-belnr             TO tg_0079-belnr,
           tg_bsik-gjahr             TO tg_0079-gjahr,
           tg_bsik-buzei             TO tg_0079-buzei,
           tg_bsik-budat             TO tg_0079-budat,
           tg_bsik-venci             TO tg_0079-zfbdt,
           tg_bsik-hkont             TO tg_0079-hkont,
           tg_bsik-blart             TO tg_0079-blart,
           tg_bsik-bldat             TO tg_0079-bldat,
           tg_bsik-waers             TO tg_0079-waers,
           tg_bsik-xblnr             TO tg_0079-xblnr,
           tg_bsik-blart             TO tg_0079-blart,
           tg_bsik-gsber             TO tg_0079-gsber,
           tg_bsik-ebeln             TO tg_0079-ebeln,
           tg_bsik-ebelp             TO tg_0079-ebelp,
           tg_bsik-bschl             TO tg_0079-bschl,
           tg_bsik-shkzg             TO tg_0079-shkzg,
           tg_bsik-vbund             TO tg_0079-vbund,  "*-CS2022000133-#74201-05.05.2022-JT-inicio
           tg_bsik-xvlr              TO tg_0079-dmbtr,
           tg_bsik-xvlu              TO tg_0079-dmbe2,
           tg_bkpf-usnam             TO tg_0079-usnam,
           sy-datum                  TO tg_0079-dt_atual,
           sy-uzeit                  TO tg_0079-hr_atual,
           "Parametro
           sy-uname                  TO tg_0079-us_proc,
           tg_bsik-zlspr             TO tg_0079-zlspr,
           tg_bsik-zlsch             TO tg_0079-zlsch,
           tg_bsik-hbkid             TO tg_0079-hbkid,
           tg_0109-codigo            TO tg_0079-codigo,
           tg_0109-clas_flx          TO tg_0079-clas_flx,
           tg_0109-seq               TO tg_0079-seq,
           tg_0109-st_calc_sdo       TO tg_0079-st_calc_sdo,
           tg_ekko-bsart             TO tg_0079-bsart,
           ''                        TO tg_0079-processo_esp,
           'SAP'                     TO tg_0079-sistema_orig,
           tg_0109-tp_prev           TO tg_0079-tp_prev.

    APPEND: tg_0079.
    CLEAR: tg_0079, tg_bsik , tg_ekko.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados BSAK
*----------------------------------------------------------------------*
  LOOP AT tg_bsak.

    CLEAR: vl_found_par,
           vl_tipo_doc,
           vl_tipo_ped,
           vl_bloq_pgto,
           vl_forma_pgto,
           tg_ekko,
           tg_0079.

    READ TABLE tg_ekko WITH KEY ebeln = tg_bsak-ebeln.

    IF ( sy-subrc = 0 ) AND ( tg_ekko-bsart IS NOT INITIAL ).
      CONCATENATE '%\' tg_ekko-bsart '\%' INTO vl_tipo_ped.
    ENDIF.

    IF tg_bsak-blart IS NOT INITIAL.
      CONCATENATE '%\' tg_bsak-blart '\%' INTO vl_tipo_doc.
    ENDIF.

    IF tg_bsak-zlspr IS NOT INITIAL.
      CONCATENATE '%\' tg_bsak-zlspr '\%' INTO vl_bloq_pgto.
    ENDIF.

    IF tg_bsak-zlsch IS NOT INITIAL.
      vl_forma_pgto = tg_bsak-zlsch.
    ENDIF.

    PERFORM get_par_flx  USING  vl_tipo_doc
                                vl_tipo_ped
                                ''
                                vl_bloq_pgto
                                vl_forma_pgto
                                tg_bsak-zuonr
                                tg_bsak-vbund  "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                abap_true      "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                'F'
                       CHANGING tg_0109
                                vl_found_par.

    IF ( vl_found_par IS INITIAL ).
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    IF ( ( tg_0109-bco_empresa IS NOT INITIAL ) AND ( tg_bsak-hbkid IS INITIAL ) )
        OR
       ( tg_0109-bco_empresa IS INITIAL ) AND ( tg_bsak-hbkid IS NOT INITIAL ) .
      CONTINUE.
    ENDIF.

    READ TABLE tg_bkpf WITH KEY bukrs = tg_bsak-bukrs
                                gjahr = tg_bsak-gjahr
                                belnr = tg_bsak-belnr.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bukrs ASSIGNING <fs_bukrs>
    WITH KEY bukrs = tg_bsak-bukrs
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE t_lfa1 ASSIGNING <fs_lfa1>
      WITH KEY lifnr = tg_bsak-lifnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF <fs_lfa1>-land1 <> <fs_bukrs>-land1.
          tg_0079-cx_internacional = 'X'.
        ENDIF.

      ENDIF.
    ENDIF.

    MOVE:  tg_bsak-bukrs             TO tg_0079-bukrs,
           tg_bsak-lifnr             TO tg_0079-lifnr,
           tg_bsak-belnr             TO tg_0079-belnr,
           tg_bsak-gjahr             TO tg_0079-gjahr,
           tg_bsak-augbl             TO tg_0079-augbl,
           tg_bsak-augdt             TO tg_0079-augdt,
           tg_bsak-buzei             TO tg_0079-buzei,
           tg_bsak-budat             TO tg_0079-budat,
           tg_bsak-venci             TO tg_0079-zfbdt,
           tg_bsak-hkont             TO tg_0079-hkont,
           tg_bsak-blart             TO tg_0079-blart,
           tg_bsak-bldat             TO tg_0079-bldat,
           tg_bsak-waers             TO tg_0079-waers,
           tg_bsak-xblnr             TO tg_0079-xblnr,
           tg_bsak-blart             TO tg_0079-blart,
           tg_bsak-gsber             TO tg_0079-gsber,
           tg_bsak-ebeln             TO tg_0079-ebeln,
           tg_bsak-ebelp             TO tg_0079-ebelp,
           tg_bsak-bschl             TO tg_0079-bschl,
           tg_bsak-shkzg             TO tg_0079-shkzg,
           tg_bsak-vbund             TO tg_0079-vbund,  "*-CS2022000133-#74201-05.05.2022-JT-inicio
           tg_bsak-xvlr              TO tg_0079-dmbtr,
           tg_bsak-xvlu              TO tg_0079-dmbe2,
           tg_bkpf-usnam             TO tg_0079-usnam,
           sy-datum                  TO tg_0079-dt_atual,
           sy-uzeit                  TO tg_0079-hr_atual,
           "Parametro
           sy-uname                  TO tg_0079-us_proc,
           tg_bsak-zlspr             TO tg_0079-zlspr,
           tg_bsak-zlsch             TO tg_0079-zlsch,
           tg_bsak-hbkid             TO tg_0079-hbkid,
           tg_0109-codigo            TO tg_0079-codigo,
           tg_0109-clas_flx          TO tg_0079-clas_flx,
           tg_0109-seq               TO tg_0079-seq,
           tg_0109-st_calc_sdo       TO tg_0079-st_calc_sdo,
           tg_ekko-bsart             TO tg_0079-bsart,
           ''                        TO tg_0079-processo_esp,
           'SAP'                     TO tg_0079-sistema_orig,
           tg_0109-tp_prev           TO tg_0079-tp_prev.

    APPEND: tg_0079.
    CLEAR: tg_0079, tg_bsak , tg_ekko.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados BSID
*----------------------------------------------------------------------*

  LOOP AT tg_bsid.

    CLEAR: vl_found_par,
           vl_tipo_doc,
           vl_tipo_ov,
           vl_bloq_pgto,
           vl_forma_pgto,
           tg_vbak,
           tg_0079.

    READ TABLE tg_vbak WITH KEY vbeln = tg_bsid-vbel2.

    IF ( sy-subrc = 0 ) AND (  tg_vbak-auart IS NOT INITIAL ).
      CONCATENATE '%\' tg_vbak-auart '\%' INTO vl_tipo_ov.
    ENDIF.

    IF tg_bsid-blart IS NOT INITIAL.
      CONCATENATE '%\' tg_bsid-blart '\%' INTO vl_tipo_doc.
    ENDIF.

    IF tg_bsid-zlspr IS NOT INITIAL.
      CONCATENATE '%\' tg_bsid-zlspr '\%' INTO vl_bloq_pgto.
    ENDIF.

    IF tg_bsid-zlsch IS NOT INITIAL.
      vl_forma_pgto = tg_bsid-zlsch.
    ENDIF.

    PERFORM get_par_flx  USING  vl_tipo_doc
                                ''
                                vl_tipo_ov
                                vl_bloq_pgto
                                vl_forma_pgto
                                tg_bsid-zuonr
                                tg_bsid-vbund  "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                abap_true      "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                'C'
                       CHANGING tg_0109
                                vl_found_par.

    IF ( vl_found_par IS INITIAL ).
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    IF ( ( tg_0109-bco_empresa IS NOT INITIAL ) AND ( tg_bsid-hbkid IS INITIAL ) )
        OR
       ( tg_0109-bco_empresa IS INITIAL ) AND ( tg_bsid-hbkid IS NOT INITIAL ) ..
      CONTINUE.
    ENDIF.

    READ TABLE tg_bkpf WITH KEY bukrs = tg_bsid-bukrs
                                gjahr = tg_bsid-gjahr
                                belnr = tg_bsid-belnr.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bukrs[] ASSIGNING <fs_bukrs>
    WITH KEY bukrs = tg_bsid-bukrs
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE t_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>)
      WITH KEY kunnr = tg_bsid-kunnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF <fs_kna1>-land1 <> <fs_bukrs>-land1.
          tg_0079-cx_internacional = 'X'.
        ENDIF.

      ENDIF.

    ENDIF.

    MOVE:  tg_bsid-bukrs             TO tg_0079-bukrs,
           tg_bsid-kunnr             TO tg_0079-kunnr,
           tg_bsid-belnr             TO tg_0079-belnr,
           tg_bsid-gjahr             TO tg_0079-gjahr,
           tg_bsid-buzei             TO tg_0079-buzei,
           tg_bsid-budat             TO tg_0079-budat,
           tg_bsid-venci             TO tg_0079-zfbdt,
           tg_bsid-hkont             TO tg_0079-hkont,
           tg_bsid-blart             TO tg_0079-blart,
           tg_bsid-bldat             TO tg_0079-bldat,
           tg_bsid-waers             TO tg_0079-waers,
           tg_bsid-xblnr             TO tg_0079-xblnr,
           tg_bsid-blart             TO tg_0079-blart,
           tg_bsid-gsber             TO tg_0079-gsber,
           tg_bsid-vbel2             TO tg_0079-vbel2,
           tg_bsid-bschl             TO tg_0079-bschl,
           tg_bsid-shkzg             TO tg_0079-shkzg,
           tg_bsid-vbund             TO tg_0079-vbund,  "*-CS2022000133-#74201-05.05.2022-JT-inicio
           tg_bsid-xvlr              TO tg_0079-dmbtr,
           tg_bsid-xvlu              TO tg_0079-dmbe2,
           tg_bkpf-usnam             TO tg_0079-usnam,
           sy-datum                  TO tg_0079-dt_atual,
           sy-uzeit                  TO tg_0079-hr_atual,
           "Parametro
           sy-uname                  TO tg_0079-us_proc,
           tg_bsid-zlspr             TO tg_0079-zlspr,
           tg_bsid-zlsch             TO tg_0079-zlsch,
           tg_bsid-hbkid             TO tg_0079-hbkid,
           tg_0109-codigo            TO tg_0079-codigo,
           tg_0109-clas_flx          TO tg_0079-clas_flx,
           tg_0109-seq               TO tg_0079-seq,
           tg_0109-st_calc_sdo       TO tg_0079-st_calc_sdo,
           tg_vbak-auart             TO tg_0079-auart,
           ''                        TO tg_0079-processo_esp,
           'SAP'                     TO tg_0079-sistema_orig,
           tg_0109-tp_prev           TO tg_0079-tp_prev.

    APPEND: tg_0079.
    CLEAR: tg_0079, tg_bsid, tg_vbak.


  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados ZIMP Pagamentos
*----------------------------------------------------------------------*

  CLEAR: vl_found_par, vl_processo_esp, vl_sistema_orig.

  vl_processo_esp  = 'ZIMP'.
  vl_sistema_orig  = 'SAP'.

  SELECT SINGLE *
    INTO tg_0109
    FROM zfit0109
   WHERE processo_esp  EQ vl_processo_esp
     AND sistema_orig  EQ vl_sistema_orig.

  IF sy-subrc = 0.
    vl_found_par = 'X'.
  ENDIF.

  IF ( vl_found_par IS NOT INITIAL ) AND ( tg_0109-ocultar IS INITIAL ).

    LOOP AT tg_zimp_lanc_imp_ct.

      CLEAR: tg_0079.

      tg_zimp_lanc_imp_ct-processo_esp = vl_processo_esp.
      tg_zimp_lanc_imp_ct-sistema_orig = vl_sistema_orig.

      READ TABLE tg_zimp_lanc_impost WITH KEY doc_imposto = tg_zimp_lanc_imp_ct-doc_imposto
                                              bukrs       = tg_zimp_lanc_imp_ct-bukrs.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE tg_zimp_cad_lote WITH KEY bukrs = tg_zimp_lanc_impost-bukrs
                                           lote  = tg_zimp_lanc_impost-lote.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

*---> 26/05/2023 - Migração S4 - JS
      DATA: vlr_valor_imp TYPE zfit0079-dmbtr.

      DATA: aux_vlr_string   TYPE string.
      DATA: aux_vlr_decimal  TYPE p DECIMALS 2.

      aux_vlr_string = tg_zimp_lanc_imp_ct-valor_imp.
      TRANSLATE aux_vlr_string USING ', '.
      CONDENSE aux_vlr_string NO-GAPS.

      aux_vlr_decimal = aux_vlr_string.

      vlr_valor_imp = aux_vlr_decimal .


      CLEAR: aux_vlr_decimal ,aux_vlr_string.

      "vlr_valor_imp = conv #( tg_zimp_lanc_imp_ct-valor_imp ).

*<--- 26/05/2023 - Migração S4 - JS

      PERFORM f_atrib_valor USING tg_zimp_cad_lote-bukrs
                                  tg_zimp_lanc_impost-waers
*---> 26/05/2023 - Migração S4 - JS
                                  "tg_zimp_lanc_imp_ct-valor_imp
                                  vlr_valor_imp
*<--- 26/05/2023 - Migração S4 - JS
                                  tg_zimp_lanc_imp_ct-valor_for
                                  0
                                  0
                         CHANGING tg_zimp_lanc_imp_ct-xvlr
                                  tg_zimp_lanc_imp_ct-xvlu.

      MOVE:  tg_zimp_cad_lote-bukrs             TO tg_0079-bukrs,
             tg_zimp_lanc_imp_ct-lifnr          TO tg_0079-lifnr,
             tg_zimp_lanc_impost-dt_venc        TO tg_0079-zfbdt,
             tg_zimp_lanc_impost-waers          TO tg_0079-waers,
             tg_zimp_lanc_imp_ct-gsber          TO tg_0079-gsber,
             abs( tg_zimp_lanc_imp_ct-xvlr )    TO tg_0079-dmbtr,
             abs( tg_zimp_lanc_imp_ct-xvlu )    TO tg_0079-dmbe2,
             tg_zimp_lanc_impost-usuario        TO tg_0079-usnam,
             sy-datum                           TO tg_0079-dt_atual,
             sy-uzeit                           TO tg_0079-hr_atual,
             "Parametro
             sy-uname                           TO tg_0079-us_proc,
             tg_zimp_lanc_impost-doc_imposto    TO tg_0079-doc_imposto,
             tg_zimp_lanc_imp_ct-seqitem        TO tg_0079-seqitem,
             tg_zimp_cad_lote-lote              TO tg_0079-lote,
             ''                                 TO tg_0079-planilha,
             tg_0109-codigo                     TO tg_0079-codigo,
             tg_0109-clas_flx                   TO tg_0079-clas_flx,
             tg_0109-seq                        TO tg_0079-seq,
             tg_0109-st_calc_sdo                TO tg_0079-st_calc_sdo,
             tg_zimp_lanc_imp_ct-processo_esp   TO tg_0079-processo_esp,
             tg_zimp_lanc_imp_ct-sistema_orig   TO tg_0079-sistema_orig,
             tg_0109-tp_prev                    TO tg_0079-tp_prev.

      APPEND: tg_0079.
      CLEAR: tg_0079.


    ENDLOOP.

  ENDIF.

*----------------------------------------------------------------------*
* Processamento dados programação de pagamento do SIGAM
*----------------------------------------------------------------------*

  LOOP AT tg_0110.

    CLEAR: tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0110-processo_esp
       AND sistema_orig  EQ tg_0110-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_0110-bukrs                      TO tg_0079-bukrs,
           tg_0110-lifnr                      TO tg_0079-lifnr,
           tg_0110-dt_vcto                    TO tg_0079-zfbdt,
           tg_0110-waers                      TO tg_0079-waers,
           tg_0110-gsber                      TO tg_0079-gsber,
           abs( tg_0110-xvlr )                TO tg_0079-dmbtr,
           abs( tg_0110-xvlu )                TO tg_0079-dmbe2,
           sy-datum                           TO tg_0079-dt_atual,
           sy-uzeit                           TO tg_0079-hr_atual,
           "Parametro
           sy-uname                           TO tg_0079-us_proc,
           tg_0110-planilha                   TO tg_0079-planilha,
           tg_0110-planilha_itm               TO tg_0079-planilha_itm,
           tg_0109-codigo                     TO tg_0079-codigo,
           tg_0109-clas_flx                   TO tg_0079-clas_flx,
           tg_0109-seq                        TO tg_0079-seq,
           tg_0110-tipo                       TO tg_0079-tp_pg_sigam,
           tg_0110-id_invoice                 TO tg_0079-id_invoice,
           tg_0110-ds_porto                   TO tg_0079-ds_porto,
           tg_0109-st_calc_sdo                TO tg_0079-st_calc_sdo,
           tg_0110-processo_esp               TO tg_0079-processo_esp,
           tg_0110-sistema_orig               TO tg_0079-sistema_orig,
           tg_0109-tp_prev                    TO tg_0079-tp_prev.

    APPEND: tg_0079.
    CLEAR: tg_0079.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento de Dados NDF
*----------------------------------------------------------------------*

  LOOP AT tg_0083.

    CLEAR: vl_found_par, tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0083-processo_esp
       AND sistema_orig  EQ tg_0083-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE tg_0094 WITH KEY bukrs       = tg_0083-bukrs
                                bco_bloom_1 = tg_0083-cont_part_deal_c.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0083-xv5 = 0 ).
      CONTINUE.
    ENDIF.

    IF tg_0083-xv5 > 0.
      CLEAR: tg_0094-kunnr.
    ELSE.
      CLEAR: tg_0094-lifnr.
    ENDIF.

    MOVE:  tg_0083-bukrs                      TO tg_0079-bukrs,
           tg_0094-lifnr                      TO tg_0079-lifnr,
           tg_0094-kunnr                      TO tg_0079-kunnr,
           tg_0083-date_period_1              TO tg_0079-zfbdt,
           tg_0094-hkont                      TO tg_0079-hkont,
           tg_0083-currency_1                 TO tg_0079-waers,
           abs( tg_0083-xv5 )                 TO tg_0079-dmbtr,
           abs( tg_0083-xv6 )                 TO tg_0079-dmbe2,
           sy-datum                           TO tg_0079-dt_atual,
           sy-uzeit                           TO tg_0079-hr_atual,
           "Parametro
           sy-uname                           TO tg_0079-us_proc,
           tg_0109-codigo                     TO tg_0079-codigo,
           tg_0109-clas_flx                   TO tg_0079-clas_flx,
           tg_0109-seq                        TO tg_0079-seq,
           tg_0109-st_calc_sdo                TO tg_0079-st_calc_sdo,
           tg_0083-processo_esp               TO tg_0079-processo_esp,
           tg_0083-sistema_orig               TO tg_0079-sistema_orig,
           tg_0109-tp_prev                    TO tg_0079-tp_prev,
           tg_0083-trade_id                   TO tg_0079-trade_id.

    APPEND: tg_0079.
    CLEAR: tg_0079.

  ENDLOOP.

  LOOP AT tg_0096.

    CLEAR: vl_found_par, tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0096-processo_esp
       AND sistema_orig  EQ tg_0096-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE tg_0094 WITH KEY bukrs       = tg_0096-bukrs
                                bco_bloom_1 = tg_0096-cont_part_deal_c.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0096-xv5 = 0 ).
      CONTINUE.
    ENDIF.

    IF tg_0096-xv5 > 0.
      CLEAR: tg_0094-kunnr.
    ELSE.
      CLEAR: tg_0094-lifnr.
    ENDIF.

    MOVE:  tg_0096-bukrs                      TO tg_0079-bukrs,
           tg_0094-lifnr                      TO tg_0079-lifnr,
           tg_0094-kunnr                      TO tg_0079-kunnr,
           tg_0096-date_period_1              TO tg_0079-zfbdt,
           tg_0094-hkont                      TO tg_0079-hkont,
           tg_0096-currency_1                 TO tg_0079-waers,
           abs( tg_0096-xv5 )                 TO tg_0079-dmbtr,
           abs( tg_0096-xv6 )                 TO tg_0079-dmbe2,
           sy-datum                           TO tg_0079-dt_atual,
           sy-uzeit                           TO tg_0079-hr_atual,
           "Parametro
           sy-uname                           TO tg_0079-us_proc,
           tg_0109-codigo                     TO tg_0079-codigo,
           tg_0109-clas_flx                   TO tg_0079-clas_flx,
           tg_0109-seq                        TO tg_0079-seq,
           tg_0109-st_calc_sdo                TO tg_0079-st_calc_sdo,
           tg_0096-processo_esp               TO tg_0079-processo_esp,
           tg_0096-sistema_orig               TO tg_0079-sistema_orig,
           tg_0109-tp_prev                    TO tg_0079-tp_prev,
           tg_0096-trade_id                   TO tg_0079-trade_id.

    APPEND: tg_0079.
    CLEAR: tg_0079.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento Lctos Manuais
*----------------------------------------------------------------------*

  LOOP AT tg_0115.

    CLEAR: tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE codigo = tg_0115-codigo_flx.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF tg_0115-clas_flx IS NOT INITIAL.
      tg_0079-clas_flx  = tg_0115-clas_flx.
    ELSE.
      tg_0079-clas_flx  = tg_0109-clas_flx.
    ENDIF.

    "Grava ZFIT0079
    MOVE: tg_0115-cd_prev          TO tg_0079-cd_prev,
          tg_0115-bukrs            TO tg_0079-bukrs,
          tg_0115-dep_resp         TO tg_0079-dep_resp,
          tg_0115-codigo_flx       TO tg_0079-codigo,
          tg_0109-tp_prev          TO tg_0079-tp_prev,
          tg_0115-dt_vcto          TO tg_0079-zfbdt,
          tg_0109-st_calc_sdo      TO tg_0079-st_calc_sdo,
          tg_0115-waers            TO tg_0079-waers,
          tg_0115-dmbtr            TO tg_0079-dmbtr,
          tg_0115-dmbe2            TO tg_0079-dmbe2,
          tg_0115-observ           TO tg_0079-sgtxt,
          tg_0115-observ2          TO tg_0079-sgtxt2,
          sy-uname                 TO tg_0079-us_proc,
          tg_0115-usnam            TO tg_0079-usnam,
          sy-datum                 TO tg_0079-dt_atual,
          sy-uzeit                 TO tg_0079-hr_atual,
          tg_0115-cx_internacional TO tg_0079-cx_internacional.

    APPEND: tg_0079.
    CLEAR: tg_0079.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados XRT
*----------------------------------------------------------------------*

  LOOP AT tg_0112.

    CLEAR: tg_0109, tg_0119, vl_found_par.

    vl_proc_esp_xrt = tg_0112-processo_esp.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ vl_proc_esp_xrt
       AND sistema_orig  EQ tg_0112-sistema_orig.

    IF sy-subrc EQ 0.
      vl_found_par = 'X'.
    ENDIF.

    IF vl_found_par IS INITIAL.

      vl_proc_esp_xrt = tg_0112-processo_esp02.

      SELECT SINGLE *
        INTO tg_0109
        FROM zfit0109
       WHERE processo_esp  EQ vl_proc_esp_xrt
         AND sistema_orig  EQ tg_0112-sistema_orig.

      IF sy-subrc EQ 0.
        vl_found_par = 'X'.
      ENDIF.

    ENDIF.

    IF vl_found_par IS INITIAL.

      vl_proc_esp_xrt = tg_0112-processo_esp03.

      SELECT SINGLE *
        INTO tg_0109
        FROM zfit0109
       WHERE processo_esp  EQ vl_proc_esp_xrt
         AND sistema_orig  EQ tg_0112-sistema_orig.

      IF sy-subrc EQ 0.
        vl_found_par = 'X'.
      ENDIF.

    ENDIF.

    IF vl_found_par IS INITIAL.

      vl_proc_esp_xrt = tg_0112-processo_esp04.

      SELECT SINGLE *
        INTO tg_0109
        FROM zfit0109
       WHERE processo_esp  EQ vl_proc_esp_xrt
         AND sistema_orig  EQ tg_0112-sistema_orig.

      IF sy-subrc EQ 0.
        vl_found_par = 'X'.
      ENDIF.

    ENDIF.

    IF vl_found_par IS INITIAL.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_0112-bukrs                      TO tg_0119-bukrs,
           tg_0112-data_vencimento            TO tg_0119-zfbdt,
           tg_0112-opr_numero                 TO tg_0119-opr_numero,
           tg_0112-con_codigo                 TO tg_0119-con_codigo,
           tg_0112-mdo_codigo                 TO tg_0119-mdo_codigo,
           tg_0112-par_tipo                   TO tg_0119-par_tipo,
           tg_0112-mdo_tipo                   TO tg_0119-mdo_tipo,
           tg_0112-waers                      TO tg_0119-waers,

           tg_0112-bukrs_opr                  TO tg_0119-bukrs_opr,
           tg_0112-agente                     TO tg_0119-agente,
           tg_0112-regra_val                  TO tg_0119-regra_val,

           abs( tg_0112-xvlr )                TO tg_0119-dmbtr,
           abs( tg_0112-xvlu )                TO tg_0119-dmbe2,
           sy-datum                           TO tg_0119-dt_atual,
           sy-uzeit                           TO tg_0119-hr_atual,
           "Parametro
           sy-uname                           TO tg_0119-us_proc,
           tg_0109-codigo                     TO tg_0119-codigo,
           tg_0109-clas_flx                   TO tg_0119-clas_flx,
           tg_0109-seq                        TO tg_0119-seq,
           tg_0109-st_calc_sdo                TO tg_0119-st_calc_sdo,
           tg_0109-tp_prev                    TO tg_0119-tp_prev,
           vl_proc_esp_xrt                    TO tg_0119-processo_esp,
           tg_0112-sistema_orig               TO tg_0119-sistema_orig.

    APPEND: tg_0119.

  ENDLOOP.

  "Resgate Aplicações - Sobra Caixa.
  LOOP AT tg_0112_sld_aplic.

    CLEAR: tg_0109, tg_0119.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0112_sld_aplic-processo_esp
       AND sistema_orig  EQ tg_0112_sld_aplic-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_0112_sld_aplic-bukrs            TO tg_0119-bukrs,
           sy-datum                           TO tg_0119-zfbdt,
           tg_0112_sld_aplic-opr_numero       TO tg_0119-opr_numero,
           tg_0112_sld_aplic-con_codigo       TO tg_0119-con_codigo,
           tg_0112_sld_aplic-mdo_codigo       TO tg_0119-mdo_codigo,
           tg_0112_sld_aplic-par_tipo         TO tg_0119-par_tipo,
           tg_0112_sld_aplic-mdo_tipo         TO tg_0119-mdo_tipo,
           tg_0112_sld_aplic-waers            TO tg_0119-waers,

           tg_0112_sld_aplic-bukrs_opr        TO tg_0119-bukrs_opr,
           tg_0112_sld_aplic-agente           TO tg_0119-agente,
           tg_0112_sld_aplic-regra_val        TO tg_0119-regra_val,

           abs( tg_0112_sld_aplic-xvlr )      TO tg_0119-dmbtr,
           abs( tg_0112_sld_aplic-xvlu )      TO tg_0119-dmbe2,
           sy-datum                           TO tg_0119-dt_atual,
           sy-uzeit                           TO tg_0119-hr_atual,
           "Parametro
           sy-uname                           TO tg_0119-us_proc,
           tg_0109-codigo                     TO tg_0119-codigo,
           tg_0109-clas_flx                   TO tg_0119-clas_flx,
           tg_0109-seq                        TO tg_0119-seq,
           tg_0109-st_calc_sdo                TO tg_0119-st_calc_sdo,
           tg_0109-tp_prev                    TO tg_0119-tp_prev,
           tg_0112_sld_aplic-processo_esp     TO tg_0119-processo_esp,
           tg_0112_sld_aplic-sistema_orig     TO tg_0119-sistema_orig.

    APPEND: tg_0119.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados Adiantamento
*----------------------------------------------------------------------*
  LOOP AT tg_0046.

    CLEAR: tg_0109, tg_0079.

    READ TABLE tg_0045 WITH KEY nro_sol = tg_0046-nro_sol.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0046-processo_esp
       AND sistema_orig  EQ tg_0046-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_0045-bukrs                      TO tg_0079-bukrs,
           tg_0045-nro_sol                    TO tg_0079-nro_sol,
           tg_0045-ebeln                      TO tg_0079-ebeln,
           tg_0046-ebelp                      TO tg_0079-ebelp,
           tg_0045-lifnr                      TO tg_0079-lifnr,
           tg_0045-zlsch                      TO tg_0079-zlsch,
           tg_0045-hbkid                      TO tg_0079-hbkid,
           tg_0045-dep_resp                   TO tg_0079-dep_resp,
           tg_0045-sgtxt                      TO tg_0079-sgtxt,
           tg_0045-usnam                      TO tg_0079-usnam,
           tg_0045-dt_pgto                    TO tg_0079-zfbdt,
           tg_0045-moeda_pgto                 TO tg_0079-waers,
           tg_0046-xvlr                       TO tg_0079-dmbtr,
           tg_0046-xvlu                       TO tg_0079-dmbe2,
           sy-datum                           TO tg_0079-dt_atual,
           sy-uzeit                           TO tg_0079-hr_atual,
           "Parametro
           sy-uname                           TO tg_0079-us_proc,
           tg_0109-codigo                     TO tg_0079-codigo,
           tg_0109-clas_flx                   TO tg_0079-clas_flx,
           tg_0109-seq                        TO tg_0079-seq,
           tg_0109-st_calc_sdo                TO tg_0079-st_calc_sdo,
           tg_0109-tp_prev                    TO tg_0079-tp_prev,
           tg_0046-processo_esp               TO tg_0079-processo_esp,
           tg_0046-sistema_orig               TO tg_0079-sistema_orig.


    vl_obj_key_prev  = tg_0045-nro_sol && tg_0046-ebeln && tg_0046-ebelp.
    vl_seq_item_prev = '0001'.

    PERFORM f_monta_obj_key_prev USING '001' vl_obj_key_prev vl_seq_item_prev
                              CHANGING tg_0079.

    APPEND: tg_0079.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados VBSEGK
*----------------------------------------------------------------------*
  LOOP AT tg_vbsegk.

    CLEAR: tg_0109, tg_0079, tg_bkpf.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_vbsegk-processo_esp
       AND sistema_orig  EQ tg_vbsegk-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    READ TABLE tg_bkpf WITH KEY bukrs = tg_vbsegk-bukrs
                                gjahr = tg_vbsegk-gjahr
                                belnr = tg_vbsegk-belnr.

    READ TABLE tg_bukrs ASSIGNING <fs_bukrs>
    WITH KEY bukrs = tg_bsik-bukrs
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE t_lfa1 ASSIGNING <fs_kna1>
      WITH KEY lifnr = tg_vbsegk-lifnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF <fs_lfa1>-land1 <> <fs_bukrs>-land1.
          tg_0079-cx_internacional = 'X'.
        ENDIF.

      ENDIF.

    ENDIF.

    MOVE:  tg_vbsegk-bukrs             TO tg_0079-bukrs,
           tg_vbsegk-lifnr             TO tg_0079-lifnr,
           tg_vbsegk-belnr             TO tg_0079-belnr,
           tg_vbsegk-gjahr             TO tg_0079-gjahr,
           tg_vbsegk-buzei             TO tg_0079-buzei,
           tg_vbsegk-venci             TO tg_0079-zfbdt,
           tg_vbsegk-hkont             TO tg_0079-hkont,
           tg_vbsegk-swaer             TO tg_0079-waers,
           tg_vbsegk-gsber             TO tg_0079-gsber,
           tg_vbsegk-bschl             TO tg_0079-bschl,
           tg_vbsegk-shkzg             TO tg_0079-shkzg,
           tg_vbsegk-xvlr              TO tg_0079-dmbtr,
           tg_vbsegk-xvlu              TO tg_0079-dmbe2,
           tg_bkpf-usnam               TO tg_0079-usnam,
           sy-datum                    TO tg_0079-dt_atual,
           sy-uzeit                    TO tg_0079-hr_atual,
           "Parametro
           sy-uname                    TO tg_0079-us_proc,
           tg_vbsegk-zlspr             TO tg_0079-zlspr,
           tg_vbsegk-zlsch             TO tg_0079-zlsch,
           tg_vbsegk-hbkid             TO tg_0079-hbkid,
           tg_0109-codigo              TO tg_0079-codigo,
           tg_0109-clas_flx            TO tg_0079-clas_flx,
           tg_0109-seq                 TO tg_0079-seq,
           tg_0109-st_calc_sdo         TO tg_0079-st_calc_sdo,
           tg_0109-processo_esp        TO tg_0079-processo_esp,
           tg_0109-sistema_orig        TO tg_0079-sistema_orig,
           tg_0109-tp_prev             TO tg_0079-tp_prev.

    APPEND: tg_0079.
    CLEAR: tg_0079, tg_vbsegk.
  ENDLOOP.



*----------------------------------------------------------------------*
* Processamento dados Adiantamento Frete
*----------------------------------------------------------------------*
  LOOP AT tg_zlest0140.

    CLEAR: tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_zlest0140-processo_esp
       AND sistema_orig  EQ tg_zlest0140-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_zlest0140-bukrs                      TO tg_0079-bukrs,
           tg_zlest0140-lifnr                      TO tg_0079-lifnr,
           tg_zlest0140-observ(140)                TO tg_0079-sgtxt,
           tg_zlest0140-observ+140(70)             TO tg_0079-sgtxt2,
           tg_zlest0140-usuario                    TO tg_0079-usnam,
           tg_zlest0140-data                       TO tg_0079-zfbdt,
           'BRL'                                   TO tg_0079-waers,
           tg_zlest0140-xvlr                       TO tg_0079-dmbtr,
           tg_zlest0140-xvlu                       TO tg_0079-dmbe2,
           sy-datum                                TO tg_0079-dt_atual,
           sy-uzeit                                TO tg_0079-hr_atual,
           tg_zlest0140-processo_esp               TO tg_0079-processo_esp,
           tg_zlest0140-sistema_orig               TO tg_0079-sistema_orig,
           "Parametro
           sy-uname                                TO tg_0079-us_proc,
           tg_0109-codigo                          TO tg_0079-codigo,
           tg_0109-clas_flx                        TO tg_0079-clas_flx,
           tg_0109-seq                             TO tg_0079-seq,
           tg_0109-st_calc_sdo                     TO tg_0079-st_calc_sdo,
           tg_0109-tp_prev                         TO tg_0079-tp_prev.

    vl_obj_key_prev  = tg_zlest0140-bukrs && tg_zlest0140-lifnr && tg_zlest0140-data.
    vl_seq_item_prev = '0001'.

    PERFORM f_monta_obj_key_prev USING '002' vl_obj_key_prev vl_seq_item_prev
                              CHANGING tg_0079.

    APPEND: tg_0079.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados Saldo Frete
*----------------------------------------------------------------------*

  LOOP AT tg_0141_sld_fre_grp.

    CLEAR: tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0141_sld_fre_grp-processo_esp
       AND sistema_orig  EQ tg_0141_sld_fre_grp-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_0141_sld_fre_grp-bukrs                      TO tg_0079-bukrs,
           tg_0141_sld_fre_grp-lifnr                      TO tg_0079-lifnr,
           tg_0141_sld_fre_grp-dt_saldo_frete             TO tg_0079-zfbdt,
           'BRL'                                          TO tg_0079-waers,
           tg_0141_sld_fre_grp-xvlr                       TO tg_0079-dmbtr,
           tg_0141_sld_fre_grp-xvlu                       TO tg_0079-dmbe2,
           sy-datum                                       TO tg_0079-dt_atual,
           sy-uzeit                                       TO tg_0079-hr_atual,
           tg_0141_sld_fre_grp-processo_esp               TO tg_0079-processo_esp,
           tg_0141_sld_fre_grp-sistema_orig               TO tg_0079-sistema_orig,
           "Parametro
           sy-uname                                       TO tg_0079-us_proc,
           tg_0109-codigo                                 TO tg_0079-codigo,
           tg_0109-clas_flx                               TO tg_0079-clas_flx,
           tg_0109-seq                                    TO tg_0079-seq,
           tg_0109-st_calc_sdo                            TO tg_0079-st_calc_sdo,
           tg_0109-tp_prev                                TO tg_0079-tp_prev.

    vl_obj_key_prev  = tg_0141_sld_fre_grp-bukrs && tg_0141_sld_fre_grp-lifnr && tg_0141_sld_fre_grp-dt_saldo_frete.
    vl_seq_item_prev = '0001'.

    PERFORM f_monta_obj_key_prev USING '003' vl_obj_key_prev vl_seq_item_prev
                              CHANGING tg_0079.

    APPEND: tg_0079.

  ENDLOOP.

*----------------------------------------------------------------------*
* Processamento dados Frete - OUTROS
*----------------------------------------------------------------------*

  LOOP AT tg_0141_outros_grp.

    CLEAR: tg_0109, tg_0079.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_0141_outros_grp-processo_esp
       AND sistema_orig  EQ tg_0141_outros_grp-sistema_orig.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_0141_outros_grp-bukrs                      TO tg_0079-bukrs,
           tg_0141_outros_grp-cd_adiministra             TO tg_0079-lifnr,
           tg_0141_outros_grp-dt_util                     TO tg_0079-zfbdt,
           'BRL'                                          TO tg_0079-waers,
           tg_0141_outros_grp-xvlr                       TO tg_0079-dmbtr,
           tg_0141_outros_grp-xvlu                       TO tg_0079-dmbe2,
           sy-datum                                       TO tg_0079-dt_atual,
           sy-uzeit                                       TO tg_0079-hr_atual,
           tg_0141_outros_grp-processo_esp               TO tg_0079-processo_esp,
           tg_0141_outros_grp-sistema_orig               TO tg_0079-sistema_orig,
           "Parametro
           sy-uname                                       TO tg_0079-us_proc,
           tg_0109-codigo                                 TO tg_0079-codigo,
           tg_0109-clas_flx                               TO tg_0079-clas_flx,
           tg_0109-seq                                    TO tg_0079-seq,
           tg_0109-st_calc_sdo                            TO tg_0079-st_calc_sdo,
           tg_0109-tp_prev                                TO tg_0079-tp_prev.

    vl_obj_key_prev  = tg_0141_outros_grp-bukrs && tg_0141_outros_grp-cd_adiministra && tg_0141_outros_grp-dt_util.
    vl_seq_item_prev = '0001'.

    PERFORM f_monta_obj_key_prev USING '004' vl_obj_key_prev vl_seq_item_prev
                              CHANGING tg_0079.

    APPEND: tg_0079.

  ENDLOOP.

*-------------------------------------------------------------------------------*
* Processamento de Previsões de Recebimento a partir de Lançamentos de Pagamento
*-------------------------------------------------------------------------------*
  LOOP AT tg_bsik_for_bsid.

    CLEAR: tg_0109, tg_0079.

    CLEAR: vl_tipo_ped.
    CONCATENATE '%\' tg_bsik_for_bsid-bsart '\%' INTO vl_tipo_ped.

    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_bsik_for_bsid-processo_esp
       AND sistema_orig  EQ tg_bsik_for_bsid-sistema_orig
       AND tipo_ped      LIKE vl_tipo_ped . " US 79059 - CBRAND

    IF sy-subrc NE 0.
*** US 79059 - Inicio - CBRAND
      SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE processo_esp  EQ tg_bsik_for_bsid-processo_esp
       AND sistema_orig  EQ tg_bsik_for_bsid-sistema_orig.

      IF sy-subrc NE 0.
*** US 79059 - Fim - CBRAND
        CONTINUE.
      ENDIF.
    ENDIF.

    IF ( tg_0109-ocultar IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE:  tg_bsik_for_bsid-bukrs             TO tg_0079-bukrs,
           tg_bsik_for_bsid-lifnr             TO tg_0079-lifnr,
           tg_bsik_for_bsid-gjahr             TO tg_0079-gjahr,
           tg_bsik_for_bsid-buzei             TO tg_0079-buzei,
           tg_bsik_for_bsid-budat             TO tg_0079-budat,
           tg_bsik_for_bsid-venci             TO tg_0079-zfbdt,
           tg_bsik_for_bsid-hkont             TO tg_0079-hkont,
           tg_bsik_for_bsid-blart             TO tg_0079-blart,
           tg_bsik_for_bsid-bldat             TO tg_0079-bldat,
           tg_bsik_for_bsid-waers             TO tg_0079-waers,
           tg_bsik_for_bsid-xblnr             TO tg_0079-xblnr,
           tg_bsik_for_bsid-blart             TO tg_0079-blart,
           tg_bsik_for_bsid-gsber             TO tg_0079-gsber,
           tg_bsik_for_bsid-ebeln             TO tg_0079-ebeln,
           tg_bsik_for_bsid-ebelp             TO tg_0079-ebelp,
           tg_bsik_for_bsid-bschl             TO tg_0079-bschl,
           tg_bsik_for_bsid-shkzg             TO tg_0079-shkzg,
           tg_bsik_for_bsid-xvlr              TO tg_0079-dmbtr,
           tg_bsik_for_bsid-xvlu              TO tg_0079-dmbe2,
           tg_bkpf-usnam                      TO tg_0079-usnam,
           sy-datum                           TO tg_0079-dt_atual,
           sy-uzeit                           TO tg_0079-hr_atual,
           "Parametro
           sy-uname                           TO tg_0079-us_proc,
           tg_bsik_for_bsid-zlspr             TO tg_0079-zlspr,
           tg_bsik_for_bsid-zlsch             TO tg_0079-zlsch,
           tg_bsik_for_bsid-hbkid             TO tg_0079-hbkid,
           tg_0109-codigo                     TO tg_0079-codigo,
           tg_0109-clas_flx                   TO tg_0079-clas_flx,
           tg_0109-seq                        TO tg_0079-seq,
           tg_0109-st_calc_sdo                TO tg_0079-st_calc_sdo,
           tg_ekko-bsart                      TO tg_0079-bsart,
           tg_bsik_for_bsid-processo_esp      TO tg_0079-processo_esp,
           tg_bsik_for_bsid-sistema_orig      TO tg_0079-sistema_orig,
           tg_0109-tp_prev                    TO tg_0079-tp_prev.

    vl_obj_key_prev  = tg_bsik_for_bsid-bukrs && '-' && tg_bsik_for_bsid-belnr && '-' && tg_bsik_for_bsid-buzei.
    vl_seq_item_prev = '0001'.

    PERFORM f_monta_obj_key_prev USING '005' vl_obj_key_prev vl_seq_item_prev
                              CHANGING tg_0079.

    APPEND: tg_0079.

  ENDLOOP.




ENDFORM.                    " PROCESSA_DADOS


*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .

  DATA BEGIN OF tg_0079_clone OCCURS 0.
  INCLUDE STRUCTURE zfit0079.
  DATA END OF tg_0079_clone.

  DATA: vl_max_versao  TYPE zfit0079-versao.

  DATA: vl_dt_base_versao TYPE zfit0079-dt_base_versao,
        vl_hora_versao    TYPE zfit0079-hora_versao,
        vl_versao         TYPE zfit0079-versao.

*=========================================================================
** Get Versão Processamento.
*=========================================================================
  vl_dt_base_versao = sy-datum.
  vl_hora_versao    = sy-uzeit.

  SELECT MAX( versao )
    INTO vl_max_versao
    FROM zfit0111
   WHERE dt_base_versao = vl_dt_base_versao.

  IF ( sy-subrc NE 0 ) OR
     ( vl_max_versao IS INITIAL ) OR
     ( vl_max_versao = 0 ).
    vl_versao = 1.
  ELSE.
    vl_versao = ( vl_max_versao + 1 ).
  ENDIF.

  IF tg_0079[] IS NOT INITIAL.

    "Atribuição de Versão de Processamento
    LOOP AT tg_0079.

      tg_0079-dt_base_versao = vl_dt_base_versao.
      tg_0079-hora_versao    = vl_hora_versao.
      tg_0079-versao         = vl_versao.

      MODIFY tg_0079.

    ENDLOOP.

    MODIFY zfit0079 FROM TABLE tg_0079.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar os registros!' TYPE 'S'.
      RETURN.
    ENDIF.

  ENDIF.

  "XRT
  IF tg_0119[] IS NOT INITIAL.

    "Atribuição de Versão de Processamento
    LOOP AT tg_0119.

      tg_0119-dt_base_versao = vl_dt_base_versao.
      tg_0119-hora_versao    = vl_hora_versao.
      tg_0119-versao         = vl_versao.

      MODIFY tg_0119.

    ENDLOOP.

    MODIFY zfit0119 FROM TABLE tg_0119.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar os registros!' TYPE 'S'.
      RETURN.
    ENDIF.

  ENDIF.

  LOOP AT tg_bukrs.

    CALL FUNCTION 'ZFI_PROC_RESUMO_FLX'
      EXPORTING
        i_data_ini       = it_dt_venc-low
        i_bukrs          = tg_bukrs-bukrs
        i_dt_base_versao = vl_dt_base_versao
        i_hora_versao    = vl_hora_versao
        i_versao         = vl_versao
      EXCEPTIONS
        m_error          = 1
        OTHERS           = 2.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar os registros!' TYPE 'S'.
      RETURN.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.
  MESSAGE 'Processamento concluído com sucesso!' TYPE 'S'.

  IF vg_error_prc IS NOT INITIAL.
    MESSAGE vg_error_prc TYPE 'S'.
  ENDIF.

ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_TIPO_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_values USING p_tipo .

  DATA: vl_value        TYPE string,
        vl_value_append TYPE string,
        vl_count        TYPE i,
        vl_idx          TYPE sytabix,
        vl_tam_str      TYPE i.

  LOOP AT tg_0109.

    CASE p_tipo.
      WHEN 'TP_DOC'.
        vl_value  = tg_0109-tipo_doc.
      WHEN 'TP_PED'.
        vl_value  = tg_0109-tipo_ped.
      WHEN 'TP_OV'.
        vl_value  = tg_0109-tipo_ov.
    ENDCASE.

    vl_tam_str = strlen( vl_value ).

    IF vl_tam_str = 0.
      CONTINUE.
    ENDIF.

    vl_count = 0.
    vl_idx   = 1.
    CLEAR: vl_value_append.

    DO vl_tam_str TIMES.

      IF ( vl_value+vl_count(1) NE ',' ).
        CONCATENATE vl_value_append vl_value+vl_count(1) INTO vl_value_append.
      ENDIF.

      IF ( vl_value+vl_count(1) EQ ',' ) OR ( vl_tam_str = vl_idx ).

        IF ( vl_value_append IS NOT INITIAL ) .

          CASE p_tipo.
            WHEN 'TP_DOC'.

*              CLEAR: TG_0109_DET.
*              MOVE-CORRESPONDING TG_0109 TO TG_0109_DET.
*              TG_0109_DET-BLART = VL_VALUE_APPEND.
*              APPEND TG_0109_DET.

              READ TABLE it_tp_doc WITH KEY low = vl_value_append.
              IF sy-subrc NE 0.

                it_tp_doc-sign   = 'I'.
                it_tp_doc-option = 'EQ'.
                it_tp_doc-low    = vl_value_append.
                it_tp_doc-high   = vl_value_append.
                APPEND it_tp_doc.

              ENDIF.

            WHEN 'TP_PED'.

*              CLEAR: TG_0109_DET.
*              MOVE-CORRESPONDING TG_0109 TO TG_0109_DET.
*              TG_0109_DET-BSART = VL_VALUE_APPEND.
*              APPEND TG_0109_DET.

              READ TABLE it_tp_ped WITH KEY low = vl_value_append.
              IF sy-subrc NE 0.

                it_tp_ped-sign   = 'I'.
                it_tp_ped-option = 'EQ'.
                it_tp_ped-low    = vl_value_append.
                it_tp_ped-high   = vl_value_append.
                APPEND it_tp_ped.

              ENDIF.

            WHEN 'TP_OV'.

*              CLEAR: TG_0109_DET.
*              MOVE-CORRESPONDING TG_0109 TO TG_0109_DET.
*              TG_0109_DET-AUART = VL_VALUE_APPEND.
*              APPEND TG_0109_DET.

              READ TABLE it_tp_ov WITH KEY low = vl_value_append.
              IF sy-subrc NE 0.

                it_tp_ov-sign   = 'I'.
                it_tp_ov-option = 'EQ'.
                it_tp_ov-low    = vl_value_append.
                it_tp_ov-high   = vl_value_append.
                APPEND it_tp_ov.

              ENDIF.

          ENDCASE.

        ENDIF.

        CLEAR: vl_value_append.

      ENDIF.

      ADD: 1 TO vl_count,
           1 TO vl_idx.

    ENDDO.

  ENDLOOP.

ENDFORM.                    " CARREGA_VALUES

*&---------------------------------------------------------------------*
*&      Form  INICIA_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inicia_variaveis .

  DATA: vg_dt_high TYPE zfit0079-zfbdt.

  CLEAR: vg_err_consulta, vg_error_prc.

  REFRESH: tg_0079, tg_bukrs.

  vg_dt_high = sy-datum.
  ADD 30 TO vg_dt_high.

  it_dt_venc-sign   = 'I'.
  it_dt_venc-option = 'BT'.
  it_dt_venc-low    = sy-datum.
  it_dt_venc-high   = vg_dt_high.
  APPEND it_dt_venc.

*  IT_DT_VENC-SIGN   = 'I'.
*  IT_DT_VENC-OPTION = 'BT'.
*  IT_DT_VENC-LOW    = S_ZFBDT-LOW.
*  IT_DT_VENC-HIGH   = S_ZFBDT-HIGH.
*  APPEND IT_DT_VENC.



*  IF S_ZFBDT-HIGH IS NOT INITIAL.
*
*    IT_DT_VENC-SIGN   = 'I'.
*    IT_DT_VENC-OPTION = 'BT'.
*    IT_DT_VENC-LOW    = S_ZFBDT-LOW.
*    IT_DT_VENC-HIGH   = S_ZFBDT-HIGH.
*    APPEND IT_DT_VENC.
*
*  ELSE.

*    VG_DT_HIGH = S_ZFBDT-LOW.
*    ADD 30 TO VG_DT_HIGH.
*
*    IT_DT_VENC-SIGN   = 'I'.
*    IT_DT_VENC-OPTION = 'BT'.
*    IT_DT_VENC-LOW    = S_ZFBDT-LOW.
*    IT_DT_VENC-HIGH   = VG_DT_HIGH.
*    APPEND IT_DT_VENC.

*  ENDIF.

ENDFORM.                    " INICIA_VARIAVEIS
*&---------------------------------------------------------------------*
*&      Form  GET_PAR_FLX
*&---------------------------------------------------------------------*
FORM get_par_flx  USING    p_tipo_doc    TYPE string
                           p_tipo_ped    TYPE string
                           p_tipo_ov     TYPE string
                           p_bloq_pgto   TYPE string
                           p_forma_pgto  TYPE dzlsch
                           p_zuonr       TYPE dzuonr
                           p_vbund       TYPE vbund   "*-CS2022000133-#74201-05.05.2022-JT-inicio
                           p_soc_parc    TYPE c       "*-CS2022000133-#74201-05.05.2022-JT-inicio
                           p_forn_cli    TYPE c
                  CHANGING p_0109        TYPE zfit0109
                           p_found_par   TYPE c.

  "Priorizar soc.parceira com 'S' ou 'N' - Com bloqueio de Pagamento
  PERFORM get_parametros USING p_tipo_doc
                               p_tipo_ped
                               p_tipo_ov
                               p_bloq_pgto
                               p_forma_pgto
                               p_zuonr
                               p_vbund         "*-CS2022000133-#74201-05.05.2022-JT-inicio
                               p_soc_parc      "*-CS2022000133-#74201-05.05.2022-JT-inicio
                               p_forn_cli
                               abap_true  "Bloqueio de Pagamento
                      CHANGING p_0109
                               p_found_par.

* Buscar demais priorizacoes - Com bloqueio de Pagamento
  IF p_found_par = abap_false.
    PERFORM get_parametros USING p_tipo_doc
                                 p_tipo_ped
                                 p_tipo_ov
                                 p_bloq_pgto
                                 p_forma_pgto
                                 p_zuonr
                                 p_vbund        "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                 abap_false     "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                 p_forn_cli
                                 abap_true
                        CHANGING p_0109
                                 p_found_par.
  ENDIF.


  " Priorizar soc.parceira com 'S' ou 'N' - Sem bloqueio de Pagamento
  IF p_found_par = abap_false.
    PERFORM get_parametros USING p_tipo_doc
                                 p_tipo_ped
                                 p_tipo_ov
                                 p_bloq_pgto
                                 p_forma_pgto
                                 p_zuonr
                                 p_vbund         "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                 p_soc_parc      "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                 p_forn_cli
                                 abap_false
                        CHANGING p_0109
                                 p_found_par.
  ENDIF.

* Buscar demais priorizacoes - Sem bloqueio de Pagamento
  IF p_found_par = abap_false.
    PERFORM get_parametros USING p_tipo_doc
                                 p_tipo_ped
                                 p_tipo_ov
                                 p_bloq_pgto
                                 p_forma_pgto
                                 p_zuonr
                                 p_vbund        "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                 abap_false     "*-CS2022000133-#74201-05.05.2022-JT-inicio
                                 p_forn_cli
                                 abap_false
                        CHANGING p_0109
                                 p_found_par.
  ENDIF.

ENDFORM.

**************************************************************************************
* obtem parametros conforme priorizacao
**************************************************************************************
FORM get_parametros USING  p_tipo_doc    TYPE string
                           p_tipo_ped    TYPE string
                           p_tipo_ov     TYPE string
                           p_bloq_pgto   TYPE string
                           p_forma_pgto  TYPE dzlsch
                           p_zuonr       TYPE dzuonr
                           p_vbund       TYPE vbund   "*-CS2022000133-#74201-05.05.2022-JT-inicio
                           p_soc_parc    TYPE c       "*-CS2022000133-#74201-05.05.2022-JT-inicio
                           p_forn_cli    TYPE c
                           p_ck_bloq_pgto TYPE c
                  CHANGING p_0109        TYPE zfit0109
                           p_found_par   TYPE c.

  DATA: vl_clas_flx     TYPE zfit0109-clas_flx,
        vl_soc_parceira TYPE char1.

  CLEAR: p_found_par.

*-CS2022000133-#74201-05.05.2022-JT-inicio
  IF p_soc_parc = abap_true.
    IF p_vbund IS INITIAL.
      vl_soc_parceira  = 'N'.
    ELSE.
      vl_soc_parceira  = 'S'.
    ENDIF.
  ELSE.
    vl_soc_parceira    = abap_off.
  ENDIF.
*-CS2022000133-#74201-05.05.2022-JT-fim

  IF p_forn_cli EQ 'F'.  "Partidas de Fornecedores

    vl_clas_flx = 'S'.

    IF p_ck_bloq_pgto EQ abap_true.

      "Busca: Tipo Documento / Tipo Pedido / Forma Pgto / Bloqueio Pgto
      IF ( p_tipo_doc   IS NOT INITIAL ) AND
         ( p_tipo_ped   IS NOT INITIAL ) AND
         ( p_forma_pgto IS NOT INITIAL ) AND
         ( p_bloq_pgto  IS NOT INITIAL ).
        SELECT SINGLE *
          INTO p_0109
          FROM zfit0109
         WHERE tipo_doc   LIKE p_tipo_doc
           AND tipo_ped   LIKE p_tipo_ped
           AND forma_pgto   EQ p_forma_pgto
           AND bloq_pgto  LIKE p_bloq_pgto
           AND soc_parceira EQ vl_soc_parceira  "*-CS2022000133-#74201-05.05.2022-JT-fim
           AND clas_flx     EQ vl_clas_flx.
        IF sy-subrc = 0.
          p_found_par = 'X'.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Tipo Pedido / Bloqueio Pgto
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_tipo_ped   IS NOT INITIAL ) AND
           ( p_bloq_pgto  IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ped   LIKE p_tipo_ped
             AND forma_pgto   EQ ''
             AND bloq_pgto  LIKE p_bloq_pgto
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Bloqueio Pgto
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_bloq_pgto  IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ped     EQ ''
             AND forma_pgto   EQ ''
             AND bloq_pgto  LIKE p_bloq_pgto
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Bloqueio Pgto
      IF p_found_par IS INITIAL.
        IF ( p_bloq_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE bloq_pgto  LIKE p_bloq_pgto
             AND tipo_doc     EQ ''
             AND tipo_ped     EQ ''
             AND forma_pgto   EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

      "Busca: Tipo Documento / Tipo Pedido / Forma Pgto
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_tipo_ped   IS NOT INITIAL ) AND
           ( p_forma_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ped   LIKE p_tipo_ped
             AND forma_pgto   EQ p_forma_pgto
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Tipo Pedido
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_tipo_ped   IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ped   LIKE p_tipo_ped
             AND forma_pgto   EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Processo Especifico
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc IS NOT INITIAL ) AND
           ( p_zuonr  IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc     LIKE p_tipo_doc
             AND tipo_ped     EQ   ''
             AND forma_pgto   EQ   ''
             AND bloq_pgto    EQ   ''
             AND processo_esp EQ p_zuonr
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ped     EQ ''
             AND forma_pgto   EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Pedido
      IF p_found_par IS INITIAL.
        IF ( p_tipo_ped IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_ped   LIKE p_tipo_ped
             AND tipo_doc     EQ ''
             AND forma_pgto   EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Forma Pgto
      IF p_found_par IS INITIAL.
        IF ( p_forma_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE forma_pgto EQ p_forma_pgto
             AND tipo_doc     EQ ''
             AND tipo_ped     EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ELSEIF p_forn_cli EQ 'C'.  "Partidas de Clientes.

    vl_clas_flx = 'E'.

    IF p_ck_bloq_pgto EQ abap_true.

      "Busca: Tipo Documento / Tipo Ordem Venda / Forma Pgto / Bloqueio Pgto
      IF ( p_tipo_doc   IS NOT INITIAL ) AND
         ( p_tipo_ov    IS NOT INITIAL ) AND
         ( p_forma_pgto IS NOT INITIAL ) AND
         ( p_bloq_pgto  IS NOT INITIAL ).
        SELECT SINGLE *
          INTO p_0109
          FROM zfit0109
         WHERE tipo_doc   LIKE p_tipo_doc
           AND tipo_ov    LIKE p_tipo_ov
           AND forma_pgto   EQ p_forma_pgto
           AND bloq_pgto  LIKE p_bloq_pgto
           AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
           AND clas_flx     EQ vl_clas_flx.
        IF sy-subrc = 0.
          p_found_par = 'X'.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Tipo Ordem Venda / Bloqueio Pgto
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc  IS NOT INITIAL ) AND
           ( p_tipo_ov   IS NOT INITIAL ) AND
           ( p_bloq_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ov    LIKE p_tipo_ov
             AND forma_pgto   EQ ''
             AND bloq_pgto  LIKE p_bloq_pgto
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Bloqueio Pgto
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_bloq_pgto  IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ov      EQ ''
             AND forma_pgto   EQ ''
             AND bloq_pgto  LIKE p_bloq_pgto
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Bloqueio Pgto
      IF p_found_par IS INITIAL.
        IF ( p_bloq_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE bloq_pgto  LIKE p_bloq_pgto
             AND tipo_doc     EQ ''
             AND tipo_ov      EQ ''
             AND forma_pgto   EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

      "Busca: Tipo Documento / Tipo Ordem Venda / Forma Pgto
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_tipo_ov   IS NOT INITIAL ) AND
           ( p_forma_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ov    LIKE p_tipo_ov
             AND forma_pgto   EQ p_forma_pgto
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Tipo Ordem Venda
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc   IS NOT INITIAL ) AND
           ( p_tipo_ov   IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ov    LIKE p_tipo_ov
             AND forma_pgto   EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento / Processo Especifico
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc IS NOT INITIAL ) AND
           ( p_zuonr  IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc     LIKE p_tipo_doc
             AND tipo_ped     EQ   ''
             AND forma_pgto   EQ   ''
             AND bloq_pgto    EQ   ''
             AND processo_esp EQ p_zuonr
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Documento
      IF p_found_par IS INITIAL.
        IF ( p_tipo_doc IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_doc   LIKE p_tipo_doc
             AND tipo_ov      EQ ''
             AND forma_pgto   EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Tipo Ordem Venda
      IF p_found_par IS INITIAL.
        IF ( p_tipo_ov IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE tipo_ov    LIKE p_tipo_ov
             AND tipo_doc     EQ ''
             AND forma_pgto   EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      "Busca: Forma Pgto
      IF p_found_par IS INITIAL.
        IF ( p_forma_pgto IS NOT INITIAL ).
          SELECT SINGLE *
            INTO p_0109
            FROM zfit0109
           WHERE forma_pgto   EQ p_forma_pgto
             AND tipo_doc     EQ ''
             AND tipo_ov      EQ ''
             AND bloq_pgto    EQ ''
             AND soc_parceira EQ vl_soc_parceira "*-CS2022000133-#74201-05.05.2022-JT-fim
             AND clas_flx     EQ vl_clas_flx.
          IF sy-subrc = 0.
            p_found_par = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.


  ENDIF.

ENDFORM.

FORM load_vbsegk.

  "PSA
  "               AND TRIM(ZFBDT) <> ''
  "               AND TRIM(ZFBDT) <> '00000000'
  "               AND ( TO_DATE(ZFBDT,'YYYYMMDD' ) + NVL(ZBD1T,0) ) BETWEEN  TRUNC(:sy-datum) AND TRUNC(:sy-datum + 30)

  LOOP AT tg_bukrs.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS FOR
            SELECT BUKRS, LIFNR, UMSKS, UMSKZ, ZUONR, GJAHR, BELNR, BUZEI, SWAER, GSBER, BSCHL, SHKZG,
                   ZLSCH, ZLSPR, HKONT, SGTXT, HBKID, ZFBDT, ZBD1T, DMBTR, DMBE2, WRBTR
              FROM VBSEGK
             WHERE 1 = 1
and trim(ZFBDT) <> '00000000'
and length(trim(ZFBDT)) = 8
               AND ADD_DAYS(cast(trim(ZFBDT) as date),TO_NUMBER(ZBD1T)) BETWEEN  current_date AND ADD_DAYS(current_date,30)
               AND BUKRS = :TG_BUKRS-BUKRS
               AND SHKZG = 'H'
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT DOCUMENTOS INTO
        :TG_VBSEGK-BUKRS,
        :TG_VBSEGK-LIFNR,
        :TG_VBSEGK-UMSKS,
        :TG_VBSEGK-UMSKZ,
        :TG_VBSEGK-ZUONR,
        :TG_VBSEGK-GJAHR,
        :TG_VBSEGK-BELNR,
        :TG_VBSEGK-BUZEI,
        :TG_VBSEGK-SWAER,
        :TG_VBSEGK-GSBER,
        :TG_VBSEGK-BSCHL,
        :TG_VBSEGK-SHKZG,
        :TG_VBSEGK-ZLSCH,
        :TG_VBSEGK-ZLSPR,
        :TG_VBSEGK-HKONT,
        :TG_VBSEGK-SGTXT,
        :TG_VBSEGK-HBKID,
        :TG_VBSEGK-ZFBDT,
        :TG_VBSEGK-ZBD1T,
        :TG_VBSEGK-DMBTR,
        :TG_VBSEGK-DMBE2,
        :TG_VBSEGK-WRBTR.
      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        APPEND tg_vbsegk.
        CLEAR: tg_vbsegk.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DOCUMENTOS
    ENDEXEC.

  ENDLOOP.

  DATA(lt_vbsegk) = tg_vbsegk[].
  SORT lt_vbsegk BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_vbsegk COMPARING lifnr.

  IF lt_vbsegk IS NOT INITIAL.

    SELECT lifnr land1
      FROM lfa1
      INTO TABLE t_lfa1
      FOR ALL ENTRIES IN lt_vbsegk
      WHERE lifnr = lt_vbsegk-lifnr.
    IF sy-subrc IS INITIAL.
      SORT t_lfa1 BY lifnr.
    ENDIF.

  ENDIF.
*  SELECT BUKRS LIFNR UMSKS UMSKZ ZUONR GJAHR BELNR BUZEI SWAER GSBER BSCHL SHKZG
*         ZLSCH ZLSPR HKONT SGTXT HBKID ZFBDT ZBD1T DMBTR DMBE2
*    FROM VBSEGK INTO TABLE TG_VBSEGK
*   WHERE BUKRS IN S_BUKRS
*     AND SHKZG EQ 'H'.

ENDFORM.

FORM load_bsik.

  "PSA
  "                 AND TRIM(ZFBDT) <> ''
  "               AND TRIM(ZFBDT) <> '00000000'
  "               AND ( TO_DATE(ZFBDT,'YYYYMMDD' ) + NVL(ZBD1T,0) ) BETWEEN  TRUNC(:sy-datum) AND TRUNC(:sy-datum + 30)
  "xblnr

  LOOP AT tg_bukrs.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS FOR
            SELECT BUKRS,LIFNR,UMSKS,UMSKZ,AUGDT,AUGBL,ZUONR,GJAHR,BELNR,BUZEI,BUDAT,BLDAT,WAERS,XBLNR,BLART,GSBER,EBELN,EBELP,BSCHL,SHKZG,ZLSCH,ZLSPR,HKONT,SGTXT,HBKID,ZFBDT,ZBD1T,DMBTR,DMBE2,WRBTR,VBUND
              FROM BSIK
             WHERE 1 = 1
and ZFBDT <> '00000000'
and length(ZFBDT) = 8
and ADD_DAYS(cast(trim(ZFBDT) as date),TO_NUMBER(trim(ZBD1T))) between current_date and ADD_DAYS(current_date,30)
               AND BUKRS = :TG_BUKRS-BUKRS
               AND SHKZG = 'H'
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT DOCUMENTOS INTO
        :TG_BSIK-BUKRS,
        :TG_BSIK-LIFNR,
        :TG_BSIK-UMSKS,
        :TG_BSIK-UMSKZ,
        :TG_BSIK-AUGDT,
        :TG_BSIK-AUGBL,
        :TG_BSIK-ZUONR,
        :TG_BSIK-GJAHR,
        :TG_BSIK-BELNR,
        :TG_BSIK-BUZEI,
        :TG_BSIK-BUDAT,
        :TG_BSIK-BLDAT,
        :TG_BSIK-WAERS,
        :TG_BSIK-XBLNR,
        :TG_BSIK-BLART,
        :TG_BSIK-GSBER,
        :TG_BSIK-EBELN,
        :TG_BSIK-EBELP,
        :TG_BSIK-BSCHL,
        :TG_BSIK-SHKZG,
        :TG_BSIK-ZLSCH,
        :TG_BSIK-ZLSPR,
        :TG_BSIK-HKONT,
        :TG_BSIK-SGTXT,
        :TG_BSIK-HBKID,
        :TG_BSIK-ZFBDT,
        :TG_BSIK-ZBD1T,
        :TG_BSIK-DMBTR,
        :TG_BSIK-DMBE2,
        :TG_BSIK-WRBTR,
        :TG_BSIK-VBUND.

      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        APPEND tg_bsik.
        CLEAR: tg_bsik.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DOCUMENTOS
    ENDEXEC.

  ENDLOOP.

  DATA(lt_bsik) = tg_bsik[].
  SORT lt_bsik BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_bsik COMPARING lifnr.

  IF lt_bsik IS NOT INITIAL.

    SELECT lifnr land1
      FROM lfa1
      APPENDING TABLE t_lfa1
      FOR ALL ENTRIES IN lt_bsik
      WHERE lifnr = lt_bsik-lifnr.
    IF sy-subrc IS INITIAL.
      SORT t_lfa1 BY lifnr.
    ENDIF.

  ENDIF.

*  SELECT BUKRS LIFNR UMSKS UMSKZ AUGDT AUGBL ZUONR GJAHR BELNR BUZEI BUDAT
*         BLDAT WAERS XBLNR BLART GSBER EBELN EBELP BSCHL SHKZG ZLSCH ZLSPR HKONT
*         SGTXT HBKID ZFBDT ZBD1T DMBTR DMBE2
*    FROM BSIK INTO TABLE TG_BSIK
*   WHERE BUKRS IN S_BUKRS
*     AND SHKZG EQ 'H'.

ENDFORM.

FORM load_bsid.

  "PSA
  "                 AND TRIM(ZFBDT) <> ''
  "               AND TRIM(ZFBDT) <> '00000000'
  "               AND ( TO_DATE(ZFBDT,'YYYYMMDD' ) + NVL(ZBD1T,0) ) BETWEEN  TRUNC(:sy-datum) AND TRUNC(:sy-datum + 30)
  "xblnr

  LOOP AT tg_bukrs.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS FOR
            SELECT BUKRS, KUNNR, UMSKS, UMSKZ, ZUONR, GJAHR, BELNR, BUZEI, BUDAT, BLDAT, WAERS,
                   XBLNR, BLART, GSBER, VBEL2, VPOS2, HKONT, SGTXT, HBKID, BSCHL, SHKZG, ZLSCH, ZLSPR,
                   ZFBDT, ZBD1T, DMBTR, DMBE2, WRBTR, VBUND
              FROM BSID
             WHERE 1 = 1
               and trim(ZFBDT) <> '00000000'
               and length(trim(ZFBDT)) = 8
               AND ADD_DAYS(cast(trim(ZFBDT) as date),TO_NUMBER(ZBD1T)) BETWEEN  current_date AND ADD_DAYS(current_date,30)
               AND BUKRS = :TG_BUKRS-BUKRS
               AND SHKZG = 'S'
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT DOCUMENTOS INTO
        :TG_BSID-BUKRS,
        :TG_BSID-KUNNR,
        :TG_BSID-UMSKS,
        :TG_BSID-UMSKZ,
        :TG_BSID-ZUONR,
        :TG_BSID-GJAHR,
        :TG_BSID-BELNR,
        :TG_BSID-BUZEI,
        :TG_BSID-BUDAT,
        :TG_BSID-BLDAT,
        :TG_BSID-WAERS,
        :TG_BSID-XBLNR,
        :TG_BSID-BLART,
        :TG_BSID-GSBER,
        :TG_BSID-VBEL2,
        :TG_BSID-VPOS2,
        :TG_BSID-HKONT,
        :TG_BSID-SGTXT,
        :TG_BSID-HBKID,
        :TG_BSID-BSCHL,
        :TG_BSID-SHKZG,
        :TG_BSID-ZLSCH,
        :TG_BSID-ZLSPR,
        :TG_BSID-ZFBDT,
        :TG_BSID-ZBD1T,
        :TG_BSID-DMBTR,
        :TG_BSID-DMBE2,
        :TG_BSID-WRBTR,
        :TG_BSID-VBUND.
      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        APPEND tg_bsid.
        CLEAR: tg_bsid.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DOCUMENTOS
    ENDEXEC.

  ENDLOOP.

  DATA(lt_bsid) = tg_bsid[].
  SORT lt_bsid BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_bsid COMPARING kunnr.

  IF lt_bsid IS NOT INITIAL.

    SELECT kunnr land1
      FROM kna1
      INTO TABLE t_kna1
      FOR ALL ENTRIES IN lt_bsid
      WHERE kunnr = lt_bsid-kunnr.
    IF sy-subrc IS INITIAL.
      SORT t_kna1 BY kunnr.
    ENDIF.

  ENDIF.

*  SELECT BUKRS KUNNR UMSKS UMSKZ ZUONR GJAHR BELNR BUZEI BUDAT BLDAT WAERS
*         XBLNR BLART GSBER VBEL2 VPOS2 HKONT SGTXT HBKID BSCHL SHKZG ZLSCH ZLSPR
*         ZFBDT ZBD1T DMBTR DMBE2
*    FROM BSID INTO TABLE TG_BSID
*   WHERE BUKRS IN S_BUKRS
*     AND ZFBDT >= IT_DT_VENC-LOW
*     AND SHKZG EQ 'S'.

ENDFORM.

FORM f_atrib_valor USING p_bukrs      TYPE bukrs
                         p_waers_doc  TYPE waers
                         p_dmbtr      TYPE dmbtr
                         p_dmbe2      TYPE dmbe2
                         p_wrbtr      TYPE wrbtr
                         p_kursf      TYPE ukurs_curr
                CHANGING c_dmbtr      TYPE dmbtr
                         c_dmbe2      TYPE dmbe2.

  DATA: vl_msg_error TYPE string,
        vl_c_dmbtr   TYPE string,
        vl_c_dmbe2   TYPE string.

  CALL FUNCTION 'ZFI_CONV_MOEDA_FLX'
    EXPORTING
      i_bukrs         = p_bukrs
      i_waers_doc     = p_waers_doc
      i_dmbtr         = p_dmbtr
      i_dmbe2         = p_dmbe2
      i_wrbtr         = p_wrbtr
      i_kursf_fix     = p_kursf
      i_tx_usd_brl    = vg_tx_usd_brl
      i_tx_usd_ars    = vg_tx_usd_ars
      i_tx_eur_brl    = vg_tx_eur_brl
      i_tx_eur_usd    = vg_tx_eur_usd
    IMPORTING
      e_msg           = vl_msg_error
    TABLES
      t_curr_inf      = tg_curr_inf
    CHANGING
      c_dmbtr         = c_dmbtr
      c_dmbe2         = c_dmbe2
    EXCEPTIONS
      data_inconplete = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    vl_c_dmbtr = p_dmbtr.
    vl_c_dmbe2 = p_dmbe2.

    CONCATENATE 'Erro Proc.: Emp:' p_bukrs
                'Moeda:'           p_waers_doc
                '1ª Moeda :'       vl_c_dmbtr
                '2ª Moeda :'       vl_c_dmbe2
                'Detalhe:'         vl_msg_error
           INTO vg_error_prc SEPARATED BY space.
  ENDIF.


ENDFORM.

FORM f_monta_obj_key_prev  USING p_processo
                                 p_obj_key_prev  TYPE zfit0079-obj_key_prev
                                 p_seq_item_prev TYPE zfit0079-seqitem
                        CHANGING p_0079          TYPE zfit0079.

  CASE p_processo.
    WHEN '001'. "Adiantamento Pedido
      p_0079-obj_key_prev = p_processo && p_obj_key_prev.
      p_0079-seqitem      = p_seq_item_prev.
    WHEN '002'. "Adiantamento Frete
      p_0079-obj_key_prev = p_processo && p_obj_key_prev.
      p_0079-seqitem      = p_seq_item_prev.
    WHEN '003'. "Saldo Frete
      p_0079-obj_key_prev = p_processo && p_obj_key_prev.
      p_0079-seqitem      = p_seq_item_prev.
    WHEN '004'. "Frete Outros
      p_0079-obj_key_prev = p_processo && p_obj_key_prev.
      p_0079-seqitem      = p_seq_item_prev.
    WHEN '005'. "Prev. Automatica Rec. X Pgto
      p_0079-obj_key_prev = p_processo && p_obj_key_prev.
      p_0079-seqitem      = p_seq_item_prev.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_proximo_dia_util CHANGING p_data TYPE sy-datum.

  DATA: v_data_aux TYPE sy-datum.

  CHECK p_data IS NOT INITIAL.

  v_data_aux = p_data.

  "Jogar para o dia útil anterior
  zcl_miro=>get_proximo_dia_util(
    EXPORTING
      i_data_base = v_data_aux
      i_signum    = '+'
      i_ck_data_zles0145 = abap_true
    RECEIVING
      r_data      = v_data_aux
    EXCEPTIONS
      erro        = 1
      OTHERS      = 2 ).

  IF v_data_aux IS NOT INITIAL.
    p_data = v_data_aux.
  ENDIF.

ENDFORM.

FORM load_bsik_for_bsid.

  DATA: lva_bukrs_pgto TYPE j_1bbranch-bukrs,
        l_tvarv        TYPE tvarv-name.

  DATA:  vl_tipo_doc      TYPE string.

  READ TABLE it_dt_venc INTO DATA(lwa_dt_venc) INDEX 1.

  CHECK sy-subrc EQ 0.

  CLEAR: tg_0175[], tg_ekko_aux[], tg_ekpo[].
  SELECT *
    INTO TABLE tg_0175
    FROM zfit0175.

  IF tg_0175[] IS INITIAL.
    MESSAGE 'Não encontrado Parametros Previsao Automatica Intercompany' TYPE 'S'.
    EXIT.
  ENDIF.

  "PSA S1
  "            AND TRIM(ZFBDT)           <> ''
  "            AND TRIM(ZFBDT)           <> '00000000'
  "            AND ( TO_DATE(ZFBDT,'YYYYMMDD' ) + NVL(ZBD1T,0) ) BETWEEN  TO_DATE(:LWA_DT_VENC-LOW,'YYYYMMDD' )  AND TRUNC(TO_DATE(:LWA_DT_VENC-HIGH,'YYYYMMDD' ) + 30)
  "PSA S2
  "             AND A.AUGDT               >= :SY-DATUM
  "             AND B.MANDT               = A.MANDT
  "xblnr
  TRY.
      EXEC SQL.
        OPEN DOCUMENTOS FOR
         SELECT C.BUKRS, A.BUKRS as BUKRS_P, LPAD( A.BUKRS || '01',10,'0' ) AS LIFNR, A.UMSKS, A.UMSKZ, A.AUGDT, A.AUGBL, A.ZUONR, A.GJAHR, A.BELNR, A.BUZEI, A.BUDAT,
                A.BLDAT, A.WAERS, A.BLART, A.GSBER, A.EBELN, A.EBELP, A.BSCHL, A.SHKZG, A.ZLSCH, A.ZLSPR, A.HKONT,
                A.SGTXT, A.HBKID, A.ZFBDT, A.ZBD1T, A.DMBTR, A.DMBE2, A.WRBTR, A.xblnr
          FROM BSIK       A,
               LFA1       B,
               J_1BBRANCH C
          WHERE 1 = 1
            AND C.MANDT               = '300'
            AND A.LIFNR               = B.LIFNR
            AND SUBSTR(B.LIFNR,7,10)  = C.BRANCH
            AND SHKZG                 = 'H'
            AND A.HBKID               <> ' '
            AND B.KTOKK               = 'ZFIC'
and trim(ZFBDT) <> '00000000'
and length(trim(ZFBDT)) = 8
            AND ADD_DAYS(cast(trim(ZFBDT) as date),TO_NUMBER(ZBD1T)) BETWEEN  cast(:LWA_DT_VENC-LOW as date) AND ADD_DAYS(cast(:LWA_DT_VENC-HIGH as date ),30)

         UNION ALL

          SELECT C.BUKRS, A.BUKRS as BUKRS_P, LPAD( A.BUKRS || '01',10,'0' ) AS LIFNR, A.UMSKS, A.UMSKZ, A.AUGDT, A.AUGBL, A.ZUONR, A.GJAHR, A.BELNR, A.BUZEI, A.BUDAT,
                 A.BLDAT, A.WAERS, A.BLART, A.GSBER, A.EBELN, A.EBELP, A.BSCHL, A.SHKZG, A.ZLSCH, A.ZLSPR, A.HKONT,
                 A.SGTXT, A.HBKID, A.ZFBDT, A.ZBD1T, A.DMBTR, A.DMBE2, A.WRBTR, A.xblnr
            FROM BSAK       A,
                 LFA1       B,
                 J_1BBRANCH C
           WHERE 1 = 1
             AND A.LIFNR               = B.LIFNR
             AND SUBSTR(B.LIFNR,7,10)  = C.BRANCH
             AND SHKZG                 = 'H'
             AND A.HBKID               <> ' '
             AND B.KTOKK               = 'ZFIC'
             AND TRIM(ZFBDT)           <> '00000000'
             and ZFBDT <> ''
             AND SUBSTR(A.AUGBL,1,2) in ('15', '20')
             AND A.AUGDT               >= current_date

      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS INTO
      :TG_BSIK_FOR_BSID-BUKRS,
      :TG_BSIK_FOR_BSID-BUKRS_P,
      :TG_BSIK_FOR_BSID-LIFNR,
      :TG_BSIK_FOR_BSID-UMSKS,
      :TG_BSIK_FOR_BSID-UMSKZ,
      :TG_BSIK_FOR_BSID-AUGDT,
      :TG_BSIK_FOR_BSID-AUGBL,
      :TG_BSIK_FOR_BSID-ZUONR,
      :TG_BSIK_FOR_BSID-GJAHR,
      :TG_BSIK_FOR_BSID-BELNR,
      :TG_BSIK_FOR_BSID-BUZEI,
      :TG_BSIK_FOR_BSID-BUDAT,
      :TG_BSIK_FOR_BSID-BLDAT,
      :TG_BSIK_FOR_BSID-WAERS,
      :TG_BSIK_FOR_BSID-BLART,
      :TG_BSIK_FOR_BSID-GSBER,
      :TG_BSIK_FOR_BSID-EBELN,
      :TG_BSIK_FOR_BSID-EBELP,
      :TG_BSIK_FOR_BSID-BSCHL,
      :TG_BSIK_FOR_BSID-SHKZG,
      :TG_BSIK_FOR_BSID-ZLSCH,
      :TG_BSIK_FOR_BSID-ZLSPR,
      :TG_BSIK_FOR_BSID-HKONT,
      :TG_BSIK_FOR_BSID-SGTXT,
      :TG_BSIK_FOR_BSID-HBKID,
      :TG_BSIK_FOR_BSID-ZFBDT,
      :TG_BSIK_FOR_BSID-ZBD1T,
      :TG_BSIK_FOR_BSID-DMBTR,
      :TG_BSIK_FOR_BSID-DMBE2,
      :TG_BSIK_FOR_BSID-WRBTR,
      :TG_BSIK_FOR_BSID-XBLNR.

    ENDEXEC.
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
      APPEND tg_bsik_for_bsid.
      CLEAR: tg_bsik_for_bsid.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS
  ENDEXEC.

  DELETE tg_bsik_for_bsid WHERE zlspr IS NOT INITIAL. "Remove Pagamentos Bloqueados

****  US - 79059 - CBRAND - Inicio
  IF tg_bsik_for_bsid[] IS NOT INITIAL.
    SELECT ebeln bsart
      FROM ekko APPENDING TABLE tg_ekko_aux
       FOR ALL ENTRIES IN tg_bsik_for_bsid
     WHERE ebeln EQ tg_bsik_for_bsid-ebeln.

    SELECT ebeln matnr matkl
     FROM ekpo APPENDING TABLE tg_ekpo
      FOR ALL ENTRIES IN tg_ekko_aux
    WHERE ebeln EQ tg_ekko_aux-ebeln.
  ENDIF.

  LOOP AT tg_bsik_for_bsid.

    READ TABLE tg_ekko_aux WITH KEY ebeln = tg_bsik_for_bsid-ebeln.

    CHECK ( sy-subrc = 0 ).

    READ TABLE tg_ekpo WITH KEY ebeln = tg_ekko_aux-ebeln.

    CHECK sy-subrc = 0.

    CHECK tg_bsik_for_bsid-bukrs IN rg_bukrs_proc.

    LOOP AT tg_0175 WHERE  bukrs_pgto  = tg_bsik_for_bsid-bukrs_p
                       AND bukrs_rcbto = tg_bsik_for_bsid-bukrs .

      CLEAR: vl_tipo_doc.
      vl_tipo_doc = tg_0175-bsart.

      FIND tg_ekko_aux-bsart IN vl_tipo_doc.
      IF sy-subrc = 0 AND tg_0175-umsks = tg_bsik_for_bsid-umsks AND tg_0175-umskz = tg_bsik_for_bsid-umskz.
        tg_bsik_for_bsid-certo = 'X'.
        tg_bsik_for_bsid-bsart = tg_ekko_aux-bsart.
        MODIFY tg_bsik_for_bsid.
        CONTINUE.
      ENDIF.

      IF tg_0175-matkl IS NOT INITIAL AND  tg_0175-matkl = tg_ekpo-matkl
        AND tg_0175-umsks = tg_bsik_for_bsid-umsks AND tg_0175-umskz = tg_bsik_for_bsid-umskz.
        tg_bsik_for_bsid-certo = 'X'.
        MODIFY tg_bsik_for_bsid.
        CONTINUE.
      ENDIF.

      IF tg_0175-matnr IS NOT INITIAL AND tg_0175-matnr = tg_ekpo-matnr
        AND tg_0175-umsks = tg_bsik_for_bsid-umsks AND tg_0175-umskz = tg_bsik_for_bsid-umskz.
        tg_bsik_for_bsid-certo = 'X'.
        MODIFY tg_bsik_for_bsid.
        CONTINUE.
      ENDIF.

      FIND tg_ekko_aux-bsart IN vl_tipo_doc.
      IF sy-subrc = 0 AND tg_0175-umsks = tg_bsik_for_bsid-umsks AND tg_0175-umskz = tg_bsik_for_bsid-umskz
        AND tg_0175-matkl = tg_ekpo-matkl.
        tg_bsik_for_bsid-certo = 'X'.
        MODIFY tg_bsik_for_bsid.
        CONTINUE.
      ENDIF.

      FIND tg_ekko_aux-bsart IN vl_tipo_doc.
      IF sy-subrc = 0 AND tg_0175-umsks = tg_bsik_for_bsid-umsks AND tg_0175-umskz = tg_bsik_for_bsid-umskz
        AND tg_0175-matnr = tg_ekpo-matnr.
        tg_bsik_for_bsid-certo = 'X'.
        MODIFY tg_bsik_for_bsid.
        CONTINUE.
      ENDIF.

      FIND tg_ekko_aux-bsart IN vl_tipo_doc.
      IF sy-subrc = 0 AND tg_0175-umsks = tg_bsik_for_bsid-umsks AND tg_0175-umskz = tg_bsik_for_bsid-umskz
        AND tg_0175-matnr = tg_ekpo-matnr AND tg_0175-matkl = tg_ekpo-matkl.
        tg_bsik_for_bsid-certo = 'X'.
        MODIFY tg_bsik_for_bsid.
        CONTINUE.
      ENDIF.
      tg_bsik_for_bsid-bsart = tg_ekko_aux-bsart.
      MODIFY tg_bsik_for_bsid.
    ENDLOOP.


  ENDLOOP.

  DELETE tg_bsik_for_bsid[] WHERE certo IS INITIAL.

****  US - 79059 - CBRAND - Fim

** Código original - US - 79059 - CBRAND -Inicio
*-CS2021000936 - 11.10.2021 - JT - inicio
*  FREE tg_bukrs_prev.
* US - 79059 - Inicio - CBRAND
*  LOOP AT tg_bukrs.
*    l_tvarv = 'ZFI0100_REC_AUT_BUKRS_' && tg_bukrs.
*
*    SELECT *
*      INTO TABLE @DATA(t_tvarvc)
*      FROM tvarvc
*     WHERE name = @l_tvarv.
*
*    LOOP AT t_tvarvc        INTO DATA(w_tvarvc).
*      wa_bukrs_prev-bukrs      = tg_bukrs.
*      wa_bukrs_prev-bukrs_prev = w_tvarvc-low.
*      APPEND wa_bukrs_prev    TO tg_bukrs_prev.
*    ENDLOOP.
*  ENDLOOP.
* US - 79059 - Fim - CBRAND

* LOOP AT tg_bukrs.
*   CASE tg_bukrs .
*     WHEN '0001'.
*       lva_bukrs_pgto = '0015'.
*     WHEN OTHERS.
*       CONTINUE.
*   ENDCASE.
*  LOOP AT tg_bukrs_prev INTO wa_bukrs_prev.
*    tg_bukrs       = wa_bukrs_prev-bukrs.
*    lva_bukrs_pgto = wa_bukrs_prev-bukrs_prev.
*-CS2021000936 - 11.10.2021 - JT - fim
*    TRY.
*        EXEC SQL.
*          OPEN DOCUMENTOS FOR
*           SELECT C.BUKRS, LPAD( A.BUKRS || '01',10,'0' ) AS LIFNR, A.UMSKS, A.UMSKZ, A.AUGDT, A.AUGBL, A.ZUONR, A.GJAHR, A.BELNR, A.BUZEI, A.BUDAT,
*                  A.BLDAT, A.WAERS, A.XBLNR, A.BLART, A.GSBER, A.EBELN, A.EBELP, A.BSCHL, A.SHKZG, A.ZLSCH, A.ZLSPR, A.HKONT,
*                  A.SGTXT, A.HBKID, A.ZFBDT, A.ZBD1T, A.DMBTR, A.DMBE2, A.WRBTR
*            FROM BSIK       A,
*                 LFA1       B,
*                 J_1BBRANCH C
*            WHERE A.MANDT               = :SY-MANDT
*              AND B.MANDT               = :SY-MANDT
*              AND C.MANDT               = '300'
*              AND A.LIFNR               = B.LIFNR
*              AND SUBSTR(B.LIFNR,7,10)  = C.BRANCH
*              AND A.BUKRS               = :LVA_BUKRS_PGTO
*              AND SHKZG                 = 'H'
*              AND A.UMSKZ               = 'F'
*              AND A.UMSKS               = 'A'
*              AND A.HBKID               <> ' '
*              AND B.KTOKK               = 'ZFIC'
*              AND C.BUKRS               = :TG_BUKRS
*              AND TRIM(ZFBDT)           IS NOT NULL
*              AND TRIM(ZFBDT)           <> '00000000'

*              AND ( TO_DATE(ZFBDT,'YYYYMMDD' ) + NVL(ZBD1T,0) ) BETWEEN  TO_DATE(:LWA_DT_VENC-LOW,'YYYYMMDD' )  AND TRUNC(TO_DATE(:LWA_DT_VENC-HIGH,'YYYYMMDD' ) + 30)
*
*           UNION ALL
*
*            SELECT C.BUKRS, LPAD( A.BUKRS || '01',10,'0' ) AS LIFNR, A.UMSKS, A.UMSKZ, A.AUGDT, A.AUGBL, A.ZUONR, A.GJAHR, A.BELNR, A.BUZEI, A.BUDAT,
*                   A.BLDAT, A.WAERS, A.XBLNR, A.BLART, A.GSBER, A.EBELN, A.EBELP, A.BSCHL, A.SHKZG, A.ZLSCH, A.ZLSPR, A.HKONT,
*                   A.SGTXT, A.HBKID, A.ZFBDT, A.ZBD1T, A.DMBTR, A.DMBE2, A.WRBTR
*              FROM BSAK       A,
*                   LFA1       B,
*                   J_1BBRANCH C
*             WHERE A.MANDT               = :SY-MANDT
*               AND B.MANDT               = :SY-MANDT
*               AND C.MANDT               = '300'
*               AND A.LIFNR               = B.LIFNR
*               AND SUBSTR(B.LIFNR,7,10)  = C.BRANCH
*               AND A.BUKRS               = :LVA_BUKRS_PGTO
*               AND SHKZG                 = 'H'
*               AND A.UMSKZ               = 'F'
*               AND A.UMSKS               = 'A'
*               AND A.HBKID               <> ' '
*               AND B.KTOKK               = 'ZFIC'
*               AND C.BUKRS               = :TG_BUKRS
*               AND TRIM(ZFBDT)           IS NOT NULL
*               AND TRIM(ZFBDT)           <> '00000000'
*               AND SUBSTR(A.AUGBL,1,2) in ('15', '20')
*               AND A.AUGDT               >= :SY-DATUM
*
*        ENDEXEC.
*      CATCH cx_sy_native_sql_error INTO exc_ref.
*        error_text = exc_ref->get_text( ).
*        MESSAGE error_text TYPE 'E' RAISING erro_sql.
*    ENDTRY.
*
*    DO.
*      EXEC SQL.
*        FETCH NEXT DOCUMENTOS INTO
*        :TG_BSIK_FOR_BSID-BUKRS,
*        :TG_BSIK_FOR_BSID-LIFNR,
*        :TG_BSIK_FOR_BSID-UMSKS,
*        :TG_BSIK_FOR_BSID-UMSKZ,
*        :TG_BSIK_FOR_BSID-AUGDT,
*        :TG_BSIK_FOR_BSID-AUGBL,
*        :TG_BSIK_FOR_BSID-ZUONR,
*        :TG_BSIK_FOR_BSID-GJAHR,
*        :TG_BSIK_FOR_BSID-BELNR,
*        :TG_BSIK_FOR_BSID-BUZEI,
*        :TG_BSIK_FOR_BSID-BUDAT,
*        :TG_BSIK_FOR_BSID-BLDAT,
*        :TG_BSIK_FOR_BSID-WAERS,
*        :TG_BSIK_FOR_BSID-XBLNR,
*        :TG_BSIK_FOR_BSID-BLART,
*        :TG_BSIK_FOR_BSID-GSBER,
*        :TG_BSIK_FOR_BSID-EBELN,
*        :TG_BSIK_FOR_BSID-EBELP,
*        :TG_BSIK_FOR_BSID-BSCHL,
*        :TG_BSIK_FOR_BSID-SHKZG,
*        :TG_BSIK_FOR_BSID-ZLSCH,
*        :TG_BSIK_FOR_BSID-ZLSPR,
*        :TG_BSIK_FOR_BSID-HKONT,
*        :TG_BSIK_FOR_BSID-SGTXT,
*        :TG_BSIK_FOR_BSID-HBKID,
*        :TG_BSIK_FOR_BSID-ZFBDT,
*        :TG_BSIK_FOR_BSID-ZBD1T,
*        :TG_BSIK_FOR_BSID-DMBTR,
*        :TG_BSIK_FOR_BSID-DMBE2,
*        :TG_BSIK_FOR_BSID-WRBTR.
*
*      ENDEXEC.
*      IF sy-subrc <> 0.
*        EXIT.
*      ELSE.
*        APPEND tg_bsik_for_bsid.
*        CLEAR: tg_bsik_for_bsid.
*      ENDIF.
*    ENDDO.
*
*    EXEC SQL.
*      CLOSE DOCUMENTOS
*    ENDEXEC.
*
*  ENDLOOP.
** Código original - US - 79059 - CBRAND - Fim

ENDFORM.
