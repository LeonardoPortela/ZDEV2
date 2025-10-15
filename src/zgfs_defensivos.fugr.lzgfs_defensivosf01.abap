*----------------------------------------------------------------------*
***INCLUDE LZGFS_DEFENSIVOSF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  regras parametrizacao material
*&---------------------------------------------------------------------*
FORM f_recupera_parametros USING    p_vbeln
                                    p_matnr
                           CHANGING p_qtd_dias_vencimento
                                    p_bloq_lotes_vencidos
                                    p_lote_vencmto_proximo
                                    p_regra_vencto.

  DATA: w_0301       TYPE zsdt0301.

  FREE: p_qtd_dias_vencimento,
        p_bloq_lotes_vencidos,
        p_lote_vencmto_proximo,
        p_regra_vencto,
        w_0301.

  SELECT SINGLE vbeln, vkorg
    INTO @DATA(w_vbak)
    FROM vbak
   WHERE vbeln = @p_vbeln.

  CHECK sy-subrc = 0.

  SELECT SINGLE matnr, matkl
    INTO @DATA(w_mara)
    FROM mara
   WHERE matnr = @p_matnr.

  CHECK sy-subrc = 0.

*-busca por material
  SELECT SINGLE *
    INTO w_0301
    FROM zsdt0301
   WHERE vkorg     = w_vbak-vkorg
     AND matnr     = w_mara-matnr
     AND cancelado = abap_false.
  IF sy-subrc = 0.
    p_regra_vencto = abap_true.
  ELSE.
*---busca por Grupo material
    SELECT SINGLE *
      INTO w_0301
      FROM zsdt0301
     WHERE vkorg     = w_vbak-vkorg
       AND matkl     = w_mara-matkl
       AND cancelado = abap_false.
    IF sy-subrc = 0.
      p_regra_vencto = abap_true.
    ENDIF.
  ENDIF.

  p_qtd_dias_vencimento  = w_0301-qtd_dias_vencimento.
  p_bloq_lotes_vencidos  = w_0301-bloq_lotes_vencidos.
  p_lote_vencmto_proximo = w_0301-lote_vencmto_proximo.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECAO_DADOS
*&---------------------------------------------------------------------*
FORM f_selecao_dados  USING p_ordem_carga TYPE zsdt0304.

  DATA: l_qte_lib   TYPE char50,
        l_clabs     TYPE mchb-clabs,
        l_total     TYPE zsdt0134-lfimg,
        lr_lgort    TYPE RANGE OF lgort_d WITH HEADER LINE,
        l_total_lib TYPE zsdt0134-lfimg.

  FREE: t_zsdt0001, it_mch1, t_lotes, t_lotes_tot.

* CS2023000389 ZSDT0112 - Exibir o depósito e ajustar a regra de escolha de lotes - Ini
  SELECT lgort FROM zsdt0335
    INTO TABLE @DATA(it_0335).
  IF sy-subrc IS INITIAL.
    LOOP AT it_0335 INTO DATA(wa_0335).
      lr_lgort-sign   = 'I'.
      lr_lgort-option = 'EQ'.
      lr_lgort-low    = wa_0335-lgort.
      APPEND lr_lgort.
      CLEAR lr_lgort.
    ENDLOOP.
*  ELSE.
*    MESSAGE s024(sd) WITH 'Nenhum depósito parametrizado transação ZSDT0335'(I01) DISPLAY LIKE 'W'.
  ENDIF.
* CS2023000389 ZSDT0112 - Exibir o depósito e ajustar a regra de escolha de lotes - Fim

*-------------------------------
* CABECALHO
*-------------------------------
  gv_qte_lib = p_ordem_carga-qte_lib.
  WRITE p_ordem_carga-qte_lib TO l_qte_lib.
  CONDENSE l_qte_lib.
  CONCATENATE 'Quantidade de Material na Carga:' l_qte_lib
         INTO g_quant_carga SEPARATED BY space.

*-------------------------------
* selecao lotes
*-------------------------------
  SELECT zsdt0134~*
    INTO TABLE @t_zsdt0134
    FROM zsdt0134
   INNER JOIN vbap  ON vbap~vbeln = zsdt0134~vbeln
                   AND vbap~posnr = zsdt0134~posnr
   WHERE vbap~matnr       = @p_ordem_carga-matnr
     AND vbap~werks       = @p_ordem_carga-werks
     AND zsdt0134~vbeln   = @p_ordem_carga-vbeln       "CS2025000249 - ajuste para filtrar pelo vbeln - GABRIEL AVILA - BG 193487"
     AND zsdt0134~posnr   = @p_ordem_carga-posnr       "CS2025000249 - ajuste para filtrar pelo posnr - GABRIEL AVILA - BG 193487"
     AND zsdt0134~nro_cg  = @p_ordem_carga-nro_cgd     "CS2025000249 - ajuste para filtrar pelo nro_cg - GABRIEL AVILA - BG 193487"
     AND zsdt0134~status  = @abap_false.

  IF t_zsdt0134[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0001
      INTO TABLE t_zsdt0001
       FOR ALL ENTRIES IN t_zsdt0134
     WHERE ch_referencia = t_zsdt0134-ch_referencia.

    DELETE t_zsdt0001 WHERE doc_rem = abap_off.
  ENDIF.

  SELECT *
    FROM mchb
    INTO TABLE it_mchb
   WHERE matnr EQ p_ordem_carga-matnr
     AND werks EQ p_ordem_carga-werks.

  SELECT *
    FROM mslb
    INTO TABLE it_mslb
   WHERE matnr EQ p_ordem_carga-matnr
     AND werks EQ p_ordem_carga-werks.

  IF it_mchb[] IS NOT INITIAL.
    SELECT *
      FROM mch1
      INTO TABLE it_mch1
       FOR ALL ENTRIES IN it_mchb
     WHERE charg EQ it_mchb-charg
       AND matnr EQ it_mchb-matnr.
  ENDIF.

*------------------------------------
* total utilizado em todas as cargas
*------------------------------------
  LOOP AT t_zsdt0134  INTO w_zsdt0134.
    CLEAR: w_lotes.
    w_lotes-vbeln        = w_zsdt0134-vbeln.
    w_lotes-posnr        = w_zsdt0134-posnr.
    w_lotes-nro_cg       = w_zsdt0134-nro_cg.
    w_lotes-charg        = w_zsdt0134-charg.
    w_lotes-werks        = p_ordem_carga-werks.
    w_lotes-matnr        = p_ordem_carga-matnr.
    w_lotes-lgort        = w_zsdt0134-lgort.
    w_lotes-lifnr        = w_zsdt0134-lifnr.
    w_lotes-lfimg        = w_zsdt0134-lfimg.
    w_lotes-lfimg_ori    = w_zsdt0134-lfimg.
    COLLECT w_lotes   INTO t_lotes.
  ENDLOOP.

*------------------------------------
* agrupa por armazen / deposito
*------------------------------------
  LOOP AT t_zsdt0134  INTO w_zsdt0134.
    CLEAR: w_lotes.

    READ TABLE t_zsdt0001 INTO w_zsdt0001 WITH KEY ch_referencia = w_zsdt0134-ch_referencia.
    CHECK sy-subrc <> 0.

    w_lotes-charg        = w_zsdt0134-charg.
    w_lotes-lgort        = w_zsdt0134-lgort.
    w_lotes-lifnr        = w_zsdt0134-lifnr.
    w_lotes-lfimg        = w_zsdt0134-lfimg.
    w_lotes-lfimg_ori    = w_zsdt0134-lfimg.
    COLLECT w_lotes   INTO t_lotes_tot.
  ENDLOOP.

*------------------------------------
* so ficam lotes desta carga
*------------------------------------
  DELETE t_lotes WHERE vbeln  <> p_ordem_carga-vbeln
                    OR posnr  <> p_ordem_carga-posnr
                    OR nro_cg <> p_ordem_carga-nro_cgd.

  SORT t_lotes     BY charg.
  SORT t_lotes_tot BY charg.

*------------------------------------
* ajusta utilizacao livre pra cada lote
*------------------------------------
  LOOP AT t_lotes INTO w_lotes.
    l_tabix = sy-tabix.

    CLEAR l_clabs.

    READ TABLE it_mchb INTO wa_mchb WITH KEY charg = w_lotes-charg
                                             lgort = w_lotes-lgort.
    IF sy-subrc = 0.
      l_clabs = l_clabs + wa_mchb-clabs.
    ELSE.
      READ TABLE it_mchb INTO wa_mchb WITH KEY charg = w_lotes-charg.
      IF sy-subrc = 0.
        l_clabs = l_clabs + wa_mchb-clabs.
      ENDIF.
    ENDIF.

    READ TABLE it_mslb INTO wa_mslb WITH KEY charg = w_lotes-charg
                                             lifnr = w_lotes-lifnr.
    IF sy-subrc = 0.
      l_clabs = l_clabs + wa_mslb-lblab.
    ELSE.
      READ TABLE it_mslb INTO wa_mslb WITH KEY charg = w_lotes-charg.
      IF sy-subrc = 0.
        l_clabs = l_clabs + wa_mslb-lblab.
      ENDIF.
    ENDIF.

    w_lotes-clabs         = w_lotes-clabs     + l_clabs.
    w_lotes-clabs_ori     = w_lotes-clabs_ori + l_clabs.
    MODIFY t_lotes     FROM w_lotes INDEX l_tabix.
  ENDLOOP.

  LOOP AT it_mchb   INTO wa_mchb.
    CLEAR: w_lotes, w_lotes_tot.

    READ TABLE t_lotes_tot INTO w_lotes_tot WITH KEY charg = wa_mchb-charg
                                                     lgort = wa_mchb-lgort.

    READ TABLE t_lotes INTO w_lotes WITH KEY nro_cg = p_ordem_carga-nro_cgd
                                             vbeln  = p_ordem_carga-vbeln
                                             posnr  = p_ordem_carga-posnr
                                             charg  = wa_mchb-charg
                                             lgort  = wa_mchb-lgort.
    IF sy-subrc = 0.
      w_lotes-clabs     = w_lotes-clabs - w_lotes_tot-lfimg.
      w_lotes-clabs_ori = w_lotes-clabs + w_lotes-lfimg.
      MODIFY t_lotes FROM w_lotes INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    w_lotes-vbeln      = p_ordem_carga-vbeln.
    w_lotes-posnr      = p_ordem_carga-posnr.
    w_lotes-nro_cg     = p_ordem_carga-nro_cgd.
    w_lotes-charg      = wa_mchb-charg.
    w_lotes-werks      = wa_mchb-werks.
    w_lotes-matnr      = wa_mchb-matnr.
    w_lotes-lgort      = wa_mchb-lgort.
    w_lotes-clabs_ori  = wa_mchb-clabs - w_lotes_tot-lfimg.
    w_lotes-clabs      = wa_mchb-clabs - w_lotes_tot-lfimg.
    COLLECT w_lotes INTO t_lotes.
  ENDLOOP.

  LOOP AT it_mslb   INTO wa_mslb.
    CLEAR: w_lotes, w_lotes_tot.

    READ TABLE t_lotes_tot INTO w_lotes_tot WITH KEY charg = wa_mslb-charg
                                                     lifnr = wa_mslb-lifnr.

    READ TABLE t_lotes INTO w_lotes WITH KEY nro_cg = p_ordem_carga-nro_cgd
                                                 vbeln  = p_ordem_carga-vbeln
                                                 posnr  = p_ordem_carga-posnr
                                                 charg  = wa_mslb-charg
                                                 lifnr  = wa_mslb-lifnr.
    IF sy-subrc = 0.
      w_lotes-clabs     = w_lotes-clabs - w_lotes_tot-lfimg.
      w_lotes-clabs_ori = w_lotes-clabs + w_lotes-lfimg.
      MODIFY t_lotes FROM w_lotes INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    w_lotes-vbeln      = p_ordem_carga-vbeln.
    w_lotes-posnr      = p_ordem_carga-posnr.
    w_lotes-nro_cg     = p_ordem_carga-nro_cgd.
    w_lotes-charg      = wa_mslb-charg.
    w_lotes-werks      = wa_mslb-werks.
    w_lotes-matnr      = wa_mslb-matnr.
    w_lotes-lifnr      = wa_mslb-lifnr.
    w_lotes-clabs_ori  = wa_mslb-lblab - w_lotes_tot-lfimg.
    w_lotes-clabs      = wa_mslb-lblab - w_lotes_tot-lfimg.
    COLLECT w_lotes INTO t_lotes.
  ENDLOOP.

*------------------------------------
* ajusta data vencimento
*------------------------------------
  LOOP AT t_lotes    INTO w_lotes.
    l_tabix = sy-tabix.
    READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = w_lotes-matnr
                                             charg = w_lotes-charg.
    IF sy-subrc IS INITIAL.
      w_lotes-vfdat     = wa_mch1-vfdat.
      MODIFY t_lotes FROM w_lotes INDEX l_tabix.
    ENDIF.
  ENDLOOP.

* CS2023000389 ZSDT0112 - Exibir o depósito e ajustar a regra de escolha de lotes - Ini
  IF lr_lgort[] IS NOT INITIAL.
    DELETE t_lotes WHERE lgort IN lr_lgort. " RJF
*    IF t_lotes IS INITIAL.
*      MESSAGE s024(sd) WITH 'Nenhum depósito parametrizado na transação ZSDT0335'(I01) DISPLAY LIKE 'W'.
*      EXIT.
*    ENDIF.
  ENDIF.
* CS2023000389 ZSDT0112 - Exibir o depósito e ajustar a regra de escolha de lotes - Fim

  DELETE t_lotes WHERE clabs IS INITIAL
                   AND lfimg IS INITIAL.

  IF l_edit = abap_false.
    DELETE t_lotes WHERE lfimg IS INITIAL.
    EXIT.
  ENDIF.

*------------------------
* regras parametrizacao material
*------------------------
  IF l_bloq_lotes_vencidos = abap_true.
    DELETE t_lotes WHERE vfdat < sy-datum.
  ENDIF.

  IF l_regra_vencto = abap_true AND l_qtd_dias_vencimento IS NOT INITIAL.
    l_data_vencto = sy-datum + l_qtd_dias_vencimento.
    DELETE t_lotes WHERE vfdat <= l_data_vencto.
  ENDIF.

*------------------------
* analisa vencimentos proximos
*------------------------
  PERFORM f_analisa_vencimento.

*------------------------
* sugere quant.vincular
*------------------------
  FREE: l_total, l_total_lib.

  LOOP AT t_lotes INTO w_lotes.
    l_total = l_total + w_lotes-lfimg.
  ENDLOOP.

  l_total_lib = p_ordem_carga-qte_lib - l_total.

  FREE l_total.

  SORT t_lotes BY vfdat charg.

*  SORT t_lotes BY vfdat clabs.

*  LOOP AT t_lotes INTO w_lotes.
*    l_tabix = sy-tabix.
*
*    CHECK w_lotes-lfimg    IS INITIAL.
*    CHECK w_lotes-clabs     > 0.
*    CHECK w_lotes-bloq_data = abap_off.
*
*    l_total = l_total + w_lotes-clabs.
*
*    IF l_total > l_total_lib.
*      w_lotes-lfimg = w_lotes-clabs - ( l_total - l_total_lib ).
*      w_lotes-clabs = w_lotes-clabs - w_lotes-lfimg.
*      w_lotes-edit  = abap_true.
*      MODIFY t_lotes FROM w_lotes INDEX l_tabix.
*      EXIT.
*    ENDIF.
*
*    w_lotes-lfimg     = w_lotes-clabs.
*    w_lotes-clabs     = w_lotes-clabs - w_lotes-lfimg.
*    w_lotes-edit      = abap_true.
*    MODIFY t_lotes FROM w_lotes INDEX l_tabix.
*  ENDLOOP.

ENDFORM.

*******************************************************************************************
* analisar vencimento
*******************************************************************************************
FORM f_analisa_vencimento.
  DATA: v_data  TYPE datum,
        v_lgort TYPE mseg-lgort,
        v_lifnr TYPE mseg-lifnr.
  CHECK l_lote_vencmto_proximo = abap_true.

  SORT t_lotes[] BY vfdat. "RJF

  LOOP AT t_lotes INTO w_lotes.
    w_lotes-gr_lot = abap_false.
    MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING gr_lot.
  ENDLOOP.

*  t_lotes_ori[] = t_lotes[].
*  t_lotes_aux[] = t_lotes[].
  t_lotes_gr[]  = t_lotes[].

  LOOP AT t_lotes_gr INTO w_lotes_gr WHERE lfimg IS NOT INITIAL. " Group lote
    IF w_lotes_gr-lfimg IS NOT INITIAL.
      v_lifnr = w_lotes_gr-lifnr.

      IF v_lifnr IS NOT INITIAL.
        LOOP AT t_lotes INTO w_lotes WHERE lifnr EQ v_lifnr.
          w_lotes-gr_lot = abap_true.
          MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING gr_lot.
        ENDLOOP.
        EXIT.
      ENDIF.
*    IF w_lotes_gr-lgort IS NOT INITIAL.
      v_lgort = w_lotes_gr-lgort.

      IF v_lgort IS NOT INITIAL.
        LOOP AT t_lotes INTO w_lotes WHERE lgort EQ v_lgort.
          w_lotes-gr_lot = abap_true.
          MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING gr_lot.
        ENDLOOP.
        EXIT.
      ENDIF.
    ELSE.
*      w_lotes-gr_lot = abap_false.

      v_lifnr = w_lotes-lifnr.

      IF v_lifnr IS NOT INITIAL.
        LOOP AT t_lotes INTO w_lotes WHERE lifnr EQ v_lifnr.
          w_lotes-gr_lot = abap_false.
          MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING gr_lot.
        ENDLOOP.
        EXIT.
      ENDIF.
*    IF w_lotes_gr-lgort IS NOT INITIAL.
      v_lgort = w_lotes-lgort.

      IF v_lgort IS NOT INITIAL.
        LOOP AT t_lotes INTO w_lotes WHERE lgort EQ v_lgort.
          w_lotes-gr_lot = abap_false.
          MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING gr_lot.
        ENDLOOP.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR gv_valid.
  LOOP AT t_lotes INTO w_lotes WHERE gr_lot EQ abap_true.
    gv_valid = gv_valid + w_lotes-lfimg.
    CLEAR: w_lotes-color,
           w_lotes-bloq_data.
    MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING color bloq_data.
  ENDLOOP.

  IF gv_valid IS INITIAL AND sy-ucomm EQ 'SALVAR'. "Controle screen
    EXIT.
  ENDIF.

  t_lotes_ori[] = t_lotes[].
  t_lotes_aux[] = t_lotes[].

*-----------------------------------
*-verifica vencimento dos lotes sem armazem
*-----------------------------------
  DELETE t_lotes_aux WHERE clabs <= 0.
*  DELETE t_lotes_aux WHERE lifnr IS NOT INITIAL.

  SORT t_lotes_aux BY vfdat.

  FREE:  t_lotes_saldo_date.
  t_lotes_saldo_date = t_lotes_aux.
  SORT t_lotes_saldo_date BY vfdat.


  CLEAR: w_lotes, l_data_prox.
  LOOP AT t_lotes_aux  INTO w_lotes WHERE gr_lot EQ abap_true.
    IF w_lotes-lfimg IS INITIAL.
      l_data_prox = w_lotes-vfdat.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_data_prox IS INITIAL.
    l_data_prox = '99991231'.
  ENDIF.
*CS2021000218 -r ajustes na mont. carga - BG #109923 - inicio
*  LOOP AT t_lotes  INTO w_lotes WHERE vfdat  > l_data_prox
*                                  AND lifnr IS INITIAL.
  gv_qte_libc = gv_qte_lib. "" IR147451/ CS1129139 - RJF - 121849
  FREE: l_erro, gv_msg, gv_msg1, gv_charg. "" IR147451/ CS1129139 - RJF - 121849

* Valida diferentes gr_lot
  LOOP AT t_lotes  INTO w_lotes WHERE lfimg IS NOT INITIAL AND gr_lot IS INITIAL.
    MESSAGE s024(sd) WITH 'Não pode selecionar lotes em grupos diferentes'(e06)  w_lotes-charg
                          ' (armazém ou deposito).'(e07)  DISPLAY LIKE 'E'.
    gv_msg   = TEXT-e06.
    gv_charg = w_lotes-charg.
    gv_msg1  = TEXT-e07.
    l_erro = abap_true.
    EXIT.
  ENDLOOP.

  IF l_erro IS INITIAL.
    LOOP AT t_lotes  INTO w_lotes WHERE gr_lot EQ abap_true. "lifnr IS INITIAL and gr_lot eq abap_true.
      IF gv_qte_libc IS INITIAL. "" IR147451/ CS1129139 - RJF - 121849
        IF ( w_lotes-vfdat > l_data_prox OR ( v_data IS NOT INITIAL AND v_data <> w_lotes-vfdat ) ).

          w_lotes-bloq_data = abap_true.
          w_lotes-color     = 'C601'.
          MODIFY t_lotes FROM w_lotes INDEX sy-tabix.
        ELSE.
          IF v_data IS INITIAL.
            v_data = w_lotes-vfdat.
          ENDIF.
        ENDIF.
        " IR147451/ CS1129139 - Ini - RJF - 121849
      ELSE.
        IF gv_qte_libc IS NOT INITIAL.
          v_data = w_lotes-vfdat.
          gv_qte_libc = gv_qte_libc - w_lotes-lfimg.
        ENDIF.
      ENDIF.

      IF gv_qte_libc IS NOT INITIAL.
        IF ( w_lotes-lfimg LT w_lotes-clabs_ori AND ( gv_qte_libc GT w_lotes-clabs_ori OR ( gv_qte_libc LT w_lotes-clabs_ori AND gv_qte_libc IS NOT INITIAL ) ) ).
          l_erro = abap_true.
*        MESSAGE s024(sd) WITH 'Deve ser vinculado a qtd total do Lote '(e05)  w_lotes-charg
          MESSAGE s024(sd) WITH 'Deve ser vinculado o saldo da carga ou a qtd total, do lote'(e05)  w_lotes-charg
                                ' pois ele está com vencimento mais próximo.'(e02)  DISPLAY LIKE 'E'.
          gv_msg   = TEXT-e05.
          gv_charg = w_lotes-charg.
          gv_msg1  = TEXT-e02.
          EXIT.
        ENDIF.
      ENDIF.
      " " IR147451/ CS1129139 - Fim - RJF - 121849
    ENDLOOP.

    " IR147451/ CS1129139 - Ini - RJF - 121849
    DATA: lv_valid, lv_not_found, lv_charg TYPE mseg-charg, lv_ntot TYPE zsdt0134-lfimg.
    FREE: lv_valid, lv_not_found.
*  IF gv_qte_libc IS INITIAL.
    LOOP AT t_lotes  INTO w_lotes WHERE gr_lot EQ abap_true. "lifnr IS NOT INITIAL and gr_lot eq abap_true.
      IF w_lotes-lfimg IS NOT INITIAL.
        lv_ntot = lv_ntot + w_lotes-lfimg.
        IF lv_valid IS NOT INITIAL AND lv_not_found IS NOT INITIAL.
          l_erro = abap_true.
*          MESSAGE s024(sd) WITH 'Deve ser vinculado o Saldo total do Lote '(e01)  lv_charg
          MESSAGE s024(sd) WITH 'Deve ser vinculado o saldo da carga ou a qtd total, do lote'(e05)  lv_charg
                                ' pois ele está com vencimento mais próximo.'(e02)  DISPLAY LIKE 'E'.
          gv_msg   = TEXT-e01.
          gv_charg = lv_charg.
          gv_msg1  = TEXT-e02.
          EXIT.
        ENDIF.
        lv_valid = abap_on.
      ELSEIF w_lotes-lfimg IS INITIAL AND sy-tabix EQ 1.
        l_erro = abap_true.
*        MESSAGE s024(sd) WITH 'Deve ser vinculado o Saldo total do Lote '(e01)  w_lotes-charg
        MESSAGE s024(sd) WITH 'Deve ser vinculado o saldo da carga ou a qtd total, do lote'(e05)  w_lotes-charg
                             ' pois ele está com vencimento mais próximo.'(e02)  DISPLAY LIKE 'E'.
        gv_msg   = TEXT-e01.
        gv_charg = w_lotes-charg.
        gv_msg1  = TEXT-e02.
        EXIT.
      ELSE.
        lv_not_found = abap_on.
        lv_charg = w_lotes-charg.
      ENDIF.
    ENDLOOP.
*    IF lv_valid IS INITIAL AND lv_not_found IS NOT INITIAL.
*      l_erro = abap_true.
*      MESSAGE s024(sd) WITH 'Nenhuma info vinculada o Saldo total do Lote '(e03) DISPLAY LIKE 'E'.
*      gv_msg = TEXT-e03.
*      EXIT.
*    ENDIF.

*    IF gv_qte_lib NE lv_ntot AND l_erro IS INITIAL.
    IF ( gv_qte_lib NE lv_ntot AND gv_qte_lib IS NOT INITIAL AND lv_ntot IS NOT INITIAL ) AND l_erro IS INITIAL.
      l_erro = abap_true.
      MESSAGE s024(sd) WITH 'Qtd. vinculada no(s) lote(s) divergente.'(e04) DISPLAY LIKE 'E'.
      gv_msg = TEXT-e04.
      EXIT.
    ENDIF.

    IF ( gv_qte_libc NE lv_ntot AND gv_qte_libc IS NOT INITIAL AND lv_ntot IS NOT INITIAL ) AND l_erro IS INITIAL.
      l_erro = abap_true.
      MESSAGE s024(sd) WITH 'Qtd. vinculada no(s) lote(s) divergente.'(e04) DISPLAY LIKE 'E'.
      gv_msg = TEXT-e04.
      EXIT.
    ENDIF.

  ENDIF.
  " IR147451/ CS1129139 - Fim - RJF - 121849

*CS2021000218 -r ajustes na mont. carga - BG #109923 - fim
*-----------------------------------
*-verifica vencimento dos lotes com armazem
*-----------------------------------
  t_lotes_aux[] = t_lotes_ori[].

  DELETE t_lotes_aux WHERE clabs <= 0.
*  DELETE t_lotes_aux WHERE lifnr IS INITIAL.

  SORT t_lotes_aux BY vfdat.

  CLEAR: w_lotes, l_data_prox.
  LOOP AT t_lotes_aux  INTO w_lotes WHERE gr_lot EQ abap_true.
    IF w_lotes-lfimg IS INITIAL.
      l_data_prox = w_lotes-vfdat.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_data_prox IS INITIAL.
    l_data_prox = '99991231'.
  ENDIF.

  LOOP AT t_lotes  INTO w_lotes WHERE vfdat      > l_data_prox
                                   AND gr_lot EQ abap_true.
*                                  AND lifnr IS NOT INITIAL.
    w_lotes-bloq_data = abap_true.
    w_lotes-color     = 'C601'.
    MODIFY t_lotes FROM w_lotes INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* SALVAR LOTE
*******************************************************************************************
FORM f_salvar_lote CHANGING p_erro.

  DATA: l_total     TYPE zsdt0134-lfimg.

* IR147451/ CS1129139 - Transação ZSDT0112 - erro ao tentar desvincular
  LOOP AT t_lotes INTO w_lotes.
    gv_valid = gv_valid + w_lotes-lfimg.
  ENDLOOP.
  IF gv_valid IS INITIAL AND sy-ucomm EQ 'SALVAR'.
    CLEAR: p_erro, gv_msg.
  ENDIF.
* IR147451/ CS1129139 - Transação ZSDT0112 - erro ao tentar desvincular

  IF p_erro IS INITIAL AND gv_msg IS INITIAL. "

    FREE: p_erro, l_total.

    PERFORM f_analisa_vencimento.

    LOOP AT t_lotes INTO w_lotes.
      IF w_lotes-clabs > 0 AND w_lotes-lfimg > w_lotes-clabs_ori.
        p_erro = abap_true.
        MESSAGE s024(sd) WITH 'Qtd. vinculada no lote ' w_lotes-charg ' maior que a Utilização Livre.' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF w_lotes-bloq_data = abap_true AND
         w_lotes-lfimg     IS NOT INITIAL.

        LOOP AT t_lotes INTO DATA(w_lote_data) WHERE bloq_data = abap_off
                                                 AND lfimg     = 0.
          IF w_lotes-lifnr IS     INITIAL AND w_lote_data-lifnr IS     INITIAL.
            EXIT.
          ENDIF.
          IF w_lotes-lifnr IS NOT INITIAL AND w_lote_data-lifnr IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.

        p_erro = abap_true.
*        MESSAGE s024(sd) WITH 'Deve ser vinculado o Saldo total do Lote '  w_lote_data-charg
        MESSAGE s024(sd) WITH 'Deve ser vinculado o saldo da carga ou a qtd total, do lote'(e05) w_lote_data-charg
                             ' pois ele está com vencimento mais próximo.'  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      l_total = l_total + w_lotes-lfimg.
    ENDLOOP.

    IF l_total > w_ordem_carga-qte_lib.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Qtd. vinculada maior que a Qtd. do' ' Material na Carga.' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CHECK p_erro = abap_false.

*--------------------------
* Gravar tabela - cancelamentos
*--------------------------
    LOOP AT t_lotes        INTO w_lotes WHERE edit  = abap_true
                                          AND lfimg = 0.
      CLEAR w_zsdt0134.

      "CS2025000249 - ajuste para filtrar pelo vbeln - GA - 193487" -->>
      READ TABLE t_zsdt0134 INTO w_zsdt0134 WITH KEY charg   = w_lotes-charg
                                                     vbeln   = w_lotes-vbeln     "CS2025000249 - ajuste para filtrar pelo vbeln - GABRIEL AVILA - 193487"
                                                     posnr   = w_lotes-posnr     "CS2025000249 - ajuste para filtrar pelo posnr - GABRIEL AVILA - 193487"
                                                     nro_cg  = w_lotes-nro_cg.   "CS2021000218 -r ajustes na mont. carga - BG #109923 - AJUSTAR

*      READ TABLE t_zsdt0134 INTO w_zsdt0134 WITH KEY charg = w_lotes-charg
*                                                     lgort = w_lotes-lgort
*                                                     lifnr = w_lotes-lifnr
*                                                     nro_cg = w_lotes-nro_cg. "CS2021000218 -r ajustes na mont. carga - BG #109923
*      IF sy-subrc <> 0.
*        READ TABLE t_zsdt0134 INTO w_zsdt0134 WITH KEY charg = w_lotes-charg.
*      ENDIF.

      "CS2025000249 - ajuste para filtrar pelo vbeln - GA - 193487" <-----

      CHECK sy-subrc = 0.

      w_zsdt0134-status       = abap_true.
      w_zsdt0134-user_canc    = sy-uname.
      w_zsdt0134-dt_canc      = sy-datum.
      w_zsdt0134-hr_can       = sy-uzeit.
      MODIFY zsdt0134      FROM w_zsdt0134.
    ENDLOOP.

*--------------------------
* Gravar tabela - ativos
*--------------------------
    LOOP AT t_lotes        INTO w_lotes WHERE edit  = abap_true
                                          AND lfimg > 0.

      CLEAR w_zsdt0134.

      "CS2025000249 - ajuste para filtrar pelo vbeln - GABRIEL AVILA - BG 193487" -->>
      READ TABLE t_zsdt0134 INTO w_zsdt0134 WITH KEY charg   = w_lotes-charg
                                                     vbeln   = w_lotes-vbeln
                                                     posnr   = w_lotes-posnr
                                                     lgort   = w_lotes-lgort
                                                     lifnr   = w_lotes-lifnr
                                                     nro_cg  = w_lotes-nro_cg.

*      READ TABLE t_zsdt0134 INTO w_zsdt0134 WITH KEY charg = w_lotes-charg
*                                               lgort = w_lotes-lgort
*                                               lifnr = w_lotes-lifnr.
*      IF sy-subrc <> 0.
*        READ TABLE t_zsdt0134 INTO w_zsdt0134 WITH KEY charg = w_lotes-charg.
*      ENDIF.
      "CS2025000249 - ajuste para filtrar pelo vbeln - GABRIEL AVILA - BG 193487" <-----

      w_zsdt0134-mandt         = sy-mandt.
      w_zsdt0134-nro_cg        = w_ordem_carga-nro_cgd.
      w_zsdt0134-vbeln         = w_ordem_carga-vbeln.
      w_zsdt0134-posnr         = w_ordem_carga-posnr.
      w_zsdt0134-nr_rot        = w_ordem_carga-nr_rot.
      w_zsdt0134-ch_referencia = abap_off.
      w_zsdt0134-charg         = w_lotes-charg.
      w_zsdt0134-lgort         = w_lotes-lgort.
      w_zsdt0134-lifnr         = w_lotes-lifnr.
      w_zsdt0134-lfimg         = w_lotes-lfimg.
      w_zsdt0134-status        = abap_off.
      w_zsdt0134-usnam         = sy-uname.
      w_zsdt0134-data_atual    = sy-datum.
      w_zsdt0134-hora_atual    = sy-uzeit.
      MODIFY zsdt0134       FROM w_zsdt0134.
    ENDLOOP.

    COMMIT WORK AND WAIT.

  ELSE.
    IF gv_msg IS INITIAL.
      gv_msg = 'Erro! Verificar quantidade/lote.'.
    ENDIF.
    MESSAGE s024(sd) WITH gv_msg
                          gv_charg
                          gv_msg1 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

*******************************************************************************************
* historico
*******************************************************************************************
FORM f_historico USING p_charg.

  FREE: t_hist.

  SELECT *
    INTO TABLE @DATA(t_0134)
    FROM zsdt0134
   WHERE charg  = @p_charg
     AND status = @abap_off.

  CHECK t_0134[] IS NOT INITIAL.

  SELECT *
    INTO TABLE @DATA(t_0001)
    FROM zsdt0001
     FOR ALL ENTRIES IN @t_0134
   WHERE ch_referencia = @t_0134-ch_referencia.

  IF t_0001[] IS NOT INITIAL.
    SELECT *
      INTO TABLE @DATA(t_jdoc)
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN @t_0001
     WHERE docnum = @t_0001-nro_nf_prod.
  ENDIF.

  LOOP AT t_0134 INTO DATA(w_0134).
    CLEAR: w_hist.

    READ TABLE t_0001 INTO DATA(w_0001) WITH KEY ch_referencia = w_0134-ch_referencia.
    READ TABLE t_jdoc INTO DATA(w_jdoc) WITH KEY docnum        = w_0001-nro_nf_prod.

    w_hist-charg        = w_0134-charg.

    IF w_0134-lifnr IS NOT INITIAL.
      w_hist-lifnr      = w_0134-lifnr.
    ELSE.
      w_hist-lifnr      = w_0134-lgort.
    ENDIF.

    w_hist-nro_cg       = w_0134-nro_cg.
    w_hist-nr_romaneio  = w_0001-nr_romaneio.
    w_hist-lfimg        = w_0134-lfimg.
    w_hist-doc_rem      = w_0001-doc_rem.
    w_hist-nfenum       = w_jdoc-nfenum.
    w_hist-data_atual   = w_0134-data_atual.

    SHIFT   : w_hist-doc_rem LEFT DELETING LEADING '0',
              w_hist-nfenum  LEFT DELETING LEADING '0'.
    CONDENSE: w_hist-doc_rem,
              w_hist-nfenum.

    APPEND w_hist      TO t_hist.

    CLEAR: w_0001, w_jdoc.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv.

  FREE: t_fieldcatalog,
        t_fieldcatalog2.

  w_layout-zebra      = abap_false.
* w_layout-edit       = abap_true. " Makes all Grid editable
  w_layout-no_totarr  = abap_true.
  w_layout-no_totexp  = abap_true.
  w_layout-no_totline = abap_false.
  w_layout-no_toolbar = abap_false.
* w_layout-sel_mode   = 'A'.
  w_layout-cwidth_opt = abap_false.
* w_layout-stylefname = 'CELLTAB'.
  w_layout-info_fname = 'COLOR'.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.

    PERFORM fill_it_fieldcatalog1 USING:
            01 'CHARG'        'T_LOTES'    ' '  ' '   ' '  ' '  ' '   ' '   ' '   ' '   'Lote'                 '12',
            02 'LGORT'        'MSEG'       ' '  ' '   ' '  ' '  ' '   ' '   ' '   ' '   'Depósito'             '05',
            03 'LIFNR'        'MSEG'       ' '  ' '   ' '  ' '  ' '   ' '   ' '   ' '   'Armazém'              '08',
            04 'WERKS'        'MSEG'       ' '  ' '   ' '  ' '  ' '   ' '   ' '   ' '   'Centro'               '06',
            05 'MATNR'        'MSEG'       ' '  ' '   ' '  ' '  ' '   ' '   ' '   ' '   'Material'             '10',
            06 'VFDAT'        'MCHL'       ' '  ' '   ' '  ' '  ' '   ' '   ' '   ' '   'Vencimento'           '10',
            07 'CLABS'        'MCHB'       ' '  ' '   ' '  ' '  ' '   ' '   'X'   ' '   'Utiliz.Livre'         '11',
            08 'LFIMG'        'ZSDT0134'   ' ' l_edit ' '  ' '  ' '   ' '   'X'   ' '   'Qtde.Vincular'        '12'.

    PERFORM toolbar_alv1.
    PERFORM sort_alv1.

    IF m_event_handler IS INITIAL.
      CREATE OBJECT m_event_handler.
*     SET HANDLER : m_event_handler->toolbar         FOR g_grid.
      SET HANDLER : m_event_handler->user_command    FOR g_grid.
    ENDIF.

    " SET_TABLE_FOR_FIRST_DISPLAY
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_sort              = lt_sort1
        it_fieldcatalog      = t_fieldcatalog
        it_outtab            = t_lotes.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler=>on_data_changed4   FOR g_grid.
    SET HANDLER : lcl_event_handler=>handle_on_f1       FOR g_grid.
    SET HANDLER : lcl_event_handler=>on_double_click    FOR g_grid.

    CALL METHOD g_grid->set_focus
      EXPORTING
        control = g_grid.

    l_row           = 1.
    l_col-fieldname = 'LFIMG'.

    CALL METHOD g_grid->set_current_cell_via_id
      EXPORTING
        is_row_id    = l_row
        is_column_id = l_col.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2 EXPORTING container_name = g_container2.
    CREATE OBJECT g_grid2 EXPORTING i_parent = g_custom_container2.

    PERFORM fill_it_fieldcatalog2 USING:
            01 'DATA_ATUAL'   'ZSDT0134'   ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Dt.Vinc.Carga'        '10',
            02 'CHARG'        'T_HIST'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Lote'                 '10',
            03 'LIFNR'        'MSEG'       ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Depósito/Armazém'     '05',
            04 'LFIMG'        'ZSDT0134'   ' '  ' '  ' '  ' '  ' '   ' '   'X'   ' '   'Quantidade'           '08',
            05 'NRO_CG'       'ZSDT0134'   ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Carga'                '05',
            06 'NFENUM'       'J_1BNFDOC'  ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Nf-e'                 '05',
            07 'NR_ROMANEIO'  'ZSDT0001'   ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Romaneio'             '05',
            08 'DOC_REM'      'ZSDT0001'   ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Remessa'              '08'.

    PERFORM toolbar_alv2.
    PERFORM sort_alv2.

    IF m_event_handler2 IS INITIAL.
      CREATE OBJECT m_event_handler2.
*     SET HANDLER : m_event_handler2->toolbar FOR g_grid2.
      SET HANDLER : m_event_handler2->user_command FOR g_grid2.
    ENDIF.

    CALL METHOD g_grid2->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout
        it_toolbar_excluding = pt_exclude2
        i_save               = 'U' "abap_true
      CHANGING
        it_sort              = lt_sort2
        it_fieldcatalog      = t_fieldcatalog2
        it_outtab            = t_hist.

    CALL METHOD g_grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler2=>on_data_changed4 FOR g_grid2.
    SET HANDLER : lcl_event_handler2=>handle_on_f1     FOR g_grid.

  ELSE.
    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

*  CALL METHOD g_grid2->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM fill_it_fieldcatalog1 USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_checkbox)
                                 VALUE(p_dosum)
                                 VALUE(p_f4)
                                 VALUE(p_header)
                                 VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  CLEAR: wa_fieldcatalog.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-reptext    = p_header.
  wa_fieldcatalog-scrtext_l  = p_header.
  wa_fieldcatalog-scrtext_s  = p_header.
  wa_fieldcatalog-scrtext_m  = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  IF p_fieldname = 'VFDAT'.
    wa_fieldcatalog-datatype = 'DATS'.
  ENDIF.

  APPEND wa_fieldcatalog TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM fill_it_fieldcatalog2 USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_checkbox)
                                 VALUE(p_dosum)
                                 VALUE(p_f4)
                                 VALUE(p_header)
                                 VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  CLEAR: wa_fieldcatalog.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-reptext    = p_header.
  wa_fieldcatalog-scrtext_l  = p_header.
  wa_fieldcatalog-scrtext_s  = p_header.
  wa_fieldcatalog-scrtext_m  = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.

  APPEND wa_fieldcatalog TO t_fieldcatalog2.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM sort_alv1.
  FREE: lt_sort1, ls_sort.

  ls_sort-spos      = '1'.
  ls_sort-fieldname = 'VFDAT'.
  ls_sort-up        = abap_true.
  ls_sort-subtot    = abap_false.
  APPEND ls_sort TO lt_sort1.
*
  ls_sort-spos      = '2'.
  ls_sort-fieldname = 'CHARG'.
  ls_sort-up        = abap_true.
  ls_sort-subtot    = abap_true.
  APPEND ls_sort TO lt_sort1.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM sort_alv2.
  FREE: lt_sort2, ls_sort.

  ls_sort-spos      = '1'.
  ls_sort-fieldname = 'LIFNR'.
  ls_sort-up        = abap_true.
  ls_sort-subtot    = abap_true.
  APPEND ls_sort TO lt_sort2.

  ls_sort-spos      = '2'.
  ls_sort-fieldname = 'DATA_ATUAL'.
  ls_sort-up        = abap_true.
  ls_sort-subtot    = abap_false.
  APPEND ls_sort TO lt_sort2.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv1.

  FREE: pt_exclude.
* APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv2.

  FREE: pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude2.

ENDFORM.

*******************************************************************************************
*******************************************************************************************
