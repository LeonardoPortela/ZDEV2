FUNCTION z_les_frete_pagos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_OPCAO) TYPE  CHAR10
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_EMPRESA STRUCTURE  T001 OPTIONAL
*"      IT_DATA STRUCTURE  BSAD OPTIONAL
*"----------------------------------------------------------------------
*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Função para o report de frete pagos                                       |*
*| programa: ZLESR0026..                                                     |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Marcos Santos ( marcos.santos@grupomaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

  DATA: v_lines             TYPE i,
        v_dmbtr             TYPE zlest0105-dmbtr,
        v_dmbe2             TYPE zlest0105-dmbe2,
        v_ev_prt_ini        TYPE bsak-augdt,
        v_aquav_ini         TYPE bsak-augdt,
        v_fprop_ini         TYPE bsak-augdt,
        "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
        lva_proc_retroativo TYPE c,
        lva_dt_aux          TYPE sy-datum,
        lva_dias_busca      TYPE i,
        "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
        v_vl_pago_lote      TYPE zlest0105-vl_pago_lote.

  DATA: vl_tx_cambio TYPE ukurs_curr,
        vl_gdatu     TYPE gdatu_inv.

  DATA: it_branch_aux TYPE TABLE OF j_1bbranch.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  CREATE OBJECT obj_zcl_util_sd.

  CLEAR: it_bukrs[], it_augdt[], it_gjahr[].

  DESCRIBE TABLE it_empresa LINES var_linhas.

  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_empresa.
      READ TABLE it_empresa INTO gw_empresa INDEX 1.
      it_bukrs-sign   = 'I'.
      it_bukrs-option = 'EQ'.
      it_bukrs-low    = gw_empresa-bukrs.
      APPEND it_bukrs.
    WHEN: '2'.
      it_bukrs-sign   = 'I'.
      it_bukrs-option = 'EQ'.
      READ TABLE it_empresa INTO gw_empresa INDEX 1.
      it_bukrs-low    = gw_empresa-bukrs.
      CLEAR: gw_empresa.
      READ TABLE it_empresa INTO gw_empresa INDEX 2.
      it_bukrs-high   = gw_empresa-bukrs.
      APPEND it_bukrs.
  ENDCASE.

  CLEAR: var_linhas.

  DESCRIBE TABLE it_data LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.


      CLEAR: gw_data.
      READ TABLE it_data INTO gw_data INDEX 1.
      it_augdt-sign   = 'I'.
      it_augdt-option = 'EQ'.
      it_augdt-low    = gw_data-augdt.
      APPEND it_augdt.

      CLEAR: gw_gjahr.

      it_gjahr-sign   = 'I'.
      it_gjahr-option = 'EQ'.
      it_gjahr-low    = gw_data-augdt(4).

      APPEND it_gjahr.

      CLEAR: gw_gjahr.

      it_gjahr-sign   = 'I'.
      it_gjahr-option = 'EQ'.
      it_gjahr-low    = gw_data-augdt(4) - 1.

      APPEND it_gjahr.




    WHEN: '2'.

      it_augdt-sign   = 'I'.
      it_augdt-option = 'BT'.
      CLEAR: gw_data.
      READ TABLE it_data INTO gw_data INDEX 1.
      it_augdt-low    = gw_data-augdt.

      CLEAR: gw_gjahr.

      it_gjahr-sign   = 'I'.
      it_gjahr-option = 'EQ'.
      it_gjahr-low    = gw_data-augdt(4).

      APPEND it_gjahr.

      CLEAR: gw_data.
      READ TABLE it_data INTO gw_data INDEX 2.
      it_augdt-high   = gw_data-augdt.
      APPEND it_augdt.

      CLEAR: gw_gjahr.

      it_gjahr-sign   = 'I'.
      it_gjahr-option = 'EQ'.
      it_gjahr-low    = gw_data-augdt(4).

      APPEND it_gjahr.

      CLEAR: gw_gjahr.

      it_gjahr-sign   = 'I'.
      it_gjahr-option = 'EQ'.
      it_gjahr-low    = gw_data-augdt(4) - 1.

      APPEND it_gjahr.




  ENDCASE.

  "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
  CLEAR: lva_proc_retroativo.
  lva_dias_busca = 32.
  lva_dt_aux = sy-datum - lva_dias_busca.

  LOOP AT it_data INTO DATA(gw_data_aux) WHERE augdt < lva_dt_aux.
    lva_proc_retroativo = abap_true.
    EXIT.
  ENDLOOP.
  "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP


  REFRESH: gt_saida, gt_saida_aux, gt_zlest0105.
  CLEAR: gw_saida, gw_saida_aux, gw_zlest0105.

*-CS2022000256 - 24.03.2022 - JT - inicio
*-------------------------------------
*-parametrizacao centro custo x centro lucro x material
*-------------------------------------
  SELECT *
    FROM zlest0217
    INTO TABLE t_zlest0217.

  SORT t_zlest0217 BY kostl prctr matnr_serv.

  FREE: r_0217_prctr, r_0217_kostl.

  LOOP AT t_zlest0217 INTO w_zlest0217.
    IF w_zlest0217-prctr IS NOT INITIAL.
      r_0217_prctr-sign   = 'I'.
      r_0217_prctr-option = 'EQ'.
      r_0217_prctr-low    = w_zlest0217-prctr.
      APPEND r_0217_prctr.
    ENDIF.
    IF w_zlest0217-kostl IS NOT INITIAL.
      r_0217_kostl-sign   = 'I'.
      r_0217_kostl-option = 'EQ'.
      r_0217_kostl-low    = w_zlest0217-kostl.
      APPEND r_0217_kostl.
    ENDIF.
  ENDLOOP.

  IF r_0217_prctr[] IS INITIAL.
    r_0217_prctr-sign   = 'I'.
    r_0217_prctr-option = 'EQ'.
    r_0217_prctr-low    = '9999999999'.
    APPEND r_0217_prctr.
  ENDIF.

  IF r_0217_kostl[] IS INITIAL.
    r_0217_kostl-sign   = 'I'.
    r_0217_kostl-option = 'EQ'.
    r_0217_kostl-low    = '9999999999'.
    APPEND r_0217_kostl.
  ENDIF.

*-------------------------------------
*-parametrizacao cnpj
*-------------------------------------
  SELECT *
    FROM zlest0218
    INTO TABLE t_zlest0218.

  SORT t_zlest0218 BY stcd1.
*-CS2022000256 - 24.03.2022 - JT - fim

  "CS2018000311 - 16.03.2018
  CLEAR: r_auggj[], r_bsart[], r_blart[], r_blart1[], r_waers[], r_lifnr[], r_lifnr1[], r_lifnr2[].

  CLEAR: tg_setleaf_eleva[].

  IF it_augdt[] IS NOT INITIAL.
    r_auggj-sign   = 'I'.
    r_auggj-option = 'EQ'.
    r_auggj-low    = it_augdt-low(4).
    r_auggj-high   = it_augdt-low(4).
    APPEND r_auggj.

    IF ( it_augdt-high(4) IS NOT INITIAL ) AND ( it_augdt-high(4) NE it_augdt-low(4) ).
      r_auggj-sign   = 'I'.
      r_auggj-option = 'EQ'.
      r_auggj-low    = it_augdt-high(4).
      r_auggj-high   = it_augdt-high(4).
      APPEND r_auggj.
    ENDIF.
  ENDIF.

  CASE p_opcao.
    WHEN 'R_AD_PI'.
      "Tipo Pedido
      r_bsart-sign   = 'I'.
      r_bsart-option = 'EQ'.
      r_bsart-low    = 'ZFTE'.
      APPEND r_bsart.

      r_bsart-low    = 'ZSEM'.
      APPEND r_bsart.

      r_bsart-low    = 'ZDEF'.
      APPEND r_bsart.

      "Tipo Documento
      r_blart-sign   = 'I'.
      r_blart-option = 'EQ'.
      r_blart-low    = 'IN'.
      APPEND r_blart.

      "Moeda
      r_waers-sign   = 'I'.
      r_waers-option = 'EQ'.
      r_waers-low    = 'BRL'.
      APPEND r_waers.

    WHEN 'R_AQ_PA'.

      "Tipo Documento
      r_blart-sign   = 'I'.
      r_blart-option = 'EQ'.
      r_blart-low    = 'ME'.
      APPEND r_blart.

      r_blart-low    = 'MF'.
      APPEND r_blart.

      "Moeda
      r_waers-sign   = 'I'.
      r_waers-option = 'EQ'.
      r_waers-low    = 'BRL'.
      APPEND r_waers.

      "Fornecedor
      r_lifnr-sign   = 'I'.
      r_lifnr-option = 'EQ'.
      r_lifnr-low    = '0000000161'.
      APPEND r_lifnr.

    WHEN 'R_EV_PRT'.

*-CS2022000256 - 24.03.2022 - JT - inicio
*      SELECT *
*        FROM setleaf APPENDING CORRESPONDING FIELDS OF TABLE tg_setleaf_eleva
*       WHERE setname = 'MAGGI_ZLES0079_ELEVA'.

*      LOOP AT tg_setleaf_eleva WHERE valfrom IS NOT INITIAL.
*
*        SELECT SINGLE *
*          FROM setlinet INTO @DATA(_wl_setlinet)
*         WHERE setname = @tg_setleaf_eleva-setname
*           AND lineid  = @tg_setleaf_eleva-lineid.
*
*        IF ( sy-subrc = 0 ).
*          tg_setleaf_eleva-descript = _wl_setlinet-descript.
*          MODIFY tg_setleaf_eleva.
*        ENDIF.
*
*        "Fornecedor
*        r_lifnr-sign   = 'I'.
*        r_lifnr-option = 'EQ'.
*        r_lifnr-low    = tg_setleaf_eleva-valfrom.
*        APPEND r_lifnr.
*
*        IF tg_setleaf_eleva-descript IS INITIAL.
*          r_lifnr1-sign   = 'I'.
*          r_lifnr1-option = 'EQ'.
*          r_lifnr1-low    = tg_setleaf_eleva-valfrom.
*          APPEND r_lifnr1.
*        ELSE.
*          r_lifnr2-sign   = 'I'.
*          r_lifnr2-option = 'EQ'.
*          r_lifnr2-low    = tg_setleaf_eleva-valfrom.
*          APPEND r_lifnr2.
*        ENDIF.
*
*      ENDLOOP.

*      IF r_lifnr[] IS INITIAL.
*        MESSAGE 'Nenhum fornecedor parametrizado para a opção de Elevação Portuária!' TYPE 'S'.
*        EXIT.
*      ENDIF.

      FREE: r_lifnr, r_lifnr1, r_lifnr2.
*-CS2022000256 - 24.03.2022 - JT - fim

    WHEN 'R_AQUAV'.

      "Tipo Documento
      r_blart-sign   = 'I'.
      r_blart-option = 'EQ'.
      r_blart-low    = 'MF'. APPEND r_blart.
      r_blart-low    = 'ME'. APPEND r_blart.
      r_blart-low    = 'FT'. APPEND r_blart.

      "Fornecedor
      CLEAR: it_branch_aux[].
      SELECT *
        FROM j_1bbranch INTO TABLE it_branch_aux
       WHERE bukrs  IN ( '0041' )
         AND branch NE '0001'.

      r_lifnr-sign   = 'I'.
      r_lifnr-option = 'EQ'.
      LOOP AT it_branch_aux INTO DATA(wl_branch_aux).
        r_lifnr-low  = wl_branch_aux-branch .
        r_lifnr-low  = |{ r_lifnr-low ALPHA = IN } |.
        APPEND r_lifnr.
      ENDLOOP.

      "Moeda
      r_waers-sign   = 'I'.
      r_waers-option = 'EQ'.
      r_waers-low    = 'BRL'.
      APPEND r_waers.

    WHEN 'R_FPROP'.

    WHEN OTHERS.
  ENDCASE.
  "Fim CS2018000311 - 16.03.2018


  IF lva_proc_retroativo EQ abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    CASE p_opcao.
      WHEN: 'R_AD_CX'.
        "Perform Utilizado para selecionar informações do Adiantamento Caixa.
        "Não foi utilizado a seleção do perform SELECIONAR_DADOS porque este
        "fluxo é diferente, partindo primeiro na seleção de um SET e tornando
        "a tabela principal ZLEST0045.
        PERFORM: selecionar_dados_adt_caixa USING p_opcao.
      WHEN: 'R_AD_LQ'.
        PERFORM: selecionar_dados_adt_lq.
      WHEN: 'R_AD_PI'.
        "PERFORM: SELECIONAR_DADOS_ADT_INSUMOS USING P_OPCAO.
        PERFORM: selecionar_dados_adt_n1 USING p_opcao,
                 selecionar_dados_juros  USING p_opcao. "Seleção Juros
      WHEN: 'R_EV_PRT'.
        PERFORM: selecionar_dados_adt_n1 USING p_opcao,
                 selecionar_dados_juros  USING p_opcao. "Seleção Juros
*-CS2022000256 - 24.03.2022 - JT - inicio
*              filtrar_portuario       USING p_opcao. "Seleção Juros
*-CS2022000256 - 24.03.2022 - JT - fim
      WHEN: 'R_COM'.
        PERFORM: selecionar_dados_compensados.
      WHEN: 'R_AQ_PA'.
        PERFORM: selecionar_dados_adt_n1 USING p_opcao,
                 selecionar_dados_juros  USING p_opcao. "Seleção Juros
      WHEN 'R_AQUAV'.
        PERFORM: selecionar_dados        USING p_opcao abap_true,
                 selecionar_dados_adt_n1 USING p_opcao,
                 selecionar_dados_juros  USING p_opcao. "Seleção Juros

        DELETE gt_saida WHERE lifnr = '0000001003'.
        DELETE gt_saida WHERE lifnr = '0000001013'.
        DELETE gt_saida WHERE lifnr = '0000009402'.

      WHEN 'R_FPROP'.
        PERFORM: selecionar_dados_frete_proprio USING p_opcao.
      WHEN OTHERS.
        PERFORM selecionar_dados USING p_opcao abap_false.

    ENDCASE.



*-------------------------------------------------------------*
* Determinação Grupo de Contas
*-------------------------------------------------------------*
    CLEAR: gt_lfa1_ktokk[].

    IF gt_saida[] IS NOT INITIAL.
      SELECT *
        FROM lfa1 APPENDING TABLE gt_lfa1_ktokk
         FOR ALL ENTRIES IN gt_saida
       WHERE lifnr EQ gt_saida-lifnr.
    ENDIF.

*-CS2022000256 - 24.03.2022 - JT - inicio
    SELECT mara~matnr, mara~matkl, makt~maktx
      FROM mara
     INNER JOIN makt  ON makt~spras = @sy-langu
                     AND makt~matnr = mara~matnr
      INTO TABLE @DATA(t_mara)
       FOR ALL ENTRIES IN @gt_saida
     WHERE mara~matnr EQ @gt_saida-matnr.

    SORT t_mara BY matnr.
*-CS2022000256 - 24.03.2022 - JT - fim

    LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_aux>).
      READ TABLE gt_lfa1_ktokk WITH KEY lifnr = <fs_saida_aux>-lifnr.
      CHECK sy-subrc EQ 0.
      <fs_saida_aux>-ktokk = gt_lfa1_ktokk-ktokk.
    ENDLOOP.

  ENDIF. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  "------------------------------------------------------------------
  " Transferência p/ tabela ZLEST0105
  "------------------------------------------------------------------
  CLEAR: var_tipo_reg.

  CASE p_opcao.
    WHEN: 'R_AD_CX'.
      var_tipo_reg = 'FA'.
    WHEN: 'PAG'.
      var_tipo_reg = 'FP'.
    WHEN 'R_AD_PI'.
      var_tipo_reg = 'IA_IP'. "Tipo Definido no Registro
    WHEN 'R_EV_PRT'.
      var_tipo_reg = 'EA_EP'. "Tipo Definido no Registro
    WHEN 'R_AQUAV'.
      var_tipo_reg = 'AQ'.
    WHEN 'R_AQ_PA'.
      var_tipo_reg = 'PA'.
    WHEN 'R_FPROP'.
      var_tipo_reg = 'PR'.
  ENDCASE.

  "Lançamentos Manuais
  CLEAR: gt_zlest0105_mn[].
  IF var_tipo_reg = 'IA_IP'.
    SELECT *
      FROM zlest0105 INTO TABLE gt_zlest0105_mn
     WHERE bukrs      IN it_bukrs
       AND augdt      IN it_augdt
       AND tipo       IN ('IA','IP')
       AND inc_manual EQ 'X'.
  ELSEIF var_tipo_reg = 'EA_EP'.
    SELECT *
      FROM zlest0105 INTO TABLE gt_zlest0105_mn
     WHERE bukrs      IN it_bukrs
       AND augdt      IN it_augdt
       AND tipo       IN ('EA','EP')
       AND inc_manual EQ 'X'.
  ELSE.
    SELECT *
      FROM zlest0105 INTO TABLE gt_zlest0105_mn
     WHERE bukrs   IN it_bukrs
       AND augdt   IN it_augdt
       AND tipo    EQ var_tipo_reg
       AND inc_manual EQ 'X'.
  ENDIF.

  IF lva_proc_retroativo EQ abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    IF p_opcao = 'R_AD_PI'.. "Insumos
      DELETE gt_saida    WHERE augdt < '20170516'.
    ENDIF.

    IF p_opcao = 'R_EV_PRT'. "Elevação Portuaria
      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_ev_prt_ini)
       WHERE setname = 'MAGGI_EV_PRT_INI'.

      IF ( sy-subrc EQ 0 ) AND ( _wl_ev_prt_ini-valfrom IS NOT INITIAL ).
        v_ev_prt_ini = _wl_ev_prt_ini-valfrom.
        DELETE gt_saida    WHERE augdt < v_ev_prt_ini.
      ENDIF.
    ENDIF.

    IF ( p_opcao EQ 'R_AQUAV' ) OR
       ( p_opcao EQ 'R_AQ_PA' ).
      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_zles0079_aquav_ini)
       WHERE setname = 'ZLES0079_AQUAV_INI'.

      IF ( sy-subrc EQ 0 ) AND ( _wl_zles0079_aquav_ini-valfrom IS NOT INITIAL ).
        v_aquav_ini  = _wl_zles0079_aquav_ini-valfrom.
        DELETE gt_saida WHERE augdt < v_aquav_ini.
      ENDIF.
    ENDIF.

    IF ( p_opcao EQ 'R_FPROP' ).
      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_zles0079_fprop_ini)
       WHERE setname = 'ZLES0079_FPROP_INI'.

      IF ( sy-subrc EQ 0 ) AND ( _wl_zles0079_fprop_ini-valfrom IS NOT INITIAL ).
        v_fprop_ini  = _wl_zles0079_fprop_ini-valfrom.
        DELETE gt_saida WHERE augdt < v_fprop_ini.
      ELSE.
        CLEAR: gt_saida[].
      ENDIF.
    ENDIF.

*------------------------------------------------*
* Tratamento Lançamentos Empresa 0032
*------------------------------------------------*
    RANGES: r_matkl_graos FOR mara-matkl.

    CLEAR: r_matkl_graos[].

    APPEND VALUE #( sign = 'I' option = 'EQ' low = '700110' ) TO r_matkl_graos.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '700170' ) TO r_matkl_graos.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ''       ) TO r_matkl_graos.

    DELETE gt_saida WHERE bukrs = '0032' AND ( ktokk EQ 'ZFIC' OR matkl NOT IN r_matkl_graos ).

    "CHECK NOT GT_SAIDA[] IS INITIAL OR  GT_ZLEST0105_MN[] IS NOT INITIAL.

    CLEAR: gt_bkpf_grv[], gt_vttk_grv[], gt_tvtkt_grv[], gt_j_1bnfdoc_grv[].
    IF gt_saida[] IS NOT INITIAL.
      SELECT *
        FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE gt_bkpf_grv
         FOR ALL ENTRIES IN gt_saida
       WHERE bukrs EQ gt_saida-bukrs
         AND belnr EQ gt_saida-belnr.

      SELECT *
        FROM vttk APPENDING CORRESPONDING FIELDS OF TABLE gt_vttk_grv
         FOR ALL ENTRIES IN gt_saida
       WHERE tknum EQ gt_saida-tknum.

      SELECT *
        FROM j_1bnfdoc INTO TABLE gt_j_1bnfdoc_grv
         FOR ALL ENTRIES IN gt_saida
       WHERE docnum EQ gt_saida-docnum.
    ENDIF.

  ENDIF. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  IF gt_zlest0105_mn[] IS NOT INITIAL.
    SELECT *
      FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE gt_bkpf_grv
       FOR ALL ENTRIES IN gt_zlest0105_mn
     WHERE bukrs EQ gt_zlest0105_mn-bukrs
       AND belnr EQ gt_zlest0105_mn-belnr.

    SELECT *
      FROM vttk APPENDING CORRESPONDING FIELDS OF TABLE gt_vttk_grv
       FOR ALL ENTRIES IN gt_zlest0105_mn
     WHERE tknum EQ gt_zlest0105_mn-tknum.
  ENDIF.

  IF gt_vttk_grv[] IS NOT INITIAL.
    SELECT *
      FROM tvtkt INTO TABLE gt_tvtkt_grv
       FOR ALL ENTRIES IN gt_vttk_grv
     WHERE spras = sy-langu
       AND shtyp = gt_vttk_grv-shtyp.
  ENDIF.

  IF lva_proc_retroativo EQ abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    LOOP AT gt_saida INTO gw_saida.

      DATA(_tabix) = sy-tabix.
      DATA(_del)   = abap_false.

      CASE var_tipo_reg.
        WHEN 'EA_EP'. "Elevação Portuária.

*-CS2022000256 - 24.03.2022 - JT - inicio
*        READ TABLE tg_setleaf_eleva WITH KEY valfrom = gw_saida-lifnr.
*        IF ( sy-subrc EQ 0 ).
*          CASE tg_setleaf_eleva-descript(1).
*            WHEN abap_true.
          IF gw_saida-stcd1 IS NOT INITIAL.
            READ TABLE t_zlest0218 INTO w_zlest0218 WITH KEY stcd1(8) = gw_saida-stcd1(8).
            IF sy-subrc EQ 0.
*  -CS2022000256 - 24.03.2022 - JT - fim
              READ TABLE gt_bkpf_grv WITH KEY bukrs = gw_saida-bukrs
                                              belnr = gw_saida-belnr.
              IF sy-subrc EQ 0.
                gw_saida-zfbdt = gt_bkpf_grv-bldat.
                gw_saida-augdt = gt_bkpf_grv-bldat.

                CLEAR: vl_tx_cambio.
                MOVE gw_saida-augdt TO vl_gdatu.

                obj_zcl_util_sd->set_kurst('B').
                obj_zcl_util_sd->set_waerk('USD').
                obj_zcl_util_sd->set_tcurr('BRL').
                obj_zcl_util_sd->set_data( vl_gdatu ).

                vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

                IF vl_tx_cambio > 0.
                  IF gt_bkpf_grv-waers EQ 'BRL'.
                    gw_saida-dmbe2   = gw_saida-dmbtr / vl_tx_cambio.
                  ELSE.
                    gw_saida-dmbtr   = gw_saida-dmbe2 * vl_tx_cambio.
                  ENDIF.
                  gw_saida-tx_camb = vl_tx_cambio.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
*-CS2022000256 - 24.03.2022 - JT - inicio
*          ENDCASE.
*        ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

          IF gw_saida-augdt NOT IN it_augdt.
            _del = abap_true.
          ENDIF.

          IF v_ev_prt_ini IS NOT INITIAL.
            IF gw_saida-augdt < v_ev_prt_ini.
              _del = abap_true.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

      IF _del EQ abap_true.
        DELETE gt_saida INDEX _tabix.
      ELSE.
        MODIFY gt_saida FROM gw_saida INDEX _tabix.
      ENDIF.

    ENDLOOP.

  ENDIF. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  IF ( var_tipo_reg IS NOT INITIAL ).

    DATA(_atualizar_lctos_zlest0105) = abap_true. "Ajuste para configurar remontagem dados tabela zles0105

    IF lva_proc_retroativo EQ abap_true. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
      _atualizar_lctos_zlest0105 = abap_false.
    ENDIF.

    "Deletar Lançamentos para refazer tabela.
    IF ( it_bukrs IS NOT INITIAL ) AND ( it_augdt IS NOT INITIAL ).

      DATA: vl_del_0105 TYPE c.

      CLEAR: gt_zlest0105_aux[].

      ""Ajuste para configurar remontagem dados tabela zles0105 - Ini
      DATA: v_dt_ini_refazer_lctos TYPE bsak-augdt.

      IF var_tipo_reg IS NOT INITIAL AND
         lva_proc_retroativo EQ abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP.

        DATA: lva_set_ref_lcto TYPE setleaf-setname.

        CONCATENATE 'ZLES0079_REF_LCO_' var_tipo_reg INTO lva_set_ref_lcto.

        SELECT SINGLE *
          FROM setleaf INTO @DATA(_wl_set_ref_lctos)
         WHERE setname EQ @lva_set_ref_lcto.

        IF ( sy-subrc EQ 0 ) AND ( _wl_set_ref_lctos-valfrom IS NOT INITIAL ).
          v_dt_ini_refazer_lctos = _wl_set_ref_lctos-valfrom.

          READ TABLE it_augdt INTO DATA(lwa_data_ini_filtro) INDEX 1.
          IF ( sy-subrc EQ 0 ) AND ( lwa_data_ini_filtro-low IS NOT INITIAL ) AND ( lwa_data_ini_filtro-low >= v_dt_ini_refazer_lctos ).
            _atualizar_lctos_zlest0105 = abap_true.
          ELSE.
            _atualizar_lctos_zlest0105 = abap_false.
          ENDIF.
        ELSEIF var_tipo_reg EQ 'EA_EP'.
          "or var_tipo_reg EQ      "Adicionar novos tipos aqui no futuro para configurar a remontagem de tabela com a entrada de novas regras de processamento
          _atualizar_lctos_zlest0105 = abap_false.
        ENDIF.

      ENDIF.
      "Ajuste para configurar remontagem dados tabela zles0105. - Fim

      IF var_tipo_reg = 'IA_IP'.
        SELECT *
          FROM zlest0105 INTO TABLE gt_zlest0105_aux
         WHERE bukrs   IN it_bukrs
           AND augdt   IN it_augdt
           AND tipo    IN ('IA','IP')
           AND inc_manual EQ ''.
      ELSEIF var_tipo_reg = 'EA_EP'.

        SELECT *
          FROM zlest0105 INTO TABLE gt_zlest0105_aux
         WHERE bukrs   IN it_bukrs
           AND augdt   IN it_augdt
           AND tipo    IN ('EA','EP')
           AND inc_manual EQ ''.

        IF ( gt_saida[] IS NOT INITIAL ) AND ( _atualizar_lctos_zlest0105 EQ abap_true ).  "Ajuste para configurar remontagem dados tabela zles0105
          SELECT *
            FROM zlest0105 APPENDING CORRESPONDING FIELDS OF TABLE gt_zlest0105_aux
             FOR ALL ENTRIES IN gt_saida
           WHERE bukrs EQ gt_saida-bukrs
             AND belnr EQ gt_saida-belnr.
        ENDIF.

      ELSE.
        SELECT *
          FROM zlest0105 INTO TABLE gt_zlest0105_aux
         WHERE bukrs   IN it_bukrs
           AND augdt   IN it_augdt
           AND tipo    EQ var_tipo_reg
           AND inc_manual EQ ''.
      ENDIF.


      IF _atualizar_lctos_zlest0105 EQ abap_false.  "Ajuste para configurar remontagem dados tabela zles0105.

        LOOP AT gt_zlest0105_aux INTO gw_zlest0105_aux.
          CLEAR: gw_zlest0105.
          MOVE-CORRESPONDING gw_zlest0105_aux TO gw_zlest0105.
          APPEND gw_zlest0105 TO gt_zlest0105.
        ENDLOOP.

      ELSE.

        SORT gt_zlest0105_aux BY bukrs belnr augbl docnum tknum chvid tipo estorno.

        LOOP AT gt_zlest0105_aux INTO gw_zlest0105_aux WHERE estorno = ''.

          CLEAR: vl_del_0105.

          READ TABLE gt_zlest0105_aux INTO gw_zlest0105 WITH KEY bukrs    = gw_zlest0105_aux-bukrs
                                                                 belnr    = gw_zlest0105_aux-belnr
                                                                 augbl    = gw_zlest0105_aux-augbl
                                                                 docnum   = gw_zlest0105_aux-docnum
                                                                 tknum    = gw_zlest0105_aux-tknum
                                                                 chvid    = gw_zlest0105_aux-chvid
                                                                 tipo     = gw_zlest0105_aux-tipo
                                                                 estorno  = 'X'
                                                                 BINARY SEARCH.
          CASE sy-subrc.
            WHEN 0.
              IF ( gw_zlest0105-estorno_manual IS INITIAL ).
                vl_del_0105 = 'X'.
              ENDIF.
            WHEN OTHERS.
              vl_del_0105 = 'X'.
          ENDCASE.

          IF vl_del_0105 IS NOT INITIAL.
            DELETE FROM zlest0105 WHERE bukrs          = gw_zlest0105_aux-bukrs
                                    AND belnr          = gw_zlest0105_aux-belnr
                                    AND augbl          = gw_zlest0105_aux-augbl
                                    AND docnum         = gw_zlest0105_aux-docnum
                                    AND tknum          = gw_zlest0105_aux-tknum
                                    AND chvid          = gw_zlest0105_aux-chvid
                                    AND tipo           = gw_zlest0105_aux-tipo
                                    AND estorno_manual = ''.
          ENDIF.

        ENDLOOP.

      ENDIF.

      "COMMIT WORK.
    ENDIF.
    "Fim - Refazer Lançamentos ZLEST015para refazer tabela.

    IF _atualizar_lctos_zlest0105 EQ abap_true.

      "Busca registros das Tables Standard que não existem na ZLEST0105
      LOOP AT gt_saida INTO gw_saida.

        "Se tiver modificação/inclusão manual, não prosseguir
        READ TABLE gt_zlest0105_mn WITH KEY bukrs      = gw_saida-bukrs
                                            belnr      = gw_saida-belnr
                                            augbl      = gw_saida-augbl
                                            docnum     = gw_saida-docnum
                                            tknum      = gw_saida-tknum
                                            xblnr      = gw_saida-xblnr
                                            chvid      = gw_saida-chvid
                                            tipo       = gw_saida-tipo.
        CHECK sy-subrc NE 0.

        IF gw_saida-xblnr EQ 'ESTORNO'.
          SELECT SINGLE *
            INTO gw_zlest0105
            FROM zlest0105
           WHERE belnr   = gw_saida-belnr
             AND augbl   = gw_saida-augbl
             AND bukrs   = gw_saida-bukrs
             AND docnum  = gw_saida-docnum
             AND tknum   = gw_saida-tknum
             AND xblnr   = gw_saida-xblnr
             AND chvid   = gw_saida-chvid
             AND tipo    = gw_saida-tipo
             AND inc_manual = ''
             AND estorno = 'X'.
        ELSE.
          SELECT SINGLE *
            INTO gw_zlest0105
            FROM zlest0105
            WHERE belnr   = gw_saida-belnr
              AND augbl   = gw_saida-augbl
              AND bukrs   = gw_saida-bukrs
              AND docnum  = gw_saida-docnum
              AND tknum   = gw_saida-tknum
              AND xblnr   = gw_saida-xblnr
              AND chvid   = gw_saida-chvid
              AND inc_manual = ''
              AND tipo    = gw_saida-tipo.
        ENDIF.

        IF sy-subrc <> 0.

          CLEAR: gw_zlest0105.

          MOVE-CORRESPONDING gw_saida TO gw_zlest0105.

*  -CS2022000256 - 24.03.2022 - JT - inicio
          READ TABLE t_mara INTO DATA(w_mara) WITH KEY matnr = gw_zlest0105-matnr
                                              BINARY SEARCH.
          IF sy-subrc = 0.
            gw_zlest0105-matkl = w_mara-matkl.
            gw_zlest0105-maktx = w_mara-maktx.
          ENDIF.
*  -CS2022000256 - 24.03.2022 - JT - fim

          gw_zlest0105-us_proc  = sy-uname.
          gw_zlest0105-dt_atual = sy-datum.
          gw_zlest0105-hr_atual = sy-uzeit.

          IF gw_saida-xblnr EQ 'ESTORNO'.
            gw_zlest0105-estorno = 'X'.
          ENDIF.

          APPEND gw_zlest0105 TO gt_zlest0105.

          INSERT zlest0105 FROM gw_zlest0105.
          "COMMIT WORK.

        ELSE.
          CLEAR: gw_zlest0105.

          MOVE-CORRESPONDING gw_saida TO gw_zlest0105.
          APPEND gw_zlest0105 TO gt_zlest0105.
        ENDIF.

        "Agrupar Partidas do documento.
        CLEAR: v_lines, v_dmbtr, v_dmbe2, v_vl_pago_lote.

        LOOP AT gt_zlest0105 INTO DATA(_wl_0105) WHERE bukrs      = gw_zlest0105-bukrs
                                                   AND belnr      = gw_zlest0105-belnr
                                                   AND augbl      = gw_zlest0105-augbl
                                                   AND docnum     = gw_zlest0105-docnum
                                                   AND tknum      = gw_zlest0105-tknum
                                                   AND xblnr      = gw_zlest0105-xblnr
                                                   AND chvid      = gw_zlest0105-chvid
                                                   AND tipo       = gw_zlest0105-tipo
                                                   AND estorno    = gw_zlest0105-estorno.

          ADD 1 TO v_lines.
          ADD _wl_0105-dmbtr        TO v_dmbtr.
          ADD _wl_0105-dmbe2        TO v_dmbe2.
          ADD _wl_0105-vl_pago_lote TO v_vl_pago_lote.
        ENDLOOP.

        IF v_lines > 1.
          gw_zlest0105-dmbtr        = v_dmbtr.
          gw_zlest0105-dmbe2        = v_dmbe2.
          gw_zlest0105-vl_pago_lote = v_vl_pago_lote.
          MODIFY zlest0105 FROM gw_zlest0105.
        ENDIF.
        "Fim Agrupamento

      ENDLOOP.

    ENDIF.

    "Transferência Lancamentos Manuais
    LOOP AT gt_zlest0105_mn.

      DELETE gt_zlest0105 WHERE bukrs      = gt_zlest0105_mn-bukrs
                            AND belnr      = gt_zlest0105_mn-belnr
                            AND augbl      = gt_zlest0105_mn-augbl
                            AND docnum     = gt_zlest0105_mn-docnum
                            AND tknum      = gt_zlest0105_mn-tknum
                            AND xblnr      = gt_zlest0105_mn-xblnr
                            AND chvid      = gt_zlest0105_mn-chvid
                            AND tipo       = gt_zlest0105_mn-tipo
                            AND estorno    = gt_zlest0105_mn-estorno.

      CLEAR: gw_zlest0105.
      MOVE-CORRESPONDING gt_zlest0105_mn TO gw_zlest0105.
      APPEND gw_zlest0105 TO gt_zlest0105.

      "Remover estorno de lançamentos manuais.
      IF lva_proc_retroativo EQ abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
        DELETE FROM zlest0105 WHERE bukrs    = gt_zlest0105_mn-bukrs
                                AND belnr    = gt_zlest0105_mn-belnr
                                AND augbl    = gt_zlest0105_mn-augbl
                                AND docnum   = gt_zlest0105_mn-docnum
                                AND tknum    = gt_zlest0105_mn-tknum
                                AND chvid    = gt_zlest0105_mn-chvid
                                AND tipo     = gt_zlest0105_mn-tipo
                                AND estorno  = 'X'.
      ENDIF.

    ENDLOOP.

    "Copia Dados tabela Z p/ Tabela Aux.
    REFRESH: gt_saida_aux.
    CLEAR: gw_saida_aux, gw_zlest0105.

    LOOP AT gt_zlest0105 INTO gw_zlest0105.

      MOVE-CORRESPONDING gw_zlest0105 TO gw_saida_aux.
      gw_saida_aux-exc_est = icon_execute_object.

*-CS2022000256 - 24.03.2022 - JT - inicio
      READ TABLE gt_saida INTO gw_saida WITH KEY bukrs  = gw_zlest0105-bukrs
                                                 belnr  = gw_zlest0105-belnr
                                                 augbl  = gw_zlest0105-augbl
                                                 docnum = gw_zlest0105-docnum
                                                 tknum  = gw_zlest0105-tknum
                                                 xblnr  = gw_zlest0105-xblnr
                                                 chvid  = gw_zlest0105-chvid
                                                 tipo   = gw_zlest0105-tipo.
      IF sy-subrc = 0.
        gw_saida_aux-kostl =  gw_saida-kostl.
        gw_saida_aux-prctr =  gw_saida-prctr.
        gw_saida_aux-stcd1 =  gw_saida-stcd1.
      ELSE.
        FREE: gw_saida_aux-kostl,
              gw_saida_aux-prctr,
              gw_saida_aux-stcd1.
      ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

      CLEAR: gw_lfa1.
      IF gw_saida_aux-lifnr IS NOT INITIAL.
        SELECT SINGLE *
          INTO gw_lfa1
          FROM lfa1
         WHERE lifnr = gw_saida_aux-lifnr.

        IF sy-subrc EQ 0.
          gw_saida_aux-name1 = gw_lfa1-name1.
        ENDIF.
      ENDIF.

      IF gw_saida_aux-tknum IS NOT INITIAL.
        READ TABLE gt_vttk_grv WITH KEY tknum = gw_saida_aux-tknum.
        IF sy-subrc EQ 0.
          READ TABLE gt_tvtkt_grv WITH KEY shtyp = gt_vttk_grv-shtyp.
          IF sy-subrc EQ 0.
            gw_saida_aux-bezei = gt_tvtkt_grv-shtyp && '-' && gt_tvtkt_grv-bezei.
          ENDIF.
        ENDIF.
      ENDIF.

      IF gw_saida_aux-docnum IS NOT INITIAL.
        READ TABLE gt_j_1bnfdoc_grv WITH KEY docnum = gw_saida_aux-docnum.
        IF sy-subrc EQ 0.
          gw_saida_aux-ctenum = gt_j_1bnfdoc_grv-nfenum.
        ENDIF.
      ENDIF.

      CASE gw_saida_aux-add03.
        WHEN 'Proprio'.
          gw_saida_aux-tp_frete = 'P'.
        WHEN 'Terceiro'.
          gw_saida_aux-tp_frete = 'T'.
      ENDCASE.

      APPEND gw_saida_aux TO gt_saida_aux.

      CLEAR: gw_saida_aux, gw_zlest0105.

    ENDLOOP.

  ENDIF.

  COMMIT WORK.

  "Busca lançamentos ZLEST0105 que foram estornardos nas tabelas standard, para atualização
*  REFRESH: GT_ZLEST0105.
*  SELECT *
*    FROM ZLEST0105 AS A INTO TABLE GT_ZLEST0105
*   WHERE A~BUKRS   IN IT_BUKRS
*     AND A~AUGDT   IN IT_AUGDT
*     AND A~TIPO    EQ VAR_TIPO_REG
*     AND A~ESTORNO EQ ''
*     AND NOT EXISTS ( SELECT *
*                       FROM ZLEST0105 AS B
*                      WHERE B~BUKRS   = A~BUKRS
*                        AND B~BELNR   = A~BELNR
*                        AND B~AUGBL   = A~AUGBL
*                        AND B~TIPO    = A~TIPO
*                        AND B~ESTORNO NE '' ).
*
*  LOOP AT GT_ZLEST0105 INTO GW_ZLEST0105.
*    CLEAR: VAR_DOC_ESTORNO.
*
*    IF ( GW_ZLEST0105-BELNR IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-BUKRS IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-BUDAT IS NOT INITIAL ).
*
*      CLEAR: GW_BKPF.
*      SELECT SINGLE *
*        FROM BKPF INTO CORRESPONDING FIELDS OF GW_BKPF
*       WHERE BELNR EQ GW_ZLEST0105-BELNR
*         AND BUKRS EQ GW_ZLEST0105-BUKRS
*         AND GJAHR EQ GW_ZLEST0105-BUDAT(4)
*         AND STBLG EQ ''.
*
*      IF SY-SUBRC NE 0.
*        VAR_DOC_ESTORNO = 'X'.
*      ENDIF.
*    ENDIF.
*
*    IF ( GW_ZLEST0105-AUGBL IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-BUKRS IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-AUGDT IS NOT INITIAL ) AND
*       ( VAR_DOC_ESTORNO IS INITIAL ).
*
*      CLEAR: GW_BKPF.
*      SELECT SINGLE *
*        FROM BKPF INTO CORRESPONDING FIELDS OF GW_BKPF
*       WHERE BELNR EQ GW_ZLEST0105-AUGBL
*         AND BUKRS EQ GW_ZLEST0105-BUKRS
*         AND GJAHR EQ GW_ZLEST0105-AUGDT(4)
*         AND STBLG EQ ''.
*
*      IF SY-SUBRC NE 0.
*        VAR_DOC_ESTORNO = 'X'.
*      ENDIF.
*    ENDIF.
*
*    IF VAR_DOC_ESTORNO IS NOT INITIAL.
*      GW_ZLEST0105-US_PROC  = SY-UNAME.
*      GW_ZLEST0105-DT_ATUAL = SY-DATUM.
*      GW_ZLEST0105-HR_ATUAL = SY-UZEIT.
*      GW_ZLEST0105-XBLNR    = 'ESTORNO'.
*      GW_ZLEST0105-DMBTR    = GW_ZLEST0105-DMBTR * -1.
*      GW_ZLEST0105-DMBE2    = GW_ZLEST0105-DMBE2 * -1.
*      GW_ZLEST0105-VL_PAGO_LOTE = GW_ZLEST0105-VL_PAGO_LOTE * -1.
*      GW_ZLEST0105-ESTORNO  = 'X'.
*      INSERT ZLEST0105 FROM GW_ZLEST0105.
*      COMMIT WORK.
*    ENDIF.
*  ENDLOOP.

  "------------------------------------------------------------------
  " Transferência Fim
  "------------------------------------------------------------------
  IF ( var_tipo_reg IS NOT INITIAL ).
    it_resultado[] = gt_saida_aux[].
  ELSE.
    it_resultado[] = gt_saida[].
  ENDIF.

ENDFUNCTION.
