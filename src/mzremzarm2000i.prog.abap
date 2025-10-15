
*----------------------------------------------------------------------*
***INCLUDE MZREMZARM2000I .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE ok_code.
    WHEN c_backr OR c_exitr OR c_cancelr.
      CLEAR: ok_code.
      CLEAR: it_messagem[].
      vg_dynnr_000 = c_1000.
    WHEN c_rosaida.
      CLEAR: ok_code.
      PERFORM registrar_movimento_mercadoria.
    WHEN c_rologsa.
      CLEAR: ok_code.
      PERFORM chama_log.
    WHEN c_rodocm.
      PERFORM chama_doc_material.
    WHEN c_roestorno.
      CLEAR: ok_code.
      PERFORM chama_estorno.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2000  INPUT

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_SAIDA
*&---------------------------------------------------------------------*
*       Registrar saída
*----------------------------------------------------------------------*
FORM registrar_movimento_mercadoria .

  DATA: vg_verficar TYPE sy-subrc,
        v_status(2),
        answer      TYPE c LENGTH 1.

  PERFORM verifica_selecao_romaneio USING vg_verficar.

  IF vg_verficar IS INITIAL.
    TRY.
        CALL METHOD zcl_romaneio=>get_status_opus
          EXPORTING
            i_referencia   = wa_romaneio-ch_referencia
            i_tp_movimento = wa_romaneio-tp_movimento
          RECEIVING
            r_status       = v_status.
      CATCH zcx_error INTO DATA(zcx_error2).
        zcx_error2->zif_error~published_erro( EXPORTING i_msgty = 'I' i_msgty_display = 'E' ).
        RETURN.
    ENDTRY.

    IF v_status EQ 'CA'.
      MESSAGE w009.
    ELSEIF v_status NE 'CO' AND wa_romaneio-tp_movimento = 'E' .
      MESSAGE w008.
    ELSEIF v_status NE 'FE' AND wa_romaneio-tp_movimento = 'S' .
      MESSAGE w010.
    ELSE.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer.

      IF answer EQ 'J'.
        PERFORM registrar_remessa_armazenagem USING wa_romaneio wa_pedidos.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE w003.
  ENDIF.

ENDFORM.                    " REGISTRAR_SAIDA

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_ROMANEIO
*&---------------------------------------------------------------------*
*       Verifica seleção de romaneio do pedido
*----------------------------------------------------------------------*
FORM verifica_selecao_romaneio  USING vg_verficar TYPE sy-subrc.

*  READ TABLE IT_ROMANEIO INTO WA_ROMANEIO WITH KEY MARCK = C_X.
*  VG_VERFICAR = SY-SUBRC.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = tl_index_rows.

  vg_verficar = 4.
  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE it_romaneio INTO wa_romaneio INDEX wl_index_rows-index.
    vg_verficar = sy-subrc.
  ENDLOOP.
ENDFORM.                    " VERIFICA_SELECAO_ROMANEIO

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_REMESSA_ARMAZENAGEM
*&---------------------------------------------------------------------*
*       Registrar remessa p/ armazenagem - validação
*----------------------------------------------------------------------*
FORM registrar_remessa_armazenagem  USING p_romaneio TYPE zarm_romaneio p_pedido TYPE zarm_pedido.

  DATA: wa_roma TYPE zsdt0001.

  IF wa_pedidos-elikz EQ c_x.
    MESSAGE w007.
  ENDIF.

*  IF VG_RETORNO IS INITIAL.
*    SELECT SINGLE * INTO WA_ROMA
*      FROM ZSDT0001
*     WHERE CH_REFERENCIA EQ P_ROMANEIO-CH_REFERENCIA
*       AND VBELN         EQ P_ROMANEIO-VBELN.
*      .
*  ELSE.
  SELECT SINGLE * INTO wa_roma
     FROM zsdt0001
    WHERE  ch_referencia EQ p_romaneio-ch_referencia.
*  ENDIF.

  IF sy-subrc IS INITIAL.
    "Check se o romaneio foi ja tem lançamento NFe.
    SELECT SINGLE * FROM j_1bnfdoc
      INTO @DATA(w_j_1bnfdoc)
    WHERE nfenum EQ @wa_roma-nfnum
      AND docdat EQ @wa_roma-docdat
      AND bukrs  EQ @wa_roma-bukrs
      AND branch EQ @wa_roma-branch
      AND parid  EQ @wa_roma-parid
      AND docnum NE @space
      AND cancel EQ @space
      AND doctyp NE '5'.

    IF sy-subrc EQ 0.
*      MESSAGE w004 WITH p_romaneio-nr_romaneio INTO wa_messagem-texto.
      wa_messagem-icone = icon_led_green.
      CONCATENATE 'Nota ' wa_roma-nfnum 'ja foi realizada o lançamento de entrada!' INTO wa_messagem-texto SEPARATED BY space.
      APPEND wa_messagem TO it_messagem.
      EXIT.
    ENDIF.


    IF wa_roma-status EQ c_x OR wa_roma-doc_material IS NOT INITIAL.
      MESSAGE w005 WITH p_romaneio-nr_romaneio.
    ELSE.
      MOVE-CORRESPONDING wa_roma TO p_romaneio.
      "
      PERFORM f_lock_rom USING 'B' p_romaneio-ch_referencia. "Bloqueia romaneio
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        PERFORM registrar USING p_romaneio p_pedido.
      ENDIF.
      "
      PERFORM f_lock_rom USING 'D' p_romaneio-ch_referencia. "Desbloqueia romaneio
      "
    ENDIF.
  ELSE.
    MESSAGE w004 WITH p_romaneio-nr_romaneio.
  ENDIF.

ENDFORM.                    " REGISTRAR_REMESSA_ARMAZENAGEM

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR
*&---------------------------------------------------------------------*
*       Registrar remessa p/ armazenagem - registrar
*----------------------------------------------------------------------*
FORM registrar  USING p_romaneio TYPE zarm_romaneio p_pedido TYPE zarm_pedido.
  "
  DATA: tg_0023     TYPE TABLE OF zmm0023,
        wa_0023     TYPE zmm0023,
        wa_0023_aux TYPE sy-subrc,
        "VL_CENTRO_A TYPE WERKS_D,
        vl_clabs_f  TYPE labst,
        vl_centro_a TYPE werks_d,
        vl_clabs_a  TYPE labst,
        vl_clabs_e  TYPE labst,
        vl_total    TYPE labst,
        vl_aux      TYPE char18,
        vl_msn1     TYPE char50,
        vl_msn2     TYPE char50.

  DATA: goodsmvt_header  TYPE bapi2017_gm_head_01,
        goodsmvt_code    TYPE bapi2017_gm_code,
        wa_item          TYPE bapi2017_gm_item_create,
        goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
        extensionin	     TYPE TABLE OF bapiparex,
        goodsmvt_headret TYPE bapi2017_gm_head_ret,
        materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
        matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year,
        it_return        TYPE TABLE OF bapiret2,
        wa_return        TYPE bapiret2,
        vg_msgnr         TYPE msgnr,
        wa_roma          TYPE zsdt0001,
        p_data_ent       TYPE datum,
        p_data_ent2      TYPE datum,
        p_data_val       TYPE datum,
        v_in_mwskz       TYPE j_1btaxcodev-in_mwskz,
        vl_descript      TYPE setlinet-descript,
        vl_regio         TYPE lfa1-regio,
        wa_part          TYPE lfa1,
        vl_lifnr         TYPE lfa1-lifnr,
        tl_texto         TYPE catsxt_longtext_itab,
        wl_texto         TYPE LINE OF catsxt_longtext_itab,
        it_zmmt0065      TYPE TABLE OF zmmt0065,
        wa_zmmt0065      TYPE zmmt0065,
        wa_j_1bnflin     TYPE j_1bnflin,
        vg_refkey        TYPE j_1bnflin-refkey,
        v_safra_a        TYPE zsdt0001-nr_safra,
        e_status(1),
        e_messa(64),
        wa_zmmt0017      TYPE zmmt0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

  DATA:
    regio_e      TYPE lfa1-regio,
    regio_d      TYPE lfa1-regio,
    vwerks_e     TYPE lfa1-lifnr,
    vwerks_d     TYPE lfa1-lifnr,
    pos          TYPE i,
    v_forn_orig  TYPE lfa1-lifnr,
    v_forn_dest  TYPE lfa1-lifnr,
    v_ownpr      TYPE mbew-ownpr,
    v_matkl      TYPE mara-matkl,
    v_regio_orig TYPE lfa1-regio,
    v_regio_dest TYPE lfa1-regio,
    wa_zmmt0154  TYPE zmmt0154,
    lc_dados     TYPE zsde0185,    "*-US191846-01.10.2025-#191846-JT-inicio
    lc_retorno   TYPE zmmt0154_t,  "*-US191846-01.10.2025-#191846-JT-inicio
    wc_retorno   TYPE zmmt0154.    "*-US191846-01.10.2025-#191846-JT-inicio

  CLEAR: it_messagem[].

  "goodsmvt_header-pstng_date = sy-datum.
  goodsmvt_header-pstng_date = sy-datum.

  p_data_ent = p_romaneio-dt_movimento .

  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = p_data_ent
      p_bukrs        = p_romaneio-bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    IMPORTING
      p_data_val     = p_data_val
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    goodsmvt_header-doc_date = p_data_val.
  ENDIF.

  IF p_romaneio-fat_contingencia_ecc EQ abap_true.
    goodsmvt_header-pstng_date = p_romaneio-dt_movimento.
  ENDIF.

  IF p_romaneio-fat_contingencia_ecc EQ abap_true AND
     vg_retorno IS INITIAL. "Remessa armazenagem

    DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

    CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
      EXPORTING
        i_ch_referencia         = p_romaneio-ch_referencia
        i_get_dados_fat_ecc     = abap_true
      IMPORTING
        e_dados_faturamento_ecc = lwa_faturamento_ecc.

    IF lwa_faturamento_ecc-data_lcto_nf_arm IS INITIAL.
      MESSAGE 'Data Lacto NF-e Arm não encontrado no ECC'  TYPE 'E'.
      RETURN.
    ENDIF.

    goodsmvt_header-pstng_date = lwa_faturamento_ecc-data_lcto_nf_arm.
    goodsmvt_header-doc_date   = lwa_faturamento_ecc-data_lcto_nf_arm.

    p_data_ent2 = lwa_faturamento_ecc-data_lcto_nf_arm.

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = p_data_ent2
        p_bukrs        = p_romaneio-bukrs
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      IMPORTING
        p_data_val     = p_data_val
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.


  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      i_bukrs  = p_romaneio-bukrs
      i_data   = p_data_val
    IMPORTING
      e_status = e_status
      e_messa  = e_messa
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF  e_status = 'E'.
    MESSAGE e000(z01) WITH e_messa.
    EXIT.
  ENDIF.

  IF p_romaneio-tp_movimento = 'E'.
    goodsmvt_header-doc_date = p_romaneio-docdat.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_pedido-ebeln
    IMPORTING
      output = p_pedido-ebeln.

  goodsmvt_header-header_txt = p_pedido-ebeln.
  goodsmvt_code-gm_code      = c_04.

* ---> S4 Migration - 21/06/2023 - FC - Inicio

  DATA(v_len) = strlen( p_pedido-matnr ).

  IF v_len > 18.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = p_pedido-matnr
      IMPORTING
        output = wa_item-material_long.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = p_pedido-matnr
      IMPORTING
        output = wa_item-material.
  ENDIF.

* <--- S4 Migration - 21/06/2023 - FC - Fim

*iNCLUDE: MM07MFJ1
*MODULO-FUNÇÃO: J_1B_IM_NF_DOCUMENT_GENERATE
*
*IF SY-TCODE EQ 'ZMM0019'
*"VERIFICAR PARÂMETRO ZNFE (S/N)
*(SAPMM07M)T156-J_1BNFTYPE
*(SAPLJ1BF)T156-J_1BNFTYPE
*ENDIF.

*  IF p_romaneio-nfe IS INITIAL.
*    SET PARAMETER ID 'ZNFE' FIELD 'N'.
*  ELSE.
*    SET PARAMETER ID 'ZNFE' FIELD 'S'.
*  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_pedido-werks
    IMPORTING
      output = vl_lifnr.

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro   = vl_lifnr
      p_partype    = 'B'
    CHANGING
      wa_info_part = wa_part.

  TRY .
      zcl_deposito=>zif_deposito~get_instance(
        )->get_deposito_material_filial(
        EXPORTING
          i_matnr          = p_romaneio-matnr    " Nº do material
          i_tp_produto     = CONV #( COND string( WHEN p_romaneio-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )    " Tipo de Produto
          i_bukrs          = p_romaneio-bukrs    " Empresa
          i_branch         = p_romaneio-branch    " Local de negócios
        IMPORTING
          e_lgort          = p_pedido-lgort " Depósito
          e_centro_a_fixar = DATA(e_centro_a_fixar)
        ).
    CATCH zcx_deposito INTO DATA(ex_deposito).    "
      ex_deposito->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
  ENDTRY.

  "Envio de Remessa para armazenagem
  IF vg_retorno IS INITIAL.

    CLEAR: goodsmvt_header-ref_doc_no.
    wa_item-plant           = p_pedido-werks.
    wa_item-stge_loc        = p_pedido-lgort.
    wa_item-batch           = p_romaneio-nr_safra.
    wa_item-move_type       = 'Z41'.
    wa_item-vendor          = p_pedido-lifnr.
    wa_item-entry_qnt       = p_romaneio-peso_liq.
    wa_item-ext_base_amount = p_romaneio-peso_liq * p_pedido-netpr.
    "wa_item-tax_code        = 'ZE'.

    SELECT SINGLE t~descript
      FROM setleaf AS s
     INNER JOIN setlinet AS t ON t~setname EQ s~setname AND t~lineid EQ s~lineid
      INTO vl_descript
     WHERE s~setname = 'MAGGI_ZMM0019_IVA_SAIDA'
       AND s~valfrom = wa_part-regio .

    IF sy-subrc NE 0.
      MESSAGE 'Código do imposto não existe, entrar em contato com a Área Fiscal SET(MAGGI_ZMM0019_IVA_SAIDA)' TYPE 'E'.
      EXIT.
    ELSEIF vl_descript IS INITIAL.
      MESSAGE 'Código do imposto em branco, entrar em contato com a Área Fiscal SET(MAGGI_ZMM0019_IVA_SAIDA)' TYPE 'E'.
      EXIT.
    ENDIF.

    "CS2017001932 04/09/2017
    SELECT *
     FROM zmm0023
     INTO TABLE tg_0023.

    SORT tg_0023 BY  werks ASCENDING matnr ASCENDING matkl ASCENDING cwerks DESCENDING."PBALVES
*    SORT tg_0023 BY  werks ASCENDING matnr ASCENDING cwerks DESCENDING.
    CLEAR wa_0023.
    READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = p_pedido-werks
                                             matnr = p_pedido-matnr. "lê o primeiro CS2023000120 Urgente - Atualização tela de bloqueio
    "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA
    IF sy-subrc NE 0.
      SELECT SINGLE matkl INTO @DATA(_matkl) FROM mara WHERE matnr = @p_pedido-matnr.
      READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = p_pedido-werks
                                               matkl = _matkl.
    ENDIF.

    "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
    CLEAR wa_0023_aux.
    IF
      sy-subrc NE 0.
      wa_0023_aux = sy-subrc.
    ENDIF.
    "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

    IF NOT sy-subrc = 0 OR wa_0023-status NE 'A'.

      CLEAR: vl_clabs_f,vl_clabs_a,vl_centro_a.

      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - BG
      CLEAR wa_zmmt0017.
      SELECT SINGLE centro_a_fixar
         FROM zmmt0017
         INTO vl_centro_a
       WHERE  matnr       EQ wa_item-material
         AND  centro_fixo EQ p_pedido-werks " smc #108346
         AND  lgort       EQ p_pedido-lgort.

      IF sy-subrc = 0.
        wa_zmmt0017-centro_a_fixar = vl_centro_a.
      ENDIF.

*      zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
*        exporting
*          i_material      = wa_item-material
*          i_centro_fixo   = p_pedido-werks
*          i_deposito      = p_pedido-lgort
*         I_EUDR          =
*        importing
*          e_single_depara = wa_zmmt0017
*      ).

      IF wa_zmmt0017 IS NOT INITIAL.
        vl_centro_a = wa_zmmt0017-centro_a_fixar.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM
        IF wa_0023_aux IS NOT INITIAL.
          MESSAGE e897(sd) WITH  'Falta parâmetros na ZMM0029. '
                                    'Favor entrar em contato com '
                                     'a área de controladoria e estoque. '.
        ENDIF.
        "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC


        SELECT SINGLE clabs
            FROM mchb
            INTO vl_clabs_f
          WHERE  matnr EQ wa_item-material
            AND  werks EQ p_pedido-werks
            AND  lgort EQ p_pedido-lgort
            AND  charg EQ p_romaneio-nr_safra.

        IF NOT e_centro_a_fixar IS INITIAL.
          IF p_romaneio-nr_safra GT '2019'.
            CONCATENATE p_romaneio-nr_safra '_' p_romaneio-branch INTO v_safra_a.
          ELSE.
            v_safra_a = p_romaneio-nr_safra.
          ENDIF.
          SELECT SINGLE clabs
            FROM mchb
            INTO vl_clabs_a
          WHERE  matnr EQ wa_item-material
            AND  werks EQ e_centro_a_fixar
            AND  lgort EQ p_pedido-lgort
            AND  charg EQ v_safra_a.
        ENDIF.

        vl_total = vl_clabs_a + vl_clabs_f .

        IF  p_romaneio-peso_liq GT vl_total.
          vl_aux = vl_total.
          CONDENSE vl_aux NO-GAPS.
          CONCATENATE 'O total'
                      vl_aux
                      'do centro'
                 INTO vl_msn1 SEPARATED BY space.
          CONCATENATE p_pedido-werks
                      'e'
                      e_centro_a_fixar
                 INTO vl_msn2 SEPARATED BY space.
          MESSAGE e897(sd) WITH vl_msn1
                                vl_msn2
                                'é menor que a quantidade da remessa'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    "CS2017001932 - busca IVA
    SELECT SINGLE  *
       FROM ekko
       INTO @DATA(wa_ekko)
       WHERE ebeln EQ @p_pedido-ebeln.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_ekko-lifnr
      IMPORTING
        output = v_forn_orig.


    SELECT SINGLE ekpo~werks
       INTO vwerks_d
       FROM ekpo
      WHERE ekpo~ebeln = p_pedido-ebeln.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vwerks_d
      IMPORTING
        output = v_forn_dest.

    SELECT SINGLE ownpr FROM mbew INTO v_ownpr WHERE bwkey = p_pedido-werks AND  matnr = wa_item-material.


    SELECT SINGLE matkl FROM mara INTO v_matkl WHERE matnr = wa_item-material.


    SELECT SINGLE regio FROM lfa1 INTO v_regio_orig WHERE lifnr = v_forn_orig.


    SELECT SINGLE regio FROM lfa1 INTO v_regio_dest WHERE lifnr = v_forn_dest.

    IF 'ZARM_ZARS' CS wa_ekko-bsart.
      SELECT SINGLE regio FROM lfa1 INTO v_regio_orig WHERE lifnr = v_forn_dest.

      SELECT SINGLE regio FROM lfa1 INTO v_regio_dest WHERE lifnr = v_forn_orig.
    ENDIF.

    DATA(lv_matnr)  = wa_item-material.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = wa_item-material
      IMPORTING
        output = lv_matnr.

*-US191846-01.10.2025-#191846-JT-inicio
*    SELECT SINGLE * FROM  zmmt0154 INTO wa_zmmt0154 WHERE
*              bsart       = wa_ekko-bsart
*              AND uf_orig = v_regio_orig
*              AND uf_dest = v_regio_dest
*              AND ownpr   = v_ownpr
*              AND matnr   = lv_matnr.
*    IF sy-subrc IS NOT INITIAL.
*      SELECT SINGLE * FROM  zmmt0154 INTO wa_zmmt0154 WHERE
*                    bsart         = wa_ekko-bsart
*                    AND uf_orig   = v_regio_orig
*                    AND uf_dest   = v_regio_dest
*                    AND ownpr     = v_ownpr
*                    AND matkl     = v_matkl.
*
*    ENDIF.


    CLEAR: lc_dados, wa_zmmt0154.
    lc_dados-ebeln-valor   = p_pedido-ebeln.
    lc_dados-bsart-valor   = wa_ekko-bsart.
    lc_dados-uf_orig-valor = v_regio_orig.
    lc_dados-uf_dest-valor = v_regio_dest.
    lc_dados-matnr-valor   = lv_matnr.
    lc_dados-ownpr-valor   = v_ownpr.
    lc_dados-bukrs-valor   = p_pedido-bukrs.
    lc_dados-werks-valor   = p_pedido-werks.
    lc_dados-direcao-valor = '2'. "saida

    lc_retorno = zcl_leis_fiscais=>get_impostos( i_dados = lc_dados i_todos = abap_false ).

    READ TABLE lc_retorno INTO wa_zmmt0154 INDEX 1.

    IF wa_zmmt0154-mwskz IS INITIAL.
      DATA: lva_msg_error TYPE string.

      lva_msg_error = |Não encontrado parametros com IVA na ZMM0185 para o tipo Pedido: { wa_ekko-bsart } Origem: { v_regio_orig }|.
      lva_msg_error = |{ lva_msg_error } Destino: { v_regio_dest } Material: { lv_matnr } Produção Interna: { v_ownpr } Direção: Saida!|.
      lva_msg_error = |{ lva_msg_error } Criar FI para Departamento Fiscal|.
      MESSAGE lva_msg_error TYPE 'I'.
      RETURN.
    ENDIF.

*-US191846-01.10.2025-#191846-JT-fim

    IF wa_zmmt0154 IS NOT INITIAL.
      vl_descript       = wa_zmmt0154-mwskz.
    ENDIF.


    wa_item-tax_code        = vl_descript.

    APPEND wa_item TO goodsmvt_item.

  ELSE.
    "Retorno de Remessa para armazenagem
    wa_item-plant           = p_pedido-werks.
    wa_item-stge_loc        = p_pedido-lgort.
    wa_item-batch           = p_romaneio-nr_safra.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = p_romaneio-nfnum
      IMPORTING
        output = goodsmvt_header-ref_doc_no.

    CONCATENATE goodsmvt_header-ref_doc_no '-' p_romaneio-series INTO goodsmvt_header-ref_doc_no.

    IF p_romaneio-nfe IS INITIAL.
      wa_item-move_type       = 'Z45'.
    ELSE.
      wa_item-move_type       = 'Z42'.
    ENDIF.

*    select single  *
*    from ekko
*    into @data(wa_ekko2)
*    where ebeln eq @p_pedido-ebeln.
*    if wa_ekko2-bsart eq 'ZARS' and wa_item-move_type = 'Z42'.
*      wa_item-po_number = p_pedido-ebeln.
*      wa_item-po_item   = '00010'.
*    endif.

    wa_item-vendor          = p_pedido-lifnr.
    wa_item-entry_qnt       = p_romaneio-peso_fiscal.
    wa_item-ext_base_amount = p_romaneio-netwr.
    "wa_item-ext_base_amount = p_romaneio-peso_fiscal * p_pedido-netpr.



    SELECT SINGLE t~descript
      FROM setleaf AS s
     INNER JOIN setlinet AS t ON t~setname EQ s~setname AND t~lineid EQ s~lineid
      INTO vl_descript
     WHERE s~setname = 'MAGGI_ZMM0019_IVA_ENTRAD'
       AND s~valfrom = wa_part-regio .

    wa_item-tax_code  = vl_descript.
    IF sy-subrc NE 0.
      MESSAGE 'Código do imposto não existe, entrar em contato com a Área Fiscal SET(MAGGI_ZMM0019_IVA_ENTRAD)' TYPE 'E'.
      EXIT.
    ELSEIF vl_descript IS INITIAL.
      MESSAGE 'Código do imposto em branco, entrar em contato com a Área Fiscal SET(MAGGI_ZMM0019_IVA_ENTRAD)' TYPE 'E'.
      EXIT.
    ENDIF.

    "wa_item-tax_code        = 'ZJ'.

*   Verifica Set ZMM0019_LDC
    PERFORM z_verifica_set USING wa_item-plant
                        CHANGING wa_item-tax_code.

    APPEND wa_item TO goodsmvt_item.
  ENDIF.

  FREE extensionin.
  IF p_romaneio-ch_referencia IS NOT INITIAL.
    APPEND VALUE #( structure = 'ROMANEIO' valuepart1 = p_romaneio-ch_referencia ) TO extensionin.
  ENDIF.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = goodsmvt_header
      goodsmvt_code    = goodsmvt_code
    IMPORTING
      goodsmvt_headret = goodsmvt_headret
      materialdocument = materialdocument
      matdocumentyear  = matdocumentyear
    TABLES
      goodsmvt_item    = goodsmvt_item
      extensionin      = extensionin
      return           = it_return.

  "SET PARAMETER ID 'ZNFE' FIELD 'Z'.

  LOOP AT it_return INTO wa_return.

    CASE wa_return-type.
      WHEN c_s.
        wa_messagem-icone = icon_led_green.
      WHEN c_e.
        wa_messagem-icone = icon_led_red.
      WHEN c_w.
        wa_messagem-icone = icon_led_yellow.
    ENDCASE.

    WRITE wa_return-number TO vg_msgnr.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = wa_return-id
        msg_no                 = vg_msgnr
        msg_var1               = wa_return-message_v1
        msg_var2               = wa_return-message_v2
        msg_var3               = wa_return-message_v3
        msg_var4               = wa_return-message_v4
      IMPORTING
        msg_text               = wa_messagem-texto
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    APPEND wa_messagem TO it_messagem.

  ENDLOOP.

  IF NOT materialdocument IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    WAIT UP TO 1 SECONDS.

    LOOP AT it_romaneio INTO wa_romaneio WHERE ch_referencia = p_romaneio-ch_referencia.
      vg_tabix = sy-tabix.
      wa_romaneio-icone        = icon_checked.
      wa_romaneio-status       = c_x.
      wa_romaneio-doc_material = materialdocument.
      wa_romaneio-ano_material = matdocumentyear.

      MOVE-CORRESPONDING wa_romaneio TO wa_roma.
      wa_roma-status       = c_x.
      wa_roma-doc_material = materialdocument.
      wa_roma-ano_material = matdocumentyear.
      MODIFY zsdt0001 FROM wa_roma.
      COMMIT WORK.

      MODIFY it_romaneio INDEX vg_tabix FROM wa_romaneio TRANSPORTING icone status doc_material ano_material.

      "Bloquear Romaneio Opus
      CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
        EXPORTING
          cd_referencia = wa_roma-ch_referencia
          tp_bloqueio   = abap_true.

    ENDLOOP.


    CONCATENATE materialdocument matdocumentyear INTO vg_refkey.

    SELECT SINGLE *
      FROM j_1bnflin
      INTO wa_j_1bnflin
      WHERE refkey = vg_refkey.

    "TEXT NF
    IF sy-subrc = 0.
      REFRESH: tl_texto.
      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Texto da NOTA'
        CHANGING
          ch_text  = tl_texto.

      LOOP AT tl_texto INTO wl_texto.
        wa_zmmt0065-docnum  = wa_j_1bnflin-docnum.
        wa_zmmt0065-seqnum  = sy-tabix.
        wa_zmmt0065-message = wl_texto.
        APPEND wa_zmmt0065 TO it_zmmt0065.
      ENDLOOP.
      MODIFY zmmt0065 FROM TABLE it_zmmt0065.
      COMMIT WORK.
    ENDIF.



    IF vg_retorno IS NOT INITIAL.
      UPDATE zsdt0001 SET vbeln = p_romaneio-vbeln
         WHERE ch_referencia = p_romaneio-ch_referencia.
      COMMIT WORK.
    ENDIF.

    wa_messagem-icone = icon_led_green.
    CONCATENATE 'Documento de material' materialdocument 'gerado para o ano' matdocumentyear '!' INTO wa_messagem-texto SEPARATED BY space.
    APPEND wa_messagem TO it_messagem.
  ELSE.
    wa_messagem-icone = icon_led_red.
    wa_messagem-texto = 'Processamento não concluido!'.
    APPEND wa_messagem TO it_messagem.
  ENDIF.

ENDFORM.                    " REGISTRAR


*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       SHDB
*----------------------------------------------------------------------*
FORM f_bdc_field  USING  VALUE(p_flag) VALUE(p_fnam) VALUE(p_fval).

  CLEAR it_bdc.
  IF NOT p_flag IS INITIAL.
    it_bdc-program  = p_fnam.
    it_bdc-dynpro   = p_fval.
    it_bdc-dynbegin = c_x.
  ELSE.
    it_bdc-fnam = p_fnam.
    it_bdc-fval = p_fval.
  ENDIF.
  APPEND it_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  CHAMA_LOG
*&---------------------------------------------------------------------*
*       Chama log de processamento.
*----------------------------------------------------------------------*
FORM chama_log .
  CALL SCREEN 9999 STARTING AT 10 10.
ENDFORM.                    " CHAMA_LOG

*&---------------------------------------------------------------------*
*&      Module  CHAMA_DOC_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       Chama Documento de Material
*----------------------------------------------------------------------*
MODULE chama_doc_material INPUT.
  DATA opt TYPE ctu_params.
  IF it_romaneio-doc_material IS INITIAL.
    MESSAGE w006 WITH it_romaneio-nr_romaneio.
  ELSE.
*    SET PARAMETER ID: 'MBN' FIELD it_romaneio-doc_material,
*                      'MJA' FIELD it_romaneio-ano_material.
*    CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]
    FREE tl_bdc.
    PERFORM f_preencher_dynpro USING:
               'X' 'SAPLMIGO'            '0001',
               ' ' 'BDC_OKCODE'          'OK_GO',
               ' ' 'BDC_SUBSCR'          'SAPLMIGO',
               ' ' 'GODYNPRO-ACTION'     'A04',
               ' ' 'GODYNPRO-REFDOC'     'R02',
               ' ' 'BDC_SUBSCR'          'SAPLMIGO',
               ' ' 'BDC_CURSOR'          'GODYNPRO-MAT_DOC',
               ' ' 'GODYNPRO-MAT_DOC'    it_romaneio-doc_material,
               ' ' 'GODYNPRO-DOC_YEAR'   it_romaneio-ano_material,
               ' ' 'BDC_SUBSCR'          'SAPLMIGO'.

    opt-dismode = 'E'.
    opt-defsize = ' '.

    CALL TRANSACTION 'MIGO' USING tl_bdc OPTIONS FROM opt.
  ENDIF.

ENDMODULE.                 " CHAMA_DOC_MATERIAL  INPUT

*----------------------------------------------------------------------*
*  MODULE tab_romaneios_change_tc_attr OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE tab_romaneios_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_romaneio LINES tab_romaneios-lines.
ENDMODULE.                    "TAB_ROMANEIOS_CHANGE_TC_ATTR OUTPUT

*----------------------------------------------------------------------*
*  MODULE tab_romaneios_mark INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE tab_romaneios_mark INPUT.
  MODIFY it_romaneio
    INDEX tab_romaneios-current_line
    TRANSPORTING marck.
ENDMODULE.                    "TAB_ROMANEIOS_MARK INPUT

*&---------------------------------------------------------------------*
*&      Form  CHAMA_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       Chama documento de material selecionado.
*----------------------------------------------------------------------*
FORM chama_doc_material .

  DATA opt TYPE ctu_params.
  DATA: vg_verficar TYPE sy-subrc.
  PERFORM verifica_selecao_romaneio USING vg_verficar.
  IF vg_verficar IS INITIAL.
    IF wa_romaneio-doc_material IS INITIAL.
      MESSAGE w006 WITH wa_romaneio-nr_romaneio.
    ELSE.
*      SET PARAMETER ID: 'MBN' FIELD wa_romaneio-doc_material,
*                        'MJA' FIELD wa_romaneio-ano_material.
*      CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_no_auth_check = 'X'
          i_mblnr         = wa_romaneio-doc_material
          i_mjahr         = wa_romaneio-ano_material.

*      FREE tl_bdc.
*      PERFORM f_preencher_dynpro USING:
*           'X' 'SAPLMIGO'            '0001',
*           ' ' 'BDC_OKCODE'          '=OK_GO',
*           ' ' 'BDC_SUBSCR'          'SAPLMIGO',
*           ' ' 'GODYNPRO-ACTION'     'A04',
*           ' ' 'GODYNPRO-REFDOC'     'R02',
*           ' ' 'BDC_SUBSCR'          'SAPLMIGO',
*           ' ' 'BDC_CURSOR'          'GODYNPRO-MAT_DOC',
*           ' ' 'GODYNPRO-MAT_DOC'    wa_romaneio-doc_material,
*           ' ' 'GODYNPRO-DOC_YEAR'   wa_romaneio-ano_material,
*           ' ' 'BDC_SUBSCR'          'SAPLMIGO'.
*
*      opt-dismode = 'E'.
*      opt-defsize = ' '.
*
*      CALL TRANSACTION 'MIGO' USING tl_bdc OPTIONS FROM opt.
    ENDIF.
  ELSE.
    MESSAGE w003.
  ENDIF.

ENDFORM.                    " CHAMA_DOC_MATERIAL

*&---------------------------------------------------------------------*
*&      Module  CHAMA_MOV_MERCADORIA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chama_mov_mercadoria INPUT.

  DATA: answer  TYPE c LENGTH 1.

  IF it_romaneio-status IS INITIAL.

    MOVE it_romaneio TO wa_romaneio.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = TEXT-001
        textline1 = TEXT-002
        textline2 = TEXT-003
      IMPORTING
        answer    = answer.

    IF answer EQ 'J'.
      PERFORM registrar_remessa_armazenagem USING wa_romaneio wa_pedidos.
    ENDIF.

  ELSE.
    MESSAGE w005 WITH wa_romaneio-nr_romaneio.
  ENDIF.

ENDMODULE.                 " CHAMA_MOV_MERCADORIA  INPUT
*&---------------------------------------------------------------------*
*&      Module  MOSTRA_PEDIDOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mostra_pedidos INPUT.
*  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
*        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE,
*        VLINHAS       TYPE I.
*
*  DATA: BEGIN OF TL_PEDIDO OCCURS 0,
*          VBELN TYPE EKET-EBELN,
*        END OF TL_PEDIDO.
*
*  CLEAR VLINHAS.
*  LOOP AT IT_ROMANEIO WHERE  MARCK = 'X'.
*    ADD 1 TO VLINHAS.
*  ENDLOOP.
*  IF VLINHAS NE 1.
*    MESSAGE 'Selecione apenas uma linha, para pedidos' TYPE 'I'.
*    EXIT.
*  ENDIF.
*  READ TABLE IT_ROMANEIO WITH KEY MARCK = 'X'.
**  IF IT_ROMANEIO-VBELN IS INITIAL.
*  SELECT *
*   FROM EKKO
*   INTO TABLE @DATA(_T_EKKO)
*   WHERE BUKRS  =  @IT_ROMANEIO-BUKRS
*   AND   BSART   = 'ZARM'
*   AND   LIFNR    = @IT_ROMANEIO-PARID.
*
*  IF  _T_EKKO[] IS NOT INITIAL.
*    SELECT  *
*      FROM EKPO
*      INTO TABLE @DATA(_T_EKPO)
*      FOR ALL ENTRIES IN @_T_EKKO
*      WHERE EBELN EQ @_T_EKKO-EBELN
*      AND   MATNR EQ @IT_ROMANEIO-MATNR
*      AND   WERKS EQ @IT_ROMANEIO-BRANCH.
*
*    IF _T_EKPO[] IS NOT INITIAL.
*      SELECT EBELN
*        FROM EKET
*        INTO TABLE TL_PEDIDO
*        FOR ALL ENTRIES IN _T_EKPO
*        WHERE EBELN = _T_EKPO-EBELN
*        AND   EBELP = _T_EKPO-EBELP
*        AND   CHARG = IT_ROMANEIO-NR_SAFRA.
*
*    ENDIF.
*  ENDIF.
*
*  IF LINES( TL_PEDIDO ) GT 1.
*    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*      EXPORTING
*        RETFIELD        = 'VBELN'
*        DYNPPROG        = SY-REPID
*        DYNPNR          = SY-DYNNR
*        DYNPROFIELD     = 'IT_ROMANEIO-VBELN'
*        VALUE_ORG       = 'S'
*      TABLES
*        VALUE_TAB       = TL_PEDIDO
*        RETURN_TAB      = TL_RETURN_TAB
*        DYNPFLD_MAPPING = TL_DSELC.
*  ENDIF.
**  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GRAVA_PEDIDOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE grava_pedidos INPUT.
  MODIFY it_romaneio
      INDEX tab_romaneios-current_line
      TRANSPORTING vbeln.
ENDMODULE.

FORM f_lock_rom USING p_status
                      p_ch_referencia.

  CASE p_status.
    WHEN 'B'. "Bloqueio

      CALL FUNCTION 'ENQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia  = p_ch_referencia
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    WHEN 'D'. "Desbloqueio

      CALL FUNCTION 'DEQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia = p_ch_referencia.
  ENDCASE.

ENDFORM.

*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2594   text
*      -->P_2595   text
*      -->P_2596   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.


ENDFORM.
