class ZCL_REMESSA_ARMAZENAGEM definition
  public
  final
  create public .

public section.

  interfaces ZIF_REMESSA_ARMAZENAGEM .
protected section.
private section.

  data AT_ROMANEIO type ZSDT0001 .
ENDCLASS.



CLASS ZCL_REMESSA_ARMAZENAGEM IMPLEMENTATION.


  METHOD zif_remessa_armazenagem~gerar_remessa_com_pesagem_opus.

    DATA: lva_debug_flg      TYPE bapidlvcontrol-debug_flg,
          lva_ship_point     TYPE bapidlvcreateheader-ship_point,
          lva_dlv_type       TYPE bapidlvcreateheader-dlv_type,
          lva_salesorg       TYPE bapidlvcreateheader-salesorg,
          lva_distr_chan     TYPE bapidlvcreateheader-distr_chan,
          lva_division       TYPE bapidlvcreateheader-division,
          lva_ship_to        TYPE bapidlvcreateheader-ship_to,
          lva_date_usage     TYPE bapidlvcreateheader-date_usage,

          lwa_dlv_item       TYPE bapidlvnorefitem,
          lit_dlv_items      TYPE z_bapidlvnorefitem_t,

          lwa_dlv_dates      TYPE bapidlvdeadln,
          lit_dlv_dates      TYPE /spe/bapidlvdeadln_t,


          lwa_header_data    TYPE bapiobdlvhdrchg,
          lwa_header_control TYPE bapiobdlvhdrctrlchg,

          lwa_header_partner TYPE bapidlvpartnerchg,
          lit_header_partner TYPE TABLE OF bapidlvpartnerchg,


          lit_item_control   TYPE TABLE OF bapiobdlvitemctrlchg,
          lwa_item_control   TYPE bapiobdlvitemctrlchg,

          lwa_item_data      TYPE bapiobdlvitemchg,
          lit_item_data      TYPE TABLE OF bapiobdlvitemchg,

          lwa_item_data_spl  TYPE /spe/bapiobdlvitemchg,
          lit_item_data_spl  TYPE TABLE OF /spe/bapiobdlvitemchg,

          lwa_vbkok_wa       TYPE vbkok,

          lwa_vbpok          TYPE vbpok,
          lit_vbpok          TYPE TABLE OF vbpok,

          lit_prot           TYPE TABLE OF prott,

*---> 01/06/2023 - Migração S4 - JS
*          lit_return           TYPE BAPIRET2TAB
          lit_return         TYPE bapiret2_t
*<--- 01/06/2023 - Migração S4 - JS
          .

    DATA: lva_timestamp  TYPE bapidlvdeadln-timestamp_utc,
          lva_deliv_numb TYPE bapishpdelivnumb-deliv_numb,
          lva_xblnr      TYPE vbrk-xblnr.

    CLEAR: r_delivery, lva_deliv_numb.

    SELECT SINGLE *
      FROM likp INTO @DATA(lwa_likp)
     WHERE xblnr EQ @i_ch_ref_romaneio.

    IF sy-subrc EQ 0.
      MESSAGE |Remessa: { lwa_likp-vbeln } já gerada para o romaneio!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
     WHERE ch_referencia EQ @i_ch_ref_romaneio.

    CHECK sy-subrc EQ 0.


    SELECT SINGLE *
      FROM ekko INTO @DATA(lwa_ekko)
     WHERE ebeln EQ @lwa_zsdt0001-vbeln.

    CHECK sy-subrc EQ 0 AND ( lwa_ekko-bsart = 'ZARM' ).

    SELECT SINGLE *
      FROM eket INTO @DATA(lwa_eket)
     WHERE ebeln EQ @lwa_ekko-ebeln.

*---------------------------------------------------------------------*
*   Dados Cabeçalho
*---------------------------------------------------------------------*
    lva_ship_point = lwa_zsdt0001-branch.
    lva_dlv_type   = 'ZLO'.
    lva_salesorg   = lwa_zsdt0001-bukrs.
    lva_distr_chan = '10'. "Mercado Interno
    lva_division   = '01'. "Trading
    lva_ship_to    = lwa_zsdt0001-id_cli_dest.

*---------------------------------------------------------------------*
*   Itens Remessa
*---------------------------------------------------------------------*

    lwa_dlv_item-ref_item        = '000010'.
    lwa_dlv_item-item_categ      = 'ZDLN'.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
*    lwa_dlv_item-material        = lwa_zsdt0001-matnr.
    DATA(v_len) = strlen( lwa_zsdt0001-matnr ).
    IF v_len > 18.
      lwa_dlv_item-material_long = lwa_zsdt0001-matnr.
    ELSE.
      lwa_dlv_item-material      = lwa_zsdt0001-matnr.
    ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
    lwa_dlv_item-dlv_qty         = lwa_zsdt0001-peso_liq.
    lwa_dlv_item-sales_unit      = 'KG'.
    lwa_dlv_item-sales_unit_iso  = 'KG'.
    lwa_dlv_item-sales_unit_iso  = 'KG'.
    lwa_dlv_item-plant           = lwa_zsdt0001-branch.


    TRY.
        zcl_deposito=>zif_deposito~get_instance(
          )->get_deposito_material_filial(
          EXPORTING
            i_matnr          = lwa_zsdt0001-matnr
            i_tp_produto     = CONV #( COND string( WHEN lwa_zsdt0001-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )
            i_bukrs          = lwa_zsdt0001-bukrs
            i_branch         = lwa_zsdt0001-branch
          IMPORTING
            e_lgort          = DATA(_lgort_remessa)
            e_centro_a_fixar = DATA(e_centro_a_fixar)
          ).

        lwa_dlv_item-stge_loc = _lgort_remessa.
      CATCH zcx_deposito INTO DATA(ex_deposito).    " .
        ex_deposito->zif_error~published_erro( EXPORTING i_msgty = 'W' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.

    APPEND lwa_dlv_item TO lit_dlv_items.

*---------------------------------------------------------------------*
*   Datas Fornecimento
*---------------------------------------------------------------------*

* WSHDRLFDAT       'WS DELIVERY     LIKP'     Delivery date
* WSHDRWADAT       'WS GOODS ISSUE  LIKP'     Plannned goods issue date
* WSHDRLDDAT       'WS LOADING      LIKP'     Loading date
* WSHDRTDDAT       'WS TRANSP DISP  LIKP'     Transportation planning date
* WSHDRMBDAT       'WS MATERIAL AV. LIKP'     Material availability date

    lva_date_usage = '0'.

    lwa_dlv_dates-timetype = 'WSHDRWADAT'.

    CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
      EXPORTING
        i_bukrs   = lwa_zsdt0001-bukrs
        i_branch  = lwa_zsdt0001-branch
      IMPORTING
        e_tznzone = lwa_dlv_dates-timezone.

    GET TIME STAMP FIELD lwa_dlv_dates-timestamp_utc.

    APPEND lwa_dlv_dates TO lit_dlv_dates.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATENOREF'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        ship_point = lva_ship_point
        dlv_type   = lva_dlv_type
        salesorg   = lva_salesorg
        distr_chan = lva_distr_chan
        division   = lva_division
        ship_to    = lva_ship_to
        date_usage = lva_date_usage
        debug_flg  = lva_debug_flg
      IMPORTING
        delivery   = lva_deliv_numb
      TABLES
        dates      = lit_dlv_dates
        dlv_items  = lit_dlv_items
        return     = lit_return.

    IF lva_deliv_numb IS INITIAL.
      READ TABLE lit_return INTO DATA(lwa_return) WITH KEY type = 'E'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    r_delivery = lva_deliv_numb.

*---------------------------------------------------------------------*
*   Incluir Chave Referencia do Romaneio no cabeçalho da Remessa
*---------------------------------------------------------------------*

    lva_xblnr = lwa_zsdt0001-ch_referencia.

    CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
      EXPORTING
        i_vbeln = lva_deliv_numb
        i_xblnr = lva_xblnr.

*---------------------------------------------------------------------*
*   Modificações do Cabeçalho
*---------------------------------------------------------------------*

    lwa_header_data-deliv_numb        = lva_deliv_numb.
    lwa_header_control-deliv_numb     = 'X'.

*---------------------------------------------------------------------*
*   Modificações do Parceiros
*---------------------------------------------------------------------*
    lwa_header_partner-upd_mode_partn = 'I'.
    lwa_header_partner-deliv_numb     = lva_deliv_numb.
    lwa_header_partner-itm_number     = '000010'.
    lwa_header_partner-partn_role     = 'PC'.
    lwa_header_partner-partner_no     = lwa_zsdt0001-parid.
    APPEND lwa_header_partner TO lit_header_partner.

    lwa_header_partner-upd_mode_partn = 'I'.
    lwa_header_partner-deliv_numb     = lva_deliv_numb.
    lwa_header_partner-itm_number     = '000010'.
    lwa_header_partner-partn_role     = 'SP'.
    lwa_header_partner-partner_no     = i_lifnr_sp.
    APPEND lwa_header_partner TO lit_header_partner.

    lwa_header_partner-upd_mode_partn = 'I'.
    lwa_header_partner-deliv_numb     = lva_deliv_numb.
    lwa_header_partner-itm_number     = '000010'.
    lwa_header_partner-partn_role     = 'LR'.
    lwa_header_partner-partner_no     = lwa_zsdt0001-id_cli_dest.
    APPEND lwa_header_partner TO lit_header_partner.

    lwa_header_partner-upd_mode_partn = 'I'.
    lwa_header_partner-deliv_numb     = lva_deliv_numb.
    lwa_header_partner-itm_number     = '000010'.
    lwa_header_partner-partn_role     = 'WL'.
    lwa_header_partner-partner_no     = lwa_ekko-lifnr.
    APPEND lwa_header_partner TO lit_header_partner.

*---------------------------------------------------------------------*
*   Modificações de Itens
*---------------------------------------------------------------------*
    lwa_item_data-deliv_numb          = lva_deliv_numb.
    lwa_item_data-deliv_item          = '000010'.
    lwa_item_data-batch               = lwa_eket-charg.
    lwa_item_data-dlv_qty             = lwa_zsdt0001-peso_liq.
    lwa_item_data-dlv_qty_imunit      = lwa_zsdt0001-peso_liq.
    lwa_item_data-fact_unit_nom       = 1.
    lwa_item_data-fact_unit_denom     = 1.
    lwa_item_data-gross_wt            = lwa_zsdt0001-peso_liq.
    lwa_item_data-net_weight          = lwa_zsdt0001-peso_liq.
    APPEND lwa_item_data TO lit_item_data.

    lwa_item_control-deliv_numb       = lva_deliv_numb.
    lwa_item_control-deliv_item       = '000010'.
    lwa_item_control-chg_delqty       = 'X'.
    lwa_item_control-volume_flg       = 'X'.
    lwa_item_control-net_wt_flg       = 'X'.
    lwa_item_control-gross_wt_flg     = 'X'.
    APPEND lwa_item_control TO lit_item_control.

*---------------------------------------------------------------------*
*   Modificações de Picking
*---------------------------------------------------------------------*
*    lwa_item_data_spl-deliv_numb      = lva_deliv_numb.
*    lwa_item_data_spl-deliv_item      = '000010'.
*    lwa_item_data_spl-stge_loc        = _lgort_remessa.
*    APPEND lwa_item_data_spl to lit_item_data_spl.

    CLEAR: lit_return[].
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = lwa_header_data
        header_control = lwa_header_control
        delivery       = lva_deliv_numb
      TABLES
        header_partner = lit_header_partner
        item_data      = lit_item_data
        item_control   = lit_item_control
        return         = lit_return
        item_data_spl  = lit_item_data_spl.

    IF lines( lit_return[] ) NE 0.
      READ TABLE lit_return INTO lwa_return WITH KEY type = 'E'.
      EXIT.
    ENDIF.

*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    lwa_vbkok_wa-vbeln_vl    = lva_deliv_numb.
*    lwa_vbkok_wa-vbeln       = lva_deliv_numb.
*    lwa_vbkok_wa-wabuc       = 'X'.
*    lwa_vbkok_wa-wadat_ist   = sy-datum.
*
*    lwa_vbpok-vbeln_vl       = lva_deliv_numb.
*    lwa_vbpok-posnr_vl       = '000010'.
*    lwa_vbpok-vbeln          = lva_deliv_numb.
*    lwa_vbpok-posnn          = '000010'.
*    lwa_vbpok-matnr          = lwa_zsdt0001-matnr.
*    lwa_vbpok-pikmg          = lwa_zsdt0001-peso_liq.
*    lwa_vbpok-charg          = lwa_eket-charg.
*    lwa_vbpok-lgort          = _lgort_remessa.
*    lwa_vbpok-brgew          = lwa_zsdt0001-peso_liq.
*    lwa_vbpok-ntgew          = lwa_zsdt0001-peso_liq.
*    lwa_vbpok-gewei          = 'KG'.
*    lwa_vbpok-vbtyp_n        = 'V'.
*    APPEND lwa_vbpok TO lit_vbpok.
*
*    CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
*      EXPORTING
*        vbkok_wa                 = lwa_vbkok_wa
*        synchron                 = 'X'
*        if_error_messages_send_1 = 'X'
*      TABLES
*        vbpok_tab                = lit_vbpok
*        prot                     = lit_prot.
*
*
*    IF lines( lit_prot[] ) NE 0.
*      READ TABLE lit_return INTO lwa_return WITH KEY type = 'E'.
*      EXIT.
*    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.


  method ZIF_REMESSA_ARMAZENAGEM~ESTORNAR_SAIDA_ESTOQUE.

    DATA: lit_return       TYPE TABLE OF bapiret2,
          lwa_head_ret     TYPE bapi2017_gm_head_ret,
          lva_data_estorno TYPE bapi2017_gm_head_02-pstng_date,
          lva_erro         TYPE c LENGTH 1.

    CLEAR: lva_erro, r_estornado.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
     WHERE ch_referencia EQ @i_ch_referencia.

    CHECK sy-subrc EQ 0 AND lwa_zsdt0001-tp_movimento = 'S'.

    IF lwa_zsdt0001-doc_material IS INITIAL OR lwa_zsdt0001-ano_material IS INITIAL.
      SELECT SINGLE * INTO @DATA(LWA_mkpf_exists)
        FROM mkpf as a
       WHERE zch_referencia EQ @i_ch_referencia
         and not exists ( SELECT mblnr
                            FROM mseg as b
                           WHERE b~smbln = a~mblnr ).


      IF sy-subrc eq 0.
        lwa_zsdt0001-doc_material = LWA_mkpf_exists-mblnr.
        lwa_zsdt0001-ano_material = LWA_mkpf_exists-mjahr.
      ELSE.
        r_estornado = abap_true.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_EZSDT0001'
      EXPORTING
        ch_referencia  = lwa_zsdt0001-ch_referencia
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    DATA(lva_doc_estornado) = abap_false.

    SELECT SINGLE *
      FROM mkpf INTO @DATA(wa_mkpf)
      WHERE mblnr EQ @lwa_zsdt0001-doc_material
      AND   mjahr EQ @lwa_zsdt0001-ano_material.

    IF sy-subrc NE 0.
      lva_doc_estornado = abap_true.
    ELSE.
      SELECT SINGLE *
        FROM mseg INTO @DATA(lwa_mseg_estorno)
       WHERE smbln = @lwa_zsdt0001-doc_material
         AND sjahr = @lwa_zsdt0001-ano_material.

      IF sy-subrc EQ 0.
        lva_doc_estornado = abap_true.
      ENDIF.
    ENDIF.

    IF lva_doc_estornado = abap_false.

      IF wa_mkpf-budat+0(6) = sy-datum+0(6).
        lva_data_estorno = wa_mkpf-budat.
      ELSE.
        lva_data_estorno = sy-datum.
      ENDIF.

      "lwa_head_ret-mat_doc  = lwa_zsdt0001-doc_material.
      "lwa_head_ret-doc_year = lwa_zsdt0001-ano_material.

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = lwa_zsdt0001-doc_material
          matdocumentyear     = lwa_zsdt0001-ano_material
          goodsmvt_pstng_date = lva_data_estorno
        IMPORTING
          goodsmvt_headret    = lwa_head_ret
        TABLES
          return              = lit_return.

      IF lwa_head_ret-mat_doc IS INITIAL.
        LOOP AT lit_return INTO DATA(sl_return) WHERE type = 'E'.
          MESSAGE i000(z01) WITH 'Não foi possível estornar o documento Material:'
                                 sl_return-message(40)
                                 sl_return-message+40(40).

          lva_erro = abap_true.
        ENDLOOP.
      ENDIF.

    ELSE.
      CLEAR: lva_erro. "Continuar processo e limpar campo do documento de material, pois os mesmo não existe
    ENDIF.

    IF lva_erro eq abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      r_estornado = abap_true.

      UPDATE zsdt0001
         SET status       = ''
             doc_material = ''
             ano_material = ''
       WHERE ch_referencia = lwa_zsdt0001-ch_referencia.

      COMMIT WORK.

      CALL FUNCTION 'DEQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia = lwa_zsdt0001-ch_referencia.

      CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
        EXPORTING
          cd_referencia = lwa_zsdt0001-ch_referencia
          tp_bloqueio   = space.

      MESSAGE s000(z01) WITH 'Movimento estoque Estornado!'.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EZSDT0001'
      EXPORTING
        ch_referencia = lwa_zsdt0001-ch_referencia.

  endmethod.


  method ZIF_REMESSA_ARMAZENAGEM~GERAR_SAIDA_ESTOQUE.

    DATA: tg_0023    TYPE TABLE OF zmm0023,
          wa_0023    TYPE zmm0023,
          vl_clabs_f TYPE labst,
          vl_clabs_a TYPE labst,
          vl_clabs_e TYPE labst,
          vl_total   TYPE labst,
          vl_aux     TYPE char18,
          vl_msn1    TYPE char50,
          vl_msn2    TYPE char50,
          vl_msg     TYPE string.

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
          e_messa(64).

    CLEAR: e_mblnr, e_mjahr, r_gerou.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
     WHERE ch_referencia EQ @i_ch_referencia.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE * INTO @DATA(lwa_mkpf_exists)
      FROM mkpf AS a
     WHERE zch_referencia EQ @i_ch_referencia
       AND NOT EXISTS ( SELECT mblnr
                          FROM mseg AS b
                         WHERE b~smbln = a~mblnr ).


    IF sy-subrc EQ 0.
      MESSAGE |Documento Estoque: { lwa_mkpf_exists-mblnr } já gerado para o romaneio!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM ekko INTO @DATA(lwa_ekko)
     WHERE ebeln EQ @lwa_zsdt0001-vbeln.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM ekpo INTO @DATA(lwa_ekpo)
     WHERE ebeln EQ @lwa_ekko-ebeln.

    CHECK sy-subrc EQ 0.

    goodsmvt_header-pstng_date = sy-datum.

    p_data_ent = lwa_zsdt0001-dt_movimento .

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = p_data_ent
        p_bukrs        = lwa_zsdt0001-bukrs
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

    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = lwa_zsdt0001-bukrs
        i_data   = p_data_val
      IMPORTING
        e_status = e_status
        e_messa  = e_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF e_status = 'E'.
      MESSAGE e000(z01) WITH e_messa.
      EXIT.
    ENDIF.

    goodsmvt_header-header_txt = lwa_zsdt0001-vbeln.
    goodsmvt_code-gm_code      = '04'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lwa_zsdt0001-matnr
      IMPORTING
        output = wa_item-material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_ekpo-werks
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
            i_matnr          = lwa_zsdt0001-matnr
            i_tp_produto     = CONV #( COND string( WHEN lwa_zsdt0001-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )    " Tipo de Produto
            i_bukrs          = lwa_zsdt0001-bukrs
            i_branch         = lwa_zsdt0001-branch
          IMPORTING
            e_lgort          = lwa_ekpo-lgort "
            e_centro_a_fixar = DATA(e_centro_a_fixar)
          ).
      CATCH zcx_deposito INTO DATA(ex_deposito).    "
        ex_deposito->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        EXIT.
    ENDTRY.

    wa_item-plant           = lwa_ekpo-werks.
    wa_item-stge_loc        = lwa_ekpo-lgort.
    wa_item-batch           = lwa_zsdt0001-nr_safra.
    wa_item-move_type       = 'Z41'.
    wa_item-vendor          = lwa_ekko-lifnr.
    wa_item-entry_qnt       = lwa_zsdt0001-peso_liq.
    wa_item-ext_base_amount = lwa_zsdt0001-peso_liq * lwa_ekpo-netpr.

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

    SELECT * FROM zmm0023 INTO TABLE tg_0023.

SORT tg_0023 BY  werks ASCENDING matnr ASCENDING matkl ASCENDING cwerks DESCENDING."PBALVES
*    SORT tg_0023 BY  werks ASCENDING matnr ASCENDING cwerks DESCENDING.
    CLEAR wa_0023.
    READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = lwa_ekpo-werks
                                             matnr = lwa_ekpo-matnr. "lê o primeiro CS2023000120 Urgente - Atualização tela de bloqueio
"141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA

      IF sy-subrc NE 0.
       select single MATKL into @DATA(_MATKL) from MARA Where matnr = @lwa_ekpo-matnr.
       READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = lwa_ekpo-werks
                                                matkl = _MATKL.
    Endif.


"141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
    IF
      sy-subrc NE 0.
      MESSAGE e897(sd) WITH  'Falta parâmetros na ZMM0029. '
                                'Favor entrar em contato com '
                                 'a área de controladoria e estoque. '.
      ENDIF.
""141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

    IF NOT sy-subrc = 0 OR wa_0023-status NE 'A'.


      CLEAR: vl_clabs_f,vl_clabs_a.

      SELECT SINGLE clabs
          FROM mchb INTO vl_clabs_f
        WHERE  matnr EQ wa_item-material
          AND  werks EQ lwa_ekpo-werks
          AND  lgort EQ lwa_ekpo-lgort
          AND  charg EQ lwa_zsdt0001-nr_safra.

      IF NOT e_centro_a_fixar IS INITIAL.
        IF lwa_zsdt0001-nr_safra GT '2019'.
          CONCATENATE lwa_zsdt0001-nr_safra '_' lwa_zsdt0001-branch INTO v_safra_a.
        ELSE.
          v_safra_a = lwa_zsdt0001-nr_safra.
        ENDIF.
        SELECT SINGLE clabs
          FROM mchb
          INTO vl_clabs_a
        WHERE  matnr EQ wa_item-material
          AND  werks EQ e_centro_a_fixar
          AND  lgort EQ lwa_ekpo-lgort
          AND  charg EQ v_safra_a.
      ENDIF.

      vl_total = vl_clabs_a + vl_clabs_f .

      IF  lwa_zsdt0001-peso_liq GT vl_total.
        vl_aux = vl_total.
        CONDENSE vl_aux NO-GAPS.
        CONCATENATE 'O total'
                    vl_aux
                    'do centro e material'
               INTO vl_msn1 SEPARATED BY space.
        CONCATENATE lwa_ekpo-werks
                    'e'
                    e_centro_a_fixar
               INTO vl_msn2 SEPARATED BY space.
        MESSAGE e897(sd) WITH vl_msn1
                              vl_msn2
                              'é menor que a quantidade da remessa'.
        EXIT.
      ENDIF.
    ENDIF.

    wa_item-tax_code        = vl_descript.

    APPEND wa_item TO goodsmvt_item.

    FREE extensionin.
    APPEND VALUE #( structure = 'ROMANEIO' valuepart1 = lwa_zsdt0001-ch_referencia ) TO extensionin.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
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

    IF materialdocument IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      r_gerou = abap_true.
      e_mblnr = materialdocument.
      e_mjahr = matdocumentyear.

      WAIT UP TO 1 SECONDS.

      UPDATE zsdt0001 SET status       = abap_true
                          doc_material = materialdocument
                          ano_material = matdocumentyear
       WHERE ch_referencia = lwa_zsdt0001-ch_referencia.

      "Bloquear Romaneio Opus
      CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
        EXPORTING
          cd_referencia = lwa_zsdt0001-ch_referencia
          tp_bloqueio   = abap_true.

      CONCATENATE materialdocument matdocumentyear INTO vg_refkey.

      SELECT SINGLE *
        FROM j_1bnflin INTO wa_j_1bnflin
        WHERE refkey = vg_refkey.

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

      MESSAGE |Documento de material { materialdocument } gerado para o ano { matdocumentyear } !| TYPE 'S'.
    ELSE.
      READ TABLE it_return INTO DATA(lwa_return) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        MESSAGE ID lwa_return-id
           TYPE 'I'
         NUMBER lwa_return-number
           WITH lwa_return-message_v1 lwa_return-message_v2 lwa_return-message_v3 lwa_return-message_v4.
      ENDIF.
    ENDIF.

  endmethod.
ENDCLASS.
