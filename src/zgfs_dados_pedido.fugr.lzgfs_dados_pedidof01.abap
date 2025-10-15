*----------------------------------------------------------------------*
***INCLUDE LZGFS_DADOS_PEDIDOF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_gerar_remessa
*&---------------------------------------------------------------------*
FORM f_gerar_remessa_novo.

  DATA: lc_tipo_delivery       TYPE char01,
        lc_cabecalho           TYPE zde_bapi_remessa_cab_t,
        lw_cabecalho           TYPE zde_bapi_remessa_cab,
        lw_items               TYPE zde_bapi_remessa_item,
        lc_items               TYPE zde_bapi_remessa_item_t,
        lc_parceiros           TYPE zde_bapi_remessa_parceiros_t,
        lt_dados_itens         TYPE zib_nfe_dist_itm_t,
        lt_dados_lotes         TYPE zib_nfe_dist_lot_t,
        lc_lotes               TYPE zib_nfe_dist_lot_t,
        lc_gerar_movimento     TYPE char01,
        lc_particao_lote       TYPE char01,
        lc_gerou               TYPE char01,
        lc_frete_remessa_trans TYPE REF TO zcl_frete_remessa_trans.

  CREATE OBJECT lc_frete_remessa_trans.

*--------------------------------------
* gerar remessa
*--------------------------------------
  MOVE-CORRESPONDING: it_itens_alv[]   TO lt_dados_itens[],
                      it_lotes_alv_t[] TO lt_dados_lotes[].

  TRY.
      lc_gerou = lc_frete_remessa_trans->set_criar_remessa( EXPORTING t_itens_ped     = lt_dados_itens
                                                                      t_lotes_ped     = lt_dados_lotes
                                                            IMPORTING e_dados_remessa = gt_dados_remessa
                                                                      e_log_remessa   = gt_log_remessa ).
    CATCH zcx_delivery INTO DATA(ex_zcx_delivery).
      DATA(l_mesg) = ex_zcx_delivery->msgv1 && ex_zcx_delivery->msgv2 && ex_zcx_delivery->msgv3 && ex_zcx_delivery->msgv4.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_gerar_remessa
*&---------------------------------------------------------------------*
FORM f_gerar_remessa .


  DATA: nlinhas      TYPE i,
        v_due_date   TYPE ledat,
        v_deliv_numb TYPE bapishpdelivnumb-deliv_numb,
        v_erro       TYPE c LENGTH 1,
        v_vgpos      TYPE lips-vgpos,
        v_tabix      TYPE sy-tabix,
        v_route1     TYPE ekpv-route,
        v_route2     TYPE ekpv-route,
        w_ekpo1      TYPE ekpo,
        w_ekpo2      TYPE ekpo,
        w_eket1      TYPE eket,
        w_eket2      TYPE eket,
        vl_tot_menge TYPE ekpo-menge.

  REFRESH: t_itens, gt_dados_processa_po.
  LOOP AT gt_dados_pedidos INTO  wa_dados_po WHERE NOT ebeln IS INITIAL.

  ENDLOOP.
  SELECT SINGLE route FROM ekpv INTO v_route1
    WHERE ebeln = wa_dados_po-ebeln
    AND   ebelp = wa_dados_po-ebelp.

  SELECT SINGLE * FROM ekpo INTO w_ekpo1
   WHERE ebeln = wa_dados_po-ebeln
   AND   ebelp = wa_dados_po-ebelp.

  SELECT SINGLE * FROM eket INTO w_eket1
   WHERE ebeln = wa_dados_po-ebeln
   AND   ebelp = wa_dados_po-ebelp.

  CLEAR v_erro.
  LOOP AT gt_dados_pedidos INTO wa_dados_pedidos WHERE NOT ebeln IS INITIAL.

    SELECT SINGLE menge
           FROM ekpo
           INTO @DATA(vl_ekpo_menge)
           WHERE ebeln EQ @wa_dados_pedidos-ebeln
           AND   ebelp EQ @wa_dados_pedidos-ebelp.
    IF sy-subrc NE 0.
      v_erro = 'X'.
      MESSAGE s024(sd) WITH TEXT-003 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SUM( menge )
           FROM ekbe
           INTO @DATA(vl_ekbe_menge)
           WHERE ebeln EQ @wa_dados_pedidos-ebeln
           AND   ebelp EQ @wa_dados_pedidos-ebelp
           AND   vgabe EQ '8'.

    vl_tot_menge = vl_ekpo_menge - vl_ekbe_menge.
    IF wa_dados_pedidos-menge GT vl_tot_menge.
      v_erro = 'X'.
      MESSAGE s024(sd) WITH TEXT-002 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE route FROM ekpv INTO v_route2
    WHERE ebeln = wa_dados_pedidos-ebeln
    AND   ebelp = wa_dados_pedidos-ebelp.

    IF v_route1 NE v_route2.
      v_erro = 'X'.
      MESSAGE s024(sd) WITH TEXT-004 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM ekpo INTO w_ekpo2
        WHERE ebeln = wa_dados_pedidos-ebeln
        AND   ebelp = wa_dados_pedidos-ebelp.

    IF  w_ekpo1-inco1 NE  w_ekpo2-inco1 OR
        w_ekpo1-inco2 NE  w_ekpo2-inco2.
      v_erro = 'X'.
      MESSAGE s024(sd) WITH TEXT-005 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM eket INTO w_eket2
      WHERE ebeln = wa_dados_pedidos-ebeln
      AND   ebelp = wa_dados_pedidos-ebelp.

    IF w_eket1-eindt NE w_eket2-eindt.
      v_erro = 'X'.
      MESSAGE s024(sd) WITH TEXT-006 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF v_erro = 'X'.
    EXIT.
  ENDIF.


  LOOP AT gt_dados_pedidos INTO wa_dados_pedidos WHERE NOT ebeln IS INITIAL.

    v_tabix = sy-tabix.

    SELECT SINGLE *
      FROM ekko INTO @DATA(wa_ekko)
     WHERE ebeln = @wa_dados_pedidos-ebeln
       AND bsart = 'ZUB'.

    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.

*      SELECT vbeln, posnr UP TO 1 ROWS
*         FROM lips
*         INTO @DATA(wa_lips)
*         WHERE vgbel = @wa_dados_pedidos-ebeln.
*      ENDSELECT.
*
*      IF sy-subrc = 0.
*        CONTINUE.
*      ENDIF.

      SELECT SINGLE ebeln, ebelp, matnr, werks, lgort, menge, meins
             FROM ekpo
             INTO @DATA(wa_ekpo)
             WHERE ebeln = @wa_dados_pedidos-ebeln
             AND   ebelp = @wa_dados_pedidos-ebelp.


      SELECT ebeln, ebelp, lifn2 UP TO 1 ROWS
             FROM ekpa
             INTO @DATA(wa_ekpa)
             WHERE ebeln = @wa_ekpo-ebeln
             AND   parvw  = 'PR'.
      ENDSELECT.


      SELECT ebeln, ebelp, charg UP TO 1 ROWS
             FROM eket
             INTO @DATA(wa_eket)
             WHERE ebeln = @wa_ekpo-ebeln
             AND  ebelp  = @wa_ekpo-ebelp.
      ENDSELECT.



      wa_dados_pedidos-ebeln     = wa_ekpo-ebeln.
      wa_dados_pedidos-ebelp     = wa_ekpo-ebelp.
*      wa_dados_pedidos-menge     = wa_ekpo-menge.
      wa_dados_pedidos-meins     = wa_ekpo-meins.
      wa_dados_pedidos-matnr     = wa_ekpo-matnr.
      wa_dados_pedidos-ped_werks = wa_ekpo-werks.
      wa_dados_pedidos-lgort     = wa_ekpo-lgort.

      "    Transformar XPED-RESWR é um código de fornecedor
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekpo-werks
        IMPORTING
          output = wa_dados_pedidos-ped_entrega.


      wa_dados_pedidos-charg      = wa_eket-charg.
      wa_dados_pedidos-ped_coleta = wa_ekpa-lifn2.
      MODIFY gt_dados_pedidos FROM wa_dados_pedidos INDEX v_tabix.

    ENDIF.

  ENDLOOP.

  CLEAR: t_retorno[], t_items[].

  SORT gt_dados_pedidos BY ebeln ebelp.

  LOOP AT gt_dados_pedidos INTO wa_dados_pedidos WHERE NOT ebeln IS INITIAL.


    CLEAR w_itens.
    w_itens-ref_doc        = wa_dados_pedidos-ebeln.
    w_itens-ref_item       = wa_dados_pedidos-ebelp.
    w_itens-dlv_qty        = wa_dados_pedidos-menge.
    w_itens-sales_unit     = wa_dados_pedidos-meins.
    w_itens-sales_unit_iso = wa_dados_pedidos-meins .
    APPEND w_itens TO t_itens.

    wa_dados_processa_po-ebeln       = wa_dados_pedidos-ebeln.
    wa_dados_processa_po-ebelp       = wa_dados_pedidos-ebelp.
    wa_dados_processa_po-menge       = wa_dados_pedidos-menge.
    wa_dados_processa_po-meins       = wa_dados_pedidos-meins.
    wa_dados_processa_po-matnr       = wa_dados_pedidos-matnr.
    wa_dados_processa_po-ped_werks   = wa_dados_pedidos-ped_werks.
    wa_dados_processa_po-ped_entrega = wa_dados_pedidos-ped_entrega.
    wa_dados_processa_po-lgort       = wa_dados_pedidos-lgort.
    wa_dados_processa_po-charg       = wa_dados_pedidos-charg.
    wa_dados_processa_po-ped_coleta  = wa_dados_pedidos-ped_coleta.
    APPEND wa_dados_processa_po TO gt_dados_processa_po.

    wa_dados_po = wa_dados_pedidos.
  ENDLOOP.

  CLEAR v_deliv_numb.
  IF t_itens[] IS NOT INITIAL.

    v_due_date = sy-datum.

    CLEAR: t_retorno[].
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_STO'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        due_date          = v_due_date
      IMPORTING
        delivery          = v_deliv_numb
      TABLES
        stock_trans_items = t_itens
        return            = t_retorno
        deliveries        = t_items.

    READ TABLE t_retorno WITH KEY type = 'S'.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "Forçar a liberação do documento gerado
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.

      CLEAR:  t_retorno2[], header_partner[], item_data[], item_control[], item_data_spl[].

      wa_dados_remessa-rem_vbeln   =  v_deliv_numb.
      wa_dados_remessa-ped_ebeln   =  wa_dados_po-ebeln.
      wa_dados_remessa-rem_brgew   =  wa_dados_po-menge.
      APPEND wa_dados_remessa TO gt_dados_remessa.

      w_header_data-deliv_numb     = v_deliv_numb.
      w_header_control-deliv_numb  = 'X'.

      header_partner-upd_mode_partn = 'I'.
      header_partner-deliv_numb     = v_deliv_numb.
      header_partner-itm_number     = '000010'.
      header_partner-partn_role     = 'PC'.
      header_partner-partner_no     = wa_dados_po-ped_coleta.
      APPEND header_partner.

      header_partner-upd_mode_partn = 'I'.
      header_partner-deliv_numb     = v_deliv_numb.
      header_partner-itm_number     = '000010'.
      header_partner-partn_role     = 'LR'.
      header_partner-partner_no     = wa_dados_po-ped_entrega.
      APPEND header_partner.

      LOOP AT gt_dados_processa_po INTO wa_dados_processa_po.

        item_data-deliv_numb          = v_deliv_numb.

        SELECT SINGLE posnr
          FROM lips
          INTO  item_data-deliv_item
          WHERE vbeln = v_deliv_numb
          AND   vgbel = wa_dados_processa_po-ebeln
          AND   vgpos = wa_dados_processa_po-ebelp.

        item_data-batch               = wa_dados_processa_po-charg.
        item_data-dlv_qty             = wa_dados_processa_po-menge.
        item_data-dlv_qty_imunit      = wa_dados_processa_po-menge.
        item_data-fact_unit_nom       = 1.
        item_data-fact_unit_denom     = 1.
        item_data-gross_wt            = wa_dados_processa_po-menge.
        item_data-net_weight          = wa_dados_processa_po-menge.
        APPEND item_data.
        "
        item_control-deliv_numb       = v_deliv_numb.
        item_control-deliv_item       = item_data-deliv_item.
        item_control-chg_delqty       = 'X'.
        item_control-volume_flg       = 'X'.
        item_control-net_wt_flg       = 'X'.
        item_control-gross_wt_flg     = 'X'.
        APPEND item_control.
        "
        item_data_spl-deliv_numb      = v_deliv_numb.
        item_data_spl-deliv_item      = item_data-deliv_item.
        item_data_spl-stge_loc        = wa_dados_processa_po-lgort.
        APPEND item_data_spl.
      ENDLOOP.


      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = w_header_data
          header_control = w_header_control
          delivery       = v_deliv_numb
        TABLES
          header_partner = header_partner
          item_data      = item_data
          item_control   = item_control
          return         = t_retorno2
          item_data_spl  = item_data_spl.


      nlinhas = 0.
      DESCRIBE TABLE t_retorno2 LINES nlinhas.

      IF nlinhas IS INITIAL.
        CLEAR: tl_vbpok,sl_vbpok.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        LOOP AT gt_dados_processa_po INTO wa_dados_processa_po.
          sl_vbkok_wa-vbeln_vl    = v_deliv_numb.
          sl_vbkok_wa-vbeln       = v_deliv_numb.
          sl_vbkok_wa-wabuc       = 'X'.
          sl_vbkok_wa-wadat_ist   = sy-datum. "data atual .
          sl_vbpok-vbeln_vl       = v_deliv_numb.

          SELECT SINGLE posnr
          FROM lips
          INTO  sl_vbpok-posnr_vl
          WHERE vbeln = v_deliv_numb
          AND   vgbel = wa_dados_processa_po-ebeln
          AND   vgpos = wa_dados_processa_po-ebelp.

          sl_vbpok-vbeln          = v_deliv_numb.
          sl_vbpok-posnn          = sl_vbpok-posnr_vl.

          sl_vbpok-matnr          = wa_dados_processa_po-matnr.
          sl_vbpok-pikmg          = wa_dados_processa_po-menge.
          sl_vbpok-charg          = wa_dados_processa_po-charg.
          sl_vbpok-lgort          = wa_dados_processa_po-lgort.

          sl_vbpok-brgew          = wa_dados_processa_po-menge.
          sl_vbpok-ntgew          = wa_dados_processa_po-menge.
          sl_vbpok-gewei          = 'KG'.
          sl_vbpok-vbtyp_n        = 'V'.
          APPEND sl_vbpok TO tl_vbpok.
        ENDLOOP.

        CLEAR: tl_prot.
        CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
          EXPORTING
            vbkok_wa                 = sl_vbkok_wa
            synchron                 = 'X'
            if_error_messages_send_1 = 'X'
          TABLES
            vbpok_tab                = tl_vbpok
            prot                     = tl_prot.

        IF NOT tl_prot[] IS INITIAL.
          v_erro = 'X'.
        ENDIF.

        IF v_erro IS INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          "Forçar a liberação do documento gerado
          CALL FUNCTION 'DEQUEUE_ALL'
            EXPORTING
              _synchron = 'X'.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          PERFORM f_apaga_delivery USING v_deliv_numb.
          DELETE gt_dados_remessa WHERE rem_vbeln EQ v_deliv_numb.

          CLEAR:  t_return.
          LOOP AT tl_prot INTO sl_prot.

            w_return-type    = sl_prot-msgty.
            w_return-id      = sl_prot-msgid.
            w_return-number  = sl_prot-msgno.
            w_return-message_v1 = sl_prot-msgv1.
            w_return-message_v2 = sl_prot-msgv2.
            w_return-message_v3 = sl_prot-msgv3.
            w_return-message_v4 = sl_prot-msgv4.

            APPEND  w_return TO t_return.
            CLEAR: w_return .
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = sl_prot-msgid
                msgnr               = sl_prot-msgno
                msgv1               = sl_prot-msgv1
                msgv2               = sl_prot-msgv2
                msgv3               = sl_prot-msgv3
                msgv4               = sl_prot-msgv4
              IMPORTING
                message_text_output = wa_log_remessa-message.

            wa_log_remessa-ebeln    = wa_dados_po-ebeln.
            wa_log_remessa-tipo_msg = sl_prot-msgty.
            wa_log_remessa-number   = sl_prot-msgno.

            APPEND wa_log_remessa TO gt_log_remessa.
            CLEAR wa_log_remessa.

          ENDLOOP.
          IF t_return[] IS NOT INITIAL.

*              w_alv_transf-rem_vbeln = ''.
*              MODIFY t_alv_transf FROM w_alv_transf INDEX p_index.
*
            v_vgpos = wa_dados_po-ebelp.
            zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = v_deliv_numb
                                                                                      i_ebeln      = wa_dados_po-ebeln
                                                                                      i_ebelp      = v_vgpos
                                                                                      i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                      i_commit     = abap_true
                                                                            CHANGING  t_return     = t_return[] ).

*              MESSAGE s024(sd) WITH TEXT-148 DISPLAY LIKE 'E'.
*              EXIT.

          ENDIF.
        ENDIF.

      ELSE.

        LOOP AT t_retorno2 INTO DATA(wa_retorno2).
          wa_log_remessa-ebeln    = wa_dados_po-ebeln.
          wa_log_remessa-tipo_msg = wa_retorno2-type.
          wa_log_remessa-number   = wa_retorno2-number.

          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = wa_retorno2-id
              msgnr               = wa_retorno2-number
              msgv1               = wa_retorno2-message_v1
              msgv2               = wa_retorno2-message_v2
              msgv3               = wa_retorno2-message_v3
              msgv4               = wa_retorno2-message_v4
            IMPORTING
              message_text_output = wa_log_remessa-message.

          APPEND wa_log_remessa TO gt_log_remessa.
          CLEAR wa_log_remessa.
        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        v_vgpos = wa_dados_po-ebelp.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = v_deliv_numb
                                                                                  i_ebeln      = wa_dados_po-ebeln
                                                                                  i_ebelp      = v_vgpos
                                                                                  i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_retorno2[] ).
        PERFORM f_apaga_delivery USING v_deliv_numb.
        DELETE gt_dados_remessa WHERE rem_vbeln EQ v_deliv_numb.
*          MESSAGE s024(sd) WITH TEXT-148 DISPLAY LIKE 'E'.
*          EXIT.



      ENDIF.

    ELSE.

      LOOP AT t_retorno INTO DATA(wa_retorno).
        wa_log_remessa-ebeln    = wa_dados_po-ebeln.
        wa_log_remessa-tipo_msg = wa_retorno-type.
        wa_log_remessa-number   = wa_retorno-number.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = wa_retorno-id
            msgnr               = wa_retorno-number
            msgv1               = wa_retorno-message_v1
            msgv2               = wa_retorno-message_v2
            msgv3               = wa_retorno-message_v3
            msgv4               = wa_retorno-message_v4
          IMPORTING
            message_text_output = wa_log_remessa-message.

        APPEND wa_log_remessa TO gt_log_remessa.
        CLEAR wa_log_remessa.
      ENDLOOP.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      v_vgpos = wa_dados_po-ebelp.
      zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = v_deliv_numb
                                                                                i_ebeln      = wa_dados_po-ebeln
                                                                                i_ebelp      = v_vgpos
                                                                                i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                i_commit     = abap_true
                                                                      CHANGING  t_return     = t_retorno[] ).
*        MESSAGE s024(sd) WITH TEXT-148 DISPLAY LIKE 'E'.
*        EXIT.
    ENDIF.

  ENDIF.

  CLEAR: gt_dados_processa_po[].




ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_apaga_delivery
*&---------------------------------------------------------------------*
FORM f_apaga_delivery  USING    p_v_deliv_numb  TYPE bapishpdelivnumb-deliv_numb.

  sl_hdata-deliv_numb = p_v_deliv_numb.
  sl_hcont-deliv_numb = p_v_deliv_numb.
  sl_hcont-dlv_del    = 'X'.
  vl_delivery         = p_v_deliv_numb.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    TABLES
      return         = tl_bapiret2.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valida_qtd
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_valida_qtd CHANGING vl_valida_qtd TYPE string.

  DATA: vl_tot_menge  TYPE ekpo-menge.

  LOOP AT gt_dados_pedidos INTO wa_dados_pedidos WHERE NOT ebeln IS INITIAL.

    SELECT SINGLE menge
           FROM ekpo
           INTO @DATA(vl_ekpo_menge)
           WHERE ebeln EQ @wa_dados_pedidos-ebeln
           AND   ebelp EQ @wa_dados_pedidos-ebelp.
    IF sy-subrc NE 0.
      vl_valida_qtd = TEXT-003.
    ENDIF.

    SELECT SUM( menge )
           FROM ekbe
           INTO @DATA(vl_ekbe_menge)
           WHERE ebeln EQ @wa_dados_pedidos-ebeln
           AND   ebelp EQ @wa_dados_pedidos-ebelp
           AND   vgabe EQ '8'.

    vl_tot_menge = vl_ekpo_menge - vl_ekbe_menge.
    IF wa_dados_pedidos-menge GT vl_tot_menge.
      vl_valida_qtd = TEXT-002.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibi_popup_erro
*&---------------------------------------------------------------------*
FORM f_exibi_popup_erro .

  CHECK gt_log_remessa[] IS NOT INITIAL.

*-----------------------------
* colunas alv
*-----------------------------
  PERFORM f_preenche_fcat USING :
   '01' ''          ''            'GT_LOG_REMESSA'  'EBELN'      'Pedido'        '12'     ''    ''     ''    '' '' ' ',
   '02' ''          ''            'GT_LOG_REMESSA'  'TIPO_MSG'   'Tipo de Msg'   '10'     ''    ''     ''    '' '' ' ',
   '03' ''          ''            'GT_LOG_REMESSA'  'NUMBER'     'Nº Msg'        '08'     ''    ''     ''    '' '' ' ',
   '04' ''          ''            'GT_LOG_REMESSA'  'MESSAGE'    'Desc.Msg'      '150'    ''    ''     ''    '' '' ' '.


*-----------------------------
* layout
*-----------------------------
  ls_variant-report = sy-repid && 'XXX'.
  l_grid_title      = 'Log de Erros'.

*-----------------------------
* exibe alvv
*-----------------------------
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = it_fieldcat[]
*     it_sort               = t_sort[]
*     i_callback_user_command = 'USER_COMMAND_COMPRO'
      i_grid_title          = l_grid_title
      i_save                = 'X'
      is_variant            = ls_variant
      i_screen_start_column = 40
      i_screen_start_line   = 08
      i_screen_end_column   = 145
      i_screen_end_line     = 18
    TABLES
      t_outtab              = gt_log_remessa.

*  DATA: linha_selecionada TYPE slis_selfield,
*        _exit             TYPE c.
*
*  IF ( gt_log_remessa IS NOT INITIAL ).
*
*    DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(
*
*    ( fieldname = 'EBELN           '        seltext_m = 'Pedido'        outputlen = '12' )
*    ( fieldname = 'TIPO_MSG        '        seltext_m = 'Tipo de msg '  outputlen = '10' )
*    ( fieldname = 'NUMBER          '        seltext_m = 'Nº msg      '  outputlen = '08' )
*    ( fieldname = 'MESSAGE         '        seltext_m = 'Desc.msg    '  outputlen = '150' ) ).
*
*    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
*      EXPORTING
*        i_title     = 'Log de processamento'
*        i_selection = 'X'
*        i_tabname   = 'gt_log_remessa'
*        i_zebra     = 'X'
*        it_fieldcat = tl_fieldcat
*      IMPORTING
*        es_selfield = linha_selecionada
*        e_exit      = _exit
*      TABLES
*        t_outtab    = gt_log_remessa.
*  ENDIF.

ENDFORM.

FORM f_preenche_fcat   USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox)
                              VALUE(p_icon).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-icon          = p_icon.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat.

ENDFORM.
