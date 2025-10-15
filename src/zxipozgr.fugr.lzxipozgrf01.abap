*----------------------------------------------------------------------*
***INCLUDE LZXIPOZGRF01 .
*----------------------------------------------------------------------*

FORM yf_executa_pedido_sigam TABLES pt_zmmpozgr       STRUCTURE zmmt_po_zgr.

  DATA: lf_reg_ok.

* Classifica documento de pedido
  SORT pt_zmmpozgr BY obj_key.

*--------------------------------------------------------------------------------------------------------------*
*  Checar duplicidade Pedido
*--------------------------------------------------------------------------------------------------------------*
  DATA(pt_zmmpozgr_aux) = pt_zmmpozgr[].

  LOOP AT pt_zmmpozgr_aux INTO DATA(wl_po_aux) WHERE zst_atlz EQ 'I'.
    SELECT SINGLE *
      FROM zmmt_po_zgr INTO @DATA(wl_pedido_key)
     WHERE obj_key EQ @wl_po_aux-obj_key.

    IF ( sy-subrc EQ 0 ) AND ( wl_pedido_key-ebeln IS NOT INITIAL ) AND ( wl_po_aux-obj_key IS NOT INITIAL ).
      CONCATENATE 'Pedido:' wl_pedido_key-ebeln 'já criado para a chave de referência:' wl_po_aux-obj_key
            INTO DATA(_msg_pedido_dupl)  SEPARATED BY space .

      APPEND VALUE #( obj_key         = wl_po_aux-obj_key
                      interface       = '08'
                      dt_atualizacao  = sy-datum
                      hr_atualizacao  = sy-uzeit
                      id              = 'MM'
                      type            = 'E'
                      num             = '999'
                      message         = _msg_pedido_dupl
                      message_v1      = wl_pedido_key-ebeln ) TO yt_log_pozgr.

      DELETE pt_zmmpozgr WHERE obj_key = wl_po_aux-obj_key.
    ENDIF.
  ENDLOOP.


* Lê sequencialmente a tabela recebida pelo SIGAM
  LOOP AT pt_zmmpozgr.

*   Salva área de dados antes da quebra
    w_zmmt_po_zgr = pt_zmmpozgr.

*   Inicializa tabelas da bapi
    AT NEW obj_key.

      REFRESH: yt_poitem,     yt_poitemx,
               yt_poschedule, yt_poschedulex,
               yt_return,     yt_poaccount,
               yt_poaccountx.

      CLEAR:   yt_poitem     ,yt_poitemx,
               yt_poschedule ,yt_poschedulex,
               yt_poaccount  ,yt_poaccountx ,
               yt_return     ,vg_erro, vg_no_price_from_po.

*     Salva dados de controle
      CLEAR vn_ebelp.
      vc_obj_key = w_zmmt_po_zgr-obj_key.
      vc_purchaseorder = w_zmmt_po_zgr-ebeln.

*     Dados do Header do Pedido
      IF w_zmmt_po_zgr-zst_atlz = cc_i.
        PERFORM yf_movimenta_dados_bapi USING cc_x.
      ENDIF.

    ENDAT.

*   Dados do Item do Pedido
    PERFORM yf_movimenta_dados_bapi USING space.

*   Processa registros de pedido
    AT END OF obj_key.

*     Bloqueia o registro para processamento
      CALL FUNCTION 'ENQUEUE_EZMMT_PO_ZGR'
        EXPORTING
          obj_key        = vc_obj_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CHECK: sy-subrc IS INITIAL.
      IF vg_erro NE cc_x.
*     Determina o processamento
        IF w_zmmt_po_zgr-zst_atlz = cc_i.

          PERFORM yf_bapi_po_create1 CHANGING w_expheader.

        ELSEIF w_zmmt_po_zgr-zst_atlz = cc_a.

          PERFORM yf_bapi_po_change USING vc_purchaseorder.

        ENDIF.
      ENDIF.

*     Gera log de execução para o retorno da interface
      PERFORM yf_gera_log_execucao CHANGING lf_reg_ok.

*     Atualiza status de registros processados
      PERFORM yf_atlzstatus_zmmt_po_zgr USING lf_reg_ok.

*     Desbloqueia o registro processado
      CALL FUNCTION 'DEQUEUE_EZMMT_PO_ZGR'
        EXPORTING
          obj_key = vc_obj_key.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " YF_EXECUTA_PEDIDO_SIGAM

*&---------------------------------------------------------------------*
*&      Form  YF_MOVIMENTA_DADOS_BAPI
*&---------------------------------------------------------------------*
FORM yf_movimenta_dados_bapi USING VALUE(p_header).
  DATA : vl_zona_o TYPE lfa1-lzone,
         vl_zona_d TYPE lfa1-lzone,
         vl_lifnr  TYPE lfa1-lifnr,
         vl_kalnr  TYPE ckmlhd-kalnr,
         vl_ano    TYPE ckmlcr-bdatj,
         vl_mes    TYPE ckmlcr-poper.

  DATA: v_ano_int     TYPE i,
        v_centro_lote TYPE zsdt_depara_cen-centro_real.

  IF p_header = cc_x.

*   Dados de cabeçalho da BAPI
    w_poheader-comp_code   = w_zmmt_po_zgr-bukrs.
    w_poheader-doc_type    = w_zmmt_po_zgr-bsart.
    w_poheader-vendor      = w_zmmt_po_zgr-lifnr.
    w_poheader-pmnttrms    = w_zmmt_po_zgr-zterm.
    w_poheader-purch_org   = w_zmmt_po_zgr-ekorg.
    w_poheader-pur_group   = w_zmmt_po_zgr-ekgrp.
    w_poheader-currency    = w_zmmt_po_zgr-waers.
    IF NOT w_zmmt_po_zgr-bedat IS INITIAL.
      w_poheader-creat_date  = w_zmmt_po_zgr-bedat.
    ENDIF.
    w_poheader-collect_no  = w_zmmt_po_zgr-collect_no. "*-CS2022000332-#78064-07.06.2022-JT-inicio
    w_poheader-incoterms1  = 'CIF'.
    w_poheader-incoterms2  = 'CIF'.

    w_poheaderx-comp_code  = cc_x.
    w_poheaderx-doc_type   = cc_x.
    w_poheaderx-vendor     = cc_x.
    w_poheaderx-pmnttrms   = cc_x.
    w_poheaderx-purch_org  = cc_x.
    w_poheaderx-pur_group  = cc_x.
    w_poheaderx-currency   = cc_x.
    w_poheaderx-collect_no = cc_x. "*-CS2022000332-#78064-07.06.2022-JT-inicio
    w_poheaderx-incoterms1 = cc_x.
    w_poheaderx-incoterms2 = cc_x.
    IF NOT w_zmmt_po_zgr-bedat IS INITIAL.
      w_poheaderx-creat_date = cc_x.
    ENDIF.
    IF w_zmmt_po_zgr-bsart EQ 'ZUB'.
      w_poheaderx-suppl_plnt = cc_x.
      w_poheader-suppl_plnt = w_zmmt_po_zgr-werks.
    ENDIF.

    EXIT.

  ENDIF.

  vn_ebelp = 0.
  "LOOP AT IT_ZMMPO_ITEM_ZGR INTO W_ZMMPO_ITEM_ZGR WHERE OBJ_KEY = W_ZMMT_PO_ZGR-OBJ_KEY .
  ADD 10 TO vn_ebelp.
* Movimenta dados do Item
  yt_poitem-po_item   = vn_ebelp.
  "yt_poitem-po_item   = w_zmmt_po_zgr-ebelp.

*--> Inicio Migração FP - 06/07/2023
*  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*    EXPORTING
*      input  = w_zmmt_po_zgr-matnr
*      "INPUT  = W_ZMMPO_ITEM_ZGR-MATNR
*    IMPORTING
*      output = yt_poitem-material.

  DATA(lv_tam) = strlen( w_zmmt_po_zgr-matnr ).
  IF lv_tam > 18.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = w_zmmt_po_zgr-matnr
        "INPUT  = W_ZMMPO_ITEM_ZGR-MATNR
      IMPORTING
        output = yt_poitem-material_long.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = w_zmmt_po_zgr-matnr
        "INPUT  = W_ZMMPO_ITEM_ZGR-MATNR
      IMPORTING
        output = yt_poitem-material.
  ENDIF.

*--> Fim Migração FP - 06/07/2023

  yt_poitem-plant     = w_zmmt_po_zgr-werks.
  yt_poitem-stge_loc  = w_zmmt_po_zgr-lgort.
  yt_poitem-quantity  = w_zmmt_po_zgr-menge.
  "YT_POITEM-QUANTITY  = W_ZMMPO_ITEM_ZGR-MENGE.
  yt_poitem-po_unit   = w_zmmt_po_zgr-meins.

  SELECT SINGLE * INTO @DATA(wa_t001)
    FROM t001
   WHERE bukrs EQ @w_zmmt_po_zgr-bukrs.

  IF w_zmmt_po_zgr-bsart NE 'ZUB'.

    vg_no_price_from_po = 'X'.

    IF  wa_t001-waers EQ w_zmmt_po_zgr-waers..
      SELECT SINGLE stprs
        INTO yt_poitem-net_price
        FROM mbew
       WHERE matnr = yt_poitem-material
         AND bwkey = w_zmmt_po_zgr-werks.
    ELSE.
      "--------------------------
      SELECT SINGLE kalnr
        INTO vl_kalnr
        FROM ckmlhd
       WHERE matnr = yt_poitem-material
         AND bwkey = w_zmmt_po_zgr-werks.

      "      Pegar o valor do campo CKMLHD-KALNR
      "      Selecionar na tabela CKMLCR conforme abaixo :

      DATA(ck_preco) = abap_false.
      vl_ano = sy-datum(4)."ano
      vl_mes = sy-datum+4(2)."MES DA DATA DO SISTEMA.
      DATA(ck_meses_busca) = 0.

      WHILE ck_preco = abap_false AND ck_meses_busca LE 12.

        ADD 1 TO ck_meses_busca.

        IF vl_mes = 001.
          vl_ano = vl_ano - 1.
          vl_mes = 012.
        ELSE.
          vl_mes = vl_mes - 1.
        ENDIF.

* ---> S4 Migration - 06/07/2023 - FP

*        SELECT SINGLE stprs
*          INTO yt_poitem-net_price
*          FROM ckmlcr
*         WHERE kalnr = vl_kalnr
*           AND bdatj = vl_ano
*           AND poper = vl_mes
*           AND waers = w_zmmt_po_zgr-waers ."MOEDA DO PEDIDO.

        DATA: lt_ckmlcr  TYPE TABLE OF ckmlcr,
              lv_bdatj_1 TYPE  ckmlpp-bdatj,
              lv_poper_1 TYPE  ckmlpp-poper,
              lv_jahrper TYPE  mldoc-jahrper,
              lt_kalnr   TYPE ckmv0_matobj_tbl.

        IF vl_kalnr IS NOT INITIAL.

          lv_bdatj_1 = vl_ano.
          lv_poper_1 = vl_mes.
          APPEND INITIAL LINE TO lt_kalnr ASSIGNING FIELD-SYMBOL(<fs_kalnr>).
          <fs_kalnr>-kalnr = vl_kalnr.

          CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
            EXPORTING
              i_bdatj_1               = lv_bdatj_1
              i_poper_1               = lv_poper_1
            TABLES
              t_kalnr                 = lt_kalnr
              t_ckmlcr                = lt_ckmlcr
            EXCEPTIONS
              no_data_found           = 1
              input_data_inconsistent = 2
              buffer_inconsistent     = 3
              OTHERS                  = 4.

        ENDIF.

        DELETE lt_ckmlcr WHERE waers NE w_zmmt_po_zgr-waers.

*        IF sy-subrc IS INITIAL.
        IF lt_ckmlcr IS NOT INITIAL.
* <--- S4 Migration - 06/07/2023  - FP
          yt_poitem-net_price = lt_ckmlcr[ 1 ]-stprs.
          ck_preco = abap_true.
        ENDIF.

      ENDWHILE.

    ENDIF.

  ELSE.
    yt_poitem-plant      = w_zmmt_po_zgr-werks_d.
    yt_poitem-incoterms1 = 'CIF'.
    yt_poitem-incoterms2 = 'CIF'.

    yt_poitemx-incoterms1 = cc_x.
    yt_poitemx-incoterms2 = cc_x.

  ENDIF.

  IF w_zmmt_po_zgr-net_price IS NOT INITIAL.
    yt_poitem-net_price = w_zmmt_po_zgr-net_price.
    vg_no_price_from_po = 'X'.
  ENDIF.

  yt_poitem-tax_code  = w_zmmt_po_zgr-mwskz.
  yt_poitem-final_inv = w_zmmt_po_zgr-elikz.
  IF sy-tcode EQ 'ZMM0127'.
    yt_poitem-conf_ctrl = '0003'.
  ELSE.
    yt_poitem-conf_ctrl = '0003'.
  ENDIF.

  yt_poitem-batch     = w_zmmt_po_zgr-charg.

*  CASE YT_POITEM-MATERIAL.
*    WHEN '000000000000119892' OR '000000000000119895'.
  CASE w_zmmt_po_zgr-bukrs.
    WHEN '0100'.
      yt_poitem-batch = yt_poitem-batch && 'A'.
    WHEN '0101'.
      yt_poitem-batch = yt_poitem-batch && 'P'.
    WHEN '0015'.
    WHEN '0050'. "BUG 182008
    WHEN OTHERS.
      v_ano_int = w_zmmt_po_zgr-charg.

      IF v_ano_int >= 2020.
        CLEAR: v_centro_lote.

        SELECT SINGLE *
          FROM zsdt_depara_cen INTO @DATA(wl_dep_cen)
         WHERE centrov_1 EQ @yt_poitem-plant.

        IF ( sy-subrc EQ 0 ) AND ( yt_poitem-plant IS NOT INITIAL ).
          v_centro_lote = wl_dep_cen-centro_real.
          yt_poitem-batch = yt_poitem-batch && '_' && v_centro_lote.
        ELSE.
          yt_return-type    = 'E'.
          yt_return-message = 'Depara centro a fixar não encontrado:' && yt_poitem-plant.
          yt_return-id      = 'ME'.
          yt_return-number  = '999'.
          APPEND yt_return.
          vg_erro = cc_x.
        ENDIF.

      ENDIF.

  ENDCASE.
*  ENDCASE.


  yt_poitem-acctasscat = w_zmmt_po_zgr-acctasscat.
  "IF W_ZMMT_PO_ZGR-BUKRS EQ '0100' OR W_ZMMT_PO_ZGR-BUKRS EQ '0101'.
  yt_poitem-gr_basediv = ''.
  "ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = yt_poitem-conf_ctrl
    IMPORTING
      output = yt_poitem-conf_ctrl.

  yt_poitemx-po_item    = vn_ebelp.
  yt_poitemx-po_itemx   = cc_x.
*--> Fim Migração FP - 06/07/2023

*  yt_poitemx-material   = cc_x.

  IF lv_tam > 18.
    yt_poitemx-material_long  = cc_x.
  ELSE.
    yt_poitemx-material   = cc_x.
  ENDIF.
*<-- Fim Migração FP - 06/07/2023
  yt_poitemx-plant      = cc_x.
  yt_poitemx-stge_loc   = cc_x.
  yt_poitemx-quantity   = cc_x.
  yt_poitemx-po_unit    = cc_x.
  yt_poitemx-net_price  = cc_x.
  yt_poitemx-tax_code   = cc_x.
  yt_poitemx-final_inv  = w_zmmt_po_zgr-elikz.
  yt_poitemx-conf_ctrl  = cc_x.
  yt_poitemx-batch      = cc_x.
  yt_poitemx-acctasscat = cc_x.

  "IF W_ZMMT_PO_ZGR-BUKRS EQ '0100' OR W_ZMMT_PO_ZGR-BUKRS EQ '0101'.
  yt_poitemx-gr_basediv = cc_x.
  "ENDIF.

* Tratativa para o tipo de ação a ser efetivada
  IF w_zmmt_po_zgr-zst_atlz = cc_i.

    CLEAR: yt_poitemx-final_inv.

  ELSEIF w_zmmt_po_zgr-zst_atlz = cc_a.

    CLEAR: yt_poitemx-material,
           yt_poitemx-stge_loc.

    IF yt_poitem-quantity IS INITIAL.
      yt_poitemx-quantity = space.
    ENDIF.
    IF yt_poitem-po_unit IS INITIAL.
      yt_poitemx-po_unit = space.
    ENDIF.
    IF yt_poitem-plant IS INITIAL.
      yt_poitemx-plant = space.
    ENDIF.
    IF yt_poitem-final_inv IS INITIAL.
      yt_poitemx-final_inv = space.
    ENDIF.

  ENDIF.

  APPEND: yt_poitem,
          yt_poitemx.


  IF w_zmmt_po_zgr-bsart NE 'ZUB' AND w_zmmt_po_zgr-bsart NE 'ZGF'.
    ADD 10 TO vn_ebelp.
    yt_poitem-po_item  = vn_ebelp. "yt_poitem-po_item + 10.
    yt_poitem-ret_item = cc_x.

    yt_poitemx-po_item  = vn_ebelp. "yt_poitemx-po_item + 10.
    yt_poitemx-ret_item = cc_x.

    APPEND: yt_poitem,
            yt_poitemx.
  ENDIF.


*** Movimenta dados de schedulagem
**  yt_poschedule-po_item       = vn_ebelp.
**  yt_poschedule-delivery_date = w_zmmt_po_zgr-eindt.
**  yt_poschedule-quantity      = w_zmmt_po_zgr-menge.

  IF w_zmmt_po_zgr-gl_account IS NOT INITIAL.

    yt_poaccount-po_item    = yt_poitem-po_item.
    yt_poaccount-serial_no  = '01'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zmmt_po_zgr-gl_account
      IMPORTING
        output = yt_poaccount-gl_account.

    yt_poaccount-quantity   = 1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zmmt_po_zgr-costcenter
      IMPORTING
        output = yt_poaccount-costcenter.

    yt_poaccount-co_area    = w_zmmt_po_zgr-co_area.

    APPEND yt_poaccount.

    yt_poaccountx-po_item    = yt_poitem-po_item.
    yt_poaccountx-serial_no  = '01'.
    yt_poaccountx-gl_account = cc_x.
    yt_poaccountx-quantity   = cc_x.
    yt_poaccountx-costcenter = cc_x.
    yt_poaccountx-co_area    = cc_x.

    APPEND yt_poaccountx.
    "  BAPIMEPOACCOUNT
  ENDIF.

  IF w_zmmt_po_zgr-bsart EQ 'ZUB'.
    yt_poitemx-conf_ctrl   = space.
    yt_poitem-conf_ctrl    = space."Para o tipo de pedido zub não gera aviso de recebimento.

    yt_poschedulex-po_item       = vn_ebelp.
    yt_poschedulex-po_itemx      = cc_x.
    yt_poschedulex-delivery_date = cc_x.
    yt_poschedulex-quantity      = cc_x.

    yt_partner-partnerdesc = 'PR'.
    yt_partner-langu       = 'P'.
    yt_partner-buspartno   = w_zmmt_po_zgr-lifnr.
    APPEND yt_partner.



    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zmmt_po_zgr-werks_d
      IMPORTING
        output = vl_lifnr.


    SELECT SINGLE lzone INTO vl_zona_o FROM lfa1 WHERE lifnr = w_zmmt_po_zgr-lifnr.

    SELECT SINGLE lzone INTO vl_zona_d FROM lfa1 WHERE lifnr = vl_lifnr.

    SELECT SINGLE route INTO yt_poship-route FROM  trolz  WHERE azone = vl_zona_o AND lzone = vl_zona_d.

    IF yt_poship-route IS INITIAL.
      yt_return-type    = 'E'.
      yt_return-message = 'Itinerario não localizado'.
      yt_return-id      = 'ME'.
      yt_return-number  = '354'.


      APPEND yt_return.

      vg_erro = cc_x.

    ENDIF.

    yt_poship-po_item = yt_poitem-po_item.

    APPEND yt_poship.

    yt_poshipx-po_item = yt_poitem-po_item.
    yt_poshipx-route   = cc_x.

    APPEND yt_poshipx.


  ENDIF.

  "ENDLOOP.
**
**  APPEND: yt_poschedule,
**          yt_poschedulex.

ENDFORM.                    " YF_MOVIMENTA_DADOS_BAPI

*&---------------------------------------------------------------------*
*&      Form  YF_RETORNA_LOG_SIGAM
*&---------------------------------------------------------------------*
FORM yf_retorna_log_sigam USING i_sap_unitario TYPE char01.

  CHECK: NOT yt_log_pozgr[] IS INITIAL.

  PERFORM f_add_info_adicional TABLES yt_log_pozgr.

* Transfere controle para interface XI
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_RETURN'
*    TABLES
*      outreturn = yt_log_pozgr.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outreturn = yt_log_pozgr.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outreturn = yt_log_pozgr.
  ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

  IF i_sap_unitario IS NOT INITIAL.
    DELETE yt_log_pozgr WHERE id EQ 'BAPI'.
    LOOP AT yt_log_pozgr INTO DATA(wa_log_pozgr).
      MESSAGE ID wa_log_pozgr-id TYPE wa_log_pozgr-type NUMBER wa_log_pozgr-num
         WITH wa_log_pozgr-message_v1 wa_log_pozgr-message_v2 wa_log_pozgr-message_v3 wa_log_pozgr-message_v4.
    ENDLOOP.
  ENDIF.

  COMMIT WORK.

ENDFORM.                    " YF_RETORNA_LOG_SIGAM

*&---------------------------------------------------------------------*
*&      Form  YF_BAPI_PO_CREATE1
*&---------------------------------------------------------------------*
FORM yf_bapi_po_create1  CHANGING  s_expheader TYPE bapimepoheader.
  CLEAR: s_expheader.

  CALL FUNCTION 'BAPI_PO_CREATE1' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      poheader         = w_poheader
      poheaderx        = w_poheaderx
*     POADDRVENDOR     =
*     TESTRUN          =
*     MEMORY_UNCOMPLETE            =
*     MEMORY_COMPLETE  =
*     POEXPIMPHEADER   =
*     POEXPIMPHEADERX  =
*     VERSIONS         =
*     NO_MESSAGING     =
*     NO_MESSAGE_REQ   =
*     NO_AUTHORITY     =
      no_price_from_po = vg_no_price_from_po
    IMPORTING
*     EXPPURCHASEORDER =
      expheader        = s_expheader
*     EXPPOEXPIMPHEADER            =
    TABLES
      return           = yt_return
      poitem           = yt_poitem
      poitemx          = yt_poitemx
*     POADDRDELIVERY   =
      poschedule       = yt_poschedule
      poschedulex      = yt_poschedulex
      poaccount        = yt_poaccount
*     POACCOUNTPROFITSEGMENT       =
      poaccountx       = yt_poaccountx
*     POCONDHEADER     =
*     POCONDHEADERX    =
*     POCOND           =
*     POCONDX          =
*     POLIMITS         =
*     POCONTRACTLIMITS =
*     POSERVICES       =
*     POSRVACCESSVALUES            =
*     POSERVICESTEXT   =
*     EXTENSIONIN      =
*     EXTENSIONOUT     =
*     POEXPIMPITEM     =
*     POEXPIMPITEMX    =
*     POTEXTHEADER     =
*     POTEXTITEM       =
*     ALLVERSIONS      =
      popartner        = yt_partner
*     POCOMPONENTS     =
*     POCOMPONENTSX    =
      poshipping       = yt_poship
      poshippingx      = yt_poshipx
*     POSHIPPINGEXP    =
    .

ENDFORM.                    " YF_BAPI_PO_CREATE1

*&---------------------------------------------------------------------*
*&      Form  YF_BAPI_PO_CHANGE
*&---------------------------------------------------------------------*
FORM yf_bapi_po_change  USING p_purchaseorder.

  CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      purchaseorder = p_purchaseorder
*     POHEADER      =
*     POHEADERX     =
*     POADDRVENDOR  =
*     TESTRUN       =
*     MEMORY_UNCOMPLETE            =
*     MEMORY_COMPLETE              =
*     POEXPIMPHEADER               =
*     POEXPIMPHEADERX              =
*     VERSIONS      =
*     NO_MESSAGING  =
*     NO_MESSAGE_REQ               =
*     NO_AUTHORITY  =
*     NO_PRICE_FROM_PO             =
*   IMPORTING
*     EXPHEADER     =
*     EXPPOEXPIMPHEADER            =
    TABLES
      return        = yt_return
      poitem        = yt_poitem
      poitemx       = yt_poitemx
*     POADDRDELIVERY               =
*     POSCHEDULE    =
*     POSCHEDULEX   =
*     POACCOUNT     =
*     POACCOUNTPROFITSEGMENT       =
*     POACCOUNTX    =
*     POCONDHEADER  =
*     POCONDHEADERX =
*     POCOND        =
*     POCONDX       =
*     POLIMITS      =
*     POCONTRACTLIMITS             =
*     POSERVICES    =
*     POSRVACCESSVALUES            =
*     POSERVICESTEXT               =
*     EXTENSIONIN   =
*     EXTENSIONOUT  =
*     POEXPIMPITEM  =
*     POEXPIMPITEMX =
*     POTEXTHEADER  =
*     POTEXTITEM    =
*     ALLVERSIONS   =
*     POPARTNER     =
*     POCOMPONENTS  =
*     POCOMPONENTSX =
*     POSHIPPING    =
*     POSHIPPINGX   =
*     POSHIPPINGEXP =
*     POHISTORY     =
*     POHISTORY_TOTALS             =
*     POCONFIRMATION               =
    .

ENDFORM.                    " YF_BAPI_PO_CHANGE

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_LOG_EXECUCAO
*&---------------------------------------------------------------------*
FORM yf_gera_log_execucao CHANGING pf_reg_ok.

  DATA: lc_purchaseorder LIKE bapimepoheader-po_number,
        lc_mensagem      TYPE bapi_msg.

  CLEAR: yt_log_pozgr,
         pf_reg_ok.

* Salva documento de pedido processado
  IF w_zmmt_po_zgr-zst_atlz = cc_i.
    MOVE w_expheader-po_number TO lc_purchaseorder.
    CONCATENATE TEXT-m01 lc_purchaseorder
           INTO lc_mensagem SEPARATED BY space.
  ELSEIF w_zmmt_po_zgr-zst_atlz = cc_a.
    MOVE vc_purchaseorder      TO lc_purchaseorder.
    CONCATENATE TEXT-m02 lc_purchaseorder
           INTO lc_mensagem SEPARATED BY space.
  ENDIF.

* Movimenta informações do registro processado
  MOVE: vc_obj_key             TO yt_log_pozgr-obj_key,
        '08'                   TO yt_log_pozgr-interface,
        sy-datum               TO yt_log_pozgr-dt_atualizacao,
        sy-uzeit               TO yt_log_pozgr-hr_atualizacao.

  READ TABLE yt_return INTO DATA(wa_return) WITH KEY id = '06' number = '017' type = 'S'.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING wa_return TO yt_log_pozgr.
    yt_log_pozgr-id         = wa_return-id.
    yt_log_pozgr-type       = wa_return-type.
    yt_log_pozgr-num        = wa_return-number.
    yt_log_pozgr-message    = wa_return-message.
    yt_log_pozgr-message_v1 = wa_return-message_v1.
    yt_log_pozgr-message_v2 = wa_return-message_v2.
    yt_log_pozgr-message_v3 = wa_return-message_v3.
    yt_log_pozgr-message_v4 = wa_return-message_v4.
    APPEND yt_log_pozgr.
  ENDIF.

* Movimenta mensagem de erro
  DELETE yt_return WHERE type <> cc_e.

  LOOP AT yt_return.
    MOVE-CORRESPONDING yt_return TO yt_log_pozgr.
    yt_log_pozgr-num = yt_return-number.
*   Salva o documento processado
    IF yt_log_pozgr-message_v1 IS INITIAL.
      MOVE lc_purchaseorder TO yt_log_pozgr-message_v1.
    ELSEIF yt_log_pozgr-message_v2 IS INITIAL.
      MOVE lc_purchaseorder TO yt_log_pozgr-message_v2.
    ELSEIF yt_log_pozgr-message_v3 IS INITIAL.
      MOVE lc_purchaseorder TO yt_log_pozgr-message_v3.
    ELSE.
      MOVE lc_purchaseorder TO yt_log_pozgr-message_v4.
    ENDIF.
    APPEND yt_log_pozgr.
  ENDLOOP.

* Movimenta mensagem de sucesso
  CHECK: yt_return[] IS INITIAL.

  MOVE: cc_s                TO yt_log_pozgr-type,
        cc_classe_mensagem  TO yt_log_pozgr-id,
        '000'               TO yt_log_pozgr-num,
        lc_mensagem         TO yt_log_pozgr-message,
        lc_purchaseorder    TO yt_log_pozgr-message_v1.
  APPEND yt_log_pozgr.

  pf_reg_ok = cc_x.

ENDFORM.                    " YF_GERA_LOG_EXECUCAO

*&---------------------------------------------------------------------*
*&      Form  YF_ATLZSTATUS_ZMMT_PO_ZGR
*&---------------------------------------------------------------------*
FORM yf_atlzstatus_zmmt_po_zgr  USING pf_reg_ok.

  DATA: lc_rg_atlz TYPE  zrg_atlz,
        lc_ebeln   TYPE  ebeln,
        lc_bedat   TYPE  ebdat,
        li_subrc   TYPE  sy-subrc.

  SELECT SINGLE ebeln bedat zrg_atlz
    INTO (lc_ebeln, lc_bedat, lc_rg_atlz)
    FROM zmmt_po_zgr
   WHERE obj_key = w_zmmt_po_zgr-obj_key.

  li_subrc = sy-subrc.

  IF w_zmmt_po_zgr-zst_atlz = cc_i.
    lc_ebeln = w_expheader-po_number.
    IF w_zmmt_po_zgr-bedat IS INITIAL.
      lc_bedat = sy-datum.
    ELSE.
      lc_bedat = w_zmmt_po_zgr-bedat.
    ENDIF.
  ENDIF.

  IF li_subrc IS INITIAL.
    IF lc_rg_atlz <> pf_reg_ok.

      UPDATE zmmt_po_zgr
         SET ebeln    = lc_ebeln
             bedat    = lc_bedat
             zdt_atlz = sy-datum
             zhr_atlz = sy-uzeit
             zrg_atlz = pf_reg_ok
       WHERE obj_key  = w_zmmt_po_zgr-obj_key.

    ENDIF.
  ELSE.

    w_zmmt_po_zgr-ebeln    = lc_ebeln.
    w_zmmt_po_zgr-bedat    = lc_bedat.
    w_zmmt_po_zgr-zdt_atlz = sy-datum.
    w_zmmt_po_zgr-zhr_atlz = sy-uzeit.
    w_zmmt_po_zgr-zrg_atlz = pf_reg_ok.

    INSERT INTO zmmt_po_zgr VALUES w_zmmt_po_zgr.

  ENDIF.

  CLEAR:  w_expheader,
          yt_return[],
          yt_poitem[],
          yt_poitemx[].


* Efetiva
  COMMIT WORK.

ENDFORM.                    " YF_ATLZSTATUS_ZMMT_PO_ZGR

FORM f_add_info_adicional  TABLES T_LOG_POZGR STRUCTURE ZFIE_RET_DOCUMENT.

   DATA: LWA_INFO_ADD TYPE ZSMM0001,
         lva_json_dados_add TYPE c LENGTH 30000.

   LOOP AT T_LOG_POZGR ASSIGNING FIELD-SYMBOL(<fs_log_pozgr>) WHERE TYPE = 'S' AND OBJ_KEY IS NOT INITIAL.

     CLEAR: LWA_INFO_ADD.

     SELECT SINGLE *
       FROM ZMMT_PO_ZGR INTO LWA_INFO_ADD-zmmt_po_zgr
      WHERE OBJ_KEY = <fs_log_pozgr>-obj_key.

     CHECK SY-SUBRC EQ 0.

     SELECT *
       FROM ZMMT_PO_ITEM_ZGR INTO TABLE LWA_INFO_ADD-zmmt_po_item_zgr
      WHERE OBJ_KEY = <fs_log_pozgr>-obj_key.

     lva_json_dados_add  = /ui2/cl_json=>serialize( EXPORTING data = LWA_INFO_ADD ).

     <fs_log_pozgr>-info_adicional_1 = lva_json_dados_add+0000(4000).
     <fs_log_pozgr>-info_adicional_2 = lva_json_dados_add+4000(4000).
     <fs_log_pozgr>-info_adicional_3 = lva_json_dados_add+8000(4000).


   ENDLOOP.

ENDFORM.
