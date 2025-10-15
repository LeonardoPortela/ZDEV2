class ZCL_IM_ME_PROCESS_PO_CUST definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_PROCESS_PO_CUST IMPLEMENTATION.


  METHOD if_ex_me_process_po_cust~check.

    IF sy-tcode = 'ME21N' OR sy-tcode = 'ME22N' OR sy-tcode = 'ME23N'.

      DATA(itens) = im_header->get_items( ).

      INCLUDE mm_messages_mac. "useful macros for message handling

      LOOP AT itens INTO DATA(wa_item).

        me->if_ex_me_process_po_cust~process_item( im_item = wa_item-item ).

*&============================================Comentado código abaixo devido apos a implantação S4/HANA, não tem mais RECAP / DEVK9A1RA8 / IR157481 / AOENNING
*        DATA(item) = wa_item-item->get_data( ).
*        IF item-knttp = 'A' AND item-recap IS INITIAL.
*          "Verificar se é Compra para Imobilizado
*          SELECT SINGLE * FROM j_1btxic3
*            INTO @DATA(w_j_1btxic3)
*            WHERE land1  = 'BR'
*              AND gruop  = '42'
*              AND value  = @item-j_1bnbm.
*
*          IF sy-subrc IS INITIAL.
*            ch_failed = abap_true.
*          ENDIF.
*        ELSE.
*          mmpur_remove_msg_by_context item-id 904.
*        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~close.

*    CALL FUNCTION 'MEPOBADIEX_INIT'.

    CALL FUNCTION 'ZMM_ME21N_SIGAM_SAVE'
      EXPORTING
        im_header = im_header.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  METHOD IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  METHOD if_ex_me_process_po_cust~initialize.

** initializations
*    CALL FUNCTION 'MEPOBADIEX_INIT'.



  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~open.
*    DATA: LS_MEPOHEADER TYPE MEPOHEADER.
*
*    CHECK IM_TRTYP EQ 'V' OR IM_TRTYP EQ 'A'.
*
*    LS_MEPOHEADER = IM_HEADER->GET_DATA( ).
*
*    CALL FUNCTION 'MEPOBADIEX_OPEN'
*      EXPORTING
*        IM_EBELN = LS_MEPOHEADER-EBELN.
    DATA: ls_header TYPE mepoheader.

    IF ( im_trtyp NE 'A' AND ls_header-bsart IS NOT INITIAL ).

      ls_header = im_header->get_data( ). " read PO header data

      SELECT COUNT( * )
        FROM tvarvc
        WHERE name = 'ZMM_PRICE_PO_BSART'
          AND low  = ls_header-bsart.

      IF ( sy-subrc IS INITIAL ).
        MESSAGE a006(z_mm).
      ENDIF.
    ENDIF.

    IF sy-ucomm =	'MECREA' AND sy-tcode NE 'ME21N'.
      MESSAGE a000(z_mm) WITH 'Utilizar diretamente a ME21N para criar pedido'.
    ENDIF.

    " TESTE RAMON -->
    "IF zcl_me21n_global_call=>gv_process = 'S'.


    CALL FUNCTION 'ZMM_ME21N_DADOS_SIGAM'
      EXPORTING
        im_header = im_header.


*      DATA lo_header  TYPE REF TO cl_po_header_handle_mm.
*      DATA lo_item    TYPE REF TO cl_po_item_handle_mm.
*      DATA ls_item TYPE mepoitem.
*      DATA ls_itemx TYPE mepoitemx.
*
*      lo_header ?= im_header.
*
*      ls_header = im_header->get_data( ). " read PO header data
*
*      ls_header-bsart = 'ZUB'.
*      ls_header-reswk = '0124'.
*      ls_header-ekorg = 'OC01'.
*      ls_header-ekgrp = 'G01'.
*      ls_header-bukrs = '0001'.
*
*      "ls_header-statu = '9'.
*      ls_header-aedat = sy-datum.
*      ls_header-ernam = sy-uname.
*      ls_header-pincr = '00010'.
*      ls_header-spras = sy-langu.
*      ls_header-bedat = sy-datum.
*      ls_header-upinc = '00001'.
*      ls_header-stceg_l = 'BR'.
*      "ls_header-procstat = '02'.
*      ls_header-mandt = sy-mandt.
*      ls_header-bstyp = 'F'.
*      ls_header-bsakz = 'T'.
*      ls_header-lands = 'BR'.
*
*      ls_header-id = '1'.
*
*      lo_header->my_ibs_firewall_on = 'X'.
*      lo_header->my_cust_firewall_on = 'X'.
*
*      DATA x_mmpa TYPE TABLE OF mmpa.
*      DATA y_mmpa TYPE TABLE OF mmpa.
*
*
*      " item --->>
*      CREATE OBJECT lo_item
*        EXPORTING
*          im_po_item_number = '00010'
*          im_po_header      = lo_header
**         im_po_header_id   =
*        EXCEPTIONS
*          failure           = 1
*          OTHERS            = 2.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      ls_item-id = '2'.
*      ls_item-ebelp = '00010'.
*      ls_item-matnr = '000000000000119892'.
*      ls_item-ematn = '000000000000119892'.
*      ls_item-werks = '1003'.
*      ls_item-lgort = 'ARMZ'.
*
*      ls_itemx-item_id = '2'.
*      ls_itemx-ebelp = 'X'.
*      ls_itemx-matnr = 'X'.
*      ls_itemx-ematn = 'X'.
*      ls_itemx-werks = 'X'.
*      ls_itemx-lgort = 'X'.
*
*      lo_header->my_ibs_firewall_on = 'X'.
*      lo_header->my_cust_firewall_on = 'X'.
*
*      lo_item->set_data( ls_item ).
*      lo_item->set_datax( ls_itemx ).
*
*      "lo_item->get_data( ex_data = ls_items ).
*
*      " item ----<<
*
*      lo_header->set_data( ls_header ).
*      "im_header->set_data( ls_header ).
*
*
*      "DATA lo_items TYPE TABLE OF cl_po_item_handle_mm.
*      CALL METHOD lo_header->get_items
**        EXPORTING
**          im_items_with_error =
**          im_enq_err_mat      =
**          im_enq_err_con      =
*        IMPORTING
*          ex_items = DATA(lo_items).
*
*
*
*      APPEND INITIAL LINE TO x_mmpa ASSIGNING FIELD-SYMBOL(<fs_mmpa>).
*      <fs_mmpa>-mandt = '300'.
*      <fs_mmpa>-ekorg = ls_header-ekorg.
*      <fs_mmpa>-parvw = 'PR'.
*      <fs_mmpa>-parza = '001'.
*      <fs_mmpa>-ernam = sy-uname.
*      <fs_mmpa>-erdat = sy-datum.
*      <fs_mmpa>-lifn2 = '0000000124'.
*
*      y_mmpa = x_mmpa.
*
*      "DATA(lt_lfa1) = lo_header->if_purchasing_document~get_partners( ).
*
*      CALL FUNCTION 'MM_MAINTAIN_PARTNERS'
*        EXPORTING
*          application    = 'P'
*          bstyp          = ls_header-bstyp
*          ekorg          = ls_header-ekorg
*          lifnr          = '0000000124'
*          pargr          = '0003'
*          bukrs          = ls_header-bukrs
*          subscreen_mode = 'PBO'
*          aktyp          = 'A'
*        TABLES
*          x_mmpa         = x_mmpa
*          y_mmpa         = y_mmpa.
*
*
**      DATA lt_items TYPE TABLE OF mepoitem.
**      DATA lt_itemx TYPE TABLE OF mepoitemx.
**
**      APPEND INITIAL LINE TO lt_items ASSIGNING FIELD-SYMBOL(<fs_item>).
**
**      <fs_item>-id = '2'.
**      <fs_item>-ebelp = '00010'.
**      <fs_item>-matnr = '000000000000119892'.
**      <fs_item>-ematn = '000000000000119892'.
**      <fs_item>-werks = '1003'.
**      <fs_item>-lgort = 'ARMZ'.
**
**      APPEND INITIAL LINE TO lt_itemx ASSIGNING FIELD-SYMBOL(<fs_itemx>).
**
**      <fs_itemx>-item_id = '2'.
**      <fs_itemx>-ebelp = 'X'.
**      <fs_itemx>-matnr = 'X'.
**      <fs_itemx>-ematn = 'X'.
**      <fs_itemx>-werks = 'X'.
**      <fs_itemx>-lgort = 'X'.
**
**      CALL FUNCTION 'MEPO_DOC_ITEM_PROCESS'
**        EXPORTING
**          im_itemx     = <fs_itemx>
***         IM_EKPVX     =
***       IMPORTING
***         EX_EBELP     =
***       TABLES
***         CHT_SCHEDULES                 =
***         CHT_ACCOUNTINGS               =
***         CHT_CONDITIONS                =
***         CHT_SERVICE_ACCOUNTINGS       =
***         IMT_BAPI_EKET                 =
***         IMT_BAPI_EKETX                =
***         IMT_BAPI_EKKN                 =
***         IMT_BAPI_EKKNX                =
**        CHANGING
**          ch_item      = <fs_item>
***         CH_ITEM_SERVICE_DATA          =
***         CH_EKPV      =
***         CH_EIPO      =
**        EXCEPTIONS
**          invalid_call = 1
**          OTHERS       = 2.
**
**      IF sy-subrc <> 0.
*** Implement suitable error handling here
**      ENDIF.
*
*
*
*
*
*
**      CALL FUNCTION 'MEPO_DOC_PROCESS'
**        EXPORTING
**          im_ekko               = ls_header
**          im_aktyp              = 'H'
**        TABLES
**          cht_items             = lt_items
**          imt_itemsx            = lt_itemx
**        EXCEPTIONS
**          invalid_call          = 1
**          invalid_activity_type = 2
**          OTHERS                = 3.
**
**      IF sy-subrc <> 0.
*** Implement suitable error handling here
**      ENDIF.
*
*      lo_header->my_ibs_firewall_on = space.
*      lo_header->my_cust_firewall_on = space.

    "ENDIF.

    " TESTE RAMON --<



  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~post.

    FIELD-SYMBOLS: <xekpa>    TYPE ANY TABLE.

    DATA: l_header        TYPE mepoheader.
    DATA: w_pedido_compra    TYPE zde_cargueiro_pc.
    DATA: lt_item      TYPE TABLE OF mepoitem,
          lt_erro      TYPE TABLE OF zmms004,
          it_itens     TYPE purchase_order_items,
          ls_itens     TYPE purchase_order_item,
          lt_partners  TYPE mmpur_partner_all_tab,
          ls_partner   LIKE LINE OF lt_partners,
          lt_ekpa      TYPE TABLE OF ekpa,
          ls_ekpa      TYPE ekpa,
          lo_po_doc    TYPE REF TO if_purchasing_document,
          l_erro       TYPE zmms004,
          wa_itens     TYPE purchase_order_item,
          wa_item_if   TYPE REF TO if_purchase_order_item_mm,
          l_item       TYPE mepoitem,
          l_nome_tvarv TYPE tvarvc-name,
          l_lifn2      TYPE ekpa-lifn2,
          r_ekpa       TYPE RANGE OF ekpa-lifn2,
          r_matkl      TYPE RANGE OF ekpo-matkl,
          l_mtext      TYPE char100,
          l_mtext1     TYPE char100,
          l_mtext2     TYPE char100.

    FREE: lt_ekpa, r_ekpa, r_matkl.

*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
    FREE: w_pedido_compra.

*----------------------------------------------------
*   Obtém dados do Item
*----------------------------------------------------
    CALL METHOD im_header->get_items
      RECEIVING
        re_items = it_itens.

*----------------------------------------------------
*---Obtener Header Structure
*----------------------------------------------------
    CALL METHOD im_header->get_data
      RECEIVING
        re_data = l_header.

*----------------------------------------------------
*---PARCEIROS
*----------------------------------------------------
    ASSIGN ('(SAPLMEPO)xekpa[]')           TO <xekpa>[].

    IF sy-subrc = 0 AND <xekpa>[] IS NOT INITIAL.
      LOOP AT <xekpa>                    INTO ls_ekpa.
        MOVE im_ebeln                      TO ls_ekpa-ebeln.
        APPEND ls_ekpa                     TO lt_ekpa.
        APPEND ls_ekpa                     TO w_pedido_compra-parceiros.
      ENDLOOP.
    ELSE.
      lo_po_doc ?= im_header.  " casting to purchase document
      lt_partners = lo_po_doc->get_partners( ).

      LOOP AT lt_partners                INTO ls_partner .
        MOVE-CORRESPONDING ls_partner-data TO ls_ekpa.
        MOVE im_ebeln                      TO ls_ekpa-ebeln.
        MOVE ls_partner-data-parvw         TO ls_ekpa-parvw.
        MOVE ls_partner-data-parno         TO ls_ekpa-lifn2.
        APPEND ls_ekpa                     TO lt_ekpa.
        APPEND ls_ekpa                     TO w_pedido_compra-parceiros.
      ENDLOOP.
    ENDIF.

*----------------------------------------------------
*---Itens da PO
*----------------------------------------------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_grp_mat)
     WHERE name = 'MAGGI_GR_FERTILIZANTES'.

    LOOP AT t_grp_mat INTO DATA(w_grp_mat).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_grp_mat-low ) TO r_matkl.
    ENDLOOP.

    LOOP AT it_itens INTO ls_itens.

      wa_item_if   = ls_itens-item.
*     l_item       = wa_item_if->get_data( ).

*------------------------------------------------
*-----obtem item
*------------------------------------------------
      CALL METHOD ls_itens-item->get_data
        RECEIVING
          re_data = l_item.

*-CS2020001303 - 12.11.2021 - JT - inicio
*--------------------
*-----TVARV
*--------------------
      IF l_item-matkl IN r_matkl[] AND r_matkl[] IS NOT INITIAL.
        l_nome_tvarv = 'PARCEIRO_PC_CENTRO_' && l_item-werks.

        SELECT *
          FROM tvarvc
          INTO TABLE @DATA(t_parceiro)
         WHERE name = @l_nome_tvarv.

        IF sy-subrc = 0.
          LOOP AT t_parceiro INTO DATA(w_parceiro).
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = w_parceiro-low
              IMPORTING
                output = l_lifn2.

            APPEND VALUE #( sign = 'I' option = 'EQ' low = l_lifn2 ) TO r_ekpa.
          ENDLOOP.
        ENDIF.

        CLEAR ls_ekpa.
        READ TABLE lt_ekpa INTO ls_ekpa WITH KEY parvw = 'PR'.

        CHECK ls_ekpa-lifn2 IN r_ekpa[] AND r_ekpa[] IS NOT INITIAL.

      ELSEIF l_header-bsart EQ 'ZUB'.

        "Se for pedido de transferencia, só enviar para o carguero se for do grupo de mercadoria de grãos
        SELECT SINGLE *
          FROM tvarvc INTO @DATA(LWA_STVARV_GRAOS)
         WHERE name EQ 'MAGGI_GR_GRAOS'
          AND  low  EQ @l_item-matkl.

        CHECK SY-SUBRC EQ 0.

      ELSE.
        CONTINUE.
      ENDIF.
*-CS2020001303 - 12.11.2021 - JT - fim

      MOVE im_ebeln                     TO l_header-ebeln.
      MOVE im_ebeln                     TO l_item-ebeln.
      MOVE-CORRESPONDING l_header       TO w_pedido_compra.
      MOVE-CORRESPONDING l_item         TO w_pedido_compra-item.

      TRY .
          zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
                  EXPORTING i_pedido_compra = w_pedido_compra
                  IMPORTING e_id_lote_frete = l_item-id_lote_frete ).
        CATCH zcx_integracao INTO DATA(zcx_error_msg).
          IF SY-TCODE = 'ZSDT0191' OR sy-tcode = 'ZSDT0081'.

            l_mtext = zcx_error_msg->get_text( ).
            REPLACE ALL OCCURRENCES OF '&' IN  l_mtext WITH SPACE.

            "MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO l_mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            l_mtext1 = l_mtext(50).
            l_mtext2 = l_mtext+50(50).

            CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
              EXPORTING
                titel     = 'Erro de Envio ao Carguero'
                textline1 = l_mtext1
                textline2 = l_mtext2.
          ENDIF.

        CATCH zcx_error INTO DATA(zcx_error_msg2).
          IF SY-TCODE = 'ZSDT0191' OR sy-tcode = 'ZSDT0081'.

            l_mtext = zcx_error_msg2->get_text( ).
            REPLACE ALL OCCURRENCES OF '&' IN  l_mtext WITH SPACE.

            "MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO l_mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

            l_mtext1 = l_mtext(50).
            l_mtext2 = l_mtext+50(50).

            CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
              EXPORTING
                titel     = 'Erro de Envio ao Carguero'
                textline1 = l_mtext1
                textline2 = l_mtext2.
          ENDIF.
      ENDTRY.

*------------------------------------------------
* ----atualiza ID_LOTE
*------------------------------------------------
      CALL METHOD ls_itens-item->set_data
        EXPORTING
          im_data = l_item.

    ENDLOOP.
*---CS2020000704 - JTASSONI - 01.10.2020 - fim

*----------------------------------------------------
*----------------------------------------------------
*   CALL FUNCTION 'MEPOBADIEX_POST'
*     EXPORTING
*       IM_EBELN = IM_EBELN.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
  endmethod.


  method if_ex_me_process_po_cust~process_header.
    field-symbols: <lfs_ver> type c.

    assign ('(SAPLMEPO)VER') to <lfs_ver>.
    if sy-tcode+0(3) = 'ME2' or sy-tcode+0(3) = 'ME5' or sy-tcode = 'MASS'.
      if ( <lfs_ver> ne 'A' ).
        data: ls_header type mepoheader.
        ls_header = im_header->get_data( ). " read PO header data
        if ls_header-bsart ne 'PCEE'.
          select count( * )
            from tvarvc
            where name = 'ZMM_PRICE_PO_BSART'
              and low  = ls_header-bsart.
          if ( sy-subrc is initial ).
            select count(*)
              from zmmt0160
              where ebeln = ls_header-ebeln "Exceção pedidos COUPA
              and   data  = sy-datum.
            if sy-subrc ne 0 or sy-tcode+0(3) = 'ME5'.
              if sy-tcode+0(3) = 'ME5'.
                message a013(z_mm).
              else.
                message a006(z_mm).
              endif.
            elseif   sy-tcode+0(3) = 'ME5'.
              message a013(z_mm).
            endif.
          else.
            select single bsart
              into @data(_bsart)
              from ekko
              where ebeln = @ls_header-ebeln.
            if sy-subrc = 0.
              if _bsart ne ls_header-bsart.
                message a014(z_mm).
              endif.
            endif.

          endif.
        endif.
      endif.
    endif.

  endmethod.


  METHOD if_ex_me_process_po_cust~process_item.

    DATA: lit_ekpo_ext_memory TYPE me_ekpo.
    DATA: vg_branch TYPE j_1bbranc_.

    IF sy-tcode = 'ME21N' OR sy-tcode = 'ME22N' OR sy-tcode = 'ME23N'.

      DATA(item) = im_item->get_data( ).
      INCLUDE mm_messages_mac. "useful macros for message handling

      IF item-knttp = 'A' AND item-recap IS INITIAL.

        "Verificar se é Compra para Imobilizado
        SELECT SINGLE * FROM j_1btxic3
          INTO @DATA(w_j_1btxic3)
          WHERE land1	=	'BR'
            AND gruop	=	'42'
            AND value	=	@item-j_1bnbm.

        IF sy-subrc IS INITIAL.
          "Adicionado condição, se for a empresa que esta no SET não precisa verificar o RECAP. #IR157525 / ANDERSON OENNING / 28/10/2023

          CLEAR: vg_branch.
          vg_branch = CONV #( item-werks ).
          SELECT SINGLE bukrs FROM j_1bbranch
            INTO @DATA(vg_bukrs)
            WHERE branch EQ @vg_branch.
          IF vg_bukrs IS NOT INITIAL.
            SELECT SINGLE *
            FROM setleaf
            INTO @DATA(i_data)
            WHERE setname EQ 'MAGI_MM_NOT_RECAP'
            AND valfrom EQ @vg_bukrs.
          ENDIF.


          IF i_data IS INITIAL.
            mmpur_context 904.
            mmpur_business_obj_id item-id.
            MESSAGE e000(zmmped) WITH 'Informe na ABA Dados do cliente, '  'o status do Benef. RECAP ' INTO DATA(texto).
            mmpur_message_forced sy-msgty sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          "Adicionado condição, se for a empresa que esta no SET não precisa verificar o RECAP. #IR157525 / ANDERSON OENNING / 28/10/2023
*        ELSE.
*          mmpur_remove_msg_by_context item-id 904.
        ENDIF.

      ENDIF.

    ENDIF.

    IMPORT lit_ekpo_ext_memory TO lit_ekpo_ext_memory FROM MEMORY ID 'LIT_EKPO_EXTENSION'.
    FREE MEMORY ID 'LIT_EKPO_EXTENSION'.

    CASE sy-tcode.
      WHEN 'ZMM0203'.
        READ TABLE lit_ekpo_ext_memory INTO DATA(lwa_ekpo_ext) INDEX 1.
        IF sy-subrc EQ 0 AND lit_ekpo_ext_memory[] IS NOT INITIAL.
          DATA(_item_ekpo_change) = im_item->get_data( ).

          _item_ekpo_change-zkvgr3      = lwa_ekpo_ext-zkvgr3.
          _item_ekpo_change-zkvgr4      = lwa_ekpo_ext-zkvgr4.
          _item_ekpo_change-ztrocanota  = lwa_ekpo_ext-ztrocanota.
          _item_ekpo_change-zckfreteent = lwa_ekpo_ext-zckfreteent.

          IF im_item->is_valid( ) EQ 'X'.
            im_item->set_data( im_data = _item_ekpo_change ).
          ENDIF.
        ENDIF.
    ENDCASE.




  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
  endmethod.
ENDCLASS.
