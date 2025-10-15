FUNCTION zmm_me21n_dados_sigam.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IM_HEADER) TYPE REF TO  IF_PURCHASE_ORDER_MM
*"----------------------------------------------------------------------

  DATA lv_erro TYPE c.

  DATA lo_header  TYPE REF TO cl_po_header_handle_mm.

  PERFORM f_check_process CHANGING lv_erro.

  CHECK lv_erro IS INITIAL.

  PERFORM f_processa_cab USING im_header.

  PERFORM f_processa_item USING im_header.

  PERFORM f_processa_parceiro USING im_header.

*  EXIT.
*
*  TYPES: BEGIN OF sorted_by_flushable,
*           flushable      TYPE REF TO if_flush_transport_mm,
*           changed_models TYPE mmpur_models,
*         END OF sorted_by_flushable.
*
*  DATA ls_flush_todo  TYPE sorted_by_flushable.
*  DATA lt_model TYPE TABLE OF mmpur_models.
*
*  DATA ls_header TYPE mepoheader.
*  DATA ls_item TYPE mepoitem.
*
*  DATA lo_header  TYPE REF TO cl_po_header_handle_mm.
*
*  DATA ls_itemx TYPE mepoitemx.
*
*  DATA x_mmpa TYPE TABLE OF mmpa.
*  DATA y_mmpa TYPE TABLE OF mmpa.
*
*  lo_header ?= im_header.
*
*  ls_header = im_header->get_data( ).
*
*  ls_header-bsart = 'ZUB'.
*  ls_header-reswk = '0124'.
*  ls_header-ekorg = 'OC01'.
*  ls_header-ekgrp = 'G01'.
*  ls_header-bukrs = '0001'.
*
*  "ls_header-statu = '9'.
*  ls_header-aedat = sy-datum.
*  ls_header-ernam = sy-uname.
*  ls_header-pincr = '00010'.
*  ls_header-spras = sy-langu.
*  ls_header-bedat = sy-datum.
*  ls_header-upinc = '00001'.
*  ls_header-stceg_l = 'BR'.
*  "ls_header-procstat = '02'.
*  ls_header-mandt = sy-mandt.
*  ls_header-bstyp = 'F'.
*  ls_header-bsakz = 'T'.
*  ls_header-lands = 'BR'.
*  ls_header-id = '1'.
*
*  lo_header->my_ibs_firewall_on = 'X'.
*  lo_header->my_cust_firewall_on = 'X'.
*
*  lo_header->set_data( ls_header ).
*
*  CALL METHOD lo_header->if_purchase_order_mm~create_item
*    RECEIVING
*      re_item = DATA(lo_item).
*
*  ls_item-id = '2'.
*  ls_item-ebelp = '00010'.
*  ls_item-matnr = '000000000000119892'.
*  ls_item-ematn = '000000000000119892'.
*  ls_item-werks = '1003'.
*  ls_item-lgort = 'ARMZ'.
*
*  lo_item->set_data( ls_item ).
*
*  ls_flush_todo-flushable = lo_header.
*
*  APPEND INITIAL LINE TO ls_flush_todo-changed_models ASSIGNING FIELD-SYMBOL(<fs_model>).
*
*  <fs_model>-model ?= lo_item.
*
*  CALL METHOD lo_header->if_flush_transport_mm~start
*    EXPORTING
*      im_models    = ls_flush_todo-changed_models
*    EXCEPTIONS
*      illegal_call = 01
*      error        = 02.
*
*  IF NOT sy-subrc IS INITIAL.
*    "l_at_least_one_failed = mmpur_yes.
*  ENDIF.
*
*
*  DATA(lv_valida) = lo_item->is_valid( ).
*
*  CALL METHOD lo_header->get_items
*    IMPORTING
*      ex_items = DATA(lo_items1).
*
*
*  APPEND INITIAL LINE TO x_mmpa ASSIGNING FIELD-SYMBOL(<fs_mmpa>).
*  <fs_mmpa>-mandt = '300'.
*  <fs_mmpa>-ekorg = ls_header-ekorg.
*  <fs_mmpa>-parvw = 'PR'.
*  <fs_mmpa>-parza = '001'.
*  <fs_mmpa>-ernam = sy-uname.
*  <fs_mmpa>-erdat = sy-datum.
*  <fs_mmpa>-lifn2 = '0000000124'.
*
*  y_mmpa = x_mmpa.
*
*  CALL FUNCTION 'MM_MAINTAIN_PARTNERS'
*    EXPORTING
*      application    = 'P'
*      bstyp          = ls_header-bstyp
*      ekorg          = ls_header-ekorg
*      lifnr          = '0000000124'
*      pargr          = '0003'
*      bukrs          = ls_header-bukrs
*      subscreen_mode = 'PBO'
*      aktyp          = 'A'
*    TABLES
*      x_mmpa         = x_mmpa
*      y_mmpa         = y_mmpa.
*
*
*  CALL METHOD lo_header->get_items
*    IMPORTING
*      ex_items = DATA(lo_items2).


  lo_header ?= im_header.
  lo_header->my_ibs_firewall_on = space.
  lo_header->my_cust_firewall_on = space.


ENDFUNCTION.
