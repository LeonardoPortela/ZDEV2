FUNCTION zsdmf_insumos_criar_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_ITENS_OV) TYPE  ZSDC084
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_VBELN) TYPE  VBELN
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      ET_RET STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA ls_header_in TYPE bapisdhd1.
  DATA lv_doc_simu TYPE zsded003.

  DATA lt_items_in TYPE TABLE OF bapisditm.
  DATA lt_sched    TYPE TABLE OF bapischdl.
  DATA lt_partners TYPE TABLE OF bapiparnr.
  DATA lt_cond TYPE TABLE OF bapicond.

  CHECK it_itens_ov IS NOT INITIAL.

  lv_doc_simu = it_itens_ov[ 1 ]-doc_simulacao.

  SELECT SINGLE * FROM zsdt0040
    INTO @DATA(ls_0040)
      WHERE doc_simulacao = @lv_doc_simu.

  LOOP AT it_itens_ov ASSIGNING FIELD-SYMBOL(<fs_ov_item>).

    APPEND INITIAL LINE TO lt_items_in ASSIGNING FIELD-SYMBOL(<fs_item>).
    APPEND INITIAL LINE TO lt_sched ASSIGNING FIELD-SYMBOL(<fs_sched>).
    APPEND INITIAL LINE TO lt_cond ASSIGNING FIELD-SYMBOL(<fs_cond>).

    <fs_item>-itm_number = <fs_ov_item>-posnr.
    <fs_item>-material = <fs_ov_item>-matnr.
    <fs_item>-target_qty = <fs_ov_item>-kwmeng.
    <fs_item>-target_qu = <fs_ov_item>-vrkme.
    <fs_item>-sales_unit = <fs_ov_item>-vrkme.
    <fs_item>-usage_ind = 'I'.
    <fs_item>-plant = <fs_ov_item>-werks.
    <fs_item>-batch = <fs_ov_item>-charg.
    <fs_item>-store_loc = <fs_ov_item>-lgort.
    <fs_item>-ship_point = <fs_ov_item>-werks.
    <fs_item>-matfrgtgrp = '00000001'.

    <fs_sched>-itm_number = <fs_item>-itm_number.
    <fs_sched>-sched_line = '001'.
    <fs_sched>-req_qty = <fs_item>-target_qty.
    <fs_sched>-req_dlv_bl = '10'.

    <fs_cond>-itm_number  = <fs_item>-itm_number.
    <fs_cond>-currency    = <fs_ov_item>-waerk.
    <fs_cond>-cond_value  = <fs_ov_item>-netpr.
    <fs_cond>-cond_unit   = <fs_item>-sales_unit.
    <fs_cond>-cond_type   = 'PR00'.

    IF lt_partners IS INITIAL.

      lt_partners = VALUE #( ( partn_role = 'AG' partn_numb = ls_0040-kunnr )
                       ( partn_role = 'PC' partn_numb = |{ <fs_ov_item>-werks ALPHA = IN }| ) ).

    ENDIF.

**    " preenche cabeçalho ----------
    ls_header_in-doc_type = <fs_ov_item>-auart.
    ls_header_in-doc_date = sy-datum.
    ls_header_in-sales_org  = <fs_ov_item>-vkorg.
    ls_header_in-distr_chan = <fs_ov_item>-vtweg.
    ls_header_in-sales_off  = <fs_ov_item>-vkbur.
    ls_header_in-sales_grp  = <fs_ov_item>-vkgrp.
    ls_header_in-purch_date = sy-datum.
    ls_header_in-purch_no_c = <fs_ov_item>-bstnk.
    ls_header_in-currency   = <fs_ov_item>-waerk.
    ls_header_in-pymt_meth  = <fs_ov_item>-zlsch.
    ls_header_in-fix_val_dy = <fs_ov_item>-valdt.
    ls_header_in-exrate_fi  = <fs_ov_item>-kurrf.
    ls_header_in-pmnttrms   = <fs_ov_item>-zterm.
    "ls_header_in-incoterms1 = <fs_ov_item>-inco1.
    "ls_header_in-incoterms2 = <fs_ov_item>-inco2.
    ls_header_in-division   = <fs_ov_item>-spart.

    ls_header_in-refobjtype = 'VBRK'.
    "ls_header_in-refobjkey  = i_bill_doc.
    ls_header_in-refdoctype = 'M'.
    ls_header_in-refdoc_cat = 'M'.
    "ls_header_in-ref_doc    = i_bill_doc.
    "ls_header_in-ref_doc_l  = i_bill_doc.
    "ls_header_in-

  ENDLOOP.

  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in     = ls_header_in
    IMPORTING
      salesdocument_ex    = ev_vbeln
    TABLES
      return              = et_ret
      sales_partners      = lt_partners
      sales_items_in      = lt_items_in
      sales_schedules_in  = lt_sched
      sales_conditions_in = lt_cond.
  "sales_text          = tl_text_in
  "extensionin         = tl_bapiparex.


  PERFORM f_bapi_confirm_process
    USING iv_commit
  CHANGING et_ret[]
           ev_erro.

ENDFUNCTION.
