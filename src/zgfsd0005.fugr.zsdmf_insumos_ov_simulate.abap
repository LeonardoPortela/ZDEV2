FUNCTION zsdmf_insumos_ov_simulate.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN_REF) TYPE  VBELN_VA
*"     REFERENCE(IV_POSNR) TYPE  POSNR_VA OPTIONAL
*"     REFERENCE(IV_PR00) TYPE  BAPIKBETR1 OPTIONAL
*"     REFERENCE(IV_RB00) TYPE  BAPIKBETR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_ICMI_MONT) TYPE  BAPIKBETR1
*"     REFERENCE(EV_ICMI_COND) TYPE  BAPIKBETR1
*"     REFERENCE(EV_ICVA_KBERT) TYPE  BAPIKBETR1
*"     REFERENCE(EV_ICBS_KBERT) TYPE  BAPIKBETR1
*"  TABLES
*"      ET_BAPICOND STRUCTURE  BAPICOND OPTIONAL
*"----------------------------------------------------------------------

  CLEAR: ev_icmi_mont, ev_icmi_cond, ev_icva_kbert, ev_icbs_kbert, et_bapicond[].

  DATA ls_header_in TYPE bapisdhd1.
  DATA ls_headerx_in TYPE bapisdhd1x.
  DATA ls_logic TYPE bapisdls.

  DATA lv_doc_simu TYPE zsded003.

  DATA lt_ret TYPE TABLE OF bapiret2.
  DATA lt_items_in TYPE TABLE OF bapisditm.
  DATA lt_sched    TYPE TABLE OF bapischdl.
  DATA lt_partners TYPE TABLE OF bapiparnr.
  DATA lt_cond TYPE TABLE OF bapicond.
  DATA lt_condx TYPE TABLE OF bapicondx.

  DATA lr_posnr TYPE RANGE OF posnr_va.

  IF iv_posnr IS NOT INITIAL.
    APPEND 'IEQ' && iv_posnr TO lr_posnr.
  ENDIF.

  SELECT vbak~vbeln,posnr,vbak~spart,matnr,werks,charg,lgort,kwmeng,vrkme,vbak~waerk,netpr,
         auart,vkorg,vtweg,vkbur,vkgrp,bstnk,knumv FROM vbak
    INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
      INTO TABLE @DATA(lt_ov)
        WHERE vbak~vbeln = @iv_vbeln_ref
          AND posnr IN @lr_posnr.

  CHECK sy-subrc EQ 0.

  SELECT vbeln,posnr,valdt,kurrf,zlsch,zterm,kursk FROM vbkd
    INTO TABLE @DATA(lt_vbkd)
      FOR ALL ENTRIES IN @lt_ov
        WHERE vbeln = @lt_ov-vbeln.

  SELECT parvw AS partn_role, kunnr AS partn_numb FROM vbpa
     INTO CORRESPONDING FIELDS OF TABLE @lt_partners
        WHERE vbeln = @iv_vbeln_ref
          AND parvw IN ('AG','PC').

  SELECT * FROM v_konv
    INTO TABLE @DATA(lt_konv)
      FOR ALL ENTRIES IN @lt_ov
        WHERE knumv = @lt_ov-knumv.

  LOOP AT lt_ov ASSIGNING FIELD-SYMBOL(<fs_ov>).

    APPEND INITIAL LINE TO lt_items_in ASSIGNING FIELD-SYMBOL(<fs_item>).
    APPEND INITIAL LINE TO lt_sched ASSIGNING FIELD-SYMBOL(<fs_sched>).

    <fs_item>-itm_number = <fs_ov>-posnr.
    <fs_item>-material = <fs_ov>-matnr.
    <fs_item>-target_qty = <fs_ov>-kwmeng.
    <fs_item>-target_qu = <fs_ov>-vrkme.
    <fs_item>-sales_unit = <fs_ov>-vrkme.

    <fs_item>-usage_ind = 'I'.
    <fs_item>-plant = <fs_ov>-werks.
    <fs_item>-batch = <fs_ov>-charg.
    <fs_item>-store_loc = <fs_ov>-lgort.
    <fs_item>-ship_point = <fs_ov>-werks.
    <fs_item>-matfrgtgrp = '00000001'.

    <fs_sched>-itm_number = <fs_item>-itm_number.
    <fs_sched>-sched_line = '001'.
    <fs_sched>-req_qty = <fs_item>-target_qty.
    <fs_sched>-req_dlv_bl = '10'.

    IF iv_pr00 IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_cond ASSIGNING FIELD-SYMBOL(<fs_cond>).
      <fs_cond>-itm_number  = <fs_item>-itm_number.
      <fs_cond>-currency    = <fs_ov>-waerk.
      <fs_cond>-cond_value  = iv_pr00. "<fs_ov>-netpr.
      <fs_cond>-cond_unit   = <fs_item>-sales_unit.
      <fs_cond>-cond_type   = 'PR00'.

    ENDIF.

    IF iv_rb00 IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_cond ASSIGNING <fs_cond>.
      <fs_cond>-itm_number  = <fs_item>-itm_number.
      "<fs_cond>-cond_count = '01'.
      <fs_cond>-cond_type   = 'RB00'.
      <fs_cond>-cond_value  = iv_rb00. "<fs_ov>-netpr.
      <fs_cond>-currency    = <fs_ov>-waerk.
      "<fs_cond>-cond_unit   = <fs_item>-sales_unit.

      APPEND INITIAL LINE TO lt_condx ASSIGNING FIELD-SYMBOL(<fs_condx>).
      <fs_condx>-itm_number  = <fs_item>-itm_number.
      <fs_condx>-cond_count = '01'.
      <fs_condx>-cond_type   = 'RB00'.
      <fs_condx>-cond_value  = abap_true.
      <fs_condx>-cond_unit    = abap_false.
      <fs_condx>-updateflag   = 'U'.
      <fs_condx>-currency   = abap_true.

    ENDIF.

**    " preenche cabeçalho ----------
    ls_header_in-doc_type = <fs_ov>-auart.
    ls_header_in-doc_date = sy-datum.
    ls_header_in-sales_org  = <fs_ov>-vkorg.
    ls_header_in-distr_chan = <fs_ov>-vtweg.
    ls_header_in-sales_off  = <fs_ov>-vkbur.
    ls_header_in-sales_grp  = <fs_ov>-vkgrp.
    ls_header_in-purch_date = sy-datum.
    ls_header_in-purch_no_c = <fs_ov>-bstnk.
    ls_header_in-currency   = <fs_ov>-waerk.

    READ TABLE lt_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>)
      WITH KEY vbeln = <fs_ov>-vbeln.

    CHECK sy-subrc EQ 0.

    <fs_item>-ex_rate_fi = <fs_vbkd>-kursk.
    ls_header_in-exrate_fi = <fs_vbkd>-kursk.
    ls_header_in-pymt_meth  = <fs_vbkd>-zlsch.
    ls_header_in-fix_val_dy = <fs_vbkd>-valdt.
    ls_header_in-exrate_fi  = <fs_vbkd>-kurrf.
    ls_header_in-pmnttrms   = <fs_vbkd>-zterm.
    "ls_header_in-incoterms1 = <fs_ov_item>-inco1.
    "ls_header_in-incoterms2 = <fs_ov_item>-inco2.
    ls_header_in-division   = <fs_ov>-spart.

    ls_header_in-refobjtype = 'VBRK'.
    "ls_header_in-refobjkey  = i_bill_doc.
    ls_header_in-refdoctype = 'M'.
    ls_header_in-refdoc_cat = 'M'.
    "ls_header_in-ref_doc    = i_bill_doc.
    "ls_header_in-ref_doc_l  = i_bill_doc.
    "ls_header_in-

  ENDLOOP.

  PERFORM f_preenche_x USING ls_header_in '' CHANGING ls_headerx_in.

  ls_headerx_in-updateflag = 'I'.
  ls_logic-cond_handl   = 'X'.
  ls_logic-pricing = space.


  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in     = ls_header_in
      sales_header_inx    = ls_headerx_in
      logic_switch        = ls_logic
      testrun             = abap_true
    TABLES
      return              = lt_ret
      sales_partners      = lt_partners
      sales_items_in      = lt_items_in
      sales_schedules_in  = lt_sched
      sales_conditions_in = lt_cond
      "sales_conditions_inx = lt_condx
      conditions_ex       = et_bapicond.

  ev_icmi_mont = VALUE #( et_bapicond[ cond_type = 'ICMI' ]-cond_value DEFAULT '0.00' ).
  ev_icmi_cond = VALUE #( et_bapicond[ cond_type = 'ICMI' ]-condvalue DEFAULT '0.00' ).

  ev_icva_kbert = VALUE #( et_bapicond[ cond_type = 'ICVA' ]-cond_value DEFAULT '0.00' ).
  ev_icbs_kbert = VALUE #( et_bapicond[ cond_type = 'ICBS' ]-cond_value DEFAULT '0.00' ).

ENDFUNCTION.
