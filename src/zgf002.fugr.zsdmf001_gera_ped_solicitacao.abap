function zsdmf001_gera_ped_solicitacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_NRO_SOL_OV) TYPE  ZSDED013
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_EBELN) TYPE  EBELN
*"  TABLES
*"      ET_BAPIRET2 STRUCTURE  BAPIRET2
*"  EXCEPTIONS
*"      PED_JA_EXISTE
*"      SOLICITACAO_NAO_EXISTE
*"----------------------------------------------------------------------

  data ls_header type bapimepoheader.
  data ls_headerx type bapimepoheaderx.

  data ls_expheader type bapimepoheader.

  data lt_item type table of bapimepoitem.
  data lt_itemx type table of bapimepoitemx.

  data lt_partner type table of bapiekkop.

  data lv_zub type auart value 'ZUB'.
  data lv_erro.

  check iv_nro_sol_ov is not initial.

  select single vkorg,zsdt0051~auart, zsdt0051~kunnr,
      werks, zsdt0053~matnr, lgort, charg,zmeng,zieme from zsdt0051
    inner join zsdt0053 on zsdt0051~nro_sol_ov = zsdt0053~nro_sol_ov
    into @data(ls_sol)
      where zsdt0051~nro_sol_ov = @iv_nro_sol_ov
        and zsdt0051~auart = 'ZUB'.

  check sy-subrc eq 0.

  ls_header-comp_code   = ls_sol-vkorg.
  ls_header-doc_type    = ls_sol-auart.
  ls_header-purch_org   = 'OC01'.
  ls_header-pur_group   = 'G01'.
  ls_header-currency    = 'BRL'.
  ls_header-collect_no  = iv_nro_sol_ov.
  ls_header-incoterms1  = 'CIF'.
  ls_header-incoterms2  = 'CIF'.
  ls_header-suppl_plnt = ls_sol-werks.


  perform f_preenche_x using ls_header space changing ls_headerx.

  append initial line to lt_item assigning field-symbol(<fs_item>).
  append initial line to lt_itemx assigning field-symbol(<fs_itemx>).

  add 10 to <fs_item>-po_item.

  <fs_item>-plant = |{ ls_sol-kunnr alpha = out }|.

  if strlen( <fs_item>-plant ) < 4  .
    <fs_item>-plant =  |{ <fs_item>-plant alpha = in }|.
  endif.

  <fs_item>-material = ls_sol-matnr.
  <fs_item>-batch = ls_sol-charg.

 "US163737
  if <fs_item>-plant is not initial .
    select single *
      from marc
      into @data(_marc)
      where matnr = @<fs_item>-material
      and   werks = @<fs_item>-plant.
    if sy-subrc = 0.
      if _marc-xchpf is initial.
        clear <fs_item>-batch.
      endif.

    endif.
  endif.
  "US163737

  <fs_item>-stge_loc = ls_sol-lgort.

  <fs_item>-quantity = ls_sol-zmeng.
  <fs_item>-po_unit = ls_sol-zieme.
  <fs_item>-incoterms1 = 'CIF'.
  <fs_item>-incoterms2 = 'CIF'.

  perform f_preenche_x using <fs_item> 'I' changing <fs_itemx>.

  lt_partner = value #( ( partnerdesc = 'PR' buspartno = |{ ls_sol-werks alpha = in }|  langu = sy-langu ) ).

  clear et_bapiret2[].

  call function 'BAPI_PO_CREATE1'
    exporting
      poheader         = ls_header
      poheaderx        = ls_headerx
    importing
      exppurchaseorder = ev_ebeln
      expheader        = ls_expheader
    tables
      return           = et_bapiret2
      poitem           = lt_item
      poitemx          = lt_itemx
      popartner        = lt_partner.

  perform f_bapi_confirm_process
    using iv_commit
 changing et_bapiret2[] lv_erro.

endfunction.
