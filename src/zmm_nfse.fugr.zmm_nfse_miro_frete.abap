FUNCTION zmm_nfse_miro_frete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_NFSE_001) TYPE  ZIBT_NFSE_001
*"     REFERENCE(I_GROSSAMOUNT) TYPE  /TCSR/E_NFSE_VALUE
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_BELNR) TYPE  BELNR_D
*"     REFERENCE(E_GHJAR) TYPE  GJAHR
*"  TABLES
*"      IT_WITHTAX STRUCTURE  BAPI_INCINV_CREATE_WITHTAX OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      CT_0034 STRUCTURE  ZLEST0034 OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_mess TYPE string.
  DATA lv_doc_item TYPE rblgp.

  DATA lw_bapi_header TYPE bapi_incinv_create_header.
  DATA lt_bapi_item TYPE TABLE OF bapi_incinv_create_item.
  DATA lt_accdata TYPE TABLE OF bapi_incinv_create_account.
  DATA lt_contas_item TYPE zbapi_incinv_gl_account_t.
  DATA lt_contas TYPE zbapi_incinv_gl_account_t.
  DATA lv_vlr_ajust TYPE /tcsr/e_nfse_value.
  DATA lv_vlr_impos TYPE /tcsr/e_nfse_value.
  DATA lv_vlr_vi    TYPE /tcsr/e_nfse_value.
  DATA lt_zlest0021	TYPE zde_zlest0021_t.

  lv_vlr_ajust = i_grossamount.

  CLEAR: lv_vlr_impos,lv_vlr_vi.
  LOOP AT ct_0034 ASSIGNING FIELD-SYMBOL(<fs_0034>).

    CLEAR lt_contas_item.

    ADD 1 TO lv_doc_item.

    lw_bapi_header-invoice_ind     = 'X'.
    lw_bapi_header-doc_type        = 'FT'.
    lw_bapi_header-doc_date        = <fs_0034>-zdt_conhec.
    lw_bapi_header-pstng_date      = sy-datum.
    lw_bapi_header-bline_date      = is_nfse_001-dt_vencimento.
    lw_bapi_header-comp_code       = <fs_0034>-bukrs.
    lw_bapi_header-diff_inv        = <fs_0034>-tdlnr.
    lw_bapi_header-currency        = 'BRL'.
    lw_bapi_header-header_txt      = 'Frete Terceiro'.
    lw_bapi_header-pmnt_block      = is_nfse_001-zlspr. "09.02.2023 - RAMON - 103626
    lw_bapi_header-pmnttrms        = '0004'.

    " 31.03.2023 - 107971 - RBL --------->
    IF is_nfse_001-nfse_serie IS NOT INITIAL.
      CONCATENATE is_nfse_001-nfse_numero+6(9) '-' is_nfse_001-nfse_serie INTO lw_bapi_header-ref_doc_no.
    ELSE.
      CONCATENATE is_nfse_001-nfse_numero+6(9) '-1' INTO lw_bapi_header-ref_doc_no.
    ENDIF.
    " 31.03.2023 - 107971 - RBL ---------<

    lw_bapi_header-del_costs_taxc  = <fs_0034>-iva.
    lw_bapi_header-gross_amount   =  i_grossamount.
    lw_bapi_header-alloc_nmbr      = <fs_0034>-ebeln.

    IF <fs_0034>-werks NE is_nfse_001-branch.
      lw_bapi_header-bus_area = is_nfse_001-branch.
      <fs_0034>-werks         = is_nfse_001-branch.
    ELSE.
      lw_bapi_header-bus_area        = <fs_0034>-werks.
    ENDIF.

    lw_bapi_header-calc_tax_ind    = 'X'.
    lw_bapi_header-goods_affected  = 'X'.


    lw_bapi_header-pymt_meth       = is_nfse_001-pymt_meth.
    lw_bapi_header-housebankid     =  is_nfse_001-housebankid.
    lw_bapi_header-partner_bk      = is_nfse_001-zbvtyp.

    APPEND INITIAL LINE TO lt_bapi_item ASSIGNING FIELD-SYMBOL(<fs_bapi_item>).

    <fs_bapi_item>-invoice_doc_item = lv_doc_item.
    <fs_bapi_item>-po_number     = <fs_0034>-ebeln.
    <fs_bapi_item>-po_item       = <fs_0034>-ebelp.
    <fs_bapi_item>-tax_code      = <fs_0034>-iva.

    <fs_bapi_item>-ref_doc       = <fs_0034>-lblni.
    <fs_bapi_item>-ref_doc_year  = <fs_0034>-lfgja.
    <fs_bapi_item>-de_cre_ind    = ' '.
    <fs_bapi_item>-sheet_no      = <fs_0034>-lblni.
    <fs_bapi_item>-item_amount = <fs_0034>-dmbtr - ( <fs_0034>-valor_pis + <fs_0034>-valor_cofins ).

    ADD <fs_0034>-dmbtr        TO lv_vlr_vi.
    ADD <fs_0034>-valor_pis    TO lv_vlr_impos.
    ADD <fs_0034>-valor_cofins TO lv_vlr_impos.

    IF  <fs_bapi_item>-de_cre_ind <> 'X'.

      PERFORM f_busca_contas_miro
        USING <fs_0034>-bukrs
              <fs_0034>-werks
              <fs_0034>-vttk_vsart
              <fs_0034>-shtyp
              lv_doc_item
              <fs_bapi_item>-item_amount
              <fs_0034>-zvlr_perda " 18 - Perda
              <fs_0034>-zvlr_quebra " 16 - Quebra
              <fs_0034>-dmbtr "(soma o valor da vi de todas as vt associadas a nfs-e)
              sy-datum
     CHANGING lt_contas_item[]
              lt_zlest0021.

      IF lt_contas_item IS NOT INITIAL.

        APPEND LINES OF lt_contas_item TO lt_contas.

      ENDIF.

    ENDIF.

*    SUBTRACT <fs_bapi_item>-item_amount FROM lv_vlr_ajust.

    " 15.12.2022 - ramon -->
    SUBTRACT <fs_0034>-zvlr_perda FROM lw_bapi_header-gross_amount.
    SUBTRACT <fs_0034>-zvlr_quebra FROM lw_bapi_header-gross_amount.
    " 15.12.2022 - ramon --<

  ENDLOOP.

  lv_vlr_ajust =  i_grossamount -  lv_vlr_vi - lv_vlr_impos.


  " valor do ajuste
  READ TABLE lt_zlest0021 INTO DATA(wa_zlest0021) WITH KEY operfrete = '24'.

  IF sy-subrc EQ 0.

    APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_accdata>).

    <fs_accdata>-invoice_doc_item = '000001'.
    <fs_accdata>-item_amount      = abs( lv_vlr_ajust ).
    <fs_accdata>-comp_code        =  <fs_0034>-bukrs.
    <fs_accdata>-bus_area         =  <fs_0034>-werks.

    CONCATENATE 'Ajuste de Custo' <fs_accdata>-item_text INTO <fs_accdata>-item_text SEPARATED BY space.

    IF lv_vlr_ajust > 0.
      <fs_accdata>-gl_account = wa_zlest0021-razaocred.
      <fs_accdata>-db_cr_ind  = 'S'.
    ELSE.
      <fs_accdata>-gl_account = wa_zlest0021-razaodeb.
      <fs_accdata>-db_cr_ind  = 'H'.
    ENDIF.

    "APPEND wa_contas TO e_contas.

  ENDIF.

  SORT lt_contas BY invoice_doc_item ASCENDING.

"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata       = lw_bapi_header
    IMPORTING
      invoicedocnumber = e_belnr
      fiscalyear       = e_ghjar
    TABLES
      itemdata         = lt_bapi_item
*     ACCOUNTINGDATA   = lt_accdata
      glaccountdata    = lt_contas
*     MATERIALDATA     =
*     TAXDATA          =
      withtaxdata      = it_withtax
*     VENDORITEMSPLITDATA       =
      return           = et_return
*     EXTENSIONIN      =
*     TM_ITEMDATA      =
    .

  IF e_belnr IS NOT INITIAL AND i_commit = 'X'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    " verifica se esta sem bloqueio -->>
    IF is_nfse_001-zlspr IS INITIAL.

      WAIT UP TO 1 SECONDS.

      CALL FUNCTION 'BAPI_INCOMINGINVOICE_RELEASE'
        EXPORTING
          invoicedocnumber = e_belnr
          fiscalyear       = e_ghjar
        TABLES
          return           = et_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.

    " verifica se esta sem bloqueio -->>

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

ENDFUNCTION.
