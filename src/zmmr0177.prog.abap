*&---------------------------------------------------------------------*
*& Report  ZMMR0177
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr0177.


START-OF-SELECTION.

  TYPES: BEGIN OF ty_saida,
           status            TYPE v_icon-name,
           parid             TYPE zsdt0001-parid,
           name1             TYPE lfa1-name1,
           matnr             TYPE zsdt0001-matnr,
           maktx             TYPE makt-maktx,
           ebeln(10)         TYPE c, "EKPO-EBELN,
           ebelp             TYPE ekpo-ebelp,
           waers             TYPE ekko-waers,
           bvtyp             TYPE lfbk-bvtyp,
           dt_movimento      TYPE zsdt0001-dt_movimento,
           docdat            TYPE zsdt0001-docdat,
           dt_vencimento     TYPE zmmt_ee_zgr-dt_vencimento,
           peso_fiscal       TYPE zsdt0001-peso_fiscal,
           peso_liq          TYPE zsdt0001-peso_liq,
           meins             TYPE ekpo-meins,
           nfnum_s(13)       TYPE c, "zsdt0001-nfnum e ZSDT0001-SERIES
           nfnum             TYPE zmmt0132-nfnum,
           series            TYPE zmmt0132-series,
           netwr             TYPE zsdt0001-netwr,
           icms_valor        TYPE zib_nfe_dist_itm-icms_valor,
           ipi_valor         TYPE zib_nfe_dist_itm-ipi_valor,
           pis_valor         TYPE zib_nfe_dist_itm-pis_valor,
           cof_valor         TYPE zib_nfe_dist_itm-cof_valor,
           mwskz             TYPE ekpo-mwskz,
           mm_mblnr          TYPE zmmt_ee_zgr_docs-mm_mblnr,
           mm_mjahr          TYPE zmmt_ee_zgr_docs-mm_mjahr,
           ft_belnr          TYPE zmmt_ee_zgr_docs-ft_belnr,
           ft_gjahr          TYPE zmmt_ee_zgr_docs-ft_gjahr,
           docnum            TYPE zmmt_ee_zgr_docs-docnum,
           icms_base         TYPE zib_nfe_dist_itm-icms_base,
           icms_aqt          TYPE zib_nfe_dist_itm-icms_aqt,
           icms_red_base     TYPE zib_nfe_dist_itm-icms_red_base,
           chave_nfe         TYPE zsdt0001-chave_nfe,
           ch_referencia(29) TYPE c,
           lgort             TYPE ekpo-lgort,
           webre             TYPE ekpo-webre,
           charg             TYPE eket-charg,
           zterm             TYPE ekko-zterm,
           txjcd             TYPE ekpo-txjcd,
           txz01             TYPE ekpo-txz01,
           erro_log(255)     TYPE c,
           obj_key           TYPE zmmt_ee_zgr_docs-obj_key,
           nfnum2            TYPE zmmt_ee_zgr-nfnum,
           authcod           TYPE zmmt_ee_zgr-authcod,
           nfenum            TYPE zmmt_ee_zgr-nfenum,
           docstat           TYPE zmmt_ee_zgr-docstat,
           cdv               TYPE zmmt_ee_zgr-cdv,
           amount_lc         TYPE zmmt0132-amount_lc,
           j_1bnftype        TYPE zmmt0132-j_1bnftype,
           exch_rate_v       TYPE zmmt0132-exch_rate_v,
           celltab           TYPE lvc_t_styl,
         END OF ty_saida.

  DATA: gwa_headerdata_local      TYPE bapi_incinv_create_header,
        gva_invoicedocnumber_miro TYPE bapi_incinv_fld-inv_doc_no,
        gva_ano_miro              TYPE bapi_incinv_fld-fisc_year,mber,
        git_return                TYPE TABLE OF bapiret2,
        git_taxdata               TYPE TABLE OF bapi_incinv_create_tax,
        git_withtaxdata           TYPE TABLE OF bapi_incinv_create_withtax,
        git_itemdata              TYPE TABLE OF bapi_incinv_create_item,
        git_glaccountdata         TYPE TABLE OF bapi_incinv_create_gl_account.


  IMPORT gwa_headerdata_local TO gwa_headerdata_local FROM MEMORY ID 'ZMMR0171_HEADER'.
  IMPORT git_itemdata         TO git_itemdata         FROM MEMORY ID 'ZMMR0171_ITEMDATA'.
  IMPORT git_glaccountdata    TO git_glaccountdata    FROM MEMORY ID 'ZMMR0171_ACCOUNTDATA'.
  IMPORT git_withtaxdata      TO git_withtaxdata      FROM MEMORY ID 'ZMMR0171_WITHTAXDATA'.
  IMPORT git_taxdata          TO git_taxdata          FROM MEMORY ID 'ZMMR0171_TAXDATA'.


  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata       = gwa_headerdata_local
    IMPORTING
      invoicedocnumber = gva_invoicedocnumber_miro
      fiscalyear       = gva_ano_miro
    TABLES
      itemdata         = git_itemdata
      glaccountdata    = git_glaccountdata
      withtaxdata      = git_withtaxdata
      taxdata          = git_taxdata
      return           = git_return.
  IF gva_invoicedocnumber_miro IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDIF.

  EXPORT git_return  FROM git_return TO MEMORY ID 'ZMMR0171_RETURN'.
  EXPORT gva_invoicedocnumber_miro  FROM gva_invoicedocnumber_miro TO MEMORY ID 'ZMMR0171_DOCNUMBER'.
  EXPORT  gva_ano_miro  FROM  gva_ano_miro TO MEMORY ID 'ZMMR0171_ANOMIRO'.
