FUNCTION ZMP2E_MSEG_TO_2017_GM_ITEM_SHO.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MSEG) LIKE  MSEG STRUCTURE  MSEG
*"  CHANGING
*"     REFERENCE(BAPI2017_GM_ITEM_SHOW) LIKE  BAPI2017_GM_ITEM_SHOW
*"  STRUCTURE  BAPI2017_GM_ITEM_SHOW
*"  EXCEPTIONS
*"      ERROR_CONVERTING_KEYS
*"      ERROR_CONVERTING_CURR_AMOUNT
*"      ERROR_CONVERTING_NETWORK
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|5
* <BAPI_STRUCTURE>|BAPI2017_GM_ITEM_SHOW
* <SAP_STRUCTURE>|MSEG
* <INTERN_TO_EXTERN>|X
* <APPEND FORM>|X

  DATA tmp_amount LIKE bapicurr-bapicurr.

* <BAPI_FIELD>|MOVE_PLANT
* <SAP_FIELD>|UMWRK
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-umwrk
    TO bapi2017_gm_item_show-move_plant                             .

* <BAPI_FIELD>|MOVE_STLOC
* <SAP_FIELD>|UMLGO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-umlgo
    TO bapi2017_gm_item_show-move_stloc                             .

* <BAPI_FIELD>|MOVE_BATCH
* <SAP_FIELD>|UMCHA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-umcha
    TO bapi2017_gm_item_show-move_batch                             .

* <BAPI_FIELD>|MOVE_VAL_TYPE
* <SAP_FIELD>|UMBAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-umbar
    TO bapi2017_gm_item_show-move_val_type                          .

* <BAPI_FIELD>|MVT_IND
* <SAP_FIELD>|KZBEW
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kzbew
    TO bapi2017_gm_item_show-mvt_ind                                .

* <BAPI_FIELD>|MOVE_REAS
* <SAP_FIELD>|GRUND
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-grund
    TO bapi2017_gm_item_show-move_reas                              .

* <BAPI_FIELD>|RL_EST_KEY
* <SAP_FIELD>|IMKEY
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-imkey
    TO bapi2017_gm_item_show-rl_est_key                             .

* <BAPI_FIELD>|MOVE_MAT
* <SAP_FIELD>|UMMAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-ummat
    TO bapi2017_gm_item_show-move_mat                               .

* <BAPI_FIELD>|CALC_MOTIVE
* <SAP_FIELD>|BEMOT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-bemot
    TO bapi2017_gm_item_show-calc_motive                            .

* <BAPI_FIELD>|ASSET_NO
* <SAP_FIELD>|ANLN1
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-anln1
    TO bapi2017_gm_item_show-asset_no                               .

* <BAPI_FIELD>|SUB_NUMBER
* <SAP_FIELD>|ANLN2
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-anln2
    TO bapi2017_gm_item_show-sub_number                             .

* <BAPI_FIELD>|RESERV_NO
* <SAP_FIELD>|RSNUM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-rsnum
    TO bapi2017_gm_item_show-reserv_no                              .

* <BAPI_FIELD>|RES_ITEM
* <SAP_FIELD>|RSPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-rspos
    TO bapi2017_gm_item_show-res_item                               .

* <BAPI_FIELD>|RES_TYPE
* <SAP_FIELD>|RSART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-rsart
    TO bapi2017_gm_item_show-res_type                               .

* <BAPI_FIELD>|WITHDRAWN
* <SAP_FIELD>|KZEAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kzear
    TO bapi2017_gm_item_show-withdrawn                              .

* <BAPI_FIELD>|EXPIRYDATE
* <SAP_FIELD>|VFDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-vfdat
    TO bapi2017_gm_item_show-expirydate                             .

* <BAPI_FIELD>|PROD_DATE
* <SAP_FIELD>|HSDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-hsdat
    TO bapi2017_gm_item_show-prod_date                              .

* <BAPI_FIELD>|FUND
* <SAP_FIELD>|GEBER
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-geber
    TO bapi2017_gm_item_show-fund                                   .

* <BAPI_FIELD>|FUNDS_CTR
* <SAP_FIELD>|FISTL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-fistl
    TO bapi2017_gm_item_show-funds_ctr                              .

* <BAPI_FIELD>|CMMT_ITEM
* <SAP_FIELD>|FIPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-fipos
    TO bapi2017_gm_item_show-cmmt_item                              .

* <BAPI_FIELD>|VAL_SALES_ORD
* <SAP_FIELD>|MAT_KDAUF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-mat_kdauf
    TO bapi2017_gm_item_show-val_sales_ord                          .

* <BAPI_FIELD>|VAL_S_ORD_ITEM
* <SAP_FIELD>|MAT_KDPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-mat_kdpos
    TO bapi2017_gm_item_show-val_s_ord_item                         .

* <BAPI_FIELD>|REF_DOC_IT
* <SAP_FIELD>|LFPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-lfpos
    TO bapi2017_gm_item_show-ref_doc_it                             .

* <BAPI_FIELD>|COST_OBJ
* <SAP_FIELD>|KSTRG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kstrg
    TO bapi2017_gm_item_show-cost_obj                               .

* <BAPI_FIELD>|PROFIT_SEGM_NO
* <SAP_FIELD>|PAOBJNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-paobjnr
    TO bapi2017_gm_item_show-profit_segm_no                         .

* <BAPI_FIELD>|PROFIT_CTR
* <SAP_FIELD>|PRCTR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-prctr
    TO bapi2017_gm_item_show-profit_ctr                             .

* <BAPI_FIELD>|NETWORK
* <SAP_FIELD>|NPLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-nplnr
    TO bapi2017_gm_item_show-network                                .

* <BAPI_FIELD>|PART_ACCT
* <SAP_FIELD>|VPTNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-vptnr
    TO bapi2017_gm_item_show-part_acct                              .

* <BAPI_FIELD>|REF_DOC_YR
* <SAP_FIELD>|LFBJA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-lfbja
    TO bapi2017_gm_item_show-ref_doc_yr                             .

* <BAPI_FIELD>|REF_DOC
* <SAP_FIELD>|LFBNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-lfbnr
    TO bapi2017_gm_item_show-ref_doc                                .

* <BAPI_FIELD>|ORDER_ITNO
* <SAP_FIELD>|AUFPS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-aufps
    TO bapi2017_gm_item_show-order_itno                             .

* <BAPI_FIELD>|STCK_TYPE
* <SAP_FIELD>|INSMK
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-insmk
    TO bapi2017_gm_item_show-stck_type                              .

* <BAPI_FIELD>|SPEC_STOCK
* <SAP_FIELD>|SOBKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-sobkz
    TO bapi2017_gm_item_show-spec_stock                             .

* <BAPI_FIELD>|VENDOR
* <SAP_FIELD>|LIFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-lifnr
    TO bapi2017_gm_item_show-vendor                                 .

* <BAPI_FIELD>|CUSTOMER
* <SAP_FIELD>|KUNNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kunnr
    TO bapi2017_gm_item_show-customer                               .

* <BAPI_FIELD>|SALES_ORD
* <SAP_FIELD>|KDAUF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kdauf
    TO bapi2017_gm_item_show-sales_ord                              .

* <BAPI_FIELD>|S_ORD_ITEM
* <SAP_FIELD>|KDPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kdpos
    TO bapi2017_gm_item_show-s_ord_item                             .

* <BAPI_FIELD>|SCHED_LINE
* <SAP_FIELD>|KDEIN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kdein
    TO bapi2017_gm_item_show-sched_line                             .

* <BAPI_FIELD>|MOVE_TYPE
* <SAP_FIELD>|BWART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-bwart
    TO bapi2017_gm_item_show-move_type                              .

* <BAPI_FIELD>|MAT_DOC
* <SAP_FIELD>|MBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-mblnr
    TO bapi2017_gm_item_show-mat_doc                                .

* <BAPI_FIELD>|DOC_YEAR
* <SAP_FIELD>|MJAHR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-mjahr
    TO bapi2017_gm_item_show-doc_year                               .

* <BAPI_FIELD>|MATDOC_ITM
* <SAP_FIELD>|ZEILE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-zeile
    TO bapi2017_gm_item_show-matdoc_itm                             .

* <BAPI_FIELD>|MATERIAL
* <SAP_FIELD>|MATNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-matnr
    TO bapi2017_gm_item_show-material                               .

*check if switch is active
  IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
* <BAPI_FIELD>|STK_SEGMENT
* <SAP_FIELD>|SGT_SCAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
*---> 14/06/2023 - Migração S4 - JS
*    MOVE mseg-sgt_scat TO bapi2017_gm_item_show-stk_segment
   bapi2017_gm_item_show-stk_segment = CONV #( mseg-sgt_scat ).
*<--- 14/06/2023 - Migração S4 - JS                        .
  ENDIF.

* <BAPI_FIELD>|PLANT
* <SAP_FIELD>|WERKS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-werks
    TO bapi2017_gm_item_show-plant                                  .

* <BAPI_FIELD>|STGE_LOC
* <SAP_FIELD>|LGORT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-lgort
    TO bapi2017_gm_item_show-stge_loc                               .

* <BAPI_FIELD>|BATCH
* <SAP_FIELD>|CHARG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-charg
    TO bapi2017_gm_item_show-batch                                  .

* <BAPI_FIELD>|PO_ITEM
* <SAP_FIELD>|EBELP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-ebelp
    TO bapi2017_gm_item_show-po_item                                .

* <BAPI_FIELD>|SHIPPING
* <SAP_FIELD>|EVERS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-evers
    TO bapi2017_gm_item_show-shipping                               .

* <BAPI_FIELD>|COMP_SHIP
* <SAP_FIELD>|EVERE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-evere
    TO bapi2017_gm_item_show-comp_ship                              .

* <BAPI_FIELD>|NO_MORE_GR
* <SAP_FIELD>|ELIKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-elikz
    TO bapi2017_gm_item_show-no_more_gr                             .

* <BAPI_FIELD>|ITEM_TEXT
* <SAP_FIELD>|SGTXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-sgtxt
    TO bapi2017_gm_item_show-item_text                              .

* <BAPI_FIELD>|GR_RCPT
* <SAP_FIELD>|WEMPF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-wempf
    TO bapi2017_gm_item_show-gr_rcpt                                .

* <BAPI_FIELD>|UNLOAD_PT
* <SAP_FIELD>|ABLAD
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-ablad
    TO bapi2017_gm_item_show-unload_pt                              .

* <BAPI_FIELD>|COSTCENTER
* <SAP_FIELD>|KOSTL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-kostl
    TO bapi2017_gm_item_show-costcenter                             .

* <BAPI_FIELD>|ORDERID
* <SAP_FIELD>|AUFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-aufnr
    TO bapi2017_gm_item_show-orderid                                .

* <BAPI_FIELD>|PO_NUMBER
* <SAP_FIELD>|EBELN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-ebeln
    TO bapi2017_gm_item_show-po_number                              .

* <BAPI_FIELD>|CO_BUSPROC
* <SAP_FIELD>|PRZNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-prznr
    TO bapi2017_gm_item_show-co_busproc                             .

* <BAPI_FIELD>|ACTTYPE
* <SAP_FIELD>|LSTAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-lstar
    TO bapi2017_gm_item_show-acttype                                .

* <BAPI_FIELD>|SUPPL_VEND
* <SAP_FIELD>|LLIEF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-llief
    TO bapi2017_gm_item_show-suppl_vend                             .

* <BAPI_FIELD>|X_AUTO_CRE
* <SAP_FIELD>|XAUTO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-xauto
    TO bapi2017_gm_item_show-x_auto_cre                             .

* <BAPI_FIELD>|VAL_TYPE
* <SAP_FIELD>|BWTAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-bwtar
    TO bapi2017_gm_item_show-val_type                               .

* <BAPI_FIELD>|ENTRY_QNT
* <SAP_FIELD>|ERFMG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-erfmg
    TO bapi2017_gm_item_show-entry_qnt                              .

* <BAPI_FIELD>|PO_PR_QNT
* <SAP_FIELD>|BPMNG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-bpmng
    TO bapi2017_gm_item_show-po_pr_qnt                              .

* <BAPI_FIELD>|LINE_ID
* <SAP_FIELD>|LINE_ID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-line_id
    TO bapi2017_gm_item_show-line_id                                .

* <BAPI_FIELD>|PARENT_ID
* <SAP_FIELD>|PARENT_ID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-parent_id
    TO bapi2017_gm_item_show-parent_id                              .

* <BAPI_FIELD>|LINE_DEPTH
* <SAP_FIELD>|LINE_DEPTH
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mseg-line_depth
    TO bapi2017_gm_item_show-line_depth                             .

* <BAPI_FIELD>|CURRENCY_ISO
* <SAP_FIELD>|WAERS
* <CODE_PART>|ISO_INT_CURR
* <ADD_FIELD>|CURRENCY
  MOVE mseg-waers
    TO bapi2017_gm_item_show-currency                               .
  IF NOT
  mseg-waers
  IS INITIAL.
    CALL FUNCTION 'CURRENCY_CODE_SAP_TO_ISO'
      EXPORTING
        sap_code  = mseg-waers
      IMPORTING
        iso_code  = bapi2017_gm_item_show-currency_iso
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
    CLEAR bapi2017_gm_item_show-currency_iso                           .
    ENDIF.
  ELSE.
    CLEAR bapi2017_gm_item_show-currency_iso                           .
  ENDIF.

* <BAPI_FIELD>|ENTRY_UOM_ISO
* <SAP_FIELD>|ERFME
* <CODE_PART>|ISO_INT_UNIT
* <ADD_FIELD>|ENTRY_UOM
  MOVE mseg-erfme
    TO bapi2017_gm_item_show-entry_uom                              .
  IF NOT
  mseg-erfme
  IS INITIAL.
    CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
      EXPORTING
        sap_code    = mseg-erfme
      IMPORTING
        iso_code    = bapi2017_gm_item_show-entry_uom_iso
      EXCEPTIONS
        not_found   = 1
        no_iso_code = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
    CLEAR bapi2017_gm_item_show-entry_uom_iso                          .
    ENDIF.
  ELSE.
    CLEAR bapi2017_gm_item_show-entry_uom_iso                          .
  ENDIF.

* <BAPI_FIELD>|ORDERPR_UN_ISO
* <SAP_FIELD>|BPRME
* <CODE_PART>|ISO_INT_UNIT
* <ADD_FIELD>|ORDERPR_UN
  MOVE mseg-bprme
    TO bapi2017_gm_item_show-orderpr_un                             .
  IF NOT
  mseg-bprme
  IS INITIAL.
    CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
      EXPORTING
        sap_code    = mseg-bprme
      IMPORTING
        iso_code    = bapi2017_gm_item_show-orderpr_un_iso
      EXCEPTIONS
        not_found   = 1
        no_iso_code = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
    CLEAR bapi2017_gm_item_show-orderpr_un_iso                         .
    ENDIF.
  ELSE.
    CLEAR bapi2017_gm_item_show-orderpr_un_iso                         .
  ENDIF.

* <BAPI_FIELD>|WBS_ELEM
* <SAP_FIELD>|PS_PSP_PNR
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|
  IF mseg-ps_psp_pnr
  IS INITIAL.
    CLEAR bapi2017_gm_item_show-wbs_elem                               .
  ELSE.
    CALL FUNCTION 'PSPNUM_INTERN_TO_EXTERN_CONV'
         EXPORTING
*           EDIT_IMP  = ' '
              int_num   =
         mseg-ps_psp_pnr
         IMPORTING
              ext_num   =
         bapi2017_gm_item_show-wbs_elem
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a552(b1) WITH
      mseg-ps_psp_pnr
      'PS_PSP_PNR                    '
      RAISING error_converting_keys.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|VAL_WBS_ELEM
* <SAP_FIELD>|MAT_PSPNR
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|
  IF mseg-mat_pspnr
  IS INITIAL.
    CLEAR bapi2017_gm_item_show-val_wbs_elem                           .
  ELSE.
    CALL FUNCTION 'PSPNUM_INTERN_TO_EXTERN_CONV'
         EXPORTING
*           EDIT_IMP  = ' '
              int_num   =
         mseg-mat_pspnr
         IMPORTING
              ext_num   =
         bapi2017_gm_item_show-val_wbs_elem
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a552(b1) WITH
      mseg-mat_pspnr
      'MAT_PSPNR                     '
      RAISING error_converting_keys.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|AMOUNT_SV
* <SAP_FIELD>|EXVKW
* <CODE_PART>|Z_AMOUNT_FIELD
* <ADD_FIELD>|WAERS
  IF NOT mseg-exvkw
  IS INITIAL.
    MOVE mseg-vkwrt
    TO tmp_amount.
    IF mseg-waers
    IS INITIAL.
      MESSAGE e537(b1) WITH
      'MSEG-WAERS                                                   '
      tmp_amount
      'EXVKW                         '
      RAISING error_converting_curr_amount.
    ELSE.
      CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
        EXPORTING
          currency    = mseg-waers
          sap_amount  = tmp_amount
        IMPORTING
          bapi_amount = bapi2017_gm_item_show-amount_sv
        EXCEPTIONS
          OTHERS      = 0.
    ENDIF.
  ELSE.
    CLEAR bapi2017_gm_item_show-amount_sv                              .
  ENDIF.

* <BAPI_FIELD>|AMOUNT_LC
* <SAP_FIELD>|EXBWR
* <CODE_PART>|Z_AMOUNT_FIELD
* <ADD_FIELD>|WAERS
  IF NOT mseg-exbwr
  IS INITIAL.
    MOVE mseg-exbwr
    TO tmp_amount.
    IF mseg-waers
    IS INITIAL.
      MESSAGE e537(b1) WITH
      'MSEG-WAERS                                                   '
      tmp_amount
      'EXBWR                         '
      RAISING error_converting_curr_amount.
    ELSE.
      CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
        EXPORTING
          currency    = mseg-waers
          sap_amount  = tmp_amount
        IMPORTING
          bapi_amount = bapi2017_gm_item_show-amount_lc
        EXCEPTIONS
          OTHERS      = 0.
    ENDIF.
  ELSE.
    CLEAR bapi2017_gm_item_show-amount_lc                              .
  ENDIF.

  PERFORM mp2e_mseg_to_2017_gm_item_show
    USING
      mseg
    CHANGING
      bapi2017_gm_item_show         .





ENDFUNCTION.

*---------------------------------------------------------------------*
*  FORM MP2E_MSEG_TO_2017_GM_ITEM_SHOW
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  MSEG
*  -->  BAPI2017_GM_ITEM_SHOW
*---------------------------------------------------------------------*
FORM mp2e_mseg_to_2017_gm_item_show
  USING
    mseg
    STRUCTURE mseg
  CHANGING
    bapi2017_gm_item_show
    STRUCTURE bapi2017_gm_item_show         .

  IF NOT mseg-aufpl IS INITIAL AND NOT mseg-aplzl IS INITIAL.   "1054715
    CALL FUNCTION 'INT_ACTIVITY_GET_EXT_ACTIVITY'
      EXPORTING
        i_int_act_aufpl    = mseg-aufpl
        i_int_act_aplzl    = mseg-aplzl
      IMPORTING
        e_ext_act_network  = bapi2017_gm_item_show-network
        e_ext_act_activity = bapi2017_gm_item_show-activity
      EXCEPTIONS
        not_found          = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE e297(m7) WITH bapi2017_gm_item_show-network
                            bapi2017_gm_item_show-activity
                       RAISING error_converting_network.
*   Netzplan &, Aktivität & nicht vorhanden
    ENDIF.
  ENDIF.

*----- Convert Long FM Fields & Grant ---------------------------------*
  MOVE: mseg-grant_nbr TO bapi2017_gm_item_show-grant_nbr,
        mseg-fkber     TO bapi2017_gm_item_show-func_area_long.
  CALL FUNCTION 'CMMT_ITEM_CONVERSION_OUTBOUND'
    EXPORTING
      i_cmmt_item      = mseg-fipos
    IMPORTING
      e_cmmt_item_long = bapi2017_gm_item_show-cmmt_item_long
      e_cmmt_item      = bapi2017_gm_item_show-cmmt_item
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           RAISING error_converting_keys.
  ENDIF.

ENDFORM.                    "MP2E_MSEG_TO_2017_GM_ITEM_SHOW
