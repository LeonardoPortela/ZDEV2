FUNCTION ZMP2I_2017_GM_ITEM_SHOW_TO_MSE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BAPI2017_GM_ITEM_SHOW) LIKE  BAPI2017_GM_ITEM_SHOW
*"  STRUCTURE  BAPI2017_GM_ITEM_SHOW
*"  CHANGING
*"     REFERENCE(MSEG) LIKE  MSEG STRUCTURE  MSEG
*"  EXCEPTIONS
*"      ERROR_CONVERTING_ISO_CODE
*"      ERROR_CONVERTING_KEYS
*"      ERROR_CONVERTING_CURR_AMOUNT
*"      ERROR_CONVERTING_NETWORK
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|4
* <BAPI_STRUCTURE>|BAPI2017_GM_ITEM_SHOW
* <SAP_STRUCTURE>|MSEG
* <INTERN_TO_EXTERN>|
* <APPEND FORM>|X

  DATA tmp_amount LIKE bapicurr-bapicurr.

* <BAPI_FIELD>|MOVE_PLANT
* <SAP_FIELD>|UMWRK
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_plant
    TO mseg-umwrk                                                   .

* <BAPI_FIELD>|MOVE_STLOC
* <SAP_FIELD>|UMLGO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_stloc
    TO mseg-umlgo                                                   .

* <BAPI_FIELD>|MOVE_BATCH
* <SAP_FIELD>|UMCHA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_batch
    TO mseg-umcha                                                   .

* <BAPI_FIELD>|MOVE_VAL_TYPE
* <SAP_FIELD>|UMBAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_val_type
    TO mseg-umbar                                                   .

* <BAPI_FIELD>|MVT_IND
* <SAP_FIELD>|KZBEW
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-mvt_ind
    TO mseg-kzbew                                                   .

* <BAPI_FIELD>|MOVE_REAS
* <SAP_FIELD>|GRUND
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_reas
    TO mseg-grund                                                   .

* <BAPI_FIELD>|RL_EST_KEY
* <SAP_FIELD>|IMKEY
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-rl_est_key
    TO mseg-imkey                                                   .

* <BAPI_FIELD>|MOVE_MAT
* <SAP_FIELD>|UMMAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_mat
    TO mseg-ummat                                                   .

* <BAPI_FIELD>|CALC_MOTIVE
* <SAP_FIELD>|BEMOT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-calc_motive
    TO mseg-bemot                                                   .

* <BAPI_FIELD>|ASSET_NO
* <SAP_FIELD>|ANLN1
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-asset_no
    TO mseg-anln1                                                   .

* <BAPI_FIELD>|SUB_NUMBER
* <SAP_FIELD>|ANLN2
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-sub_number
    TO mseg-anln2                                                   .

* <BAPI_FIELD>|RESERV_NO
* <SAP_FIELD>|RSNUM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-reserv_no
    TO mseg-rsnum                                                   .

* <BAPI_FIELD>|RES_ITEM
* <SAP_FIELD>|RSPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-res_item
    TO mseg-rspos                                                   .

* <BAPI_FIELD>|RES_TYPE
* <SAP_FIELD>|RSART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-res_type
    TO mseg-rsart                                                   .

* <BAPI_FIELD>|WITHDRAWN
* <SAP_FIELD>|KZEAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-withdrawn
    TO mseg-kzear                                                   .

* <BAPI_FIELD>|REF_DATE
* <SAP_FIELD>|DABRBZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-ref_date
    TO mseg-dabrbz                                                  .

* <BAPI_FIELD>|EXPIRYDATE
* <SAP_FIELD>|VFDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-expirydate
    TO mseg-vfdat                                                   .

* <BAPI_FIELD>|PROD_DATE
* <SAP_FIELD>|HSDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-prod_date
    TO mseg-hsdat                                                   .

* <BAPI_FIELD>|FUND
* <SAP_FIELD>|GEBER
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-fund
    TO mseg-geber                                                   .

* <BAPI_FIELD>|FUNDS_CTR
* <SAP_FIELD>|FISTL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-funds_ctr
    TO mseg-fistl                                                   .

* <BAPI_FIELD>|CMMT_ITEM
* <SAP_FIELD>|FIPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-cmmt_item
    TO mseg-fipos                                                   .

* <BAPI_FIELD>|VAL_SALES_ORD
* <SAP_FIELD>|MAT_KDAUF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-val_sales_ord
    TO mseg-mat_kdauf                                               .

* <BAPI_FIELD>|VAL_S_ORD_ITEM
* <SAP_FIELD>|MAT_KDPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-val_s_ord_item
    TO mseg-mat_kdpos                                               .

* <BAPI_FIELD>|REF_DOC_IT
* <SAP_FIELD>|LFPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-ref_doc_it
    TO mseg-lfpos                                                   .

* <BAPI_FIELD>|COST_OBJ
* <SAP_FIELD>|KSTRG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-cost_obj
    TO mseg-kstrg                                                   .

* <BAPI_FIELD>|PROFIT_SEGM_NO
* <SAP_FIELD>|PAOBJNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-profit_segm_no
    TO mseg-paobjnr                                                 .

* <BAPI_FIELD>|PROFIT_CTR
* <SAP_FIELD>|PRCTR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-profit_ctr
    TO mseg-prctr                                                   .

* <BAPI_FIELD>|NETWORK
* <SAP_FIELD>|NPLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-network
    TO mseg-nplnr                                                   .

* <BAPI_FIELD>|PART_ACCT
* <SAP_FIELD>|VPTNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-part_acct
    TO mseg-vptnr                                                   .

* <BAPI_FIELD>|REF_DOC_YR
* <SAP_FIELD>|LFBJA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-ref_doc_yr
    TO mseg-lfbja                                                   .

* <BAPI_FIELD>|REF_DOC
* <SAP_FIELD>|LFBNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-ref_doc
    TO mseg-lfbnr                                                   .

* <BAPI_FIELD>|STCK_TYPE
* <SAP_FIELD>|INSMK
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-stck_type
    TO mseg-insmk                                                   .

* <BAPI_FIELD>|SPEC_STOCK
* <SAP_FIELD>|SOBKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-spec_stock
    TO mseg-sobkz                                                   .

* <BAPI_FIELD>|VENDOR
* <SAP_FIELD>|LIFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-vendor
    TO mseg-lifnr                                                   .

* <BAPI_FIELD>|CUSTOMER
* <SAP_FIELD>|KUNNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-customer
    TO mseg-kunnr                                                   .

* <BAPI_FIELD>|SALES_ORD
* <SAP_FIELD>|KDAUF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-sales_ord
    TO mseg-kdauf                                                   .

* <BAPI_FIELD>|S_ORD_ITEM
* <SAP_FIELD>|KDPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-s_ord_item
    TO mseg-kdpos                                                   .

* <BAPI_FIELD>|SCHED_LINE
* <SAP_FIELD>|KDEIN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-sched_line
    TO mseg-kdein                                                   .

* <BAPI_FIELD>|MOVE_TYPE
* <SAP_FIELD>|BWART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-move_type
    TO mseg-bwart                                                   .

* <BAPI_FIELD>|MAT_DOC
* <SAP_FIELD>|MBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-mat_doc
    TO mseg-mblnr                                                   .

* <BAPI_FIELD>|DOC_YEAR
* <SAP_FIELD>|MJAHR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-doc_year
    TO mseg-mjahr                                                   .

* <BAPI_FIELD>|MATDOC_ITM
* <SAP_FIELD>|ZEILE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-matdoc_itm
    TO mseg-zeile                                                   .

* <BAPI_FIELD>|MATERIAL
* <SAP_FIELD>|MATNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-material
    TO mseg-matnr                                                   .

* <BAPI_FIELD>|PLANT
* <SAP_FIELD>|WERKS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-plant
    TO mseg-werks                                                   .

* <BAPI_FIELD>|STGE_LOC
* <SAP_FIELD>|LGORT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-stge_loc
    TO mseg-lgort                                                   .

* <BAPI_FIELD>|BATCH
* <SAP_FIELD>|CHARG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-batch
    TO mseg-charg                                                   .

* <BAPI_FIELD>|VAL_TYPE
* <SAP_FIELD>|BWTAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-val_type
    TO mseg-bwtar                                                   .

* <BAPI_FIELD>|SHIPPING
* <SAP_FIELD>|EVERS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-shipping
    TO mseg-evers                                                   .

* <BAPI_FIELD>|COMP_SHIP
* <SAP_FIELD>|EVERE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-comp_ship
    TO mseg-evere                                                   .

* <BAPI_FIELD>|NO_MORE_GR
* <SAP_FIELD>|ELIKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-no_more_gr
    TO mseg-elikz                                                   .

* <BAPI_FIELD>|ITEM_TEXT
* <SAP_FIELD>|SGTXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-item_text
    TO mseg-sgtxt                                                   .

* <BAPI_FIELD>|GR_RCPT
* <SAP_FIELD>|WEMPF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-gr_rcpt
    TO mseg-wempf                                                   .

* <BAPI_FIELD>|UNLOAD_PT
* <SAP_FIELD>|ABLAD
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-unload_pt
    TO mseg-ablad                                                   .

* <BAPI_FIELD>|COSTCENTER
* <SAP_FIELD>|KOSTL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-costcenter
    TO mseg-kostl                                                   .

* <BAPI_FIELD>|ORDERID
* <SAP_FIELD>|AUFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-orderid
    TO mseg-aufnr                                                   .

* <BAPI_FIELD>|ORDER_ITNO
* <SAP_FIELD>|AUFPS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-order_itno
    TO mseg-aufps                                                   .

* <BAPI_FIELD>|PO_ITEM
* <SAP_FIELD>|EBELP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-po_item
    TO mseg-ebelp                                                   .

* <BAPI_FIELD>|PO_PR_QNT
* <SAP_FIELD>|BPMNG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-po_pr_qnt
    TO mseg-bpmng                                                   .

* <BAPI_FIELD>|ENTRY_QNT
* <SAP_FIELD>|ERFMG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-entry_qnt
    TO mseg-erfmg                                                   .

* <BAPI_FIELD>|PO_NUMBER
* <SAP_FIELD>|EBELN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-po_number
    TO mseg-ebeln                                                   .

* <BAPI_FIELD>|CO_BUSPROC
* <SAP_FIELD>|PRZNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-co_busproc
    TO mseg-prznr                                                   .

* <BAPI_FIELD>|ACTTYPE
* <SAP_FIELD>|LSTAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-acttype
    TO mseg-lstar                                                   .

* <BAPI_FIELD>|SUPPL_VEND
* <SAP_FIELD>|LLIEF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-suppl_vend
    TO mseg-llief                                                   .

* <BAPI_FIELD>|X_AUTO_CRE
* <SAP_FIELD>|XAUTO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-x_auto_cre
    TO mseg-xauto                                                   .

* <BAPI_FIELD>|LINE_ID
* <SAP_FIELD>|LINE_ID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-line_id
    TO mseg-line_id                                                 .

* <BAPI_FIELD>|PARENT_ID
* <SAP_FIELD>|PARENT_ID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-parent_id
    TO mseg-parent_id                                               .

* <BAPI_FIELD>|LINE_DEPTH
* <SAP_FIELD>|LINE_DEPTH
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_show-line_depth
    TO mseg-line_depth                                              .

* <BAPI_FIELD>|CURRENCY_ISO
* <SAP_FIELD>|WAERS
* <CODE_PART>|ISO_INT_CURR
* <ADD_FIELD>|CURRENCY
  IF NOT bapi2017_gm_item_show-currency
  IS INITIAL.
    MOVE bapi2017_gm_item_show-currency
      TO mseg-waers                                                   .
  ELSEIF bapi2017_gm_item_show-currency_iso
  IS INITIAL.
    CLEAR mseg-waers                                                   .
  ELSE.
    CALL FUNCTION 'CURRENCY_CODE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_show-currency_iso
      IMPORTING
        sap_code  = mseg-waers
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a547(b1) WITH
      bapi2017_gm_item_show-currency_iso
      'CURRENCY_ISO                  '
      RAISING error_converting_iso_code.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|ENTRY_UOM_ISO
* <SAP_FIELD>|ERFME
* <CODE_PART>|ISO_INT_UNIT
* <ADD_FIELD>|ENTRY_UOM
  IF NOT bapi2017_gm_item_show-entry_uom
  IS INITIAL.
    MOVE bapi2017_gm_item_show-entry_uom
      TO mseg-erfme                                                   .
  ELSEIF bapi2017_gm_item_show-entry_uom_iso
  IS INITIAL.
    CLEAR mseg-erfme                                                   .
  ELSE.
    CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_show-entry_uom_iso
      IMPORTING
        sap_code  = mseg-erfme
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a548(b1) WITH
      bapi2017_gm_item_show-entry_uom_iso
      'ENTRY_UOM_ISO                 '
      RAISING error_converting_iso_code.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|ORDERPR_UN_ISO
* <SAP_FIELD>|BPRME
* <CODE_PART>|ISO_INT_UNIT
* <ADD_FIELD>|ORDERPR_UN
  IF NOT bapi2017_gm_item_show-orderpr_un
  IS INITIAL.
    MOVE bapi2017_gm_item_show-orderpr_un
      TO mseg-bprme                                                   .
  ELSEIF bapi2017_gm_item_show-orderpr_un_iso
  IS INITIAL.
    CLEAR mseg-bprme                                                   .
  ELSE.
    CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_show-orderpr_un_iso
      IMPORTING
        sap_code  = mseg-bprme
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a548(b1) WITH
      bapi2017_gm_item_show-orderpr_un_iso
      'ORDERPR_UN_ISO                '
      RAISING error_converting_iso_code.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|WBS_ELEM
* <SAP_FIELD>|PS_PSP_PNR
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|

  IF bapi2017_gm_item_show-wbs_elem
  IS INITIAL.
    CLEAR mseg-ps_psp_pnr                                              .
  ELSE.
    CALL FUNCTION 'PSPNUM_EXTERN_TO_INTERN_CONV'
      EXPORTING
        ext_num   = bapi2017_gm_item_show-wbs_elem
      IMPORTING
        int_num   = mseg-ps_psp_pnr
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a551(b1) WITH
      bapi2017_gm_item_show-wbs_elem
      'WBS_ELEM                      '
      RAISING error_converting_keys.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|VAL_WBS_ELEM
* <SAP_FIELD>|MAT_PSPNR
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|

  IF bapi2017_gm_item_show-val_wbs_elem
  IS INITIAL.
    CLEAR mseg-mat_pspnr                                               .
  ELSE.
    CALL FUNCTION 'PSPNUM_EXTERN_TO_INTERN_CONV'
      EXPORTING
        ext_num   = bapi2017_gm_item_show-val_wbs_elem
      IMPORTING
        int_num   = mseg-mat_pspnr
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a551(b1) WITH
      bapi2017_gm_item_show-val_wbs_elem
      'VAL_WBS_ELEM                  '
      RAISING error_converting_keys.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|AMOUNT_SV
* <SAP_FIELD>|EXVKW
* <CODE_PART>|Z_AMOUNT_FIELD
* <ADD_FIELD>|WAERS
  IF NOT bapi2017_gm_item_show-amount_sv
  IS INITIAL.
    IF mseg-waers
    IS INITIAL.
      MESSAGE e537(b1) WITH
      'MSEG-WAERS                                                   '
      bapi2017_gm_item_show-amount_sv
      'AMOUNT_SV                     '
      RAISING error_converting_curr_amount.
    ELSE.
      CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
        EXPORTING
          currency              = mseg-waers
          bapi_amount           = bapi2017_gm_item_show-amount_sv
        IMPORTING
          sap_amount            = tmp_amount
        EXCEPTIONS
          bapi_amount_incorrect = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE e536(b1) WITH
        bapi2017_gm_item_show-amount_sv
        mseg-waers
        'AMOUNT_SV                     '
        RAISING error_converting_curr_amount.
      ENDIF.
      MOVE tmp_amount
      TO mseg-exvkw                                                   .
    ENDIF.
  ELSE.
    CLEAR mseg-exvkw                                                   .
  ENDIF.

* <BAPI_FIELD>|AMOUNT_LC
* <SAP_FIELD>|EXBWR
* <CODE_PART>|Z_AMOUNT_FIELD
* <ADD_FIELD>|WAERS
  IF NOT bapi2017_gm_item_show-amount_lc
  IS INITIAL.
    IF mseg-waers
    IS INITIAL.
      MESSAGE e537(b1) WITH
      'MSEG-WAERS                                                   '
      bapi2017_gm_item_show-amount_lc
      'AMOUNT_LC                     '
      RAISING error_converting_curr_amount.
    ELSE.
      CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
        EXPORTING
          currency              = mseg-waers
          bapi_amount           = bapi2017_gm_item_show-amount_lc
        IMPORTING
          sap_amount            = tmp_amount
        EXCEPTIONS
          bapi_amount_incorrect = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE e536(b1) WITH
        bapi2017_gm_item_show-amount_lc
        mseg-waers
        'AMOUNT_LC                     '
        RAISING error_converting_curr_amount.
      ENDIF.
      MOVE tmp_amount
      TO mseg-exbwr                                                   .
    ENDIF.
  ELSE.
    CLEAR mseg-exbwr                                                   .
  ENDIF.

  PERFORM mp2i_2017_gm_item_show_to_mseg
    USING
      bapi2017_gm_item_show
    CHANGING
      mseg                          .





ENDFUNCTION.

*---------------------------------------------------------------------*
*  FORM MP2I_2017_GM_ITEM_SHOW_TO_MSEG
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  BAPI2017_GM_ITEM_SHOW
*  -->  MSEG
*---------------------------------------------------------------------*
FORM mp2i_2017_gm_item_show_to_mseg
  USING
    bapi2017_gm_item_show
    STRUCTURE bapi2017_gm_item_show
  CHANGING
    mseg
    STRUCTURE mseg                          .

  IF NOT bapi2017_gm_item_show-network IS INITIAL AND
     NOT bapi2017_gm_item_show-activity IS INITIAL.
    CALL FUNCTION 'EXT_ACTIVITY_GET_INT_ACTIVITY'
      EXPORTING
        i_ext_act_network  = bapi2017_gm_item_show-network
        i_ext_act_activity = bapi2017_gm_item_show-activity
      IMPORTING
        e_int_act_aufpl    = mseg-aufpl
        e_int_act_aplzl    = mseg-aplzl
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
  MOVE: bapi2017_gm_item_show-grant_nbr      TO mseg-grant_nbr,
        bapi2017_gm_item_show-func_area_long TO mseg-fkber.
  CALL FUNCTION 'CMMT_ITEM_CONVERSION_INBOUND'
    EXPORTING
      i_cmmt_item      = bapi2017_gm_item_show-cmmt_item
      i_cmmt_item_long = bapi2017_gm_item_show-cmmt_item_long
    IMPORTING
      e_cmmt_item      = mseg-fipos
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING error_converting_keys.
  ENDIF.

ENDFORM.                    "MP2I_2017_GM_ITEM_SHOW_TO_MSEG
