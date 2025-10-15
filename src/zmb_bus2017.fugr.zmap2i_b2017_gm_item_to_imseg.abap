FUNCTION ZMAP2I_B2017_GM_ITEM_TO_IMSEG.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BAPI2017_GM_ITEM_CREATE)
*"  LIKE  BAPI2017_GM_ITEM_CREATE STRUCTURE  BAPI2017_GM_ITEM_CREATE
*"  CHANGING
*"     REFERENCE(IMSEG) LIKE  IMSEG STRUCTURE  IMSEG
*"  EXCEPTIONS
*"      ERROR_CONVERTING_ISO_CODE
*"      ERROR_CONVERTING_KEYS
*"      ERROR_CONVERTING_CURR_AMOUNT
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|4
* <BAPI_STRUCTURE>|BAPI2017_GM_ITEM_CREATE
* <SAP_STRUCTURE>|IMSEG
* <INTERN_TO_EXTERN>|
* <APPEND FORM>|X

  DATA tmp_amount LIKE bapicurr-bapicurr.
  DATA lv_wbs_elem(24) TYPE c.                              "n1294212

* <BAPI_FIELD>|CMMT_ITEM
* <SAP_FIELD>|FIPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-vendrbatch
    TO imseg-licha                                                  .

* <BAPI_FIELD>|CMMT_ITEM
* <SAP_FIELD>|FIPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-cmmt_item
    TO imseg-fipos                                                  .

* <BAPI_FIELD>|FUNDS_CTR
* <SAP_FIELD>|FISTL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-funds_ctr
    TO imseg-fistl                                                  .

* <BAPI_FIELD>|FUND
* <SAP_FIELD>|GEBER
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-fund
    TO imseg-geber.

* <BAPI_FIELD>|EARMARKED_NUMBER
* <SAP_FIELD>|KBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-earmarked_number
    TO imseg-kblnr.

* <BAPI_FIELD>|EARMARKED_ITEM
* <SAP_FIELD>|KBLPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-earmarked_item
    TO imseg-kblpos.

* <BAPI_FIELD>|PROD_DATE
* <SAP_FIELD>|HSDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-prod_date
    TO imseg-hsdat                                                  .

* <BAPI_FIELD>|EXPIRYDATE
* <SAP_FIELD>|VFDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-expirydate
    TO imseg-vfdat                                                  .

* <BAPI_FIELD>|XSTOB
* <SAP_FIELD>|XSTOR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-xstob
    TO imseg-xstob                                                  .

* <BAPI_FIELD>|IND_PROPOSE_QUANX
* <SAP_FIELD>|XMEVO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-ind_propose_quanx
    TO imseg-xmevo                                                  .

* <BAPI_FIELD>|GL_ACCOUNT
* <SAP_FIELD>|KONTO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-gl_account
    TO imseg-konto                                                  .

* <BAPI_FIELD>|VAL_S_ORD_ITEM
* <SAP_FIELD>|MAT_KDPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-val_s_ord_item
    TO imseg-mat_kdpos                                              .

* <BAPI_FIELD>|VAL_SALES_ORD
* <SAP_FIELD>|MAT_KDAUF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-val_sales_ord
    TO imseg-mat_kdauf                                              .

* <BAPI_FIELD>|PROFIT_CTR
* <SAP_FIELD>|PRCTR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-profit_ctr
    TO imseg-prctr                                                  .

* <BAPI_FIELD>|PROFIT_SEGM_NO
* <SAP_FIELD>|PAOBJNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-profit_segm_no
    TO imseg-paobjnr                                                .

* <BAPI_FIELD>|COST_OBJ
* <SAP_FIELD>|KSTRG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-cost_obj
    TO imseg-kstrg                                                  .

* <BAPI_FIELD>|RL_EST_KEY
* <SAP_FIELD>|IMKEY
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-rl_est_key
    TO imseg-imkey                                                  .

* <BAPI_FIELD>|REF_DATE                                "note 548459
* <SAP_FIELD>DABRZ|                                    "note 548459
  MOVE bapi2017_gm_item_create-ref_date                  "note 548459
    TO imseg-dabrz.                                      "note 548459

* <BAPI_FIELD>|MOVE_REAS
* <SAP_FIELD>|GRUND
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_reas
    TO imseg-grund                                                  .

* <BAPI_FIELD>|REF_DOC_IT
* <SAP_FIELD>|LFPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-ref_doc_it
    TO imseg-lfpos                                                  .

* <BAPI_FIELD>|REF_DOC
* <SAP_FIELD>|LFBNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-ref_doc
    TO imseg-lfbnr                                                  .

* <BAPI_FIELD>|REF_DOC_YR
* <SAP_FIELD>|LFBJA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-ref_doc_yr
    TO imseg-lfbja                                                  .

* <BAPI_FIELD>|PART_ACCT
* <SAP_FIELD>|VPTNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-part_acct
    TO imseg-vptnr                                                  .

* <BAPI_FIELD>|NETWORK
* <SAP_FIELD>|NPLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-network
    TO imseg-nplnr                                                  .

* <BAPI_FIELD>|DELIV_NUMB_TO_SEARCH
* <SAP_FIELD>|VLIEF_AVIS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-deliv_numb_to_search
    TO imseg-vlief_avis                                             .

* <BAPI_FIELD>|STGE_TYPE_ST
* <SAP_FIELD>|UMLGT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_type_st
    TO imseg-umlgt                                                  .

* <BAPI_FIELD>|GR_NUMBER
* <SAP_FIELD>|WENUM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-gr_number
    TO imseg-wenum                                                  .

* <BAPI_FIELD>|NO_PST_CHGNT
* <SAP_FIELD>|KZKUB
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-no_pst_chgnt
    TO imseg-kzkub                                                  .

* <BAPI_FIELD>|STGE_BIN_PC
* <SAP_FIELD>|UBLGP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_bin_pc
    TO imseg-ublgp                                                  .

* <BAPI_FIELD>|STGE_TYPE_PC
* <SAP_FIELD>|UBTYP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_type_pc
    TO imseg-ubtyp                                                  .

* <BAPI_FIELD>|NO_TRANSFER_REQ
* <SAP_FIELD>|TBPKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-no_transfer_req
    TO imseg-tbpkz                                                  .

* <BAPI_FIELD>|MATYEAR_TR_CANCEL
* <SAP_FIELD>|TBBJR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-matyear_tr_cancel
    TO imseg-tbbjr                                                  .

* <BAPI_FIELD>|MATITEM_TR_CANCEL
* <SAP_FIELD>|TBBPO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-matitem_tr_cancel
    TO imseg-tbbpo                                                  .

* <BAPI_FIELD>|MATDOC_TR_CANCEL
* <SAP_FIELD>|TBBEL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-matdoc_tr_cancel
    TO imseg-tbbel                                                  .

* <BAPI_FIELD>|STGE_BIN_ST
* <SAP_FIELD>|UMLGP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_bin_st
    TO imseg-umlgp                                                  .

* <BAPI_FIELD>|SU_PL_STCK_1
* <SAP_FIELD>|ANZL1
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-su_pl_stck_1
    TO imseg-anzl1                                                  .

* <BAPI_FIELD>|STGE_BIN
* <SAP_FIELD>|LGPLA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_bin
    TO imseg-lgpla                                                  .

* <BAPI_FIELD>|STGE_TYPE
* <SAP_FIELD>|LGTYP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_type
    TO imseg-lgtyp                                                  .

* <BAPI_FIELD>|SERIALNO_AUTO_NUMBERASSIGNMENT
* <SAP_FIELD>|XSAUT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-serialno_auto_numberassignment
    TO imseg-xsaut                                                  .

* <BAPI_FIELD>|DELIV_ITEM_TO_SEARCH
* <SAP_FIELD>|VBELP_AVIS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-deliv_item_to_search
    TO imseg-vbelp_avis                                             .

* <BAPI_FIELD>|UNITTYPE_2
* <SAP_FIELD>|LETY2
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-unittype_2
    TO imseg-lety2                                                  .

* <BAPI_FIELD>|ST_UN_QTYY_2
* <SAP_FIELD>|LMEN2
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-st_un_qtyy_2
    TO imseg-lmen2                                                  .

* <BAPI_FIELD>|SU_PL_STCK_2
* <SAP_FIELD>|ANZL2
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-su_pl_stck_2
    TO imseg-anzl2                                                  .

* <BAPI_FIELD>|UNITTYPE_1
* <SAP_FIELD>|LETY1
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-unittype_1
    TO imseg-lety1                                                  .

* <BAPI_FIELD>|ST_UN_QTYY_1
* <SAP_FIELD>|LMEN1
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-st_un_qtyy_1
    TO imseg-lmen1                                                  .

* <BAPI_FIELD>|PO_NUMBER
* <SAP_FIELD>|EBELN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-po_number
    TO imseg-ebeln                                                  .

* <BAPI_FIELD>|PO_PR_QNT
* <SAP_FIELD>|BPMNG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-po_pr_qnt
    TO imseg-bpmng                                                  .

* <BAPI_FIELD>|ENTRY_QNT
* <SAP_FIELD>|ERFMG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-entry_qnt
    TO imseg-erfmg                                                  .

* <BAPI_FIELD>|VAL_TYPE
* <SAP_FIELD>|BWTAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-val_type
    TO imseg-bwtar                                                  .

* <BAPI_FIELD>|SCHED_LINE
* <SAP_FIELD>|KDEIN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-sched_line
    TO imseg-kdein                                                  .

* <BAPI_FIELD>|ITEM_TEXT
* <SAP_FIELD>|SGTXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-item_text
    TO imseg-sgtxt                                                  .

* <BAPI_FIELD>|NO_MORE_GR
* <SAP_FIELD>|ELIKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-no_more_gr
    TO imseg-elikz                                                  .

* <BAPI_FIELD>|COMP_SHIP
* <SAP_FIELD>|EVERE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-comp_ship
    TO imseg-evere                                                  .

* <BAPI_FIELD>|SHIPPING
* <SAP_FIELD>|EVERS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-shipping
    TO imseg-evers                                                  .

* <BAPI_FIELD>|PO_ITEM
* <SAP_FIELD>|EBELP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-po_item
    TO imseg-ebelp                                                  .

* <BAPI_FIELD>|S_ORD_ITEM
* <SAP_FIELD>|KDPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-s_ord_item
    TO imseg-kdpos                                                  .

* <BAPI_FIELD>|MOVE_TYPE
* <SAP_FIELD>|BWART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_type
    TO imseg-bwart                                                  .

* <BAPI_FIELD>|BATCH
* <SAP_FIELD>|CHARG
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-batch
    TO imseg-charg                                                  .

* <BAPI_FIELD>|STGE_LOC
* <SAP_FIELD>|LGORT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stge_loc
    TO imseg-lgort                                                  .

* <BAPI_FIELD>|PLANT
* <SAP_FIELD>|WERKS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-plant
    TO imseg-werks                                                  .

* <BAPI_FIELD>|MATERIAL
* <SAP_FIELD>|MATNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-material
    TO imseg-matnr                                                  .

*check if switch is active
  IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
* <BAPI_FIELD>|STK_SEGMENT
* <SAP_FIELD>|SGT_SCAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stk_segment
     TO imseg-sgt_scat                                              .
  ENDIF.

   IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
* <BAPI_FIELD>|MOVE_SEGMENT
* <SAP_FIELD>|SGT_UMSCAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_segment
     TO imseg-sgt_umscat                                              .
  ENDIF.

* <BAPI_FIELD>|SALES_ORD
* <SAP_FIELD>|KDAUF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-sales_ord
    TO imseg-kdauf                                                  .

* <BAPI_FIELD>|CUSTOMER
* <SAP_FIELD>|KUNNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-customer
    TO imseg-kunnr                                                  .

* <BAPI_FIELD>|VENDOR
* <SAP_FIELD>|LIFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-vendor
    TO imseg-lifnr                                                  .

* <BAPI_FIELD>|SPEC_STOCK
* <SAP_FIELD>|SOBKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-spec_stock
    TO imseg-sobkz                                                  .

* <BAPI_FIELD>|STCK_TYPE
* <SAP_FIELD>|INSMK
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-stck_type
    TO imseg-insmk                                                  .

* <BAPI_FIELD>|GR_RCPT
* <SAP_FIELD>|WEMPF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-gr_rcpt
    TO imseg-wempf                                                  .

* <BAPI_FIELD>|MOVE_MAT
* <SAP_FIELD>|UMMAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_mat
    TO imseg-ummat                                                  .

* <BAPI_FIELD>|WITHDRAWN
* <SAP_FIELD>|KZEAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-withdrawn
    TO imseg-kzear                                                  .

* <BAPI_FIELD>|RES_TYPE
* <SAP_FIELD>|RSART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-res_type
    TO imseg-rsart                                                  .

* <BAPI_FIELD>|RES_ITEM
* <SAP_FIELD>|RSPOS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-res_item
    TO imseg-rspos                                                  .

* <BAPI_FIELD>|RESERV_NO
* <SAP_FIELD>|RSNUM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-reserv_no
    TO imseg-rsnum                                                  .

* <BAPI_FIELD>|MVT_IND
* <SAP_FIELD>|KZBEW
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-mvt_ind
    TO imseg-kzbew                                                  .

* <BAPI_FIELD>|MOVE_VAL_TYPE
* <SAP_FIELD>|UMBAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_val_type
    TO imseg-umbar                                                  .

* <BAPI_FIELD>|MOVE_BATCH
* <SAP_FIELD>|UMCHA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_batch
    TO imseg-umcha                                                  .

* <BAPI_FIELD>|MOVE_STLOC
* <SAP_FIELD>|UMLGO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_stloc
    TO imseg-umlgo                                                  .

* <BAPI_FIELD>|MOVE_PLANT
* <SAP_FIELD>|UMWRK
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-move_plant
    TO imseg-umwrk                                                  .

* <BAPI_FIELD>|SUB_NUMBER
* <SAP_FIELD>|ANLN2
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-sub_number
    TO imseg-anln2                                                  .

* <BAPI_FIELD>|CALC_MOTIVE
* <SAP_FIELD>|BEMOT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-calc_motive
    TO imseg-bemot                                                  .

* <BAPI_FIELD>|ORDER_ITNO
* <SAP_FIELD>|AUFPS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-order_itno
    TO imseg-aufps                                                  .

* <BAPI_FIELD>|ASSET_NO
* <SAP_FIELD>|ANLN1
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-asset_no
    TO imseg-anln1                                                  .

* <BAPI_FIELD>|ORDERID
* <SAP_FIELD>|AUFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-orderid
    TO imseg-aufnr                                                  .

* <BAPI_FIELD>|COSTCENTER
* <SAP_FIELD>|KOSTL
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-costcenter
    TO imseg-kostl                                                  .

* <BAPI_FIELD>|UNLOAD_PT
* <SAP_FIELD>|ABLAD
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-unload_pt
    TO imseg-ablad                                                  .

* <BAPI_FIELD>|CO_BUSPROC
* <SAP_FIELD>|PRZNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-co_busproc
    TO imseg-prznr                                                   .

* <BAPI_FIELD>|ACTTYPE
* <SAP_FIELD>|LSTAR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-acttype
    TO imseg-lstar                                                   .

* <BAPI_FIELD>|SUPPL_VEND
* <SAP_FIELD>|LLIEF
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-suppl_vend
    TO imseg-llief.

* <BAPI_FIELD>|LINE_ID
* <SAP_FIELD>|LINE_ID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-line_id
    TO imseg-line_id.

* Global counter                                              "n1278707
  MOVE bapi2017_gm_item_create-line_id                      "n1278707
    TO imseg-global_counter.                                "n1278707

* <BAPI_FIELD>|PARENT_ID
* <SAP_FIELD>|PARENT_ID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-parent_id
    TO imseg-parent_id.

* <BAPI_FIELD>|LINE_DEPTH
* <SAP_FIELD>|LINE_DEPTH
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-line_depth
    TO imseg-line_depth.

* <BAPI_FIELD>|QUANTITY
* <SAP_FIELD>|MENGE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_item_create-quantity
    TO imseg-menge.

*Functional Area
  MOVE bapi2017_gm_item_create-func_area
    TO imseg-fkber.
*Trading partner's business area
  MOVE bapi2017_gm_item_create-tr_part_ba
    TO imseg-pargb.
*Clearing company code
  MOVE bapi2017_gm_item_create-par_compco
    TO imseg-parbu.
*Delivery
  MOVE bapi2017_gm_item_create-deliv_numb
    TO imseg-vbeln.
*Delivery item
  MOVE bapi2017_gm_item_create-deliv_item
    TO imseg-posnr.

  MOVE bapi2017_gm_item_create-unload_ptx
    TO imseg-abladx.

  MOVE bapi2017_gm_item_create-gr_rcptx
    TO imseg-wempfx.
*Number of GR/GI slips to be printed
  MOVE bapi2017_gm_item_create-nb_slips
    TO imseg-weanz.

  MOVE bapi2017_gm_item_create-nb_slipsx
    TO imseg-weanzx.
*                       end of note 356665

* Special movement indicator for warehouse management
  MOVE bapi2017_gm_item_create-spec_mvmt
    TO imseg-bsskz.

* <BAPI_FIELD>|ORDERPR_UN_ISO
* <SAP_FIELD>|BPRME
* <CODE_PART>|ISO_INT_UNIT
* <ADD_FIELD>|ORDERPR_UN
  IF NOT bapi2017_gm_item_create-orderpr_un
  IS INITIAL.
    MOVE bapi2017_gm_item_create-orderpr_un
      TO imseg-bprme                                                  .
  ELSEIF bapi2017_gm_item_create-orderpr_un_iso
  IS INITIAL.
    CLEAR imseg-bprme                                                  .
  ELSE.
    CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_create-orderpr_un_iso
      IMPORTING
        sap_code  = imseg-bprme
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a548(b1) WITH
      bapi2017_gm_item_create-orderpr_un_iso
      'ORDERPR_UN_ISO                '
      RAISING error_converting_iso_code.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|ENTRY_UOM_ISO
* <SAP_FIELD>|ERFME
* <CODE_PART>|ISO_INT_UNIT
* <ADD_FIELD>|ENTRY_UOM
  IF NOT bapi2017_gm_item_create-entry_uom
  IS INITIAL.
    MOVE bapi2017_gm_item_create-entry_uom
      TO imseg-erfme                                                  .
  ELSEIF bapi2017_gm_item_create-entry_uom_iso
  IS INITIAL.
    CLEAR imseg-erfme                                                  .
  ELSE.
    CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_create-entry_uom_iso
      IMPORTING
        sap_code  = imseg-erfme
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a548(b1) WITH
      bapi2017_gm_item_create-entry_uom_iso
      'ENTRY_UOM_ISO                 '
      RAISING error_converting_iso_code.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|ST_UN_QTYY_1_ISO
* <SAP_FIELD>|MEINS
* <CODE_PART>|ISO_UNIT
* <ADD_FIELD>|
  IF NOT
  bapi2017_gm_item_create-st_un_qtyy_1_iso
  IS INITIAL.
    CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_create-st_un_qtyy_1_iso
      IMPORTING
        sap_code  = imseg-meins
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a548(b1) WITH
      bapi2017_gm_item_create-st_un_qtyy_1_iso
      'ST_UN_QTYY_1_ISO              '
      RAISING error_converting_iso_code.
    ENDIF.
  ELSE.
    CLEAR
    imseg-meins                                                  .
  ENDIF.

* <BAPI_FIELD>|ST_UN_QTYY_2_ISO
* <SAP_FIELD>|MEINS
* <CODE_PART>|ISO_UNIT
* <ADD_FIELD>|
  IF NOT
  bapi2017_gm_item_create-st_un_qtyy_2_iso
  IS INITIAL.
    CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = bapi2017_gm_item_create-st_un_qtyy_2_iso
      IMPORTING
        sap_code  = imseg-meins
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE a548(b1) WITH
      bapi2017_gm_item_create-st_un_qtyy_2_iso
      'ST_UN_QTYY_2_ISO              '
      RAISING error_converting_iso_code.
    ENDIF.
  ELSE.
    CLEAR
    imseg-meins                                                  .
  ENDIF.

* <BAPI_FIELD>|BASE_UOM                         "1151057
* <SAP_FIELD>|MEINS                             "1151057
* <CODE_PART>|A_MOVE                            "1151057
* <ADD_FIELD>|                                  "1151057
  IF imseg-meins IS INITIAL.                                "1352414
    MOVE bapi2017_gm_item_create-base_uom                   "1151057
      TO imseg-meins.                                       "1151057
  ENDIF.                                                    "1352414

* <BAPI_FIELD>|WBS_ELEM
* <SAP_FIELD>|PS_PSP_PNR
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|

  IF bapi2017_gm_item_create-wbs_elem
  IS INITIAL.
    CLEAR imseg-ps_psp_pnr                                             .
  ELSE.
    CALL FUNCTION 'PSPNUM_EXTERN_TO_INTERN_CONV'
      EXPORTING
        ext_num   = bapi2017_gm_item_create-wbs_elem
      IMPORTING
        int_num   = imseg-ps_psp_pnr
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MOVE bapi2017_gm_item_create-wbs_elem                 "n1294212
      TO lv_wbs_elem.                                       "n1294212
      MESSAGE a551(b1) WITH
*   BAPI2017_GM_ITEM_CREATE-WBS_ELEM                          "n1294212
      lv_wbs_elem                                           "n1294212
      'WBS_ELEM                      '
      RAISING error_converting_keys.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|VAL_WBS_ELEM
* <SAP_FIELD>|MAT_PSPNR
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|

  IF bapi2017_gm_item_create-val_wbs_elem
  IS INITIAL.
    CLEAR imseg-mat_pspnr                                              .
  ELSE.
    CALL FUNCTION 'PSPNUM_EXTERN_TO_INTERN_CONV'
      EXPORTING
        ext_num   = bapi2017_gm_item_create-val_wbs_elem
      IMPORTING
        int_num   = imseg-mat_pspnr
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MOVE bapi2017_gm_item_create-val_wbs_elem             "n1294212
      TO lv_wbs_elem.                                       "n1294212
      MESSAGE a551(b1) WITH
*   BAPI2017_GM_ITEM_CREATE-VAL_WBS_ELEM                      "n1294212
      lv_wbs_elem                                           "n1294212
      'VAL_WBS_ELEM                  '
      RAISING error_converting_keys.
    ENDIF.
  ENDIF.

* <BAPI_FIELD>|AMOUNT_SV
* <SAP_FIELD>|EXVKW
* <CODE_PART>|Z_AMOUNT_FIELD
* <ADD_FIELD>|WAERS
  IF NOT bapi2017_gm_item_create-amount_sv
  IS INITIAL.
    IF imseg-waers
    IS INITIAL.
      MESSAGE e537(b1) WITH
      'IMSEG-WAERS                                                  '
      bapi2017_gm_item_create-amount_sv
      'AMOUNT_SV                     '
      RAISING error_converting_curr_amount.
    ELSE.
      CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
        EXPORTING
          currency              = imseg-waers
          bapi_amount           = bapi2017_gm_item_create-amount_sv
        IMPORTING
          sap_amount            = tmp_amount
        EXCEPTIONS
          bapi_amount_incorrect = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE e536(b1) WITH
        bapi2017_gm_item_create-amount_sv
        imseg-waers
        'AMOUNT_SV                     '
        RAISING error_converting_curr_amount.
      ENDIF.
      MOVE tmp_amount
      TO imseg-exvkw                                                  .
    ENDIF.
  ELSE.
    CLEAR imseg-exvkw                                                  .
  ENDIF.

* <BAPI_FIELD>|AMOUNT_LC
* <SAP_FIELD>|EXBWR
* <CODE_PART>|Z_AMOUNT_FIELD
* <ADD_FIELD>|WAERS
  IF NOT bapi2017_gm_item_create-amount_lc
  IS INITIAL.
    IF imseg-waers
    IS INITIAL.
      MESSAGE e537(b1) WITH
      'IMSEG-WAERS                                                  '
      bapi2017_gm_item_create-amount_lc
      'AMOUNT_LC                     '
      RAISING error_converting_curr_amount.
    ELSE.
      CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
        EXPORTING
          currency              = imseg-waers
          bapi_amount           = bapi2017_gm_item_create-amount_lc
        IMPORTING
          sap_amount            = tmp_amount
        EXCEPTIONS
          bapi_amount_incorrect = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE e536(b1) WITH
        bapi2017_gm_item_create-amount_lc
        imseg-waers
        'AMOUNT_LC                     '
        RAISING error_converting_curr_amount.
      ENDIF.
      MOVE tmp_amount
      TO imseg-exbwr                                                  .
    ENDIF.
  ELSE.
    CLEAR imseg-exbwr                                                  .
  ENDIF.

  PERFORM map2i_b2017_gm_item_to_imseg
    USING
      bapi2017_gm_item_create
    CHANGING
      imseg                         .





ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  MAP2I_B2017_GM_ITEM_TO_IMSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->BAPI2017_GM_ITEM_CREATE  text
*      -->IMSEG                    text
*----------------------------------------------------------------------*
FORM map2i_b2017_gm_item_to_imseg
  USING
    bapi2017_gm_item_create
    STRUCTURE bapi2017_gm_item_create
  CHANGING
    imseg
    STRUCTURE imseg                         .

  IF NOT bapi2017_gm_item_create-network IS INITIAL AND
     NOT bapi2017_gm_item_create-activity IS INITIAL.
    CALL FUNCTION 'EXT_ACTIVITY_GET_INT_ACTIVITY'
      EXPORTING
        i_ext_act_network  = bapi2017_gm_item_create-network
        i_ext_act_activity = bapi2017_gm_item_create-activity
      IMPORTING
        e_int_act_aufpl    = imseg-aufpl
        e_int_act_aplzl    = imseg-aplzl
      EXCEPTIONS
        not_found          = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE e297(m7) WITH bapi2017_gm_item_create-network
                            bapi2017_gm_item_create-activity
                            RAISING error_converting_network.
*   Netzplan &, Aktivität & nicht vorhanden
    ENDIF.
  ENDIF.

  IF        cl_erp_co_olc_tools=>is_rel_for_order( bapi2017_gm_item_create-orderid ) = abap_true
    AND NOT bapi2017_gm_item_create-orderid IS INITIAL
    AND NOT bapi2017_gm_item_create-activity IS INITIAL.

    CALL METHOD cl_erp_co_olc_tools=>ext_oper_get_int_oper
      EXPORTING
        if_ext_order     = bapi2017_gm_item_create-orderid
        if_ext_operation = bapi2017_gm_item_create-activity
      IMPORTING
        ef_int_order     = imseg-aplzl
        ef_int_operation = imseg-aufpl
      EXCEPTIONS
        not_found        = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      MESSAGE e801(erp_co_olc_e) WITH bapi2017_gm_item_create-activity
                                      bapi2017_gm_item_create-orderid
                                      RAISING error_converting_operation.
    ENDIF.

  ENDIF.

*----- Convert Long FM Fields & Grant ---------------------------------*
  MOVE bapi2017_gm_item_create-grant_nbr TO imseg-grant_nbr.
  CALL FUNCTION 'FUNC_AREA_CONVERSION_INBOUND'
    EXPORTING
      i_func_area      = bapi2017_gm_item_create-func_area
      i_func_area_long = bapi2017_gm_item_create-func_area_long
    IMPORTING
      e_func_area_long = imseg-fkber.

  CALL FUNCTION 'CMMT_ITEM_CONVERSION_INBOUND'
    EXPORTING
      i_cmmt_item      = bapi2017_gm_item_create-cmmt_item
      i_cmmt_item_long = bapi2017_gm_item_create-cmmt_item_long
    IMPORTING
      e_cmmt_item      = imseg-fipos
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING error_converting_keys.
  ENDIF.
*ENHANCEMENT-POINT MAP2I_B2017_GM_ITEM_TO_IMSEG_2 SPOTS ES_SAPLMB_BUS2017 STATIC.

*ENHANCEMENT-POINT MAP2I_B2017_GM_ITEM_TO_IMSEG_1 SPOTS ES_SAPLMB_BUS2017.
ENDFORM.                    "MAP2I_B2017_GM_ITEM_TO_IMSEG
