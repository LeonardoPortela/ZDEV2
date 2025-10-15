FUNCTION ZBAPI_GOODSMVT_CREATE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(GOODSMVT_HEADER) LIKE  BAPI2017_GM_HEAD_01
*"  STRUCTURE  BAPI2017_GM_HEAD_01
*"     VALUE(GOODSMVT_CODE) LIKE  BAPI2017_GM_CODE
*"  STRUCTURE  BAPI2017_GM_CODE
*"     VALUE(TESTRUN) LIKE  BAPI2017_GM_GEN-TESTRUN DEFAULT SPACE
*"     VALUE(GOODSMVT_REF_EWM) LIKE  /SPE/BAPI2017_GM_REF_EWM
*"  STRUCTURE  /SPE/BAPI2017_GM_REF_EWM OPTIONAL
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADRET) LIKE  BAPI2017_GM_HEAD_RET
*"  STRUCTURE  BAPI2017_GM_HEAD_RET
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      GOODSMVT_ITEM STRUCTURE  BAPI2017_GM_ITEM_CREATE
*"      GOODSMVT_SERIALNUMBER STRUCTURE  BAPI2017_GM_SERIALNUMBER
*"         OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"      GOODSMVT_SERV_PART_DATA
*"  STRUCTURE  /SPE/BAPI2017_SERVICEPART_DATA OPTIONAL
*"      EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"--------------------------------------------------------------------
*ENHANCEMENT-POINT bapi_goodsmvt_create_g8 SPOTS es_saplmb_bus2017 STATIC.

*ENHANCEMENT-POINT bapi_goodsmvt_create_g6 SPOTS es_saplmb_bus2017.
  DATA: l_buffer_export TYPE c VALUE false.                 "321507

* SPE Related Data declartions.
  DATA: lt_goodsmvt_rma_data TYPE TABLE OF /spe/bapi2017_ret_auth_data,
        lt_goodsmvt_timestamp TYPE TABLE OF /spe/bapi2017_timestamp,
        lt_goodsmvt_scrap_ind TYPE TABLE OF /spe/bapi2017_scrap_ind,
        lt_goodsmvt_gts_stock_type TYPE /SPE/BAPI2017_GTS_STOCK_TAB,
        ls_goodsmvt_rma_data TYPE /spe/bapi2017_ret_auth_data,
        ls_goodsmvt_timestamp TYPE /spe/bapi2017_timestamp,
        ls_goodsmvt_scrap_ind TYPE /spe/bapi2017_scrap_ind,
        ls_goodsmvt_serv_part_data TYPE /spe/bapi2017_servicepart_data.

*  reference to BAdI MB_BAPI_GOODSMVT_CREATE
  DATA lo_MB_BAPI_GOODSMVT_CREATE  TYPE REF TO MB_BAPI_GOODSMVT_CREATE.

  data : lv_sgt_catv type sgt_catv,
         lv_sgt_scope type sgt_scope.

*--> Brazil - Trigger NF geneartion via special code            "783591
  DATA: j1b_action TYPE goaction VALUE 'BGM'.               "783591
  EXPORT lv_action FROM j1b_action TO MEMORY ID 'J1B_ACTION'. "783591

*check if importing table GOODSMVT_SERV_PART_DATA is filled

  IF ( goodsmvt_serv_part_data[] IS NOT INITIAL ).

* Populate RMA data, Time stamp and Scrap indicator data into separate
* internal tables from SPE data importing table
    LOOP AT goodsmvt_serv_part_data INTO ls_goodsmvt_serv_part_data.

      CLEAR: ls_goodsmvt_rma_data,
             ls_goodsmvt_timestamp,
             ls_goodsmvt_scrap_ind.

      MOVE-CORRESPONDING ls_goodsmvt_serv_part_data TO ls_goodsmvt_rma_data.
      APPEND ls_goodsmvt_rma_data TO lt_goodsmvt_rma_data.
      MOVE-CORRESPONDING ls_goodsmvt_serv_part_data TO ls_goodsmvt_timestamp.
      APPEND ls_goodsmvt_timestamp TO lt_goodsmvt_timestamp.
      MOVE-CORRESPONDING ls_goodsmvt_serv_part_data TO ls_goodsmvt_scrap_ind.
      APPEND ls_goodsmvt_scrap_ind TO lt_goodsmvt_scrap_ind.
    ENDLOOP.
  ENDIF.

* Save RMA-Data in global table
  IF ( lt_goodsmvt_rma_data[] IS NOT INITIAL ).
    CALL FUNCTION '/SPE/RMA_DATA_SET'
      EXPORTING
        it_rma_data = lt_goodsmvt_rma_data[].
  ENDIF.

* Save Timestamp Data in global table
  CALL FUNCTION '/SPE/TIMESTAMP_DATA_SET'
    EXPORTING
      it_timestamp = lt_goodsmvt_timestamp[].

* Save Scrap Indicators in global table
  IF ( lt_goodsmvt_scrap_ind[] IS NOT INITIAL ).
    CALL FUNCTION '/SPE/SCRAP_INDICATORS_SET'
      EXPORTING
        it_scrap_indicator = lt_goodsmvt_scrap_ind[].
  ENDIF.



* check if field longnum is active and translate it to
* internal po_number
  DATA cl_change TYPE REF TO /sappspro/cl_numbers.
  DATA lf_active TYPE boolean.
  FIELD-SYMBOLS: <itab> LIKE goodsmvt_item.

  CLEAR: lf_active.
  CREATE OBJECT cl_change.

  CALL METHOD cl_change->is_active
    RECEIVING
      rv_active = lf_active.

  IF lf_active = 'X'.
    LOOP AT goodsmvt_item ASSIGNING <itab>.

      TRY.
          <itab>-po_number =
             /sappspro/cl_numbers=>lookup_by_longnum( <itab>-longnum ).
        CATCH /sappspro/cx_number_not_found.
*               number does not exist.
      ENDTRY.
    ENDLOOP.
  ENDIF.

  DO 1 TIMES.                                               "321507
* Reset Data Definition ***********************************************
    CLEAR: return,
           global_error,
           loc_tab,
           t_imseg,
          t_emseg,                                          "544764
          currency,                                         "774104
           s_imkpf.

    REFRESH: return,
             t_imseg,
             t_emseg,
             t_iseri.

*clear Segmentation buffer
    CALL FUNCTION 'VB_SGT_CLEAR_BUFF'.

* Initialize HU data                                          "n1026874
    CALL FUNCTION 'HU_PACKING_REFRESH'.                     "n1026874

* TESTRUN: Simulate a gooods movement
    f_testrun = testrun.

* Check GMCODE ********************************************************
    SELECT SINGLE * FROM t158g WHERE gmcode = goodsmvt_code.
    IF sy-subrc <> 0.
      global_error = true.
      PERFORM set_msg_to_bapiret2 TABLES return
                                  USING  'M7'
                                         'E'
                                         '259'
                                          goodsmvt_code
                                          l_blank_msgv
                                          l_blank_msgv
                                          l_blank_msgv
                                          'GOODSMVT_CODE'
                                          0
                                          space.
    ENDIF.

    CHECK global_error = false.

* map head to internal structure **************************************
    CALL FUNCTION 'MAP2I_B2017_GM_HEAD_01_TO_IMKP'
      EXPORTING
        bapi2017_gm_head_01 = goodsmvt_header
      CHANGING
        imkpf               = s_imkpf.
    s_imkpf-usnam = sy-uname.

* Map additonal header fields for EWM reference document to internal
* structure

    CALL FUNCTION '/SPE/MAP_GOODSMVT_REF_EWM'
      EXPORTING
        goodsmvt_ref_ewm = goodsmvt_ref_ewm
      CHANGING
        imkpf            = s_imkpf.


* map items to internal structure *************************************
    LOOP AT goodsmvt_item.
      loc_tab = sy-tabix.
      CLEAR: t_imseg,
             ean_entry.
      IF currency IS INITIAL.
        PERFORM currency_determination TABLES   return
                                       USING    goodsmvt_item
                                                loc_tab
                                       CHANGING currency.
      ENDIF.
      t_imseg-waers = currency.
      CALL FUNCTION 'MAP2I_B2017_GM_ITEM_TO_IMSEG' "#EC CI_USAGE_OK[2438006]
        EXPORTING
          bapi2017_gm_item_create      = goodsmvt_item
        CHANGING
          imseg                        = t_imseg
        EXCEPTIONS
          error_converting_iso_code    = 1
          error_converting_curr_amount = 2
          OTHERS                       = 3.
      IF sy-subrc <> 0.
        global_error = true.
        PERFORM sy_msg_to_bapiret2 TABLES return
                                   USING  loc_tab
                                          'GOODSMVT_ITEM'.
      ENDIF.

      IF cl_psm_core_switch_check=>psm_fm_core_bud_per_rev_1( ) IS NOT INITIAL.
        MOVE goodsmvt_item-budget_period
             TO t_imseg-budget_pd .
      ENDIF.
*-----------------------------------------------------------------------
* Modification for country version Brazil                    note 722167
      IF NOT GOODSMVT_ITEM-EXT_BASE_AMOUNT IS INITIAL.
        MOVE GOODSMVT_ITEM-EXT_BASE_AMOUNT TO T_IMSEG-J_1BEXBASE.
      ENDIF.

      IF NOT GOODSMVT_ITEM-TAX_CODE IS INITIAL.
        MOVE GOODSMVT_ITEM-TAX_CODE TO T_IMSEG-MWSKZ.
      ENDIF.
* End Modification for country Brazil
*-----------------------------------------------------------------------
* Currency must be initial. CLEAR.
      CLEAR t_imseg-waers.
* If there is a reservation - field XRERE has to be filled
      IF NOT t_imseg-rsnum IS INITIAL AND
         t_imseg-kzbew IS INITIAL.
        t_imseg-xrere = x.
      ENDIF.
* EAN is not an element of IMSEG...
      MOVE goodsmvt_item-ean_upc TO ean_upc.
      IF NOT ean_upc IS INITIAL AND
         t_imseg-matnr IS INITIAL AND
         t_imseg-kzbew IS INITIAL.
* Bring EAN to material number. If there is no entry: no error.
* The error will be send by MB_CREATE_GOODS_MOVEMENT
        CLEAR ean_entry.
        SELECT * FROM m_mat1n WHERE ean11 = ean_upc.
          CHECK ean_entry < 1.
          ean_entry = ean_entry + 1.
        ENDSELECT.
        IF ean_entry = 1.
          t_imseg-matnr = m_mat1n-matnr.
        ENDIF.

        IF cl_ops_switch_check=>sfsw_segmentation_03( ) EQ abap_on. " Release 1.6

     " redetermine the segment value from the EAN number
        " api to be called only for scope 1 article

        CALL FUNCTION 'SGTG_CHECK_CAT_REL'
          EXPORTING
            IV_MATNR       = m_mat1n-matnr
           IV_WERKS       =  t_imseg-werks
         IMPORTING
           EV_SCOPE       = lv_sgt_scope.

       IF lv_sgt_scope EQ '1'.

          CALL METHOD CL_SGT_EAN=>VALIDATE_MATNR_EAN_MIGO
          EXPORTING
            IV_EAN11    = ean_upc
            IV_MATNR    = m_mat1n-matnr
            IV_MEINH    = t_imseg-meins
             IV_LIFNR    = t_imseg-lifnr
           IMPORTING
            EV_SGT_CATV = lv_sgt_catv
          EXCEPTIONS
            NOT_FOUND   = 1
            others      = 2
                .
          t_imseg-sgt_scat = lv_sgt_catv.

       ENDIF.
       ENDIF.

      ENDIF.
      CLEAR t_imseg-kzear_old.
*Put the BWH specific data to item
      If NOT goodsmvt_serv_part_data[] IS INITIAL.
        CLEAR ls_goodsmvt_serv_part_data.

        READ TABLE goodsmvt_serv_part_data INTO ls_goodsmvt_serv_part_data
        WITH KEY line_id = t_imseg-line_id.

        IF sy-subrc EQ 0.
          t_imseg-spe_gts_stock_ty = ls_goodsmvt_serv_part_data-gts_stock_type.
          t_imseg-spe_um_gts_st_ty = ls_goodsmvt_serv_part_data-move_gts_stock_type.
*ENHANCEMENT-POINT EHP_BAPI_GOODSMVT_CREATE_01 SPOTS ES_SAPLMB_BUS2017.
        ENDIF.

      ENDIF.
      IF goodsmvt_ref_ewm        IS NOT INITIAL AND        "v_n_1977564
         goodsmvt_item-reserv_no IS NOT INITIAL AND
         cl_ewm_switch_check=>man_sfws_sc_ewm_man_2( ) = cl_ewm_switch_check=>switch_active_true.
* Goods movement posting from EWM with reference to a reservation item:
* Fill complete reservation data to allow posting even if order is in
* status TECO (and hence RESB-XLOEK ='X')
        TRY.                                                "n_2442675
            CALL METHOD /spe/cl_ewm_material_request=>imseg_fill
          EXPORTING
             i_aufnr  = goodsmvt_item-orderid
             i_rsnum  = goodsmvt_item-reserv_no
             i_rspos  = goodsmvt_item-res_item
             i_rsart  = goodsmvt_item-res_type
           CHANGING
             cs_imseg = t_imseg.

          CATCH /spe/cx_ewm_man INTO DATA(lx_ewm_man).    "v_n_2442675
            global_error = true.
            PERFORM set_msg_to_bapiret2 TABLES return
                                        USING  lx_ewm_man->t100key-msgid
                                               /spe/if_const=>c_msgty-error
                                               lx_ewm_man->t100key-msgno
                                               lx_ewm_man->t100key-attr1
                                               lx_ewm_man->t100key-attr2
                                               lx_ewm_man->t100key-attr3
                                               lx_ewm_man->t100key-attr4
                                               'EWM_RESERVATION_FILL'
                                               0
                                               space.
          EXIT.
        ENDTRY.                                           "^_n_2442675
      ENDIF.                                               "^_n_1977564
*ENHANCEMENT-POINT bapi_goodsmvt_create_01 SPOTS es_saplmb_bus2017.
      APPEND t_imseg.
    ENDLOOP.

    CHECK global_error = false.

* map serialnumber to internal structure ******************************
    LOOP AT goodsmvt_serialnumber.
      CLEAR t_iseri.
      CALL FUNCTION 'MAP2I_B2017_GM_SERIALNUMBER'
        EXPORTING
          bapi2017_gm_serialnumber = goodsmvt_serialnumber
        CHANGING
          iseri                    = t_iseri.
      APPEND t_iseri.
    ENDLOOP.
*ENHANCEMENT-POINT EHP603_BAPI_GOODSMVT_CREATE_02 SPOTS ES_SAPLMB_BUS2017.

* Transfer information to local memory of serial number management
* 965218: always call this function to supply xbpsn with the right data
*    IF NOT t_iseri[] IS INITIAL.                                 "965218
    CALL FUNCTION 'DOCUMENT_SERIALNOS_IMPORT'
      TABLES
        ser_tab = t_iseri.
*    ENDIF.                                                       "965218

* Initialization of serialnumber tables *******************************
    CALL FUNCTION 'SERIALPROFILE_CHECK'
         EXPORTING
              operation            = 'SNCL'
         EXCEPTIONS error_message.
    IF sy-subrc <> 0.
      global_error = true.
      PERFORM sy_msg_to_bapiret2 TABLES return
                                 USING  0
                                        'GOODSMVT_HEADER'.
    ENDIF.

    CHECK global_error = false.

*ENHANCEMENT-POINT bapi_goodsmvt_create_02 SPOTS es_saplmb_bus2017.

* Set flag XBAPI FOR MB_CREATE_GOODS_MOVEMENT *************************
    CALL FUNCTION 'MB_SET_BAPI_FLAG'
      EXPORTING
        action = '1'.

* Move STATUS (used by serialno., prod.order,...) to memory
    CALL FUNCTION 'STATUS_BUFFER_EXPORT_TO_MEMORY'          "321507
         EXPORTING                                          "321507
              i_memory_id = 'BAPI_GOODSMVT_CREATE'.         "321507
    l_buffer_export = true.                                 "321507
* Call BAdI MB_BAPI_GOODSMVT_CREATE to fill own fields
    TRY.
        get badi lo_mb_bapi_GOODSMVT_CREATE.
      CATCH cx_badi_not_implemented.                    "#EC NO_HANDLER
    ENDTRY.

    TRY.

        call badi lo_mb_bapi_goodsmvt_create->extensionin_to_matdoc
          EXPORTING
            EXTENSION_IN = EXTENSIONIN[]
          CHANGING
            CS_IMKPF     = S_IMKPF
            CT_IMSEG     = T_IMSEG[]
            CT_RETURN    = return.
      CATCH cx_badi_initial_reference.                  "#EC NO_HANDLER

    ENDTRY.

* Beg hint 984907/1310186 error processing
* Error processing after BADI is called
    IF NOT return IS INITIAL.
       global_error = true.

       DATA: badiret like return.
       CLEAR badiret.
       MOVE-CORRESPONDING return to badiret.
      clear return.

      PERFORM set_msg_to_bapiret2 TABLES return
                                  USING  badiret-id
                                         badiret-type
                                         badiret-number
                                          badiret-message_v1
                                          badiret-message_v2
                                          badiret-message_v3
                                          badiret-message_v4
                                          space
                                          0
                                          space.
    ENDIF.

    CHECK global_error = false.
* End hint 984907/1310186

* Check format of the posting date and document date         v_n1550163

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = s_imkpf-budat
      EXCEPTIONS
        plausibility_check_failed = 1.
    IF sy-subrc <> 0.
      global_error = true.
      PERFORM set_msg_to_bapiret2 TABLES return
                            USING  'BC'
                                   'E'
                                   '011'
                                   'GOODSMVT_HEADER-PSTNG_DATE'
                                    s_imkpf-budat
                                    l_blank_msgv
                                    l_blank_msgv
                                    'GOODSMVT_HEADER-PSTNG_DATE'
                                    0
                                    space.
    ENDIF.

    IF s_imkpf-bldat IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = s_imkpf-bldat
        EXCEPTIONS
          plausibility_check_failed = 1.
      IF sy-subrc <> 0.
        global_error = true.
        PERFORM set_msg_to_bapiret2 TABLES return
                              USING  'BC'
                                     'E'
                                     '011'
                                     'GOODSMVT_HEADER-DOC_DATE'
                                      s_imkpf-bldat
                                      l_blank_msgv
                                      l_blank_msgv
                                      'GOODSMVT_HEADER-DOC_DATE'
                                      0
                                      space.
      ENDIF.
    ENDIF.

    CHECK global_error = false.
*                                                            ^_n1550163

* Call function MB_CREATE_GOODS_MOVEMENT ******************************
    PERFORM mb_create_goods_movement.

* Check the result ****************************************************
    PERFORM return_handling TABLES return.

* Posting only possible, if there was no error until now **************
* or the bapi runs in modus 'TESTRUN'.
    CHECK global_error = false.
    CHECK f_testrun IS INITIAL.

* Call function MB_POST_GOODS_MOVEMENT ********************************
    PERFORM mb_post_goods_movement TABLES return
                                   USING  goodsmvt_headret.
    MOVE goodsmvt_headret-mat_doc  TO materialdocument.
    MOVE goodsmvt_headret-doc_year TO matdocumentyear.
  ENDDO.                                                    "321507

  FREE MEMORY ID 'J1B_ACTION'.                              "783591

***********************************************************************
* Error/Testrun handling:
* Get STATUS back from memory if error or bapi runs in mode 'TESTRUN'
* But only if status was filled during this BAPI.
  CHECK l_buffer_export = true.                             "321507
  IF global_error = true OR NOT f_testrun IS INITIAL.       "321507
*ENHANCEMENT-POINT bapi_goodsmvt_create_03 SPOTS es_saplmb_bus2017.
*ENHANCEMENT-POINT SAPLMB_BUS2017_20 SPOTS ES_SAPLMB_BUS2017.
    CALL FUNCTION 'STATUS_BUFFER_REFRESH'.                  "321507
    CALL FUNCTION 'STATUS_BUFFER_IMPORT_FROM_MEMO'          "321507
         EXPORTING                                          "321507
              i_memory_id = 'BAPI_GOODSMVT_CREATE'          "321507
              i_client    = sy-mandt.                       "321507

* clear buffer for batch data in error case                 "n981969
    CALL FUNCTION 'VB_INIT_ALL'.                            "n981969

* clear buffer for classification data in error case        "921353
    CALL FUNCTION 'CLAP_DDB_INIT_CLASSIFICATION'            "921353
     EXPORTING                                              "921353
       INIT_ALL       = 'X'.                                "921353
* clear buffer for QM data in error case
  CALL FUNCTION 'QPL1_RESET_CURRENT_LOTS'                  "1521757
         EXCEPTIONS                                        "1521757
         OTHERS = 1.                                       "1521757
  CALL FUNCTION 'QCPR_INITIALIZE'.                         "1521757

  ENDIF.                                                    "321507

* Clear flag XBAPI *****************************************************
  CALL FUNCTION 'MB_SET_BAPI_FLAG'                          "386958
       EXPORTING action = '4'.                              "386958

*ENHANCEMENT-POINT bapi_goodsmvt_create_g7 SPOTS es_saplmb_bus2017.
ENDFUNCTION.
