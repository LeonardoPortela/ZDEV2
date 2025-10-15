FUNCTION zidoc_input_mbgmcr.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWF_PARAM-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWF_PARAM-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  this function module is generated                                   *
*          never change it manually, please!        19.03.2015         *
*----------------------------------------------------------------------*

  DATA:
    e1mbgmcr                    LIKE e1mbgmcr,
    e1bp2017_gm_head_01         LIKE e1bp2017_gm_head_01,
    e1bp2017_gm_code            LIKE e1bp2017_gm_code,
    /spe/e1bp2017_gm_ref_ewm    LIKE /spe/e1bp2017_gm_ref_ewm,
    e1bp2017_gm_item_create     LIKE e1bp2017_gm_item_create,
    e1bp2017_gm_item_create1    LIKE e1bp2017_gm_item_create1,
    e1bp2017_gm_serialnumber    LIKE e1bp2017_gm_serialnumber,
    /spe/e1bp2017_servicepart_d LIKE /spe/e1bp2017_servicepart_d,
    e1bpparex                   LIKE e1bpparex,

    goodsmvt_headret            LIKE
                 bapi2017_gm_head_ret,
    materialdocument            LIKE
                 bapi2017_gm_head_ret-mat_doc,
    matdocumentyear             LIKE
                  bapi2017_gm_head_ret-doc_year,
    goodsmvt_header             LIKE
                  bapi2017_gm_head_01,
    goodsmvt_code               LIKE
                    bapi2017_gm_code,
    testrun                     LIKE
                          bapi2017_gm_gen-testrun,
    goodsmvt_ref_ewm            LIKE
                 /spe/bapi2017_gm_ref_ewm,

    goodsmvt_item               LIKE bapi2017_gm_item_create
                              OCCURS 0 WITH HEADER LINE,
    goodsmvt_serialnumber       LIKE bapi2017_gm_serialnumber
                      OCCURS 0 WITH HEADER LINE,
    return                      LIKE bapiret2
                                     OCCURS 0 WITH HEADER LINE,
    goodsmvt_serv_part_data     LIKE /spe/bapi2017_servicepart_data
                    OCCURS 0 WITH HEADER LINE,
    extensionin                 LIKE bapiparex
                                OCCURS 0 WITH HEADER LINE,

    t_edidd                     LIKE edidd OCCURS 0 WITH HEADER LINE,
    bapi_retn_info              LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

* context-of-insertion
  DATA: error_flag,
        bapi_idoc_status LIKE bdidocstat-status.
* begin-of-insertion
*ENHANCEMENT-POINT IDOC_INPUT_MBGMCR_03 SPOTS ES_SAPLMB_BUS2017 .

* end-of-insertion

  in_update_task = 'X'.
  CLEAR call_transaction_done.
* check if the function is called correctly                            *
  READ TABLE idoc_contrl INDEX 1.
  IF sy-subrc <> 0.
    EXIT.
  ELSEIF idoc_contrl-mestyp <> 'MBGMCR'.
    RAISE wrong_function_called.
  ENDIF.

* go through all IDocs                                                 *
  LOOP AT idoc_contrl.
*   select segments belonging to one IDoc                              *
    REFRESH t_edidd.
    LOOP AT idoc_data WHERE docnum = idoc_contrl-docnum.
      APPEND idoc_data TO t_edidd.
    ENDLOOP.

*   through all segments of this IDoc                                  *
    CLEAR error_flag.
    REFRESH bapi_retn_info.
    CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
      LOOP AT t_edidd INTO idoc_data.

        CASE idoc_data-segnam.

          WHEN 'E1MBGMCR'.

            e1mbgmcr = idoc_data-sdata.
            MOVE e1mbgmcr-testrun
              TO testrun.


          WHEN 'E1BP2017_GM_HEAD_01'.

            e1bp2017_gm_head_01 = idoc_data-sdata.
            MOVE-CORRESPONDING e1bp2017_gm_head_01
               TO goodsmvt_header.                          "#EC ENHOK

            IF e1bp2017_gm_head_01-pstng_date
               IS INITIAL.
              CLEAR goodsmvt_header-pstng_date.
            ENDIF.
            IF e1bp2017_gm_head_01-doc_date
               IS INITIAL.
              CLEAR goodsmvt_header-doc_date.
            ENDIF.

          WHEN 'E1BP2017_GM_CODE'.

            e1bp2017_gm_code = idoc_data-sdata.
            MOVE-CORRESPONDING e1bp2017_gm_code
               TO goodsmvt_code.                            "#EC ENHOK


          WHEN '/SPE/E1BP2017_GM_REF_EWM'.

            /spe/e1bp2017_gm_ref_ewm = idoc_data-sdata.
            MOVE-CORRESPONDING /spe/e1bp2017_gm_ref_ewm
               TO goodsmvt_ref_ewm.                         "#EC ENHOK


          WHEN 'E1BP2017_GM_ITEM_CREATE'.

            e1bp2017_gm_item_create = idoc_data-sdata.
            MOVE-CORRESPONDING e1bp2017_gm_item_create
               TO goodsmvt_item.                            "#EC ENHOK
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
            DATA(v_len) = strlen( e1bp2017_gm_item_create-material ).
            IF v_len > 18.
              goodsmvt_item-material_long = e1bp2017_gm_item_create-material.
            ELSE.
              goodsmvt_item-material      = e1bp2017_gm_item_create-material.
            ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
            IF e1bp2017_gm_item_create-ref_date
               IS INITIAL.
              CLEAR goodsmvt_item-ref_date.
            ENDIF.
            IF e1bp2017_gm_item_create-expirydate
               IS INITIAL.
              CLEAR goodsmvt_item-expirydate.
            ENDIF.
            IF e1bp2017_gm_item_create-prod_date
               IS INITIAL.
              CLEAR goodsmvt_item-prod_date.
            ENDIF.
            READ TABLE t_edidd TRANSPORTING NO FIELDS WITH KEY
                         segnam = 'E1BP2017_GM_ITEM_CREATE1'.
            IF sy-subrc <> 0.
              APPEND goodsmvt_item.
            ENDIF.

          WHEN 'E1BP2017_GM_ITEM_CREATE1'.

            e1bp2017_gm_item_create1 = idoc_data-sdata.
            MOVE-CORRESPONDING e1bp2017_gm_item_create1
               TO goodsmvt_item.                            "#EC ENHOK
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
            v_len = strlen( e1bp2017_gm_item_create1-material_long ).
            IF v_len > 18.
              goodsmvt_item-material_long = e1bp2017_gm_item_create1-material_long.
            ELSE.
              goodsmvt_item-material      = e1bp2017_gm_item_create1-material_long.
            ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
            APPEND goodsmvt_item.

          WHEN 'E1BP2017_GM_SERIALNUMBER'.

            e1bp2017_gm_serialnumber = idoc_data-sdata.
            MOVE-CORRESPONDING e1bp2017_gm_serialnumber
               TO goodsmvt_serialnumber.                    "#EC ENHOK

            APPEND goodsmvt_serialnumber.

          WHEN '/SPE/E1BP2017_SERVICEPART_D'.

            /spe/e1bp2017_servicepart_d = idoc_data-sdata.
            MOVE-CORRESPONDING /spe/e1bp2017_servicepart_d
               TO goodsmvt_serv_part_data.                  "#EC ENHOK

            IF /spe/e1bp2017_servicepart_d-date
               IS INITIAL.
              CLEAR goodsmvt_serv_part_data-date.
            ENDIF.
            APPEND goodsmvt_serv_part_data.

* context-of-insertion
          WHEN 'E1BPPAREX'.

            e1bpparex = idoc_data-sdata.
            MOVE-CORRESPONDING e1bpparex
               TO extensionin.                              "#EC ENHOK

            APPEND extensionin.
* begin-of-insertion
          WHEN OTHERS.
*ENHANCEMENT-POINT IDOC_INPUT_MBGMCR_01 SPOTS ES_SAPLMB_BUS2017 .

* end-of-insertion

        ENDCASE.

      ENDLOOP.
    ENDCATCH.
    IF sy-subrc = 1.
*     write IDoc status-record as error and continue                   *
      CLEAR bapi_retn_info.
      bapi_retn_info-type   = 'E'.
      bapi_retn_info-id     = 'B1'.
      bapi_retn_info-number = '527'.
      bapi_retn_info-message_v1 = idoc_data-segnam.
      bapi_idoc_status      = '51'.
      PERFORM idoc_status_mbgmcr
              TABLES t_edidd
                     idoc_status
                     return_variables
               USING idoc_contrl
                     bapi_retn_info
                     bapi_idoc_status
                     workflow_result.
      CONTINUE.
    ENDIF.
*   call BAPI-function in this system                                  *
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header         = goodsmvt_header
        goodsmvt_code           = goodsmvt_code
        testrun                 = testrun
        goodsmvt_ref_ewm        = goodsmvt_ref_ewm
      IMPORTING
        goodsmvt_headret        = goodsmvt_headret
        materialdocument        = materialdocument
        matdocumentyear         = matdocumentyear
      TABLES
        goodsmvt_item           = goodsmvt_item
        goodsmvt_serialnumber   = goodsmvt_serialnumber
        return                  = return
        goodsmvt_serv_part_data = goodsmvt_serv_part_data
        extensionin             = extensionin
      EXCEPTIONS
        OTHERS                  = 1.
    IF sy-subrc <> 0.
*     write IDoc status-record as error                                *
      CLEAR bapi_retn_info.
      bapi_retn_info-type       = 'E'.
      bapi_retn_info-id         = sy-msgid.
      bapi_retn_info-number     = sy-msgno.
      bapi_retn_info-message_v1 = sy-msgv1.
      bapi_retn_info-message_v2 = sy-msgv2.
      bapi_retn_info-message_v3 = sy-msgv3.
      bapi_retn_info-message_v4 = sy-msgv4.
      bapi_idoc_status          = '51'.
      PERFORM idoc_status_mbgmcr
              TABLES t_edidd
                     idoc_status
                     return_variables
               USING idoc_contrl
                     bapi_retn_info
                     bapi_idoc_status
                     workflow_result.
    ELSE.
      LOOP AT return.
        IF NOT return IS INITIAL.
          CLEAR bapi_retn_info.
          MOVE-CORRESPONDING return TO bapi_retn_info.      "#EC ENHOK
          IF return-type = 'A' OR return-type = 'E'.
            error_flag = 'X'.
          ENDIF.
          APPEND bapi_retn_info.
        ENDIF.
      ENDLOOP.
      LOOP AT bapi_retn_info.
*       write IDoc status-record                                       *
        IF error_flag IS INITIAL.
          bapi_idoc_status = '53'.
        ELSE.
          bapi_idoc_status = '51'.
          IF bapi_retn_info-type = 'S'.
            CONTINUE.
          ENDIF.
        ENDIF.
        PERFORM idoc_status_mbgmcr
                TABLES t_edidd
                       idoc_status
                       return_variables
                 USING idoc_contrl
                       bapi_retn_info
                       bapi_idoc_status
                       workflow_result.
      ENDLOOP.
      IF sy-subrc <> 0.
*      'RETURN' is empty write idoc status-record as successful        *
        CLEAR bapi_retn_info.
        bapi_retn_info-type       = 'S'.
        bapi_retn_info-id         = 'B1'.
        bapi_retn_info-number     = '501'.
        bapi_retn_info-message_v1 = 'CREATEFROMDATA'.
        bapi_idoc_status          = '53'.
        PERFORM idoc_status_mbgmcr
                TABLES t_edidd
                       idoc_status
                       return_variables
                 USING idoc_contrl
                       bapi_retn_info
                       bapi_idoc_status
                       workflow_result.
      ENDIF.
      IF error_flag IS INITIAL.
*       write linked object keys                                       *
        CLEAR return_variables.
        return_variables-wf_param = 'Appl_Objects'.
        READ TABLE return_variables WITH KEY wf_param = 'Appl_Objects'.
        MOVE materialdocument
          TO return_variables-doc_number+00.
        IF sy-subrc <> 0.
          APPEND return_variables.
        ELSE.
          MODIFY return_variables INDEX sy-tabix.
        ENDIF.
        READ TABLE return_variables WITH KEY wf_param = 'Appl_Objects'.
        MOVE matdocumentyear
          TO return_variables-doc_number+10.
        IF sy-subrc <> 0.
          APPEND return_variables.
        ELSE.
          MODIFY return_variables INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.                             " idoc_contrl






ENDFUNCTION.


* subroutine writing IDoc status-record                                *
FORM idoc_status_mbgmcr
     TABLES idoc_data    STRUCTURE  edidd
            idoc_status  STRUCTURE  bdidocstat
            r_variables  STRUCTURE  bdwfretvar
      USING idoc_contrl  LIKE  edidc
            VALUE(retn_info) LIKE   bapiret2
            status       LIKE  bdidocstat-status
            wf_result    LIKE  bdwf_param-result.

  CLEAR idoc_status.
  idoc_status-docnum   = idoc_contrl-docnum.
  idoc_status-msgty    = retn_info-type.
  idoc_status-msgid    = retn_info-id.
  idoc_status-msgno    = retn_info-number.
  idoc_status-appl_log = retn_info-log_no.
  idoc_status-msgv1    = retn_info-message_v1.
  idoc_status-msgv2    = retn_info-message_v2.
  idoc_status-msgv3    = retn_info-message_v3.
  idoc_status-msgv4    = retn_info-message_v4.
  idoc_status-repid    = sy-repid.
  idoc_status-status   = status.

  CASE retn_info-parameter.
    WHEN 'GOODSMVTREFEWM'
      OR 'GOODSMVT_REF_EWM'
         .
      LOOP AT idoc_data WHERE
                        segnam = '/SPE/E1BP2017_GM_REF_EWM'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'GOODSMVTSERVPARTDATA'
      OR 'GOODSMVT_SERV_PART_DATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = '/SPE/E1BP2017_SERVICEPART_D'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'GOODSMVTCODE'
      OR 'GOODSMVT_CODE'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'E1BP2017_GM_CODE'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'GOODSMVTHEADER'
      OR 'GOODSMVT_HEADER'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'E1BP2017_GM_HEAD_01'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'GOODSMVTITEM'
      OR 'GOODSMVT_ITEM'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'E1BP2017_GM_ITEM_CREATE'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'GOODSMVTSERIALNUMBER'
      OR 'GOODSMVT_SERIALNUMBER'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'E1BP2017_GM_SERIALNUMBER'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'EXTENSIONIN'
         .
* context-of-insertion
      LOOP AT idoc_data WHERE
                        segnam = 'E1BPPAREX'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
* begin-of-insertion
*ENHANCEMENT-POINT IDOC_INPUT_MBGMCR_02 SPOTS ES_SAPLMB_BUS2017 .
* end-of-insertion
    WHEN 'TESTRUN'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'E1MBGMCR'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.

  ENDCASE.

  INSERT idoc_status INDEX 1.

  IF idoc_status-status = '51'.
    wf_result = '99999'.
    r_variables-wf_param   = 'Error_IDOCs'.
    r_variables-doc_number = idoc_contrl-docnum.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
  ELSEIF idoc_status-status = '53'.
    CLEAR wf_result.
    r_variables-wf_param = 'Processed_IDOCs'.
    r_variables-doc_number = idoc_contrl-docnum.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
    r_variables-wf_param = 'Appl_Object_Type'.
    r_variables-doc_number = 'BUS2017'.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
  ENDIF.

ENDFORM.                               " IDOC_STATUS_MBGMCR
