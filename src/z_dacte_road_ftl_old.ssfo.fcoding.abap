*&---------------------------------------------------------------------*
*&      Form  fill_origin_doc_tab
*&---------------------------------------------------------------------*
*       Fill table of origin documents in main window
*----------------------------------------------------------------------*
FORM fill_origin_doc_tab
  TABLES it_doc_ref STRUCTURE j_1bcte_d_docref
  USING  is_service_taker TYPE j_1bprnfeinnad
  CHANGING ct_origin_doc TYPE gty_origin_doc_tab.

  CONSTANTS:
    lc_model_nfe(2) TYPE c VALUE '55',
    lc_doc_type_nf(2) TYPE c VALUE 'NF',
    lc_doc_type_nfe(4) TYPE c VALUE 'NF-e'.

  DATA:
    lv_count TYPE i,
    lv_column TYPE i,
    ls_doc_ref TYPE j_1bcte_d_docref,
    ls_output TYPE gty_origin_doc,
    lv_cnpj_serv_taker TYPE char15.

  FIELD-SYMBOLS:
    <doc_type> TYPE char10,
    <doc_info> TYPE char80.

  IF it_doc_ref[] IS NOT INITIAL.
    LOOP AT it_doc_ref[] INTO ls_doc_ref.
*     Fill left and right table alternating line by line
      lv_column = lv_count MOD 2.

      IF lv_column = 0.
        ASSIGN ls_output-doc_type_left TO <doc_type>.
        ASSIGN ls_output-doc_info_left TO <doc_info>.
      ELSE.
        ASSIGN ls_output-doc_type_right TO <doc_type>.
        ASSIGN ls_output-doc_info_right TO <doc_info>.
      ENDIF.

      CASE ls_doc_ref-model.
        WHEN lc_model_nfe.
          <doc_type> = lc_doc_type_nfe.
          CONCATENATE ls_doc_ref-regio
                      ls_doc_ref-nfyear
                      ls_doc_ref-nfmonth
                      ls_doc_ref-stcd1
                      ls_doc_ref-model
                      ls_doc_ref-serie
                      ls_doc_ref-nfnum9
                      ls_doc_ref-docnum9
                      ls_doc_ref-cdv
                 INTO <doc_info>.
        WHEN OTHERS.
          <doc_type> = lc_doc_type_nf.

          IF is_service_taker-cgc IS NOT INITIAL.
            lv_cnpj_serv_taker = is_service_taker-cgc.
          ELSE.
            lv_cnpj_serv_taker = is_service_taker-cpf.
          ENDIF.

          CONCATENATE lv_cnpj_serv_taker
                      '                               '
                      ls_doc_ref-nfseries
                      '/'
                      ls_doc_ref-nfnum
                 INTO <doc_info>
                 SEPARATED BY ' '
                 RESPECTING BLANKS.
      ENDCASE.

      IF lv_column = 1.
        APPEND ls_output TO ct_origin_doc.
        CLEAR ls_output.
      ENDIF.

      lv_count = lv_count + 1.
    ENDLOOP.

    IF ls_output IS NOT INITIAL.
      APPEND ls_output TO ct_origin_doc.
    ENDIF.
  ENDIF.

ENDFORM.                    "fill_origin_doc_tab


*&---------------------------------------------------------------------*
*&      Form  read_text_fields
*&---------------------------------------------------------------------*
*       Move imported texts to global variables
*----------------------------------------------------------------------*
FORM read_text_fields
  TABLES    it_text_fields TYPE j_1bprnfetext_tab
  CHANGING cv_service_type TYPE ddtext
           cv_service_taker TYPE ddtext
           cv_cfop_text TYPE ddtext
           cv_transp_mode TYPE ddtext
           cv_strt_jcd TYPE text60
           cv_end_jcd TYPE text60
           cv_icms_taxsit TYPE j_1btaxlwd.

  DATA: ls_text_fields TYPE j_1bprnfetext.

  LOOP AT it_text_fields[] INTO ls_text_fields.
    CASE ls_text_fields-name.
      WHEN 'J_1BCTE_SERV_TP'.
        cv_service_type = ls_text_fields-value.
      WHEN 'J_1BCTE_SERVICE_TAKER'.
        cv_service_taker = ls_text_fields-value.
      WHEN 'J_1BCFOTXT'.
        cv_cfop_text = ls_text_fields-value.
      WHEN 'J_1BCTE_TRANSPTN_MODE'.
        cv_transp_mode = ls_text_fields-value.
        TRANSLATE cv_transp_mode TO UPPER CASE.
      WHEN 'J_1BCTE_SJCD'.
        cv_strt_jcd = ls_text_fields-value.
      WHEN 'J_1BCTE_EJCD'.
        cv_end_jcd = ls_text_fields-value.
      WHEN 'J_1BTAXLW1'.
        cv_icms_taxsit = ls_text_fields-value.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "read_text_fields

*&---------------------------------------------------------------------*
*&      Form  FORMAT_ACCESS_KEY
*&---------------------------------------------------------------------*
*       Access key format: after 4 digits, enter a dot
*----------------------------------------------------------------------*
FORM format_access_key
  USING     iv_access_key TYPE j_1b_nfe_access_key_dtel44
  CHANGING  cv_access_key_string TYPE string.

  DATA:
    lv_formatted_key(54) TYPE c.

  CONSTANTS:
    lc_edit_mask(54) TYPE c VALUE
      '____.____.____.____.____.____.____.____.____.____.____'.

  WRITE iv_access_key
    TO lv_formatted_key
    USING EDIT MASK lc_edit_mask.

  cv_access_key_string = lv_formatted_key.
ENDFORM.                    "format_access_key

*&---------------------------------------------------------------------*
*&      Form  prepare_taxes_for_display
*&---------------------------------------------------------------------*
*       Move tax values to global variables
*----------------------------------------------------------------------*
FORM prepare_taxes_for_display
  TABLES   it_tax STRUCTURE j_1bprnfestx
  USING    is_lin TYPE j_1bnflin
  CHANGING cv_icms_base TYPE j_1bbase
           cv_icms_rate TYPE j_1btxrate
           cv_icms_taxval TYPE j_1btaxval
           cv_icms_basered TYPE j_1btxbase
           cv_icms_st_taxval TYPE j_1btaxval.

  DATA:
    ls_tax TYPE j_1bprnfestx.

  LOOP AT it_tax[] INTO ls_tax.
    CASE ls_tax-taxgrp.
*     Only consider tax groups ICMS or ICST
      WHEN 'ICMS' OR 'ICST'.
        cv_icms_rate   = ls_tax-rate.
        cv_icms_basered = 100 - ls_tax-basered1.

        IF is_lin-taxsit = '6'.
          cv_icms_base = ls_tax-othbas.
        ELSE.
          cv_icms_base = ls_tax-base.
        ENDIF.

        IF ls_tax-taxgrp = 'ICMS'.
          cv_icms_taxval = ls_tax-taxval.
        ELSE.
          cv_icms_st_taxval = ls_tax-taxval.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "prepare_taxes_for_display

*&---------------------------------------------------------------------*
*&      Form  assign_nfnet_and_nftot
*&---------------------------------------------------------------------*
*       Assign totals
*----------------------------------------------------------------------*
FORM assign_nfnet_and_nftot
  USING     is_additional_fields TYPE j_1bprnfehd
  CHANGING  cv_nfnet TYPE j_1bnfnet
            cv_nftot TYPE j_1bnftot.

  cv_nfnet = is_additional_fields-nfnet.
  cv_nftot = is_additional_fields-nftot.
ENDFORM.                    "assign_nfnet_and_nftot

*&---------------------------------------------------------------------*
*&      Form  shorten_cfop_to_4_digits
*&---------------------------------------------------------------------*
*       Shorten CFOP to four digits
*----------------------------------------------------------------------*
FORM shorten_cfop_to_4_digits
  USING     iv_cfop TYPE j_1bcfop
  CHANGING  cv_cfop TYPE char4.
  cv_cfop = iv_cfop(4).
ENDFORM.                    "shorten_cfop_to_4_digits

*&---------------------------------------------------------------------*
*&      Form  assign_cte_type
*&---------------------------------------------------------------------*
*       Assign description of CT-e type
*----------------------------------------------------------------------*
FORM assign_cte_type
  USING     iv_cte_type TYPE j_1bcttype_xml
  CHANGING  cv_cte_type TYPE text60.
  CASE iv_cte_type.
    WHEN '0'.
      cv_cte_type = 'Normal'.
    WHEN '1'.
      cv_cte_type = 'Complemento de Valores'.
    WHEN '2'.
      cv_cte_type = 'Anulação de Valores'.
    WHEN '3'.
      cv_cte_type = 'Substituto'.
  ENDCASE.
ENDFORM.                    "assign_cte_type

*&---------------------------------------------------------------------*
*&      Form  assign_start_and_end_region
*&---------------------------------------------------------------------*
*       Assign start and end region
*----------------------------------------------------------------------*
FORM assign_start_and_end_region
  USING    is_doc TYPE j_1bnfdoc
  CHANGING cv_strt_region TYPE char2
           cv_end_region TYPE char2.

  cv_strt_region = is_doc-cte_strt_lct(2).
  cv_end_region  = is_doc-cte_end_lct(2).
ENDFORM.                    "assign_start_and_end_location
