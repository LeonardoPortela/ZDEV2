class ZWS_2485_DOC_CHANGE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_IMP_AEI_WS_2485_DOC_CHANGE .
protected section.
private section.
ENDCLASS.



CLASS ZWS_2485_DOC_CHANGE IMPLEMENTATION.


  METHOD if_imp_aei_ws_2485_doc_change~change_data.
  ENDMETHOD.


METHOD if_imp_aei_ws_2485_doc_change~change_doc.
*--> 27.09.2023 09:33:09 - Migração S4 – ML - Início
  DATA: cl_proxy         TYPE REF TO zco_bapi_ar_ws_2485_port_type,
        lo_sys_exception TYPE REF TO cx_ai_system_fault,
        v_input	         TYPE zbapi_ar_ws_2485_input,
        v_i_caedet       TYPE zj1a_ws_fecaedet,
        v_i_cbteasoc     TYPE zj1a_ws_cbteasoc,
        v_i_aliciva      TYPE zj1a_ws_aliciva,
        v_i_tributo      TYPE zj1a_ws_tributo,
        v_i_opcional     TYPE zj1a_ws_opcional,
        v_i_perido_asoc  TYPE zzrsi_j_1a_ws_periodo_asoc,
        v_detresp        TYPE j_1a_ws_fedet_resp,
        v_event          TYPE evento,
        v_error          TYPE j_1a_ws_err,
        v_obs            TYPE j_1a_ws_obs,
        v_output         TYPE zbapi_ar_ws_2485_output,
        lv_message       TYPE string.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      CLEAR: v_input,
             v_output.

      v_input-w_req-cuit_comp_code = j_1a_ws_req_2485-cuit_comp_code.
      v_input-w_req-quantity = j_1a_ws_req_2485-quantity.
      v_input-w_req-brnch = j_1a_ws_req_2485-brnch.
      v_input-w_req-doc_type = j_1a_ws_req_2485-doc_type.

      LOOP AT j_1a_ws_req_2485-caedet INTO DATA(v_caedet).
        CLEAR: v_i_caedet.
        v_i_caedet-concepto = v_caedet-concepto.
        v_i_caedet-doc_taxid_code = v_caedet-doc_taxid_code.
        v_i_caedet-buyer_tax_id = v_caedet-buyer_tax_id.
        v_i_caedet-odn_from = v_caedet-odn_from.
        v_i_caedet-odn_to = v_caedet-odn_to.
        v_i_caedet-doc_date = v_caedet-doc_date.
        v_i_caedet-tot_doc_amt = v_caedet-tot_doc_amt.
        v_i_caedet-total_notax_amt = v_caedet-total_notax_amt.
        v_i_caedet-taxable_net_amt = v_caedet-taxable_net_amt.
        v_i_caedet-tot_exmt_amt = v_caedet-tot_exmt_amt.
        v_i_caedet-tot_tax_amt = v_caedet-tot_tax_amt.
        v_i_caedet-tot_vat_amt = v_caedet-tot_vat_amt.
        v_i_caedet-serv_start_date = v_caedet-serv_start_date.
        v_i_caedet-serv_end_date = v_caedet-serv_end_date.
        v_i_caedet-due_date = v_caedet-due_date.
        v_i_caedet-curr_code = v_caedet-curr_code.
        v_i_caedet-exchange_rate = v_caedet-exchange_rate.

        LOOP AT v_caedet-cbteasoc INTO DATA(v_cbteasoc).
          CLEAR: v_i_cbteasoc.
          v_i_cbteasoc-ref_doc_typ = v_cbteasoc-ref_doc_typ.
          v_i_cbteasoc-ref_brnch = v_cbteasoc-ref_brnch.
          v_i_cbteasoc-ref_odn_num = v_cbteasoc-ref_odn_num.
          v_i_cbteasoc-ref_brnch_prefix = v_cbteasoc-ref_brnch_prefix.
          APPEND v_i_cbteasoc TO v_i_caedet-cbteasoc-item.
        ENDLOOP.

        LOOP AT v_caedet-tributo INTO DATA(v_tributo).
          CLEAR: v_i_tributo.
          v_i_tributo-tax_id = v_tributo-tax_id.
          v_i_tributo-tax_desc = v_tributo-tax_desc.
          v_i_tributo-base_amt = v_tributo-base_amt.
          v_i_tributo-tax_rate = v_tributo-tax_rate.
          v_i_tributo-tax_amt = v_tributo-tax_amt.
          APPEND v_i_tributo TO v_i_caedet-tributo-item.
        ENDLOOP.

        LOOP AT v_caedet-aliciva INTO DATA(v_aliciva).
          CLEAR: v_i_aliciva.
          v_i_aliciva-vat_taxid = v_aliciva-vat_taxid.
          v_i_aliciva-taxable_base = v_aliciva-taxable_base.
          v_i_aliciva-vat_amt = v_aliciva-vat_amt.
          APPEND v_i_aliciva TO v_i_caedet-aliciva-item.
        ENDLOOP.

        LOOP AT v_caedet-opcional INTO DATA(v_opcional).
          CLEAR: v_i_opcional.
          v_i_opcional-id = v_opcional-id.
          v_i_opcional-value = v_opcional-value.
          APPEND v_i_opcional TO v_i_caedet-opcional-item.
        ENDLOOP.

        "v_i_caedet-per_asoc_fchdesde = v_caedet-per_asoc_fchdesde.
        "v_i_caedet-per_asoc_fchhasta = v_caedet-per_asoc_fchhasta.
        CLEAR: v_i_perido_asoc.
        v_i_perido_asoc-fchdesde = v_caedet-periodo_asoc-fchdesde.
        v_i_perido_asoc-fchhasta = v_caedet-periodo_asoc-fchhasta.
        v_i_caedet-periodo_asoc = v_i_perido_asoc.

        v_i_caedet-flag_periodo = v_caedet-flag_periodo.

* Inicio ZID 16695 / Adaptar FE por RG 5616/2024
        v_i_caedet-can_mis_mon_ext = 'N'.
        v_i_caedet-condicion_iva_receptor = v_caedet-condicion_iva_receptor_id.
        SHIFT v_i_caedet-condicion_iva_receptor LEFT DELETING LEADING space.
* Fin ZID 16695 / Adaptar FE por RG 5616/2024

        APPEND v_i_caedet TO v_input-w_req-caedet-item.
      ENDLOOP.

      v_input-w_req-brnch_prefix = j_1a_ws_req_2485-brnch_prefix.


      CALL METHOD cl_proxy->bapi_ar_ws_2485
        EXPORTING
          input  = v_input
        IMPORTING
          output = v_output.

      j_1a_ws_resp_2485-cuit_comp_code = v_output-w_resp-cuit_comp_code.
      j_1a_ws_resp_2485-brnch = v_output-w_resp-brnch.
      j_1a_ws_resp_2485-doc_type = v_output-w_resp-doc_type.
      j_1a_ws_resp_2485-process_date = v_output-w_resp-process_date.
      j_1a_ws_resp_2485-quantity = v_output-w_resp-quantity.
      j_1a_ws_resp_2485-resultado = v_output-w_resp-resultado.

      LOOP AT v_output-w_resp-detresp-item INTO DATA(v_r_detresp).
        CLEAR: v_detresp.
        v_detresp-concepto = v_r_detresp-concepto.
        v_detresp-doc_taxid_code = v_r_detresp-doc_taxid_code.
        v_detresp-buyer_tax_id = v_r_detresp-buyer_tax_id.
        v_detresp-odn_from = v_r_detresp-odn_from.
        v_detresp-odn_to = v_r_detresp-odn_to.
        v_detresp-doc_date = v_r_detresp-doc_date.
        v_detresp-det_resultado = v_r_detresp-det_resultado.
        v_detresp-cae = v_r_detresp-cae.
        v_detresp-cae_due_date = v_r_detresp-cae_due_date.

        LOOP AT v_r_detresp-obs-item INTO DATA(v_r_obs).
          CLEAR: v_obs.
          v_obs-code = v_r_obs-code.
          v_obs-msg = v_r_obs-msg.
          APPEND v_obs TO v_detresp-obs.
        ENDLOOP.

        APPEND v_detresp TO j_1a_ws_resp_2485-detresp.
      ENDLOOP.

      LOOP AT v_output-w_resp-event-item INTO DATA(v_r_event).
        CLEAR: v_event.
        v_event-event_codigo = v_r_event-event_codigo.
        v_event-event_desc = v_r_event-event_desc.
        APPEND v_event TO j_1a_ws_resp_2485-event.
      ENDLOOP.

      LOOP AT v_output-w_resp-error-item INTO DATA(v_r_error).
        CLEAR: v_error.
        v_error-err_code = v_r_error-err_code.
        v_error-err_desc = v_r_error-err_desc.
        APPEND v_error TO j_1a_ws_resp_2485-error.
      ENDLOOP.

      CLEAR: return.
      return-type = v_output-return-type.
      return-id = v_output-return-id.
      return-number = v_output-return-number.
      return-message = v_output-return-message.
      return-log_no = v_output-return-log_no.
      return-log_msg_no = v_output-return-log_msg_no.
      return-message_v1 = v_output-return-message_v1.
      return-message_v2 = v_output-return-message_v2.
      return-message_v3 = v_output-return-message_v3.
      return-message_v4 = v_output-return-message_v4.
      return-parameter = v_output-return-parameter.
      return-row = v_output-return-row.
      return-field = v_output-return-field.
      return-system = v_output-return-system.

    CATCH cx_ai_system_fault INTO lo_sys_exception.
      CONCATENATE  lo_sys_exception->code lo_sys_exception->errortext INTO lv_message.
  ENDTRY.
*<-- 27.09.2023 09:33:09 - Migração S4 – ML – Fim
ENDMETHOD.


  METHOD if_imp_aei_ws_2485_doc_change~change_per_asoc.


    DATA: lv_days     TYPE string.
    DATA: lv_fchdesde TYPE string.
    DATA: lv_fchhasta TYPE string.
    DATA: lv_year(4)  TYPE c.
    DATA: lv_month(2) TYPE c.
    DATA: lv_day(2)   TYPE c.
    DATA: lv_days_f   TYPE t5a4a-dlydy.
    DATA: lv_date     TYPE p0001-begda.


    CONSTANTS lc_tvarv_perio TYPE c LENGTH 22 VALUE 'ZFACTURA_ELE_AR_FECHAS'.


    IF is_cae_det-cbteasoc IS INITIAL . "AND ( is_j_1acae-doccls EQ 'C' OR is_j_1acae-doccls EQ 'D' ) . "View = V_T003_B_I


      SELECT SINGLE fkart , fkdat FROM vbrk
        INTO @DATA(wa_vbrk)
        WHERE vbeln = @is_j_1acae-cae_ref.

      SELECT SINGLE high FROM tvarvc INTO lv_days WHERE name = lc_tvarv_perio AND low = wa_vbrk-fkart.
      IF sy-subrc IS INITIAL.
        lv_days_f = lv_days.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = wa_vbrk-fkdat
            days      = lv_days_f
            months    = 0
            signum    = '-'
            years     = 0
          IMPORTING
            calc_date = lv_date.
        lv_fchdesde = lv_date.

        lv_year  = wa_vbrk-fkdat+0(4).
        lv_month = wa_vbrk-fkdat+4(2).
        lv_day   = wa_vbrk-fkdat+6(2).
        CONCATENATE lv_year lv_month lv_day INTO lv_fchhasta.
        lv_year  = lv_fchdesde+0(4).
        lv_month = lv_fchdesde+4(2).
        lv_day   = lv_fchdesde+6(2).
        CLEAR lv_fchdesde.
        CONCATENATE lv_year lv_month lv_day INTO lv_fchdesde.
*
        " is_cae_det-per_asoc_fchdesde = lv_fchdesde.
        " is_cae_det-per_asoc_fchhasta = lv_fchhasta.
        es_per_asoc-fchdesde  = lv_fchdesde.
        es_per_asoc-fchhasta = lv_fchhasta.

**"Solução para utilizar mapeamento PI -- Provisório após migração utilizar campos standard.
**        is_cae_det-per_asoc_fchdesde = lv_fchdesde.
**        is_cae_det-per_asoc_fchhasta = lv_fchhasta.

      ENDIF.
    ENDIF.


  ENDMETHOD.


METHOD if_imp_aei_ws_2485_doc_change~change_rej_doc.
*--> 27.09.2023 09:33:09 - Migração S4 – ML - Início
  DATA: cl_proxy   TYPE REF TO zco_bapi_ar_ws_rej_resp_2485_p,
        v_input	   TYPE zbapi_ar_ws_rej_resp_2485_inpu,
        v_output   TYPE zbapi_ar_ws_rej_resp_2485_outp,
        v_cbteasoc TYPE j_1a_ws_cbteasoc,
        v_tributo  TYPE j_1a_ws_tributo,
        v_aliciva  TYPE j_1a_ws_aliciva,
        v_opcional TYPE j_1a_ws_opcional,
        v_obs      TYPE j_1a_ws_obs,
        v_error    TYPE j_1a_ws_err,
        v_event    TYPE evento.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      CLEAR: v_input,
             v_output.

      v_input-cuit_comp_code = cuit_comp_code.
      v_input-doc_type = doc_type.
      v_input-brnch = brnch.
      v_input-odn_no = odn_no.
      v_input-monitor_flag = monitor_flag.
      "v_input-brnch_prefix = brnch_prefix.

      CALL METHOD cl_proxy->bapi_ar_ws_rej_resp_2485
        EXPORTING
          input  = v_input
        IMPORTING
          output = v_output.

      j_1a_ws_resultget-concepto = v_output-j_1a_rej_2485-concepto.
      j_1a_ws_resultget-doc_taxid_code = v_output-j_1a_rej_2485-doc_taxid_code.
      j_1a_ws_resultget-buyer_tax_id = v_output-j_1a_rej_2485-buyer_tax_id.
      j_1a_ws_resultget-odn_from = v_output-j_1a_rej_2485-odn_from.
      j_1a_ws_resultget-odn_to = v_output-j_1a_rej_2485-odn_to.
      j_1a_ws_resultget-doc_date = v_output-j_1a_rej_2485-doc_date.
      j_1a_ws_resultget-tot_doc_amt = v_output-j_1a_rej_2485-tot_doc_amt.
      j_1a_ws_resultget-total_notax_amt = v_output-j_1a_rej_2485-total_notax_amt.
      j_1a_ws_resultget-taxable_net_amt = v_output-j_1a_rej_2485-taxable_net_amt.
      j_1a_ws_resultget-tot_exmt_amt = v_output-j_1a_rej_2485-tot_exmt_amt.
      j_1a_ws_resultget-tot_tax_amt = v_output-j_1a_rej_2485-tot_tax_amt.
      j_1a_ws_resultget-tot_vat_amt = v_output-j_1a_rej_2485-tot_vat_amt.
      j_1a_ws_resultget-serv_start_date = v_output-j_1a_rej_2485-serv_start_date.
      j_1a_ws_resultget-serv_end_date = v_output-j_1a_rej_2485-serv_end_date.
      j_1a_ws_resultget-due_date = v_output-j_1a_rej_2485-due_date.
      j_1a_ws_resultget-curr_code = v_output-j_1a_rej_2485-curr_code.
      j_1a_ws_resultget-exchange_rate = v_output-j_1a_rej_2485-exchange_rate.

      LOOP AT v_output-j_1a_rej_2485-cbteasoc-item INTO DATA(v_r_cbteasoc).
        CLEAR: v_cbteasoc.
        v_cbteasoc-ref_doc_typ = v_r_cbteasoc-ref_doc_typ.
        v_cbteasoc-ref_brnch = v_r_cbteasoc-ref_brnch.
        v_cbteasoc-ref_odn_num = v_r_cbteasoc-ref_odn_num.
        v_cbteasoc-ref_brnch_prefix = v_r_cbteasoc-ref_brnch_prefix.
        APPEND v_cbteasoc TO j_1a_ws_resultget-cbteasoc.
      ENDLOOP.

      LOOP AT v_output-j_1a_rej_2485-tributo-item INTO DATA(v_r_tributo).
        CLEAR: v_tributo.
        v_tributo-tax_id = v_r_tributo-tax_id.
        v_tributo-tax_desc = v_r_tributo-tax_desc.
        v_tributo-base_amt = v_r_tributo-base_amt.
        v_tributo-tax_rate = v_r_tributo-tax_rate.
        v_tributo-tax_amt = v_r_tributo-tax_amt.
        APPEND v_tributo TO j_1a_ws_resultget-tributo.
      ENDLOOP.

      LOOP AT v_output-j_1a_rej_2485-aliciva-item INTO DATA(v_r_aliciva).
        CLEAR: v_aliciva.
        v_aliciva-vat_taxid = v_r_aliciva-vat_taxid.
        v_aliciva-taxable_base = v_r_aliciva-taxable_base.
        v_aliciva-vat_amt = v_r_aliciva-vat_amt.
        APPEND v_aliciva TO j_1a_ws_resultget-aliciva.
      ENDLOOP.

      LOOP AT v_output-j_1a_rej_2485-opcional-item INTO DATA(v_r_opcional).
        CLEAR: v_opcional.
        v_opcional-id = v_r_opcional-id.
        v_opcional-value = v_r_opcional-value.
        APPEND v_opcional TO j_1a_ws_resultget-opcional.
      ENDLOOP.

      j_1a_ws_resultget-resultado = v_output-j_1a_rej_2485-resultado.
      j_1a_ws_resultget-cae_num = v_output-j_1a_rej_2485-cae_num.
      j_1a_ws_resultget-issuing_type = v_output-j_1a_rej_2485-issuing_type.
      j_1a_ws_resultget-cae_due_date = v_output-j_1a_rej_2485-cae_due_date.
      j_1a_ws_resultget-process_date = v_output-j_1a_rej_2485-process_date.

      LOOP AT v_output-j_1a_rej_2485-obs-item INTO DATA(v_r_obs).
        CLEAR: v_obs.
        v_obs-code = v_r_obs-code.
        v_obs-msg = v_r_obs-msg.
        APPEND v_obs TO j_1a_ws_resultget-obs.
      ENDLOOP.

      j_1a_ws_resultget-branch = v_output-j_1a_rej_2485-branch.
      j_1a_ws_resultget-doc_type = v_output-j_1a_rej_2485-doc_type.

      LOOP AT v_output-j_1a_rej_2485-error-item INTO DATA(v_r_error).
        CLEAR: v_error.
        v_error-err_code = v_r_error-err_code.
        v_error-err_desc = v_r_error-err_desc.
        APPEND v_error TO j_1a_ws_resultget-error.
      ENDLOOP.

      LOOP AT v_output-j_1a_rej_2485-event-item INTO DATA(v_r_event).
        CLEAR: v_event.
        v_event-event_codigo = v_r_event-event_codigo.
        v_event-event_desc = v_r_event-event_desc.
        APPEND v_event TO j_1a_ws_resultget-event.
      ENDLOOP.

      xml_data = v_output-xml_data.

    CATCH cx_ai_system_fault.
  ENDTRY.
ENDMETHOD.
ENDCLASS.
