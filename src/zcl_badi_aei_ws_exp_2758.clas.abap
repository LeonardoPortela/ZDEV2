CLASS ZCL_BADI_AEI_WS_EXP_2758 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_BADI_AEI_WS_EXP_2758
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES IF_BADI_INTERFACE .
    INTERFACES IF_IMP_AEI_WS_2758 .
  PROTECTED SECTION.
*"* protected components of class CL_SAMPLE_WS_EXP_2758
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class CL_SAMPLE_WS_EXP_2758
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BADI_AEI_WS_EXP_2758 IMPLEMENTATION.


  METHOD IF_IMP_AEI_WS_2758~CHANGE_DOC.
    TYPES: BEGIN OF TY_VBKD,
             POSNR TYPE VBKD-POSNR,
             BSTKD TYPE VBKD-BSTKD,
           END   OF TY_VBKD.


    FIELD-SYMBOLS <J_1ACAE> TYPE J_1ACAE.
    DATA: L_CAMPO(30),
          L_AUBEL     TYPE VBRP-AUBEL,
          LV_TEXT1    TYPE EIKP-TEXT1,
          LV_LAND1    TYPE T001-LAND1.
    DATA: L_J_1A_WS_EX_FEX_PERMISOS TYPE J_1A_WS_EX_FEX_PERMISOS,
          LS_J_1A_CTYCODE           TYPE J_1A_CTYCODE.
    DATA LT_VBKD TYPE TABLE OF TY_VBKD.
    DATA LW_VBKD TYPE          TY_VBKD.


    IF J_1A_WS_EX_FEXAUTH_REQ-CMP-CBTE_TIPO EQ '19'.
      MOVE 'N' TO FEXAUTH_REQ_CH-CMP-PERMISO_EXISTENTE.
      MOVE '(SAPLJ1AE)w_j_1acae' TO L_CAMPO.
      ASSIGN (L_CAMPO) TO <J_1ACAE>.
      IF SY-SUBRC EQ 0.
        IF <J_1ACAE>-CAE_REFTYP EQ 'B'.
*>>> MODIF 07.04.2017
* Obtenemos el pedido de la factura
          CLEAR L_AUBEL.
          SELECT SINGLE AUBEL
            INTO L_AUBEL
            FROM VBRP
           WHERE VBELN EQ <J_1ACAE>-CAE_REF.
*<<< MODIF 07.04.2017
          SELECT POSNR
                 BSTKD
            INTO TABLE LT_VBKD
                  FROM VBKD
*                  WHERE VBELN EQ  <J_1ACAE>-CAE_REF
                  WHERE VBELN EQ L_AUBEL
                  ORDER BY POSNR.

* Buscamos el el destino de mercadería
          SELECT SINGLE TEXT1
            INTO LV_TEXT1
            FROM EIKP
           WHERE REFNR = <J_1ACAE>-CAE_REF.

          IF SY-SUBRC IS INITIAL.
            CLEAR: LV_LAND1,
                   LS_J_1A_CTYCODE.
            LV_LAND1 = LV_TEXT1.
            SELECT SINGLE *
              FROM J_1A_CTYCODE
              INTO LS_J_1A_CTYCODE
             WHERE LAND1 = LV_LAND1.
          ENDIF.

          LOOP AT LT_VBKD INTO LW_VBKD.
            CHECK NOT LW_VBKD-BSTKD IS INITIAL.
            MOVE LW_VBKD-BSTKD(16)                   TO L_J_1A_WS_EX_FEX_PERMISOS-ID_PERMISOS.
*            MOVE J_1A_WS_EX_FEXAUTH_REQ-CMP-DST_CMP  TO L_J_1A_WS_EX_FEX_PERMISOS-DST_MERC.
            MOVE LS_J_1A_CTYCODE-J_1ACTYCODE  TO L_J_1A_WS_EX_FEX_PERMISOS-DST_MERC.
            APPEND  L_J_1A_WS_EX_FEX_PERMISOS TO FEXAUTH_REQ_CH-CMP-PERMISOS.
            MOVE 'S' TO FEXAUTH_REQ_CH-CMP-PERMISO_EXISTENTE.
          ENDLOOP.
        ENDIF.
      ENDIF.
      MOVE 'X' TO DATA_UPDATE.
    ENDIF.


  ENDMETHOD.


  METHOD if_imp_aei_ws_2758~change_rej_doc.
*--> 27.09.2023 14:00:58 - Migração S4 – ML - Início
    DATA: cl_proxy    TYPE REF TO zco_bapi_ar_ws_exp_rej_resp_27,
          v_input	    TYPE zbapi_ar_ws_exp_rej_resp_27584,
          v_output    TYPE zbapi_ar_ws_exp_rej_resp_27583,
          v_permisos  TYPE j_1a_ws_ex_fex_permisos,
          v_cmps_asoc TYPE j_1a_ws_ex_fex_cmps_asoc,
          v_item      TYPE j_1a_ws_ex_fexauth_item,
          v_fexerr    TYPE j_1a_ws_err,
          v_fexevents TYPE evento.

    TRY.
        IF cl_proxy IS NOT BOUND.
          CREATE OBJECT cl_proxy.
        ENDIF.

        CLEAR: v_input,
               v_output.

        v_input-cuit_comp_code = cuit_comp_code.
        v_input-cbte_tipo = cbte_tipo.
        v_input-punto_vta = punto_vta.
        v_input-cbte_nro = cbte_nro.
        v_input-monitor_flag = monitor_flag.

        CALL METHOD cl_proxy->bapi_ar_ws_exp_rej_resp_2758
          EXPORTING
            input  = v_input
          IMPORTING
            output = v_output.

        e_result-id = v_output-w_result-id.
        e_result-fecha_cbte = v_output-w_result-fecha_cbte.
        e_result-cbte_tipo = v_output-w_result-cbte_tipo.
        e_result-punto_vta = v_output-w_result-punto_vta.
        e_result-cbte_nro = v_output-w_result-cbte_nro.
        e_result-tipo_expo = v_output-w_result-tipo_expo.
        e_result-permiso_existinte = v_output-w_result-permiso_existinte.
        e_result-dst_cmp = v_output-w_result-dst_cmp.
        e_result-cliente = v_output-w_result-cliente.
        e_result-cuit_pais_cliente = v_output-w_result-cuit_pais_cliente.
        e_result-domicilio_cliente = v_output-w_result-domicilio_cliente.
        e_result-id_impositivo = v_output-w_result-id_impositivo.
        e_result-moneda_id = v_output-w_result-moneda_id.
        e_result-moneda_ctz = v_output-w_result-moneda_ctz.
        e_result-obs_comerciales = v_output-w_result-obs_comerciales.
        e_result-imp_total = v_output-w_result-imp_total.
        e_result-obs = v_output-w_result-obs.
        e_result-forma_pago = v_output-w_result-forma_pago.
        e_result-incoterms = v_output-w_result-incoterms.
        e_result-incoterms_ds = v_output-w_result-incoterms_ds.
        e_result-idioma_cbte = v_output-w_result-idioma_cbte.

        LOOP AT v_output-w_result-permisos-item INTO DATA(v_r_permisos).
          CLEAR: v_permisos.
          v_permisos-id_permisos = v_r_permisos-id_permisos.
          v_permisos-dst_merc = v_r_permisos-dst_merc.
          APPEND v_permisos TO e_result-permisos.
        ENDLOOP.

        LOOP AT v_output-w_result-cmps_asoc-item INTO DATA(v_r_cmps_asoc).
          CLEAR: v_cmps_asoc.
          v_cmps_asoc-cbte_tipo = v_r_cmps_asoc-cbte_tipo.
          v_cmps_asoc-cbte_punto_vta = v_r_cmps_asoc-cbte_punto_vta.
          v_cmps_asoc-cbte_nro = v_r_cmps_asoc-cbte_nro.
          v_cmps_asoc-cbte_cuit = v_r_cmps_asoc-cbte_cuit.
          APPEND v_cmps_asoc TO e_result-cmps_asoc.
        ENDLOOP.

        LOOP AT v_output-w_result-item-item INTO DATA(v_r_item).
          CLEAR: v_item.
          v_item-pro_ds = v_r_item-pro_ds.
          v_item-pro_qty = v_r_item-pro_qty.
          v_item-pro_umed = v_r_item-pro_umed.
          v_item-pro_precio_uni = v_r_item-pro_precio_uni.
          v_item-pro_bonificacion = v_r_item-pro_bonificacion.
          v_item-pro_total_item = v_r_item-pro_total_item.
          APPEND v_item TO e_result-item.
        ENDLOOP.


        e_result-cae = v_output-w_result-cae.
        e_result-fch_venc_cae = v_output-w_result-fch_venc_cae.
        e_result-resultado = v_output-w_result-resultado.
        e_result-motivos_obs = v_output-w_result-motivos_obs.

        LOOP AT v_output-w_result-fexerr-item INTO DATA(v_r_fexerr).
          CLEAR: v_fexerr.
          v_fexerr-err_code = v_r_fexerr-err_code.
          v_fexerr-err_desc = v_r_fexerr-err_desc.
          APPEND v_fexerr TO e_result-fexerr.
        ENDLOOP.

        LOOP AT v_output-w_result-fexevents-item INTO DATA(v_r_fexevents).
          CLEAR: v_fexevents.
          v_fexevents-event_codigo = v_r_fexevents-event_codigo.
          v_fexevents-event_desc = v_r_fexevents-event_desc.
          APPEND v_fexevents TO e_result-fexevents.
        ENDLOOP.

        xml_data = v_output-xml_data.

      CATCH cx_ai_system_fault.
    ENDTRY.
*<-- 27.09.2023 14:00:58 - Migração S4 – ML – Fim
  ENDMETHOD.


  METHOD if_imp_aei_ws_2758~change_exp_conn_test_2758.
*--> 27.09.2023 11:15:15 - Migração S4 – ML - Início
    DATA: cl_proxy TYPE REF TO zco_bapi_ar_ws_exp_conn_test_p,
          v_input	 TYPE zbapi_ar_ws_exp_conn_test_inpu,
          v_output TYPE zbapi_ar_ws_exp_conn_test_outp.

    TRY.
        IF cl_proxy IS NOT BOUND.
          CREATE OBJECT cl_proxy.
        ENDIF.

        CLEAR: v_input,
               v_output.

        v_input-cuit_comp_code = cuit_comp_code.
        v_input-ws_service = ws_service.

        CALL METHOD cl_proxy->bapi_ar_ws_exp_conn_test
          EXPORTING
            input  = v_input
          IMPORTING
            output = v_output.

        appserver = v_output-appserver.
        authserver = v_output-authserver.
        dbserver = v_output-dbserver.

      CATCH cx_ai_system_fault.
    ENDTRY.
*<-- 27.09.2023 11:15:15 - Migração S4 – ML – Fim
  ENDMETHOD.


  METHOD if_imp_aei_ws_2758~change_exp_fexauthorize_2758.
*--> 27.09.2023 11:15:15 - Migração S4 – ML - Início
    DATA: cl_proxy      TYPE REF TO zco_bapi_ar_ws_exp_fexauthoriz,
          v_input	      TYPE zbapi_ar_ws_exp_fexauthorize_i,
          v_output      TYPE zbapi_ar_ws_exp_fexauthorize_o,
          v_i_permisos  TYPE zj1a_ws_ex_fex_permisos,
          v_i_cmps_asoc TYPE zj1a_ws_ex_fex_cmps_asoc,
          v_i_item      TYPE zj1a_ws_ex_fexauth_item,
          v_fexerr      TYPE j_1a_ws_err,
          v_fexevents   TYPE evento.

    TRY.
        IF cl_proxy IS NOT BOUND.
          CREATE OBJECT cl_proxy.
        ENDIF.

        CLEAR: v_input,
               v_output.

        v_input-w_req-cuit_comp_code = w_req-cuit_comp_code.
        v_input-w_req-cmp-id = w_req-cmp-id.
        v_input-w_req-cmp-fecha_cbte = w_req-cmp-fecha_cbte.
        v_input-w_req-cmp-cbte_tipo = w_req-cmp-cbte_tipo.
        v_input-w_req-cmp-punto_vta = w_req-cmp-punto_vta.
        v_input-w_req-cmp-cbte_nro = w_req-cmp-cbte_nro.
        v_input-w_req-cmp-tipo_expo = w_req-cmp-tipo_expo.
        v_input-w_req-cmp-permiso_existente = w_req-cmp-permiso_existente.
        v_input-w_req-cmp-dst_cmp = w_req-cmp-dst_cmp.
        v_input-w_req-cmp-cliente = w_req-cmp-cliente.
        v_input-w_req-cmp-cuit_pais_cliente = w_req-cmp-cuit_pais_cliente.
        v_input-w_req-cmp-domicilio_cliente = w_req-cmp-domicilio_cliente.
        v_input-w_req-cmp-id_impositivo = w_req-cmp-id_impositivo.
        v_input-w_req-cmp-moneda_id = w_req-cmp-moneda_id.
        v_input-w_req-cmp-moneda_ctz = w_req-cmp-moneda_ctz.
        v_input-w_req-cmp-obs_comerciales = w_req-cmp-obs_comerciales.
        v_input-w_req-cmp-imp_total = w_req-cmp-imp_total.
        v_input-w_req-cmp-obs = w_req-cmp-obs.
        v_input-w_req-cmp-forma_pago = w_req-cmp-forma_pago.
        v_input-w_req-cmp-incoterms = w_req-cmp-incoterms.
        v_input-w_req-cmp-incoterms_ds = w_req-cmp-incoterms_ds.
        v_input-w_req-cmp-idioma_cbte = w_req-cmp-idioma_cbte.

        LOOP AT w_req-cmp-permisos INTO DATA(v_permisos).
          CLEAR: v_i_permisos.
          v_i_permisos-id_permisos = v_permisos-id_permisos.
          v_i_permisos-dst_merc = v_permisos-dst_merc.
          APPEND v_i_permisos TO v_input-w_req-cmp-permisos-item.
        ENDLOOP.

        LOOP AT w_req-cmp-cmps_asoc INTO DATA(v_cmps_asoc).
          CLEAR: v_i_cmps_asoc.
          v_i_cmps_asoc-cbte_tipo = v_cmps_asoc-cbte_tipo.
          v_i_cmps_asoc-cbte_punto_vta = v_cmps_asoc-cbte_punto_vta.
          v_i_cmps_asoc-cbte_nro = v_cmps_asoc-cbte_nro.
          v_i_cmps_asoc-cbte_cuit = v_cmps_asoc-cbte_cuit.
          APPEND v_i_cmps_asoc TO v_input-w_req-cmp-cmps_asoc-item.
        ENDLOOP.

        LOOP AT w_req-cmp-item INTO DATA(v_item).
          CLEAR: v_i_item.
          v_i_item-pro_codigo = v_item-pro_codigo.
          v_i_item-pro_ds = v_item-pro_ds.
          v_i_item-pro_qty = v_item-pro_qty.
          v_i_item-pro_umed = v_item-pro_umed.
          v_i_item-pro_precio_uni = v_item-pro_precio_uni.
          v_i_item-pro_bonificacion = v_item-pro_bonificacion.
          v_i_item-pro_total_item = v_item-pro_total_item.
          APPEND v_i_item TO v_input-w_req-cmp-item-item.
        ENDLOOP.

        "v_input-w_req-cmp-fecha_pago = w_req-cmp-fecha_pago.

        CALL METHOD cl_proxy->bapi_ar_ws_exp_fexauthorize
          EXPORTING
            input  = v_input
          IMPORTING
            output = v_output.

        w_resp-id = v_output-w_resp-id.
        w_resp-cuit_comp_code = v_output-w_resp-cuit_comp_code.
        w_resp-cbte_tipo = v_output-w_resp-cbte_tipo.
        w_resp-punto_vta = v_output-w_resp-punto_vta.
        w_resp-cbte_nro = v_output-w_resp-cbte_nro.
        w_resp-cae = v_output-w_resp-cae.
        w_resp-fch_venc_cae = v_output-w_resp-fch_venc_cae.
        w_resp-fch_cbte = v_output-w_resp-fch_cbte.
        w_resp-resultado = v_output-w_resp-resultado.
        w_resp-reproceso = v_output-w_resp-reproceso.
        w_resp-motivos_obs = v_output-w_resp-motivos_obs.

        LOOP AT v_output-w_resp-fexerr-item INTO DATA(v_r_fexerr).
          CLEAR: v_fexerr.
          v_fexerr-err_code = v_r_fexerr-err_code.
          v_fexerr-err_desc = v_r_fexerr-err_desc.
          APPEND v_fexerr TO w_resp-fexerr.
        ENDLOOP.

        LOOP AT v_output-w_resp-fexevents-item INTO DATA(v_r_fexevents).
          CLEAR: v_fexevents.
          v_fexevents-event_codigo = v_r_fexevents-event_codigo.
          v_fexevents-event_desc = v_r_fexevents-event_desc.
          APPEND v_fexevents TO w_resp-fexevents.
        ENDLOOP.

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

      CATCH cx_ai_system_fault.
    ENDTRY.
*<-- 27.09.2023 11:15:15 - Migração S4 – ML – Fim
  ENDMETHOD.


  method IF_IMP_AEI_WS_2758~CHANGE_ITEM_2758.
  endmethod.
ENDCLASS.
