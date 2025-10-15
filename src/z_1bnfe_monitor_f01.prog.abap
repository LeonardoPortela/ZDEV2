*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*-#133089-21.02.2024-JT-inicio
  DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
        vg_faturamento_autom      TYPE char01,
        vg_ch_referencia          TYPE zsdt0001-ch_referencia,
        vg_ch_faturamento         TYPE zlest0240-ch_faturamento.
*-#133089-12.02.2024-JT-fim

*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F01
*&---------------------------------------------------------------------*
  FORM set_screen_status_100 .

    DATA: tl_parametros         TYPE ustyp_t_parameters,
          wl_parametros         TYPE ustyp_parameters,
          vl_canc_doc_interface TYPE c.

    DATA: p_emite   TYPE char01,
          p_empresa TYPE bukrs,
          p_partner TYPE j_1bparid.

    CLEAR it_fcode.
* Determine action buttons that should not be displayed.

    CLEAR: tl_parametros[].

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        user_name           = sy-uname
*       WITH_TEXT           =
      TABLES
        user_parameters     = tl_parametros
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.

    CASE sy-tcode.
      WHEN 'ZNFE'.

* Enable action "Delete Log Entries"?
        IF gf_log_entries_exist IS INITIAL OR
           gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_delete_log.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

* Enable action "Request Cancellation"?
        IF gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_req_cancel.
          APPEND wa_fcode TO it_fcode.
        ENDIF.
        "ENDIF.
* Enable action "Contingency"?
        "IF gf_authorization_nfe_35 IS INITIAL.
        wa_fcode = c_fcode_conting.
        APPEND wa_fcode TO it_fcode.
        "ENDIF.
* Enable action "Contingency Reset"?
        "IF gf_authorization_nfe_35 IS INITIAL.
        wa_fcode = c_fcode_conting_rs.
        APPEND wa_fcode TO it_fcode.
        "ENDIF.
* Enable action "Send NF-e"?
        IF gf_authorization_nfe_35 IS INITIAL.
          wa_fcode = c_fcode_send_nfe.
          APPEND wa_fcode TO it_fcode.
        ENDIF.
* Enable actions "Maintaine Contingency Centrally"?
        IF gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_cont_region.
          APPEND wa_fcode TO it_fcode.
          wa_fcode = c_fcode_cont_branch.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        AUTHORITY-CHECK OBJECT 'ZCCORRECNF' ID 'Z_CCORREC' FIELD '1'.

        IF ( sy-subrc NE 0 ).
          wa_fcode = c_fcode_ccorrec.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        "Check Permissão Cancelamento Extemporaneo.
        READ TABLE tl_parametros INTO wl_parametros
          WITH KEY parid = 'ZCANC_EXTEMP_INT'.
        IF sy-subrc NE 0.
          wa_fcode = c_fcode_canc_extem.
          APPEND wa_fcode TO it_fcode.
        ENDIF.
        "Fim Check Permissão Cancelamento Extemporaneo.

*** Inicio - Rubenilson Pereira - 09.10.25 #192341
 "Check Permissão para inserir valor de frete e seguro
        READ TABLE tl_parametros INTO wl_parametros
        WITH KEY parid = 'ZFRETE_SEGURO'.
        IF sy-subrc NE 0.
          wa_fcode = c_fcode_frete_segur.
          APPEND wa_fcode TO it_fcode.
        ENDIF.
*** Fim - Rubenilson Pereira - 09.10.25 #192341

      WHEN 'ZCTE'.

* Enable action "Delete Log Entries"?
        IF gf_log_entries_exist IS INITIAL OR
           gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_delete_log2.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

* Enable action "Request Cancellation"?
        IF gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_req_cancel2.
          APPEND wa_fcode TO it_fcode.
        ENDIF.
        "ENDIF.
* Enable action "Contingency"?
        "IF gf_authorization_nfe_35 IS INITIAL.
        wa_fcode = c_fcode_conting2.
        APPEND wa_fcode TO it_fcode.
        "ENDIF.
* Enable action "Contingency Reset"?
        "IF gf_authorization_nfe_35 IS INITIAL.
        wa_fcode = c_fcode_conting_rs2.
        APPEND wa_fcode TO it_fcode.
        "ENDIF.
* Enable action "Send NF-e"?
        IF gf_authorization_nfe_35 IS INITIAL.
          wa_fcode = c_fcode_send_nfe2.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

* Enable actions "Maintaine Contingency Centrally"?
        IF gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_cont_region2.
          APPEND wa_fcode TO it_fcode.
          wa_fcode = c_fcode_cont_branch2.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        READ TABLE bukrs INDEX 1.
        p_empresa = bukrs-low.

        wa_fcode = c_fcode_canc_extem.
        APPEND wa_fcode TO it_fcode.

      WHEN 'ZMDFE'.

* Enable action "Delete Log Entries"?
        IF gf_log_entries_exist IS INITIAL OR
           gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_delete_log.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

* Enable action "Request Cancellation"?
        IF gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_req_cancel.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        wa_fcode = c_fcode_conting.
        APPEND wa_fcode TO it_fcode.

        wa_fcode = c_fcode_conting_rs.
        APPEND wa_fcode TO it_fcode.

        IF gf_authorization_nfe_35 IS INITIAL.
          wa_fcode = c_fcode_send_mdfe.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        IF gf_authorization_nfe_85 IS INITIAL.
          wa_fcode = c_fcode_cont_region.
          APPEND wa_fcode TO it_fcode.
          wa_fcode = c_fcode_cont_branch.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        "Check Permissão Cancelamento Extemporaneo.
        READ TABLE tl_parametros INTO wl_parametros
          WITH KEY parid = 'ZCANC_EXTEMP_INT'.

        IF sy-subrc NE 0.
          wa_fcode = c_fcode_canc_extem.
          APPEND wa_fcode TO it_fcode.
        ENDIF.
        "Fim Check Permissão Cancelamento Extemporaneo.

        IF gf_authorization_mdf_01 EQ abap_false.
          wa_fcode = c_fcode_close_mdfe.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

        IF gf_authorization_mdf_02 EQ abap_false.
          wa_fcode = c_fcode_emitir_mdfe.
          APPEND wa_fcode TO it_fcode.
        ENDIF.

    ENDCASE.

    "Desabilitar o botão de Exportação
    wa_fcode = c_fcode_exporta.
    APPEND wa_fcode TO it_fcode.

    READ TABLE tl_parametros INTO wl_parametros
      WITH KEY parid = 'ZREENVIAR_DOC_GRC'.

    IF sy-subrc NE 0.
      wa_fcode = c_fcode_reenviar_grc.
      APPEND wa_fcode TO it_fcode.

      wa_fcode = c_fcode_cng_doc.
      APPEND wa_fcode TO it_fcode.

      wa_fcode = c_fcode_reenv_contin.  "*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
      APPEND wa_fcode TO it_fcode.
    ENDIF.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_user_fat)
     WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
       AND low  = @sy-uname.

    IF sy-subrc NE 0.
      wa_fcode = c_fcode_sincdocecc.
      APPEND wa_fcode TO it_fcode.
    ENDIF.


* Set screen status
    IF it_fcode IS INITIAL.
      CASE sy-tcode.
        WHEN 'ZNFE'.
          SET PF-STATUS 'SCREEN_100'.
        WHEN 'ZCTE'.
          SET PF-STATUS 'SCREEN_100C'.
        WHEN 'ZNFE_TERC'.
          SET PF-STATUS 'SCREEN_101'.
        WHEN 'ZCTE_TERC'.
          SET PF-STATUS 'SCREEN_101C'.
        WHEN 'ZMDFE'.
          SET PF-STATUS 'SCREEN_109'.
      ENDCASE.
    ELSE.
      CASE sy-tcode.
        WHEN 'ZNFE'.
          SET PF-STATUS 'SCREEN_100'  EXCLUDING it_fcode.
        WHEN 'ZCTE'.
          SET PF-STATUS 'SCREEN_100C' EXCLUDING it_fcode.
        WHEN 'ZNFE_TERC'.
          SET PF-STATUS 'SCREEN_101'  EXCLUDING it_fcode.
        WHEN 'ZCTE_TERC'.
          SET PF-STATUS 'SCREEN_101C' EXCLUDING it_fcode.
        WHEN 'ZMDFE'.
          SET PF-STATUS 'SCREEN_109' EXCLUDING it_fcode.
      ENDCASE.
    ENDIF.

  ENDFORM.                    " set_screen_status_100

  MODULE user_command_0109 INPUT.

    CASE sy-ucomm.
      WHEN 'CONFIRM'.

        IF wg_active_change-docnum IS INITIAL.
          MESSAGE 'Informe um numero de documento!' TYPE 'S'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM j_1bnfe_active INTO @DATA(wl_active_01)
         WHERE docnum EQ @wg_active_change-docnum.

        IF sy-subrc NE 0.
          MESSAGE 'Informe um numero de documento eletronico!' TYPE 'S'.
          EXIT.
        ENDIF.

*&***************************************************************Ajuste ZMDFe / Forçar encerramento docnum / Projeto S4/HANA / ANDERSON OENNING / 15-10-2023.

        IF wl_active_01-model = '58'.

          SELECT SINGLE *
            FROM tvarvc INTO @DATA(lwa_tvarvc)
           WHERE name = 'ZMDFE_AJUSTE_ST_MONITOR'.

          IF sy-subrc EQ 0.

            CLEAR: wg_zsdt0102.
            SELECT SINGLE * FROM zsdt0102
              INTO wg_zsdt0102
              WHERE docnum EQ wg_active_change-docnum.
            IF sy-subrc NE 0.
              MESSAGE |Documento eletronico { wg_active_change-docnum } não encontrado!| TYPE 'S'.
              EXIT.
            ENDIF.

            "Fazer alteração no status.
            IF wg_active_change-encerrado IS NOT INITIAL.
              wg_zsdt0102-encerrado  = abap_true. "Encerrado
              wg_zsdt0102-tp_authcod = '1'. "Autorizado
              wg_zsdt0102-usuario_enc = sy-uname.
              wg_zsdt0102-motivo_enc = 'Transação ZMDF-e'.
            ELSE.
              wg_zsdt0102-encerrado  = abap_false. "Encerrado
              wg_zsdt0102-tp_authcod = '1'. "Autorizado
              CLEAR: wg_zsdt0102-usuario_enc.
              wg_zsdt0102-motivo_enc = |Status Encerramento desmarcado por { sy-uname } |.
            ENDIF.

            IF wg_zsdt0102 IS NOT INITIAL.
              MODIFY zsdt0102 FROM wg_zsdt0102.
              COMMIT WORK.

            ENDIF.
          ENDIF.
        ENDIF.


        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(wl_doc_01)
         WHERE docnum EQ @wg_active_change-docnum.

        CHECK sy-subrc EQ 0.

        CHECK ( wg_active_change-change_st_doc EQ abap_true ) OR ( wg_active_change-change_dt_auth EQ abap_true ).

        IF wg_active_change-change_st_doc EQ abap_true.
          wl_active_01-docsta       = wg_active_change-docsta.
          wl_active_01-scssta       = wg_active_change-scssta.
          wl_active_01-cancel       = wg_active_change-cancel.
          wl_active_01-action_requ  = wg_active_change-action_requ.
          wl_active_01-msstat       = wg_active_change-msstat.

          wl_doc_01-docstat         = wg_active_change-docsta.
          wl_doc_01-cancel          = wg_active_change-cancel.
        ENDIF.

        IF wg_active_change-change_dt_auth EQ abap_true.
          wl_active_01-code     = wg_active_change-code.
          wl_active_01-docnum9  = wg_active_change-docnum9.
          wl_active_01-cdv      = wg_active_change-cdv.
          wl_active_01-authcod  = wg_active_change-authcod.
          wl_active_01-authdate = wg_active_change-authdate.
          wl_active_01-authtime = wg_active_change-authtime.

          wl_doc_01-manual      = wg_active_change-manual.
          wl_doc_01-authcod     = wg_active_change-authcod.
          wl_doc_01-authdate    = wg_active_change-authdate.
          wl_doc_01-authtime    = wg_active_change-authtime.
        ENDIF.

        CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
          EXPORTING
            i_acttab     = wl_active_01
            i_doc        = wl_doc_01
            i_updmode    = 'U'
          EXCEPTIONS
            update_error = 1
            OTHERS       = 2.

        IF sy-subrc EQ 0.
          MESSAGE 'Documento atualizado com sucesso!' TYPE 'S'.
        ELSE.
          MESSAGE 'Houve um erro ao atualizar o documento!' TYPE 'S'.
        ENDIF.


      WHEN 'CANCEL'.
        LEAVE TO SCREEN 0.
    ENDCASE.



  ENDMODULE.

  MODULE status_0109 OUTPUT.
    SET PF-STATUS 'PF0109'.
*  SET TITLEBAR 'xxx'.
  ENDMODULE.
