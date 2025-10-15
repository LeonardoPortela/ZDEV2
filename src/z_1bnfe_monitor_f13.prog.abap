*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 12/11/2024|DEVK9A1XAW |NSEGATIN       | Verificação de impedimento de cancelamento do retorno [157683]*
*--------------------------------------------------------------------------------------------------------*
*& 05/02/2025|DEVK9A1XAW |NSEGATIN       | Ajuste da validação da Nota Fiscal de Retorno [165575].       *
*--------------------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F13
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  request_cancellation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM request_cancellation .

  DATA: wa_active_log        TYPE j_1bnfe_active,           "1090279
        p_emite_ciot         TYPE char01,
        wa_j_1bnfdoc         TYPE j_1bnfdoc,
        it_cte_ciot          TYPE TABLE OF zcte_ciot,
        wa_cte_ciot          TYPE zcte_ciot,
        wa_cte_identifica    TYPE zcte_identifica,
        p_tknum              TYPE tknum,
        vg_tabix             TYPE sy-tabix,
        it_alv_selection_adm TYPE TABLE OF j_1bnfe_active WITH KEY docnum,
        wl_zlest0061         TYPE zlest0061,
        wl_j1bnfdoc          TYPE j_1bnfdoc,
        _param               TYPE  ustyp_t_parameters,
        autorizado(1).

  DATA(obj)            = NEW zcl_solicitacao_ov( ).
  DATA: text_status TYPE val_text,
        msg         TYPE char255.


  LOOP AT it_selected_rows INTO wa_selected_rows.
    index = sy-tabix.
  ENDLOOP.

* Check if a selection was made
  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

  CASE sy-tcode.
    WHEN 'ZNFE'.
      AUTHORITY-CHECK OBJECT 'ZSDCANCNFE' ID 'Z_CANC_NFE' FIELD '1'.
    WHEN 'ZCTE'.
      AUTHORITY-CHECK OBJECT 'ZSDCANCCTE' ID 'Z_CANC_CTE' FIELD '1'.
  ENDCASE.

  IF sy-subrc NE 0.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
    RETURN.
  ENDIF.

* Check authorization
  LOOP AT it_alv_selection INTO wa_alv_selection.

    "Verificar se existe MDF Ativa """"""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT * INTO TABLE @DATA(it_zsdt0105)
      FROM zsdt0105
     WHERE docnum EQ @wa_alv_selection-docnum.

    IF sy-subrc IS INITIAL.
      SELECT * INTO TABLE @DATA(it_zsdt0102)
        FROM zsdt0102
         FOR ALL ENTRIES IN @it_zsdt0105
       WHERE docnum EQ @it_zsdt0105-docnum_ref.
    ENDIF.

    LOOP AT it_zsdt0102 INTO DATA(wa_zsdt0102).
      IF wa_zsdt0102-autorizado EQ abap_true AND
         wa_zsdt0102-cancel     EQ abap_false.
        MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '005'.
      ENDIF.
    ENDLOOP.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

* Check authorization to cancel authorized NF-e documents
    IF wa_alv_selection-docsta = '1' AND
       gf_authorization_nfe_85 IS INITIAL.

      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '041'.
    ENDIF.
* Check authorization to cancel rejected NF-e documents
    IF wa_alv_selection-docsta = '2' AND
       gf_authorization_nfe_85 IS INITIAL.

      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '057'.
    ENDIF.

* Check Form not initial, else cancel through application
    IF wa_alv_selection-form IS INITIAL.                    "01368159
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '102'.           "01368159
    ENDIF.                                                  "01368159

    SELECT SINGLE * INTO wa_j_1bnfdoc FROM j_1bnfdoc WHERE docnum EQ wa_alv_selection-docnum.

    "===========================================Initial USER STORY 75024 21/03/2022 - Anderson Oenning
    "check user create document
    IF wa_j_1bnfdoc-crenam EQ 'WSSE_SIGAM'. "Se usuario de criação do documento for 'WSSE_SIGAM' não deixar cancelar.

* // pega os parametros do usuario
      CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
        EXPORTING
          user_name           = sy-uname
        TABLES
          user_parameters     = _param
        EXCEPTIONS
          user_name_not_exist = 1
          OTHERS              = 2.

      DELETE _param WHERE parid NE 'ZCANC_EXTEMP_INT'.

      "Check parametro ZCANC_EXTEMP_INT existente no perfil usuario.
      IF _param[] IS INITIAL.
        MESSAGE ID 'ZNFE_DISTRI' TYPE 'E' NUMBER '163'.
      ENDIF.
    ENDIF.
    "===========================================END USER STORY 75024 21/03/2022 - Anderson Oenning


    SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
      FROM j_1bnfe_active WHERE docnum EQ @wa_alv_selection-docnum.

    IF wa_j_1bnfe_active-docsta EQ '1'.
      CASE wa_j_1bnfe_active-model.
        WHEN zcl_doc_eletronico=>zif_doc_eletronico~at_st_model_cte.
          AUTHORITY-CHECK OBJECT 'ZSDCANCCTE' ID 'Z_CANC_CTE' FIELD '1'.
        WHEN zcl_doc_eletronico=>zif_doc_eletronico~at_st_model_nfe.
          AUTHORITY-CHECK OBJECT 'ZSDCANCNFE' ID 'Z_CANC_NFE' FIELD '1'.
        WHEN zcl_doc_eletronico=>zif_doc_eletronico~at_st_model_mdfe.
          "Rogerval mandou tirar
          "AUTHORITY-CHECK OBJECT 'ZSDCANCMFE' ID 'Z_CANC_MFE' FIELD '1'.
      ENDCASE.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '041'.
      ENDIF.
    ENDIF.

    PERFORM verifica_escrituracao_entrada USING wa_j_1bnfdoc.

    IF sy-tcode EQ 'ZCTE'.

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum = wa_alv_selection-docnum
        CHANGING
          p_bukrs  = wa_j_1bnfdoc-bukrs
          p_parid  = wa_j_1bnfdoc-parid
          p_partyp = wa_j_1bnfdoc-partyp
          p_tknum  = p_tknum.

      CALL FUNCTION 'Z_CIOT_EMPRESA_PARCEIRO'
        EXPORTING
          p_empresa    = wa_j_1bnfdoc-bukrs
          p_parid      = wa_j_1bnfdoc-parid
          p_partyp     = wa_j_1bnfdoc-partyp
          p_dt_posicao = wa_j_1bnfdoc-docdat
          p_tknum      = p_tknum
        IMPORTING
          p_emite      = p_emite_ciot.

      "Verificar se o docnum esta gravado na tabela do aquaviário.
      CLEAR: wl_zlest0061, wl_j1bnfdoc.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO wl_j1bnfdoc
       WHERE docnum = wa_alv_selection-docnum
         AND doctyp = '2'.

      IF ( sy-subrc = 0 ) AND ( wl_j1bnfdoc-docref IS NOT INITIAL ).
        DATA(_complemento) = 'X'.
      ENDIF.

      SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE docnum EQ wa_alv_selection-docnum.
      IF ( sy-subrc NE 0 ) AND ( wl_j1bnfdoc-docref IS NOT INITIAL ).
        CLEAR: wl_zlest0061.
        SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE docnum EQ wl_j1bnfdoc-docref.
      ENDIF.

      IF ( wl_zlest0061 IS INITIAL ).

        IF ( p_emite_ciot IS NOT INITIAL ) AND ( _complemento IS INITIAL ).

          CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
            EXPORTING
              p_cte_avulso = wa_nfe_alv-docnum
            TABLES
              it_cte_ciot  = it_cte_ciot
            EXCEPTIONS
              nao_ciot     = 1
              OTHERS       = 2.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_cte_ciot INTO wa_cte_ciot.

              CASE wa_cte_ciot-st_ciot. "CS2019000648 Status 4 5 6 não podem ser cancelado 291019
                WHEN '6'.
                  text_status = obj->get_txt_dominio( dominio = 'ZST_CIOT' value = CONV #( wa_cte_ciot-st_ciot ) ).
                  msg = |Não é possivel cancelar viagem com status "{ text_status }"!|.
                  MESSAGE msg TYPE 'E'.
                  CONTINUE.
              ENDCASE.


              IF ( wa_cte_ciot-st_ciot NE '8' ) AND ( 1 = 2 ). "Inversão Processo Cancelamento - Cancelar CT-e Primeiro, depois Viagem
                MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com status diferente de "Cancelado"'.
              ELSE.
                SELECT SINGLE * INTO wa_cte_identifica
                  FROM zcte_identifica
                 WHERE docnum EQ wa_cte_ciot-docnum.

                IF ( wa_cte_identifica-reason  IS INITIAL ) AND
                   ( wa_cte_identifica-reason1 IS INITIAL ) AND
                   ( wa_cte_identifica-reason2 IS INITIAL ) AND
                   ( wa_cte_identifica-reason3 IS INITIAL ) AND
                   ( wa_cte_identifica-reason4 IS INITIAL ).

                  CALL FUNCTION 'Z_SD_INFO_CTE_CANCELAR'
                    EXPORTING
                      p_docnum  = wa_cte_ciot-docnum
                      i_reason  = wa_cte_identifica-reason
                      i_reason1 = wa_cte_identifica-reason1
                      i_reason2 = wa_cte_identifica-reason2
                      i_reason3 = wa_cte_identifica-reason3
                      i_reason4 = wa_cte_identifica-reason4
                    IMPORTING
                      reason    = wa_cte_identifica-reason
                      reason1   = wa_cte_identifica-reason1
                      reason2   = wa_cte_identifica-reason2
                      reason3   = wa_cte_identifica-reason3
                      reason4   = wa_cte_identifica-reason4
                    EXCEPTIONS
                      error     = 1
                      OTHERS    = 2.

                  MODIFY zcte_identifica FROM wa_cte_identifica.
                ENDIF.

                wa_alv_selection-reason   = wa_cte_identifica-reason.
                wa_alv_selection-reason1  = wa_cte_identifica-reason1.
                wa_alv_selection-reason2  = wa_cte_identifica-reason2.
                wa_alv_selection-reason3  = wa_cte_identifica-reason3.
                wa_alv_selection-reason4  = wa_cte_identifica-reason4.
                APPEND wa_alv_selection TO it_alv_selection_adm.
              ENDIF.
            ENDLOOP.
          ENDIF.

        ENDIF.

      ELSE.
        CONTINUE.
      ENDIF.
**<<<------"157683 - NMS - INI------>>>
    ELSEIF sy-tcode EQ 'ZNFE'.
**<<<------"165575 - NMS - INI------>>>
      SELECT  COUNT( * ) FROM zsdt_export INTO @DATA(vl_dbcount) WHERE docnum EQ @wa_alv_selection-docnum.
      CHECK vl_dbcount IS NOT INITIAL.
**<<<------"165575 - NMS - FIM------>>>
      TRY.
* Verificação de impedimento de cancelamento do retorno.
          zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->validar_cancelamento_retorno( EXPORTING i_docnum = wa_alv_selection-docnum
                                                                                                            IMPORTING e_erro = DATA(vl_erro) ).
**<<<------"165575 - NMS - INI------>>>
          CLEAR vl_dbcount.
**<<<------"165575 - NMS - FIM------>>>
        CATCH zcx_controle_retorno_rfl INTO DATA(zcxl_controle_rfl).
          zcxl_controle_rfl->published_erro( EXPORTING i_msgty = 'E' i_msgty_display = 'W' ).
**<<<------"165575 - NMS - INI------>>>
          CLEAR vl_dbcount.
**<<<------"165575 - NMS - FIM------>>>
          RETURN.

      ENDTRY.
**<<<------"157683 - NMS - FIM------>>>
    ENDIF.

  ENDLOOP.

* Ask user to confirm cancellation
  MOVE index TO gf_index.
  CONCATENATE TEXT-160 gf_index
       INTO gs_101-textline1
       SEPARATED BY ' '.
  gs_101-textline2 = TEXT-170.

  CALL SCREEN '101' STARTING AT 25 6.

* Verify user's confirmation to cancel
  IF gf_cancel IS INITIAL.
    RETURN.
  ELSE.                                                     "1144194
*   User input for cancellation reason:
    IF sy-tcode EQ 'ZCTE'.
      LOOP AT it_alv_selection_adm INTO wa_alv_selection.
        DELETE it_alv_selection     WHERE docnum EQ wa_alv_selection-docnum.
        DELETE it_alv_selection_mod WHERE docnum EQ wa_alv_selection-docnum.
      ENDLOOP.
    ENDIF.
    IF it_alv_selection_adm[] IS INITIAL.
      CALL SCREEN '0102'.                                   "1144194
      IF sy-ucomm = 'BACK'.                                 "1165155
        LEAVE TO SCREEN 0.                                  "1165155
      ENDIF.
    ENDIF.                                                  "1165155
  ENDIF.

  CLEAR it_alv_error.
  REFRESH it_active_mod.                                    "1090279

  IF sy-tcode EQ 'ZCTE'.
    LOOP AT it_alv_selection_adm INTO wa_alv_selection.
      APPEND wa_alv_selection TO it_alv_selection.
      APPEND wa_alv_selection TO it_alv_selection_mod.
    ENDLOOP.
  ENDIF.

  CASE sy-tcode.
    WHEN 'ZMDFE'.

      LOOP AT it_alv_selection INTO wa_active_log.
        TRY .
            READ TABLE it_alv_selection_mod INTO DATA(wa_motivo) WITH KEY docnum = wa_active_log-docnum.
            DATA(obj_mdfe) = NEW zcl_mdfe( i_docnum = wa_active_log-docnum i_nmdfe = wa_active_log-nfnum9 ).
            obj_mdfe->set_just_canc( wa_motivo-reason1 && wa_motivo-reason2 && wa_motivo-reason3 && wa_motivo-reason4 ).
            obj_mdfe->cancelar_mdfe( ).
            CLEAR: obj_mdfe.

            SELECT SINGLE * INTO wa_active_mod
              FROM j_1bnfe_active
             WHERE docnum EQ wa_nfe_alv-docnum.

            DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
            MOVE-CORRESPONDING wa_active_log TO wa_nfe_alv.
            PERFORM registra_envio_data USING wa_nfe_alv c_x.
          CATCH cx_root.
            MOVE-CORRESPONDING wa_active_log TO wa_nfe_alv.
            PERFORM registra_envio_data USING wa_nfe_alv c_x.
        ENDTRY.
      ENDLOOP.

    WHEN OTHERS.

*-CS2024000086-26.09.2024-#151423-JT-inicio
      IF sy-tcode = 'ZNFE'.
        LOOP AT it_alv_selection INTO DATA(w_alv_sel).
          PERFORM f_cancelar_vt_vi_frete USING w_alv_sel-docnum.
        ENDLOOP.
      ENDIF.
*-CS2024000086-26.09.2024-#151423-JT-fim

      CALL FUNCTION 'J_1B_NFE_SEND_REQUESTS'
        TABLES
          it_acttab     = it_alv_selection
          et_errtab     = it_alv_error
          et_active_mod = it_active_mod                         "1090279
          it_acttab_mod = it_alv_selection_mod                  "1151112
        EXCEPTIONS
          status_error  = 1
          rfc_failure   = 2
          OTHERS        = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        LOOP AT it_alv_selection INTO wa_active_log.
          MOVE-CORRESPONDING wa_active_log TO wa_nfe_alv.
          PERFORM registra_envio_data USING wa_nfe_alv c_x.
        ENDLOOP.
      ENDIF.

  ENDCASE.

*
* COMMIT has been moved inside function J_1B_NFE_SEND_REQUESTS "1259918
*  IF it_alv_error IS INITIAL.                        "1090279  1259918
*    COMMIT WORK.                                     "1090279  1259918
*  ENDIF.                                             "1090279  1259918
*
                                                            "1090279
* Update ALV display                                          "1090279
  PERFORM grid_update USING space.                          "1090279

  IF NOT it_alv_error IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
  ELSE.
    MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
  ENDIF.

ENDFORM.                    " request_cancellation

*-CS2024000086-26.09.2024-#151423-JT-inicio
***********************************************************************************
* cancelar VT / VI
***********************************************************************************
FORM f_cancelar_vt_vi_frete USING p_docnum.

*  DATA: lwa_romaneio              TYPE zsdt0001,
*        lc_cockpit                TYPE zcockpit,                          "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico, "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        t_saida                   TYPE zde_les_saida_zsdt0001_t,          "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        w_saida                   TYPE zde_les_saida_zsdt0001,            "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        w_zsdt0001                TYPE zde_les_zsdt0001,                  "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        t_return                  TYPE TABLE OF bapireturn1,              "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        lc_erro                   TYPE char01,                            "*-CS2024000086-26.09.2024-#151423-JT-inicio
*        lc_gera_transp            TYPE char01.                            "*-CS2024000086-26.09.2024-#151423-JT-inicio
*
*  CREATE OBJECT lc_faturamento_automatico.
*
*  SELECT SINGLE *
*   FROM tvarvc INTO @DATA(tvarvc_bloq_frete_vt_vi)
*  WHERE name = 'ZNFE_NO_BLOCK_FRETE_CPT'.
*
*  CHECK sy-subrc NE 0.
*
*  CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
*    EXPORTING
*      i_docnum   = p_docnum
*    IMPORTING
*      e_zsdt0001 = lwa_romaneio.
*
*  CHECK lwa_romaneio IS NOT INITIAL AND lwa_romaneio-tp_movimento = 'S' AND lwa_romaneio-doc_rem IS NOT INITIAL.
*
*  CHECK lwa_romaneio-agente_frete IS NOT INITIAL.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lwa_romaneio-agente_frete
*    IMPORTING
*      output = lwa_romaneio-agente_frete.
*
*  SELECT SINGLE *
*    FROM lfa1 INTO @DATA(lwa_lfa1_check)
*   WHERE lifnr = @lwa_romaneio-agente_frete.
*
*  CHECK sy-subrc EQ 0.
*
**--------------------------
**-- tipo do cockpit
**--------------------------
*  lc_cockpit = lc_faturamento_automatico->get_ck_tipo_selecao( lwa_romaneio-ch_referencia ).
*
**--------------------------
**-- selecao romaneio para gerar VT/VI
**--------------------------
*  PERFORM f_selecao_fat_autom           IN PROGRAM zlesr0102    USING lwa_romaneio-ch_referencia
*                                                                      lc_cockpit.
*  PERFORM f_saida                       IN PROGRAM zlesr0102.
*  PERFORM f_recuperar_dados             IN PROGRAM zlesr0102 CHANGING t_saida
*                                                                      w_zsdt0001
*                                                                      w_saida.
*  PERFORM f_set_tipo_faturamento        IN PROGRAM zlesr0102    USING abap_off.
*  PERFORM f_check_gera_vt_frota_propria IN PROGRAM zlesr0102    USING lwa_romaneio-ch_referencia
*                                                             CHANGING lc_gera_transp.
*
*  CHECK lc_gera_transp = abap_true.
*
*  CHECK w_zsdt0001-fknum IS NOT INITIAL.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      text = |Estornando documentos de transporte e custo para o romaneio { CONV i( lwa_romaneio-nr_romaneio ) }.|.
*
**---------------------------
** estorno VT / VI
**---------------------------
*  PERFORM f_estorno_custo          IN PROGRAM zlesr0102 CHANGING w_saida.
*
*  DO 5 TIMES.
*    PERFORM f_repare_docs_romaneio IN PROGRAM zlesr0102 CHANGING w_saida.
*    IF w_saida-doccus IS INITIAL OR w_saida-doccus(1) = '@'.
*      EXIT.
*    ELSE.
*      WAIT UP TO 3 SECONDS.
*    ENDIF.
*  ENDDO.
*
*  IF w_saida-doccus IS INITIAL OR w_saida-doccus(1) = '@'.
*    MESSAGE |Documento de Transporte Estornado para o romaneio { lwa_romaneio-nr_romaneio }!| TYPE 'I'.
*  ENDIF.
*
ENDFORM.
*-CS2024000086-26.09.2024-#151423-JT-fim

***********************************************************************************
***********************************************************************************
