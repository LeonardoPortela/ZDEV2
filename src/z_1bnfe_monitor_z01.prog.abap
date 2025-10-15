*----------------------------------------------------------------------*
***INCLUDE Z_1BNFE_MONITOR_Z01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ENVIO_CTE
*&---------------------------------------------------------------------*
  FORM verifica_envio_cte  USING    l_subrc.

    DATA: mt_nfe_active TYPE TABLE OF j_1bnfe_active INITIAL SIZE 0 WITH HEADER LINE,
          wa_nfe_active TYPE j_1bnfe_active,
          wa_j_1bnfdoc  TYPE j_1bnfdoc,
          wa_cte_vttk   TYPE vttk,
          it_cte_ciot   TYPE TABLE OF zcte_ciot,
          wa_cte_ciot   TYPE zcte_ciot,
          p_emite_ciot  TYPE char01,
          p_empresa     TYPE bukrs,
          p_tp_forne    TYPE ztp_fornecimento,
          p_tknum       TYPE tknum,
          wa_zlest0061  TYPE zlest0061,
          wl_j1bnfdoc   TYPE j_1bnfdoc.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      index = sy-tabix.
    ENDLOOP.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    l_subrc = 0.

    READ TABLE bukrs INDEX 1.
    p_empresa = bukrs-low.

    LOOP AT it_selected_rows INTO wa_selected_rows.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

      CLEAR: mt_nfe_active, p_emite_ciot.

      DATA(sy_subrc) = 0.
      PERFORM f_sinc_documento_sap_ecc USING wa_nfe_alv-docnum 0 CHANGING sy_subrc.

      CHECK sy_subrc EQ 0.


      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum   = wa_nfe_alv-docnum
        CHANGING
          p_tp_forne = p_tp_forne.

      CALL FUNCTION 'Z_SD_NFES_DA_CTE'
        EXPORTING
          mp_docnum     = wa_nfe_alv-docnum
        TABLES
          mt_nfe_active = mt_nfe_active
        CHANGING
          mr_vttk       = wa_cte_vttk.

      IF p_tp_forne NE 'A'.

        LOOP AT mt_nfe_active INTO wa_nfe_active.

          IF wa_nfe_active-docsta NE '1'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 001 WITH wa_nfe_active-docnum.
            l_subrc = 4.
            RETURN.
          ELSE.
            IF wa_nfe_active-cancel IS NOT INITIAL.
              MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 002 WITH wa_nfe_active-docnum.
              l_subrc = 4.
              RETURN.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.

      "========================================INICIO USER STORY 108309 / AOENNING. / Valida  DOCNUM SUBCONTRATAÇÃO. /ZLES0181

      SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
      FROM j_1bnflin
      WHERE docnum EQ @wa_nfe_active-docnum.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM vbrp INTO @DATA(wa_vbrp)
        WHERE vbeln = @wa_j_1bnflin-refkey(10)
        AND posnr = @wa_j_1bnflin-refitm.
        IF sy-subrc EQ 0.
          SELECT SINGLE * INTO @DATA(wa_vbak)
          FROM vbak
          WHERE vbeln EQ @wa_vbrp-aubel.
          IF sy-subrc EQ 0 AND wa_vbak-auart EQ 'ZSSF'.
            SELECT SINGLE * FROM zlest0194 INTO @DATA(wa_zlest0194)
            WHERE docnum_sub EQ @wa_nfe_active-docnum.
            IF sy-subrc NE 0.
              MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 045 WITH wa_nfe_active-docnum wa_j_1bnflin-refkey(10).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "========================================FIM USER STORY 108309 / AOENNING. / Valida

      "Verifica status de transporte
      IF NOT wa_cte_vttk IS INITIAL.
        IF wa_cte_vttk-stdis IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 016 WITH wa_cte_vttk-tknum.
        ELSEIF wa_cte_vttk-streg IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 017 WITH wa_cte_vttk-tknum.
        ELSEIF wa_cte_vttk-stlbg IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 018 WITH wa_cte_vttk-tknum.
        ELSEIF wa_cte_vttk-stlad IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 019 WITH wa_cte_vttk-tknum.
        ELSEIF wa_cte_vttk-stabf IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 020 WITH wa_cte_vttk-tknum.
        ELSEIF wa_cte_vttk-sttbg IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 021 WITH wa_cte_vttk-tknum.
        ENDIF.
      ENDIF.

      SELECT SINGLE * INTO wa_j_1bnfdoc FROM j_1bnfdoc WHERE docnum EQ wa_nfe_alv-docnum.

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum = wa_nfe_alv-docnum
        CHANGING
          p_bukrs  = wa_j_1bnfdoc-bukrs
          p_parid  = wa_j_1bnfdoc-parid
          p_partyp = wa_j_1bnfdoc-partyp
          p_tknum  = p_tknum.

      CLEAR: wa_zlest0061.
      SELECT SINGLE * FROM zlest0061 INTO wa_zlest0061 WHERE docnum EQ wa_nfe_alv-docnum.

      IF sy-subrc NE 0.

        CLEAR: wl_j1bnfdoc.
        SELECT SINGLE *
          FROM j_1bnfdoc INTO wl_j1bnfdoc
         WHERE docnum = wa_nfe_alv-docnum
           AND doctyp = '2'.

        IF ( sy-subrc = 0 ) AND ( wl_j1bnfdoc-docref IS NOT INITIAL ).
          CLEAR: wa_zlest0061.
          SELECT SINGLE * FROM zlest0061 INTO wa_zlest0061 WHERE docnum EQ wl_j1bnfdoc-docref.
        ENDIF.

      ENDIF.

      IF ( wa_zlest0061 IS NOT INITIAL ) OR
         ( wa_zlest0061       IS INITIAL     AND
           wl_j1bnfdoc-docref IS NOT INITIAL AND
           wl_j1bnfdoc-doctyp = '2'    ). "Complementar


      ELSE.

        CALL FUNCTION 'Z_CIOT_EMPRESA_PARCEIRO'
          EXPORTING
            p_empresa    = wa_j_1bnfdoc-bukrs
            p_partyp     = wa_j_1bnfdoc-partyp
            p_parid      = wa_j_1bnfdoc-parid
            p_dt_posicao = wa_j_1bnfdoc-docdat
            p_tknum      = p_tknum
          IMPORTING
            p_emite      = p_emite_ciot.

        IF p_emite_ciot IS NOT INITIAL.

          "Verificar Viagem CIOT
          CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
            EXPORTING
              p_cte_avulso = wa_nfe_alv-docnum
            TABLES
              it_cte_ciot  = it_cte_ciot
            EXCEPTIONS
              nao_ciot     = 1
              OTHERS       = 2.

          IF sy-subrc <> 0.

            l_subrc = sy-subrc.

            "Atribuir rejeição ao documento
            "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
            "                                 SY-MSGV1 SY-MSGV2
            "                                 SY-MSGV3 SY-MSGV4.

            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

          ELSE.

            LOOP AT it_cte_ciot INTO wa_cte_ciot.
              CASE wa_cte_ciot-st_ciot.
                WHEN '0' OR space.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "pendente"'   ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "pendente"'.
                WHEN '1'.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "enviado"'  ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "enviado"'.
                WHEN '3'.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "Rejeitado"'  ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "Rejeitado"'.
                WHEN '4'.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Nr. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "creditado"'  ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Nr. CIOT' wa_cte_ciot-nr_ciot 'com estatus de "creditado"'.
                WHEN '5'.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "fechado (pago cockpit)"'  ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "fechado (pago cockpit)"'.
                WHEN '7'.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "Enviado Cancelamento"'  ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "Enviado Cancelamento"'.
                WHEN '8'.
                  "Atribuir rejeição ao documento
                  "PERFORM ALTERAR_STATUS_CTE USING '1' WA_NFE_ALV-DOCNUM
                  "                                 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT
                  "                                 'com estatus de "Cancelado"'  ''.

                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "Cancelado"'.
              ENDCASE.
            ENDLOOP.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.



  ENDFORM.                    " VERIFICA_ENVIO_CTE

  FORM alterar_status_cte USING p_tp_authcod TYPE zib_nota_fiscal_sap-tp_authcod
                                p_docnum     TYPE zib_nota_fiscal_sap-nu_documento_sap
                                p_msgv1
                                p_msgv2
                                p_msgv3
                                p_msgv4.

    DATA: it_notas TYPE TABLE OF zib_nota_fiscal_sap,
          wa_nota  TYPE zib_nota_fiscal_sap,
          v_docnum TYPE j_1bnfe_active-docnum.

    CLEAR: it_notas[], wa_nota.

    CHECK p_docnum IS NOT INITIAL.

    "Verifica se CT-e está autorizado.
    SELECT SINGLE docnum
      FROM j_1bnfe_active INTO v_docnum
     WHERE docnum   = p_docnum
       AND docsta   = '1'.

    CHECK sy-subrc NE 0.

    wa_nota-tp_authcod       = p_tp_authcod.
    wa_nota-nu_documento_sap = p_docnum.
    wa_nota-dt_authcod       = sy-datum.
    IF sy-timlo IS INITIAL.
      wa_nota-hr_authcod   = sy-uzeit.
    ELSE.
      wa_nota-hr_authcod   = sy-timlo.
    ENDIF.

    CONCATENATE p_msgv1 p_msgv2 p_msgv3 p_msgv4 INTO wa_nota-ms_erro SEPARATED BY space.

    APPEND wa_nota TO it_notas.

    CALL FUNCTION 'Z_SD_INBOUND_NFE_XML'
      TABLES
        it_notas = it_notas.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NOTA_POSSUI_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM verifica_nota_possui_cte  USING    p_l_subrc.

    DATA: wl_zlest0146 TYPE zlest0146,
          lt_zlest0147 TYPE zlest0147_t,
          lt_zlest0168 TYPE zlest0168_t,
          v_doc_rateio TYPE char01.

    DATA: mr_status TYPE c LENGTH 1,
          mr_cte    TYPE j_1bnfe_active.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      index = sy-tabix.
    ENDLOOP.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    l_subrc = 0.

    LOOP AT it_selected_rows INTO wa_selected_rows.

      CALL FUNCTION 'Z_SD_CTE_DA_NFE'
        EXPORTING
          p_docnum  = wa_nfe_alv-docnum
        CHANGING
          mr_cte    = mr_cte
          mr_status = mr_status.

      IF mr_status EQ 'M'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 005 WITH wa_nfe_active-docnum.
      ELSEIF mr_status EQ 'E'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 006 WITH mr_cte-docnum.
      ELSEIF mr_status EQ 'N'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 007 WITH mr_cte-docnum.
      ELSEIF mr_status EQ 'D'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 008 WITH mr_cte-docnum.
      ELSEIF mr_status EQ 'F'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 009 WITH mr_cte-docnum.
      ENDIF.

      "Check se Documento está registrado no CCT
      CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
        EXPORTING
          i_docnum     = wa_nfe_alv-docnum
        IMPORTING
          e_zlest0146  = wl_zlest0146
          e_zlest0147  = lt_zlest0147
          e_zlest0168  = lt_zlest0168
          e_doc_rateio = v_doc_rateio.

      IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 041 WITH wa_nfe_alv-docnum wl_zlest0146-id_recepcao.
      ENDIF.

      "Check Vinculo DU-e
      SELECT SINGLE *
        FROM zsdt0172 AS a INTO @DATA(_wl_0172)
       WHERE docnum EQ @wa_nfe_alv-docnum
         AND EXISTS ( SELECT *
                        FROM zsdt0170 AS b
                       WHERE b~id_due = a~id_due
                         AND b~loekz  = @abap_false ).

      IF sy-subrc EQ 0.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 042 WITH wa_nfe_alv-docnum _wl_0172-id_due.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0173 AS a INTO @DATA(_wl_0173)
       WHERE docnum EQ @wa_nfe_alv-docnum
         AND EXISTS ( SELECT *
                        FROM zsdt0170 AS b
                       WHERE b~id_due = a~id_due
                         AND b~loekz  = @abap_false ).

      IF sy-subrc EQ 0.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 042 WITH wa_nfe_alv-docnum _wl_0173-id_due.
      ENDIF.

    ENDLOOP.

  ENDFORM.                    " VERIFICA_NOTA_POSSUI_CTE

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ENVIO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SUBRC  text
*----------------------------------------------------------------------*
  FORM verifica_envio_data  USING  l_subrc.

    DATA: vl_guia_agro         TYPE c,         "<<<------"188425 - NMS ------->>>
          vl_msg_numguia_usada TYPE char128.   "<<<------"188425 - NMS ------->>>

    DATA: vg_j_1bnfdoc    TYPE j_1bnfdoc,
          wa_zjcnd_branch TYPE zjcnd_branch,
          wa_j_1bbranch   TYPE j_1bbranch,
          wa_adrc         TYPE adrc.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      index = sy-tabix.
    ENDLOOP.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    l_subrc = 0.

    LOOP AT it_selected_rows INTO wa_selected_rows.

      CLEAR: vg_j_1bnfdoc.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
**<<<------"188425 - NMS - INI------>>>
      CLEAR: eg_guia_agro.
* Validação da Guia Agropecuária
      PERFORM zf_valida_guia_agropecuaria USING    wa_nfe_alv-docnum
                                          CHANGING eg_guia_agro
                                                   vl_guia_agro.
      IF     NOT vl_guia_agro IS INITIAL AND
                 eg_guia_agro IS INITIAL.
        MESSAGE |Obrigatório o preenchimento da Guia Agropecuária para esta Nota.| TYPE 'E'.
        RETURN.

      ELSEIF NOT vl_guia_agro IS INITIAL AND
             NOT eg_guia_agro IS INITIAL.
* Verifica se o Número da Guia Agropecuária já foi usada.
        PERFORM zf_valida_num_guia_agro_usada USING    eg_guia_agro
                                              CHANGING vl_msg_numguia_usada.

        IF NOT vl_msg_numguia_usada IS INITIAL.
          MESSAGE vl_msg_numguia_usada TYPE 'E'.

        ENDIF.

      ENDIF.
**<<<------"188425 - NMS - FIM------>>>
      IF wa_nfe_alv-nfnum9 IS INITIAL.

        SELECT SINGLE * INTO vg_j_1bnfdoc
          FROM j_1bnfdoc
         WHERE docnum EQ wa_nfe_alv-docnum.

        "Verifica status de transporte
        IF vg_j_1bnfdoc-docdat NE sy-datum.

          CASE sy-tcode.
            WHEN 'ZNFE'.
              AUTHORITY-CHECK OBJECT 'ZSDRETRONF' ID 'Z_DT_RETNF' FIELD '1'.
            WHEN 'ZCTE'.
              sy-subrc = 1.
          ENDCASE.

          IF sy-subrc NE 0.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 022.
            l_subrc = 4.
            RETURN.
          ENDIF.

        ENDIF.

        SELECT SINGLE * INTO wa_j_1bbranch
          FROM j_1bbranch
         WHERE bukrs       EQ vg_j_1bnfdoc-bukrs
           AND branch      EQ vg_j_1bnfdoc-branch.

        IF sy-subrc IS INITIAL.

          CLEAR: wa_adrc.

          SELECT SINGLE * INTO wa_adrc
            FROM adrc
           WHERE addrnumber EQ wa_j_1bbranch-adrnr.

          IF ( wa_adrc-country EQ 'BR' ) AND ( wa_adrc-region EQ 'MT' ).

            SELECT SINGLE * INTO wa_zjcnd_branch
              FROM zjcnd_branch
             WHERE bukrs       EQ vg_j_1bnfdoc-bukrs
               AND branch      EQ vg_j_1bnfdoc-branch
               AND dt_emissao  LE vg_j_1bnfdoc-pstdat
               AND dt_validade GE vg_j_1bnfdoc-pstdat.

            IF NOT sy-subrc IS INITIAL.
              MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 036 WITH vg_j_1bnfdoc-branch.
            ENDIF.

          ENDIF.

        ENDIF.

        SELECT SINGLE *
          FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
         WHERE docnum EQ @wa_nfe_alv-docnum.

        IF ( sy-subrc EQ 0 ) AND ( _wl_zfiwrt0008-tcode_org = 'ZNFW0009' ).
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 043.
          l_subrc = 4.
          RETURN.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDFORM.                    " VERIFICA_ENVIO_DATA

*&---------------------------------------------------------------------*
*&      Form  REGISTRA_ENVIO_DATA
*&---------------------------------------------------------------------*
*       Registra log de processamento de autorização / cancelamento
*----------------------------------------------------------------------*
  FORM registra_envio_data USING registro LIKE wa_nfe_alv tp_autorizacao TYPE xfeld.

    DATA: wa_zib_nfe TYPE zib_nfe.

    "Autorização
    IF tp_autorizacao IS INITIAL.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
      SELECT SINGLE * INTO wa_zib_nfe
        FROM zib_nfe
       WHERE docnum = wa_nfe_alv-docnum.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

      wa_zib_nfe-docnum     = wa_nfe_alv-docnum.
      wa_zib_nfe-date_aut_1 = sy-datum.
      wa_zib_nfe-time_aut_1 = sy-uzeit.
      wa_zib_nfe-user_aut_1 = sy-uname.
      MODIFY zib_nfe FROM wa_zib_nfe.
      "Cancelamento
    ELSE.

      SELECT SINGLE * INTO wa_zib_nfe
        FROM zib_nfe
       WHERE docnum = wa_nfe_alv-docnum.

      IF sy-subrc IS INITIAL.
        wa_zib_nfe-date_aut_2 = sy-datum.
        wa_zib_nfe-time_aut_2 = sy-uzeit.
        wa_zib_nfe-user_aut_2 = sy-uname.
        MODIFY zib_nfe FROM wa_zib_nfe.
      ENDIF.

    ENDIF.

  ENDFORM.                    " REGISTRA_ENVIO_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_DADOS_EXPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM set_dados_exportacao .

    DATA: wa_j_1bnfe_active TYPE j_1bnfe_active,
          it_itens          TYPE TABLE OF j_1bnflin INITIAL SIZE 0,
          wa_itens          TYPE j_1bnflin.

* Check authorization
    IF gf_authorization_nfe_35 IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
    ENDIF.

* Check if an NF-e selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

* Send NF-e that was posted under contingency
    CLEAR subrc.
    REFRESH it_active_mod.                                  "1090279
    LOOP AT it_selected_rows INTO wa_selected_rows.

      CLEAR: subrc.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

* Send NF-e only if form is not initial                    "1396498
      IF wa_nfe_alv-form IS INITIAL.                        "1396498
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '103'.         "1396498
      ENDIF.                                                "1396498
*
      SELECT SINGLE * INTO wa_j_1bnfe_active
        FROM j_1bnfe_active
       WHERE docnum EQ wa_nfe_alv-docnum.

      IF NOT (  ( ( wa_j_1bnfe_active-docsta IS INITIAL ) OR ( wa_j_1bnfe_active-docsta EQ '3' ) ) AND
                  ( wa_j_1bnfe_active-cancel IS INITIAL ) ).
        MESSAGE w014(zsimetrya) WITH wa_nfe_alv-docnum.
        subrc = 'X'.
      ENDIF.

      SELECT * INTO TABLE it_itens
        FROM j_1bnflin
       WHERE docnum EQ wa_nfe_alv-docnum.

      READ TABLE it_itens INDEX 1 INTO wa_itens.

*    IF ( ( SY-SUBRC IS INITIAL ) AND ( WA_ITENS-CFOP(1) EQ '7' ) ).
*      IF SUBRC IS INITIAL.                                  "1265172
*        CALL FUNCTION 'Z_SD_INFO_NFE_EXPORTACAO'
*          EXPORTING
*            P_DOCNUM = WA_NFE_ALV-DOCNUM.
*      ENDIF.                                                "1265172
*    ELSE.
*      MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER '023' WITH 'CFOP não é do tipo exportação!'.
*    ENDIF.


    ENDLOOP.

* Update ALV display                                          "1090279
    PERFORM grid_update USING space.                        "1090279

    IF NOT subrc IS INITIAL.
      CASE sy-tcode.
        WHEN 'ZNFE'.
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
      ENDCASE.
    ELSE.
      CASE sy-tcode.
        WHEN 'ZNFE'.
          MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
      ENDCASE.
    ENDIF.

  ENDFORM.                    " SET_DADOS_EXPORTACAO


*&---------------------------------------------------------------------*
*&      Form  CHAMA_VISUALIZACAO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM chama_visualizacao_cte .

    DATA: wa_j_1bnfdoc TYPE j_1bnfdoc,
          p_emite_ciot TYPE char01,
          vg_msgv1     TYPE symsgv,
          vg_msgv2     TYPE symsgv,
          vg_msgv3     TYPE symsgv,
          vg_msgv4     TYPE symsgv.

    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    READ TABLE it_selected_rows INDEX 2 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '009'.
    ENDIF.

    READ TABLE it_selected_rows INDEX 1 INTO wa_selected_rows.
    READ TABLE it_nfe_alv INDEX wa_selected_rows-index INTO wa_nfe_alv.

    CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
      EXPORTING
        p_cte_avulso       = wa_nfe_alv-docnum
        p_chamar_tela      = 'X'
        p_gravar_dados     = 'X'
      EXCEPTIONS
        n_info_cte         = 1
        n_loc_cte          = 2
        n_eletronico       = 3
        n_modelo_57        = 4
        n_status           = 5
        inf_docnum         = 6
        inf_propveiculo    = 7
        nao_docnum         = 8
        nao_rtrc           = 9
        nao_conta_corrente = 10
        moto_nao_pf        = 11
        n_placa_cad        = 12
        sem_notas          = 13
        OTHERS             = 14.

    IF NOT sy-subrc IS INITIAL.

      vg_msgv1 = sy-msgv1.
      vg_msgv2 = sy-msgv2.
      vg_msgv3 = sy-msgv3.
      vg_msgv4 = sy-msgv4.

      CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
        EXPORTING
          p_cte_avulso   = wa_nfe_alv-docnum
          p_chamar_tela  = space
          p_gravar_dados = space
          p_apagar_dados = c_x.

      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH vg_msgv1 vg_msgv2 vg_msgv3 vg_msgv4.

    ENDIF.

  ENDFORM.                    " CHAMA_VISUALIZACAO_CTE

*&---------------------------------------------------------------------*
*&      Form  SOLICITAR_CIOT
*&---------------------------------------------------------------------*
  FORM solicitar_ciot RAISING zcx_error. "*-#133089-12.02.2024-JT

    DATA: wa_j_1bnfdoc      TYPE j_1bnfdoc,
          p_emite_ciot      TYPE char01,
          wa_j_1bnflin      TYPE j_1bnflin,
          wa_vbfa           TYPE vbfa,
          wa_vbak           TYPE vbak,
          wa_vttk           TYPE vttk,
          wa_zcte_info_nota TYPE zcte_info_nota.

*-#133089-12.02.2024-JT-inicio
    IF vg_faturamento_autom = abap_true.
      CREATE OBJECT lc_faturamento_automatico.
    ENDIF.
*-#133089-12.02.2024-JT-fim

    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    READ TABLE it_selected_rows INDEX 2 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '009'.
    ENDIF.

    READ TABLE it_selected_rows INDEX 1 INTO wa_selected_rows.
    READ TABLE it_nfe_alv INDEX wa_selected_rows-index INTO wa_nfe_alv.

    CALL FUNCTION 'Z_SD_INFO_CTE_SEGURO'
      EXPORTING
        p_cte_avulso          = wa_nfe_alv-docnum
        p_cte_verifica_seguro = abap_true
      EXCEPTIONS
        erro                  = 1
        OTHERS                = 2.

    IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          EXIT.
        WHEN abap_true.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
          EXIT.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

    SELECT SINGLE * FROM j_1bnflin INTO wa_j_1bnflin  WHERE docnum EQ wa_nfe_alv-docnum.

    SELECT SINGLE * FROM vbfa INTO wa_vbfa WHERE vbeln EQ wa_j_1bnflin-refkey
                                             AND vbtyp_n = 'M'.

    SELECT SINGLE * FROM vbak INTO wa_vbak WHERE vbeln EQ wa_vbfa-vbelv.

    IF wa_vbak-tknum IS INITIAL.
      SELECT SINGLE *
        FROM zcte_identifica INTO @DATA(wl_identifica_tmp)
       WHERE docnum EQ @wa_nfe_alv-docnum.

      IF ( sy-subrc EQ 0 ) AND ( wl_identifica_tmp-tknum IS NOT INITIAL ).
        wa_vbak-tknum = wl_identifica_tmp-tknum.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM vttk INTO wa_vttk WHERE tknum EQ wa_vbak-tknum.

    IF ( sy-subrc EQ 0 ) AND ( wa_vttk-shtyp EQ 'Z026' ).

      SELECT SINGLE * FROM zcte_info_nota INTO wa_zcte_info_nota WHERE docnum EQ wa_nfe_alv-docnum.

      IF ( sy-subrc NE 0 ).
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '137'.
          WHEN abap_true.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '137' INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ELSE.
*-#133089-21.02.2024-JT-inicio
        CALL FUNCTION 'Z_SD_EMITE_CIOT'
          EXPORTING
            p_cte_avulso        = wa_nfe_alv-docnum
            p_tela_visualiza    = 'X'
            p_faturamento_autom = vg_faturamento_autom  "*-#133089-21.02.2024-JT
            p_ch_referencia     = vg_ch_referencia      "*-#133089-21.02.2024-JT
          EXCEPTIONS
            sem_dados_ciot      = 1
            nao_ciot            = 2
            erro_status         = 3
            erro_web_service    = 4
            erro_status_cred    = 5
            erro_status_canc    = 6
            erro_solicitacao    = 7
            OTHERS              = 8.
        IF sy-subrc <> 0.
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            WHEN abap_true.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
          ENDCASE.
        ENDIF.
*-#133089-21.02.2024-JT-fim

*       CALL FUNCTION 'Z_SD_EMITE_CIOT'
*         EXPORTING
*           p_cte_avulso     = wa_nfe_alv-docnum
*           p_tela_visualiza = 'X'.
*-#133089-21.02.2024-JT-fim

        PERFORM grid_update_viagem USING wa_nfe_alv-docnum.

      ENDIF.

    ELSEIF ( sy-subrc EQ 0 ) AND ( wa_vttk-shtyp NE 'Z026' ).

*-#133089-21.02.2024-JT-inicio
      CALL FUNCTION 'Z_SD_EMITE_CIOT'
        EXPORTING
          p_cte_avulso        = wa_nfe_alv-docnum
          p_tela_visualiza    = 'X'
          p_faturamento_autom = vg_faturamento_autom  "*-#133089-21.02.2024-JT
          p_ch_referencia     = vg_ch_referencia      "*-#133089-21.02.2024-JT
        EXCEPTIONS
          sem_dados_ciot      = 1
          nao_ciot            = 2
          erro_status         = 3
          erro_web_service    = 4
          erro_status_cred    = 5
          erro_status_canc    = 6
          erro_solicitacao    = 7
          OTHERS              = 8.
      IF sy-subrc <> 0.
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          WHEN abap_true.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
        ENDCASE.
      ENDIF.

*     CALL FUNCTION 'Z_SD_EMITE_CIOT'
*       EXPORTING
*         p_cte_avulso     = wa_nfe_alv-docnum
*         p_tela_visualiza = 'X'.
*-#133089-21.02.2024-JT-fim

      PERFORM grid_update_viagem USING wa_nfe_alv-docnum.

    ELSE.

      SELECT SINGLE *
        FROM zlest0194
       WHERE docnum_sub = @wa_nfe_alv-docnum
        INTO @DATA(wa_zlest0194).

      IF wa_zlest0194-docnum_sub IS NOT INITIAL.

*-#133089-21.02.2024-JT-inicio
        CALL FUNCTION 'Z_SD_EMITE_CIOT'
          EXPORTING
            p_cte_avulso        = wa_nfe_alv-docnum
            p_tela_visualiza    = 'X'
            p_faturamento_autom = vg_faturamento_autom  "*-#133089-21.02.2024-JT
            p_ch_referencia     = vg_ch_referencia      "*-#133089-21.02.2024-JT
          EXCEPTIONS
            sem_dados_ciot      = 1
            nao_ciot            = 2
            erro_status         = 3
            erro_web_service    = 4
            erro_status_cred    = 5
            erro_status_canc    = 6
            erro_solicitacao    = 7
            OTHERS              = 8.
        IF sy-subrc <> 0.
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            WHEN abap_true.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
          ENDCASE.
        ENDIF.

*       CALL FUNCTION 'Z_SD_EMITE_CIOT'
*         EXPORTING
*           p_cte_avulso     = wa_nfe_alv-docnum
*           p_tela_visualiza = 'X'.
*-#133089-21.02.2024-JT-fim

        PERFORM grid_update_viagem USING wa_nfe_alv-docnum.

      ENDIF.

    ENDIF.

  ENDFORM.                    " SOLICITAR_CIOT


*&---------------------------------------------------------------------*
*&      Form  SET_STATUS_CIOT
*&---------------------------------------------------------------------*
*       Verifica estados dos CIOT emitidos para a viagem
*----------------------------------------------------------------------*
*      <--P_WA_NFE_ALV  text
*----------------------------------------------------------------------*
  FORM set_status_ciot  CHANGING p_wa_nfe_alv LIKE wa_nfe_alv.

    DATA: it_cte_ciot  TYPE TABLE OF zcte_ciot WITH HEADER LINE,
          wa_cte_ciot  TYPE zcte_ciot,
          wa_cte_cioa  TYPE zcte_ciot,
          p_emite_ciot TYPE char01,
          wa_j_1bnfdoc TYPE j_1bnfdoc,
          p_tknum      TYPE tknum.

    CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
      EXPORTING
        p_cte_avulso = p_wa_nfe_alv-docnum
      TABLES
        it_cte_ciot  = it_cte_ciot
      EXCEPTIONS
        nao_ciot     = 1
        OTHERS       = 2.

    IF sy-subrc IS INITIAL.

      CLEAR: wa_cte_cioa.

      LOOP AT it_cte_ciot INTO wa_cte_ciot.

        CALL FUNCTION 'Z_SD_ICON_STATUS_CIOT'
          EXPORTING
            p_st_ciot = wa_cte_ciot-st_ciot
          IMPORTING
            icone     = p_wa_nfe_alv-status_ciot.

        MOVE-CORRESPONDING wa_cte_ciot TO wa_cte_cioa.

      ENDLOOP.

    ELSE.

      p_wa_nfe_alv-status_ciot = icon_complete.

      SELECT SINGLE * INTO wa_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum EQ p_wa_nfe_alv-docnum.

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum = p_wa_nfe_alv-docnum
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

      IF NOT p_emite_ciot IS INITIAL.
        p_wa_nfe_alv-status_ciot = icon_alert.
      ENDIF.

    ENDIF.

  ENDFORM.                    " SET_STATUS_CIOT

*&---------------------------------------------------------------------*
*&      Form  SET_DADOS_IMPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM set_dados_importacao .

    DATA: wa_j_1bnfe_active TYPE j_1bnfe_active,
          it_itens          TYPE TABLE OF j_1bnflin INITIAL SIZE 0,
          wa_itens          TYPE j_1bnflin.

* Check authorization
    IF gf_authorization_nfe_35 IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
    ENDIF.

* Check if an NF-e selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

* Send NF-e that was posted under contingency
    CLEAR subrc.
    REFRESH it_active_mod.                                  "1090279
    LOOP AT it_selected_rows INTO wa_selected_rows.

      CLEAR: subrc.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

* Send NF-e only if form is not initial                    "1396498
      IF wa_nfe_alv-form IS INITIAL.                        "1396498
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '103'.         "1396498
      ENDIF.                                                "1396498
*
      SELECT SINGLE * INTO wa_j_1bnfe_active
        FROM j_1bnfe_active
       WHERE docnum EQ wa_nfe_alv-docnum.

      IF NOT (  ( ( wa_j_1bnfe_active-docsta IS INITIAL ) OR ( wa_j_1bnfe_active-docsta EQ '3' ) ) AND
                  ( wa_j_1bnfe_active-cancel IS INITIAL ) ).
        MESSAGE w014(zsimetrya) WITH wa_nfe_alv-docnum.
        subrc = 'X'.
      ENDIF.

      SELECT * INTO TABLE it_itens
        FROM j_1bnflin
       WHERE docnum EQ wa_nfe_alv-docnum.

      READ TABLE it_itens INDEX 1 INTO wa_itens.

      IF ( ( sy-subrc IS INITIAL ) AND ( wa_itens-cfop(1) EQ '3' ) ).
        IF subrc IS INITIAL.                                "1265172
          CALL FUNCTION 'Z_ADD_INF_IMPORTACAO'
            EXPORTING
              p_docnum = wa_nfe_alv-docnum.
        ENDIF.                                              "1265172
      ELSE.
        MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER '023' WITH 'CFOP não é do tipo importação!'.
      ENDIF.

    ENDLOOP.

* Update ALV display                                          "1090279
    PERFORM grid_update USING space.                        "1090279

    IF NOT subrc IS INITIAL.
      CASE sy-tcode.
        WHEN 'ZNFE'.
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
      ENDCASE.
    ELSE.
      CASE sy-tcode.
        WHEN 'ZNFE'.
          MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
      ENDCASE.
    ENDIF.

  ENDFORM.                    " SET_DADOS_IMPORTACAO


*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ESCRITURACAO_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM verifica_escrituracao_entrada  USING  p_selection TYPE j_1bnfdoc.

    DATA: vg_parceiro     TYPE j_1bparid,
          vg_filial       TYPE j_1bbranc_,
          wa_filial       TYPE j_1bbranch,
          vg_nota_entrada TYPE j_1bnfdoc.

    CHECK p_selection-direct EQ '2'.

    vg_parceiro = p_selection-parid.

    IF p_selection-partyp EQ 'B'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_parceiro
        IMPORTING
          output = vg_parceiro.

      vg_parceiro = vg_parceiro+6(4).

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = vg_parceiro
        IMPORTING
          output = vg_parceiro.

    ENDIF.

    IF strlen( vg_parceiro ) LE 4.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_parceiro
        IMPORTING
          output = vg_parceiro.

      vg_filial = vg_parceiro+6(4).

      SELECT SINGLE * INTO wa_filial
        FROM j_1bbranch
        WHERE branch EQ vg_filial.

      IF sy-subrc IS INITIAL.

        IF p_selection-partyp EQ 'B'.
          CONCATENATE p_selection-bukrs p_selection-branch INTO vg_parceiro.
        ELSE.

          vg_parceiro = p_selection-branch.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vg_parceiro
            IMPORTING
              output = vg_parceiro.

        ENDIF.

        SELECT SINGLE * INTO vg_nota_entrada
          FROM j_1bnfdoc
         WHERE direct EQ '1'
           AND bukrs  EQ wa_filial-bukrs
           AND branch EQ wa_filial-branch
           AND nfe    EQ p_selection-nfe
           AND parid  EQ vg_parceiro
           AND series EQ p_selection-series
           AND model  EQ p_selection-model
           AND nfenum EQ p_selection-nfenum
           AND cancel NE 'X'
           AND doctyp NE '5'.

        IF sy-subrc IS INITIAL.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Doc. Fiscal com Entrada Escriturada.' 'Documento:' vg_nota_entrada-docnum.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDFORM.                    " VERIFICA_ESCRITURACAO_ENTRADA

**&---------------------------------------------------------------------*
**&      Form  IMPRIME_DECLARACAO_MOTORISTA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM IMPRIME_DECLARACAO_MOTORISTA .
*
** Check if an NF-e selection was made
*  if it_selected_rows is initial.
*    message id 'J1B_NFE' type 'E' number '030'.
*    return.
*  endif.
*
*  LOOP AT it_selected_rows INTO wa_selected_rows.
*
*    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
** process numbering only for RFC call types 1 and 2
*
*      CALL FUNCTION 'Z_SD_PRINT_DECLARA'
*        EXPORTING
*          DOC_NUMERO     = wa_nfe_alv-docnum
*        EXCEPTIONS
*          NAO_LOCALIZADO = 1
*          OTHERS         = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.                    " IMPRIME_DECLARACAO_MOTORISTA

  FORM cancelamento_extemp_interface .

    DATA: vl_message    TYPE string,
          var_answer    TYPE c,
          ls_acttab_new TYPE j_1bnfe_active,
          ls_doc_new    TYPE j_1bnfdoc,
          _param        TYPE  ustyp_t_parameters.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.

    CHECK sy-subrc = 0.

    CLEAR: vl_message, var_answer, ls_acttab_new, ls_doc_new.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM j_1bnfdoc AS a INTO ls_doc_new
     WHERE a~docnum EQ wa_nfe_alv-docnum
       AND a~crenam IN ( 'R3JOB', 'JOBADM' )
       AND EXISTS ( SELECT docnum
                      FROM j_1bnfe_active AS b
                     WHERE b~docnum EQ a~docnum
                       AND b~docsta EQ '1'
                       AND b~scssta EQ '' ).

    CHECK ( sy-subrc = 0 ) AND ( ls_doc_new IS NOT INITIAL ).

    "Documento Gerado pelo SAP(GRC) x SIGAM
    SELECT SINGLE * INTO @DATA(wa_zsdt0231)
      FROM zsdt0231
     WHERE obj_key EQ @wa_nfe_alv-docnum.

    IF sy-subrc NE 0.

      CONCATENATE 'Deseja realmente cancelar o documento' wa_nfe_alv-docnum '?'
             INTO vl_message SEPARATED BY space.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = vl_message
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

*   Get current status of NF-e
      CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
        EXPORTING
          i_docnum = wa_nfe_alv-docnum
        IMPORTING
          e_acttab = ls_acttab_new
        EXCEPTIONS
          no_entry = 1
          OTHERS   = 2.

      CHECK sy-subrc EQ 0.

      "Nota Fiscal
      MOVE '1'                   TO ls_doc_new-docstat.
      "Active Nota Fiscal
      MOVE 'C'                   TO ls_acttab_new-action_requ.
      MOVE '2'                   TO ls_acttab_new-scssta.
      MOVE 'X'                   TO ls_acttab_new-cancel.
      MOVE 'B'                   TO ls_acttab_new-msstat.


      MODIFY j_1bnfdoc      FROM ls_doc_new.
      MODIFY j_1bnfe_active FROM ls_acttab_new.

      COMMIT WORK.

    ELSE.

      CONCATENATE 'Deseja realmente desvincular todos os movimentos do SAP vincualdo ao documento' wa_nfe_alv-docnum '?'
             INTO vl_message SEPARATED BY space.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = vl_message
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      UPDATE j_1bnflin SET reftyp = space refkey = space
       WHERE docnum EQ wa_nfe_alv-docnum.

      COMMIT WORK.

    ENDIF.

  ENDFORM.

  FORM force_reenvio_grc.

    DATA: var_answer    TYPE c.

    CLEAR: var_answer.

    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.

    CHECK sy-subrc = 0.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = |Deseja realmente forçar o reenvio do documento { wa_nfe_alv-docnum } ?|
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK var_answer EQ '1'.

    CALL FUNCTION 'Z_GRC_AJUSTA_TP_EMISSAO'
      EXPORTING
        i_docnum          = wa_nfe_alv-docnum
      EXCEPTIONS
        no_numbering      = 1
        not_unique_server = 2
        update_error      = 3
        error             = 4
        OTHERS            = 5.
    IF sy-subrc EQ 0.

      WAIT UP TO 5 SECONDS.

      PERFORM send_nfe_again.

      MESSAGE 'Documento reenviado ao GRC!' TYPE 'S'.
    ELSE.
      MESSAGE 'Houve um erro ao reenviar o documento ao GRC!' TYPE 'S'.
    ENDIF.

  ENDFORM.

  FORM verifica_canc_nota  USING l_subrc.

    DATA: vbrp_vbeln TYPE vbrp-vbeln.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      l_subrc = 4.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    LOOP AT it_selected_rows INTO wa_selected_rows.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

      CHECK ( sy-subrc = 0 ) AND ( wa_nfe_alv-docnum IS NOT INITIAL ).

*   "Verifica se a Remessa tem Entrada lançada.  IR059834 Inicio
      SELECT SINGLE *
        FROM j_1bnflin
        INTO @DATA(vl_reftyp)
        WHERE docnum EQ @wa_nfe_alv-docnum
          AND reftyp EQ 'BI'.

      IF sy-subrc IS INITIAL AND vl_reftyp IS NOT INITIAL.

        "vbrp_vbeln = vl_reftyp.
        vbrp_vbeln = vl_reftyp-refkey.

        SELECT SINGLE vgbel
          FROM vbrp
          INTO @DATA(vl_vgbel)
          WHERE vbeln EQ @vbrp_vbeln.

        IF sy-subrc IS INITIAL AND vl_vgbel IS NOT INITIAL.

          SELECT SINGLE vbeln
            FROM likp
            INTO @DATA(vl_vbeln)
            WHERE vbeln EQ @vl_vgbel.
          IF sy-subrc IS INITIAL AND vl_vbeln IS NOT INITIAL.

            SELECT SINGLE *
              FROM zsdt0001
              INTO @DATA(w_zsdt0001)
             WHERE doc_rem = @vl_vbeln
               AND tp_movimento = 'E'.

            IF sy-subrc IS INITIAL.
              l_subrc = 4.

              MESSAGE ID 'ZSIMETRYA'
              TYPE 'E'
              NUMBER 044
              WITH wa_nfe_alv-docnum
                   w_zsdt0001-nr_romaneio
                   w_zsdt0001-nr_safra
                   w_zsdt0001-branch.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF. "IR059834 Fim

      "Verificar se Remessa formação Lote já foi vinculada à um retorno.
      SELECT SINGLE *
        FROM zsdt_retlote INTO @DATA(wl_zsdt_retlote)
       WHERE docnum EQ @wa_nfe_alv-docnum.

      IF sy-subrc = 0.
        l_subrc = 4.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 039 WITH wl_zsdt_retlote-docnum_ret.
      ENDIF.

      TRY.
          zcl_boletim_producao=>zif_boletim_producao~get_instance(
          )->check_permissao_modificacao( i_docnum = CONV #( wa_nfe_alv-docnum ) ).
        CATCH zcx_boletim_producao INTO DATA(zcx_bol_prod).
          l_subrc = 4.
          zcx_bol_prod->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
          RETURN.
      ENDTRY.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      DATA: var_answer TYPE c.

      DATA(lva_chave_nfe_check) = zcl_util_sd=>get_chave_docnum_fiscal( EXPORTING i_docnum = wa_nfe_alv-docnum ).
      IF lva_chave_nfe_check IS NOT INITIAL.

        DATA(lwa_romaneio_insumos) = zcl_les_utils=>get_romaneio_documento_fiscal( EXPORTING i_docnum = wa_nfe_alv-docnum  ).

        SELECT SINGLE *
          FROM zsdt0375 INTO @DATA(lwa_zsdt0375)
         WHERE chave_nfe EQ @lva_chave_nfe_check.

        IF sy-subrc EQ 0.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmação'
              text_question         = 'NF-e emitida para embarque no CD LUFT, já está em trânsito! Deseja realmente Cancelar?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              default_button        = '1'
              display_cancel_button = ''
            IMPORTING
              answer                = var_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF var_answer NE '1'.
            l_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_romaneio_insumos IS INITIAL.
            l_subrc = 4.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Romaneio NF-e não encontrado!'.
            RETURN.
          ENDIF.

          IF lwa_romaneio_insumos-id_interface = '48' AND lwa_romaneio_insumos-nro_cg IS NOT INITIAL.

            DATA(lva_msg_error) =
             zcl_carga_saida_insumos=>check_permissao_carga( EXPORTING
                i_nro_carga = lwa_romaneio_insumos-nro_cg
                i_atividade = '14' ).

            IF lva_msg_error IS NOT INITIAL.
              l_subrc = 4.
              MESSAGE lva_msg_error TYPE 'E'.
              RETURN.
            ENDIF.

          ELSE.
            l_subrc = 4.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Operação não permitida!'.
            RETURN.
          ENDIF.


        ENDIF.

        IF lwa_romaneio_insumos-id_interface = '48' AND lwa_romaneio_insumos-nro_cg IS NOT INITIAL.

          DATA(lva_emb_luft) =
             zcl_carga_saida_insumos=>get_embarque_luft( EXPORTING
                i_nro_cg = lwa_romaneio_insumos-nro_cg ).

          IF lva_emb_luft EQ abap_true.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = 'NF-e emitida para embarque e integração com CD LUFT! CD LUFT precisa cancelar a mesma primeiro! Confirma que o Cancelamento já foi feito pela LUFT?'
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF var_answer NE '1'.
              l_subrc = 4.
              RETURN.
            ENDIF.

            lva_msg_error =
             zcl_carga_saida_insumos=>check_permissao_carga( EXPORTING
                i_nro_carga = lwa_romaneio_insumos-nro_cg
                i_atividade = '13' ).

            IF lva_msg_error IS NOT INITIAL.
              l_subrc = 4.
              MESSAGE lva_msg_error TYPE 'E'.
              RETURN.
            ENDIF.


          ENDIF.

        ENDIF.

      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----



      "Verificar se Retorno formação Lote já foi vinculada à O.V Exportação.
      SELECT SINGLE *
        FROM zsdt_export INTO @DATA(wl_zsdt_export)
       WHERE docnum EQ @wa_nfe_alv-docnum
         AND ordem  NE ''.

      IF sy-subrc = 0.
        l_subrc = 4.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 040 WITH wl_zsdt_export-ordem.
      ENDIF.

    ENDLOOP.



  ENDFORM.

  FORM verifica_valor_registro  USING p_docnum TYPE j_1bnfdoc-docnum
                             CHANGING p_subrc.

    DATA: tg_lin         TYPE TABLE OF j_1bnflin WITH HEADER LINE,
          tg_tax         TYPE j_1bnfstx OCCURS 10 WITH HEADER LINE,
          lo_cte_switch  TYPE REF TO cl_j_1bcte_swf,
          lx_cte_related TYPE abap_bool VALUE abap_false,
          v_netwr        TYPE j_1bnflin-netwr,
          v_netwr_lim    TYPE j_1bnflin-netwr,
          wa_lin_e       TYPE j_1bnflin,
          wa_lin_i       TYPE j_1binlin.


    CLEAR: v_netwr, p_subrc.

    CHECK p_docnum IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(wl_doc)
     WHERE docnum = @p_docnum.

    CHECK sy-subrc = 0.

    SELECT *
      FROM j_1bnflin INTO TABLE tg_lin
     WHERE docnum = p_docnum.

    CHECK tg_lin[] IS NOT INITIAL.

* Verifica se o documento está relacionado a um CT-e
    lo_cte_switch = cl_j_1bcte_swf=>get_instance( ).
    IF lo_cte_switch->is_cte_ctx_by_model( wl_doc-model )  = abap_true OR
       lo_cte_switch->is_cte_ctx_by_docnum( iv_docnum = wl_doc-docref ) = abap_true.
*   Documento é relacionado a CT-e  quando:
*     - Documento processado pertence ao modelo 57
*     - Documento processado não pertence a modelar 57 mas referes a CT-e. (Por exemplo, NF anulando uma CT-e)
      lx_cte_related = 'X'.
    ELSE.
      lx_cte_related = ' '.
    ENDIF.

    LOOP AT tg_lin.

      CLEAR: wa_lin_e, wa_lin_i, tg_tax[].

      MOVE-CORRESPONDING tg_lin TO wa_lin_e.

      SELECT *
        FROM j_1bnfstx INTO TABLE tg_tax
       WHERE docnum = tg_lin-docnum
         AND itmnum = tg_lin-itmnum.

      CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION_I'
        EXPORTING
          nf_item                 = wa_lin_e
          iv_nf_direction         = wl_doc-direct
          ix_posted_with_xml_data = wl_doc-autom_incoming
          ix_cte_related          = lx_cte_related
        IMPORTING
          ext_item                = wa_lin_i
        TABLES
          nf_item_tax             = tg_tax.

      IF wa_lin_i-nftot > 0 .
        ADD wa_lin_i-nftot TO v_netwr.
      ELSEIF wa_lin_i-nfnett > 0 .
        ADD wa_lin_i-nfnett TO v_netwr.
      ELSEIF wa_lin_i-netwrt > 0.
        ADD wa_lin_i-netwrt TO v_netwr.
      ENDIF.

    ENDLOOP.

* "// IR054082 inicio
    DATA limite_fiscal TYPE REF TO zcl_integracao_grc_new_nfe.
    CREATE OBJECT limite_fiscal.
    "Comentado bug : DEVK9A0VOM - SD-W-LP-BLOQUEIO CANCELAMENTO REMESSA BUG: 57677

    zcl_integracao_grc_new_nfe=>zif_integracao_grc_new_nfe~get_instance(
    )->set_limite_fiscal( i_model = wl_doc-model
    )->get_limite_fiscal( IMPORTING
        e_limite = v_netwr_lim ).

    IF v_netwr_lim IS NOT INITIAL.
* "// IR054082 Fim

      CHECK v_netwr_lim > 0 .

      IF v_netwr > v_netwr_lim.
        SELECT SINGLE *
          FROM zsdt0146 INTO @DATA(wl_0146)
         WHERE docnum = @p_docnum.
        IF sy-subrc NE 0.
          p_subrc = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_CIOT_CANCELADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SUBRC  text
*----------------------------------------------------------------------*
  FORM verifica_ciot_cancelado  USING l_subrc.

    DATA: it_cte_ciot   TYPE TABLE OF zcte_ciot.

    "Verificar Viagem CIOT
    CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
      EXPORTING
        p_cte_avulso = wa_nfe_alv-docnum
      TABLES
        it_cte_ciot  = it_cte_ciot
      EXCEPTIONS
        nao_ciot     = 1
        OTHERS       = 2.

    LOOP AT it_cte_ciot INTO DATA(wa_cte_ciot).

      CASE wa_cte_ciot-st_ciot.
        WHEN '0'.
          "L_SUBRC = 4.
          "MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT 'com estatus de "pendente"'.
        WHEN '1'.
          l_subrc = 4.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "enviado"'.
        WHEN '3'.
          "L_SUBRC = 4.
          "MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT 'com estatus de "Rejeitado"'.
        WHEN '4'.
          l_subrc = 4.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Nr. CIOT' wa_cte_ciot-nr_ciot 'com estatus de "creditado"'.
        WHEN '6'.
          l_subrc = 4.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "fechado (pago cockpit)"'.
        WHEN '7'.
          l_subrc = 4.
          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' wa_cte_ciot-cd_ciot 'com estatus de "Enviado Cancelamento"'.
        WHEN '8'.
          "L_SUBRC = 4.
          "MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT 'com estatus de "Enviado Cancelamento"'.
      ENDCASE.

    ENDLOOP.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENCERRAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM encerrar .

    DATA: zcl_mdfe TYPE REF TO zcl_mdfe.

* Check authorization
    IF gf_authorization_mdf_01 IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
    ENDIF.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    LOOP AT it_selected_rows INTO DATA(wa_documento).

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_documento-index.

      CHECK ( sy-subrc = 0 ) AND ( wa_nfe_alv-docnum IS NOT INITIAL ).

      IF NOT ( wa_nfe_alv-docnum IS INITIAL ) AND
         NOT ( wa_nfe_alv-nfnum9 IS INITIAL ).

        CLEAR: zcl_mdfe.

        CREATE OBJECT zcl_mdfe
          EXPORTING
            i_nmdfe  = wa_nfe_alv-nfnum9
            i_docnum = wa_nfe_alv-docnum.

        zcl_mdfe->encerrar_mdfe( i_solicita_motivo = abap_true ).
        CLEAR: zcl_mdfe.

        SELECT SINGLE * INTO wa_active_mod
          FROM j_1bnfe_active
         WHERE docnum EQ wa_nfe_alv-docnum.

        DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
        APPEND wa_active_mod TO it_active_mod.              "1090279
        PERFORM registra_envio_data USING wa_nfe_alv space.

        gf_docnum = wa_nfe_alv-docnum.

      ELSE.
        MESSAGE 'MDF-e não localizado para Encerramento!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ENDLOOP.

    PERFORM grid_update USING space.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_MDFE_REFERENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GF_DOCNUM  text
*----------------------------------------------------------------------*
  FORM fill_mdfe_reference  USING p_gf_docnum TYPE j_1bnfe_active-docnum.

    CLEAR: gt_mdfe[], gt_mdfe.

    CHECK sy-tcode EQ 'ZMDFE'.

    "MDF-e Documentos
    SELECT *
      FROM zsdt0105
      INTO TABLE @DATA(it_zsdt0105)
     WHERE docnum_ref EQ @p_gf_docnum.

    LOOP AT  it_zsdt0105 INTO DATA(wa_zsdt0105).

      CLEAR: gw_mdfe.

      gw_mdfe-status = icon_warning.
      gw_mdfe-nmdfe  = wa_zsdt0105-nmdfe.
      gw_mdfe-docnum = wa_zsdt0105-docnum.

      SELECT SINGLE *
        INTO @DATA(wa_j1bnfdoc)
        FROM j_1bnfdoc
       WHERE docnum EQ @wa_zsdt0105-docnum.

      gw_mdfe-bukrs  = wa_j1bnfdoc-bukrs.
      gw_mdfe-branch = wa_j1bnfdoc-branch.

      APPEND gw_mdfe TO gt_mdfe.
    ENDLOOP.

    CLEAR: wa_zsdt0105.

* Update ALV distplay
    CALL METHOD gc_alv_mdfe->refresh_table_display.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_MDFE_AVULSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM criar_mdfe_avulsa .

    ctl_alv_nfe->get_selected_rows(
      IMPORTING
        et_index_rows = DATA(et_index_rows)
    ).

    IF et_index_rows[] IS NOT INITIAL.

      READ TABLE et_index_rows INDEX 1 INTO DATA(wa_index_rows).

      READ TABLE it_nfe_alv INTO DATA(wa_selecionado) INDEX wa_index_rows-index.

      CALL FUNCTION 'Z_GRC_MDFE_AVULSA'
        EXPORTING
          i_docnum = wa_selecionado-docnum.

    ELSE.
      CALL FUNCTION 'Z_GRC_MDFE_AVULSA'.
    ENDIF.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM estornar .

    DATA: zcl_mdfe TYPE REF TO zcl_mdfe.

    CHECK sy-tcode EQ 'ZMDFE'.

* Check if a selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    LOOP AT it_selected_rows INTO DATA(wa_documento).

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_documento-index.

      CHECK ( sy-subrc = 0 ) AND ( wa_nfe_alv-docnum IS NOT INITIAL ).

      IF NOT ( wa_nfe_alv-docnum IS INITIAL ).

        CLEAR: zcl_mdfe.

        CREATE OBJECT zcl_mdfe
          EXPORTING
            i_docnum = wa_nfe_alv-docnum.

        zcl_mdfe->estornar_mdfe( ).
        CLEAR: zcl_mdfe.

        SELECT SINGLE * INTO wa_active_mod
          FROM j_1bnfe_active
         WHERE docnum EQ wa_nfe_alv-docnum.

        DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
        APPEND wa_active_mod TO it_active_mod.              "1090279
        PERFORM registra_envio_data USING wa_nfe_alv space.

        gf_docnum = wa_nfe_alv-docnum.

      ELSE.
        MESSAGE 'MDF-e não localizado para Encerramento!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ENDLOOP.

    PERFORM grid_update USING space.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COMPLEMENTA_ZLEST0194
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM complementa_zlest0194 .

    IF wa_nfe_alv-docnum IS NOT INITIAL.

      SELECT SINGLE *
        FROM zcte_identifica
       WHERE docnum = @wa_nfe_alv-docnum
        INTO @DATA(wa_zcte_identifica).

      IF wa_zcte_identifica-tpserv = '1'.

        SELECT SINGLE *
          FROM zlest0194
         WHERE docnum_sub = @wa_nfe_alv-docnum
          INTO @DATA(wa_zlest0194).

        IF sy-subrc IS INITIAL.
          wa_nfe_alv-nfnum9 = wa_active_mod-nfnum9.

          IF wa_nfe_alv-nfnum9 IS NOT INITIAL.

            SELECT SINGLE *
              FROM j_1bnfe_active
             WHERE docnum = @wa_nfe_alv-docnum
              INTO @DATA(wa_j_1bnfe_active).

            CONCATENATE wa_j_1bnfe_active-regio  wa_j_1bnfe_active-nfyear  wa_j_1bnfe_active-nfmonth
                        wa_j_1bnfe_active-stcd1  wa_j_1bnfe_active-model   wa_j_1bnfe_active-serie
                        wa_j_1bnfe_active-nfnum9 wa_j_1bnfe_active-docnum9 wa_j_1bnfe_active-cdv
                   INTO wa_zlest0194-chave_cte_sub.
            wa_zlest0194-nr_cte_sub    = wa_nfe_alv-nfnum9.
            wa_zlest0194-serie_cte_sub = wa_nfe_alv-serie.
            MODIFY zlest0194 FROM wa_zlest0194.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PERMISSAO_CANCEL_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SUBRC  text
*----------------------------------------------------------------------*
  FORM f_check_permissao_cancel_cte USING l_subrc.

    DATA: it_cte_ciot   TYPE TABLE OF zcte_ciot.

    LOOP AT it_selected_rows INTO wa_selected_rows.

      IF l_subrc IS NOT INITIAL.
        EXIT.
      ENDIF.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

*    SELECT SINGLE *
*      from j_1bnfdoc INTO @DATA(lwa_doc)
*     WHERE docnum EQ @wa_nfe_alv-docnum.
*
*    IF SY-SUBRC EQ 0 AND lwa_doc-docstat NE '1' AND lwa_doc-xmlvers >= 4.
*      l_subrc = 4.
*      MESSAGE |Ação não necessária! Prosseguir com as demais etapas do estorno! Docnum: { wa_nfe_alv-docnum }. | TYPE 'W'.
*      CONTINUE.
*    ENDIF.

      "Verificar Viagem CIOT
      CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
        EXPORTING
          p_cte_avulso = wa_nfe_alv-docnum
        TABLES
          it_cte_ciot  = it_cte_ciot
        EXCEPTIONS
          nao_ciot     = 1
          OTHERS       = 2.

      LOOP AT it_cte_ciot INTO DATA(wa_cte_ciot).

        SELECT *
            INTO TABLE @DATA(it_vfkp)
            FROM vfkp
           WHERE rebel EQ @wa_cte_ciot-tknum
             AND netwr NE 0       "Valor líquido em moeda do item custos de frete
             AND ebeln NE @space  "Nº do documento de compras
             AND lblni NE @space. "Nº folha registro de serviços

        CHECK sy-subrc IS INITIAL.

        LOOP AT it_vfkp INTO DATA(wa_vfkp).

          "MIRO
          SELECT SINGLE rbkp~belnr INTO @DATA(vl_belnr)
            FROM rseg
           INNER JOIN rbkp ON rbkp~belnr = rseg~belnr AND rbkp~gjahr = rseg~gjahr AND rbkp~stblg = ''
           WHERE rseg~ebeln EQ @wa_vfkp-ebeln
             AND rseg~lfbnr EQ @wa_vfkp-lblni.

          IF sy-subrc IS INITIAL.
            l_subrc = 4.
*          MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' WA_CTE_CIOT-CD_CIOT 'com estatus de "fechado (pago cockpit)"'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Custo de Frete possui MIRO referenciada: ' vl_belnr '-' wa_nfe_alv-docnum.
*          MESSAGE I836(SD) WITH |Custo de Frete possui MIRO referenciada: { VL_BELNR } - { WA_NFE_ALV-DOCNUM }|.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDFORM.

  FORM f_sinc_documento_sap_ecc  USING p_docnum     TYPE j_1bnfdoc-docnum
                                       p_docnum_ecc TYPE j_1bnfdoc-docnum
                              CHANGING p_subrc.

    DATA: var_answer TYPE c.
    DATA: it_urllist TYPE tihttpurls2.
    DATA: lwa_user_fat        TYPE tvarvc,
          lwa_romaneio        TYPE zsdt0001,
          lwa_zlest0108       TYPE zlest0108,
          lwa_zib_nfe         TYPE zib_nfe,
          lwa_zlest0143       TYPE zlest0143,
          lwa_faturamento_ecc TYPE zde_compare_faturamento.


    CALL FUNCTION 'HTTP_GET_URL2'
      EXPORTING
        handlerclass     = 'ZCL_FMCALL_DOC_FISCAL'
      IMPORTING
        urllist          = it_urllist
      EXCEPTIONS
        http_not_enabled = 1
        OTHERS           = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Erro ao obter ao URL' TYPE 'E'.
      RETURN.
    ENDIF.

    READ TABLE it_urllist WITH KEY protocol = 'http' INTO DATA(wa_urllist).

    DATA(wa_dominio) = wa_urllist-protocol && '://' && wa_urllist-host && ':' && wa_urllist-port && wa_urllist-url.

    CLEAR: lwa_user_fat, lwa_romaneio, lwa_zlest0143, lwa_zlest0108.

    SELECT SINGLE *
      FROM tvarvc INTO lwa_user_fat
     WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
       AND low  = sy-uname.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(lwa_doc)
     WHERE docnum EQ @p_docnum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(lwa_lin)
     WHERE docnum EQ @p_docnum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(lwa_active)
     WHERE docnum EQ @p_docnum.

    CHECK sy-subrc EQ 0.

    CASE lwa_doc-model.
      WHEN '55'.

        IF p_docnum_ecc IS NOT INITIAL.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0007'
            EXPORTING
              i_docnum                = p_docnum_ecc
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

        ELSE.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
            EXPORTING
              i_docnum   = p_docnum
            IMPORTING
              e_zsdt0001 = lwa_romaneio.

          IF lwa_romaneio IS INITIAL.

            IF lwa_user_fat IS NOT INITIAL.
              MESSAGE 'Romaneio da NF-e não encontrado' TYPE 'E'.
            ENDIF.

            EXIT.

          ENDIF.

          CHECK lwa_romaneio-fat_contingencia_ecc EQ abap_true.

          p_subrc = 4.

          IF ( lwa_user_fat IS INITIAL ).
            MESSAGE 'Usuario não autorizado a gerar nota de romaneio de faturado no SAP ECC' TYPE 'E'.
          ENDIF.

          IF lwa_doc-nfenum IS NOT INITIAL.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = 'Numeração documento já definida! Deseja sincronizar novamente com o ECC?'
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            CHECK var_answer EQ '1'.

          ENDIF.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
            EXPORTING
              i_ch_referencia         = lwa_romaneio-ch_referencia
              i_get_dados_fat_ecc     = abap_true
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

          CASE lwa_lin-reftyp.
            WHEN 'MD'.

              IF lwa_romaneio-doc_material IS NOT INITIAL.  "Processo Remessa armazenagem
                lwa_faturamento_ecc-data_lcto_nf   = lwa_faturamento_ecc-data_lcto_nf_arm.
                lwa_faturamento_ecc-chave_nfe      = lwa_faturamento_ecc-chave_nfe_arm.
                lwa_faturamento_ecc-authcode_nfe   = lwa_faturamento_ecc-authcode_nfe_arm.
              ENDIF.

            WHEN 'ZW'.

              lwa_faturamento_ecc-data_lcto_nf   = lwa_faturamento_ecc-data_lcto_nf_rem.
              lwa_faturamento_ecc-chave_nfe      = lwa_faturamento_ecc-chave_nfe_rem.
              lwa_faturamento_ecc-authcode_nfe   = lwa_faturamento_ecc-authcode_nfe_rem.

          ENDCASE.

        ENDIF.

        IF strlen( lwa_faturamento_ecc-chave_nfe ) EQ 44.

          IF lwa_doc-docdat <> lwa_faturamento_ecc-data_lcto_nf.
            MESSAGE 'Data Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_doc-model <> lwa_faturamento_ecc-chave_nfe+20(2).
            MESSAGE 'Modelo Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_active-serie <> lwa_faturamento_ecc-chave_nfe+22(3).
            MESSAGE 'Serie Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_active-stcd1 <> lwa_faturamento_ecc-chave_nfe+6(14).
            MESSAGE 'CNPJ Emissor Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          lwa_doc-docstat = '1'.
          lwa_doc-nfenum  = lwa_faturamento_ecc-chave_nfe+25(9).

          MODIFY j_1bnfdoc FROM lwa_doc.

          lwa_active-nfnum9      = lwa_faturamento_ecc-chave_nfe+25(9).
          lwa_active-tpemis      = lwa_faturamento_ecc-chave_nfe+34(1).
          lwa_active-docnum9     = lwa_faturamento_ecc-chave_nfe+34(9).
          lwa_active-cdv         = lwa_faturamento_ecc-chave_nfe+43(1).
          lwa_active-docsta      = '1'.
          lwa_active-scssta      = '0'.
          lwa_active-action_requ = 'C'.
          lwa_active-msstat      = 'A'.
          lwa_active-code        = '100'.

          MODIFY j_1bnfe_active FROM lwa_active.

          CLEAR: lwa_zib_nfe.

          lwa_zib_nfe-docnum       = p_docnum.
          lwa_zib_nfe-authcod      = lwa_faturamento_ecc-authcode_nfe.
          lwa_zib_nfe-code         = '100'.
          lwa_zib_nfe-docnum9      = lwa_active-docnum9.
          lwa_zib_nfe-cdv          = lwa_active-cdv.
          lwa_zib_nfe-date_aut_1   = lwa_doc-docdat.
          lwa_zib_nfe-ds_url_danfe = wa_dominio && '/getnfepdf?sap-client=300&i_docnum=' && p_docnum.

          MODIFY zib_nfe FROM lwa_zib_nfe.

          IF lwa_romaneio-doc_material IS NOT INITIAL.  "Processo Remessa armazenagem
            CASE lwa_lin-reftyp.
              WHEN 'MD'.
                SELECT SINGLE *
                  FROM mkpf INTO @DATA(lwa_mkpf)
                 WHERE mblnr EQ @lwa_lin-refkey(10).

                IF sy-subrc EQ 0.
                  DATA: lit_mkpf TYPE TABLE OF mkpf.
                  DATA: lit_mseg TYPE TABLE OF mseg.

                  CLEAR: lit_mkpf[], lit_mseg[].

                  lwa_mkpf-xblnr = lwa_active-nfnum9 && '-' && lwa_active-serie.

                  APPEND lwa_mkpf TO lit_mkpf.

                  SELECT *
                     FROM mseg INTO TABLE lit_mseg
                    WHERE mblnr EQ lwa_mkpf-mblnr.

                  CALL FUNCTION 'MB_CHANGE_DOCUMENT'
                    TABLES
                      zmkpf = lit_mkpf
                      zmseg = lit_mseg.

                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = 'X'.
                ENDIF.
            ENDCASE.
          ENDIF.


          "Dados Averbação
          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0004'
            EXPORTING
              i_docnum    = p_docnum
            IMPORTING
              e_zlest0143 = lwa_zlest0143.

          SELECT SINGLE *
            FROM zlest0143 INTO @DATA(zlest0143_exists)
           WHERE docnum = @p_docnum.

          IF sy-subrc NE 0 AND lwa_zlest0143 IS NOT INITIAL.
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZLESAVSEG'
              IMPORTING
                number                  = lwa_zlest0143-cd_averbacao
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.

            lwa_zlest0143-docnum = p_docnum.
            MODIFY zlest0143 FROM lwa_zlest0143.
          ENDIF.

          MESSAGE 'Dados de Autorização Sincronizados com SAP ECC!' TYPE 'I'.
        ELSE.
          MESSAGE 'NF-e não foi autorizada no SAP ECC' TYPE 'I'.
        ENDIF.

        p_subrc = 4.

      WHEN '57'.

        DATA: lwa_zcte_ciot TYPE zcte_ciot,
              lva_modal     TYPE c LENGTH 2.

        CLEAR: lwa_zcte_ciot, lva_modal.

        IF p_docnum_ecc IS NOT INITIAL.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0007'
            EXPORTING
              i_docnum                = p_docnum_ecc
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

          SELECT SINGLE *
            FROM zcte_ciot INTO lwa_zcte_ciot
           WHERE docnum EQ p_docnum.

        ELSE.

          DATA(_contingencia) = abap_false.
          DATA(_achou_processo) = abap_false.


          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
            EXPORTING
              i_docnum   = p_docnum
            IMPORTING
              e_zsdt0001 = lwa_romaneio.

          SELECT SINGLE *
            FROM zcte_ciot INTO lwa_zcte_ciot
           WHERE docnum EQ p_docnum.

          IF sy-subrc EQ 0. "CTE Rodoviario
            lva_modal =  '01'.
          ENDIF.

          IF lwa_romaneio IS NOT INITIAL.
            _achou_processo = abap_true.
            _contingencia = lwa_romaneio-fat_contingencia_ecc.
          ELSEIF lwa_zcte_ciot-tknum IS NOT INITIAL.
            SELECT SINGLE *
              FROM vttp INTO @DATA(lwa_vttp)
             WHERE tknum EQ @lwa_zcte_ciot-tknum.

            IF sy-subrc EQ 0.
              SELECT SINGLE *
                FROM zlest0108 INTO @DATA(lwa_zlest0108_tmp)
               WHERE vbeln EQ @lwa_vttp-vbeln.

              IF sy-subrc EQ 0.
                _achou_processo = abap_true.
                _contingencia   = lwa_zlest0108_tmp-fat_contingencia_ecc.
                lwa_zlest0108   = lwa_zlest0108_tmp.
              ENDIF.
            ENDIF.
          ENDIF.

          IF _achou_processo EQ abap_false AND lva_modal = '01'.

            IF lwa_user_fat IS NOT INITIAL.
              MESSAGE 'Romaneio da CT-e não encontrado' TYPE 'E'.
            ENDIF.

            EXIT.

          ENDIF.

          CHECK _contingencia EQ abap_true.

          p_subrc = 4.

          IF ( lwa_user_fat IS INITIAL ).
            MESSAGE 'Usuario não autorizado a gerar nota de romaneio de faturado no SAP ECC' TYPE 'E'.
          ENDIF.

          IF lwa_doc-nfenum IS NOT INITIAL.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = 'Numeração documento já definida! Deseja sincronizar novamente com o ECC?'
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            CHECK var_answer EQ '1'.

          ENDIF.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
            EXPORTING
              i_ch_referencia         = lwa_romaneio-ch_referencia
              i_vbeln                 = lwa_zlest0108-vbeln
              i_get_dados_fat_ecc     = abap_true
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

        ENDIF.

        IF strlen( lwa_faturamento_ecc-chave_cte ) EQ 44.

          IF lwa_doc-docdat <> lwa_faturamento_ecc-data_lcto_cte.
            MESSAGE 'Data Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_doc-model <> lwa_faturamento_ecc-chave_cte+20(2).
            MESSAGE 'Modelo Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_active-serie <> lwa_faturamento_ecc-chave_cte+22(3).
            MESSAGE 'Serie Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_active-stcd1 <> lwa_faturamento_ecc-chave_cte+6(14).
            MESSAGE 'CNPJ Emissor Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.


          lwa_doc-docstat = '1'.
          lwa_doc-nfenum  = lwa_faturamento_ecc-chave_cte+25(9).

          MODIFY j_1bnfdoc FROM lwa_doc.

          lwa_active-nfnum9      = lwa_faturamento_ecc-chave_cte+25(9).
          lwa_active-tpemis      = lwa_faturamento_ecc-chave_cte+34(1).
          lwa_active-docnum9     = lwa_faturamento_ecc-chave_cte+34(9).
          lwa_active-cdv         = lwa_faturamento_ecc-chave_cte+43(1).
          lwa_active-docsta      = '1'.
          lwa_active-scssta      = '0'.
          lwa_active-action_requ = 'C'.
          lwa_active-msstat      = 'A'.
          lwa_active-code        = '100'.

          MODIFY j_1bnfe_active FROM lwa_active.

          CLEAR: lwa_zib_nfe.

          lwa_zib_nfe-docnum       = p_docnum.
          lwa_zib_nfe-authcod      = lwa_faturamento_ecc-authcode_cte.
          lwa_zib_nfe-code         = '100'.
          lwa_zib_nfe-docnum9      = lwa_active-docnum9.
          lwa_zib_nfe-cdv          = lwa_active-cdv.
          lwa_zib_nfe-date_aut_1   = lwa_doc-docdat.
          lwa_zib_nfe-ds_url_danfe = wa_dominio && '/getctepdf?sap-client=300&i_docnum=' && p_docnum.

          MODIFY zib_nfe FROM lwa_zib_nfe.

          IF lwa_zcte_ciot IS NOT INITIAL AND lwa_faturamento_ecc-tip_nucontrato IS NOT INITIAL.

            lwa_zcte_ciot-link_contrato           = lwa_faturamento_ecc-tip_link_contrato.
            lwa_zcte_ciot-link_pedagio            = lwa_faturamento_ecc-tip_link_pedagio.
            lwa_zcte_ciot-link_resumo             = lwa_faturamento_ecc-tip_link_resumo.
            lwa_zcte_ciot-link_carga_pedagio      = lwa_faturamento_ecc-tip_link_carga_pedagio.
            lwa_zcte_ciot-nucontrato              = lwa_faturamento_ecc-tip_nucontrato.
            lwa_zcte_ciot-st_ciot                 = lwa_faturamento_ecc-tip_st_ciot.
            lwa_zcte_ciot-nr_ciot                 = lwa_faturamento_ecc-tip_nr_ciot.

            MODIFY zcte_ciot FROM lwa_zcte_ciot.

          ENDIF.

          "Dados Averbação
          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0004'
            EXPORTING
              i_docnum    = p_docnum
            IMPORTING
              e_zlest0143 = lwa_zlest0143.

          SELECT SINGLE *
            FROM zlest0143 INTO zlest0143_exists
           WHERE docnum = p_docnum.

          IF sy-subrc NE 0 AND lwa_zlest0143 IS NOT INITIAL.
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZLESAVSEG'
              IMPORTING
                number                  = lwa_zlest0143-cd_averbacao
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.

            lwa_zlest0143-docnum = p_docnum.
            MODIFY zlest0143 FROM lwa_zlest0143.
          ENDIF.

          MESSAGE 'Dados de Autorização Sincronizados com SAP ECC!' TYPE 'I'.
        ELSE.
          MESSAGE 'CT-e não foi autorizado no SAP ECC' TYPE 'I'.
        ENDIF.

        p_subrc = 4.

      WHEN '58'.

        DATA: lwa_zsdt0102  TYPE zsdt0102.
        DATA: lit_zsdt0105  TYPE TABLE OF zsdt0105.

        CLEAR: lwa_zsdt0102, lit_zsdt0105[], lwa_zcte_ciot.

        SELECT SINGLE *
          FROM zsdt0102 INTO lwa_zsdt0102
         WHERE docnum EQ p_docnum.

        IF sy-subrc NE 0.
          p_subrc = 4.
          MESSAGE 'MDF-e não encontrado' TYPE 'E'.
          RETURN.
        ENDIF.

        IF p_docnum_ecc IS NOT INITIAL.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0007'
            EXPORTING
              i_docnum                = p_docnum_ecc
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

        ELSE.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
            EXPORTING
              i_docnum   = p_docnum
            IMPORTING
              e_zsdt0001 = lwa_romaneio.

          IF lwa_romaneio IS NOT INITIAL.
            _achou_processo = abap_true.
            _contingencia = lwa_romaneio-fat_contingencia_ecc.
          ELSE.

            SELECT SINGLE *
              FROM zsdt0105 INTO @DATA(lwa_zsdt0105)
             WHERE docnum_ref EQ @p_docnum.

            IF sy-subrc EQ 0 AND lwa_zsdt0105-docnum IS NOT INITIAL.

              SELECT SINGLE *
                FROM zcte_ciot INTO lwa_zcte_ciot
               WHERE docnum EQ lwa_zsdt0105-docnum.

              IF sy-subrc EQ 0.
                SELECT SINGLE *
                  FROM vttp INTO lwa_vttp
                 WHERE tknum EQ lwa_zcte_ciot-tknum.

                IF sy-subrc EQ 0.
                  SELECT SINGLE *
                    FROM zlest0108 INTO lwa_zlest0108_tmp
                   WHERE vbeln EQ lwa_vttp-vbeln.

                  IF sy-subrc EQ 0.
                    _achou_processo = abap_true.
                    _contingencia   = lwa_zlest0108_tmp-fat_contingencia_ecc.
                    lwa_zlest0108   = lwa_zlest0108_tmp.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          IF _achou_processo EQ abap_false.

            IF lwa_user_fat IS NOT INITIAL.
              MESSAGE 'Romaneio da CT-e não encontrado' TYPE 'E'.
            ENDIF.

            EXIT.

          ENDIF.

          CHECK _contingencia EQ abap_true.

          p_subrc = 4.

          IF ( lwa_user_fat IS INITIAL ).
            MESSAGE 'Usuario não autorizado a gerar nota de romaneio de faturado no SAP ECC' TYPE 'E'.
          ENDIF.

          IF lwa_doc-nfenum IS NOT INITIAL.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = 'Numeração documento já definida! Deseja sincronizar novamente com o ECC?'
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            CHECK var_answer EQ '1'.

          ENDIF.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
            EXPORTING
              i_ch_referencia         = lwa_romaneio-ch_referencia
              i_vbeln                 = lwa_zlest0108-vbeln
              i_get_dados_fat_ecc     = abap_true
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

        ENDIF.

        IF strlen( lwa_faturamento_ecc-chave_mdfe ) EQ 44.

          IF lwa_doc-docdat <> lwa_faturamento_ecc-data_lcto_mdfe.
            MESSAGE 'Data Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_doc-model <> lwa_faturamento_ecc-chave_mdfe+20(2).
            MESSAGE 'Modelo Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_active-serie <> lwa_faturamento_ecc-chave_mdfe+22(3).
            MESSAGE 'Serie Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.

          IF lwa_active-stcd1 <> lwa_faturamento_ecc-chave_mdfe+6(14).
            MESSAGE 'CNPJ Emissor Documento diferente do ECC!' TYPE 'I'.
            p_subrc = 4.
            RETURN.
          ENDIF.


          lwa_doc-docstat = '1'.
          lwa_doc-nfenum  = lwa_faturamento_ecc-chave_mdfe+25(9).

          MODIFY j_1bnfdoc FROM lwa_doc.

          lwa_active-nfnum9      = lwa_faturamento_ecc-chave_mdfe+25(9).
          lwa_active-tpemis      = lwa_faturamento_ecc-chave_mdfe+34(1).
          lwa_active-docnum9     = lwa_faturamento_ecc-chave_mdfe+34(9).
          lwa_active-cdv         = lwa_faturamento_ecc-chave_mdfe+43(1).
          lwa_active-docsta      = '1'.
          lwa_active-scssta      = '0'.
          lwa_active-action_requ = 'C'.
          lwa_active-msstat      = 'A'.
          lwa_active-code        = '100'.

          MODIFY j_1bnfe_active FROM lwa_active.

          CLEAR: lwa_zib_nfe.

          lwa_zib_nfe-docnum       = p_docnum.
          lwa_zib_nfe-authcod      = lwa_faturamento_ecc-authcode_mdfe.
          lwa_zib_nfe-code         = '100'.
          lwa_zib_nfe-docnum9      = lwa_active-docnum9.
          lwa_zib_nfe-cdv          = lwa_active-cdv.
          lwa_zib_nfe-date_aut_1   = lwa_doc-docdat.
          lwa_zib_nfe-ds_url_danfe = wa_dominio && '/getmdfepdf?sap-client=300&i_docnum=' && p_docnum.

          MODIFY zib_nfe FROM lwa_zib_nfe.

          lwa_zsdt0102-autorizado  = abap_true.
          lwa_zsdt0102-encerrado   = lwa_faturamento_ecc-mdfe_encerrado.
          lwa_zsdt0102-url_sefaz   = lwa_zib_nfe-ds_url_danfe.
          lwa_zsdt0102-nmdfe       = lwa_active-nfnum9.
          lwa_zsdt0102-docnum9     = lwa_active-docnum9.
          lwa_zsdt0102-code        = lwa_active-code.
          lwa_zsdt0102-cdv         = lwa_active-cdv.
          lwa_zsdt0102-serie       = lwa_active-serie.

          UPDATE zsdt0102
             SET nmdfe = lwa_zsdt0102-nmdfe
                 serie = lwa_zsdt0102-serie
           WHERE docnum EQ p_docnum.

          UPDATE zsdt0105
             SET nmdfe = lwa_zsdt0102-nmdfe
           WHERE docnum_ref EQ p_docnum.

          MODIFY zsdt0102 FROM lwa_zsdt0102.


          MESSAGE 'Dados de Autorização Sincronizados com SAP ECC!' TYPE 'I'.
        ELSE.
          MESSAGE 'MDF-e não foi autorizada no SAP ECC' TYPE 'I'.
        ENDIF.

        p_subrc = 4.

        ""UNLOCK SM12
        DATA : it_enque TYPE STANDARD TABLE OF seqg3.
        DATA: lv_arq_block TYPE seqg3-garg.

        lv_arq_block = sy-mandt + p_docnum.


        CALL FUNCTION 'ENQUE_READ'
          EXPORTING
            gclient = sy-mandt
*           GNAME   = ' '
            garg    = lv_arq_block
*           GUNAME  = SY-UNAME
*       IMPORTING
*           NUMBER  =
*           SUBRC   =
          TABLES
            enq     = it_enque.

        IF it_enque IS NOT INITIAL.

          CALL FUNCTION 'ENQUE_DELETE'
            TABLES
              enq = it_enque.
          CLEAR: it_enque, it_enque[].

        ENDIF.

*      <<<


    ENDCASE.


  ENDFORM.

  FORM f_sinc_docnum_ecc .

    DATA: lva_docnum_ecc TYPE j_1bdocnum.
    DATA: lt_fields  TYPE ty_sval.
    DATA: ls_fields  LIKE LINE OF lt_fields.

    DATA: lv_return   TYPE c.

    IF lines( it_selected_rows[] ) <> 1.
      MESSAGE 'Selecione apenas uma linha' TYPE 'I'.
      RETURN.
    ENDIF.

    CLEAR: lt_fields[].

    CLEAR ls_fields.
    ls_fields-tabname   = 'J_1BNFDOC'.
    ls_fields-fieldtext = 'Documento ECC'.
    ls_fields-field_obl = abap_false.
    ls_fields-fieldname = 'DOCNUM'.
    APPEND ls_fields TO lt_fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Histórico de Alterações'
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    READ TABLE lt_fields INTO DATA(lwa_field_docnum) WITH KEY fieldname = 'DOCNUM'.

    lva_docnum_ecc = lwa_field_docnum-value.

    IF lva_docnum_ecc IS INITIAL.
      MESSAGE 'Nenhum documento foi informado!' TYPE 'I'.
      RETURN.
    ENDIF.

* Send NF-e that was posted under contingency
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.

    CHECK sy-subrc EQ 0.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
    CHECK sy-subrc EQ 0 AND wa_nfe_alv-docnum IS NOT INITIAL.

    DATA(_sy_subrc) = 0.
    PERFORM f_sinc_documento_sap_ecc USING wa_nfe_alv-docnum lva_docnum_ecc CHANGING _sy_subrc.


  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ck_geracao_vt_vi_frete_cpt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_NFE_ALV_DOCNUM
*&      <-- SUBRC
*&---------------------------------------------------------------------*
  FORM f_ck_geracao_vt_vi_frete_cpt USING p_docnum     TYPE j_1bnfdoc-docnum
                                 CHANGING p_subrc.

    DATA: lwa_romaneio TYPE zsdt0001,
          lc_cockpit   TYPE zcockpit,                          "*-CS2024000086-26.09.2024-#151423-JT-inicio
          lc_erro      TYPE char01.                            "*-CS2024000086-26.09.2024-#151423-JT-inicio

*-CS2024000086-26.09.2024-#151423-JT-inicio
*    SELECT SINGLE *
*     FROM tvarvc INTO @DATA(tvarvc_bloq_frete_vt_vi)
*    WHERE name = 'ZNFE_NO_BLOCK_FRETE_CPT'.
*
*    CHECK sy-subrc NE 0.
*-CS2024000086-26.09.2024-#151423-JT-inicio

    CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
      EXPORTING
        i_docnum   = p_docnum
      IMPORTING
        e_zsdt0001 = lwa_romaneio.

*-CS2024000086-26.09.2024-#151423-JT-inicio
*    CHECK lwa_romaneio IS NOT INITIAL AND lwa_romaneio-tp_movimento = 'S' AND lwa_romaneio-doc_rem IS NOT INITIAL.
*
*    CHECK lwa_romaneio-agente_frete IS NOT INITIAL.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lwa_romaneio-agente_frete
*      IMPORTING
*        output = lwa_romaneio-agente_frete.
*-CS2024000086-26.09.2024-#151423-JT-inicio


    TRY.
        DATA(lc_gerou_vt) =  zcl_faturamento=>zif_faturamento~get_instance( )->get_status_gerou_vt_vi( lwa_romaneio-ch_referencia ).

      CATCH zcx_error INTO DATA(_zcx_error).
        p_subrc = c_x.
        MESSAGE ID _zcx_error->msgid TYPE 'I' NUMBER _zcx_error->msgno WITH _zcx_error->msgv1 _zcx_error->msgv2 _zcx_error->msgv3 _zcx_error->msgv4.
        RETURN.
    ENDTRY.
*-CS2024000086-26.09.2024-#151423-JT-fim

*-CS2024000086-26.09.2024-#151423-JT-inicio-COMENTADO
*    SELECT SINGLE *
*      FROM lfa1 INTO @DATA(lwa_lfa1_check)
*     WHERE lifnr = @lwa_romaneio-agente_frete.
*
*    CHECK sy-subrc EQ 0.
*
*    CHECK lwa_lfa1_check-ktokk NE 'ZFIC' OR ( lwa_lfa1_check-ktokk = 'ZFIC' AND lc_gera_transp = abap_true ). "*-CS2024000086-26.09.2024-#151423-JT-inicio
*
*    SELECT SINGLE *
*      FROM likp INTO @DATA(lwa_likp)
*     WHERE vbeln = @lwa_romaneio-doc_rem.
*
*    CHECK ( sy-subrc EQ 0 AND lwa_likp-inco1 = 'CPT' ) OR lc_gera_transp = abap_true.
*
*    DATA(_rem_conta_ordem) = abap_false.
*    PERFORM f_check_rem_conta_ordem USING lwa_romaneio      CHANGING _rem_conta_ordem.
*
*    CHECK _rem_conta_ordem EQ abap_false.
*
**-CS2024000086-26.09.2024-#151423-JT-inicio
*    DATA(_gerou_vt_vi) = abap_false.
*
*    IF lwa_romaneio-fknum IS NOT INITIAL.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lwa_romaneio-fknum
*        IMPORTING
*          output = lwa_romaneio-fknum.
*
*      SELECT SINGLE *
*        FROM vfkp INTO @DATA(lwa_vfkp)
*       WHERE fknum = @lwa_romaneio-fknum
*         AND fkpty = 'Z001'.
*
*      IF sy-subrc EQ 0.
*
*        IF lwa_vfkp-netwr = 0.
*          p_subrc = c_x.
*          MESSAGE |Documento de Custo { lwa_romaneio-fknum } criado sem valor!| TYPE 'I'.
*          RETURN.
*        ELSE.
*          _gerou_vt_vi = abap_true.
*        ENDIF.
*      ENDIF.
*
*      IF _gerou_vt_vi = abap_false.
*        p_subrc = c_x.
*        MESSAGE |Documento de custo não gerado para o romaneio { lwa_romaneio-nr_romaneio }!| TYPE 'W'.
*      ENDIF.
*    ENDIF.
*-CS2024000086-26.09.2024-#151423-JT-fim-COMENTADO

  ENDFORM.

  FORM f_check_rem_conta_ordem USING p_zsdt0001 TYPE zsdt0001
                            CHANGING p_rem_conta_ordem.

    DATA: t_set    TYPE TABLE OF rgsb4,
          w_set    TYPE rgsb4,
          t_tvarvc TYPE TABLE OF tvarvc,
          w_tvarvc TYPE tvarvc.

    RANGES:
          r_cfop            FOR j_1bnflin-cfop,
          r_matkl           FOR j_1bnflin-matkl.

    FREE: r_cfop, r_matkl, t_set, p_rem_conta_ordem.

    CHECK p_zsdt0001-ch_referencia IS NOT INITIAL.

*---------------------------------
* ler set
*---------------------------------
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'MAGGI_CFOP_VENDA_IND'
      TABLES
        set_values    = t_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    LOOP AT t_set INTO w_set.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_cfop.
    ENDLOOP.

*---------------------------------
* ler TVARVset
*---------------------------------
    SELECT *
      FROM tvarvc
      INTO TABLE t_tvarvc
     WHERE name = 'MAGGI_GR_FERTILIZANTES'.

    LOOP AT t_tvarvc INTO w_tvarvc.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_matkl.
    ENDLOOP.

*---------------------------------
* ler OV
*---------------------------------
    SELECT matkl, j_1bcfop
      FROM vbap
      INTO @DATA(w_vbap)
        UP TO 1 ROWS
     WHERE vbeln = @p_zsdt0001-vbeln
       AND matnr = @p_zsdt0001-matnr.
    ENDSELECT.

    IF sy-subrc         = 0           AND
       w_vbap-j_1bcfop IN r_cfop[]    AND
       w_vbap-matkl    IN r_matkl[]   AND
       r_cfop[]        IS NOT INITIAL AND
       r_matkl[]       IS NOT INITIAL.
      p_rem_conta_ordem = abap_true.
    ENDIF.

  ENDFORM.

*-#133089-12.02.2024-JT-inicio
*******************************************************************
* SELECAO FATURAEMENTO AUTOMATICO
* CHAMADA PELA CLASSE ZCL_AUTOMATIZAR_FATURAMENTO
*******************************************************************
  FORM f_selecao_fat_autom USING p_ch_fat
                                 p_ch_ref
                                 p_docnum.

    FREE: it_selected_rows, it_nfe_alv, it_alv_selection, gt_uf_perc.

    vg_faturamento_autom    = abap_true.
    vg_ch_faturamento       = p_ch_fat.
    vg_ch_referencia        = p_ch_ref.
    gf_authorization_nfe_35 = abap_true.                    "FF #170994

*---UFs percurso
    SELECT *
      FROM zlest0243
      INTO TABLE @DATA(t_0243)
     WHERE ch_faturamento = @p_ch_fat
       AND cancelado      = @abap_false.

    SORT t_0243 BY nm_sequencia.

    LOOP AT t_0243    INTO DATA(w_0243).
      gw_uf_perc-uf      = w_0243-bland.
      APPEND gw_uf_perc TO gt_uf_perc.
    ENDLOOP.

    SELECT SINGLE *
      FROM j_1bnfe_active
      INTO @DATA(w_active)
     WHERE docnum = @p_docnum.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING w_active   TO wa_nfe_alv.
    PERFORM set_status_ciot CHANGING wa_nfe_alv.

    wa_selected_rows-index = 1.
    APPEND wa_selected_rows       TO it_selected_rows.
    APPEND wa_nfe_alv             TO it_nfe_alv.
    MOVE-CORRESPONDING wa_nfe_alv TO wa_alv_selection.
    APPEND wa_alv_selection       TO it_alv_selection.

  ENDFORM.
*******************************************************************
*******************************************************************
*-#133089-12.02.2024-JT-fim
**<<<------"188425 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form ZF_GUIA_AGROPECUARIA
*&---------------------------------------------------------------------*
*& Chamada do preenchimento da Guia Agropecuária
*&---------------------------------------------------------------------*
  FORM zf_guia_agropecuaria.

    DATA vl_callscr_110 TYPE c.

* Check if an NF-e selection was made
    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.

    ENDIF.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
* Validação da Guia Agropecuária.
      PERFORM zf_valida_guia_agropecuaria USING    wa_nfe_alv-docnum
                                          CHANGING eg_guia_agro
                                                   vl_callscr_110.
* verifica s chamada da tela 110.
      CHECK NOT vl_callscr_110 IS INITIAL.
      eg_guia_agro_ori = eg_guia_agro.
* Preenchimento da Guia Agropecuária
      CALL SCREEN '0110' STARTING AT 5 2.
      CLEAR: eg_guia_agro, eg_guia_agro_ori.

    ENDLOOP.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_VALIDA_GUIA_AGROPECUARIA
*&---------------------------------------------------------------------*
*& Validação da Guia Agropecuária
*&---------------------------------------------------------------------*
*&      --> UV_DOCNUM    Nº Documento da Nota Fiscal SAP
*&      <-- CE_GUIA_AGRO Estrutura de campos da Guia Agropecuária
*&      <-- CV_CALLSCR   Valida chamada da tela 110.
*&---------------------------------------------------------------------*
  FORM zf_valida_guia_agropecuaria  USING    uv_docnum    TYPE j_1bdocnum
                                    CHANGING ce_guia_agro LIKE eg_guia_agro
                                             cv_callscr.

* Busca o(s) item(ns) da Nota.
    SELECT * FROM j_1bnflin INTO TABLE @DATA(tl_nflin) WHERE docnum EQ @uv_docnum.
    CHECK sy-subrc IS INITIAL.
* Monta o Range do NCM.
    DATA(rl_ncm) = VALUE zde_prod_ncm_ranges_t( FOR el_nflin IN tl_nflin ( sign = 'I' option = 'EQ' low = el_nflin-nbm ) ).
* Verifica se algum NCM de algum item da Nota está parametrizado.
    SELECT * FROM zncmagrop INTO TABLE @DATA(tl_zncmagrop) WHERE ncm IN @rl_ncm.
* Monta o Range do NCM Parametrizado.
    rl_ncm = VALUE #( FOR el_zncmagrop IN tl_zncmagrop ( sign = 'I' option = 'EQ' low = el_zncmagrop-ncm ) ).
* Verifica se tem NCM parametrizado.
    CHECK NOT rl_ncm IS INITIAL.
* Elimina os itens da Nota que o NCM não está Parametrizado.
    DELETE tl_nflin WHERE nbm NOT IN rl_ncm.
* Verifica se há itens da Nota.
    CHECK NOT tl_nflin[] IS INITIAL.
* Monta o Range do CFOP
    DATA(rl_cfop) = VALUE zsdt_cfop_lin( FOR el_nflin IN tl_nflin ( sign = 'I' option = 'EQ' low = el_nflin-cfop ) ).
* Verifica se algum CFOP de algum item da Nota está parametrizado.
    SELECT * FROM zfopexagrop INTO TABLE @DATA(tl_zfopexagrop) WHERE cfop IN @rl_cfop.
* Monta o Range do CFOP Parametrizado como exceção.
    rl_cfop = VALUE #( FOR el_zfopexagrop IN tl_zfopexagrop ( sign = 'I' option = 'EQ' low = el_zfopexagrop-cfop ) ).
    CLEAR cv_callscr.
*    LOOP AT tl_nflin TRANSPORTING NO FIELDS WHERE cfop NOT IN rl_cfop.
    LOOP AT tl_nflin INTO DATA(el_nflin2).
      IF ( NOT rl_cfop[]      IS INITIAL   AND
           NOT el_nflin2-cfop IN rl_cfop ) OR
               rl_cfop[]      IS INITIAL.
        SELECT SINGLE docnum tpguia ufguia serieguia nguia
          FROM zagroguiatrans
          INTO ce_guia_agro
        WHERE docnum EQ wa_nfe_alv-docnum.

        cv_callscr = abap_on.
        EXIT.

      ENDIF.

    ENDLOOP.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_SALVA_GUIA_AGROPECUARIA
*&---------------------------------------------------------------------*
*& Salva dados da Guia de Agropecuária
*&---------------------------------------------------------------------*
*&      --> UE_GUIA_AGRO Estrutura de campos da Guia Agropecuária
*&---------------------------------------------------------------------*
  FORM zf_salva_guia_agropecuaria USING ue_guia_agro LIKE eg_guia_agro.

    TABLES: zagroguiatrans.

    DATA vl_msg_numguia_usada TYPE char128.

* Verifica se o Número da Guia Agropecuária já foi usada.
    PERFORM zf_valida_num_guia_agro_usada USING    ue_guia_agro
                                          CHANGING vl_msg_numguia_usada.

    IF vl_msg_numguia_usada IS INITIAL.
      zagroguiatrans = CORRESPONDING #( eg_guia_agro ).
      zagroguiatrans-us_registro = sy-uname.
      zagroguiatrans-dt_registro = sy-datlo.
      zagroguiatrans-hr_registro = sy-timlo.

      MODIFY zagroguiatrans.

      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        MESSAGE |Dados da Guia Agropecuária salvo com sucesso.| TYPE 'S'.

      ELSE.
        ROLLBACK WORK.
        MESSAGE |Erro ao Salvar os Dados da Guia Agropecuária.| TYPE 'S' DISPLAY LIKE 'E'.

      ENDIF.

    ELSE.
      MESSAGE vl_msg_numguia_usada TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE SCREEN.

    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_SRH_TP_GUIA_AGRO
*&---------------------------------------------------------------------*
*& Ajuda de pesquisa do Tipo da Guia Agropecuária
*&---------------------------------------------------------------------*
  FORM zf_srh_tp_guia_agro.

    TYPES: BEGIN OF ty_tpguia,
             tpguia TYPE j_1btpguia,
             descri TYPE bezei30,
           END   OF ty_tpguia.

    DATA: tl_rettab TYPE TABLE OF ddshretval,
          tl_fields TYPE TABLE OF dfies,
          tl_dyread TYPE TABLE OF dynpread,
          tl_tpguia TYPE TABLE OF ty_tpguia.

    DATA: el_dyread TYPE          dynpread.

    DATA: vl_retfield  TYPE fieldname,
          vl_dynprofld TYPE dynfnam,
          vl_valor     TYPE char3.

    tl_tpguia = VALUE #( ( tpguia = '01' descri = 'GTA - Guia de Trânsito Animal' )
                         ( tpguia = '02' descri = 'TTA - Termo de Trânsito Animal' )
                         ( tpguia = '03' descri = 'DTA - Documento de Transferência Animal' ) ).

    PERFORM get_fields_of_value_tab IN PROGRAM saplsdhi
                                        TABLES tl_tpguia
                                               tl_fields
                                      CHANGING vl_retfield.

    READ TABLE tl_fields INTO DATA(el_fields) INDEX 1.
    CLEAR: tl_rettab, vl_dynprofld.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = el_fields-fieldname
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = vl_dynprofld
        window_title    = el_fields-fieldtext
        value_org       = sy-abcde+18(1) "S
      TABLES
        value_tab       = tl_tpguia
        field_tab       = tl_fields
        return_tab      = tl_rettab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL.
* Verifica se a irformação seleciona da TI.
      CHECK NOT tl_rettab[] IS INITIAL.

      READ TABLE tl_rettab INTO DATA(el_rettab) INDEX 1.
      vl_retfield         = 'EG_GUIA_AGRO-TPGUIA'.
      eg_guia_agro-tpguia = vl_valor = el_rettab-fieldval.

      MOVE: vl_retfield TO el_dyread-fieldname,
            0           TO el_dyread-stepl,
            vl_valor    TO el_dyread-fieldvalue,
            abap_on     TO el_dyread-fieldinp.
      APPEND el_dyread  TO tl_dyread.
      CLEAR  el_dyread.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = sy-cprog
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = tl_dyread
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          undefind_error       = 7
          OTHERS               = 8.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.

    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_SRH_UF_GUIA_AGRO
*&---------------------------------------------------------------------*
*& Ajuda de pesquisa da UF da Guia Agropecuária
*&---------------------------------------------------------------------*
  FORM zf_srh_uf_guia_agro.

    DATA: tl_rettab TYPE TABLE OF ddshretval,
          tl_fields TYPE TABLE OF dfies,
          tl_dyread TYPE TABLE OF dynpread.

    DATA: el_dyread TYPE          dynpread.

    DATA: vl_retfield  TYPE fieldname,
          vl_dynprofld TYPE dynfnam,
          vl_valor     TYPE char3.

    SELECT bland, bezei
      FROM t005u
      INTO TABLE @DATA(tl_ufguia)
    WHERE spras EQ 'P'
      AND land1 EQ 'BR'.

    PERFORM get_fields_of_value_tab IN PROGRAM saplsdhi
                                        TABLES tl_ufguia
                                               tl_fields
                                      CHANGING vl_retfield.

    READ TABLE tl_fields INTO DATA(el_fields) INDEX 1.
    CLEAR: tl_rettab, vl_dynprofld.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = el_fields-fieldname
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = vl_dynprofld
        window_title    = el_fields-fieldtext
        value_org       = sy-abcde+18(1) "S
      TABLES
        value_tab       = tl_ufguia
        field_tab       = tl_fields
        return_tab      = tl_rettab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL.
* Verifica se a irformação seleciona da TI.
      CHECK NOT tl_rettab[] IS INITIAL.

      READ TABLE tl_rettab INTO DATA(el_rettab) INDEX 1.
      vl_retfield         = 'EG_GUIA_AGRO-UFGUIA'.
      eg_guia_agro-ufguia = vl_valor = el_rettab-fieldval.

      MOVE: vl_retfield TO el_dyread-fieldname,
            0           TO el_dyread-stepl,
            vl_valor    TO el_dyread-fieldvalue,
            abap_on     TO el_dyread-fieldinp.
      APPEND el_dyread  TO tl_dyread.
      CLEAR  el_dyread.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = sy-cprog
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = tl_dyread
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          undefind_error       = 7
          OTHERS               = 8.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.

    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_NFE_GUIA_AGRO_AUTOR
*&---------------------------------------------------------------------*
*& Verifica se a NF da Guia Agropecuária está autorizada
*&---------------------------------------------------------------------*
*&      <-> PT_GUIA_AGRO Estrutura de campos da Guia Agropecuária
*&      <-- CV_AUTOR     Retorna se a NF-e está Autorizada ou não
*&---------------------------------------------------------------------*
  FORM zf_nfe_guia_agro_autor   TABLES pt_guia_agro LIKE tg_guia_agro
                              CHANGING cv_autor     TYPE c.

    SELECT * FROM j_1bnfe_active
      INTO TABLE @DATA(tl_nfe_active)
      FOR ALL ENTRIES IN @pt_guia_agro
    WHERE docnum EQ @pt_guia_agro-docnum
      AND docsta EQ '1'
      AND code   EQ '100'.

    IF sy-subrc IS INITIAL.
      READ TABLE tl_nfe_active INTO DATA(el_nfe_active) INDEX 1.
      DELETE pt_guia_agro WHERE docnum NE el_nfe_active-docnum.
      cv_autor = abap_on.

    ELSE.
      cv_autor = abap_off.

    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_VALIDA_NUM_GUIA_AGRO_USADA
*&---------------------------------------------------------------------*
*& Verifica se o Número da Guia Agropecuária já foi usada
*&---------------------------------------------------------------------*
*&      --> UE_GUIA_AGRO Estrutura de campos da Guia Agropecuária
*&      <-- VL_MSG_NUMGUIA_USADA
*&---------------------------------------------------------------------*
  FORM zf_valida_num_guia_agro_usada USING    ue_guia_agro LIKE eg_guia_agro
                                     CHANGING cv_mesagem   TYPE char128.

    DATA: tl_guia_agro LIKE TABLE OF eg_guia_agro.

    SELECT docnum tpguia ufguia serieguia nguia
      FROM zagroguiatrans
      INTO TABLE tl_guia_agro
    WHERE docnum    NE ue_guia_agro-docnum
      AND ufguia    EQ ue_guia_agro-ufguia
      AND serieguia EQ ue_guia_agro-serieguia
      AND nguia     EQ ue_guia_agro-nguia.

    IF sy-subrc IS INITIAL.
* Verifica se a NF da Guia Agropecuária está autorizada.
      PERFORM zf_nfe_guia_agro_autor TABLES   tl_guia_agro
                                     CHANGING vl_autor.

      IF vl_autor IS INITIAL.
        CLEAR cv_mesagem.

      ELSE.
        READ TABLE tl_guia_agro INTO DATA(el_guia_agro) INDEX 1.
        DATA(vl_nguia)     = CONV string( |{ el_guia_agro-nguia     ALPHA = OUT }| ).
        DATA(vl_serieguia) = CONV string( |{ el_guia_agro-serieguia ALPHA = OUT }| ).
        DATA(vl_docnum)    = CONV string( |{ el_guia_agro-docnum    ALPHA = OUT }| ).
        CONDENSE vl_nguia     NO-GAPS.
        CONDENSE vl_serieguia NO-GAPS.
        CONDENSE vl_docnum    NO-GAPS.
        cv_mesagem = |Guia Agropecuária do Estado { el_guia_agro-ufguia } N° { vl_nguia } e Série { vl_serieguia } já utilizada na NF-e { vl_docnum }.|.

      ENDIF.

    ELSE.
      CLEAR cv_mesagem.

    ENDIF.

  ENDFORM.
**<<<------"188425 - NMS - FIM------>>>

*** Inicio - Rubenilson Pereira - 09.10.2025 #192341
*&---------------------------------------------------------------------*
*& Form f_insere_frete_seguro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_insere_frete_seguro .
    DATA: lt_fields         TYPE TABLE OF sval,
          lv_returncode     TYPE c,
          lv_icon_ok_push   TYPE icon-name,
          wa_doc_header     TYPE j_1bnfdoc,
          it_doc_partner    TYPE TABLE OF j_1bnfnad,
          it_doc_item       TYPE TABLE OF j_1bnflin,
          it_doc_item_tax   TYPE TABLE OF j_1bnfstx,
          it_doc_header_msg TYPE TABLE OF j_1bnfftx,
          it_doc_refer_msg  TYPE TABLE OF j_1bnfref,
          it_doc_ot_partner TYPE TABLE OF j_1bnfcpd,
          lv_soma           TYPE j_1bdylin-netfre,
          lv_answer         TYPE c,
          lv_return_code    TYPE c.

    IF it_selected_rows IS INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.
    ENDIF.

    READ TABLE it_selected_rows INDEX 2 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '009'.
    ELSE.
      READ TABLE it_selected_rows ASSIGNING FIELD-SYMBOL(<fs_selected_rows>) INDEX 1.
      IF sy-subrc IS INITIAL.
        READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX <fs_selected_rows>-index.
        IF sy-subrc IS INITIAL AND wa_nfe_alv-nfnum9 IS NOT INITIAL OR
           ( wa_nfe_alv-nfnum9 IS INITIAL AND wa_nfe_alv-cancel IS NOT INITIAL ).
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '199' WITH 'NF-e selecionada nao pode ser alterada'.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
    <fs_fields>-tabname   = 'J_1BDYLIN'.
    <fs_fields>-fieldname = 'DOCNUM'.
    <fs_fields>-value     = wa_nfe_alv-docnum.
    <fs_fields>-field_attr = '02'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-tabname   = 'J_1BDYLIN'.
    <fs_fields>-fieldname = 'NETFRE'.
    <fs_fields>-comp_tab  = 'J_1BNFDOC'.
    <fs_fields>-comp_field = 'WAERK'.
    <fs_fields>-field_attr = '01'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-tabname   = 'J_1BNFDOC'.
    <fs_fields>-fieldname = 'WAERK'.
    <fs_fields>-field_attr = '04'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-tabname   = 'J_1BDYLIN'.
    <fs_fields>-fieldname = 'NETINS'.
    <fs_fields>-comp_tab  = 'J_1BNFDOC'.
    <fs_fields>-comp_field = 'WAERK'.
    <fs_fields>-field_attr = '01'.

    lv_icon_ok_push       = icon_okay  .

    CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
      EXPORTING
        formname          = 'F_VALIDA_FRETE'
        programname       = sy-repid
        popup_title       = 'Informações de Frete e Seguro'
        ok_pushbuttontext = ''
        first_pushbutton  = 'ACEITAR'
      IMPORTING
        returncode        = lv_return_code
      TABLES
        fields            = lt_fields
      EXCEPTIONS
        error_in_fields   = 1
        OTHERS            = 2.
    IF sy-subrc = 0 AND lv_return_code <> 'A'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = 'Confirma a alteração?'
          text_button_1  = 'Sim'
          text_button_2  = 'Não'
        IMPORTING
          answer         = lv_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc = 0.
        IF lv_answer <> '1'.
          RETURN.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
        EXPORTING
          doc_number         = wa_nfe_alv-docnum
        IMPORTING
          doc_header         = wa_doc_header
        TABLES
          doc_partner        = it_doc_partner
          doc_item           = it_doc_item
          doc_item_tax       = it_doc_item_tax
          doc_header_msg     = it_doc_header_msg
          doc_refer_msg      = it_doc_refer_msg
          doc_ot_partner     = it_doc_ot_partner
        EXCEPTIONS
          document_not_found = 1
          docum_lock         = 2
          OTHERS             = 3.
      IF sy-subrc EQ 0.

        LOOP AT lt_fields ASSIGNING <fs_fields>.
          IF <fs_fields>-fieldname EQ 'DOCNUM' .
            CONTINUE.
          ENDIF.
          lv_soma = lv_soma + <fs_fields>-value.
        ENDLOOP.

        LOOP AT it_doc_item ASSIGNING FIELD-SYMBOL(<fs_doc_item>).

          <fs_doc_item>-netwr = <fs_doc_item>-netwr - lv_soma.
          <fs_doc_item>-netpr = <fs_doc_item>-netwr / <fs_doc_item>-menge.

          READ TABLE lt_fields ASSIGNING <fs_fields>
          WITH KEY fieldname = 'NETFRE'.
          IF sy-subrc IS INITIAL.
            <fs_doc_item>-netfre = <fs_fields>-value.
          ENDIF.

          READ TABLE lt_fields ASSIGNING <fs_fields>
          WITH KEY fieldname = 'NETINS'.
          IF sy-subrc IS INITIAL.
            <fs_doc_item>-netins = <fs_fields>-value.
          ENDIF.

          CALL FUNCTION 'J_1B_NF_DOCUMENT_UPDATE'
            EXPORTING
              doc_number            = wa_nfe_alv-docnum
              doc_header            = wa_doc_header
            TABLES
              doc_partner           = it_doc_partner
              doc_item              = it_doc_item
              doc_item_tax          = it_doc_item_tax
              doc_header_msg        = it_doc_header_msg
              doc_refer_msg         = it_doc_refer_msg
              doc_ot_partner        = it_doc_ot_partner
            EXCEPTIONS
              document_not_found    = 1
              update_problem        = 2
              doc_number_is_initial = 3
              OTHERS                = 4.
          IF sy-subrc IS INITIAL.
            MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '199' WITH 'Nota atualizada com sucesso!'.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDFORM.

  FORM f_valida_frete TABLES fields STRUCTURE sval
                       USING code
                    CHANGING error  STRUCTURE  svale
                      show_popup.

    DATA(lt_fields) = fields[].
    DELETE lt_fields WHERE value IS INITIAL OR fieldname = 'DOCNUM'.
    IF lt_fields IS INITIAL.
      error-msgid = 'J1B_NFE'.
      error-msgno = '199'.
      error-msgty = 'E'.
      error-msgv1 = 'Favor prencher um dos campos'.
      show_popup = 'X'.
    ENDIF.
  ENDFORM.
*** Fim - Rubenilson Pereira - 09.10.2025 #192341
