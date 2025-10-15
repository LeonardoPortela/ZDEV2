*&---------------------------------------------------------------------*
*& Report  ZAA11
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zaa11.
*/===========================================================================\*
*| Tabelas                                                                   |*
*/===========================================================================\*
TABLES: anla, anlz, bkpf, zaa005.

*/===========================================================================\*
*| Types                                                                     |*
*/===========================================================================\*
TYPES: BEGIN OF ty_dados_imob,
         semaf         TYPE char7,              "Semáforo Status Validação (Verm Nv1 / Am Nv2 / Ver Nv3)
         aprov         TYPE char7,              "Status de Aprovação do Imobilizado
         anln1         TYPE anlz-anln1,         "Nº Imobilizado
         anln2         TYPE anlz-anln2,         "Sub Nº Imobilizado
         bukrs         TYPE anlz-bukrs,         "Empresa
         gsber         TYPE anlz-gsber,         "Filial
         kostl         TYPE anlz-kostl,         "Centro de Custo
         cskt_ltext    TYPE cskt-ltext,         "Descr. Centro de Custo  "*-CS2022000978-31.01.2023-#93774-JT
         aktiv         TYPE anla-aktiv,         "Data Incorporação
         txt50         TYPE anla-txt50,         "Denominação 01
         txa50         TYPE anla-txa50,         "Denominação 02
         invnr         TYPE anla-invnr,         "Chassi
         sernr         TYPE anla-sernr,         "Série
         stort         TYPE anlz-stort,         "Localização
         raumn         TYPE anlz-raumn,         "Sala
         kfzkz         TYPE anlz-kfzkz,         "Placa
         zimob_v       TYPE tb_ioa_calc_text, "char7,     "Status de Verificação
         zimob_v2      TYPE tb_ioa_calc_text, "char7,     "Status de Verificação ano anterior  " Rubenilson - 15.01.24 - US128402
         anexo         TYPE char7,              "Coluna para Botão de Anexo
         texto         TYPE char7,              "Coluna para Botão de Texto
         zimob_p       TYPE char7,              "Plaqueta? (S/N)
         zimob_a       TYPE zaa005-zimob_a,     "Nível de Aprovação
*         ZIMOB_A2   TYPE ZAA005-ZIMOB_A,     "Nível da próxima aprovação
         usuar_aa      TYPE zaa004-usuar_aa,    "Próximo aprovador
         avali         TYPE char7,              "Status de Aprovação/Retorno
         observ        TYPE zaa005-observ,
         cc_sobra      TYPE zaa005-cc_sobra,
         manual        TYPE zaa005-manual,
         cellstyles    TYPE lvc_t_styl,
         anlkl         TYPE anla-anlkl,
         desc_classe   TYPE ankt-txk50,         " Rubenilson - 15.01.24 - US128402
         conta         TYPE t095-ktansw,        "Conta        "*-CS2022000978-31.01.2023-#93774-JT-inicio
         desc_conta    TYPE skat-txt50,         "Descr. Conta "*-CS2022000978-31.01.2023-#93774-JT-inicio
         img           TYPE zaa005-img,         "Descr. Conta "*-CS2022000978-31.01.2023-#93774-JT-inicio
         inventariante TYPE zaa005-inventariante,         "Descr. Conta "*-CS2022000978-31.01.2023-#93774-JT-inicio
         equnr         TYPE equi-equnr,         "Equipamento            "User Story 153788 // MMSILVA - 07.10.2024
         eqktx         TYPE eqkt-eqktx,         "Desc. do Equipamento   "User Story 153788 // MMSILVA - 07.10.2024
         kostl_iloa    TYPE iloa-kostl,         "Centro de Custo        "User Story 153788 // MMSILVA - 07.10.2024
         swerk         TYPE iloa-swerk,         "Filial                 "User Story 153788 // MMSILVA - 07.10.2024
         vlr_aquis_brl TYPE anlc-kansw,         " Rubenilson - 15.01.24 - US128402
         vlr_resid_brl TYPE anlc-kansw,         " Rubenilson - 15.01.24 - US128402
         vlr_aquis_usd TYPE anlc-kansw,         " Rubenilson - 15.01.24 - US128402
         vlr_resid_usd TYPE anlc-kansw.         " Rubenilson - 15.01.24 - US128402
TYPES: END OF ty_dados_imob.

*-CS2022000978-31.01.2023-#93774-JT-inicio
TYPES: BEGIN OF ty_conta,
         ktopl TYPE t095-ktopl,
         ktogr TYPE t095-ktogr,
         afabe TYPE t095-afabe,
         saknr TYPE skat-saknr,
         txt50 TYPE skat-txt50.
TYPES: END   OF ty_conta,
*-CS2022000978-31.01.2023-#93774-JT-fim

BEGIN OF ty_fleet_aux,
  anln1 TYPE zzimobi,
  anln2 TYPE anla-anln2,
END OF ty_fleet_aux.


*/===========================================================================\*
*| Data                                                                      |*
*/===========================================================================\*
DATA: it_dados_imob   TYPE STANDARD TABLE OF ty_dados_imob,
      it_fleet_aux    TYPE TABLE OF ty_fleet_aux,
      it_iloa_aux     TYPE TABLE OF iloa,
      it_equz_aux     TYPE TABLE OF equz,
      it_equi_aux     TYPE TABLE OF equi,
      it_eqkt_aux     TYPE TABLE OF eqkt,
      wa_dados_imob   TYPE ty_dados_imob,
      wa_sobra_fisica TYPE ty_dados_imob,
      it_zaa004       TYPE STANDARD TABLE OF zaa004,
      wa_zaa004       TYPE zaa004,
      it_zaa004_aprov TYPE STANDARD TABLE OF zaa004,
      wa_zaa004_aprov TYPE zaa004,
      it_zaa005       TYPE STANDARD TABLE OF zaa005,
      wa_zaa005       TYPE zaa005,
      wa_smart_form   TYPE zaae_termo_inventario,
      it_smart_form   TYPE TABLE OF zaae_termo_inventario,
      it_smart_quadro TYPE zaat_termo_quadro,
      wa_smart_quadro TYPE zaae_termo_quadro,
      it_smart_exec   TYPE zaat_termo_executor,
      wa_smart_exec   TYPE zaae_termo_executor,
      it_dd07v        TYPE TABLE OF dd07v,
      wa_dd07v        TYPE dd07v,
      it_imobil       TYPE TABLE OF anla,
      wa_imobil       TYPE anla,
      it_anla         TYPE TABLE OF anla,
      wa_anla         TYPE anla,
      it_cskt         TYPE TABLE OF cskt, "*-CS2022000978-31.01.2023-#93774-JT
      wa_cskt         TYPE cskt,          "*-CS2022000978-31.01.2023-#93774-JT
      it_conta        TYPE TABLE OF ty_conta, "*-CS2022000978-31.01.2023-#93774-JT
      wa_conta        TYPE ty_conta, "*-CS2022000978-31.01.2023-#93774-JT
      it_fleet        TYPE TABLE OF fleet,  "User Story 153788 // MMSILVA - 07.10.2024
      wa_fleet        TYPE fleet,           "User Story 153788 // MMSILVA - 07.10.2024
      it_equi         TYPE TABLE OF equi,   "User Story 153788 // MMSILVA - 07.10.2024
      wa_equi         TYPE equi,            "User Story 153788 // MMSILVA - 07.10.2024
      it_eqkt         TYPE TABLE OF eqkt,   "User Story 153788 // MMSILVA - 07.10.2024
      wa_eqkt         TYPE eqkt,            "User Story 153788 // MMSILVA - 07.10.2024
      it_iloa         TYPE TABLE OF iloa,   "User Story 153788 // MMSILVA - 07.10.2024
      wa_iloa         TYPE iloa,            "User Story 153788 // MMSILVA - 07.10.2024
      it_equz         TYPE TABLE OF equz,   "User Story 153788 // MMSILVA - 08.10.2024
      wa_equz         TYPE equz.            "User Story 153788 // MMSILVA - 08.10.2024

DATA: manager TYPE REF TO cl_gos_manager.

DATA: it_texto TYPE STANDARD TABLE OF tline,
      wa_texto TYPE tline,
      wa_name  TYPE thead-tdname.

DATA: vg_1vtela    TYPE char1,   "Check se janela de status de verificação é a primeira a ser aberta
      vg_1ptela    TYPE char1,   "Check se janela de status da plaqueta é a primeira a ser aberta
      ok_code      TYPE sy-ucomm,
      vg_data      TYPE sy-datum,
      vg_row       TYPE lvc_s_roid-row_id,
      vg_auth_full TYPE char1,
      vg_zaa005    TYPE char1,
      t_exerc      TYPE TABLE OF rgsb4.

DATA: it_screen_status TYPE TABLE OF sy-ucomm.

DATA: lv_anln1          TYPE string,      "User Story 153788 // MMSILVA - 07.10.2024
      lv_length         TYPE i VALUE 30,  "User Story 153788 // MMSILVA - 07.10.2024
      lv_current_length TYPE i,           "User Story 153788 // MMSILVA - 07.10.2024
      lv_spaces         TYPE string.      "User Story 153788 // MMSILVA - 07.10.2024

RANGES:
      r_gjahr       FOR bkpf-gjahr.

*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_eventhandler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      handle_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id e_row_id es_row_no sender.

* Ini - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
*    CLASS-METHODS:
*      on_data_changed FOR EVENT data_changed  OF cl_gui_alv_grid
*        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
* Fim - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

ENDCLASS.                    "lcl_eventhandler DEFINITION

*/===========================================================================\*
*| Tela de Seleção                                                           |*
*/===========================================================================\*
SELECTION-SCREEN BEGIN OF BLOCK filtros WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs   FOR anlz-bukrs, "OBLIGATORY
                  p_gsber   FOR anlz-gsber, " OBLIGATORY,
                  p_kostl   FOR anlz-kostl,
                  p_gjahr   FOR bkpf-gjahr NO-EXTENSION NO INTERVALS,"OBLIGATORY
*-CS2019000323 - 02.09.2021 - JT - inicio
                  p_status  FOR zaa005-zimob_v,
                  p_ccsobr  FOR zaa005-cc_sobra.
  PARAMETERS    : p_manual   AS CHECKBOX.
*-CS2019000323 - 02.09.2021 - JT - fim
SELECTION-SCREEN END OF BLOCK filtros.

*-CS2019000323 - 02.09.2021 - JT - inicio
AT SELECTION-SCREEN OUTPUT.

  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).

  IF sy-dynnr = 1000.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'PF1000'
        p_program = sy-repid
      TABLES
        p_exclude = it_screen_status.

  ENDIF.

AT SELECTION-SCREEN. "110128 CS2023000293 Imp. dados invent. /  ZAA16 - PSA
*  IF p_gsber[] IS INITIAL AND p_kostl[] IS INITIAL.
*    MESSAGE s024(sd) WITH TEXT-100 TEXT-101 DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
**-CS2019000323 - 02.09.2021 - JT - fim

  CASE sy-ucomm.
    WHEN 'F01'.
      CALL TRANSACTION 'ZAA16_IMP' WITH AUTHORITY-CHECK.
    WHEN 'ONLI'.
      LOOP AT SCREEN.
        IF p_gsber[] IS INITIAL AND p_kostl[] IS INITIAL.
          IF screen-name(7) = 'P_GSBER'.
            screen-required = '2'.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Empresa é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.
      IF p_gsber[] IS INITIAL.
        MESSAGE 'Filial é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.
      IF p_kostl[] IS INITIAL OR p_bukrs IS INITIAL OR p_gjahr IS INITIAL.
        MESSAGE 'Centro de Custoé Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.
      IF p_gjahr IS INITIAL.
        MESSAGE 'Ano do Iventário é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim
*	WHEN .
*          SUBMIT zregister_data     WITH p_db_tab  = 'ZGLT0111'
*                                WITH p_stcnam = 'ZGLT0111_OUT'
*                                WITH p_scmant = '0171'
*                                WITH p_title = 'Cadastro de Valor de Patrimônio Líquido'
*    AND RETURN.
      "WHEN .
      "WHEN OTHERS.
  ENDCASE.

*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD on_hotspot_click.
    DATA: row  TYPE i.
    CLEAR: row.
    CONDENSE e_row_id NO-GAPS.
    row = e_row_id.

    CASE e_column_id.
      WHEN 'IMG'.
        READ TABLE it_dados_imob ASSIGNING FIELD-SYMBOL(<call_popup>) INDEX row.

        DATA: _url TYPE string.

        _url = <call_popup>-img.

        IF <call_popup>-img IS NOT INITIAL.

          cl_gui_frontend_services=>execute(
            EXPORTING
              application = 'chrome.exe'
              parameter   = _url
            EXCEPTIONS
              OTHERS      = 1 ).
        ENDIF.


      WHEN OTHERS.
    ENDCASE.



*e_column_id e_row_id es_row_no sender
*    CASE column.
*      WHEN 'VALID'.
*        CHECK <call_popup>-valid = icon_led_red.
*        IF <call_popup> IS NOT INITIAL.
*          MESSAGE <call_popup>-error TYPE 'I'.
*        ENDIF.
*      WHEN  'CHECK_CALC'.
*
*        DATA: ans TYPE c. "answer.
*        CLEAR: ans.
*
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            titlebar              = ' '
**           DIAGNOSE_OBJECT       = ' '
*            text_question         = 'Deseja Limpar o valor do Check Calculo ?'
*            text_button_1         = 'Sim'
*            "icon_button_1  = 'ICON_OKAY'
*            text_button_2         = 'Não'
*            "icon_button_2  = 'ICON_OKAY'
*            default_button        = '2'
*            display_cancel_button = ''
*          IMPORTING
*            answer                = ans
*          EXCEPTIONS
*            text_not_found        = 1
*            OTHERS                = 2.
*        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*
*        IF ans EQ '1'.
*          <call_popup>-check_calc = abap_false.
*          <call_popup>-vlr_multa_rbdo = abap_false.
*          <call_popup>-vlr_juros_rbdo = abap_false.
**          PERFORM calcular.
**          on_data_changed_finished(
**            e_modified = 'X'
**          ).
*          gr_table->refresh( ).
*        ELSE.
*          EXIT.
*        ENDIF.
*
*      WHEN OTHERS.
*    ENDCASE.

  ENDMETHOD.


  METHOD handle_button_click .

    DATA: wa_obj  TYPE borident.
    DATA: ctl_alv TYPE REF TO cl_gui_alv_grid.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab.

    DATA: wl_header    TYPE thead.
    DATA: wl_name  TYPE thead-tdname.

    DATA: ip_mode TYPE sgs_rwmod.

    DATA: anexo_obj     TYPE REF TO cl_gos_manager,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident.


    READ TABLE it_dados_imob INTO wa_dados_imob INDEX es_row_no-row_id.


    CASE es_col_id.
      WHEN 'ANEXO'.  "Anexar

        CREATE OBJECT anexo_obj TYPE cl_gos_manager.

*        IF MANAGER IS NOT INITIAL.
*          CALL METHOD MANAGER->UNPUBLISH.
*          CLEAR: MANAGER.
*        ENDIF.

        "Só permite anexar quando imobilizado estiver em nivel de aprovação 1 ou 0 e usuário pertinente
        IF ( wa_dados_imob-zimob_a EQ 0 OR wa_dados_imob-zimob_a EQ 1 ) AND ( wa_dados_imob-usuar_aa EQ sy-uname ).
          ip_mode = 'E'.
        ELSE.
          ip_mode = 'R'.
        ENDIF.

*        WA_OBJ-OBJTYPE = 'ZAA11'.
*        CONCATENATE P_GJAHR-LOW WA_DADOS_IMOB-ANLN1 WA_DADOS_IMOB-ANLN2
*                    WA_DADOS_IMOB-GSBER WA_DADOS_IMOB-KOSTL INTO WA_OBJ-OBJKEY.
*
*        CREATE OBJECT MANAGER
*          EXPORTING
*            IS_OBJECT        = WA_OBJ
*            IP_NO_COMMIT     = 'R'
*            IP_MODE          = IP_MODE
*          EXCEPTIONS
*            OBJECT_INVALID   = 1
*            CALLBACK_INVALID = 2
*            OTHERS           = 3.
*
*        CLEAR WA_DADOS_IMOB.

        IF wa_dados_imob-anexo EQ '@02@'.
          vl_ip_service = 'PCATTA_CREA'.
        ELSE.
          vl_ip_service = 'VIEW_ATTA'.
        ENDIF.

        CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                    wa_dados_imob-gsber wa_dados_imob-kostl INTO wa_bor-objkey.

        wa_bor-objtype = 'ZAA11'.

        anexo_obj->set_rw_mode( ip_mode = ip_mode ).

        anexo_obj->start_service_direct(
          EXPORTING
            ip_service       = vl_ip_service
            is_object        = wa_bor
          EXCEPTIONS
            no_object        = 1
            object_invalid   = 2
            execution_failed = 3
            OTHERS           = 4 ).

        COMMIT WORK.

        CLEAR wa_dados_imob.

*        CREATE OBJECT ANEXO_OBJ TYPE CL_GOS_MANAGER.
*
*        IF WA_DADOS_IMOB-ANEXO EQ '@02@'.
*          VL_IP_SERVICE = 'PCATTA_CREA'.
*        ELSE.
*          VL_IP_SERVICE = 'VIEW_ATTA'.
*        ENDIF.
*
*        CONCATENATE P_GJAHR-LOW WA_DADOS_IMOB-ANLN1 WA_DADOS_IMOB-ANLN2
*                    WA_DADOS_IMOB-GSBER WA_DADOS_IMOB-KOSTL INTO WA_BOR-OBJKEY.
*
*        WA_BOR-OBJTYPE = 'ZAA11'.
*
*        ANEXO_OBJ->SET_RW_MODE( IP_MODE = IP_MODE ).
*
*        ANEXO_OBJ->START_SERVICE_DIRECT(
*          EXPORTING
*            IP_SERVICE         = VL_IP_SERVICE
*            IS_OBJECT          = WA_BOR
*          EXCEPTIONS
*            NO_OBJECT          = 1
*            OBJECT_INVALID     = 2
*            EXECUTION_FAILED   = 3
*            OTHERS             = 4 ).
*
*        COMMIT WORK.

      WHEN 'TEXTO'. "Texto

        REFRESH: it_texto, tl_texto.
        CLEAR: wl_name, wl_texto.



        CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                     wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id        = 'ZAA1'
            language  = sy-langu
            name      = wl_name
            object    = 'ZAA02'
          TABLES
            lines     = it_texto
          EXCEPTIONS
            id        = 1
            language  = 2
            name      = 3
            not_found = 4
            OTHERS    = 5.

        IF sy-subrc IS INITIAL.
          LOOP AT it_texto INTO wa_texto.
            MOVE: wa_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title = 'Observação'
          CHANGING
            ch_text  = tl_texto.

        IF sy-ucomm EQ 'CX_CONT'.  "Salvar

          READ TABLE it_dados_imob INTO wa_dados_imob INDEX es_row_no-row_id.

          "Só permite anexar quando imobilizado estiver em nivel de aprovação 1 ou 0 e usuário pertinente
          IF ( wa_dados_imob-zimob_a EQ 0 OR wa_dados_imob-zimob_a EQ 1 ) AND ( wa_dados_imob-usuar_aa EQ sy-uname ).

            IF tl_texto[] IS NOT INITIAL.

              MOVE '@6X@' TO wa_dados_imob-texto.
              MODIFY it_dados_imob FROM wa_dados_imob INDEX es_row_no-row_id TRANSPORTING texto.

              READ TABLE it_dados_imob INTO wa_dados_imob INDEX es_row_no-row_id.
              CLEAR: it_texto.

              LOOP AT tl_texto INTO wl_texto.
                MOVE: '*'      TO wa_texto-tdformat,
                      wl_texto TO wa_texto-tdline.
                APPEND wa_texto TO it_texto.
              ENDLOOP.

              CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                          wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_header-tdname.

              wl_header-tdobject = 'ZAA02'.
              wl_header-tdid     = 'ZAA1'.
              wl_header-tdspras  = sy-langu.

              CALL FUNCTION 'SAVE_TEXT'
                EXPORTING
                  header          = wl_header
                  savemode_direct = 'X'
                TABLES
                  lines           = it_texto
                EXCEPTIONS
                  id              = 1
                  language        = 2
                  name            = 3
                  object          = 4
                  OTHERS          = 5.

              CLEAR it_texto.
*              COMMIT WORK.

              CALL FUNCTION 'COMMIT_TEXT'
                EXPORTING
                  object          = 'ZAA02'
                  name            = wl_header-tdname
                  id              = 'ZAA1'
                  language        = sy-langu
                  savemode_direct = ' '.

            ELSE.

              CLEAR: wl_name.
              CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                          wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name.

              CALL FUNCTION 'DELETE_TEXT'
                EXPORTING
                  id        = 'ZAA1'
                  language  = sy-langu
                  name      = wl_name
                  object    = 'ZAA02'
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.

              COMMIT WORK.

              MOVE '@6Y@' TO wa_dados_imob-texto.
              MODIFY it_dados_imob FROM wa_dados_imob INDEX es_row_no-row_id TRANSPORTING texto.
              CLEAR: wa_dados_imob.
            ENDIF.

          ENDIF.
        ENDIF.

*      WHEN 'ZIMOB_V'. "Status de Verificação
**        CLEAR: WA_DADOS_IMOB.
**        READ TABLE IT_DADOS_IMOB INTO WA_DADOS_IMOB INDEX ES_ROW_NO-ROW_ID.
**
**        IF ( WA_DADOS_IMOB-ZIMOB_V = 3 ).
**
**          SET PARAMETER ID 'BUK' FIELD WA_DADOS_IMOB-BUKRS.
**          SET PARAMETER ID 'WRK' FIELD WA_DADOS_IMOB-GSBER.
**          SET PARAMETER ID 'AN1' FIELD WA_DADOS_IMOB-ANLN1.
**          CALL TRANSACTION 'ZAA18' AND SKIP FIRST SCREEN.
**
**        ENDIF.
*
*        VG_ROW = ES_ROW_NO-ROW_ID.
*        VG_1VTELA = ABAP_TRUE.
*        CALL SCREEN 0200 STARTING AT 05 05.
*        CLEAR:WA_DADOS_IMOB.

      WHEN 'ZIMOB_P'. "Status da Plaqueta

        vg_row = es_row_no-row_id.
        vg_1ptela = abap_true.
        CALL SCREEN 0300 STARTING AT 05 05.
        CLEAR:wa_dados_imob.

      WHEN 'AVALI'.

        CALL FUNCTION 'ZENQUEUE_INV_IMOBILIZADO'
          EXPORTING
            chave          = wa_dados_imob-anln1
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        REFRESH: it_texto.
        CLEAR: wl_name.

        CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                     wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id        = 'ZAA2'
            language  = sy-langu
            name      = wl_name
            object    = 'ZAA02'
          TABLES
            lines     = it_texto
          EXCEPTIONS
            id        = 1
            language  = 2
            name      = 3
            not_found = 4
            OTHERS    = 5.

        IF sy-subrc IS INITIAL.
          LOOP AT it_texto INTO wa_texto.
            MOVE: wa_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title = 'Observação'
          CHANGING
            ch_text  = tl_texto.

        IF sy-ucomm EQ 'CX_CONT'.  "Salvar

          READ TABLE it_dados_imob INTO wa_dados_imob INDEX es_row_no-row_id.

          IF tl_texto[] IS NOT INITIAL.

            CLEAR: it_texto.

            LOOP AT tl_texto INTO wl_texto.
              MOVE: '*'        TO wa_texto-tdformat,
                      wl_texto TO wa_texto-tdline.
              APPEND wa_texto TO it_texto.
            ENDLOOP.

            CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                        wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_header-tdname.

            wl_header-tdobject = 'ZAA02'.
            wl_header-tdid     = 'ZAA2'.
            wl_header-tdspras  = sy-langu.

            CALL FUNCTION 'SAVE_TEXT'
              EXPORTING
                header          = wl_header
                savemode_direct = 'X'
              TABLES
                lines           = it_texto
              EXCEPTIONS
                id              = 1
                language        = 2
                name            = 3
                object          = 4
                OTHERS          = 5.

            CALL FUNCTION 'COMMIT_TEXT'
              EXPORTING
                object          = 'ZAA02'
                name            = wl_header-tdname
                id              = 'ZAA2'
                language        = sy-langu
                savemode_direct = ' '.

            CLEAR it_texto.

          ELSE.

            CLEAR: wl_name.
            CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                        wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name.

            CALL FUNCTION 'DELETE_TEXT'
              EXPORTING
                id        = 'ZAA2'
                language  = sy-langu
                name      = wl_name
                object    = 'ZAA02'
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            COMMIT WORK.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'ZDENQUEUE_INV_IMOBILIZADO'
          EXPORTING
            chave = wa_dados_imob-anln1.

    ENDCASE.

    LEAVE TO SCREEN 0100.

  ENDMETHOD .

* Ini - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
*  METHOD on_data_changed.
*
*    DATA: seltab    TYPE TABLE OF rsparams,
*          wl_name   TYPE thead-tdname,
*          vl_bloq   TYPE char1,
*          lv_value  TYPE lvc_value,
*          ls_stable TYPE lvc_s_stbl.

*    CHECK er_data_changed->mt_good_cells[] IS NOT INITIAL.
*
*    LOOP AT er_data_changed->mt_good_cells[] INTO DATA(w_cells).
*      "LOOP AT ET_GOOD_CELLS[] INTO DATA(W_CELLS).
*      CASE w_cells-fieldname.
*
**-CS2019000323 - 02.09.2021 - JT - inicio
*        WHEN 'OBSERV'.
*          lv_value = w_cells-value.
*
*          READ TABLE it_dados_imob INTO wa_dados_imob INDEX w_cells-row_id.
*
*          IF sy-subrc = 0.
*            UPDATE zaa005 SET observ = lv_value
*                        WHERE anln1  = wa_dados_imob-anln1
*                          AND anln2  = wa_dados_imob-anln2
*                          AND bukrs  = wa_dados_imob-bukrs
*                          AND gsber  = wa_dados_imob-gsber
*                          AND kostl  = wa_dados_imob-kostl
*                          AND gjahr  =  p_gjahr-low.
*            COMMIT WORK AND WAIT.
*          ENDIF.
**-CS2019000323 - 02.09.2021 - JT - fim
*
*        WHEN 'ZIMOB_V'.
*          vg_row = w_cells-row_id.
*
*          READ TABLE it_dados_imob INTO wa_dados_imob INDEX w_cells-row_id.
*
*          DATA(wa_zaa005_mod) = VALUE zaa005(
*              anln1   = wa_dados_imob-anln1
*              anln2   = wa_dados_imob-anln2
*              bukrs   = wa_dados_imob-bukrs
*              gsber   = wa_dados_imob-gsber
*              kostl   = wa_dados_imob-kostl
*              gjahr   = p_gjahr-low
*              zimob_a = wa_dados_imob-zimob_a
*              zimob_p = wa_dados_imob-zimob_p ).
*
*          CASE w_cells-value.
*            WHEN '0'.
*              wa_zaa005_mod-zimob_v = '0'.
*              wa_dados_imob-zimob_v = '0'.
*            WHEN '1'.
*              wa_zaa005_mod-zimob_v = '1'.
*              wa_dados_imob-zimob_v = '1'.
*            WHEN '2'.
*              wa_zaa005_mod-zimob_v = '2'.
*              wa_dados_imob-zimob_v = '2'.
*
*              "Busca Texto
*
**-CS2019000323 - 02.09.2021 - JT - inicio
*              REFRESH: it_texto.
**              CLEAR: wl_name.
**              CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
**                          wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name.
**
**              CALL FUNCTION 'READ_TEXT'
**                EXPORTING
**                  id                      = 'ZAA1'
**                  language                = sy-langu
**                  name                    = wl_name
**                  object                  = 'ZAA02'
**                TABLES
**                  lines                   = it_texto
**                EXCEPTIONS
**                  id                      = 1
**                  language                = 2
**                  name                    = 3
**                  not_found               = 4
**                  object                  = 5
**                  reference_check         = 6
**                  wrong_access_to_archive = 7
**                  OTHERS                  = 8.
*
*              "Bloqueia salvar novo status
**             IF sy-subrc IS NOT INITIAL.
*              IF wa_dados_imob-observ IS INITIAL.
*                vl_bloq = abap_true.
*                MESSAGE text-009 TYPE 'S' DISPLAY LIKE 'E'.
*              ENDIF.
**-CS2019000323 - 02.09.2021 - JT - fim
*
*              CLEAR: it_texto.
*
*            WHEN '3'.
*
*              wa_zaa005_mod-zimob_v = '3'.
*              wa_dados_imob-zimob_v = '3'.
*
*              DATA(seltab_wa) = VALUE rsparams(
*                selname = 'P_BUKRS' sign = 'I' option = 'EQ' low = wa_zaa005_mod-bukrs ).
*              APPEND seltab_wa TO seltab[]. CLEAR: seltab_wa.
*              seltab_wa = VALUE rsparams(
*               selname = 'P_WERKS' sign = 'I' option = 'EQ' low = wa_zaa005_mod-gsber ).
*              APPEND seltab_wa TO seltab[]. CLEAR: seltab_wa.
*              seltab_wa = VALUE rsparams(
*                selname = 'P_ANLN1' sign = 'I' option = 'EQ' low = wa_zaa005_mod-anln1 ).
*              APPEND seltab_wa TO seltab[]. CLEAR: seltab_wa.
*
*              MODIFY zaa005 FROM wa_zaa005_mod.
*              MODIFY it_dados_imob FROM wa_dados_imob INDEX vg_row.
*
**              SUBMIT zaa13 WITH SELECTION-TABLE seltab AND RETURN. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
*
*              CLEAR: wa_zaa005_mod, wa_dados_imob.
*
*            WHEN '4'.
*              wa_zaa005_mod-zimob_v = '4'.
*              wa_dados_imob-zimob_v = '4'.
*
*          ENDCASE.
*
*          "Bloqueia salvar caso Imob não esteja no nível 0 ou 1 de aprovação
*          IF NOT ( ( wa_dados_imob-zimob_a EQ 0 OR wa_dados_imob-zimob_a EQ 1 ) AND ( wa_dados_imob-usuar_aa EQ sy-uname ) ). "( WA_DADOS_IMOB-ZIMOB_A NE 0 OR WA_DADOS_IMOB-ZIMOB_A NE 1 ) OR WA_DADOS_IMOB-USUAR_AA NE SY-UNAME.
*            vl_bloq = abap_true.
*          ENDIF.
*
*          IF vl_bloq IS INITIAL.
*            MODIFY zaa005 FROM wa_zaa005_mod.
*            MODIFY it_dados_imob FROM wa_dados_imob INDEX vg_row.
*            MESSAGE text-006 TYPE 'S' DISPLAY LIKE 'S'.
*            CLEAR: wa_zaa005_mod, vg_row, wa_dados_imob, vl_bloq.
*            "LEAVE TO SCREEN 0.
*          ELSE.
*            MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
*            CLEAR: wa_zaa005_mod, vg_row, wa_dados_imob, vl_bloq.
*            "LEAVE TO SCREEN 0.
*          ENDIF.
*
*      ENDCASE.
*    ENDLOOP.
*  ENDMETHOD.
* Fim - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION

*/===========================================================================\*
*| Start of Selection                                                        |*
*/===========================================================================\*
START-OF-SELECTION.

  PERFORM check_autorizacao.
  PERFORM seleciona_dados.
  PERFORM manipula_dados.
  PERFORM filtra_resultado.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTORIZACAO
*&---------------------------------------------------------------------*
*    Busca usuários cadastrados como aprovadores para acesso
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_autorizacao .

  FREE: vg_auth_full,
        vg_data.

*-CS2019000323 - 02.09.2021 - JT - inicio
  SELECT *
    FROM usr05
    INTO @DATA(w_usr05)
      UP TO 1 ROWS
   WHERE bname = @sy-uname
     AND parid = 'ZAA16'
     AND parva = '*'.
  ENDSELECT.

  IF sy-subrc = 0.
    vg_auth_full = abap_true.
  ELSE.
    SELECT * FROM zaa004
      INTO TABLE it_zaa004[]
      WHERE gsber    IN p_gsber
        AND gjahr    IN p_gjahr
        AND kostl    IN p_kostl
        AND usuar_aa EQ sy-uname
        AND ( dt_ini LE sy-datum AND
            dt_fim GE sy-datum ).

    IF it_zaa004[] IS INITIAL.
      MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

  SELECT * FROM j_1bbranch
    INTO TABLE @DATA(it_j_1bbranch)
    WHERE branch IN @p_gsber.
  SORT it_j_1bbranch[] BY bukrs.

  DELETE ADJACENT DUPLICATES FROM it_j_1bbranch[] COMPARING bukrs.
  DATA(vl_lines) = lines( it_j_1bbranch[] ).

  IF vl_lines NE 1.
    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSE.

    READ TABLE it_j_1bbranch INTO DATA(wa_j_1bbranch) INDEX 1.

    SELECT SINGLE dt_ate
      FROM zaa006
      INTO vg_data
      WHERE bukrs EQ wa_j_1bbranch-bukrs
        AND gjahr IN p_gjahr.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ENDIF.

*  CALL FUNCTION 'FIRST_AND_LAST_DAY_IN_YEAR_GET'
*    EXPORTING
*      I_GJAHR        = P_GJAHR-LOW
*      I_PERIV        = 'K1'
*    IMPORTING
*      E_LAST_DAY     = VL_UDATA
*    EXCEPTIONS
*      INPUT_FALSE    = 1
*      T009_NOTFOUND  = 2
*      T009B_NOTFOUND = 3
*      OTHERS         = 4.
*
*  IF SY-DATUM > VL_UDATA.
*    VG_DATA = VL_UDATA.
*  ELSE.
*    VG_DATA = SY-DATUM.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .

  TYPES: ty_anln1 TYPE RANGE OF anlz-anln1,
         ty_anln2 TYPE RANGE OF anlz-anln2,
         ty_bukrs TYPE RANGE OF anlz-bukrs,
         ty_kostl TYPE RANGE OF anlz-kostl.

  FREE: t_exerc,
        r_gjahr,
        vg_zaa005,
        it_dados_imob.

*-CS2019000323 - 02.09.2021 - JT - inicio
*-------------------------------------------------------
* set MAGGI_IVADUPLABASE
*-------------------------------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZAA16_EXERC'
    TABLES
      set_values    = t_exerc
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_exerc INTO DATA(w_exerc).
    IF w_exerc-from IN p_gjahr[].
      vg_zaa005 = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
*-CS2019000323 - 02.09.2021 - JT - fim

*-CS2019000323 - 02.09.2021 - JT - inicio
  IF vg_auth_full = abap_false.
    SELECT z4~bukrs, z4~gsber, z4~kostl,
           z4~gjahr, z4~nivel_aa, z4~usuar_aa,
           z4~fluxo_baixa, z4~dt_ini, z4~dt_fim
          FROM zaa004 AS z4
          INTO TABLE @DATA(it_zaa004)
          WHERE z4~bukrs IN @p_bukrs
            AND z4~gsber IN @p_gsber
            AND z4~gjahr IN @p_gjahr
            AND z4~kostl IN @p_kostl
            AND z4~usuar_aa EQ @sy-uname
            AND z4~fluxo_baixa NOT LIKE 'X'
            AND ( dt_ini LE @sy-datum AND
                  dt_fim GE @sy-datum ).
    IF sy-subrc = 0.
      SORT it_zaa004[] BY kostl ASCENDING.
      DATA(r_kostl) = VALUE ty_kostl( FOR w_z4 IN it_zaa004[] (
                                          sign   = 'I'
                                          option = 'EQ'
                                          low    = w_z4-kostl ) ).
      SORT r_kostl[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM r_kostl[] COMPARING low.
    ENDIF.

    TRY.
        DATA(v_inicio_proc) = it_zaa004[ 1 ]-dt_ini.
        DATA(v_fim_proc) = it_zaa004[ 1 ]-dt_fim.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ELSE.
    r_kostl[]     = p_kostl[].
    v_inicio_proc = '20010101'.
    v_fim_proc    = '99991231'.
  ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

*-CS2019000323 - 02.09.2021 - JT - inicio
  IF vg_zaa005 = abap_true.
    SELECT anlz~anln1, anlz~anln2, anlz~bukrs, anlz~gsber,
           anlz~kostl, anla~aktiv, anla~txt50, anla~txa50,
           anla~invnr, anla~sernr, anlz~stort, anlz~raumn, anlz~kfzkz, anla~anlkl
        INTO TABLE @DATA(it_imob)
        FROM anlz
        INNER JOIN anla
        ON ( anla~bukrs = anlz~bukrs AND anla~anln1 = anlz~anln1 AND anla~anln2 = anlz~anln2 )
        WHERE anlz~gsber IN @p_gsber
          AND anlz~bukrs IN @p_bukrs
          AND anlz~kostl IN @r_kostl
          AND anlz~bdatu GE @vg_data
          AND anlz~adatu LE @vg_data
          "AND ( ANLA~ERDAT GE @V_INICIO_PROC AND
          AND   anla~erdat LE @v_fim_proc
          "AND ( ANLA~AEDAT GE @V_INICIO_PROC AND
          "      ANLA~AEDAT LE @V_FIM_PROC )
          AND ( anla~deakt GT @vg_data OR
                anla~deakt EQ '00000000' )
        %_HINTS ORACLE 'A'.

    IF sy-subrc = 0.
      SORT it_imob[] BY anln1 ASCENDING
                        anln2 ASCENDING
                        gsber ASCENDING
                        kostl ASCENDING.
      MOVE-CORRESPONDING it_imob[] TO it_dados_imob[].
    ENDIF.

    IF wa_sobra_fisica IS NOT INITIAL.

      SELECT anlz~anln1, anlz~anln2, anlz~bukrs, anlz~gsber,
           anlz~kostl, anla~aktiv, anla~txt50, anla~txa50,
           anla~invnr, anla~sernr, anlz~stort, anlz~raumn, anlz~kfzkz, anla~anlkl
        INTO TABLE @DATA(it_imob_sobra)
        FROM anlz
        INNER JOIN anla
        ON ( anla~bukrs = anla~bukrs AND anla~anln1 = anlz~anln1 AND anla~anln2 = anlz~anln2 )
        WHERE anlz~gsber IN @p_gsber
          AND anlz~bukrs EQ @wa_sobra_fisica-bukrs
          AND anlz~anln1 EQ @wa_sobra_fisica-anln1
          AND anlz~anln2 EQ @wa_sobra_fisica-anln2
          AND anlz~kostl EQ @wa_sobra_fisica-kostl
          AND ( anla~aedat GE @v_inicio_proc AND
                anla~aedat LE @v_fim_proc )
          AND ( anla~deakt GT @vg_data OR
                anla~deakt EQ '00000000' ).
      "AND ANLZ~BDATU GE @VG_DATA
      "AND ANLZ~ADATU LE @VG_DATA
      "AND ( ANLA~DEAKT GT @VG_DATA OR
      "     ANLA~DEAKT EQ '00000000' ).

      IF sy-subrc = 0.

        CLEAR: wa_dados_imob, wa_sobra_fisica.
        SORT it_imob_sobra[] BY anln1 ASCENDING
                                anln2 ASCENDING
                                gsber ASCENDING
                                kostl ASCENDING.
        LOOP AT it_imob_sobra[] INTO DATA(w_sobra).
          MOVE-CORRESPONDING w_sobra TO wa_dados_imob.
          APPEND wa_dados_imob TO it_dados_imob[].
        ENDLOOP.

      ELSE.

        CLEAR: wa_dados_imob, wa_sobra_fisica.
        MESSAGE 'Imobilizado não encontrado!' TYPE 'I'.
        EXIT.

      ENDIF.

    ENDIF.

    DELETE ADJACENT DUPLICATES FROM it_dados_imob COMPARING anln1 anln2 bukrs gsber kostl.

    DATA(r_anln1) = VALUE ty_anln1(
          FOR _imob IN it_dados_imob[] (  sign = 'I'
                                          option = 'EQ'
                                          low = _imob-anln1 )  ).
    SORT r_anln1 BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM r_anln1 COMPARING low.

    CHECK ( it_dados_imob[] IS NOT INITIAL ).

    SELECT *
      FROM zaa005
      INTO TABLE it_zaa005
      FOR ALL ENTRIES IN it_dados_imob
      WHERE anln1 EQ it_dados_imob-anln1
        AND anln2 EQ it_dados_imob-anln2
        AND bukrs IN p_bukrs
        AND gsber IN p_gsber
        AND kostl IN p_kostl
        AND gjahr EQ p_gjahr-low
*-CS2019000323 - 02.09.2021 - JT - inicio
        AND zimob_v  IN p_status
        AND cc_sobra IN p_ccsobr
        AND manual   EQ p_manual.
*-CS2019000323 - 02.09.2021 - JT - fim

    IF sy-subrc = 0.

      LOOP AT it_dados_imob[] ASSIGNING FIELD-SYMBOL(<w_dados_check>).
        READ TABLE it_zaa005[] INTO wa_zaa005 WITH KEY anln1 = <w_dados_check>-anln1
                                                       anln2 = <w_dados_check>-anln2
                                                       kostl = <w_dados_check>-kostl
                                                       gjahr = p_gjahr-low.
*                                             TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          <w_dados_check>-bukrs = '9999'.
        ENDIF.



*-CS2019000323 - 02.09.2021 - JT - inicio
        IF wa_zaa005-observ IS INITIAL.
          CONCATENATE p_gjahr-low           <w_dados_check>-anln1 <w_dados_check>-anln2
                      <w_dados_check>-gsber <w_dados_check>-kostl INTO wa_name.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id        = 'ZAA1'
              language  = sy-langu
              name      = wa_name
              object    = 'ZAA02'
            TABLES
              lines     = it_texto
            EXCEPTIONS
              id        = 1
              language  = 2
              name      = 3
              not_found = 4
              OTHERS    = 5.

          IF sy-subrc IS INITIAL.
            LOOP AT it_texto INTO wa_texto.
              IF sy-tabix = 1.
                wa_zaa005-observ = wa_texto-tdline.
              ELSE.
                CONCATENATE wa_zaa005-observ wa_texto-tdline
                       INTO wa_zaa005-observ.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        "Retirar caracter especial: IR074178
        wa_zaa005-observ = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( wa_zaa005-observ ) ) ).

        <w_dados_check>-observ = wa_zaa005-observ.
*-CS2019000323 - 02.09.2021 - JT - fim

      ENDLOOP.

      DELETE it_dados_imob[] WHERE bukrs = '9999'.

    ENDIF.
  ELSE.

*** Inicio - Rubenilson - 15.01.24 - 128402
*    select *
*      from zaa005
*      into table it_zaa005
*     where bukrs in p_bukrs
*       and gsber in p_gsber
*       and kostl in p_kostl
*       and gjahr in p_gjahr
**-CS2019000323 - 02.09.2021 - JT - inicio
*       and zimob_v  in p_status
*       and cc_sobra in p_ccsobr
*       and manual   eq p_manual.
**-CS2019000323 - 02.09.2021 - JT - fim

    SELECT a~mandt a~anln1 a~anln2 a~bukrs a~gsber a~kostl a~gjahr a~zimob_v a~zimob_a a~zimob_p a~zimob_r a~zusap_4 a~zdtap_4 a~zhrap_4 a~aktiv a~txt50
           a~txa50 a~invnr a~sernr a~kfzkz a~stort a~raumn a~dt_proc a~hr_proc a~us_proc a~observ a~cc_sobra a~manual a~img a~inventariante
      FROM zaa005 AS a
      INNER JOIN anlz AS b
      ON  b~anln1 = a~anln1
      AND b~kostl = a~kostl
      INTO TABLE it_zaa005
     WHERE a~bukrs IN p_bukrs
       AND a~gsber IN p_gsber
       AND a~kostl IN p_kostl
       AND a~gjahr IN p_gjahr
       AND a~zimob_v  IN p_status
       AND a~cc_sobra IN p_ccsobr
       AND a~manual   EQ p_manual
       AND b~bdatu >= sy-datum.
*** Fim - Rubenilson - 15.01.24 - 128402

    IF sy-subrc = 0.
      SORT it_zaa005 BY anln1 anln2.
      DELETE ADJACENT DUPLICATES FROM it_zaa005 COMPARING anln1 anln2.
    ENDIF.

    LOOP AT it_zaa005 INTO wa_zaa005.
      IF wa_zaa005-observ IS INITIAL.
        CONCATENATE p_gjahr-low      wa_zaa005-anln1 wa_zaa005-anln2
                    wa_zaa005-gsber  wa_zaa005-kostl INTO wa_name.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id        = 'ZAA1'
            language  = sy-langu
            name      = wa_name
            object    = 'ZAA02'
          TABLES
            lines     = it_texto
          EXCEPTIONS
            id        = 1
            language  = 2
            name      = 3
            not_found = 4
            OTHERS    = 5.

        IF sy-subrc IS INITIAL.
          LOOP AT it_texto INTO wa_texto.
            IF sy-tabix = 1.
              wa_zaa005-observ = wa_texto-tdline.
            ELSE.
              CONCATENATE wa_zaa005-observ wa_texto-tdline
                     INTO wa_zaa005-observ.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "Retirar caracter especial: IR074178
      wa_zaa005-observ = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( wa_zaa005-observ ) ) ).

      MOVE-CORRESPONDING wa_zaa005 TO wa_dados_imob.
      APPEND wa_dados_imob         TO it_dados_imob.
    ENDLOOP.
*-CS2019000323 - 02.09.2021 - JT - fim
  ENDIF.

  SELECT *
    FROM zaa004
    INTO TABLE it_zaa004_aprov
    FOR ALL ENTRIES IN it_dados_imob
      WHERE bukrs EQ it_dados_imob-bukrs
        AND gsber EQ it_dados_imob-gsber
        AND kostl EQ it_dados_imob-kostl
        AND gjahr EQ p_gjahr-low.

  DELETE it_zaa004_aprov[] WHERE fluxo_baixa IS NOT INITIAL.

  SELECT * FROM anla
    INTO TABLE it_anla
    FOR ALL ENTRIES IN it_dados_imob
    WHERE anln1 EQ it_dados_imob-anln1.

*-CS2022000978-31.01.2023-#93774-JT-inicio
  SELECT *
    FROM cskt
    INTO TABLE it_cskt
     FOR ALL ENTRIES IN it_dados_imob
   WHERE spras = sy-langu
     AND kostl = it_dados_imob-kostl.

  IF it_anla[] IS NOT INITIAL.
    SELECT t095~ktopl  t095~ktogr  t095~afabe
           skat~saknr  skat~txt50
      FROM t095
      INNER JOIN skat ON skat~spras = sy-langu
                     AND skat~ktopl = t095~ktopl
                     AND skat~saknr = t095~ktansw
      INTO TABLE it_conta
       FOR ALL ENTRIES IN it_anla
     WHERE t095~ktogr = it_anla-ktogr
       AND t095~afabe = '01'.
  ENDIF.

  SORT it_cskt BY kostl datbi DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_cskt
                        COMPARING kostl.
*-CS2022000978-31.01.2023-#93774-JT-fim


* User Story 153788 // MMSILVA - 07.10.2024 - Inicio
*  loop at it_anla into data(ls_anla).
*
*    lv_anln1 = ls_anla-anln1.
*
*    while lv_anln1 cp '0*'.
*      lv_anln1 = lv_anln1+1.
*    endwhile.
*
*    lv_current_length = strlen( lv_anln1 ).
*
*    if lv_current_length < lv_length.
*      lv_spaces = ''.
*      do lv_length - lv_current_length times.
*        lv_spaces = lv_spaces && ' '.
*      enddo.
*
*      lv_anln1 = lv_spaces && lv_anln1.
*    endif.
*
*    data it_temp_fleet type table of fleet.
*
*    select *
*      from fleet
*      into table @it_temp_fleet
*      where zzimobilizado = @lv_anln1.
*
*    append lines of it_temp_fleet to it_fleet.
*  endloop.


  FREE: it_fleet_aux.
  it_fleet_aux = VALUE #( FOR ls_anla IN it_anla ( anln1 = |{ ls_anla-anln1 ALPHA = OUT }|
                                                   anln2 = ls_anla-anln2 ) ).


  LOOP AT it_anla INTO DATA(lw_anla).
    APPEND VALUE #( anln1 = lw_anla-anln1
                    anln2 = lw_anla-anln2 ) TO it_fleet_aux.

  ENDLOOP.


  IF it_fleet_aux IS NOT INITIAL.
    SELECT * FROM fleet INTO TABLE it_fleet
    FOR ALL ENTRIES IN it_fleet_aux
    WHERE zzimobilizado EQ IT_fleet_aux-anln1.
    IF it_fleet IS NOT INITIAL.
      SELECT *
        FROM equi
        INTO TABLE it_equi
        FOR ALL ENTRIES IN it_fleet
        WHERE objnr = it_fleet-objnr.
    ENDIF.


    IF it_equi IS NOT INITIAL.
      SELECT *
        FROM eqkt
        INTO TABLE it_eqkt
        FOR ALL ENTRIES IN it_equi
        WHERE equnr = it_equi-equnr.

      SELECT *
        FROM equz
        INTO TABLE it_equz
        FOR ALL ENTRIES IN it_equi
        WHERE equnr = it_equi-equnr
        AND datbi EQ '99991231'.

      SELECT *
        FROM iloa AS a
        INTO TABLE it_iloa
        FOR ALL ENTRIES IN it_equz
        WHERE a~iloan = it_equz-iloan.
    ENDIF.

    FREE: it_iloa_aux.
    SELECT * FROM iloa INTO TABLE it_iloa_aux
      FOR ALL ENTRIES IN it_fleet_aux
      WHERE anlnr EQ it_fleet_aux-anln1+0(12)
        AND anlun EQ it_fleet_aux-anln2.

    IF it_iloa_aux IS NOT INITIAL.
      SELECT *
        FROM equz
        INTO TABLE it_equz_aux
        FOR ALL ENTRIES IN it_iloa_aux
        WHERE iloan = it_iloa_aux-iloan
        AND datbi EQ '99991231'.
      IF it_equz_aux IS NOT INITIAL.
        SELECT *
          FROM equi
          INTO TABLE it_equi_aux
          FOR ALL ENTRIES IN it_equz_aux
          WHERE equnr = it_equz_aux-equnr.

        SELECT *
        FROM eqkt
        INTO TABLE it_eqkt_aux
        FOR ALL ENTRIES IN it_equi_aux
        WHERE equnr = it_equi_aux-equnr
          AND spras = sy-langu.
      ENDIF.
    ENDIF.
  ENDIF.
* User Story 153788 // MMSILVA - 07.10.2024 - Fim

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MANIPULA_DADOS
*&---------------------------------------------------------------------*
FORM manipula_dados .

  DATA: ls_style TYPE lvc_s_styl,
        vl_cont  TYPE i.



  LOOP AT it_dados_imob INTO wa_dados_imob.

    FREE: wa_dados_imob-cellstyles, wa_cskt, wa_conta.

    vl_cont = sy-tabix.

    READ TABLE it_zaa005 INTO wa_zaa005 WITH KEY anln1 = wa_dados_imob-anln1
                                                 anln2 = wa_dados_imob-anln2
                                                 bukrs = wa_dados_imob-bukrs
                                                 gsber = wa_dados_imob-gsber
                                                 kostl = wa_dados_imob-kostl
                                                 gjahr = p_gjahr-low.

    IF sy-subrc IS INITIAL.

      IF wa_zaa005-zimob_a EQ 0.
        wa_dados_imob-semaf = '@EB@'.
        wa_dados_imob-aprov = '@F1@'.
      ELSEIF wa_zaa005-zimob_a EQ 1.
        wa_dados_imob-semaf = '@0A@'.
        wa_dados_imob-aprov = '@F1@'.
      ELSEIF wa_zaa005-zimob_a EQ 2.
        wa_dados_imob-semaf = '@09@'.
        wa_dados_imob-aprov = '@F1@'.
      ELSEIF wa_zaa005-zimob_a EQ 3.
        wa_dados_imob-semaf = '@08@'.
        wa_dados_imob-aprov = '@F1@'.
      ELSE.
        wa_dados_imob-semaf = '@08@'.
        wa_dados_imob-aprov = '@DF@'.
      ENDIF.

      IF wa_zaa005-zimob_r IS INITIAL.
        wa_dados_imob-avali = '@B8@'.
      ELSE.
        wa_dados_imob-avali = '@B9@'.
      ENDIF.

      wa_dados_imob-zimob_a = wa_zaa005-zimob_a.
      wa_dados_imob-zimob_v = wa_zaa005-zimob_v.
      wa_dados_imob-zimob_p = wa_zaa005-zimob_p.

    ELSE.
*      CONTINUE.
*-CS2019000323 - 02.09.2021 - JT - inicio
*     wa_dados_imob-zimob_v = '0'.
      wa_dados_imob-zimob_v = '2'.
*-CS2019000323 - 02.09.2021 - JT - fim
      wa_dados_imob-semaf = '@EB@'.
      wa_dados_imob-aprov = '@F1@'.
      wa_dados_imob-zimob_p = 'N'.
      wa_dados_imob-avali = '@B8@'.
    ENDIF.

    CLEAR: wa_zaa005.

    READ TABLE it_zaa004_aprov INTO wa_zaa004_aprov WITH KEY bukrs    = wa_dados_imob-bukrs
                                                             gsber    = wa_dados_imob-gsber
                                                             kostl    = wa_dados_imob-kostl
                                                             gjahr    = p_gjahr-low
                                                             nivel_aa = wa_dados_imob-zimob_a + 1.

    IF sy-subrc IS INITIAL.
      wa_dados_imob-usuar_aa = wa_zaa004_aprov-usuar_aa.
    ENDIF.

    CLEAR wa_anla.
    READ TABLE it_anla INTO wa_anla WITH KEY anln1 = wa_dados_imob-anln1.
    IF sy-subrc EQ 0.
      wa_dados_imob-anlkl = wa_anla-anlkl.
    ENDIF.

*-CS2022000978-31.01.2023-#93774-JT-inicio
    TRY.
        wa_cskt  = it_cskt[ kostl = wa_dados_imob-kostl ].
      CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
    ENDTRY.

    TRY.
        wa_conta  = it_conta[ ktogr = wa_anla-ktogr ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

    wa_dados_imob-cskt_ltext = wa_cskt-ltext.
    wa_dados_imob-conta      = wa_conta-saknr.
    wa_dados_imob-desc_conta = wa_conta-txt50.
*-CS2022000978-31.01.2023-#93774-JT-fim

    ls_style-fieldname = 'ANEXO'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO wa_dados_imob-cellstyles.

    ls_style-fieldname = 'AVALI'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO wa_dados_imob-cellstyles.

    ls_style-fieldname = 'TEXTO'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO wa_dados_imob-cellstyles.

*    LS_STYLE-FIELDNAME = 'ZIMOB_P'.
*    LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*    APPEND LS_STYLE TO WA_DADOS_IMOB-CELLSTYLES.

*    LS_STYLE-FIELDNAME = 'ZIMOB_V'.
*    LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*    APPEND LS_STYLE TO WA_DADOS_IMOB-CELLSTYLES.

    MODIFY it_dados_imob FROM wa_dados_imob INDEX vl_cont TRANSPORTING anexo      zimob_v  zimob_a
                                                                       usuar_aa   zimob_p  avali
                                                                       semaf      aprov    cellstyles
                                                                       anlkl
                                                                       cskt_ltext conta desc_conta. "*-CS2022000978-31.01.2023-#93774-JT

    CLEAR: wa_dados_imob, wa_anla.
  ENDLOOP.

  PERFORM atualiza_icones.
  SORT it_dados_imob BY kostl anln1 anln2 ASCENDING.

*** Início - Rubenilson - 15.01.24 - US128402
  IF it_dados_imob IS NOT INITIAL.

    SELECT *
      FROM ankt
      INTO TABLE @DATA(lt_ankl)
      FOR ALL ENTRIES IN @it_dados_imob
      WHERE spras = @sy-langu
        AND anlkl = @it_dados_imob-anlkl.
    IF sy-subrc IS INITIAL.
      SORT lt_ankl BY anlkl.
    ENDIF.

    DATA(lv_gjahr) = p_gjahr-low - 1.

    SELECT *
      FROM anlc
      INTO TABLE @DATA(lt_anlc)
      FOR ALL ENTRIES IN @it_dados_imob
      WHERE bukrs = @it_dados_imob-bukrs
        AND anln1 = @it_dados_imob-anln1
        AND anln2 = @it_dados_imob-anln2
        AND gjahr = @lv_gjahr.
    IF sy-subrc IS INITIAL.

      DELETE lt_anlc WHERE afabe <> '01' AND afabe <> '41'.

      SORT lt_anlc BY bukrs anln1 anln2 afabe.

    ENDIF.

    SELECT *
      FROM zaa005
      INTO TABLE @DATA(lt_zaa005)
      FOR ALL ENTRIES IN @it_dados_imob
      WHERE anln1 = @it_dados_imob-anln1
        AND anln2 = @it_dados_imob-anln2
        AND bukrs = @it_dados_imob-bukrs
        AND gsber = @it_dados_imob-gsber
        AND kostl = @it_dados_imob-kostl
        AND gjahr = @lv_gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_zaa005 BY anln1 anln2 bukrs gsber kostl.
    ENDIF.

  ENDIF.
*** Fim - Rubenilson - 15.01.24 - US128402

* User Story 153788 // MMSILVA - 07.10.2024 - Inicio
  LOOP AT it_dados_imob ASSIGNING FIELD-SYMBOL(<_new_values>).

*** Início - Rubenilson - 15.01.24 - US128402
    READ TABLE lt_ankl ASSIGNING FIELD-SYMBOL(<fs_ankl>)
    WITH KEY anlkl = <_new_values>-anlkl
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <_new_values>-desc_classe = <fs_ankl>-txk50.
    ENDIF.

    READ TABLE lt_anlc ASSIGNING FIELD-SYMBOL(<fs_anlc>)
    WITH KEY bukrs = <_new_values>-bukrs
             anln1 = <_new_values>-anln1
             anln2 = <_new_values>-anln2
             afabe = '01'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <_new_values>-vlr_aquis_brl = <fs_anlc>-kansw.
      <_new_values>-vlr_resid_brl = <fs_anlc>-kansw + <fs_anlc>-knafa + <fs_anlc>-nafag.
    ENDIF.

    READ TABLE lt_anlc ASSIGNING <fs_anlc>
    WITH KEY bukrs = <_new_values>-bukrs
             anln1 = <_new_values>-anln1
             anln2 = <_new_values>-anln2
             afabe = '41'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <_new_values>-vlr_aquis_usd = <fs_anlc>-kansw.
      <_new_values>-vlr_resid_usd = <fs_anlc>-kansw + <fs_anlc>-knafa + <fs_anlc>-nafag.
    ENDIF.

    READ TABLE lt_zaa005 ASSIGNING FIELD-SYMBOL(<fs_za005>)
    WITH KEY anln1 = <_new_values>-anln1
             anln2 = <_new_values>-anln2
             bukrs = <_new_values>-bukrs
             gsber = <_new_values>-gsber
             kostl = <_new_values>-kostl
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <_new_values>-zimob_v2 = <fs_za005>-zimob_v.
    ENDIF.
*** Fim - Rubenilson - 15.01.24 - US128402

*    while <_new_values>-anln1 cp '0*'.
*      <_new_values>-anln1 = <_new_values>-anln1+1.
*    endwhile.
    CLEAR: wa_fleet, wa_iloa, wa_equi, wa_eqkt.
    IF <_new_values>-anln1 IS NOT INITIAL AND <_new_values>-anln2 EQ 0.

      READ TABLE it_fleet INTO wa_fleet WITH KEY zzimobilizado = |{ <_new_values>-anln1 ALPHA = OUT }|.
      IF sy-subrc = 0.
        READ TABLE it_equi INTO wa_equi WITH KEY objnr = wa_fleet-objnr.
        IF sy-subrc = 0.
          <_new_values>-equnr = wa_equi-equnr.

          READ TABLE it_eqkt INTO wa_eqkt WITH KEY equnr = wa_equi-equnr.

          <_new_values>-eqktx = wa_eqkt-eqktx.

          READ TABLE it_equz INTO wa_equz WITH KEY equnr = wa_equi-equnr
                                                   datbi = '99991231'.

          READ TABLE it_iloa INTO wa_iloa WITH KEY iloan = wa_equz-iloan.

          <_new_values>-kostl_iloa = wa_iloa-kostl.
          <_new_values>-swerk = wa_iloa-swerk.

        ENDIF.
      ENDIF.
    ELSE.

      READ TABLE it_iloa_aux INTO wa_iloa WITH KEY anlnr = <_new_values>-anln1
                                                   anlun = <_new_values>-anln2.
      IF sy-subrc = 0.

        <_new_values>-kostl_iloa = wa_iloa-kostl.
        <_new_values>-swerk      = wa_iloa-swerk.

        READ TABLE it_equz_aux INTO wa_equz WITH KEY iloan = wa_iloa-iloan
                                                   datbi = '99991231'.
        IF sy-subrc = 0.
          READ TABLE it_equi_aux INTO wa_equi WITH KEY equnr = wa_equz-equnr.
          IF sy-subrc EQ 0.
            <_new_values>-equnr = wa_equi-equnr.
            READ TABLE it_eqkt_aux INTO wa_eqkt WITH KEY equnr = wa_equi-equnr.
            IF sy-subrc EQ 0.
              <_new_values>-eqktx = wa_eqkt-eqktx.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
* User Story 153788 // MMSILVA - 07.10.2024 - Fim

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ICONES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_icones .

  DATA: wl_name    TYPE thead-tdname,
        vl_obj_key TYPE sibflporb-instid,
        vl_lines   TYPE i,
        anexos     TYPE TABLE OF bdn_con.

  LOOP AT it_dados_imob INTO wa_dados_imob.

    "Atualiza Ícone de Texto
    REFRESH: it_texto.
    CLEAR: wl_name.
    CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ZAA1'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZAA02'
      TABLES
        lines                   = it_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
      wa_dados_imob-texto = '@6X@'.
    ELSE.
      wa_dados_imob-texto = '@6Y@'.
    ENDIF.

    CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
                wa_dados_imob-gsber wa_dados_imob-kostl INTO vl_obj_key.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = 'ZAA11'
        objkey             = vl_obj_key
        client             = sy-mandt
      TABLES
        gos_connections    = anexos
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    DESCRIBE TABLE anexos LINES vl_lines.

    IF vl_lines NE 0.
      wa_dados_imob-anexo = '@FM@'.
    ELSE.
      wa_dados_imob-anexo = '@02@'.
    ENDIF.

    MODIFY it_dados_imob FROM wa_dados_imob TRANSPORTING texto anexo.
    CLEAR: it_texto, vl_obj_key, vl_lines, anexos.
  ENDLOOP.



ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILTRA_RESULTADO
*&---------------------------------------------------------------------*
FORM filtra_resultado .

  IF ( it_dados_imob[] IS NOT INITIAL ).

    SELECT bukrs, anln1, anln2, deakt
      FROM anla
      INTO TABLE @DATA(tl_anla)
      FOR ALL ENTRIES IN @it_dados_imob
      WHERE bukrs EQ @it_dados_imob-bukrs
        AND anln1 EQ @it_dados_imob-anln1
        AND anln2 EQ @it_dados_imob-anln2.

    CHECK ( tl_anla[] IS NOT INITIAL ).

    LOOP AT tl_anla INTO DATA(wl_anla) WHERE deakt IS NOT INITIAL.

      DELETE it_dados_imob[] WHERE anln1 EQ wl_anla-anln1
                             AND anln2 EQ wl_anla-anln2
                             AND bukrs EQ wl_anla-bukrs.



    ENDLOOP.

    DELETE it_dados_imob[] WHERE aktiv IS INITIAL.

  ENDIF.

ENDFORM.

INCLUDE zaa11_0100.

INCLUDE zaa11_0200.

INCLUDE zaa11_0300.
