*&---------------------------------------------------------------------*
*& Report  ZLESR0143
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Monitor de Recebimentos de Frete Frota Própria          *
* Transação..: ZSDT0168                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*-----------------------------------------------------3900-------------*
REPORT zlesr0143.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: icon, zlest0194, bkpf.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_cabec.

         INCLUDE TYPE zcte_identifica.
TYPES:
         chave_cte_sub    TYPE zlest0194-chave_cte_sub,
         docnum_sub       TYPE zlest0194-docnum_sub,
         valor_prestacao  TYPE zlest0194-valor_prestacao,
         nr_cte_sub       TYPE zlest0194-nr_cte_sub,
         serie_cte_sub    TYPE zlest0194-serie_cte_sub,
         zvlr_liq_receber TYPE zlest0194-zvlr_liq_receber,
         ds_prod_pred     TYPE zlest0194-ds_prod_pred,
         model            TYPE j_1bnfdoc-model,
         taxsit           TYPE j_1bnflin-taxsit,
         stcd1            TYPE lfa1-stcd1,
         stcd3            TYPE lfa1-stcd3,
         name1            TYPE lfa1-name1.
TYPES:  END OF ty_cabec.

TYPES: BEGIN OF ty_faturar_cab,
         cod_cliente  LIKE zlest0194-kunnr_ov,
         nome_cliente TYPE lfa1-name1,
       END OF ty_faturar_cab.

TYPES: BEGIN OF ty_faturar_cab_fn,
         nr_fatura TYPE zlest0194-nr_fatura,
         data_vcto TYPE zlest0194-data_vencimento,
       END OF ty_faturar_cab_fn.

TYPES: BEGIN OF ty_faturar_cab_fg,
         nr_fatura TYPE zlest0194-nr_fatura,
         data_vcto TYPE zlest0194-data_vencimento,
       END OF ty_faturar_cab_fg.

TYPES: BEGIN OF ty_faturar_fn,
*         SELECIONAR       TYPE ZDE_SELECIONAR,
         email            TYPE icon-id,
         nr_fatura        TYPE zlest0194-nr_fatura,
         item_fatura      TYPE zlest0194-item_fatura,
         bukrs_ov         TYPE zlest0194-bukrs_ov,
         branch_ov        TYPE zlest0194-branch_ov,
         kunnr_ov         TYPE zlest0194-kunnr_ov,
         docnum_sub       TYPE zlest0194-docnum_sub,
         nr_cte_sub       TYPE zlest0194-nr_cte_sub,
         serie_cte_sub    TYPE zlest0194-serie_cte_sub,
         dt_mov_ov        TYPE zlest0194-dt_mov_ov,
         qt_carga_cte     TYPE zlest0194-qt_carga_cte,
         qt_descarga_cte  TYPE zlest0194-qt_descarga_cte,
         id_ctr           TYPE zlest0194-id_ctr,
         valor_prestacao  TYPE zlest0194-valor_prestacao,
         vl_total_merc    TYPE zlest0194-vl_total_merc,
         zpeso_diferenca  TYPE zlest0194-zpeso_diferenca,
         zquebra          TYPE zlest0194-zquebra,
         zvlr_quebra      TYPE zlest0194-zvlr_quebra,
         zperda           TYPE zlest0194-zperda,
         zvlr_perda       TYPE zlest0194-zvlr_perda,
         zvlr_liq_receber TYPE zlest0194-zvlr_liq_receber,
         data_vencimento  TYPE zlest0194-data_vencimento,
         chave_xml_cte    TYPE zlest0194-chave_xml_cte,
         obj_key_perda    TYPE zlest0194-obj_key_perda,
         ano_key_perda    TYPE bkpf-gjahr,
         nr_doc_contabil  TYPE bseg-belnr,
         ano_doc_contabil TYPE bkpf-gjahr,
         n55_chave_acesso TYPE zib_cte_dist_n55-n55_chave_acesso, "Equalização ECC X HANA #115197   - SMC
       END OF ty_faturar_fn.


TYPES: BEGIN OF ty_faturar_fg,
         status(20)       TYPE c,
         email            TYPE icon-id,
         nr_fatura        TYPE zlest0194-nr_fatura,
         item_fatura      TYPE zlest0194-item_fatura,
         bukrs_ov         TYPE zlest0194-bukrs_ov,
         branch_ov        TYPE zlest0194-branch_ov,
         kunnr_ov         TYPE zlest0194-kunnr_ov,
         docnum_sub       TYPE zlest0194-docnum_sub,
         nr_cte_sub       TYPE zlest0194-nr_cte_sub,
         serie_cte_sub    TYPE zlest0194-serie_cte_sub,
         dt_mov_ov        TYPE zlest0194-dt_mov_ov,
         qt_carga_cte     TYPE zlest0194-qt_carga_cte,
         qt_descarga_cte  TYPE zlest0194-qt_descarga_cte,
         id_ctr           TYPE zlest0194-id_ctr,
         valor_prestacao  TYPE zlest0194-valor_prestacao,
         vl_total_merc    TYPE zlest0194-vl_total_merc,
         zpeso_diferenca  TYPE zlest0194-zpeso_diferenca,
         zquebra          TYPE zlest0194-zquebra,
         zvlr_quebra      TYPE zlest0194-zvlr_quebra,
         zperda           TYPE zlest0194-zperda,
         zvlr_perda       TYPE zlest0194-zvlr_perda,
         zvlr_liq_receber TYPE zlest0194-zvlr_liq_receber,
         data_vencimento  TYPE zlest0194-data_vencimento,
         chave_xml_cte    TYPE zlest0194-chave_xml_cte,
         obj_key_perda    TYPE zlest0194-obj_key_perda,
         ano_key_perda    TYPE bkpf-gjahr,
         nr_doc_contabil  TYPE bseg-belnr,
         ano_doc_contabil TYPE bkpf-gjahr,
         fina_dat         TYPE zlest0194-fina_dat,
         fina_mot         TYPE zlest0194-fina_mot,
         fina_stat        TYPE zlest0194-fina_stat,
         fina_user        TYPE zlest0194-fina_user,
         n55_chave_acesso TYPE zib_cte_dist_n55-n55_chave_acesso,  "Equalização ECC X HANA #115197   - SMC
       END OF ty_faturar_fg.

TYPES: BEGIN OF ty_report,
         status(20)       TYPE c,
         email            TYPE icon-id,
         nr_fatura        TYPE zlest0194-nr_fatura,
         item_fatura      TYPE zlest0194-item_fatura,
         bukrs_ov         TYPE zlest0194-bukrs_ov,
         branch_ov        TYPE zlest0194-branch_ov,
         ds_prod_pred     TYPE zlest0194-ds_prod_pred,
         kunnr_ov         TYPE zlest0194-kunnr_ov,
         name1            TYPE kna1-name1,
         inicio_muni      TYPE zcte_identifica-nmunini,
         termino_muni     TYPE zcte_identifica-nmunfim,
         tipo_cte         TYPE zcte_identifica-tpcte,
         docnum_sub       TYPE zlest0194-docnum_sub,
         nr_cte_sub       TYPE zlest0194-nr_cte_sub,
         serie_cte_sub    TYPE zlest0194-serie_cte_sub,
         dt_mov_ov        TYPE zlest0194-dt_mov_ov,
         qt_carga_cte     TYPE zlest0194-qt_carga_cte,
         qt_descarga_cte  TYPE zlest0194-qt_descarga_cte,
         id_ctr           TYPE zlest0194-id_ctr,
         valor_prestacao  TYPE zlest0194-valor_prestacao,
         vl_total_merc    TYPE zlest0194-vl_total_merc,
         zpeso_diferenca  TYPE zlest0194-zpeso_diferenca,
         zquebra          TYPE zlest0194-zquebra,
         zvlr_quebra      TYPE zlest0194-zvlr_quebra,
         zperda           TYPE zlest0194-zperda,
         zvlr_perda       TYPE zlest0194-zvlr_perda,
         zvlr_liq_receber TYPE zlest0194-zvlr_liq_receber,
         data_vencimento  TYPE zlest0194-data_vencimento,
         chave_xml_cte    TYPE zlest0194-chave_xml_cte,
         obj_key_perda    TYPE zlest0194-obj_key_perda,
         ano_key_perda    TYPE bkpf-gjahr,
         nr_doc_contabil  TYPE bseg-belnr,
         ano_doc_contabil TYPE bkpf-gjahr,
         fina_mot         TYPE zlest0194-fina_mot,
         n55_chave_acesso TYPE zib_cte_dist_n55-n55_chave_acesso, "Equalização ECC X HANA #115197   - SMC
       END OF ty_report.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1.

TYPES: BEGIN OF ty_nota,
         chave       TYPE zcte_info_nota-chave,
         vl_produtos TYPE zcte_info_nota-vl_produtos,
         docnum_nf   TYPE zcte_info_nota-docnum_nf,
       END OF ty_nota.

TYPES: BEGIN OF ty_trans_cte,
         chave         TYPE zcte_doc_ant-c57_chave_acesso,
         emit_ant_nome TYPE zcte_doc_ant-emit_ant_nome,
         emit_ant_cnpj TYPE zcte_doc_ant-emit_ant_cnpj,
       END OF ty_trans_cte.

TYPES: BEGIN OF ty_modal_veiculos,
         pc_veiculo   TYPE zcte_trans-pc_veiculo,
         proprietario TYPE zcte_trans-proprietario,
         prop_nome    TYPE zcte_trans-prop_nome,
         prop_rntrc   TYPE zcte_trans-prop_rntrc,
         cd_renavam   TYPE zcte_trans-cd_renavam,
         tp_veiculo   TYPE zcte_trans-tp_veiculo,
       END OF ty_modal_veiculos.

TYPES: BEGIN OF ty_modal_moto,
         lifnr TYPE zcte_motorista-lifnr,
         xnome TYPE zcte_motorista-xnome,
         cpf   TYPE zcte_motorista-cpf,
       END OF ty_modal_moto.

TYPES: BEGIN OF ty_bloq.
TYPES:   cd_chave_cte TYPE zde_chave_doc_e.
TYPES: END OF ty_bloq.

TYPES: BEGIN OF ty_bkpf,
         belnr TYPE bkpf-belnr,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey,
       END OF ty_bkpf.

TYPES: BEGIN OF y_itens_contab,
         obj_key       TYPE awkey,
         seqitem(6)    TYPE c,
         bschl         TYPE bschl,
         gsber         TYPE  gsber,
         bukrs         TYPE bukrs,
         interface     TYPE numc2,
         bktxt         TYPE bktxt,
         bldat(10)     TYPE c,
         budat(10)     TYPE c,
         gjahr         TYPE gjahr,
         monat         TYPE monat,
         blart         TYPE blart,
         xblnr         TYPE xblnr,
         hkont         TYPE hkont,
         wrbtr         TYPE tslxx12,
         waers         TYPE waers,
         zlsch         TYPE schzw_bseg,
         sgtxt         TYPE sgtxt,
         bupla         TYPE bupla,
         waers_i       TYPE waers,
         tarifa        TYPE hslxx12,
         rg_atualizado TYPE c,
         matnr         TYPE matnr,
         quantity      TYPE menge_d,
         tknum         TYPE tknum,
         zfbdt         TYPE dzfbdt,
       END OF y_itens_contab.

TYPES: ty_nrfatura  TYPE RANGE OF zlest0194-nr_fatura.
TYPES: ty_data_vcto TYPE RANGE OF zlest0194-data_vencimento.

TYPES: BEGIN OF ty_zlest0194.
         INCLUDE STRUCTURE zlest0194.
TYPES:   awkey            TYPE bkpf-awkey,
*         ano_key_perda    TYPE bkpf-gjahr,
         ano_doc_contabil TYPE bkpf-gjahr,
       END OF ty_zlest0194.

TYPES: BEGIN OF y_custo,
         fknum TYPE vfkp-fknum,
         fkpos TYPE vfkp-fkpos,
         netwr TYPE vfkp-netwr,
         knumv TYPE vfkp-knumv,
         vbeln TYPE vfsi-vbeln,
         posnr TYPE vfsi-posnr,
         netpr TYPE vfsi-netpr,
       END OF y_custo.

TYPES: BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.
*----------------------------------------------------------------------*
* Estruturas internas -------------------------------------------------*
*----------------------------------------------------------------------*
DATA: t_faturar         TYPE TABLE OF ty_zlest0194.
DATA: t_saida_fn        TYPE TABLE OF ty_faturar_fn.
DATA: t_saida_fg        TYPE TABLE OF ty_faturar_fg.
DATA: w_saida_fg        TYPE ty_faturar_fg.
DATA: t_contabil TYPE TABLE OF y_itens_contab.

" Tabelas e Workarea BAPI_ACC
DATA: gd_documentheader LIKE bapiache09,
      wa_returnobj      LIKE zfie_returnobj,
      wa_accountgl      LIKE bapiacgl09,
      wa_receivable     LIKE bapiacar09,
      wa_payable        LIKE bapiacap09,
      wa_currencyamount LIKE bapiaccr09,
      wa_criteria       LIKE bapiacwt09,
      wa_extension1     LIKE bapiacextc,
      wa_bapiret        LIKE bapiret2.

DATA: it_accountgl      LIKE STANDARD TABLE OF wa_accountgl,
      it_receivable     LIKE STANDARD TABLE OF wa_receivable,
      it_payable        LIKE STANDARD TABLE OF wa_payable,
      it_currencyamount LIKE STANDARD TABLE OF wa_currencyamount,
      it_bapiret        LIKE STANDARD TABLE OF wa_bapiret,
      it_accountwt      LIKE STANDARD TABLE OF wa_criteria,
      it_criteria       LIKE bapiackec9 OCCURS 0 WITH HEADER LINE,
      it_accounttax     LIKE bapiactx09 OCCURS 0 WITH HEADER LINE,
      it_extension1     LIKE STANDARD TABLE OF bapiacextc,
      it_dta            TYPE TABLE OF bdcdata.

DATA: editor    TYPE REF TO cl_gui_textedit,
      c_editor  TYPE REF TO cl_gui_custom_container,
      txtopen   TYPE c,
      it_editor TYPE TABLE OF ty_editor,
      wa_editor TYPE ty_editor,
      p_respo   TYPE c.


DATA: wg_faturar TYPE zlest0194.
DATA: wg_fat_cab TYPE ty_faturar_cab.
DATA: wg_fat_cab_fn TYPE ty_faturar_cab_fn.
DATA: wg_fat_cab_fg TYPE ty_faturar_cab_fg.
DATA: w_chave_xml TYPE zlest0194-chave_xml_cte.
DATA: wa_toolbar TYPE stb_button,
      wa_layout  TYPE lvc_s_layo.

* Layout Telas
DATA:
  gs_cabec          TYPE ty_cabec,
  gs_parceiros      TYPE zlest0194,
  gs_nota           TYPE ty_nota,
  gs_trans_cte      TYPE ty_trans_cte,
  gs_dados_sap      TYPE zlest0194,
  gs_modal_veiculos TYPE ty_modal_veiculos,
  gs_modal_moto     TYPE ty_modal_moto,
  gs_layout         TYPE slis_layout_alv.

DATA: email_string TYPE string,
      email        TYPE string.
DATA  container_email TYPE REF TO cl_gui_custom_container.

* Gerenciador Arquivos
DATA: "MANAGER TYPE REF TO CL_GOS_MANAGER,
  obj     TYPE borident,
  ip_mode TYPE sgs_rwmod,
  objtype TYPE borident-objtype VALUE 'ZLES0180'.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
* Grupo de constantes que informa a aba acionada
CONSTANTS:
  BEGIN OF c_abas,
    tab1 LIKE sy-ucomm VALUE 'ABAS_F1',
    tab2 LIKE sy-ucomm VALUE 'ABAS_F2',
    tab3 LIKE sy-ucomm VALUE 'ABAS_F3',
    tab4 LIKE sy-ucomm VALUE 'ABAS_F4',
    tab5 LIKE sy-ucomm VALUE 'ABAS_F5',
    tab6 LIKE sy-ucomm VALUE 'ABAS_F6',
  END OF c_abas.

CONSTANTS:
  BEGIN OF gc_icones,
    flag_green(20)      TYPE c VALUE '@5Y@',
    flag_incomplete(20) TYPE c VALUE '@F1@',
    flag_complete(20)   TYPE c VALUE '@DF@',
    email(20)           TYPE c VALUE '@1S@',
  END OF gc_icones.

* Declaração da Tabstrip .
CONTROLS: abas        TYPE TABSTRIP.

CONTROLS : tc0202 TYPE TABLEVIEW USING SCREEN 0202,
           tc0203 TYPE TABLEVIEW USING SCREEN 0203,
           tc0205 TYPE TABLEVIEW USING SCREEN 0205,
           tc0206 TYPE TABLEVIEW USING SCREEN 0206.

DATA:
  BEGIN OF g_abas,
    subscreen   LIKE sy-dynnr,
    prog        LIKE sy-repid VALUE 'ZLESR0143',
    pressed_tab LIKE sy-ucomm VALUE c_abas-tab1,
  END OF g_abas.

*****************************************************************
* Declaração da Tabstrip de Faturar .
CONTROLS: tab_fatura TYPE TABSTRIP.

* Grupo de ABAS botão FATURAR
CONSTANTS:
  BEGIN OF c_abas_fat,
    tab1 LIKE sy-ucomm VALUE 'ABAS_FN',
    tab2 LIKE sy-ucomm VALUE 'ABAS_FG',
  END OF c_abas_fat.

DATA:
  BEGIN OF g_abas_fat,
    subscreen   TYPE scrfname,
    screen      LIKE sy-dynnr,
    prog        LIKE sy-repid VALUE 'ZLESR0143',
    pressed_tab LIKE sy-ucomm VALUE '', "C_ABAS_FAT-TAB1,
  END OF g_abas_fat.

DATA: v_subscreen TYPE c LENGTH 6.
*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: v_ucomm     TYPE sy-ucomm,
      vl_perdax   TYPE wert,
      vl_frete    TYPE wert,
      vl_merc     TYPE wert,
      vl_descarga TYPE wert.

* Comando do User
DATA:  ok_code LIKE sy-ucomm.

*---------------------------------------------------------------------
* Objetos
*---------------------------------------------------------------------
DATA: manager TYPE REF TO cl_gos_manager.
DATA: obj_cte TYPE REF TO zcl_cte_dist_g.

*---------------------------------------------------------------------
* Declaração de Range
*---------------------------------------------------------------------
DATA: r_nrfatura  TYPE ty_nrfatura WITH HEADER LINE,
      r_data_vcto TYPE ty_data_vcto WITH HEADER LINE.

*---------------------------------------------------------------------
* Classes locais (Definição)
*---------------------------------------------------------------------
DATA: ctl_alv  TYPE REF TO cl_gui_alv_grid.

CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.

    METHODS : handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.

*...Barra de Ferramentas
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

*...User Command
    METHODS handle_command_grid
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

*--------------------------------------------------------------------
*Métodos da Opção Fatura
*--------------------------------------------------------------------
    METHODS : handle_hotspot_click_fat_fg
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.

    METHODS : handle_hotspot_click_fat_fn
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.

*...Barra de Ferramentas
    METHODS handle_toolbar_nova_fatura
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_toolbar_fatura_geradas
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_command_grid_fatura
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

*    METHODS CHECK_CHANGED_DATA EXPORTING E_VALID.

ENDCLASS. "LCL_GRID_EVENT DEFINITION

*---------------------------------------------------------------------
*
* Classes locais (Implementação)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event IMPLEMENTATION.

*-----Logic to handle the HOTSPOT click
  METHOD handle_hotspot_click.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.
  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_toolbar.
*...Barra de Ferramentas
    PERFORM f_toolbar_grid CHANGING e_object.
  ENDMETHOD. "handle_toolba

  METHOD handle_command_grid.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command USING e_ucomm.
  ENDMETHOD. "handle_command_grid

*--------------------------------------------------------------------
*Métodos da Opção Fatura
*--------------------------------------------------------------------
*-----Logic to handle the HOTSPOT click
  METHOD handle_hotspot_click_fat_fg.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click_fat_fg USING e_row_id-index e_column_id.
  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

*-----Logic to handle the HOTSPOT click
  METHOD handle_hotspot_click_fat_fn.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click_fat_fg USING e_row_id-index e_column_id.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_toolbar_nova_fatura.
    PERFORM zf_elimina_botoes_header  USING e_object.
*    PERFORM ZF_ADICIONA_BOTOES_HEADER USING E_OBJECT.
  ENDMETHOD. "handle_toolba

  METHOD handle_toolbar_fatura_geradas.
    PERFORM zf_elimina_botoes_header  USING e_object.
    PERFORM zf_adiciona_botoes_header USING e_object.
  ENDMETHOD. "handle_toolba

  METHOD handle_command_grid_fatura.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command_fatura USING e_ucomm.
  ENDMETHOD. "handle_command_grid

ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION

DATA: lcl_alv           TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv TYPE REF TO cl_gui_custom_container,
      lcl_event         TYPE REF TO lcl_grid_event.

* Dados da Nova Fatura
DATA: lcl_alv_208      TYPE REF TO cl_gui_alv_grid,
      lcl_cont_alv_208 TYPE REF TO cl_gui_custom_container,
      lcl_event_208    TYPE REF TO lcl_grid_event.

* Dados da Fatura Gerada
DATA: lcl_alv_209      TYPE REF TO cl_gui_alv_grid,
      lcl_cont_alv_209 TYPE REF TO cl_gui_custom_container,
      lcl_event_209    TYPE REF TO lcl_grid_event.

DATA: v_descr TYPE c LENGTH 50.

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA: t_zlest0194       TYPE TABLE OF ty_zlest0194,
      t_kna1            TYPE TABLE OF ty_kna1,
      t_zcte_identifica TYPE TABLE OF zcte_identifica,
      t_report          TYPE TABLE OF ty_report, "ZSDS026,
      t_fieldcat        TYPE lvc_t_fcat,
      t_nota            TYPE TABLE OF ty_nota,
      t_trans_cte       TYPE TABLE OF ty_trans_cte,
      t_modal_veiculos  TYPE TABLE OF ty_modal_veiculos,
      t_modal_moto      TYPE TABLE OF ty_modal_moto,
      t_bkpf            TYPE TABLE OF ty_bkpf.

FIELD-SYMBOLS:  <fs_fcat> LIKE LINE OF t_fieldcat.
*----------------------------------------------------------------------*
* Tela de seleção -----------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_1 RADIOBUTTON GROUP rad1.
    SELECTION-SCREEN COMMENT 03(10) TEXT-004.
    PARAMETERS: r_2 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 16(10) TEXT-005.
    PARAMETERS: r_3 RADIOBUTTON GROUP rad1.
    SELECTION-SCREEN COMMENT 29(10) TEXT-006.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_numr  FOR zlest0194-nr_cte_sub,
                  s_data  FOR zlest0194-dt_mov_ov,
                  s_chave FOR zlest0194-chave_cte_sub.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs   FOR zlest0194-bukrs_ov OBLIGATORY
                            NO INTERVALS NO-EXTENSION,
                  s_branch  FOR zlest0194-branch_ov,
                  s_kunnr   FOR zlest0194-kunnr_ov,
                  s_cnpj    FOR zlest0194-kunnr_cnpj.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-038.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (50) p_param   USER-COMMAND parametros.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'PARAMETROS'.
      PERFORM zf_call_transaction_parametros.
    WHEN OTHERS.
  ENDCASE.
*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
INITIALIZATION.

  CLEAR v_descr.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = 'ICON_TABLE_SETTINGS'
      info   = 'Contabilização'(037)
    IMPORTING
      result = v_descr.

  CONCATENATE v_descr 'Contabilização'(037) INTO v_descr.

  p_param = v_descr.

START-OF-SELECTION.

  PERFORM f_processa.

  IF t_report[] IS NOT INITIAL.
    CALL SCREEN 0101. "ALV Inicial
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_COMMAND
*&---------------------------------------------------------------------*
FORM f_command  USING  e_ucomm TYPE sy-ucomm.

  DATA: vl_erro TYPE c.

  CASE v_ucomm.
    WHEN  'ATUALIZAR'.

    WHEN  'DACTE'.
      PERFORM f_dacte.
    WHEN  'CTE'.
      PERFORM f_cte.
    WHEN  'ANEXO'.
      PERFORM f_anexo.
    WHEN  'FATURAR'.
      PERFORM f_faturar.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TOOLBAR_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM f_toolbar_grid CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.

  CLEAR wa_toolbar.
  MOVE: 'MARCA' TO wa_toolbar-function,
  icon_select_all TO wa_toolbar-icon,
*  TEXT-008 TO WA_TOOLBAR-TEXT,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'DESMARCA' TO wa_toolbar-function ,
  icon_deselect_all TO wa_toolbar-icon,
*  TEXT-009 TO WA_TOOLBAR-TEXT ,
  space TO wa_toolbar-disabled .
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'DACTE' TO wa_toolbar-function ,
  icon_print TO wa_toolbar-icon ,
  TEXT-008 TO wa_toolbar-text ,
  space TO wa_toolbar-disabled .
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'CTE' TO wa_toolbar-function ,
  icon_display TO wa_toolbar-icon ,
  TEXT-009 TO wa_toolbar-text ,
  space TO wa_toolbar-disabled .
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'ANEXO' TO wa_toolbar-function ,
  icon_workflow_inbox TO wa_toolbar-icon ,
  'Fatura PDF'(010) TO wa_toolbar-text.
*  abap_true TO wa_toolbar-disabled .
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'FATURAR' TO wa_toolbar-function ,
  icon_set_state TO wa_toolbar-icon ,
  TEXT-011 TO wa_toolbar-text ,
  space TO wa_toolbar-disabled .
  APPEND wa_toolbar TO p_object->mt_toolbar.

*    elimina itens desnecessarios da barra do container
  DELETE p_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
*                                   OR function = '&MB_VARIANT'
                                 OR function = '&MB_EXPORT'
                                 OR function = '&PRINT_BACK'.
  "OR function = '&MB_SUM'
  "OR function = '&MB_SUBTOT'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TODOS
*&---------------------------------------------------------------------*
FORM f_todos .

  SELECT *
    FROM zlest0194
    INTO TABLE t_zlest0194
   WHERE nr_cte_sub    IN s_numr
     AND dt_mov_ov     IN s_data
     AND chave_cte_sub IN s_chave
     AND bukrs_ov      IN s_bukrs
     AND branch_ov     IN s_branch
     AND kunnr_ov      IN s_kunnr
     AND kunnr_cnpj    IN s_cnpj.

  "Elimina registros com documento de subcontratação incompleto
  DELETE t_zlest0194 WHERE docnum_sub IS INITIAL.

  IF t_zlest0194[] IS INITIAL.
    MESSAGE s000(z_les) WITH 'Dados não encontrados'(014) DISPLAY LIKE 'E'.
  ENDIF.

  IF t_zlest0194[] IS NOT INITIAL.

    REFRESH: t_kna1.
    SELECT kunnr name1 FROM kna1
     INTO TABLE t_kna1
    FOR ALL ENTRIES IN t_zlest0194
    WHERE kunnr = t_zlest0194-kunnr_ov.

    "Verifica se o documento subontratado foi aprovado
    SELECT docnum FROM j_1bnfe_active
      INTO TABLE @DATA(t_active)
      FOR ALL ENTRIES IN @t_zlest0194
         WHERE docnum = @t_zlest0194-docnum_sub
*** Stefanini - IR261654 - 09/10/2025 - LAZAROSR - Início de Alteração
*           AND code   = 100.
            AND code = '100'.
*** Stefanini - IR261654 - 09/10/2025 - LAZAROSR - Fim de Alteração

    IF t_active[] IS NOT INITIAL.
*      SELECT *
*        FROM zcte_identifica
*        INTO TABLE t_zcte_identifica
*         FOR ALL ENTRIES IN t_zlest0194
*       WHERE docnum = t_zlest0194-docnum_sub.

      SELECT *
        FROM zcte_identifica
        INTO TABLE t_zcte_identifica
         FOR ALL ENTRIES IN t_active
       WHERE docnum = t_active-docnum.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PENDENTES
*&---------------------------------------------------------------------*
FORM f_pendentes .

  SELECT *
    FROM zlest0194
    INTO TABLE t_zlest0194
    WHERE nr_cte_sub    IN s_numr
      AND dt_mov_ov     IN s_data
      AND chave_cte_sub IN s_chave
      AND bukrs_ov      IN s_bukrs
      AND branch_ov     IN s_branch
      AND kunnr_ov      IN s_kunnr
      AND kunnr_cnpj    IN s_cnpj
      AND augbl         EQ space
      AND obj_key_perda = space.

  "Elimina registros com documento de subcontratação incompleto
  DELETE t_zlest0194 WHERE docnum_sub IS INITIAL OR fina_stat = 'X'.

  IF t_zlest0194[] IS INITIAL.
    MESSAGE s000(z_les) WITH  'Dados não encontrados'(014) DISPLAY LIKE 'E'.
  ENDIF.

  IF t_zlest0194[] IS NOT INITIAL.

    REFRESH: t_kna1.
    SELECT kunnr name1 FROM kna1
     INTO TABLE t_kna1
    FOR ALL ENTRIES IN t_zlest0194
    WHERE kunnr = t_zlest0194-kunnr_ov.

    "Verifica se o documento subontratado foi aprovado
    SELECT docnum FROM j_1bnfe_active
      INTO TABLE @DATA(t_active)
      FOR ALL ENTRIES IN @t_zlest0194
         WHERE docnum = @t_zlest0194-docnum_sub
*** Stefanini - IR261654 - 09/10/2025 - LAZAROSR - Início de Alteração
*           AND code   = 100.
            AND code = '100'.
*** Stefanini - IR261654 - 09/10/2025 - LAZAROSR - Fim de Alteração


    IF t_active[] IS NOT INITIAL.
*      SELECT *
*        FROM zcte_identifica
*        INTO TABLE t_zcte_identifica
*         FOR ALL ENTRIES IN t_zlest0194
*       WHERE docnum = t_zlest0194-docnum_sub.

      SELECT *
        FROM zcte_identifica
        INTO TABLE t_zcte_identifica
         FOR ALL ENTRIES IN t_active
       WHERE docnum = t_active-docnum.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FINALIZADOS
*&---------------------------------------------------------------------*
FORM f_finalizados .

  DATA: r_obj_key_perda TYPE RANGE OF zlest0194-obj_key_perda.

  SELECT *
    FROM zlest0194
    INTO TABLE t_zlest0194
    WHERE nr_cte_sub    IN s_numr
      AND dt_mov_ov     IN s_data
      AND chave_cte_sub IN s_chave
      AND bukrs_ov      IN s_bukrs
      AND branch_ov     IN s_branch
      AND kunnr_ov      IN s_kunnr
      AND kunnr_cnpj    IN s_cnpj
*     AND AUGBL         NE SPACE
      AND obj_key_perda <> space
      OR fina_stat = abap_true.

  "Elimina registros com documento de subcontratação incompleto
  DELETE t_zlest0194 WHERE docnum_sub IS INITIAL.

  IF t_zlest0194[] IS INITIAL.
    MESSAGE s000(z_les) WITH 'Dados não encontrados'(014)  DISPLAY LIKE 'E'.
  ENDIF.

  IF t_zlest0194[] IS NOT INITIAL.

    REFRESH: t_kna1.
    SELECT kunnr name1 FROM kna1
     INTO TABLE t_kna1
    FOR ALL ENTRIES IN t_zlest0194
    WHERE kunnr = t_zlest0194-kunnr_ov.

    "Verifica se o documento subontratado foi aprovado
    SELECT docnum FROM j_1bnfe_active
      INTO TABLE @DATA(t_active)
      FOR ALL ENTRIES IN @t_zlest0194
         WHERE docnum = @t_zlest0194-docnum_sub
*** Stefanini - IR261654 - 09/10/2025 - LAZAROSR - Início de Alteração
*           AND code   = 100.
            AND code = '100'.
*** Stefanini - IR261654 - 09/10/2025 - LAZAROSR - Fim de Alteração

    IF t_active[] IS NOT INITIAL.
*      SELECT *
*        FROM zcte_identifica
*        INTO TABLE t_zcte_identifica
*         FOR ALL ENTRIES IN t_zlest0194
*       WHERE docnum = t_zlest0194-docnum_sub.

      SELECT *
        FROM zcte_identifica
        INTO TABLE t_zcte_identifica
         FOR ALL ENTRIES IN t_active
       WHERE docnum = t_active-docnum.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA
*&---------------------------------------------------------------------*
FORM f_processa .

  DATA: l_awkey TYPE bkpf-awkey.

  IF     r_1 = abap_true.
    PERFORM f_todos.
  ELSEIF r_2 = abap_true.
    PERFORM f_pendentes.
  ELSEIF r_3 = abap_true.
    PERFORM f_finalizados.
  ENDIF.

*&---------------------------------------------------------------------*
* Ajusta campo AWKEY
*&---------------------------------------------------------------------*
  LOOP AT t_faturar ASSIGNING FIELD-SYMBOL(<fs_awkey>).

    UNPACK <fs_awkey>-fat_sub TO <fs_awkey>-fat_sub.
    <fs_awkey>-awkey = <fs_awkey>-fat_sub.

    <fs_awkey>-obj_key_perda    = <fs_awkey>-obj_key_perda(10).
    <fs_awkey>-ano_key_perda    = <fs_awkey>-obj_key_perda+10(4).

  ENDLOOP.

* Cabeçalho do documento contábil
*  IF t_zlest0194[] IS NOT INITIAL.

  DATA(t_zlest0194_tmp) = t_zlest0194.
  DELETE t_zlest0194_tmp WHERE awkey = space.

  IF t_zlest0194_tmp[] IS NOT INITIAL.

    SELECT belnr bukrs gjahr awkey
      FROM bkpf
      INTO TABLE t_bkpf
      FOR ALL ENTRIES IN t_zlest0194_tmp
      WHERE bukrs = t_zlest0194_tmp-bukrs_ov
        AND gjahr = t_zlest0194_tmp-gjahr_vf
        AND awkey = t_zlest0194_tmp-awkey.

  ENDIF.

*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*"Equalização ECC X HANA #115197   - SMC
  SELECT *
    FROM zib_cte_dist_n55
    INTO TABLE @DATA(tl_zcte_n55)
    FOR ALL ENTRIES IN @t_zlest0194
    WHERE cd_chave_cte = @t_zlest0194-chave_xml_cte.


*&---------------------------------------------------------------------*"Equalização ECC X HANA #115197   - SMC

  REFRESH: t_report.
  LOOP AT t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>).

    READ TABLE t_zcte_identifica ASSIGNING FIELD-SYMBOL(<fs_zcte_identifica>)
                                                   WITH KEY docnum = <fs_zlest0194>-docnum_sub.
    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_report ASSIGNING FIELD-SYMBOL(<fs_report>).
      MOVE-CORRESPONDING <fs_zlest0194> TO <fs_report>.
      <fs_report>-inicio_muni  = <fs_zcte_identifica>-nmunini.
      <fs_report>-termino_muni = <fs_zcte_identifica>-nmunfim.
      <fs_report>-tipo_cte     = <fs_zcte_identifica>-tpcte.
    ELSE.
      CONTINUE.
    ENDIF.

    <fs_report>-qt_carga_cte = <fs_zlest0194>-qt_carga_cte.
    "Equalização ECC X HANA #115197   - SMC
    READ TABLE tl_zcte_n55 INTO DATA(ws_n55) WITH KEY cd_chave_cte = <fs_zlest0194>-chave_xml_cte.
    IF sy-subrc EQ 0.
      <fs_report>-n55_chave_acesso = ws_n55-n55_chave_acesso.
    ENDIF.
    "Equalização ECC X HANA #115197   - SMC
    CLEAR: <fs_report>-email, <fs_report>-status.

    IF <fs_zlest0194>-email IS NOT INITIAL.
      <fs_report>-email = gc_icones-email.
    ENDIF.

    IF <fs_zlest0194>-nr_fatura IS NOT INITIAL.
      <fs_report>-status        = gc_icones-flag_green.
      <fs_report>-obj_key_perda = <fs_zlest0194>-obj_key_perda(10).
      <fs_report>-ano_key_perda = <fs_zlest0194>-obj_key_perda+10(4).
    ENDIF.

    IF <fs_zlest0194>-obj_key_perda IS NOT INITIAL OR <fs_zlest0194>-fina_stat = abap_true.
      <fs_report>-status = gc_icones-flag_complete.
    ENDIF.

    READ TABLE t_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY
                                                 kunnr = <fs_report>-kunnr_ov.
    IF <fs_kna1> IS ASSIGNED.
      <fs_report>-name1 = <fs_kna1>-name1.
    ENDIF.

*    READ TABLE t_zcte_identifica ASSIGNING FIELD-SYMBOL(<fs_zcte_identifica>)
*                                               WITH KEY docnum = <fs_zlest0194>-docnum_sub.
*    IF sy-subrc IS INITIAL.
*      <fs_report>-inicio_muni  = <fs_zcte_identifica>-nmunini.
*      <fs_report>-termino_muni = <fs_zcte_identifica>-nmunfim.
*      <fs_report>-tipo_cte     = <fs_zcte_identifica>-tpcte.
*    ELSE.
*      CONTINUE.
*    ENDIF.

    READ TABLE t_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>)
                      WITH KEY bukrs = <fs_zlest0194>-bukrs_ov
                               gjahr = <fs_zlest0194>-gjahr_vf
                               awkey = <fs_zlest0194>-awkey.
    IF sy-subrc IS INITIAL.
      <fs_report>-nr_doc_contabil = <fs_bkpf>-belnr.
      <fs_report>-ano_doc_contabil = <fs_bkpf>-belnr.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'PF_0101'.
  SET TITLEBAR  'ST_0101'.

  PERFORM f_alv_inicial.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO TRANSACTION 'ZSDT0168'.
    WHEN 'EXIT'.
      EXIT.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_INICIAL
*&---------------------------------------------------------------------*
FORM f_alv_inicial .

  DATA: w_variant TYPE disvariant.

  IF lcl_alv IS INITIAL.

    PERFORM f_processa.

    CREATE OBJECT lcl_container_alv
      EXPORTING
        container_name              = 'ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT lcl_alv
      EXPORTING
        i_parent          = lcl_container_alv
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT lcl_event.

* Link para documentos Criados
    SET HANDLER lcl_event->handle_hotspot_click  FOR lcl_alv.

* Incluir a referência a o evento TOOLBAR
    SET HANDLER lcl_event->handle_toolbar FOR lcl_alv.

* Incluir a referência a o evento USER_COMMAND
    SET HANDLER lcl_event->handle_command_grid FOR lcl_alv.

    REFRESH t_fieldcat.
    PERFORM zf_montar_fieldcat CHANGING t_report t_fieldcat.
    PERFORM zf_ajuste_campos   CHANGING t_fieldcat.

    wa_layout-zebra      = abap_true.  "Código Zebrado
    wa_layout-cwidth_opt = abap_true.  "Ajusta tamanho na coluna
    wa_layout-box_fname  = abap_true.  "
    wa_layout-sel_mode   = 'A'.        "

    w_variant-report    = sy-repid.

    SORT t_report BY nr_fatura.
    CALL METHOD lcl_alv->set_table_for_first_display
      EXPORTING
        is_variant      = w_variant
        i_save          = 'A'
        i_default       = 'X'
        is_layout       = wa_layout
      CHANGING
        it_outtab       = t_report[]
        it_fieldcatalog = t_fieldcat[].

  ELSE.

    CALL METHOD lcl_alv->refresh_table_display.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  SET PF-STATUS 'PF_0102'.
  SET TITLEBAR 'ST_0102'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SELECIONA_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE seleciona_0102 OUTPUT.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(z_mm) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>)
                                        INDEX w_row_no-row_id.
*    IF SY-SUBRC IS INITIAL.
*      <FS_REPORT>-SELECIONAR = 'X'.
*    ENDIF.
*  ENDLOOP.

*  READ TABLE T_REPORT ASSIGNING FIELD-SYMBOL(<FS_REPORT>)
*                                     WITH KEY SELECIONAR = ABAP_TRUE.
    IF sy-subrc IS INITIAL.

      READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>)
                                           WITH KEY docnum_sub = <fs_report>-docnum_sub.

* Alimenta Cabeçalho
      PERFORM f_alimenta_cabecalho USING: <fs_report>
                                          <fs_zlest0194>.

* Alimenta Aba Parceiros
      PERFORM f_alimenta_parceiro USING: <fs_zlest0194>.

* Alimenta Aba :  Nota  Fiscal Eletronica (NF-e)
      PERFORM f_alimenta_nf USING: <fs_zlest0194>.

* Alimenta Aba : Conhecimento Transp. CT-e
      PERFORM f_alimenta_trans_cte USING: <fs_zlest0194>.

* Alimenta Aba Dados SAP
      PERFORM f_alimenta_dados_sap USING: <fs_zlest0194>.

* Alimenta Aba: Modal/Veiculos
      PERFORM f_alimenta_modal_veiculos USING: <fs_zlest0194>.

* Alimenta Aba: Modal/Motorista
      PERFORM f_alimenta_modal_moto USING: <fs_zlest0194>.


      ip_mode = 'D'.


      CLEAR obj.
      obj-objtype = objtype.

      obj-objkey = <fs_report>-chave_xml_cte.

      CREATE OBJECT manager
        EXPORTING
          is_object        = obj
          ip_no_commit     = 'R'
          ip_mode          = ip_mode
        EXCEPTIONS
          object_invalid   = 1
          callback_invalid = 2
          OTHERS           = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      manager->unpublish( ).
      SET SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO TRANSACTION 'ZSDT0168'.
    WHEN 'EXIT'.
      EXIT.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ABAS_SET  OUTPUT
*&---------------------------------------------------------------------*
MODULE abas_set OUTPUT.

  abas-activetab = g_abas-pressed_tab.
  CASE g_abas-pressed_tab.
    WHEN c_abas-tab1.
      g_abas-subscreen = '0201'.
    WHEN c_abas-tab2.
      g_abas-subscreen = '0202'.
    WHEN c_abas-tab3.
      g_abas-subscreen = '0203'.
    WHEN c_abas-tab4.
      g_abas-subscreen = '0204'.
    WHEN c_abas-tab5.
      g_abas-subscreen = '0205'.
    WHEN c_abas-tab6.
      g_abas-subscreen = '0206'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " ABAS_SET  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ABAS_GET  INPUT
*&---------------------------------------------------------------------*
MODULE abas_get INPUT.

  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_abas-tab1.
      g_abas-pressed_tab = c_abas-tab1.
    WHEN c_abas-tab2.
      g_abas-pressed_tab = c_abas-tab2.
    WHEN c_abas-tab3.
      g_abas-pressed_tab = c_abas-tab3.
    WHEN c_abas-tab4.
      g_abas-pressed_tab = c_abas-tab4.
    WHEN c_abas-tab5.
      g_abas-pressed_tab = c_abas-tab5.
    WHEN c_abas-tab6.
      g_abas-pressed_tab = c_abas-tab6.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " ABAS_GET  INPUT


*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_CABECALHO
*&---------------------------------------------------------------------*
FORM f_alimenta_cabecalho USING: p_report    TYPE ty_report "ZSDS026
                                 p_zlest0194 TYPE ty_zlest0194.

  DATA: vl_lifnr TYPE lfa1-lifnr.

  CLEAR: gs_cabec.
  gs_cabec-chave_cte_sub    = p_zlest0194-chave_cte_sub.
  gs_cabec-docnum_sub       = p_zlest0194-docnum_sub.
  gs_cabec-valor_prestacao  = p_zlest0194-valor_prestacao.
  gs_cabec-nr_cte_sub       = p_zlest0194-nr_cte_sub.
  gs_cabec-serie_cte_sub    = p_zlest0194-serie_cte_sub.
  gs_cabec-zvlr_liq_receber = p_zlest0194-zvlr_liq_receber.
  gs_cabec-ds_prod_pred     = p_zlest0194-ds_prod_pred.

  READ TABLE t_zcte_identifica ASSIGNING FIELD-SYMBOL(<fs_zcte_identifica>)
                                             WITH KEY docnum = p_zlest0194-docnum_sub.
  IF sy-subrc IS INITIAL.
    gs_cabec-tpcte            = <fs_zcte_identifica>-tpcte.
    gs_cabec-tpserv           = <fs_zcte_identifica>-tpserv.
    gs_cabec-dhemi            = <fs_zcte_identifica>-dhemi.
    gs_cabec-hremi            = <fs_zcte_identifica>-hremi.
    gs_cabec-modal            = <fs_zcte_identifica>-modal.
    gs_cabec-cfop             = <fs_zcte_identifica>-cfop.
    gs_cabec-nmunini          = <fs_zcte_identifica>-nmunini.
    gs_cabec-ufini            = <fs_zcte_identifica>-ufini.
    gs_cabec-nmunfim          = <fs_zcte_identifica>-nmunfim.
    gs_cabec-uffim            = <fs_zcte_identifica>-uffim.
    gs_cabec-toma             = <fs_zcte_identifica>-toma.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfdoc
   WHERE docnum = @p_zlest0194-docnum_sub
    INTO @DATA(wa_j_1bnfdoc).

  gs_cabec-model = wa_j_1bnfdoc-model.

  SELECT SINGLE *
    FROM j_1bnflin
   WHERE docnum = @p_zlest0194-docnum_sub
    INTO @DATA(wa_j_1bnflin).

  gs_cabec-taxsit = wa_j_1bnflin-taxsit.

  SELECT SINGLE *
    FROM vbap
   WHERE vbeln = @p_zlest0194-ov_sub
    INTO @DATA(wa_vbap).

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_vbap-gsber
      IMPORTING
        output = vl_lifnr.

    IF vl_lifnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1
       WHERE lifnr = @vl_lifnr
        INTO @DATA(wa_lfa1).

      gs_cabec-stcd1  = wa_lfa1-stcd1.
      gs_cabec-stcd3  = wa_lfa1-stcd3.
      gs_cabec-name1  = wa_lfa1-name1.

    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_PARCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_alimenta_parceiro  USING p_zlest0194 TYPE ty_zlest0194.


  CLEAR: gs_parceiros.
  gs_parceiros-reme_cnpj     = p_zlest0194-reme_cnpj.
  gs_parceiros-reme_cpf      = p_zlest0194-reme_cpf.
  gs_parceiros-reme_ie       = p_zlest0194-reme_ie.
  gs_parceiros-reme_rsocial  = p_zlest0194-reme_rsocial.
  gs_parceiros-dest_cnpj     = p_zlest0194-dest_cnpj.
  gs_parceiros-dest_cpf      = p_zlest0194-dest_cpf.
  gs_parceiros-dest_ie       = p_zlest0194-dest_ie.
  gs_parceiros-dest_rsocial  = p_zlest0194-dest_rsocial.
  gs_parceiros-exped_cnpj    = p_zlest0194-exped_cnpj.
  gs_parceiros-exped_cpf     = p_zlest0194-exped_cpf.
  gs_parceiros-exped_ie      = p_zlest0194-exped_ie.
  gs_parceiros-exped_rsocial = p_zlest0194-exped_rsocial.
  gs_parceiros-receb_cnpj    = p_zlest0194-receb_cnpj.
  gs_parceiros-receb_cpf     = p_zlest0194-receb_cpf.
  gs_parceiros-receb_ie      = p_zlest0194-receb_ie.
  gs_parceiros-receb_rsocial = p_zlest0194-receb_rsocial.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_DADOS_SAP
*&---------------------------------------------------------------------*
FORM f_alimenta_dados_sap USING p_zlest0194 TYPE ty_zlest0194.

  CLEAR: gs_dados_sap.
  gs_dados_sap-valor_prestacao  = p_zlest0194-valor_prestacao.
  gs_dados_sap-vl_total_merc    = p_zlest0194-vl_total_merc.
  gs_dados_sap-qt_carga_cte     = p_zlest0194-qt_carga_cte.
  gs_dados_sap-qt_descarga_cte  = p_zlest0194-qt_descarga_cte.
  gs_dados_sap-zpeso_diferenca  = p_zlest0194-zpeso_diferenca.
  gs_dados_sap-zquebra          = p_zlest0194-zquebra.
  gs_dados_sap-zperda           = p_zlest0194-zperda.
  gs_dados_sap-zvlr_quebra      = p_zlest0194-zvlr_quebra.
  gs_dados_sap-zvlr_perda       = p_zlest0194-zvlr_perda.
  gs_dados_sap-zvlr_liq_receber = p_zlest0194-zvlr_liq_receber.
  gs_dados_sap-dt_descarga      = p_zlest0194-dt_descarga.
  gs_dados_sap-data_vencimento  = p_zlest0194-data_vencimento.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_NF
*&---------------------------------------------------------------------*
FORM f_alimenta_nf  USING p_zlest0194 TYPE ty_zlest0194.

*  SELECT *
*    FROM ZCTE_INFO_NOTA
*   WHERE DOCNUM = @P_ZLEST0194-DOCNUM_SUB
*    INTO TABLE @DATA(TL_ZCTE_INFO_NOTA).

  SELECT *
   FROM zib_cte_dist_n55
  WHERE cd_chave_cte = @p_zlest0194-chave_xml_cte
   INTO TABLE @DATA(tl_zcte_info_nota).

  CLEAR: t_nota.
  LOOP AT tl_zcte_info_nota ASSIGNING FIELD-SYMBOL(<fs_zcte_info_nota>) .
    APPEND INITIAL LINE TO t_nota ASSIGNING FIELD-SYMBOL(<fs_nota>).
*    <FS_NOTA>-CHAVE       = <FS_NOTA>-CHAVE.
*    <FS_NOTA>-VL_PRODUTOS = <FS_NOTA>-VL_PRODUTOS.
*    <FS_NOTA>-DOCNUM_NF   = <FS_NOTA>-DOCNUM_NF.
    <fs_nota>-chave       = <fs_zcte_info_nota>-n55_chave_acesso.
    <fs_nota>-vl_produtos = <fs_zcte_info_nota>-zvlr_mercadoria.
    <fs_nota>-docnum_nf   = <fs_zcte_info_nota>-docnum_nfe.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_TRANS_CTE
*&---------------------------------------------------------------------*
FORM f_alimenta_trans_cte  USING p_zlest0194 TYPE ty_zlest0194.

  SELECT *
     FROM zcte_doc_ant
    WHERE docnum = @p_zlest0194-docnum_sub
     INTO TABLE @DATA(tl_zcte_doc_ant).

  CLEAR: t_trans_cte.
  LOOP AT tl_zcte_doc_ant ASSIGNING FIELD-SYMBOL(<fs_zcte_doc_ant>).
    APPEND INITIAL LINE TO t_trans_cte ASSIGNING FIELD-SYMBOL(<fs_trans_cte>).
    <fs_trans_cte>-chave         = <fs_zcte_doc_ant>-c57_chave_acesso.
    <fs_trans_cte>-emit_ant_nome = <fs_zcte_doc_ant>-emit_ant_nome.
    <fs_trans_cte>-emit_ant_cnpj = <fs_zcte_doc_ant>-emit_ant_cnpj.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_MODAL_VEICULOS
*&---------------------------------------------------------------------*
FORM f_alimenta_modal_veiculos  USING p_zlest0194 TYPE ty_zlest0194.

  SELECT *
     FROM zcte_trans
    WHERE docnum = @p_zlest0194-docnum_sub
     INTO TABLE @DATA(tl_zcte_trans).

  CLEAR: t_modal_veiculos.
  LOOP AT tl_zcte_trans ASSIGNING FIELD-SYMBOL(<fs_zcte_trans>).
    APPEND INITIAL LINE TO t_modal_veiculos ASSIGNING FIELD-SYMBOL(<fs_modal_veiculos>).
    <fs_modal_veiculos>-pc_veiculo   = <fs_zcte_trans>-pc_veiculo.
    <fs_modal_veiculos>-proprietario = <fs_zcte_trans>-proprietario.
    <fs_modal_veiculos>-prop_nome    = <fs_zcte_trans>-prop_nome.
    <fs_modal_veiculos>-prop_rntrc   = <fs_zcte_trans>-prop_rntrc.
    <fs_modal_veiculos>-cd_renavam   = <fs_zcte_trans>-cd_renavam.
    <fs_modal_veiculos>-tp_veiculo   = <fs_zcte_trans>-tp_veiculo.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_MODAL_MOTO
*&---------------------------------------------------------------------*
FORM f_alimenta_modal_moto  USING p_zlest0194 TYPE ty_zlest0194.

  SELECT *
    FROM zcte_motorista
   WHERE docnum = @p_zlest0194-docnum_sub
    INTO TABLE @DATA(tl_zcte_motorista).

  CLEAR: t_modal_moto.
  LOOP AT tl_zcte_motorista ASSIGNING FIELD-SYMBOL(<fs_zcte_motorista>).
    APPEND INITIAL LINE TO t_modal_moto ASSIGNING FIELD-SYMBOL(<fs_modal_moto>).
    <fs_modal_moto>-lifnr = <fs_zcte_motorista>-lifnr.
    <fs_modal_moto>-xnome = <fs_zcte_motorista>-xnome.
    <fs_modal_moto>-cpf   = <fs_zcte_motorista>-cpf.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DACTE
*&---------------------------------------------------------------------*
FORM f_dacte .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,
        l_valid      TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
*    "Selecionar apenas uma linha
    MESSAGE i000(z_mm) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING <fs_report> INDEX w_row_no-row_id.

*    CHECK SY-SUBRC IS INITIAL.

*    READ TABLE T_ZLEST0194 ASSIGNING FIELD-SYMBOL(<FS_ZLEST0194>)
*                                         WITH KEY DOCNUM_SUB = <FS_REPORT>-DOCNUM_SUB.
*
    IF sy-subrc IS INITIAL.

      TRY .
          zcl_cte_dist_g=>dacte( i_cte = <fs_report>-chave_xml_cte ).
        CATCH zcx_cte_inbound INTO DATA(ex_cte_inbound).
          ex_cte_inbound->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

*** BUG 58520 - inicio
***    ENDIF.


*    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>)
*                                      WITH KEY "DOCNUM_SUB = <FS_REPORT>-DOCNUM_SUB
*                                               selecionar = abap_true.
*    IF sy-subrc IS INITIAL.

      READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>)
                                          WITH KEY docnum_sub = <fs_report>-docnum_sub.

      IF sy-subrc IS INITIAL.

        TRY .
            zcl_cte_dist_g=>dacte( i_cte = <fs_zlest0194>-chave_cte_sub ).
          CATCH zcx_cte_inbound INTO DATA(ex_cte_inbound_sub).
            ex_cte_inbound_sub->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.
      ENDIF.

    ENDIF.
*** BUG 58520 - Fim
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANEXO
*&---------------------------------------------------------------------*
FORM f_anexo .

*----------------------------------------------------------------------
*Preenche Estrutura
*----------------------------------------------------------------------
  DATA: t_index_rows      TYPE lvc_t_row,                   "#EC NEEDED
        t_row_no          TYPE lvc_t_roid,
        t_fatura          TYPE zstles_fatura,
        t_custo           TYPE TABLE OF y_custo,
        t_collect_custo   TYPE TABLE OF y_custo,

        w_collect_custo   LIKE LINE OF t_collect_custo,
        w_row_no          LIKE LINE OF t_row_no,
        w_fatura          TYPE zstles_fatura,
        w_item            TYPE zstles_item,

        w_job_output_info TYPE ssfcrescl,
        w_control         TYPE ssfctrlop,
        w_options         TYPE ssfcompop,
        t_otf             TYPE itcoo OCCURS 0 WITH HEADER LINE,
        t_tline           TYPE TABLE OF tline WITH HEADER LINE,
        l_bin_filesize    TYPE i.

  DATA: l_linhas     TYPE sy-tabix,
        l_access_key TYPE j_1b_nfe_access_key,
        l_answer     TYPE c,
        l_data_de    TYPE zlest0194-dt_fatura,
        l_data_ate   TYPE zlest0194-dt_fatura,
        l_fm_name    TYPE  rs38l_fnam,
        l_last_linha TYPE sy-tfill.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE  t_report INTO DATA(w_report) INDEX w_row_no-row_id.
    CHECK sy-subrc IS INITIAL.

    IF w_report-nr_fatura IS INITIAL.
      MESSAGE i000(z_mm) WITH 'Item sem fatura gerada'(t05) DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
*----------------------------------------------------------------------
* Descarga frota própria frete terceiros
*----------------------------------------------------------------------
      SELECT * FROM zlest0194
        INTO TABLE @DATA(t_zlest0194)
        WHERE nr_fatura     = @w_report-nr_fatura.

      IF t_zlest0194[] IS  INITIAL.
        MESSAGE i000(z_mm) WITH 'Fatura não encontrada'(t04) DISPLAY LIKE 'E'.
        RETURN.
      ELSE.

*----------------------------------------------------------------------
* Preenche dados do Cliente
*----------------------------------------------------------------------

* Mestre de clientes (parte geral)
        SELECT kunnr, name1, stcd1, stcd3, adrnr FROM kna1
          INTO TABLE @DATA(t_kna1)
          FOR ALL ENTRIES IN @t_zlest0194
          WHERE kunnr = @t_zlest0194-kunnr_ov.

* Endereços (administração de endereços central)
        SELECT * FROM adrc
         INTO TABLE @DATA(t_adrc)
          FOR ALL ENTRIES IN @t_kna1
          WHERE addrnumber = @t_kna1-adrnr.

* Endereços (administração de endereços central)
        SELECT * FROM t001
         INTO TABLE @DATA(t_t001)
          FOR ALL ENTRIES IN @t_zlest0194
          WHERE bukrs = @t_zlest0194-bukrs_ov.

*        SELECT * FROM adrc
*         APPENDING TABLE t_adrc
*          FOR ALL ENTRIES IN t_t001
*          WHERE addrnumber = t_t001-adrnr.

* Local de negócios
        SELECT * FROM j_1bbranch
          INTO TABLE @DATA(t_branch)
          FOR ALL ENTRIES IN @t_zlest0194
          WHERE bukrs = @t_zlest0194-bukrs_ov
            AND branch = @t_zlest0194-branch_ov.

        SELECT * FROM adrc
         APPENDING TABLE t_adrc
          FOR ALL ENTRIES IN t_branch
          WHERE addrnumber = t_branch-adrnr.

        SELECT * FROM zib_cte_dist_ter
          INTO TABLE @DATA(t_zib_cte_dist_ter)
          FOR ALL ENTRIES IN @t_zlest0194
          WHERE cd_chave_cte = @t_zlest0194-chave_xml_cte.

      ENDIF.

      IF t_zib_cte_dist_ter[] IS NOT INITIAL.
        SELECT * FROM zib_cte_dist_n55
          INTO TABLE @DATA(t_zib_cte_dist_n55)
          FOR ALL ENTRIES IN @t_zib_cte_dist_ter
          WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte.
      ENDIF.

      IF t_zib_cte_dist_n55[] IS NOT INITIAL.

        DATA(t_zib_cte_dist_n55_aux2) = t_zib_cte_dist_n55.

*---> 04/07/2023 - Migração S4 - WS
        SORT  t_zib_cte_dist_n55_aux2 BY tknum.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM t_zib_cte_dist_n55_aux2 COMPARING tknum.

        SELECT p~fknum p~fkpos p~netwr p~knumv i~vbeln i~posnr i~netpr
          FROM vfkp AS p INNER JOIN vfsi AS i
          ON i~knumv = p~knumv
                  INTO TABLE t_custo
          FOR ALL ENTRIES IN t_zib_cte_dist_n55_aux2
          WHERE p~fknum = t_zib_cte_dist_n55_aux2-fknum.

        LOOP AT t_custo INTO DATA(w_custo).
          w_collect_custo-vbeln = w_custo-vbeln.
          w_collect_custo-netwr = w_custo-netwr.
          w_collect_custo-netpr = w_custo-netpr.
          COLLECT w_collect_custo INTO t_collect_custo.
        ENDLOOP.

      ENDIF.

      SORT t_zlest0194 BY dt_mov_ov.
      READ TABLE t_zlest0194 INTO DATA(w_zlest0194) INDEX 1.
      l_data_de = w_zlest0194-dt_mov_ov.

      CLEAR: w_zlest0194.
      DESCRIBE TABLE t_zlest0194 LINES l_last_linha.
      READ TABLE t_zlest0194 INTO w_zlest0194 INDEX l_last_linha.
      l_data_ate  = w_zlest0194-dt_mov_ov.

      CLEAR: w_zlest0194.
      LOOP AT t_zlest0194 INTO w_zlest0194.

        READ TABLE t_zib_cte_dist_n55 INTO  DATA(w_zib_cte_dist_n55)
                        WITH KEY cd_chave_cte = w_zlest0194-chave_xml_cte.

*----------------------------------------------------------------------
* Preenche dados de Header
*----------------------------------------------------------------------
        w_fatura-header-nr_fatura  = w_zlest0194-nr_fatura.

        IF l_data_de <> l_data_ate.
          w_fatura-header-data_de    = l_data_de.
          w_fatura-header-separador  = '-'.
          w_fatura-header-data_ate   = l_data_ate .
        ELSE.
          w_fatura-header-data_de    = l_data_de.
        ENDIF.

        w_fatura-header-dt_fatura       = w_zlest0194-dt_fatura.
        w_fatura-header-data_vencimento = w_zlest0194-data_vencimento.

        READ TABLE t_t001 INTO DATA(w_t001) WITH KEY bukrs = w_zlest0194-bukrs_ov.

        IF sy-subrc IS INITIAL.

          w_fatura-header-butxt      = w_t001-butxt.

          READ TABLE t_branch INTO DATA(w_branch) WITH KEY bukrs = w_t001-bukrs
                                                           branch = w_zlest0194-branch_ov.
          CHECK sy-subrc IS INITIAL.

          w_fatura-header-name1      = w_branch-name.

          WRITE w_branch-stcd1 TO w_fatura-header-stcd1 USING EDIT MASK '__.___.___/____-__'.
          w_fatura-header-state_insc  = w_branch-state_insc .

        ENDIF.

        READ TABLE t_adrc INTO DATA(w_adrc) WITH KEY addrnumber = w_branch-adrnr. "w_t001-adrnr.

        IF sy-subrc IS INITIAL.

          w_fatura-header-street     = w_adrc-street.
          w_fatura-header-house_num1 = w_adrc-house_num1.
          w_fatura-header-city1      = w_adrc-city1.
          w_fatura-header-city2      = w_adrc-city2.
          w_fatura-header-tel_number = w_adrc-tel_number.
          w_fatura-header-post_code1 = w_adrc-post_code1.
        ENDIF.

        READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY kunnr = w_zlest0194-kunnr_ov.

        IF sy-subrc IS INITIAL.

          w_fatura-cliente-name1       = w_kna1-name1.
          WRITE w_kna1-stcd1 TO w_fatura-cliente-stcd1 USING EDIT MASK '__.___.___/____-__'.
          w_fatura-cliente-state_insc  = w_kna1-stcd3.

*----------------------------------------------------------------------
* Preenche dados do cliente
*----------------------------------------------------------------------
          CLEAR: w_adrc.
          READ TABLE t_adrc INTO w_adrc WITH KEY addrnumber = w_kna1-adrnr.

          CHECK sy-subrc IS INITIAL.

          w_fatura-cliente-street      = w_adrc-street.
          w_fatura-cliente-house_num1  = w_adrc-house_num1.
          w_fatura-cliente-city1       = w_adrc-city1.
          w_fatura-cliente-city2       = w_adrc-city1.
          w_fatura-cliente-tel_number  = w_adrc-tel_number.
          w_fatura-cliente-post_code1  = w_adrc-post_code1.

        ENDIF.

*----------------------------------------------------------------------
* Preenche Origem\ Destino e Coleta\Entrega
*----------------------------------------------------------------------
        READ TABLE t_zib_cte_dist_ter INTO DATA(w_cte_dist_ter)
                                 WITH KEY cd_chave_cte = w_zlest0194-chave_xml_cte.

        IF sy-subrc IS INITIAL.

          w_fatura-orig_dest-origem    = w_cte_dist_ter-inicio_muni.
          w_fatura-orig_dest-destino   = w_cte_dist_ter-termino_muni.
          w_fatura-orig_dest-coleta    = w_cte_dist_ter-reme_rsocial.
          w_fatura-orig_dest-entrega   = w_cte_dist_ter-dest_rsocial.

          CLEAR: l_access_key.
          l_access_key     = w_zib_cte_dist_n55-n55_chave_acesso.
          w_item-n01_nr_nf = l_access_key-nfnum9.
*----------------------------------------------------------------------
* Preenche lista de CTe
*----------------------------------------------------------------------
          w_item-nr_fatura        = w_zlest0194-nr_fatura.
          w_item-item_fatura      = w_zlest0194-item_fatura.
          w_item-dt_fatura        = w_zlest0194-dt_fatura.
          w_item-data_vencimento  = w_zlest0194-data_vencimento.
          w_item-kunnr_ov         = w_zlest0194-kunnr_ov.
          w_item-dt_mov_ov        = w_zlest0194-dt_mov_ov.
          w_item-nr_cte_sub       = w_zlest0194-nr_cte_sub.
          w_item-numr_cte         = w_zlest0194-numr_cte.
          w_item-qt_carga_cte     = w_zlest0194-qt_carga_cte.
          w_item-qt_descarga_cte  = w_zlest0194-qt_descarga_cte.
          w_item-valor_prestacao  = w_zlest0194-valor_prestacao.
          w_item-placa_cav        = w_zlest0194-placa_cav.
          w_item-ds_prod_pred     = w_zlest0194-ds_prod_pred.


*------------------------------------------
* Valor de Perda
*------------------------------------------
          IF w_zlest0194-zvlr_perda < 0.
            w_item-zvlr_perda       = w_zlest0194-zvlr_perda * ( -1 ).
          ELSE.
            w_item-zvlr_perda       = w_zlest0194-zvlr_perda.
          ENDIF.

*------------------------------------------
* Peso de Perda
*------------------------------------------
          IF w_item-zvlr_perda IS NOT INITIAL.

            IF w_zlest0194-zperda < 0.
              w_item-zperda       = w_zlest0194-zperda * ( -1 ) .
            ELSE.
              w_item-zperda       = w_zlest0194-zperda.
            ENDIF.

          ELSE.
            CLEAR w_item-zperda.
          ENDIF.

*------------------------------------------
* Valor de Quebra
*------------------------------------------
          IF w_zlest0194-zvlr_quebra < 0.
            w_item-zvlr_quebra      = w_zlest0194-zvlr_quebra * ( -1 ).
          ELSE.
            w_item-zvlr_quebra      = w_zlest0194-zvlr_quebra.
          ENDIF.
*------------------------------------------
* Peso de Quebra
*------------------------------------------
          IF w_item-zvlr_quebra IS NOT INITIAL.

            IF w_zlest0194-zquebra < 0.
              w_item-zquebra      = w_zlest0194-zquebra * ( -1 ) .
            ELSE.
              w_item-zquebra       = w_zlest0194-zquebra.
            ENDIF.

          ELSE.
            CLEAR w_item-zquebra.
          ENDIF.


*------------------------------------------
* Valor a Receber
*------------------------------------------
          w_item-zvlr_liq_receber = w_zlest0194-zvlr_liq_receber.

* Tarifa CTE
          IF  w_item-qt_carga_cte IS NOT INITIAL.
            w_item-tarifa = (  w_item-valor_prestacao /  w_item-qt_carga_cte ) * 1000.
          ENDIF.

*Tarifa Custo
          READ TABLE t_custo  INTO w_custo WITH KEY fknum = w_zib_cte_dist_n55-fknum.

          IF sy-subrc IS INITIAL.

            READ TABLE t_collect_custo INTO w_collect_custo WITH KEY vbeln = w_custo-vbeln.
            IF sy-subrc IS INITIAL.
              w_item-zvlr_liq_receber = w_collect_custo-netpr.
            ENDIF.

          ENDIF.

        ENDIF.

        APPEND w_item TO  w_fatura-item[].

      ENDLOOP.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'ZSFLES_FATURA'
        IMPORTING
          fm_name            = l_fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*  Impresora
      w_control-no_dialog = ' '. "Evita la pantalla de opciones de salida del formulario
      w_options-tddest   = 'LOCL'.
      w_options-tdimmed  = 'X'.
      w_options-tdnewid  = 'X'.
      w_options-tdnoarch = 'X'.


      w_control-preview =  space.
      w_control-device  = 'PRINTER'.
      w_control-getotf  = ' '.

      CLEAR: w_job_output_info.
      CALL FUNCTION l_fm_name
        EXPORTING
          user_settings      = ' '
          control_parameters = w_control
          output_options     = w_options
          fatura             = w_fatura
        IMPORTING
          job_output_info    = w_job_output_info
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FATURAR
*&---------------------------------------------------------------------*
FORM f_faturar .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,
        l_valid      TYPE c.

  DATA: it_cte  TYPE zib_cte_dist_ter_t.
  DATA: gf_authorization_ft_02 TYPE c. "Faturar

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '02'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_02 = abap_true.
  ENDIF.

  IF gf_authorization_ft_02 IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-017 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  "COMENTADO-Equalização ECC X HANA #115197   - SMC
*  IF l_linhas = 0.
*    MESSAGE s000(z_les) WITH 'Selecionar uma linha'
*      DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*
*  IF l_linhas > 1.
*    MESSAGE s000(z_les) WITH 'Selecionar apenas uma linha'
*         DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
  "COMENTADO-Equalização ECC X HANA #115197   - SMC
  LOOP AT t_row_no INTO w_row_no.
    READ TABLE t_report ASSIGNING <fs_report> INDEX w_row_no-row_id.
  ENDLOOP.

  CHECK <fs_report> IS ASSIGNED.

  REFRESH: t_faturar, t_saida_fn, t_saida_fg.
  CLEAR: wg_fat_cab, wg_fat_cab_fn, wg_fat_cab_fg.

  wg_fat_cab-cod_cliente  = <fs_report>-kunnr_ov.
  wg_fat_cab-nome_cliente = <fs_report>-name1.

* Busca dados sem código de fatura
  PERFORM zf_busca_cte_fn.

* Busca dados de Faturas já Geradas
  PERFORM zf_busca_cte_fg.

  IF <fs_report>-nr_fatura IS INITIAL.
    tab_fatura-activetab = 'ABAS_FN'.
  ELSE.
    tab_fatura-activetab = 'ABAS_FG'.
  ENDIF.

  CALL SCREEN 0207.

  PERFORM f_processa.

  IF lcl_alv IS NOT INITIAL.
    CALL METHOD lcl_alv->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR
*&---------------------------------------------------------------------*
FORM f_estornar USING p_estornar TYPE char01.

  DATA:
    gf_authorization_ft_05 TYPE c, "Gerar Fatura (Prog.Pagamento)
    gf_authorization_ft_06 TYPE c. "Estornar Fatura (Prog.Pagamento)

  DATA: it_chaves_bloq TYPE TABLE OF ty_bloq,
        wa_cte_select  TYPE zib_cte_dist_ter.

  IF gf_authorization_ft_05 IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-018 TEXT-019 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  IF gf_authorization_ft_06 IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-020 TEXT-021 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

****  LOOP AT T_ZLEST0194 ASSIGNING FIELD-SYMBOL(<FS_ZLEST0194>).
****
****    READ TABLE T_REPORT ASSIGNING FIELD-SYMBOL(<FS_REPORT>)
****                                     WITH KEY DOCNUM_SUB = <FS_REPORT>-DOCNUM_SUB
****                                              SELECIONAR = ABAP_TRUE.
****    IF SY-SUBRC IS INITIAL.
****
****      CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
****        EXPORTING
****          CHAVE          = <FS_ZLEST0194>-CHAVE_CTE_SUB
****        EXCEPTIONS
****          FOREIGN_LOCK   = 1
****          SYSTEM_FAILURE = 2
****          OTHERS         = 3.
****
****      IF SY-SUBRC IS NOT INITIAL.
****        MESSAGE ID SY-MSGID TYPE 'W' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
****        CONTINUE.
****      ENDIF.
****
****      APPEND INITIAL LINE TO IT_CHAVES_BLOQ ASSIGNING FIELD-SYMBOL(<FS_CHAVES_BLOQ>).
****      <FS_CHAVES_BLOQ>-CD_CHAVE_CTE = <FS_ZLEST0194>-CHAVE_CTE_SUB.
****
****      MOVE-CORRESPONDING <FS_ZLEST0194> TO WA_CTE_SELECT.
****      CALL METHOD OBJ_CTE->GERAR_FATURA_FRETE
****        EXPORTING
****          P_CHAVE_CTE         = <FS_ZLEST0194>-CHAVE_CTE_SUB
****          P_ESTORNAR          = P_ESTORNAR
****        CHANGING
****          P_CTE               = WA_CTE_SELECT
****        EXCEPTIONS
****          NAO_ENC_FRETE       = 1
****          FATURA              = 2
****          PEDIDO              = 3
****          COD_IVA             = 4
****          BANCO_PARCEIRO      = 5
****          PARAM_CTB           = 6
****          BANCO_EMPRESA       = 7
****          SEM_VT              = 8
****          ERRO_ENTRADA_FISCAL = 9
****          MIRO_COMPENSADA     = 11
****          PESO_CHEGADA        = 12
****          OTHERS              = 13.
****
****      IF SY-SUBRC IS NOT INITIAL.
****        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
****      ENDIF.
****
****      CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
****        EXPORTING
****          CHAVE = WA_CTE_SELECT-CD_CHAVE_CTE.
****
****      DELETE IT_CHAVES_BLOQ WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.
****
****    ENDIF.
****  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0207  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0207 OUTPUT.
  SET PF-STATUS 'STATUS-0207'.
  SET TITLEBAR 'TITLE-0207'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0207  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0207 INPUT.

  v_ucomm = sy-ucomm.
  CLEAR sy-ucomm.

  CASE v_ucomm.
    WHEN 'ABAS_FN'.
      tab_fatura-activetab = 'ABAS_FN'.
      v_subscreen = 'SUB_FN'.
    WHEN 'ABAS_FG'.
      tab_fatura-activetab = 'ABAS_FG'.
      v_subscreen = 'SUB_FG'.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      EXIT.
    WHEN 'CANCEL'.
      LEAVE TO TRANSACTION 'ZSDT0168'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SELECIONA_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE seleciona_dados INPUT.

  CLEAR wg_fat_cab-nome_cliente.
  REFRESH: t_saida_fn, t_saida_fg, t_faturar.

  IF lcl_alv_208 IS NOT INITIAL.
    CALL METHOD lcl_alv_208->refresh_table_display.
  ENDIF.

  IF lcl_alv_209 IS NOT INITIAL.
    CALL METHOD lcl_alv_209->refresh_table_display.
  ENDIF.

  IF wg_fat_cab-cod_cliente IS NOT INITIAL.

    UNPACK wg_fat_cab-cod_cliente TO wg_fat_cab-cod_cliente.

    SELECT name1
      FROM kna1
      INTO wg_fat_cab-nome_cliente
      WHERE kunnr = wg_fat_cab-cod_cliente.
    ENDSELECT.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(z_les) WITH 'Cliente não encontrado!'(022) DISPLAY LIKE 'E'.
      RETURN.
    ELSE.

* Busca dados sem código de fatura
      PERFORM zf_busca_cte_fn.

* Busca dados de Faturas já Geradas
      PERFORM zf_busca_cte_fg.

    ENDIF.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ALV_SCREEN_208
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_screen_208 .

  DATA: w_variant TYPE disvariant.

  wa_layout-zebra      = abap_true.  "Código Zebrado
  wa_layout-cwidth_opt = abap_true.  "Ajusta tamanho na coluna
  wa_layout-box_fname  = abap_true.   "

*  WA_LAYOUT-SEL_MODE   = 'A'.        "

  IF lcl_alv_208 IS INITIAL.

    PERFORM zf_busca_cte_fn.

    CREATE OBJECT lcl_cont_alv_208
      EXPORTING
        container_name              = 'GRID_208'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT lcl_alv_208
      EXPORTING
        i_parent          = lcl_cont_alv_208
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT lcl_event.

* Link para documentos Criados
    SET HANDLER lcl_event->handle_hotspot_click_fat_fn FOR lcl_alv_208.

* Incluir a referência a o evento TOOLBAR
    SET HANDLER lcl_event->handle_toolbar_nova_fatura FOR lcl_alv_208.

* Incluir a referência a o evento USER_COMMAND
    SET HANDLER lcl_event->handle_command_grid_fatura FOR lcl_alv_208.

    REFRESH t_fieldcat.
    PERFORM zf_montar_fieldcat CHANGING t_saida_fn t_fieldcat.
    PERFORM zf_ajuste_campos   CHANGING t_fieldcat.


    w_variant-report    = sy-repid.

    CALL METHOD lcl_alv_208->set_table_for_first_display
      EXPORTING
        is_variant      = w_variant
        i_save          = 'A'
        i_default       = 'X'
        is_layout       = wa_layout
      CHANGING
        it_outtab       = t_saida_fn[]
        it_fieldcatalog = t_fieldcat[].

  ELSE.
    CALL METHOD lcl_alv_208->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA_FN  text
*      <--P_T_FIELDCAT  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat  CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_elimina_botoes_header USING e_object TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
*  REFRESH: E_OBJECT->MT_TOOLBAR.

  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
*                                   OR function = '&MB_VARIANT'
                                 OR function = '&MB_EXPORT'
                                 OR function = '&PRINT_BACK'.
  "OR function = '&MB_SUM'
  "OR function = '&MB_SUBTOT'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0208  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0208 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  PERFORM alv_screen_208.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0209  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0209 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  PERFORM alv_screen_209.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_SCREEN_209
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_screen_209 .

  DATA: w_variant  TYPE disvariant,
        r_fieldcat TYPE RANGE OF lvc_fname.

  wa_layout-zebra      = abap_true.  "Código Zebrado
  wa_layout-cwidth_opt = abap_true.  "Ajusta tamanho na coluna
  wa_layout-box_fname  = abap_true.   "

  IF lcl_alv_209 IS INITIAL.

    PERFORM zf_busca_cte_fg.

    CREATE OBJECT lcl_cont_alv_209
      EXPORTING
        container_name              = 'GRID_209'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT lcl_alv_209
      EXPORTING
        i_parent          = lcl_cont_alv_209
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT lcl_event.

* Link para documentos Criados
    SET HANDLER lcl_event->handle_hotspot_click_fat_fg FOR lcl_alv_209.

* Incluir a referência a o evento TOOLBAR
    SET HANDLER lcl_event->handle_toolbar_fatura_geradas FOR lcl_alv_209.

* Incluir a referência a o evento USER_COMMAND
    SET HANDLER lcl_event->handle_command_grid_fatura FOR lcl_alv_209.

    REFRESH t_fieldcat.
    PERFORM zf_montar_fieldcat CHANGING t_saida_fg t_fieldcat.
    PERFORM zf_ajuste_campos   CHANGING t_fieldcat.

    r_fieldcat = VALUE #( FOR l IN t_fieldcat WHERE ( fieldname = 'FINA_DAT' OR
                                                      "fieldname = 'FINA_MOT' OR
                                                      fieldname = 'FINA_STAT' OR
                                                      fieldname = 'FINA_USER'  )
                        ( low = l-fieldname     sign   = 'I' option = 'EQ' ) ).

    DELETE t_fieldcat WHERE fieldname IN r_fieldcat.

    SORT t_saida_fg BY nr_fatura.

    w_variant-report    = sy-repid.

    CALL METHOD lcl_alv_209->set_table_for_first_display
      EXPORTING
        is_variant      = w_variant
        i_save          = 'A'
        i_default       = 'X'
        is_layout       = wa_layout
      CHANGING
        it_outtab       = t_saida_fg[]
        it_fieldcatalog = t_fieldcat[].

  ELSE.
    CALL METHOD lcl_alv_209->refresh_table_display.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_adiciona_botoes_header USING e_object TYPE REF TO cl_alv_event_toolbar_set.
* Add Button
  DATA: w_toolbar  TYPE stb_button.

* inclui novo item na barra do container
  CLEAR w_toolbar.
  MOVE 3 TO w_toolbar-butn_type.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'EMAIL'(bf1)                TO w_toolbar-function.
  MOVE '@1S@'                      TO w_toolbar-icon.
  MOVE '0 '                        TO w_toolbar-butn_type.
  MOVE 'Envio e-mail '(031)        TO w_toolbar-quickinfo.
  MOVE 'Envio e-mail '(031)        TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'VISU_EMAIL'(bf6)                TO w_toolbar-function.
  MOVE '@1S@'                           TO w_toolbar-icon.
  MOVE '0 '                             TO w_toolbar-butn_type.
  MOVE 'Visualizar e-mail '(035)        TO w_toolbar-quickinfo.
  MOVE 'Visualizar e-mail '(035)        TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'CONTABILIZACAO'(bf2)        TO w_toolbar-function.
  MOVE '@0M@'                        TO w_toolbar-icon.
  MOVE '0 '                          TO w_toolbar-butn_type.
  MOVE 'Contabilização '(032)        TO w_toolbar-quickinfo.
  MOVE 'Contabilização '(032)        TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'ESTCONTAB'(bf3)        TO w_toolbar-function.
  MOVE '@8Y@'                          TO w_toolbar-icon.
  MOVE '0 '                            TO w_toolbar-butn_type.
  MOVE 'Estorno Contábil '(033)        TO w_toolbar-quickinfo.
  MOVE 'Estorno Contábil '(033)        TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'CONPENSACAO'(bf4)        TO w_toolbar-function.
  MOVE '@AZ@'                    TO w_toolbar-icon.
  MOVE '0 '                      TO w_toolbar-butn_type.
  MOVE 'X'                       TO w_toolbar-disabled.
  MOVE 'Compensação'(034)        TO w_toolbar-quickinfo.
  MOVE 'Compensação'(034)        TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'PDF'(bf5)               TO w_toolbar-function.
  MOVE icon_workflow_inbox      TO w_toolbar-icon.
  MOVE '0 '                     TO w_toolbar-butn_type.
  MOVE ' '                      TO w_toolbar-disabled.
  MOVE 'Fatura PDF'(010)        TO w_toolbar-quickinfo.
  MOVE 'Fatura PDF'(010)        TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.


**********************************************************************
*&pablo
**********************************************************************

  CLEAR w_toolbar.
  MOVE 'FINALIZAR'(bf7)              TO w_toolbar-function.
  MOVE '@01@'                        TO w_toolbar-icon.
  MOVE '0 '                          TO w_toolbar-butn_type.
  MOVE 'Finalizar '(044)             TO w_toolbar-quickinfo.
  MOVE 'Finalizar '(044)             TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.




  CLEAR w_toolbar.
  MOVE 'ESTFINALI'(bf8)                             TO w_toolbar-function.
  MOVE '@02@'                                       TO w_toolbar-icon.
  MOVE '0 '                                         TO w_toolbar-butn_type.
  MOVE 'Estornar Finalização '(045)                 TO w_toolbar-quickinfo.
  MOVE 'Estornar Finalização '(045)                 TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_ajuste_campos CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING <fs_fcat>.

*----------------------
*  Descrição dos campos do ALV da Tela 9000
*----------------------
    CASE <fs_fcat>-fieldname.
      WHEN 'STATUS'.
        PERFORM zf_mostra_coluna USING 'STATUS'            'Status'           .
      WHEN 'SELECIONAR'.
        PERFORM zf_mostra_coluna USING 'SELECIONAR'        ' '                .
        PERFORM zf_edit_coluna.
      WHEN 'EMAIL'.
        PERFORM zf_mostra_coluna USING 'EMAIL'             'Email'             .
      WHEN 'NR_FATURA'.
        PERFORM zf_mostra_coluna USING 'NR_FATURA'         'Fatura'            .
      WHEN 'ITEM_FATURA'.
        PERFORM zf_mostra_coluna USING 'ITEM_FATURA'       'Item Fatura'     .
      WHEN 'BUKRS_OV'.
        PERFORM zf_mostra_coluna USING 'BUKRS_OV'          'Empresa'           .
      WHEN 'BRANCH_OV'.
        PERFORM zf_mostra_coluna USING 'BRANCH_OV'         'Filial'            .
      WHEN 'DS_PROD_PRED'.
        PERFORM zf_mostra_coluna USING 'DS_PROD_PRED'      'Prod. Predominante'.
      WHEN 'KUNNR_OV'.
        PERFORM zf_mostra_coluna USING 'KUNNR_OV'          'Cód. Cliente'      .
      WHEN 'DOCNUM_SUB'.
*        PERFORM ZF_MOSTRA_COLUNA USING 'DOCNUM_SUB'        'Docnum'           .
      WHEN 'NR_CTE_SUB'.
        PERFORM zf_mostra_coluna USING 'NR_CTE_SUB'        'CT-e'              .
      WHEN 'SERIE_CTE_SUB'.
        PERFORM zf_mostra_coluna USING 'SERIE_CTE_SUB'     'Série'             .
      WHEN 'DT_MOV_OV'.
        PERFORM zf_mostra_coluna USING 'DT_MOV_OV'         'Data documento'    .
      WHEN 'QT_CARGA_CTE'.
        PERFORM zf_mostra_coluna USING 'QT_CARGA_CTE'      'Qtde. Origem'      .
      WHEN 'QT_DESCARGA_CTE'.
        PERFORM zf_mostra_coluna USING 'QT_DESCARGA_CTE'   'Qtde. Chegada'      .
      WHEN 'ID_CTR'.
        PERFORM zf_mostra_coluna USING 'ID_CTR'            'Id. CTR'            .
      WHEN 'VALOR_PRESTACAO'.
        PERFORM zf_mostra_coluna USING 'VALOR_PRESTACAO'   'Valor  Frete'       .
      WHEN 'VL_TOTAL_MERC'.
        PERFORM zf_mostra_coluna USING 'VL_TOTAL_MERC'     'Valor Total Merc.'  .
      WHEN 'ZPESO_DIFERENCA'.
        PERFORM zf_mostra_coluna USING 'ZPESO_DIFERENCA'   'Difer. Peso'        .
      WHEN 'ZQUEBRA'.
        PERFORM zf_mostra_coluna USING 'ZQUEBRA'           'Qtde. Quebra'       .
      WHEN 'ZVLR_QUEBRA'.
        PERFORM zf_mostra_coluna USING 'ZVLR_QUEBRA'       'Valor Quebra'       .
      WHEN 'ZPERDA'.
        PERFORM zf_mostra_coluna USING 'ZPERDA'            'Qtde. Perda'         .
      WHEN 'ZVLR_PERDA'.
        PERFORM zf_mostra_coluna USING 'ZVLR_PERDA'        'Valor  Perda'        .
      WHEN 'ZVLR_LIQ_RECEBER'.
        PERFORM zf_mostra_coluna USING 'ZVLR_LIQ_RECEBER'  'Valor  Líquido'      .
      WHEN 'DATA_VENCIMENTO'.
        PERFORM zf_mostra_coluna USING 'DATA_VENCIMENTO'   'Data Vencimento'     .
      WHEN 'CHAVE_XML_CTE'.
        PERFORM zf_mostra_coluna USING 'CHAVE_XML_CTE' 'CTE Contratante'."Equalização ECC X HANA #115197   - SMC
      WHEN 'N55_CHAVE_ACESSO'.
        PERFORM zf_mostra_coluna USING 'N55_CHAVE_ACESSO' 'NFe vinculada'."Equalização ECC X HANA #115197   - SMC
      WHEN 'OBJ_KEY_PERDA'.
        PERFORM zf_mostra_coluna USING 'OBJ_KEY_PERDA'   'Docto. Contábil'.
        PERFORM zf_visualizar_documentos.
      WHEN 'ANO_KEY_PERDA'.
        PERFORM zf_ocultar_coluna.
      WHEN 'NR_DOC_CONTABIL'.
        PERFORM zf_mostra_coluna USING 'NR_DOC_CONTABIL' 'Docto. Contábil CTe'   .
        PERFORM zf_visualizar_documentos.
      WHEN 'ANO_DOC_CONTABIL'.
        PERFORM zf_ocultar_coluna.
      WHEN 'FINA_MOT'.
        PERFORM zf_mostra_coluna USING 'FINA_MOT' 'Mtv Finalização'   .
        PERFORM zf_visualizar_documentos.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MOSTRA_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3763   text
*      -->P_3764   text
*----------------------------------------------------------------------*
FORM zf_mostra_coluna   USING p_fieldname p_desc.

  IF <fs_fcat>-fieldname = p_fieldname.
    CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
    <fs_fcat>-coltext = <fs_fcat>-coltext = p_desc.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EDIT_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3768   text
*      -->P_3769   text
*----------------------------------------------------------------------*
FORM zf_edit_coluna.

  <fs_fcat>-checkbox  = abap_true.
  <fs_fcat>-edit      = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_atribuir_fatura .

  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_delete       TYPE TABLE OF zcot0013,
        t_saida_fn_aux TYPE TABLE OF ty_faturar_fn,

        w_row_no       LIKE LINE OF t_row_no,

        l_nr_fatura    TYPE zlest0194-nr_fatura,
        l_linhas       TYPE sy-tabix,
        l_answer       TYPE c.

  CALL METHOD lcl_alv_208->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* Verifica número da Fatura
*----------------------------------------------------------------------*
  IF wg_fat_cab_fn-nr_fatura IS NOT INITIAL.
    PERFORM zf_valida_nr_fatura CHANGING l_nr_fatura.
  ELSE.
    PERFORM zf_gera_numero_fatura CHANGING l_nr_fatura.
  ENDIF.

*----------------------------------------------------------------------*
* Busca o último item da Fatura
*----------------------------------------------------------------------*
  CHECK l_nr_fatura IS NOT INITIAL.

  SELECT MAX( item_fatura ) FROM zlest0194
    INTO @DATA(l_item)
    WHERE nr_fatura = @l_nr_fatura.

*  SELECT * FROM ZLEST0194
*    INTO TABLE @DATA(T_ITEM)
*    WHERE NR_FATURA = @L_NR_FATURA.
*
*  SORT  T_ITEM  BY NR_FATURA ITEM_FATURA DESCENDING.
*  READ TABLE T_ITEM INTO DATA(W_ITEM) INDEX 1.
*  IF SY-SUBRC IS INITIAL.
*    DATA(L_ITEM) = W_ITEM-NR_FATURA.
*  ENDIF.

*----------------------------------------------------------------------*
* Monta tabela com itens selecionados
*----------------------------------------------------------------------*
  REFRESH: t_saida_fn_aux.
  LOOP AT t_row_no INTO w_row_no.
    READ TABLE t_saida_fn INTO DATA(w_saida_fn) INDEX w_row_no-row_id.
    IF sy-subrc IS INITIAL.
      APPEND w_saida_fn TO t_saida_fn_aux.
    ENDIF.
  ENDLOOP.

* Descarga frota própria frete terceiros
  SELECT * FROM zlest0194
   INTO TABLE t_faturar
    FOR ALL ENTRIES IN t_saida_fn_aux
    WHERE chave_xml_cte = t_saida_fn_aux-chave_xml_cte
     AND  nr_fatura = space.

*----------------------------------------------------------------------*
* Preenche número da Fatura e data de vencimento
*----------------------------------------------------------------------*
  LOOP AT  t_faturar ASSIGNING FIELD-SYMBOL(<fs_faturar>).

    READ TABLE t_saida_fn_aux INTO w_saida_fn WITH KEY chave_xml_cte = <fs_faturar>-chave_xml_cte.    "<<-RIM-SKM-IR131470-05.09.23

    <fs_faturar>-zpeso_diferenca  = w_saida_fn-zpeso_diferenca.
    <fs_faturar>-zquebra          = w_saida_fn-zquebra.
    <fs_faturar>-zperda           = w_saida_fn-zperda.
    <fs_faturar>-zvlr_quebra      = w_saida_fn-zvlr_quebra.
    <fs_faturar>-zvlr_perda       = w_saida_fn-zvlr_perda.
    <fs_faturar>-zvlr_liq_receber = w_saida_fn-zvlr_liq_receber.
    <fs_faturar>-dt_fatura        = sy-datum.
    <fs_faturar>-data_vencimento  = wg_fat_cab_fn-data_vcto.
    <fs_faturar>-nr_fatura        = l_nr_fatura.

    ADD 1 TO l_item.
    <fs_faturar>-item_fatura       = l_item.

  ENDLOOP.

  IF t_faturar[] IS NOT INITIAL.
    MODIFY zlest0194 FROM TABLE t_faturar.
    COMMIT WORK.
    MESSAGE s000(z_les) WITH 'Fatura '(027) l_nr_fatura ' salva com sucesso.'(028).
  ENDIF.

  PERFORM zf_busca_cte_fn.
  PERFORM zf_busca_cte_fg.

  CALL METHOD lcl_alv_208->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMMAND_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM f_command_fatura  USING e_ucomm TYPE sy-ucomm.

  CASE v_ucomm.
    WHEN  'EMAIL'.
      PERFORM zf_envia_email.
    WHEN  'VISU_EMAIL'.
      CALL SCREEN 0210.
    WHEN  'CONTABILIZACAO'.
      PERFORM zf_contabilizacao.
    WHEN 'ESTCONTABIL'.
      PERFORM zf_estornar_contab.
    WHEN 'PDF'.
      PERFORM zf_abre_smartforms.
    WHEN 'FINALIZAR'.
      PERFORM zf_finasem_contab_lanc.
    WHEN 'ESTFINALI'.
      PERFORM zf_finasem_contab_est.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GERA_NUMERO_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_NR_FATURA  text
*----------------------------------------------------------------------*
FORM zf_gera_numero_fatura  CHANGING p_nr_fatura.

  DATA: lv_nr_range TYPE nrnr.
  DATA: lv_object   TYPE nrobj.

  CHECK p_nr_fatura IS INITIAL.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZID_FAT'
*     SUBOBJECT               = I_SUBOBJECT
    IMPORTING
      number                  = p_nr_fatura
    EXCEPTIONS
      interval_not_found      = 01
      number_range_not_intern = 02
      object_not_found        = 03
      OTHERS                  = 08.

  CASE sy-subrc.
    WHEN 01.
      MESSAGE e000(z_les) WITH 'Intervalo não encontrado'(025).
    WHEN 02.
      MESSAGE e000(z_les) WITH 'Range '(023) lv_object 'sem intervalo não encontrado'(024).
    WHEN 03.
      MESSAGE e000(z_les) WITH 'Intervalo não encontrado'(025).
    WHEN 08.
      MESSAGE e000(z_les) WITH 'Range '(023) lv_object 'incorreto'(026).
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_CTE_FN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_busca_cte_fn .

  TYPES: BEGIN OF ty_bnfdoc,
           docnum TYPE zde_docnum_sub2,
           code   TYPE j_1bnfdoc-code,
         END OF ty_bnfdoc.

  DATA: lw_bnfdoc      TYPE          ty_bnfdoc.

  DATA: lv_tabix  TYPE sy-tabix.

  DATA: v_perc(5) TYPE c,
        vl_perda  TYPE zlest0194-zvlr_perda.

  v_perc = '0.25'.

  REFRESH: t_saida_fn, t_faturar, t_bkpf.

  CHECK wg_fat_cab-cod_cliente IS NOT INITIAL.

*&---------------------------------------------------------------------*
* Descarga frota própria frete terceiros
*&---------------------------------------------------------------------*
  IF r_1 IS NOT INITIAL OR r_2 IS NOT INITIAL.

    SELECT *
      FROM zlest0194
      INTO TABLE t_faturar
      WHERE nr_cte_sub    IN s_numr
        AND dt_mov_ov     IN s_data
        AND chave_cte_sub IN s_chave
        AND bukrs_ov      IN s_bukrs
        AND branch_ov     IN s_branch
        AND kunnr_ov      =  wg_fat_cab-cod_cliente
        AND kunnr_cnpj    IN s_cnpj
        AND nr_fatura     =  space
        AND augbl         =  space
        AND obj_key_perda =  space
        AND fina_stat     = abap_false. "Não trazer os finalizados - PSA 10214

*      ELSEIF r_3 IS NOT INITIAL.
*
*    SELECT *
*      FROM zlest0194
*      INTO TABLE t_faturar
*      WHERE nr_cte_sub    IN s_numr
*        AND dt_mov_ov     IN s_data
*        AND chave_cte_sub IN s_chave
*        AND bukrs_ov      IN s_bukrs
*        AND branch_ov     IN s_branch
*        AND kunnr_ov      =  wg_fat_cab-cod_cliente
*        AND kunnr_cnpj    IN s_cnpj
*        AND nr_fatura     =  space
*        AND augbl         =  space
*        AND obj_key_perda =  space
*        OR FINA_STAT      = abap_true.

  ENDIF.

*&---------------------------------------------------------------------*
* Ajusta campo AWKEY
*&---------------------------------------------------------------------*
  LOOP AT t_faturar ASSIGNING FIELD-SYMBOL(<fs_awkey>).
    UNPACK <fs_awkey>-fat_sub TO <fs_awkey>-fat_sub.
    <fs_awkey>-awkey = <fs_awkey>-fat_sub.
  ENDLOOP.

*&---------------------------------------------------------------------*
* Cabeçalho do documento contábil
*&---------------------------------------------------------------------*
  DATA(t_faturar_tmp) = t_faturar.
  DELETE t_faturar_tmp WHERE awkey = space OR fina_stat = abap_true. "Não trazer os finalizados - PSA 10214

  IF t_faturar_tmp[] IS NOT INITIAL.

    SELECT belnr bukrs gjahr awkey
      FROM bkpf
      INTO TABLE t_bkpf
      FOR ALL ENTRIES IN t_faturar_tmp
      WHERE awkey = t_faturar_tmp-awkey.

  ENDIF.

  LOOP AT t_faturar ASSIGNING FIELD-SYMBOL(<fs_zlest0194>) WHERE kunnr_ov EQ <fs_report>-kunnr_ov .
    lv_tabix = sy-tabix.

    SELECT SINGLE docnum code
      FROM j_1bnfdoc
      INTO CORRESPONDING FIELDS OF lw_bnfdoc
      WHERE docnum EQ <fs_zlest0194>-docnum_sub.

    IF sy-subrc EQ 0 AND
       lw_bnfdoc-code NE '100'.                 "Status SEFAZ CT-e Aprovado
      DELETE t_faturar INDEX lv_tabix.
      CONTINUE.
    ENDIF.

    APPEND INITIAL LINE TO t_saida_fn[] ASSIGNING FIELD-SYMBOL(<fs_saida_fn>).

    MOVE-CORRESPONDING <fs_zlest0194> TO <fs_saida_fn>.

*&---------------------------------------------------------------------*
*Dif. Peso
*&---------------------------------------------------------------------*
    <fs_saida_fn>-zpeso_diferenca = <fs_zlest0194>-qt_carga_cte - <fs_zlest0194>-qt_descarga_cte.

*&---------------------------------------------------------------------*
*Quebra
*&---------------------------------------------------------------------*
    <fs_saida_fn>-zquebra         = <fs_saida_fn>-zpeso_diferenca.

*&---------------------------------------------------------------------*
* Valor de Quebra
*&---------------------------------------------------------------------*
* Somente  realizar  este  cálculo se  a “quebra” for positiva
    IF <fs_saida_fn>-zquebra > 1.
      <fs_saida_fn>-zvlr_quebra = ( ( <fs_zlest0194>-valor_prestacao / <fs_zlest0194>-qt_carga_cte ) * <fs_saida_fn>-zquebra ).
    ENDIF.

*&---------------------------------------------------------------------*
*Perda
*&---------------------------------------------------------------------*
    vl_perdax = ( ( <fs_zlest0194>-qt_carga_cte * v_perc ) / 100 ).

    <fs_saida_fn>-zperda = <fs_saida_fn>-zquebra - vl_perdax.

*&---------------------------------------------------------------------*
* Valor de Perda
*&---------------------------------------------------------------------*

*** BUG 58520 - Inicio
    IF <fs_saida_fn>-zpeso_diferenca = 0.
      CLEAR <fs_saida_fn>-zperda.
    ELSE.
      IF <fs_saida_fn>-zpeso_diferenca < <fs_saida_fn>-zperda.
        CLEAR <fs_saida_fn>-zperda.
      ELSE.
        IF  <fs_saida_fn>-zpeso_diferenca > <fs_saida_fn>-zperda.
* considerar valor de perda maior que a tolerância
          IF <fs_saida_fn>-zperda > 1.
            <fs_saida_fn>-zvlr_perda = ( <fs_zlest0194>-vl_total_merc / <fs_zlest0194>-qt_carga_cte ) * <fs_saida_fn>-zperda.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
*** BUG 58520 - Fim

*** BUG 58520 - Inicio
*    * considerar valor de perda maior que a tolerância
*      IF <fs_saida_fn>-zperda > 1.
*        <fs_saida_fn>-zvlr_perda = ( <fs_zlest0194>-vl_total_merc / <fs_zlest0194>-qt_carga_cte ) * <fs_saida_fn>-zperda.
*      ENDIF.
*** BUG 58520 - Fim
*&---------------------------------------------------------------------*
*Valor Liquido
*&---------------------------------------------------------------------*
*    VL_LIQ = VL_FRETE - VL_QUEBRA - VL_PERDA.
    <fs_saida_fn>-zvlr_liq_receber = ( <fs_zlest0194>-valor_prestacao - <fs_saida_fn>-zvlr_quebra - <fs_saida_fn>-zvlr_perda ).

    READ TABLE t_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>)
                      WITH KEY bukrs = <fs_zlest0194>-bukrs_ov
                               gjahr = <fs_zlest0194>-gjahr_vf
                               awkey = <fs_zlest0194>-awkey.
    IF sy-subrc IS INITIAL.
      <fs_saida_fn>-nr_doc_contabil = <fs_bkpf>-belnr.
    ENDIF.

  ENDLOOP.

  IF lcl_alv_208 IS NOT INITIAL.
    CALL METHOD lcl_alv_208->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_CTE_FG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_busca_cte_fg .

  REFRESH: t_saida_fg, t_faturar, t_bkpf.

  CHECK wg_fat_cab-cod_cliente IS NOT INITIAL.

  IF r_1 IS NOT INITIAL. "TODOS

    SELECT *
      FROM zlest0194
      INTO TABLE t_faturar
      WHERE nr_cte_sub    IN s_numr
        AND dt_mov_ov     IN s_data
        AND chave_cte_sub IN s_chave
        AND bukrs_ov      IN s_bukrs
        AND branch_ov     IN s_branch
        AND kunnr_ov      IN s_kunnr
        AND kunnr_cnpj    IN s_cnpj
        AND nr_fatura     <> space.
    "        OR fina_stat      = abap_true
*        AND AUGBL         EQ SPACE
*        AND OBJ_KEY_PERDA = SPACE.

  ENDIF.

  IF r_2 IS NOT INITIAL. "PENDENTES

    SELECT *
      FROM zlest0194
      INTO TABLE t_faturar
      WHERE nr_cte_sub    IN s_numr
        AND dt_mov_ov     IN s_data
        AND chave_cte_sub IN s_chave
        AND bukrs_ov      IN s_bukrs
        AND branch_ov     IN s_branch
        AND kunnr_ov      IN s_kunnr
        AND kunnr_cnpj    IN s_cnpj
        AND nr_fatura     <> space
        AND augbl         EQ space
        AND obj_key_perda = space
        AND fina_stat     = abap_false. "Não trazer os finalizados - PSA 10214
  ENDIF.

  IF r_3 = abap_true. "Concluídos

    SELECT *
      FROM zlest0194
      INTO TABLE t_faturar
      WHERE nr_cte_sub    IN s_numr
        AND dt_mov_ov     IN s_data
        AND chave_cte_sub IN s_chave
        AND bukrs_ov      IN s_bukrs
        AND branch_ov     IN s_branch
        AND kunnr_ov      IN s_kunnr
        AND kunnr_cnpj    IN s_cnpj
        AND nr_fatura     <> space
        AND augbl         EQ space
        AND obj_key_perda <> space
      OR fina_stat      = abap_true.

  ENDIF.

*&---------------------------------------------------------------------*
* Ajusta campo AWKEY
*&---------------------------------------------------------------------*
  LOOP AT t_faturar ASSIGNING FIELD-SYMBOL(<fs_awkey>).
    UNPACK <fs_awkey>-fat_sub TO <fs_awkey>-fat_sub.
    <fs_awkey>-awkey = <fs_awkey>-fat_sub.
  ENDLOOP.

* Cabeçalho do documento contábil
  DATA(t_faturar_tmp) = t_faturar.
  DELETE t_faturar_tmp WHERE awkey = space OR fina_stat = abap_true. "Não trazer os finalizados - PSA 10214

  IF t_faturar_tmp[] IS NOT INITIAL.

    SELECT belnr bukrs gjahr awkey
      FROM bkpf
      INTO TABLE t_bkpf
      FOR ALL ENTRIES IN t_faturar_tmp
      WHERE bukrs IN s_bukrs
        AND awkey = t_faturar_tmp-awkey.

  ENDIF.

  LOOP AT t_faturar ASSIGNING FIELD-SYMBOL(<fs_zlest0194>) WHERE kunnr_ov EQ <fs_report>-kunnr_ov.

    APPEND INITIAL LINE TO t_saida_fg[] ASSIGNING FIELD-SYMBOL(<fs_saida_fg>).
    MOVE-CORRESPONDING <fs_zlest0194> TO <fs_saida_fg>.

    IF <fs_zlest0194>-email IS NOT INITIAL.
      <fs_saida_fg>-email = gc_icones-email.
    ENDIF.

    IF <fs_zlest0194>-nr_fatura IS NOT INITIAL.
      <fs_saida_fg>-status = gc_icones-flag_green.
    ENDIF.


    IF <fs_zlest0194>-obj_key_perda IS NOT INITIAL OR <fs_zlest0194>-fina_stat = abap_true.
      <fs_saida_fg>-status = gc_icones-flag_complete.
    ENDIF.

    <fs_saida_fg>-obj_key_perda = <fs_zlest0194>-obj_key_perda(10).
    <fs_saida_fg>-ano_key_perda = <fs_zlest0194>-obj_key_perda+10(4).


    READ TABLE t_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>)
                    WITH KEY bukrs = <fs_zlest0194>-bukrs_ov
                             gjahr = <fs_zlest0194>-gjahr_vf
                             awkey = <fs_zlest0194>-awkey.
    IF sy-subrc IS INITIAL.
      <fs_saida_fg>-nr_doc_contabil = <fs_bkpf>-belnr.
      <fs_saida_fg>-ano_doc_contabil = <fs_bkpf>-gjahr.
    ENDIF.

  ENDLOOP.

  IF lcl_alv_209 IS NOT INITIAL.
    CALL METHOD lcl_alv_209->refresh_table_display.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DESATRIBUIR_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_desatribuir_fatura .

  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_delete       TYPE TABLE OF zcot0013,
        t_saida_fg_aux TYPE TABLE OF ty_faturar_fg,

        l_linhas       TYPE sy-tabix,
        l_nr_fatura    TYPE zlest0194-nr_fatura,
        l_answer       TYPE c.

  DATA: w_faturar LIKE LINE OF t_faturar,
        w_row_no  LIKE LINE OF t_row_no.

  REFRESH: t_faturar.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.

  IF l_linhas = 0.
    MESSAGE s000(z_les) WITH 'Selecionar uma linha'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  REFRESH: t_saida_fg_aux.
  LOOP AT t_row_no INTO w_row_no.
    READ TABLE  t_saida_fg INTO DATA(w_saida_fg) INDEX w_row_no-row_id.
    IF sy-subrc IS INITIAL.
      APPEND w_saida_fg TO t_saida_fg_aux.
    ENDIF.
  ENDLOOP.


  DELETE t_saida_fg_aux WHERE status <> gc_icones-flag_green.
  DESCRIBE TABLE t_saida_fg_aux LINES l_linhas.
  IF l_linhas = 0.
    MESSAGE s000(z_les) WITH 'Não é permitido, desatribuir regist. contabilizados'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Busca Versão atual dos registros na tabela Z
  CHECK t_saida_fg_aux[] IS NOT INITIAL.

  SELECT * FROM zlest0194
    INTO TABLE t_faturar
    FOR ALL ENTRIES IN t_saida_fg_aux
    WHERE chave_xml_cte = t_saida_fg_aux-chave_xml_cte.

  LOOP AT  t_faturar ASSIGNING FIELD-SYMBOL(<fs_faturar>).

    CLEAR: <fs_faturar>-nr_fatura, <fs_faturar>-item_fatura,
           <fs_faturar>-data_vencimento, <fs_faturar>-email.

    IF <fs_faturar>-obj_key_perda IS NOT INITIAL.
      DELETE t_faturar INDEX sy-tabix.
    ENDIF.

  ENDLOOP.

  IF t_faturar[] IS NOT INITIAL.
    MODIFY zlest0194 FROM TABLE t_faturar.
    COMMIT WORK.
    MESSAGE s000(z_les) WITH 'Fatura '(027) l_nr_fatura ' desatribuida com sucesso.'(029).
  ENDIF.

  PERFORM zf_busca_cte_fn.
  PERFORM zf_busca_cte_fg.

  CALL METHOD lcl_alv_209->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0209  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0209 INPUT.

  CASE v_ucomm.
    WHEN 'BTN_DESATR'.
      PERFORM zf_desatribuir_fatura.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FILTRO_FATURAS_GERAS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE filtro_faturas_geras INPUT.

  REFRESH: r_nrfatura. CLEAR r_nrfatura.

  IF  wg_fat_cab_fg-nr_fatura IS NOT INITIAL.
    r_nrfatura-sign   = 'I'.
    r_nrfatura-option = 'EQ'.
    r_nrfatura-low    = wg_fat_cab_fg-nr_fatura.
    APPEND r_nrfatura.
  ENDIF.

  REFRESH: r_data_vcto. CLEAR r_data_vcto.
  IF wg_fat_cab_fg-data_vcto IS NOT INITIAL.
    r_data_vcto-sign   = 'I'.
    r_data_vcto-option = 'EQ'.
    r_data_vcto-low    = wg_fat_cab_fg-data_vcto.
    APPEND r_data_vcto.
  ENDIF.

* Busca dados de Faturas já Geradas
  PERFORM zf_busca_cte_fg.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDA_DATA_VCTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_data_vcto OUTPUT.
*  CLEAR WG_FAT_CAB-DATA_VCTO.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0208  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0208 INPUT.

  DATA: l_valid TYPE c,
        l_data  TYPE sy-datum,
        l_ucomm TYPE sy-ucomm.

  CASE v_ucomm.

    WHEN 'BTN_ATR'.

      IF wg_fat_cab_fn-nr_fatura IS NOT INITIAL.
        UNPACK wg_fat_cab_fn-nr_fatura TO wg_fat_cab_fn-nr_fatura.
        SELECT SINGLE data_vencimento FROM zlest0194
          INTO ( wg_fat_cab_fn-data_vcto )
          WHERE nr_fatura = wg_fat_cab_fn-nr_fatura.
      ENDIF.

      IF wg_fat_cab_fn-data_vcto IS INITIAL.
        MESSAGE s000(z_les) WITH 'Data de Vencimento não informada'(030)
                                    DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      l_data = wg_fat_cab_fn-data_vcto.
      PERFORM zf_valida_data USING l_data wg_fat_cab_fn-data_vcto.

      IF wg_fat_cab_fn-data_vcto  < sy-datum .
        MESSAGE s000(z_les) WITH 'Data de Vencimento inválida'(036)
                                    DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF l_data <> wg_fat_cab_fn-data_vcto.
        MESSAGE s000(z_les) WITH 'Data de Vencimento não é dia útil. Próxima data '(043) wg_fat_cab_fn-data_vcto
                            DISPLAY LIKE 'W'.
        RETURN.
      ELSE.
        PERFORM zf_atribuir_fatura.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_OCULTAR_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_ocultar_coluna .
  <fs_fcat>-no_out      = abap_true.
  <fs_fcat>-tech      = abap_true.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_envia_email .

  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_delete       TYPE TABLE OF zcot0013,
        t_saida_fg_aux TYPE TABLE OF ty_faturar_fg,
        w_row_no       LIKE LINE OF t_row_no,

        l_linhas       TYPE sy-tabix,
        l_answer       TYPE c.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
*    "Selecionar apenas uma linha
*    MESSAGE I000(Z_MM) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
*    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.
    READ TABLE  t_saida_fg INTO DATA(w_saida_fg) INDEX w_row_no-row_id.
    APPEND w_saida_fg TO t_saida_fg_aux.
  ENDLOOP.

  SORT t_saida_fg_aux BY nr_fatura.
  DELETE ADJACENT DUPLICATES FROM t_saida_fg_aux COMPARING nr_fatura.

  REFRESH: t_faturar.
  LOOP AT t_saida_fg_aux ASSIGNING FIELD-SYMBOL(<fs_saida_fg_aux>).

* Busca todos os itens da Fatura
    REFRESH: t_faturar.
    SELECT * FROM zlest0194
      INTO TABLE t_faturar
       WHERE nr_fatura = <fs_saida_fg_aux>-nr_fatura.

    IF t_faturar[] IS NOT INITIAL.
      DATA: enviado TYPE sofolenti1-object_id.
      CALL FUNCTION 'ZFFLES_EMAIL_FATURA_GERADAS'
        EXPORTING
          i_cod_cliente = wg_fat_cab-cod_cliente
          i_nro_fatura  = <fs_saida_fg_aux>-nr_fatura
        IMPORTING
          e_enviado     = enviado
        TABLES
          gt_saida      = t_faturar.
      DATA: texto TYPE string.

      IF enviado EQ 'SUCESSO'.
        MESSAGE 'E-mail enviado com sucesso!' TYPE 'S'.
      ELSE.
        CONCATENATE 'E-mail do Cliente'  enviado 'não foi encontrado!' INTO texto SEPARATED BY space.
        MESSAGE texto TYPE 'S' DISPLAY LIKE 'E'.
        CONTINUE.

      ENDIF.

    ENDIF.

  ENDLOOP.

* Busca dados de Faturas já Geradas
  PERFORM zf_busca_cte_fg.

ENDFORM.




FORM zf_visualizar_email.

  "MESSAGE 'Corpo do email aqui' TYPE 'I' DISPLAY LIKE 'E'.
*----------------------------------------------------------------------
* Declaração de tipos
*----------------------------------------------------------------------
  TYPES: BEGIN OF y_files,
           fname(10) TYPE c,
         END OF y_files.

  TYPES: BEGIN OF y_fatura,
           nr_fatura(20)        TYPE c, "ZLEST0194-NR_fatura,
           nr_cte_sub(20)       TYPE c, "ZLEST0194-NR_CTE_SUB,
           qt_descarga_cte(20)  TYPE c, "ZLEST0194-QT_DESCARGA_CTE,
           valor_prestacao(20)  TYPE c, "ZLEST0194-VALOR_PRESTACAO,
           zvlr_quebra(20)      TYPE c, "ZLEST0194-ZVLR_quebra
           zvlr_perda(20)       TYPE c, "ZLEST0194-ZVLR_PERDA
           zvlr_liq_receber(20) TYPE c, "ZLEST0194-ZVLR_LIQ_RECEBER,
           data_vencimento(20)  TYPE c, "ZLEST0194-DATA_VENCIMENTO,
         END OF y_fatura.

  TYPES: BEGIN OF y_soma,
           nr_fatura(20)        TYPE c,
           qt_descarga_cte(20)  TYPE c,
           valor_prestacao(20)  TYPE c,
           zvlr_liq_receber(20) TYPE c,
         END OF y_soma.

  TYPES: BEGIN OF y_collect,
           nr_fatura        TYPE zlest0194-nr_fatura,
           qt_descarga_cte  TYPE zlest0194-qt_descarga_cte,
           valor_prestacao  TYPE zlest0194-valor_prestacao,
           zvlr_liq_receber TYPE zlest0194-zvlr_liq_receber,
         END OF y_collect.

  TYPES: BEGIN OF y_email,
           kunnr     TYPE kna1-kunnr,
           adrnr     TYPE kna1-adrnr,
           smtp_addr TYPE adr6-smtp_addr,
         END OF y_email.

*----------------------------------------------------------------------
* Declaração de tabela Interna
*----------------------------------------------------------------------
  DATA: gt_objtxt   TYPE TABLE OF solisti1,
        gt_objpack  TYPE TABLE OF sopcklsti1,
        gt_fatura   TYPE TABLE OF y_fatura,
        gt_collect  TYPE TABLE OF y_collect,
        gt_soma     TYPE TABLE OF y_soma,
        gt_receiver TYPE TABLE OF somlreci1,
        gt_email    TYPE TABLE OF y_email.

*----------------------------------------------------------------------
* Declaração de Estrutura
*----------------------------------------------------------------------
  DATA: gs_fatura   LIKE LINE OF gt_fatura,
        gs_soma     LIKE LINE OF gt_soma,
        gs_collect  LIKE LINE OF gt_collect,
        gs_docdata  TYPE sodocchgi1,
        gs_objtxt   LIKE LINE OF gt_objtxt,
        gs_objpack  LIKE LINE OF gt_objpack,
        gs_receiver LIKE LINE OF gt_receiver.

*----------------------------------------------------------------------
* Declaração de Variáveis
*----------------------------------------------------------------------
  DATA: gv_nro_fatura           TYPE i,
        gv_rec_cnt              TYPE c LENGTH 10,
        gv_lines                TYPE i,
        gv_sentall              TYPE sonv-flag,
        gv_nobjid               TYPE sofolenti1-object_id,
        gv_qt_descarga_cte(20)  TYPE c,
        gv_zvlr_liq_receber(20) TYPE c.




  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_delete       TYPE TABLE OF zcot0013,
        t_saida_fg_aux TYPE TABLE OF ty_faturar_fg,
        w_row_no       LIKE LINE OF t_row_no,

        l_linhas       TYPE sy-tabix,
        l_answer       TYPE c.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
*    "Selecionar apenas uma linha
*    MESSAGE I000(Z_MM) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
*    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.
    READ TABLE  t_saida_fg INTO DATA(w_saida_fg) INDEX w_row_no-row_id.
    APPEND w_saida_fg TO t_saida_fg_aux.
  ENDLOOP.

  SORT t_saida_fg_aux BY nr_fatura.
  DELETE ADJACENT DUPLICATES FROM t_saida_fg_aux COMPARING nr_fatura.

  REFRESH: t_faturar.
  LOOP AT t_saida_fg_aux ASSIGNING FIELD-SYMBOL(<fs_saida_fg_aux>).

* Busca todos os itens da Fatura
    REFRESH: t_faturar.
    SELECT * FROM zlest0194
      INTO TABLE t_faturar
       WHERE nr_fatura = <fs_saida_fg_aux>-nr_fatura.

    IF t_faturar[] IS NOT INITIAL.

*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

      gv_nro_fatura  = <fs_saida_fg_aux>-nr_fatura.
      gv_rec_cnt = gv_nro_fatura.

      LOOP AT t_faturar INTO DATA(gs_saida_fg).

        gs_fatura-nr_fatura         = gs_saida_fg-nr_fatura.
        PACK gs_fatura-nr_fatura TO gs_fatura-nr_fatura.

        gs_fatura-nr_cte_sub        = gs_saida_fg-nr_cte_sub.
        PACK gs_fatura-nr_cte_sub TO gs_fatura-nr_cte_sub.

        gs_fatura-qt_descarga_cte   = gs_saida_fg-qt_descarga_cte.
        REPLACE '.'  WITH ',' INTO  gs_fatura-qt_descarga_cte.
        CONDENSE gs_fatura-qt_descarga_cte.

        gs_fatura-valor_prestacao   = gs_saida_fg-valor_prestacao.
        REPLACE '.'  WITH ',' INTO  gs_fatura-valor_prestacao.
        CONDENSE gs_fatura-valor_prestacao.

        gs_fatura-zvlr_quebra   = gs_saida_fg-zvlr_quebra.
        REPLACE '.'  WITH ',' INTO  gs_fatura-zvlr_quebra.
        CONDENSE gs_fatura-zvlr_quebra.

        gs_fatura-zvlr_perda   = gs_saida_fg-zvlr_perda.
        REPLACE '.'  WITH ',' INTO  gs_fatura-zvlr_perda.
        CONDENSE gs_fatura-zvlr_perda.

        gs_fatura-zvlr_liq_receber  = gs_saida_fg-zvlr_liq_receber.
        REPLACE '.'  WITH ',' INTO  gs_fatura-zvlr_liq_receber.
        CONDENSE gs_fatura-zvlr_liq_receber.

        CONCATENATE gs_saida_fg-data_vencimento+6(2) '/'
                    gs_saida_fg-data_vencimento+4(2) '/'
                    gs_saida_fg-data_vencimento(4) INTO  gs_fatura-data_vencimento.

        APPEND gs_fatura TO gt_fatura.

        MOVE-CORRESPONDING gs_saida_fg TO gs_collect.
        COLLECT gs_collect INTO gt_collect.

      ENDLOOP.

* ******************Prepare Corpo do E-mail*******************************************
*MOVE '<body style="background-color: #D7ECF3;">' TO GS_OBJTXT-LINE. " Plano de Fundo
*APPEND GS_OBJTXT TO GT_OBJTXT.
      email_string = '<!DOCTYPE HTML>' &&
           '<html>' &&
           '<head>' &&
           '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' &&
           '<meta name="viewport" content="width=device-width, initial-scale=1">' &&
           '<title>Highcharts Example</title>' &&
           '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"' &&
           'integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">' &&
           '<style type="text/css">html{overflow-x:hidden;}</style>' &&
           '</head>' &&

           '<body>'.

      email_string = email_string && '<BR>' &&
      'Aos  Cuidados do  depto de  contas a  pagar.' && '<BR><BR>' &&
      'Segue  fatura: ' && gv_rec_cnt && '<BR><BR>' &&
      '<TABLE BORDER=1>' &&
       '<TR style="background-color: #96A5AA;"> <TH> Documento </TH> ' &&
       '<TH> Peso Descarga </TH> ' &&
       '<TH> Valor Frete</TH> ' &&
       '<TH> Valor Quebra</TH> ' &&
       '<TH> Valor Perda</TH> ' &&
       '<TH> Valor Líquido </TH>' &&
       '<TH> Vencimento </TH></TR>' .






*      MOVE '<BR>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.
*      CLEAR gs_objtxt.

*      CONCATENATE 'Aos  Cuidados do  depto de  contas a  pagar.'(011) '<BR><BR>'
*      INTO gs_objtxt-line SEPARATED BY space.
*      APPEND gs_objtxt TO gt_objtxt.
*      CLEAR gs_objtxt.

*MOVE GV_TOT_CNT TO GV_REC_CNT.

*      CONCATENATE 'Segue  fatura: '(012) gv_rec_cnt '<BR><BR>' INTO gs_objtxt-line SEPARATED BY space.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.
*
*      MOVE '<TABLE BORDER=1>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col1
*      MOVE '<TR style="background-color: #96A5AA;"> <TH> Documento </TH> ' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col2
*      MOVE '<TH> Peso Descarga </TH> ' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col3
*      MOVE '<TH> Valor Frete</TH> ' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col4
*      MOVE '<TH> Valor Quebra</TH> ' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col5
*      MOVE '<TH> Valor Perda</TH> ' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col6
*      MOVE '<TH> Valor Líquido </TH>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col7
*      MOVE '<TH> Vencimento </TH></TR>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.


      LOOP AT gt_fatura INTO gs_fatura.


*col1
        IF gs_fatura-nr_cte_sub IS NOT INITIAL.
          CONCATENATE '<TR> <TD>' gs_fatura-nr_cte_sub '</TD>' INTO email.

        ELSE.
          MOVE '<TR> <TD></TD>' TO email.

        ENDIF.


        email_string = email_string && email &&
        '<TD>' && gs_fatura-qt_descarga_cte && '</TD>' &&
        '<TD>' && gs_fatura-valor_prestacao && '</TD>' &&
        '<TD>' && gs_fatura-zvlr_quebra && '</TD>' &&
        '<TD>' && gs_fatura-zvlr_perda && '</TD>' &&
        '<TD>' && gs_fatura-zvlr_liq_receber && '</TD>' &&
        '<TD>' && gs_fatura-data_vencimento && '</TD></TR>'.


**col2
*        CONCATENATE '<TD>' gs_fatura-qt_descarga_cte '</TD>' INTO gs_objtxt-line.
*        APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.
*
**col3
*        CONCATENATE '<TD>' gs_fatura-valor_prestacao '</TD>' INTO gs_objtxt-line.
*        APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.
*
**col4
*        CONCATENATE '<TD>' gs_fatura-zvlr_liq_receber '</TD>' INTO gs_objtxt-line.
*        APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col5
*        CONCATENATE '<TD>' gs_fatura-zvlr_liq_receber '</TD>' INTO gs_objtxt-line.
*        APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col6
*        CONCATENATE '<TD>' gs_fatura-zvlr_liq_receber '</TD>' INTO gs_objtxt-line.
*        APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col7
*        CONCATENATE '<TD>' gs_fatura-data_vencimento '</TD></TR>' INTO gs_objtxt-line.
*        APPEND gs_objtxt TO gt_objtxt.CLEAR gs_objtxt.

      ENDLOOP.

      READ TABLE gt_collect INTO gs_collect INDEX 1.

      gs_soma-nr_fatura          = gs_collect-nr_fatura.
      gs_soma-qt_descarga_cte    = gs_collect-qt_descarga_cte.
      gs_soma-valor_prestacao    = gs_collect-valor_prestacao.
      gs_soma-zvlr_liq_receber   = gs_collect-zvlr_liq_receber.

* Monta Linha de Totalização
      CLEAR gv_qt_descarga_cte.
      gv_qt_descarga_cte = gs_soma-qt_descarga_cte.

      REPLACE '.' WITH ',' INTO gv_qt_descarga_cte.
      CONDENSE gv_qt_descarga_cte.

*col6
      CLEAR gv_zvlr_liq_receber.
      gv_zvlr_liq_receber = gs_soma-zvlr_liq_receber.
      REPLACE '.' WITH ',' INTO gv_zvlr_liq_receber.
      CONDENSE gv_zvlr_liq_receber.

      email_string = email_string &&
      '<TR style="background-color: #FFFF00;">' &&
      '<TH> &Sigma; Total </TH> ' &&
       '<TD>' && gv_qt_descarga_cte && '</TD>' &&
       '<TD>&nbsp;</TD>' &&
       '<TD>&nbsp;</TD>' &&
       '<TD>&nbsp;</TD>' &&
       '<TD>' && gv_zvlr_liq_receber && '</TD>' &&
       '<TD>&nbsp;</TD>' &&
       '</TR>' &&
       '</TABLE>' &&
       '<BR>' && 'Atenciosamente' && '<BR>' &&
       '<BR><BR>' &&
       '<TABLE><TH>*** This is an auto generated mail from '  && sy-sysid && sy-mandt && ' system, Do not reply ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' &&
       '</body>' &&
    '</html>'.



*col1
*      MOVE '<TR style="background-color: #FFFF00;"> <TH> &Sigma; Total </TH> ' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.

*col2

*      CONCATENATE '<TD>' gv_qt_descarga_cte '</TD>' INTO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.

*col3
*      MOVE '<TD>&nbsp;</TD>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.

*col4
*      MOVE '<TD>&nbsp;</TD></TR>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.

*col5
*      MOVE '<TD>&nbsp;</TD></TR>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.



*      CONCATENATE '<TD>' gv_zvlr_liq_receber '</TD>' INTO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.

*col7
*      MOVE '<TD>&nbsp;</TD></TR>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.

*      MOVE '</TABLE>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt.  CLEAR gs_objtxt.

*      CONCATENATE '<BR>' 'Atenciosamente'(013) '<BR>'
*      text-018 INTO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt. CLEAR gs_objtxt.

*      MOVE '<BR><BR>' TO gs_objtxt-line.
*      APPEND gs_objtxt TO gt_objtxt. CLEAR gs_objtxt.

* MOVE ' <TABLE><TH>*** This is an auto generated mail from SAP-HCM, Do not reply ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' TO GS_OBJTXT-LINE.
*      CONCATENATE '<TABLE><TH>*** This is an auto generated mail from ' sy-sysid sy-mandt 'system, Do not reply ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO gs_objtxt-line SEPARATED BY space..
*      APPEND gs_objtxt TO gt_objtxt. CLEAR gs_objtxt.
    ENDIF.
  ENDLOOP.



ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_PARAMETROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_call_transaction_parametros .
  CALL TRANSACTION 'ZSDT0173'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CONTABILIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_contabilizacao .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,
        l_belnr      LIKE bkpf-belnr.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    MESSAGE s000(z_les) WITH 'Selecionar um documento contábil'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    REFRESH t_contabil.
    READ TABLE t_saida_fg INTO DATA(w_saida_fg)
                                        INDEX  w_row_no-row_id.

* Prepara Contabilização
    PERFORM zf_montar_itens USING w_saida_fg-nr_fatura
                                  w_saida_fg-item_fatura.

* Gera Contabilização
    IF t_contabil[] IS NOT INITIAL.
      PERFORM zf_gera_contabilizacao CHANGING l_belnr .
    ENDIF.

  ENDLOOP.

* Busca dados de Faturas já Geradas
  PERFORM zf_busca_cte_fg.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_FINALIZACAO_SEM_CONTABILIZACAO_LANÇAMENTO
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_finasem_contab_lanc.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,
        l_belnr      LIKE bkpf-belnr.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  "COMENTADO - Equalização ECC X HANA #115197   - SMC
*  IF l_linhas = 0.
*    MESSAGE s000(z_les) WITH 'Selecionar uma Linha'
*      DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
  "COMENTADO - Equalização ECC X HANA #115197   - SMC
  LOOP AT t_row_no INTO w_row_no.

    REFRESH t_contabil.
    CLEAR: w_saida_fg.
    READ TABLE t_saida_fg INTO w_saida_fg
                                        INDEX  w_row_no-row_id.

    IF w_saida_fg-zpeso_diferenca > 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Alerta!'
          txt1  = 'Existem Diferenças em Quebra ou Perda.'
          txt2  = 'Favor Usar Ação Contabilizar !'.
      "RETURN.
      CONTINUE.
    ELSE.


      IF w_saida_fg-fina_stat = abap_false.
        CALL SCREEN 0211 STARTING AT 20 16 ENDING AT 140 30.
      ELSE.
        MESSAGE TEXT-t07 TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.

    ENDIF.



  ENDLOOP.

  "XXXXXX
*  IF w_saida_fg-zpeso_diferenca > 0.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*      EXPORTING
*        titel = 'Alerta!'
*        txt1  = 'Existem Diferenças em Quebra ou Perda.'
*        txt2  = 'Favor Usar Ação Contabilizar !'.
*    "RETURN.
*    CONTINUE.
*  ELSE.
*
*
*    IF w_saida_fg-fina_stat = abap_false.
*      CALL SCREEN 0211 STARTING AT 20 16 ENDING AT 140 30.
*    ELSE.
*      MESSAGE text-t07 TYPE 'I' DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDIF.

  PERFORM f_faturar.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Botão Salvar e Cancelar ZF_FINALIZACAO_SEM_CONTABILIZACAO_LANÇAMENTO
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


MODULE user_command_0211 INPUT.

  CASE sy-ucomm.

    WHEN 'GRAVAR'.

      DATA: w_text TYPE char72.
      DATA: integ_comb TYPE REF TO zcl_integ_comb.
      CREATE OBJECT integ_comb.
      CLEAR: wa_editor.
      CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
      IF it_editor IS INITIAL.
        MESSAGE TEXT-t06 TYPE 'I' DISPLAY LIKE 'E'.
        txtopen = abap_true.
      ELSE.

        LOOP AT it_editor ASSIGNING FIELD-SYMBOL(<w_text>).
          w_text = |{ w_text } { <w_text>-line }|.
        ENDLOOP.

        "... 124769 CS2023000446 ZSDT0168 - Inclusão de coluna com CTe do contratante PSA

        CALL METHOD lcl_alv_209->get_selected_rows
          IMPORTING
            et_index_rows = t_index_rows
            et_row_no     = t_row_no.

        DESCRIBE TABLE t_row_no LINES l_linhas.

        SORT t_saida_fg BY zpeso_diferenca DESCENDING.

        LOOP AT t_row_no INTO w_row_no.
          CLEAR: w_saida_fg.
          READ TABLE t_saida_fg INTO w_saida_fg INDEX  w_row_no-row_id.
          IF w_saida_fg-zpeso_diferenca = 0 OR w_saida_fg-zpeso_diferenca IS INITIAL.
            IF w_saida_fg-chave_xml_cte IS NOT INITIAL.
              UPDATE zlest0194 SET fina_mot = @w_text , fina_stat = @abap_true, fina_user = @sy-uname , fina_dat = @sy-datum WHERE chave_xml_cte = @w_saida_fg-chave_xml_cte.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ENDLOOP.
        FREE: w_text,it_editor.
        PERFORM f_faturar.
      ENDIF.

    WHEN 'DESISTIR'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.




*&---------------------------------------------------------------------*
*&      Form  ZF_FINALIZACAO_SEM_CONTABILIZACAO_ESTORNO
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_finasem_contab_est.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,
        l_belnr      LIKE bkpf-belnr.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
*  IF l_linhas = 0.
*    MESSAGE s000(z_les) WITH 'Selecionar uma Linha'
*      DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
  "COMENTADO - Equalização ECC X HANA #115197   - SMC
  LOOP AT t_row_no INTO w_row_no.

    REFRESH t_contabil.
    CLEAR: w_saida_fg.
    READ TABLE t_saida_fg INTO w_saida_fg
                                        INDEX  w_row_no-row_id.
  ENDLOOP.

  IF w_saida_fg-fina_stat = abap_true.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING        "TITLEBAR = 'Confirmar'
        text_question         = 'Deseja Cancelar ?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = p_respo.

    LOOP AT t_row_no INTO w_row_no.

      REFRESH t_contabil.
      CLEAR: w_saida_fg.
      READ TABLE t_saida_fg INTO w_saida_fg
                                          INDEX  w_row_no-row_id.

      IF p_respo = 1.
        UPDATE zlest0194 SET fina_mot = '' , fina_stat = @abap_false, fina_user = @sy-uname , fina_dat = @sy-datum WHERE chave_xml_cte = @w_saida_fg-chave_xml_cte.
        COMMIT WORK.
        "CONTINUE.
      ENDIF.

    ENDLOOP.

    PERFORM f_faturar.

  ENDIF.

  IF w_saida_fg-fina_stat <> abap_true.

    MESSAGE TEXT-t08 TYPE 'I' DISPLAY LIKE 'E'.


  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MOTAR_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_montar_itens USING p_nr_fatura p_item_fatura.

  DATA: w_zsdt0254  TYPE zsdt0254,
        w_zlest0194 TYPE zlest0194.

  REFRESH: t_contabil.

* Descarga frota própria frete terceiros
  SELECT * FROM zlest0194
    INTO TABLE t_zlest0194
    WHERE nr_fatura   = p_nr_fatura
      AND item_fatura = p_item_fatura.

* De para código material
  SELECT * FROM zsdt0255
    INTO TABLE @DATA(t_zsdt0255).


  LOOP AT t_zlest0194 INTO w_zlest0194.

*-----------------------------------------------------------------------
* Valida Valores
*-----------------------------------------------------------------------
    IF w_zlest0194-zvlr_quebra IS INITIAL AND w_zlest0194-zvlr_perda IS INITIAL.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Alerta!'
          txt1  = 'Quantidade da Origem é menor ou igual a Quantidade de Chegada!'
          txt2  = 'Favor usar ação Finalizar !'.
    ELSE.


    ENDIF.

*-----------------------------------------------------------------------
* Perda / Quebra
*-----------------------------------------------------------------------
    APPEND INITIAL LINE TO t_contabil ASSIGNING FIELD-SYMBOL(<fs_contabil>).

    READ TABLE t_zsdt0255 INTO DATA(w_zsdt0255) WITH KEY operacao = ' '.

* Inserir Crédito 1
    CONCATENATE 'FP' w_zlest0194-nr_fatura
                     w_zlest0194-item_fatura
                     sy-datum(4) INTO <fs_contabil>-obj_key. "CRIAR  UMA  CHAVE FP000000000012020  (FP+18 CARACTERES SEQUENCIAL + ANO).

    <fs_contabil>-seqitem   = '000001'.
    <fs_contabil>-bschl     = '11'.
    <fs_contabil>-gsber     = w_zlest0194-branch_ov.
    <fs_contabil>-bukrs     = w_zlest0194-bukrs_ov.
    <fs_contabil>-interface = 00.
    <fs_contabil>-bktxt     = 'Ganhos e perdas'.
    <fs_contabil>-bldat     = sy-datum.
    <fs_contabil>-budat     = sy-datum. .
    <fs_contabil>-gjahr     = sy-datum(4).
    <fs_contabil>-monat     = sy-datum+4(2).
    <fs_contabil>-blart     = 'AB'.
    CONCATENATE w_zlest0194-nr_cte_sub w_zlest0194-serie_cte_sub INTO <fs_contabil>-xblnr SEPARATED BY '-'.
    <fs_contabil>-hkont   = w_zlest0194-kunnr_ov.
    <fs_contabil>-wrbtr   = w_zlest0194-zvlr_quebra + w_zlest0194-zvlr_perda.
    <fs_contabil>-waers     = 'BRL'.
    <fs_contabil>-zlsch     = 'U'.
    <fs_contabil>-bupla     = w_zlest0194-branch_ov.
    <fs_contabil>-waers_i   = 'BRL'.
    <fs_contabil>-tarifa     = w_zlest0194-zvlr_quebra + w_zlest0194-zvlr_perda.
    <fs_contabil>-matnr     = space.
    <fs_contabil>-quantity  = space.
    <fs_contabil>-zfbdt     = w_zlest0194-data_vencimento.

    CONCATENATE 'Perda/Quebra nr.Cte ' w_zlest0194-nr_cte_sub '- Série' w_zlest0194-serie_cte_sub
                                                          INTO <fs_contabil>-sgtxt.


*-----------------------------------------------------------------------
* Quebra
*-----------------------------------------------------------------------
*    IF w_zlest0194-zvlr_quebra IS INITIAL.
*      MESSAGE s000(z_les) WITH 'Valor de Quebra igual a zero'
*           DISPLAY LIKE 'E'.
**      CONTINUE.
*    ENDIF.

    IF  w_zlest0194-zvlr_quebra IS NOT INITIAL.

      READ TABLE t_zsdt0255 INTO w_zsdt0255 WITH KEY operacao     = '2' "'QUEBRA'
                                                     ds_prod_pred = w_zlest0194-ds_prod_pred.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE s000(z_les) WITH 'Produto Predominante: ' w_zlest0194-ds_prod_pred  ' não cadastrado.'
        DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO t_contabil ASSIGNING <fs_contabil>.

* Inserir débito 1
      CONCATENATE 'FP' w_zlest0194-nr_fatura
                       w_zlest0194-item_fatura
                       sy-datum(4) INTO <fs_contabil>-obj_key. "CRIAR  UMA  CHAVE FP000000000012020  (FP+18 CARACTERES SEQUENCIAL + ANO).

      <fs_contabil>-seqitem	  = '000002'.
      <fs_contabil>-bschl	    = w_zsdt0255-ch_deb.
      <fs_contabil>-gsber	    = w_zlest0194-branch_ov.
      <fs_contabil>-bukrs	    = w_zlest0194-bukrs_ov.
      <fs_contabil>-interface	= 00.
      <fs_contabil>-bktxt     = 'Ganhos e perdas'.
      <fs_contabil>-bldat     =  sy-datum.
      <fs_contabil>-budat	    = sy-datum.
      <fs_contabil>-gjahr	    = sy-datum(4).
      <fs_contabil>-monat	    = sy-datum+4(2).
      CONCATENATE w_zlest0194-nr_cte_sub w_zlest0194-serie_cte_sub INTO <fs_contabil>-xblnr SEPARATED BY '-'.
      <fs_contabil>-blart	    = 'AB'.
      <fs_contabil>-hkont     = w_zsdt0255-conta_deb.
      <fs_contabil>-wrbtr	    = w_zlest0194-zvlr_quebra.
      <fs_contabil>-waers	    = 'BRL'.
      <fs_contabil>-zlsch	    = 'U'.
      <fs_contabil>-bupla	    = w_zlest0194-branch_ov.
      <fs_contabil>-waers_i	  = 'BRL'.
      <fs_contabil>-tarifa      = w_zlest0194-zvlr_quebra.
      <fs_contabil>-matnr     = w_zsdt0255-deb_material.
      <fs_contabil>-quantity  = space.

      CONCATENATE 'Perda/Quebra nr.Cte ' w_zlest0194-nr_cte_sub '- Série' w_zlest0194-serie_cte_sub
                                                            INTO <fs_contabil>-sgtxt.

    ENDIF.
*-----------------------------------------------------------------------
* Perda
*-----------------------------------------------------------------------

*    IF w_zlest0194-zvlr_perda IS INITIAL.
*      MESSAGE s000(z_les) WITH 'Valor de Quebra igual a zero'
*           DISPLAY LIKE 'E'.
**      CONTINUE.
*    ENDIF.

    IF  w_zlest0194-zvlr_perda IS NOT INITIAL.

      READ TABLE t_zsdt0255 INTO DATA(_zsdt0255) WITH KEY operacao = '1' "'PERDA'
                                                          ds_prod_pred = w_zlest0194-ds_prod_pred.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE s000(z_les) WITH 'Produto Predominante: ' w_zlest0194-ds_prod_pred  ' não cadastrado.'
        DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO t_contabil ASSIGNING <fs_contabil>.

* Inserir débito 2
      CONCATENATE 'FP' w_zlest0194-nr_fatura
                       w_zlest0194-item_fatura
                       sy-datum(4) INTO <fs_contabil>-obj_key. "CRIAR  UMA  CHAVE FP000000000012020  (FP+18 CARACTERES SEQUENCIAL + ANO).

      <fs_contabil>-seqitem   =   '000003'.
      <fs_contabil>-bschl     =   w_zsdt0255-ch_deb. "  DA OPERAÇÃO PERDA.
      <fs_contabil>-gsber     =   w_zlest0194-branch_ov.
      <fs_contabil>-bukrs     =   w_zlest0194-bukrs_ov.
      <fs_contabil>-interface = 00.
      <fs_contabil>-bktxt     = 'Ganhos e perdas'.
      <fs_contabil>-bldat     = sy-datum.
      <fs_contabil>-budat     = sy-datum.
      <fs_contabil>-gjahr     = sy-datum(4).
      <fs_contabil>-monat     = sy-datum+4(2).
      <fs_contabil>-blart     = 'AB'.
      <fs_contabil>-hkont     = w_zsdt0255-conta_deb.
      <fs_contabil>-wrbtr     = w_zlest0194-zvlr_perda.
      <fs_contabil>-waers     = 'BRL'.
      <fs_contabil>-zlsch     = 'U'.
      <fs_contabil>-bupla     = w_zlest0194-branch_ov.
      <fs_contabil>-waers_i   = 'BRL'.
      <fs_contabil>-tarifa     = w_zlest0194-zvlr_perda.
      <fs_contabil>-matnr     = w_zsdt0255-deb_material.
      <fs_contabil>-quantity  = space.

      CONCATENATE 'Perda/Quebra nr.Cte ' w_zlest0194-nr_cte_sub '- Série' w_zlest0194-serie_cte_sub
                                                            INTO <fs_contabil>-sgtxt.

    ENDIF.

  ENDLOOP.

  "Se tiver apenas uma linha não encontrou valor de Perda e Quebra
  IF lines( t_contabil ) = 1.
    REFRESH: t_contabil.
  ENDIF.

  DELETE t_contabil WHERE obj_key = space.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_GERA_CONTABILIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_gera_contabilizacao CHANGING vobj_key.

  DATA:  dest    LIKE bdi_logsys-logsys.

  DATA: wa_kna1 TYPE kna1.
  DATA: st_vbap TYPE TABLE OF vbap WITH HEADER LINE.
  DATA: st_vbak TYPE TABLE OF vbak WITH HEADER LINE.

  DATA:
    vbudat TYPE sy-datum,
    vbldat TYPE sy-datum,
    vname1 TYPE lfa1-name1,
    vbase  TYPE zib_contabil-wrbtr.

  DATA: vg_awtyp LIKE bkpf-awtyp,
        vg_awkey LIKE bkpf-awkey,
        vg_awsys LIKE bkpf-awsys,
        vg_belnr LIKE bkpf-belnr,
        vg_bukrs LIKE bkpf-bukrs,
        vg_gjahr LIKE bkpf-gjahr,
        vg_total TYPE zib_contabil-wrbtr,
        vg_resto TYPE zib_contabil-wrbtr,
        vg_difer TYPE zib_contabil-wrbtr.

  DATA: l_tabix TYPE sy-tabix.

  REFRESH:  it_criteria,
            it_accountgl,
            it_receivable,
            it_payable,
            it_accounttax,
            it_currencyamount,
            it_extension1,
            it_bapiret,
            it_accountwt.

  CLEAR: vg_total,vg_resto,vname1.

  DATA: gd_customercpd LIKE bapiacpa09,
        gd_fica_hd     LIKE bapiaccahd.

  LOOP AT t_contabil ASSIGNING FIELD-SYMBOL(<fs_contabil>) GROUP BY <fs_contabil>-obj_key.

* Informações fixas - pré definadas para utilização da BAPI
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system = gd_documentheader-obj_sys.

    gd_documentheader-obj_type   = 'IDOC' .
    gd_documentheader-obj_key    = <fs_contabil>-obj_key.
    gd_documentheader-username   = sy-uname.
    gd_documentheader-header_txt = <fs_contabil>-bktxt.
    gd_documentheader-comp_code  = <fs_contabil>-bukrs.
    gd_documentheader-fisc_year  = <fs_contabil>-gjahr.
    gd_documentheader-doc_date   = <fs_contabil>-bldat.
    gd_documentheader-pstng_date = <fs_contabil>-budat.
    gd_documentheader-doc_type   = <fs_contabil>-blart.
    gd_documentheader-ref_doc_no = ''.
    gd_documentheader-bus_act    = 'RFBU'.
    gd_documentheader-ref_doc_no = <fs_contabil>-xblnr.

    SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ <fs_contabil>-hkont. " Dados do Cliente

*    CONCATENATE <FS_CONTABIL>-BUDAT+6(4) <FS_CONTABIL>-BUDAT+3(2) <FS_CONTABIL>-BUDAT+0(2) INTO VBUDAT.

* Preenche Item
    CLEAR l_tabix.
    LOOP AT t_contabil INTO <fs_contabil>.

      ADD 1 TO l_tabix.

* Dados da Conta
      CLEAR wa_accountgl.
      wa_accountgl-itemno_acc     = l_tabix.

      IF <fs_contabil>-bschl = '11'.

        wa_receivable-itemno_acc     = l_tabix.
        UNPACK <fs_contabil>-hkont TO wa_receivable-customer.

        wa_receivable-pymt_meth     = <fs_contabil>-zlsch.
        wa_receivable-comp_code     = <fs_contabil>-bukrs.
        wa_receivable-bus_area      = <fs_contabil>-gsber.
        wa_receivable-businessplace = <fs_contabil>-bupla.
        wa_receivable-bline_date    = <fs_contabil>-zfbdt.
        wa_receivable-item_text      = <fs_contabil>-sgtxt.
        APPEND wa_receivable TO it_receivable.

      ELSE.
        wa_accountgl-itemno_acc     = l_tabix.
        wa_accountgl-acct_type      = 'S'.
        UNPACK  <fs_contabil>-hkont TO wa_accountgl-gl_account.

        wa_accountgl-item_text      = <fs_contabil>-sgtxt.
        wa_accountgl-bus_area       = <fs_contabil>-gsber.
        wa_accountgl-doc_type       = <fs_contabil>-blart.
        wa_accountgl-taxjurcode     = wa_kna1-txjcd.
        wa_accountgl-trade_id       = wa_kna1-vbund.
        APPEND wa_accountgl TO it_accountgl.

        it_criteria-itemno_acc    = l_tabix.
        it_criteria-fieldname     = 'ARTNR'.
*---> 13/06/2023 - Migração S4 - JS
*         it_criteria-character     = <fs_contabil>-matnr.
        it_criteria-character = CONV #( <fs_contabil>-matnr ).
*<--- 13/06/2023 - Migração S4 - JS

        APPEND it_criteria.

      ENDIF.

      " Moeda Resultado
      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc      = l_tabix.
      wa_currencyamount-curr_type       = '00'.
      wa_currencyamount-currency_iso    = <fs_contabil>-waers_i.
      wa_currencyamount-currency        = <fs_contabil>-waers.

      IF <fs_contabil>-bschl = '11'.
        wa_currencyamount-amt_doccur      = <fs_contabil>-wrbtr * -1.
      ELSE.
        wa_currencyamount-amt_doccur      = <fs_contabil>-wrbtr.
      ENDIF.

      APPEND wa_currencyamount TO it_currencyamount.

    ENDLOOP.

* All tables filled - now call BAPI.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK' "#EC CI_USAGE_OK[2438131]
      DESTINATION dest "#EC CI_USAGE_OK[2628704]
      EXPORTING
        documentheader = gd_documentheader
      IMPORTING
        documentheader = gd_documentheader
*       customercpd    = gd_customercpd
*       contractheader = gd_fica_hd
      TABLES
        criteria       = it_criteria
        accountgl      = it_accountgl
        currencyamount = it_currencyamount
        return         = it_bapiret
        extension1     = it_extension1.
*
    LOOP AT it_bapiret INTO DATA(w_bapiret) WHERE type           = 'E'.
      EXIT.
    ENDLOOP.

    sy-tcode              = 'FB05'.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "#EC CI_USAGE_OK[2438131]
      EXPORTING                            "#EC CI_USAGE_OK[2628704]
        documentheader    = gd_documentheader
*       customercpd       = gd_customercpd
*       contractheader    = gd_fica_hd
      IMPORTING
        obj_type          = wa_returnobj-obj_type
        obj_key           = wa_returnobj-obj_key
        obj_sys           = wa_returnobj-obj_sys
      TABLES
        criteria          = it_criteria
        accountgl         = it_accountgl
        accountreceivable = it_receivable
        accountpayable    = it_payable
        accounttax        = it_accounttax
        currencyamount    = it_currencyamount
        extension1        = it_extension1
        return            = it_bapiret
        accountwt         = it_accountwt.

    CLEAR vobj_key.

    READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'E'.
    IF ( sy-subrc NE 0 ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      COMMIT WORK.

      SELECT awtyp awkey awsys belnr bukrs gjahr
        FROM bkpf UP TO 1 ROWS
        INTO (vg_awtyp, vg_awkey, vg_awsys,
              vg_belnr, vg_bukrs, vg_gjahr)
       WHERE ( awtyp EQ wa_returnobj-obj_type )
         AND ( awkey EQ wa_returnobj-obj_key  )
         AND ( stblg = space )
        ORDER BY awtyp awkey awsys.
      ENDSELECT.

      vobj_key = vg_belnr.

      LOOP AT t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>).
        <fs_zlest0194>-obj_key_perda = vg_belnr && vg_gjahr .
      ENDLOOP.

      IF t_zlest0194[] IS NOT INITIAL.
        MODIFY zlest0194 FROM TABLE t_zlest0194.
        COMMIT WORK.
      ENDIF.

    ELSE.

      CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
        EXPORTING
          it_message = it_bapiret.

* Listar todas as mensagens de erro.
*      LOOP AT IT_BAPIRET INTO WA_BAPIRET
*           WHERE TYPE NE 'S'.
*        WRITE: /001 ST_DADOS-TKNUM,
*           013 ST_DADOS_AUX-FKNUM,
*           025 ST_DADOS_AUX-EBELN,
*           037 ST_DADOS_AUX-EBELP,
*           043 ST_DADOS_AUX-LBLNI,
*           055 WA_BAPIRET-MESSAGE(55).
*        IF WA_BAPIRET-MESSAGE+55(55) IS NOT INITIAL.
*          WRITE /055 WA_BAPIRET-MESSAGE+55(55).
*        ENDIF.
*        IF WA_BAPIRET-MESSAGE+110(55) IS NOT INITIAL.
*          WRITE /055 WA_BAPIRET-MESSAGE+110(55).
*        ENDIF.
*        IF WA_BAPIRET-MESSAGE+165(55) IS NOT INITIAL.
*          WRITE /055 WA_BAPIRET-MESSAGE+165(55).
*        ENDIF.
*      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ESTORNAR_CONTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_estornar_contab.

  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_delete       TYPE TABLE OF zcot0013,
        t_saida_fg_aux TYPE TABLE OF ty_faturar_fg,

        l_linhas       TYPE sy-tabix,
        l_answer       TYPE c,
        l_belnr        LIKE bkpf-belnr.

  DATA: tg_bdc   TYPE TABLE OF bdcdata,
        tg_msg   TYPE TABLE OF bdcmsgcoll,

        w_row_no LIKE LINE OF t_row_no,
        wg_msg   TYPE bdcmsgcoll,

        opt      TYPE ctu_params,
        vl_stblg TYPE bkpf-stblg,
        vl_data  TYPE dats.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

*----------------------------------------------------------------------*
  "Selecionar uma linha.
*----------------------------------------------------------------------*
  DESCRIBE TABLE t_row_no LINES l_linhas.

  IF l_linhas = 0.
    MESSAGE i000(z_les) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_saida_fg INTO DATA(w_saida_fg) INDEX w_row_no-row_id.

    IF sy-subrc IS INITIAL.
      APPEND w_saida_fg TO t_saida_fg_aux.
    ENDIF.
  ENDLOOP.

  LOOP AT t_saida_fg_aux INTO DATA(w_saida_fg_aux).

* Verifica documento para estorno
    SELECT SINGLE belnr, bukrs, gjahr, budat FROM bkpf
      INTO @DATA(w_doc_contab)
      WHERE belnr = @w_saida_fg_aux-obj_key_perda
       AND bukrs IN @s_bukrs
       AND gjahr = @w_saida_fg_aux-ano_key_perda.

    IF sy-subrc IS INITIAL.

      REFRESH: it_dta.
      PERFORM zf_shdb USING: 'SAPMF05A' '0105' 'X'  ' '           ' ',
                             ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
                             ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
                             ' '        ' '    ' '  'RF05A-BELNS'  w_doc_contab-belnr,
                             ' '        ' '    ' '  'BKPF-BUKRS'   w_doc_contab-bukrs,
                             ' '        ' '    ' '  'RF05A-GJAHS'  w_doc_contab-gjahr,
                             ' '        ' '    ' '  'UF05A-STGRD' '02'.

      opt-dismode = 'N'.
      CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt
              MESSAGES INTO tg_msg.

      READ TABLE tg_msg ASSIGNING FIELD-SYMBOL(<fs_msg>)
                                        WITH KEY msgtyp = 'E'.

      IF sy-subrc IS INITIAL.

        REFRESH: it_bapiret.
        LOOP AT tg_msg ASSIGNING <fs_msg>.

          APPEND INITIAL LINE TO it_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>).

          <fs_bapiret>-type       = <fs_msg>-msgtyp.
          <fs_bapiret>-id         = <fs_msg>-msgid.
          <fs_bapiret>-number     = <fs_msg>-msgnr.
          <fs_bapiret>-message_v1 = <fs_msg>-msgv1.
          <fs_bapiret>-message_v2 = <fs_msg>-msgv2.
          <fs_bapiret>-message_v3 = <fs_msg>-msgv3.
          <fs_bapiret>-message_v4 = <fs_msg>-msgv4.

        ENDLOOP.

        DELETE ADJACENT DUPLICATES FROM it_bapiret COMPARING ALL FIELDS.

        CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
          EXPORTING
            it_message = it_bapiret.

      ELSE.

* Descarga frota própria frete terceiros
        SELECT * FROM zlest0194
          INTO TABLE t_zlest0194
          WHERE nr_fatura   = w_saida_fg_aux-nr_fatura
            AND item_fatura = w_saida_fg_aux-item_fatura.

        LOOP AT t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>).
          CLEAR: <fs_zlest0194>-obj_key_perda.
        ENDLOOP.

        IF t_zlest0194[] IS NOT INITIAL.
          MODIFY zlest0194 FROM TABLE t_zlest0194.
          COMMIT WORK.
        ENDIF.

        SELECT SINGLE stblg
          FROM bkpf INTO vl_stblg
         WHERE bukrs = w_doc_contab-bukrs
           AND belnr = w_doc_contab-belnr
           AND gjahr = w_doc_contab-gjahr.

        IF sy-subrc IS INITIAL.
* Busca dados de Faturas já Geradas
          PERFORM zf_busca_cte_fg.
          MESSAGE s000(z_les) WITH 'Documento' w_doc_contab-belnr 'estornado.'.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_shdb USING p_program p_dynpro p_dynbegin p_fnam p_fval.

  DATA: wa_dta   TYPE bdcdata.

  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_dynbegin.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIF_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_FAT_CAB_FN_NRO_FATURA  text
*      <--P_L_NR_FATURA  text
*----------------------------------------------------------------------*
FORM zf_verif_fatura  USING    p_nro_fatura_old
                      CHANGING p_l_nr_fatura.

  DATA: l_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'(042)
      text_question         = 'Deseja realmente adicionar novo item a Fatura ?'(039)
      text_button_1         = 'Sim'(040)
      text_button_2         = 'Não'(041)
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF l_answer = 1.
    p_l_nr_fatura = p_nro_fatura_old.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  VALIDA_NR_FATURA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_nr_fatura INPUT.

  DATA: l_nr_fatura TYPE zlest0194-nr_fatura.

* Verifica valida número fatura
  PERFORM zf_valida_nr_fatura CHANGING l_nr_fatura.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_NR_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_valida_nr_fatura CHANGING p_l_nr_fatura.

  CHECK wg_fat_cab_fn-nr_fatura IS NOT INITIAL.

  UNPACK wg_fat_cab_fn-nr_fatura TO wg_fat_cab_fn-nr_fatura.

  SELECT  SINGLE * FROM zlest0194
    INTO @DATA(w_nr_fatura)
    WHERE nr_fatura = @wg_fat_cab_fn-nr_fatura.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s000(z_les) WITH 'Fatura não encontrada!'(044)
    DISPLAY LIKE 'E'.
*    RETURN.
  ELSE.
    wg_fat_cab_fn-data_vcto = w_nr_fatura-data_vencimento.
    PERFORM zf_verif_fatura USING wg_fat_cab_fn-nr_fatura CHANGING p_l_nr_fatura.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM zf_handle_hotspot_click_fat_fg   USING    p_index  TYPE any
                                       p_column TYPE any.

  READ TABLE t_saida_fg INTO DATA(w_saida_fg) INDEX p_index.

  CASE p_column.
    WHEN 'OBJ_KEY_PERDA'.
      PERFORM zf_fb03 USING w_saida_fg-obj_key_perda(10)
                            w_saida_fg-ano_key_perda.
    WHEN 'NR_DOC_CONTABIL'.
      PERFORM zf_fb03 USING w_saida_fg-nr_doc_contabil
                            w_saida_fg-ano_doc_contabil.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VISUALIZAR_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_visualizar_documentos .
  <fs_fcat>-hotspot = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_fb03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_fb03 USING p_obj_key_perda p_ano.

  IF p_obj_key_perda IS NOT INITIAL.

    SELECT SINGLE belnr, bukrs, gjahr FROM bkpf
      INTO @DATA(w_transaction)
      WHERE belnr =  @p_obj_key_perda
       AND bukrs  IN @s_bukrs
       AND gjahr  =  @p_ano.

    IF sy-subrc IS INITIAL.

      SET PARAMETER ID 'BLN' FIELD w_transaction-belnr.
      SET PARAMETER ID 'BUK' FIELD w_transaction-bukrs.
      SET PARAMETER ID 'GJR' FIELD w_transaction-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM zf_handle_hotspot_click    USING  p_index  TYPE any
                                       p_column TYPE any.

  READ TABLE t_report INTO DATA(w_report) INDEX p_index.

  CASE p_column.
    WHEN 'OBJ_KEY_PERDA'.
      PERFORM zf_fb03 USING w_report-obj_key_perda w_report-ano_key_perda.
    WHEN 'NR_DOC_CONTABIL'.
      PERFORM zf_fb03 USING w_report-nr_doc_contabil w_report-ano_doc_contabil.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cte .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,
        l_valid      TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.

  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
*    "Selecionar apenas uma linha
*    MESSAGE I000(Z_MM) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
*    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report
         ASSIGNING <fs_report>  INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

    CALL SCREEN 0102.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_FAT_CAB_FN_DATA_VCTO  text
*----------------------------------------------------------------------*
FORM zf_valida_data  USING p_data_base CHANGING p_nova_data.

  DATA: i_signum               TYPE c VALUE '+',
        it_datas               TYPE TABLE OF iscal_day,
        l_vdias                TYPE i,
        r_considerar_zlest0195 TYPE c,
        wa_zlest0195           TYPE zlest0195.

  p_nova_data = p_data_base.
  l_vdias  = 0.

  WHILE l_vdias = 0.

    CLEAR: it_datas[], it_datas.

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar = 'MG'
        factory_calendar = 'ZT'
        date_from        = p_nova_data
        date_to          = p_nova_data
      TABLES
        holidays         = it_datas
      EXCEPTIONS
        OTHERS           = 1.

    IF it_datas[] IS NOT INITIAL.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = p_nova_data
          days      = 1
          months    = 0
          years     = 0
          signum    = '+' "I_SIGNUM
        IMPORTING
          calc_date = p_nova_data.

    ELSE.
      l_vdias = 1.
    ENDIF.

  ENDWHILE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ABRE_SMARTFORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_abre_smartforms .


*----------------------------------------------------------------------
*Preenche Estrutura
*----------------------------------------------------------------------
  DATA: t_index_rows      TYPE lvc_t_row,                   "#EC NEEDED
        t_row_no          TYPE lvc_t_roid,
        t_fatura          TYPE zstles_fatura,
        t_custo           TYPE TABLE OF y_custo,
        t_collect_custo   TYPE TABLE OF y_custo,

        w_collect_custo   LIKE LINE OF t_collect_custo,
        w_row_no          LIKE LINE OF t_row_no,
        w_fatura          TYPE zstles_fatura,
        w_item            TYPE zstles_item,

        w_job_output_info TYPE ssfcrescl,
        w_control         TYPE ssfctrlop,
        w_options         TYPE ssfcompop,
        t_otf             TYPE itcoo OCCURS 0 WITH HEADER LINE,
        t_tline           TYPE TABLE OF tline WITH HEADER LINE,
        l_bin_filesize    TYPE i.

  DATA: l_linhas     TYPE sy-tabix,
        l_access_key TYPE j_1b_nfe_access_key,
        l_answer     TYPE c,
        l_data_de    TYPE zlest0194-dt_fatura,
        l_data_ate   TYPE zlest0194-dt_fatura,
        l_fm_name    TYPE  rs38l_fnam,
        l_last_linha TYPE sy-tfill.

  CALL METHOD lcl_alv_209->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE  t_saida_fg INTO DATA(w_saida_fg) INDEX w_row_no-row_id.
    CHECK sy-subrc IS INITIAL.

    IF w_saida_fg-nr_fatura IS INITIAL.
      MESSAGE i000(z_mm) WITH 'Item sem fatura gerada'(t05) DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
*----------------------------------------------------------------------
* Descarga frota própria frete terceiros
*----------------------------------------------------------------------
      SELECT * FROM zlest0194
        INTO TABLE @DATA(t_zlest0194)
        WHERE nr_fatura     = @w_saida_fg-nr_fatura.

      IF t_zlest0194[] IS  INITIAL.
        MESSAGE i000(z_mm) WITH 'Fatura não encontrada'(t04) DISPLAY LIKE 'E'.
        RETURN.
      ELSE.

*----------------------------------------------------------------------
* Preenche dados do Cliente
*----------------------------------------------------------------------

* Mestre de clientes (parte geral)
        IF t_zlest0194[] IS  NOT INITIAL.

          SELECT kunnr, name1, stcd1, stcd3, adrnr FROM kna1
            INTO TABLE @DATA(t_kna1)
            FOR ALL ENTRIES IN @t_zlest0194
            WHERE kunnr = @t_zlest0194-kunnr_ov.

* Endereços (administração de endereços central)
          SELECT * FROM t001
           INTO TABLE @DATA(t_t001)
            FOR ALL ENTRIES IN @t_zlest0194
            WHERE bukrs = @t_zlest0194-bukrs_ov.

*        SELECT * FROM adrc
*         APPENDING TABLE t_adrc
*          FOR ALL ENTRIES IN t_t001
*          WHERE addrnumber = t_t001-adrnr.

* Local de negócios
          SELECT * FROM j_1bbranch
            INTO TABLE @DATA(t_branch)
            FOR ALL ENTRIES IN @t_zlest0194
            WHERE bukrs = @t_zlest0194-bukrs_ov
              AND branch = @t_zlest0194-branch_ov.

          SELECT * FROM zib_cte_dist_ter
            INTO TABLE @DATA(t_zib_cte_dist_ter)
            FOR ALL ENTRIES IN @t_zlest0194
            WHERE cd_chave_cte = @t_zlest0194-chave_xml_cte.

          SELECT * FROM zib_cte_dist_n01
            INTO TABLE @DATA(t_zib_cte_dist_n01)
            FOR ALL ENTRIES IN @t_zlest0194
            WHERE cd_chave_cte = @t_zlest0194-chave_xml_cte.

        ENDIF.

* Endereços (administração de endereços central)
        IF t_kna1[] IS NOT INITIAL.

          SELECT * FROM adrc
           INTO TABLE @DATA(t_adrc)
            FOR ALL ENTRIES IN @t_kna1
            WHERE addrnumber = @t_kna1-adrnr.

        ENDIF.

        IF t_branch[] IS NOT INITIAL.

          SELECT * FROM adrc
           APPENDING TABLE t_adrc
            FOR ALL ENTRIES IN t_branch
            WHERE addrnumber = t_branch-adrnr.

        ENDIF.

      ENDIF.

      IF t_zib_cte_dist_ter[] IS NOT INITIAL.
        SELECT * FROM zib_cte_dist_n55
          INTO TABLE @DATA(t_zib_cte_dist_n55)
          FOR ALL ENTRIES IN @t_zib_cte_dist_ter
          WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte.
      ENDIF.

      IF t_zib_cte_dist_n55[] IS NOT INITIAL.

        DATA(t_zib_cte_dist_n55_aux2) = t_zib_cte_dist_n55.
*---> 04/07/2023 - Migração S4 - WS
        SORT  t_zib_cte_dist_n55_aux2 BY tknum.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM t_zib_cte_dist_n55_aux2 COMPARING tknum.

        SELECT p~fknum p~fkpos p~netwr p~knumv i~vbeln i~posnr i~netpr
          FROM vfkp AS p INNER JOIN vfsi AS i
          ON i~knumv = p~knumv
                  INTO TABLE t_custo
          FOR ALL ENTRIES IN t_zib_cte_dist_n55_aux2
          WHERE p~fknum = t_zib_cte_dist_n55_aux2-fknum.

        LOOP AT t_custo INTO DATA(w_custo).
          w_collect_custo-vbeln = w_custo-vbeln.
          w_collect_custo-netwr = w_custo-netwr.
          w_collect_custo-netpr = w_custo-netpr.
          COLLECT w_collect_custo INTO t_collect_custo.
        ENDLOOP.

      ENDIF.

      SORT t_zlest0194 BY dt_mov_ov.
      READ TABLE t_zlest0194 INTO DATA(w_zlest0194) INDEX 1.
      l_data_de = w_zlest0194-dt_mov_ov.

      CLEAR: w_zlest0194.
      DESCRIBE TABLE t_zlest0194 LINES l_last_linha.
      READ TABLE t_zlest0194 INTO w_zlest0194 INDEX l_last_linha.
      l_data_ate  = w_zlest0194-dt_mov_ov.

      CLEAR: w_zlest0194.
      LOOP AT t_zlest0194 INTO w_zlest0194.

        READ TABLE t_zib_cte_dist_n55 INTO  DATA(w_zib_cte_dist_n55)
                        WITH KEY cd_chave_cte = w_zlest0194-chave_xml_cte.

*----------------------------------------------------------------------
* Preenche dados de Header
*----------------------------------------------------------------------
        w_fatura-header-nr_fatura  = w_zlest0194-nr_fatura.

        IF l_data_de <> l_data_ate.
          w_fatura-header-data_de    = l_data_de.
          w_fatura-header-separador  = '-'.
          w_fatura-header-data_ate   = l_data_ate .
        ELSE.
          w_fatura-header-data_de    = l_data_de.
        ENDIF.

        w_fatura-header-dt_fatura       = w_zlest0194-dt_fatura.
        w_fatura-header-data_vencimento = w_zlest0194-data_vencimento.

        READ TABLE t_t001 INTO DATA(w_t001) WITH KEY bukrs = w_zlest0194-bukrs_ov.

        IF sy-subrc IS INITIAL.

          w_fatura-header-butxt      = w_t001-butxt.

          READ TABLE t_branch INTO DATA(w_branch) WITH KEY bukrs = w_t001-bukrs
                                                           branch = w_zlest0194-branch_ov.
          CHECK sy-subrc IS INITIAL.

          w_fatura-header-name1      = w_branch-name.

          WRITE w_branch-stcd1 TO w_fatura-header-stcd1 USING EDIT MASK '__.___.___/____-__'.
          w_fatura-header-state_insc  = w_branch-state_insc .

        ENDIF.

        READ TABLE t_adrc INTO DATA(w_adrc) WITH KEY addrnumber = w_branch-adrnr. "w_t001-adrnr.

        IF sy-subrc IS INITIAL.

          w_fatura-header-street     = w_adrc-street.
          w_fatura-header-house_num1 = w_adrc-house_num1.
          w_fatura-header-city1      = w_adrc-city1.
          w_fatura-header-city2      = w_adrc-city2.
          w_fatura-header-tel_number = w_adrc-tel_number.
          w_fatura-header-post_code1 = w_adrc-post_code1.
        ENDIF.

        READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY kunnr = w_zlest0194-kunnr_ov.

        IF sy-subrc IS INITIAL.

          w_fatura-cliente-name1       = w_kna1-name1.
          WRITE w_kna1-stcd1 TO w_fatura-cliente-stcd1 USING EDIT MASK '__.___.___/____-__'.
          w_fatura-cliente-state_insc  = w_kna1-stcd3.

*----------------------------------------------------------------------
* Preenche dados do cliente
*----------------------------------------------------------------------
          CLEAR: w_adrc.
          READ TABLE t_adrc INTO w_adrc WITH KEY addrnumber = w_kna1-adrnr.

          CHECK sy-subrc IS INITIAL.

          w_fatura-cliente-street      = w_adrc-street.
          w_fatura-cliente-house_num1  = w_adrc-house_num1.
          w_fatura-cliente-city1       = w_adrc-city1.
          w_fatura-cliente-city2       = w_adrc-city1.
          w_fatura-cliente-tel_number  = w_adrc-tel_number.
          w_fatura-cliente-post_code1  = w_adrc-post_code1.

        ENDIF.

*----------------------------------------------------------------------
* Preenche Origem\ Destino e Coleta\Entrega
*----------------------------------------------------------------------
        READ TABLE t_zib_cte_dist_ter INTO DATA(w_cte_dist_ter)
                                 WITH KEY cd_chave_cte = w_zlest0194-chave_xml_cte.

        IF sy-subrc IS INITIAL.

          w_fatura-orig_dest-origem    = w_cte_dist_ter-inicio_muni.
          w_fatura-orig_dest-destino   = w_cte_dist_ter-termino_muni.
          w_fatura-orig_dest-coleta    = w_cte_dist_ter-reme_rsocial.
          w_fatura-orig_dest-entrega   = w_cte_dist_ter-dest_rsocial.

          CLEAR: l_access_key.
          IF w_zib_cte_dist_n55-n55_chave_acesso IS NOT INITIAL.

            l_access_key     = w_zib_cte_dist_n55-n55_chave_acesso.
            w_item-n01_nr_nf = l_access_key-nfnum9.

          ELSE.

            READ TABLE t_zib_cte_dist_n01 INTO DATA(w_cte_dist_n01)
                       WITH KEY cd_chave_cte = w_zlest0194-chave_xml_cte.
            IF sy-subrc IS INITIAL.
              w_item-n01_nr_nf = w_cte_dist_n01-n01_nr_nf.
            ENDIF.

          ENDIF.
*----------------------------------------------------------------------
* Preenche lista de CTe
*----------------------------------------------------------------------
          w_item-nr_fatura        = w_zlest0194-nr_fatura.
          w_item-item_fatura      = w_zlest0194-item_fatura.
          w_item-dt_fatura        = w_zlest0194-dt_fatura.
          w_item-data_vencimento  = w_zlest0194-data_vencimento.
          w_item-kunnr_ov         = w_zlest0194-kunnr_ov.
          w_item-dt_mov_ov        = w_zlest0194-dt_mov_ov.
          w_item-nr_cte_sub       = w_zlest0194-nr_cte_sub.
          w_item-numr_cte         = w_zlest0194-numr_cte.
          w_item-qt_carga_cte     = w_zlest0194-qt_carga_cte.
          w_item-qt_descarga_cte  = w_zlest0194-qt_descarga_cte.
          w_item-valor_prestacao  = w_zlest0194-valor_prestacao.
          w_item-placa_cav        = w_zlest0194-placa_cav.

*------------------------------------------
* Peso de Perda
*------------------------------------------
          IF w_zlest0194-zperda < 0.
            w_item-zperda       = w_zlest0194-zperda * ( -1 ) .
          ELSE.
            w_item-zperda       = w_zlest0194-zperda.
          ENDIF.

*------------------------------------------
* Valor de Perda
*------------------------------------------
          IF w_zlest0194-zvlr_perda < 0.
            w_item-zvlr_perda       = w_zlest0194-zvlr_perda * ( -1 ).
          ELSE.
            w_item-zvlr_perda       = w_zlest0194-zvlr_perda.
          ENDIF.

*------------------------------------------
* Valor de Quebra
*------------------------------------------
          IF w_zlest0194-zvlr_quebra < 0.
            w_item-zvlr_quebra      = w_zlest0194-zvlr_quebra * ( -1 ).
          ELSE.
            w_item-zvlr_quebra      = w_zlest0194-zvlr_quebra.
          ENDIF.



*------------------------------------------
*  "Produto" e "(-) KG Quebra"
*------------------------------------------

          w_item-ds_prod_pred =   w_zlest0194-ds_prod_pred.

          IF w_zlest0194-zquebra < 0.
            w_item-zquebra      = w_zlest0194-zquebra * ( -1 ).
          ELSE.
            w_item-zquebra      = w_zlest0194-zquebra.
          ENDIF.




*------------------------------------------
* Valor a Receber
*------------------------------------------
          w_item-zvlr_liq_receber = w_zlest0194-zvlr_liq_receber.

* Tarifa CTE
          IF  w_item-qt_carga_cte IS NOT INITIAL.
            w_item-tarifa = (  w_item-valor_prestacao /  w_item-qt_carga_cte ) * 1000.
          ENDIF.

*Tarifa Custo
          READ TABLE t_custo  INTO w_custo WITH KEY fknum = w_zib_cte_dist_n55-fknum.

          IF sy-subrc IS INITIAL.

            READ TABLE t_collect_custo INTO w_collect_custo WITH KEY vbeln = w_custo-vbeln.
            IF sy-subrc IS INITIAL.
              w_item-zvlr_liq_receber = w_collect_custo-netpr.
            ENDIF.

          ENDIF.

        ENDIF.

        APPEND w_item TO  w_fatura-item[].

      ENDLOOP.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'ZSFLES_FATURA'
        IMPORTING
          fm_name            = l_fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*  Impresora
      w_control-no_dialog = ' '. "Evita la pantalla de opciones de salida del formulario
      w_options-tddest   = 'LOCL'.
      w_options-tdimmed  = 'X'.
      w_options-tdnewid  = 'X'.
      w_options-tdnoarch = 'X'.


      w_control-preview =  space.
      w_control-device  = 'PRINTER'.
      w_control-getotf  = ' '.

      CLEAR: w_job_output_info.
      CALL FUNCTION l_fm_name
        EXPORTING
          user_settings      = ' '
          control_parameters = w_control
          output_options     = w_options
          fatura             = w_fatura
        IMPORTING
          job_output_info    = w_job_output_info
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_msg).

        MESSAGE s000(z_mm) WITH l_msg DISPLAY LIKE 'E'.
        RETURN.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
  SET PF-STATUS 'PF0210'.
  SET TITLEBAR 'TI0210'.

  PERFORM zf_visualizar_email.

  CREATE OBJECT container_email
    EXPORTING
      container_name = 'CEMAIL'.

  cl_abap_browser=>show_html(
  EXPORTING
   html_string = email_string
   title       = 'Monitor de Recebimentos Frete Frota'
*   MODAL       = ABAP_FALSE
*   FORMAT      = CL_ABAP_BROWSER=>LANDSCAPE
*   SIZE        = CL_ABAP_BROWSER=>SMALL
   container   = container_email ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0212  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0212 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  PERFORM fm_caixa_de_texto_obs.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CAIXA_DE_TEXTO_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_caixa_de_texto_obs .

  txtopen = abap_true.

  IF NOT c_editor IS INITIAL AND NOT editor IS INITIAL.
    CALL METHOD c_editor->free( ).
    CALL METHOD editor->free( ).
  ENDIF.

  CREATE OBJECT: c_editor EXPORTING container_name = 'C_SUBOBS', "CONTAINER
                   editor EXPORTING parent         = c_editor.

  CALL METHOD editor->set_toolbar_mode( toolbar_mode = editor->false ).
  CALL METHOD editor->set_statusbar_mode( statusbar_mode = editor->false ).

  CASE txtopen.
    WHEN 'X'.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->false ).
    WHEN OTHERS.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->true ).
  ENDCASE.

  CALL METHOD editor->set_text_as_stream
    EXPORTING
      text = it_editor.

  FREE: txtopen, it_editor.

ENDFORM.


MODULE status_0211 OUTPUT.
  SET PF-STATUS 'ST0211'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
