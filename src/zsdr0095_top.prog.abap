*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0095.

TABLES: zsdt0170, lfa1, kna1.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100,
         ds_status         TYPE dd07t-ddtext,
         ds_situacao_due   TYPE dd07t-ddtext,
         ds_ind_bloqueio   TYPE dd07t-ddtext,
         ds_situacao_carga TYPE dd07t-ddtext,
         ds_controle_adm   TYPE dd07t-ddtext,
         ds_nome_transpor  TYPE znom_transporte-ds_nome_transpor.
         INCLUDE STRUCTURE zsdt0170.
       TYPES END OF ty_saida_0100.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.

  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_alv_toolbar_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

DATA: go_alarm TYPE REF TO lcl_event_receiver,
      go_clock TYPE REF TO cl_gui_timer.

DATA: vg_data_lim        TYPE erdat,
      vg_time_lim        TYPE t,
      vg_time_restante   TYPE t,
      vg_tst_dif         TYPE timestamp,
      vg_tst_lim         TYPE timestamp,
      vg_tst_atual       TYPE timestamp,
      vg_time_interval   TYPE i VALUE 60,
      vg_st_logon        TYPE string VALUE 'DISCONNECTED',
      vg_time_connection TYPE erzet.

*-------------------------------------------------------------------
* Classes
*-------------------------------------------------------------------
DATA: zcl_token_siscomex TYPE REF TO zcl_token_siscomex,
      zcl_due            TYPE REF TO zcl_due.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100      TYPE TABLE OF ty_saida_0100,
      wa_saida_0100      TYPE ty_saida_0100,
      wa_registro_due    TYPE zde_registro_due,
      tg_0170            TYPE TABLE OF zsdt0170 WITH HEADER LINE,
      tg_t001            TYPE TABLE OF t001     WITH HEADER LINE,
      tg_znom_transporte TYPE TABLE OF znom_transporte WITH HEADER LINE.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao    TYPE c LENGTH 20,
      var_answer     TYPE c,
      vg_ucomm       TYPE sy-ucomm,
* Inicio - falheiros - 25.11.2022
      vg_ucomm_drawn TYPE sy-ucomm.
* Fim - falheiros - 25.11.2022
*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_novo         TYPE c VALUE 'NOVO'         LENGTH 4,
           c_del          TYPE c VALUE 'DEL'          LENGTH 4,
           c_change       TYPE c VALUE 'CHANGE'       LENGTH 6,
           c_view         TYPE c VALUE 'VIEW'         LENGTH 7,
           c_connected    TYPE c VALUE 'CONNECTED'    LENGTH 9,
           c_login        TYPE c VALUE 'LOGIN'        LENGTH 12,
           c_logout       TYPE c VALUE 'LOGOUT'       LENGTH 12,
           c_disconnected TYPE c VALUE 'DISCONNECTED' LENGTH 12,
           c_transmitir   TYPE c VALUE 'TRANSMITIR'   LENGTH 12,
           c_cons_status  TYPE c VALUE 'CONS_STATUS'  LENGTH 50,
           c_excluir      TYPE c VALUE 'EXCLUIR'      LENGTH 50,
           c_due_novo     TYPE c VALUE '1'            LENGTH 1,
           c_due_change   TYPE c VALUE '2'            LENGTH 1,
           c_due_view     TYPE c VALUE '3'            LENGTH 1.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001. "Identificação
SELECT-OPTIONS: p_id    FOR zsdt0170-id_due,
                p_bukrs FOR zsdt0170-bukrs,
                p_idref FOR zsdt0170-id_due_ref,
                p_idnom FOR zsdt0170-id_nomeacao_tran,
                p_ndue  FOR zsdt0170-numero_due,
                p_nruc  FOR zsdt0170-numero_ruc,
                p_cnpjd FOR zsdt0170-cnpj_declarante.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.  "Local Despacho
SELECT-OPTIONS: p_tpld  FOR zsdt0170-tp_cod_local_despacho,
                p_urfld FOR zsdt0170-codigo_urf_despacho,
                p_rald  FOR zsdt0170-codigo_ra_despacho,
                p_ccrpl FOR zsdt0170-cnpj_cpf_resp_loc_desp,
                p_ldend FOR zsdt0170-local_despacho_end.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.  "Informações Basicas
SELECT-OPTIONS: p_frexp  FOR zsdt0170-forma_exportacao,
                p_cetrp  FOR zsdt0170-caso_especial_transporte,
                p_stetrp FOR zsdt0170-situacao_especial,
                p_moedan FOR zsdt0170-moeda_cambio.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-004. "Local Embarque
SELECT-OPTIONS: p_urfle FOR zsdt0170-codigo_urf_embarque,
                p_rale  FOR zsdt0170-codigo_ra_embarque.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME TITLE text-006. "Status
SELECT-OPTIONS: p_sitdue   FOR zsdt0170-situacao_due   NO INTERVALS ,
                p_dt_sit   FOR zsdt0170-dt_situacao    NO INTERVALS ,
                p_indblo   FOR zsdt0170-ind_bloqueio   NO INTERVALS ,
                p_sitcar   FOR zsdt0170-situacao_carga NO INTERVALS ,
                p_ctadm    FOR zsdt0170-controle_adm   NO INTERVALS .
SELECTION-SCREEN: END OF BLOCK b6.


SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE text-005. "Administração
SELECT-OPTIONS: p_dtcr   FOR zsdt0170-dt_registro ,"DEFAULT SY-DATUM,
                p_hrcr   FOR zsdt0170-hr_registro ,
                p_uscr   FOR zsdt0170-us_registro,
                p_dtmod  FOR zsdt0170-dt_modificacao,
                p_hrmod  FOR zsdt0170-hr_modificacao,
                p_usmod  FOR zsdt0170-us_modificacao.
SELECTION-SCREEN: END OF BLOCK b5.


AT SELECTION-SCREEN OUTPUT.

  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  SET PF-STATUS 'PF1000'.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = 'PF1000'
    TABLES
      p_exclude = tg_fcode
    EXCEPTIONS
      OTHERS    = 1.

  CLEAR: vg_ucomm.

AT SELECTION-SCREEN.

  vg_ucomm = sy-ucomm. "CAPTURE USER COMMAND

  CASE vg_ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'GERAR_RUC'.

      DATA: wa_dados_geracao_ruc TYPE zde_dados_geracao_ruc.

      wa_dados_geracao_ruc-edit_fields = abap_true.

      zcl_due=>gerar_nr_ruc_with_screen( i_dados_geracao_ruc = wa_dados_geracao_ruc ).

    WHEN 'EXEC_INI'.
      PERFORM f_executar.
  ENDCASE.


START-OF-SELECTION.

  PERFORM f_executar.
