*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_TOP
*&---------------------------------------------------------------------*

REPORT zlesr0111 MESSAGE-ID zcct.

TABLES: zlest0142, zlest0146, zlest0147, zsdt0001, zsdt0170, zlest0149, zsdt0179, zsdt0180, zsdt0181, mara, lfa1.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zlest0146_cancel,
         chave_nfe    TYPE zlest0147-chave_nfe,
         chave_nff    TYPE zlest0147-chave_nff,
         emissor_cnpj TYPE zlest0147-emissor_cnpj,
         emissor_cpf  TYPE zlest0147-emissor_cpf,
         emissor_ie   TYPE zlest0147-emissor_ie,
         nfnum9       TYPE zlest0147-nfnum9,
         dt_emissao   TYPE zlest0147-dt_emissao.
         INCLUDE STRUCTURE zlest0146.
       TYPES END OF ty_zlest0146_cancel.

TYPES: BEGIN OF ty_saida_0100_01,
         id_recepcao           TYPE zlest0146-id_recepcao,
         tp_recepcao           TYPE zlest0146-tp_recepcao,
         cnpj_responsavel      TYPE zlest0146-cnpj_responsavel,
         local_codigo_urf      TYPE zlest0146-local_codigo_urf,
         local_codigo_ra       TYPE zlest0146-local_codigo_ra,
         local_latitude        TYPE zlest0146-local_latitude,
         local_longitude       TYPE zlest0146-local_longitude,
         transportador_cnpj    TYPE zlest0146-transportador_cnpj,
         transportador_cpf     TYPE zlest0146-transportador_cpf,
         peso_aferido_recepcao TYPE zlest0146-peso_aferido_recepcao,
         dt_recepcao           TYPE zlest0146-dt_recepcao,
         importado             TYPE zlest0146-importado,
         dt_importacao         TYPE zlest0146-dt_importacao,
         hr_importacao         TYPE zlest0146-hr_importacao,
         us_importacao         TYPE zlest0146-us_importacao,
         chave_nfe             TYPE zlest0147-chave_nfe,
         chave_nff             TYPE zlest0147-chave_nff,
         docnum                TYPE zlest0147-docnum,
         emissor_cnpj          TYPE zlest0147-emissor_cnpj,
         emissor_cpf           TYPE zlest0147-emissor_cpf,
         emissor_ie            TYPE zlest0147-emissor_ie,
         regio                 TYPE zlest0147-regio,
         nfyear                TYPE zlest0147-nfyear,
         nfmonth               TYPE zlest0147-nfmonth,
         model                 TYPE zlest0147-model,
         serie                 TYPE zlest0147-serie,
         nfnum9                TYPE zlest0147-nfnum9,
         nfnum                 TYPE zlest0147-nfnum,
         docnum9               TYPE zlest0147-docnum9,
         cdv                   TYPE zlest0147-cdv,
         sigla_uf_emissor      TYPE zlest0147-sigla_uf_emissor,
         dt_emissao            TYPE zlest0147-dt_emissao,
         matnr                 TYPE j_1bnflin-matnr,
         maktx                 TYPE makt-maktx,
         cd_porto              TYPE lfa1-lifnr,
         ds_porto              TYPE lfa1-name1,
         city_porto            TYPE adrc-city1,
         nf_terceiro           TYPE c LENGTH 4.
TYPES END OF ty_saida_0100_01.

TYPES: BEGIN OF ty_zlest0142.
         INCLUDE STRUCTURE zlest0142.
       TYPES  END OF ty_zlest0142.

TYPES: BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         matnr  TYPE j_1bnflin-matnr.
TYPES  END OF ty_j_1bnflin.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         adrnr TYPE lfa1-adrnr,
         stcd1 TYPE lfa1-stcd1,
         ktokk TYPE lfa1-ktokk.
TYPES  END OF ty_lfa1.

TYPES: BEGIN OF ty_adrc,
         addrnumber TYPE adrc-addrnumber,
         city1      TYPE adrc-city1.
TYPES  END OF ty_adrc.

TYPES: BEGIN OF ty_zlest0147,
         stcd1 TYPE lfa1-stcd1.
         INCLUDE STRUCTURE zlest0147.
       TYPES  END OF ty_zlest0147.

TYPES: BEGIN OF ty_msg,
         filename   TYPE sdok_filnm,
         repid      TYPE sy-repid,
         type(1)    TYPE c,
         msgno(3)   TYPE c,
         texto(255) TYPE c,
         tabix      TYPE sy-tabix,
         type_ic    TYPE c LENGTH 4,
       END OF ty_msg.


*
*TYPES: BEGIN OF TY_RECEPCAO,
*          ID_RECEPCAO             TYPE ZLEST0146-ID_RECEPCAO,
*          TP_RECEPCAO             TYPE ZLEST0146-TP_RECEPCAO,
*          CNPJ_RESPONSAVEL        TYPE ZLEST0146-CNPJ_RESPONSAVEL,
*          LOCAL_CODIGO_URF        TYPE ZLEST0146-LOCAL_CODIGO_URF,
*          LOCAL_CODIGO_RA         TYPE ZLEST0146-LOCAL_CODIGO_RA,
*          LOCAL_LATITUDE          TYPE ZLEST0146-LOCAL_LATITUDE,
*          LOCAL_LONGITUDE         TYPE ZLEST0146-LOCAL_LONGITUDE,
*          TRANSPORTADOR_CNPJ      TYPE ZLEST0146-TRANSPORTADOR_CNPJ,
*          TRANSPORTADOR_CPF       TYPE ZLEST0146-TRANSPORTADOR_CPF,
*          PESO_AFERIDO_RECEPCAO   TYPE ZLEST0146-PESO_AFERIDO_RECEPCAO,
*          DT_RECEPCAO             TYPE ZLEST0146-DT_RECEPCAO,
*          IMPORTADO               TYPE ZLEST0146-IMPORTADO,
*          DT_IMPORTACAO           TYPE ZLEST0146-DT_IMPORTACAO,
*          HR_IMPORTACAO           TYPE ZLEST0146-HR_IMPORTACAO,
*          US_IMPORTACAO           TYPE ZLEST0146-US_IMPORTACAO,
*          CHAVE_NFE               TYPE ZLEST0147-CHAVE_NFE,
*          CHAVE_NFF               TYPE ZLEST0147-CHAVE_NFF,
*          DOCNUM                  TYPE ZLEST0147-DOCNUM,
*          EMISSOR_CNPJ            TYPE ZLEST0147-EMISSOR_CNPJ,
*          EMISSOR_CPF             TYPE ZLEST0147-EMISSOR_CPF,
*          EMISSOR_IE              TYPE ZLEST0147-EMISSOR_IE,
*          REGIO                   TYPE ZLEST0147-REGIO,
*          NFYEAR                  TYPE ZLEST0147-NFYEAR,
*          NFMONTH                 TYPE ZLEST0147-NFMONTH,
*          MODEL                   TYPE ZLEST0147-MODEL,
*          SERIE                   TYPE ZLEST0147-SERIE,
*          NFNUM9                  TYPE ZLEST0147-NFNUM9,
*          NFNUM                   TYPE ZLEST0147-NFNUM,
*          DOCNUM9                 TYPE ZLEST0147-DOCNUM9,
*          CDV                     TYPE ZLEST0147-CDV,
*          SIGLA_UF_EMISSOR        TYPE ZLEST0147-SIGLA_UF_EMISSOR,
*          DT_EMISSAO              TYPE ZLEST0147-DT_EMISSAO,
*       END OF TY_RECEPCAO.

TYPES: BEGIN OF ty_file,
         linha(400),
       END OF ty_file.

TYPES: BEGIN OF ty_path,
         p_input(100) TYPE c,
       END OF ty_path.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

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
*-------------------------------------------------------------------
* Classes
*-------------------------------------------------------------------

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100_01   TYPE TABLE OF ty_saida_0100_01,
      wa_saida_0100_01   TYPE ty_saida_0100_01,
      wa_path            TYPE ty_path,
      tg_msg_out         TYPE TABLE OF ty_msg,
      tg_msg             TYPE TABLE OF ty_msg WITH HEADER LINE,
      tg_msg_aux         TYPE TABLE OF ty_msg WITH HEADER LINE,
      tg_planilha        LIKE STANDARD TABLE OF alsmex_tabline WITH HEADER LINE,
      tg_file            TYPE STANDARD TABLE OF ty_file WITH HEADER LINE INITIAL SIZE 0,
      tg_0142            TYPE TABLE OF zlest0142 WITH HEADER LINE,
      tg_0146            TYPE TABLE OF zlest0146 WITH HEADER LINE,
      tg_0146_cancel     TYPE TABLE OF ty_zlest0146_cancel WITH HEADER LINE,
      tg_0147            TYPE TABLE OF ty_zlest0147 WITH HEADER LINE,
      tg_0147_aux        TYPE TABLE OF ty_zlest0147 WITH HEADER LINE,
      tg_lin             TYPE TABLE OF ty_j_1bnflin WITH HEADER LINE,
      tg_adrc            TYPE TABLE OF ty_adrc      WITH HEADER LINE,
      tg_zsdt0168        TYPE TABLE OF zsdt0168     WITH HEADER LINE,
      tg_0170            TYPE TABLE OF zsdt0170  WITH HEADER LINE,
      tg_0179            TYPE TABLE OF zsdt0179  WITH HEADER LINE,
      tg_0180            TYPE TABLE OF zsdt0180  WITH HEADER LINE,
      tg_0181            TYPE TABLE OF zsdt0181  WITH HEADER LINE,
      tg_lfa1            TYPE TABLE OF ty_lfa1      WITH HEADER LINE,
      tg_makt            TYPE TABLE OF makt      WITH HEADER LINE,
      "TG_RECEPCAO_IMP         TYPE TABLE OF TY_RECEPCAO WITH HEADER LINE,
      "TG_RECEPCAO_IMP_NF_RAT  TYPE TABLE OF ZLEST0168 WITH HEADER LINE,
      tg_parametros      TYPE ustyp_t_parameters WITH HEADER LINE,
      wg_par_cct         TYPE zlest0149,
      it_nfe_cons_portal TYPE zde_chave_doc_e_t,
      wl_nfe_cons_portal TYPE zde_chave_doc_e,
      tg_zlest0186       TYPE TABLE OF zlest0186 WITH HEADER LINE.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao  TYPE c LENGTH 20,
      var_answer   TYPE c,
      vg_ucomm     TYPE sy-ucomm,
      vg_not_found TYPE c.

DATA: v_prefix_ent TYPE zprefix,
      v_mensagem   TYPE bapi_msg,
      t_dir_loc_f  TYPE TABLE OF sdokpath,
      t_dir_local  TYPE TABLE OF sdokpath,
      t_dir_unix   TYPE TABLE OF epsfili,
      v_file_aux   TYPE draw-filep,
      v_file_aux2  TYPE draw-filep.


*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_log_cancel         TYPE c VALUE  'LOG_CANCEL'            LENGTH 50,
           c_doc_rat_recepcao   TYPE c VALUE  'DOC_RAT_RECEPCAO'      LENGTH 50,
           c_cancel_recep_carga TYPE c VALUE  'CANCEL_RECEP_CARGA'    LENGTH 50,
           c_log_cct            TYPE c VALUE  'LOG_CCT'               LENGTH 50.

CONSTANTS: c_x            TYPE c VALUE 'X',
           c_log(10)      TYPE c VALUE 'LOG',
           c_proc(10)     TYPE c VALUE 'PROC',
           c_ent(10)      TYPE c VALUE 'ENT',
           c_all(3)       TYPE c VALUE 'ALL',
           c_asc(10)      TYPE c VALUE 'ASC',
           c_mask_loc(6)  TYPE c VALUE '*.txt',
           c_mask_unix(6) TYPE c VALUE '*.txt',
           c_u            TYPE c VALUE 'U',
           c_w            TYPE c VALUE 'W',
           c_l            TYPE c VALUE 'L',
           c_e            TYPE c VALUE 'E',
           c_s            TYPE c VALUE 'S'.

*-----------------------------------------------------------------------------------------------------*
*  Modo
*-----------------------------------------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-005.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: P_TP_IRC RADIOBUTTON GROUP RB1.                               "Importar Recepção Carga
*    SELECTION-SCREEN COMMENT 03(35) TEXT-018 FOR FIELD P_TP_IRC.
*  SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------------------------------------*
*  Visualizar
*-----------------------------------------------------------------------------------------------------*

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-010.
*
*SELECTION-SCREEN BEGIN OF LINE.
* PARAMETERS: P_RC_IMP RADIOBUTTON GROUP RB2                  MODIF ID V6.                     "Recepções de Carga por NF-e/NF-f Importadas
* SELECTION-SCREEN COMMENT 03(40) TEXT-019 FOR FIELD P_RC_IMP MODIF ID V6.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------------------------------------*
*  Carga
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-001.

SELECT-OPTIONS: p_stcd1  FOR zlest0147-emissor_cnpj NO-EXTENSION NO INTERVALS      MODIF ID cg,
                p_stcd2  FOR zlest0147-emissor_cpf  NO-EXTENSION NO INTERVALS      MODIF ID cg,
                p_stcd3  FOR zlest0147-emissor_ie   NO-EXTENSION NO INTERVALS      MODIF ID cg,
                p_dtemi  FOR zlest0147-dt_emissao                                  MODIF ID cg,
                p_numnf  FOR zlest0147-nfnum9                                      MODIF ID cg,
                p_matnr  FOR mara-matnr                                            MODIF ID cg,
                p_knfe   FOR zlest0147-chave_nfe.
SELECTION-SCREEN END OF BLOCK b3.

*-----------------------------------------------------------------------------------------------------*
*  Recepção Carga
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-012.

SELECT-OPTIONS: p_dtrcc  FOR zlest0146-dt_recepcao                                MODIF ID rcc,
                p_dtimp  FOR zlest0146-dt_importacao DEFAULT sy-datum             MODIF ID rcc,
                p_usimp  FOR zlest0146-us_importacao NO-EXTENSION NO INTERVALS    MODIF ID rcc,
                p_cdprt  FOR lfa1-lifnr                                           MODIF ID rcc.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF SCREEN 3300 AS SUBSCREEN.
SELECT-OPTIONS: p_chave FOR zlest0147-chave_nfe.
SELECTION-SCREEN END OF SCREEN 3300.


AT SELECTION-SCREEN OUTPUT.

  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  REFRESH: tg_parametros, tg_fcode.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = tg_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  READ TABLE tg_parametros INTO DATA(wl_parametros) WITH KEY parid = 'ZCCT_RCC_IMP'.
  IF sy-subrc NE 0.

    tg_fcode = 'IMPORT_RCC'.
    APPEND tg_fcode.

    tg_fcode = 'IMP_CCT'.
    APPEND tg_fcode.

  ENDIF.

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
    WHEN 'EXEC'.
      PERFORM f_executar.
    WHEN 'IMPORT_RCC'.
      PERFORM f_ler_diretorio USING 'RCC'.
    WHEN 'ERRO_RCC'.
      CALL FUNCTION 'ZSDMF_CONSULTA_CCT'.
    WHEN 'IMP_CCT'.
      SUBMIT zsdr0128 VIA SELECTION-SCREEN AND RETURN.
  ENDCASE.
