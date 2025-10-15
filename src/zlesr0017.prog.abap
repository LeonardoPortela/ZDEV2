*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0017                                                *
* Descrição  : Complemento                                             *
* Módulo     : LES                               Transação:            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 25/07/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zlesr0017 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                Type Pools                            *
*----------------------------------------------------------------------*
TYPE-POOLS: icon, zftte.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_tela,
         tdlnr               TYPE zlest0034-tdlnr,
         name1               TYPE lfa1-name1,
         tknum               TYPE zlest0034-tknum,
         fknum               TYPE zlest0034-fknum,
         nfenum              TYPE zlest0034-nfenum,
         zdt_mov             TYPE zlest0034-zdt_mov,
         zdt_vencto          TYPE zlest0034-zdt_vencto,
         nr_conhec           TYPE zlest0034-nr_conhec,
         series              TYPE zlest0034-series,
         zdt_conhec          TYPE zlest0034-zdt_conhec,
         matnr               TYPE zlest0034-matnr,
         maktx               TYPE makt-maktx,
         matns               TYPE zlest0034-matns,
         comp_valor          TYPE zlest0042-comp_valor,
         comp_tipo           TYPE zlest0042-tipo,
         bvtyp               TYPE bvtyp,
         texto               TYPE char50,
         bankl               TYPE char03,
         banka               TYPE banka,
         bankn               TYPE bankn,
         agenc               TYPE char15,
         text1               TYPE t007s-text1,
         iva                 TYPE t007a-mwskz,
         nfe                 TYPE zlest0034-nfe,
         multimodal          TYPE zlest0034-multimodal,
         maktg               TYPE makt-maktg,
         cotacao             TYPE ukurs_curr,
         it_impostos_retidos TYPE zles0043_imp_retidos_t,
       END OF type_tela,

       BEGIN OF type_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END   OF type_lfa1,

       BEGIN OF type_lfbk,
         lifnr TYPE lfbk-lifnr,
         banks TYPE lfbk-banks,
         bankl TYPE lfbk-bankl,
         bankn TYPE lfbk-bankn,
         bkont TYPE lfbk-bkont,
         bvtyp TYPE lfbk-bvtyp,
       END   OF type_lfbk,

       BEGIN OF type_bnka,
         banks TYPE bnka-banks,
         bankl TYPE bnka-bankl,
         banka TYPE bnka-banka,
       END   OF type_bnka,

       BEGIN OF type_t007s,
         spras TYPE t007s-spras,
         kalsm TYPE t007s-kalsm,
         mwskz TYPE t007s-mwskz,
         text1 TYPE t007s-text1,
       END   OF type_t007s,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         spras TYPE makt-spras,
         maktx TYPE makt-maktx,
         maktg TYPE makt-maktg,
       END   OF type_makt,

       BEGIN OF type_zlsch,
         tknum TYPE vttk-tknum,
         zlsch TYPE zib_contabil-zlsch,
         bvtyp TYPE bvtyp,
       END   OF type_zlsch,

       BEGIN OF type_erro,
         tipo   TYPE bapi_mtype,
         numero TYPE bapi_mtype,
         msg    TYPE bapi_msg,
       END   OF type_erro.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: ti_dados     TYPE TABLE OF zftte_dados,
      ti_saida     TYPE TABLE OF zftte_dados,
      ti_lfa1      TYPE TABLE OF type_lfa1,
      ti_lfbk      TYPE TABLE OF type_lfbk,
      ti_bnka      TYPE TABLE OF type_bnka,
      ti_0200      TYPE TABLE OF zftte_dados,
      ti_t007s     TYPE TABLE OF type_t007s,
      ti_zlest0042 TYPE TABLE OF zlest0042,
      ti_zlest0021 TYPE TABLE OF zlest0021,
      ti_makt      TYPE TABLE OF type_makt,
      ti_zlsch     TYPE TABLE OF type_zlsch,
      ti_erro      TYPE TABLE OF type_erro,
      t_tool       TYPE ui_functions,
      t_fcat       TYPE TABLE OF lvc_s_fcat,
      s_cont       TYPE REF TO cl_gui_custom_container,
      s_alv        TYPE REF TO cl_gui_alv_grid,
      s_layout     TYPE lvc_s_layo,
      s_tela       TYPE type_tela,
      s_dados      TYPE zftte_dados,
      s_dados2     TYPE zftte_dados2,
      ti_eventos   TYPE TABLE OF zftte_eventos,
      wa_eventos   TYPE zftte_eventos.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: vg_index  TYPE syindex,
      vg_total  TYPE syindex,
      vg_okcode TYPE syucomm.

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_table          TYPE char8  VALUE 'TI_SAIDA',
           c_x              TYPE char1  VALUE 'X',
           c_confirmar      TYPE char9  VALUE 'CONFIRMAR',
           c_proximo        TYPE char7  VALUE 'PROXIMO',
           c_anterior       TYPE char8  VALUE 'ANTERIOR',
           c_cancelar       TYPE char8  VALUE 'CANCELAR',
           c_impostos_r(10) TYPE c VALUE 'IMPOSTOS_R',
           c_back           TYPE char4  VALUE 'BACK',
           c_exit           TYPE char4  VALUE 'EXIT',
           c_canc           TYPE char4  VALUE 'CANC',
           c_f02            TYPE char4  VALUE 'F-02',
           c_t              TYPE char1  VALUE 'T',
           c_i0             TYPE char2  VALUE 'I0',
           c_i1             TYPE char2  VALUE 'I1',
           c_s1             TYPE char2  VALUE 'S1',
           c_i8             TYPE char2  VALUE 'I8',
           c_21             TYPE char2  VALUE '21',
           c_1              TYPE char1  VALUE '1',
           c_5              TYPE char1  VALUE '5',
           c_9              TYPE char1  VALUE '9',
           c_s              TYPE c VALUE 'S',
           c_w              TYPE c VALUE 'W',
           c_e              TYPE c VALUE 'E',
           c_cancel         TYPE c LENGTH 6 VALUE 'CANCEL'.


CLASS lcl_event_eventos DEFINITION DEFERRED.

DATA: t_dynpfields      TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1 WITH HEADER LINE,
      t_ret             TYPE TABLE OF ddshretval,
      wa_event_eventos  TYPE REF TO lcl_event_eventos,
      wa_cont_eventos   TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv_eventos    TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
      it_fcat_eventos   TYPE TABLE OF lvc_s_fcat            , "Controle VLA: catálogo de campos
      wa_layout_eventos TYPE lvc_s_layo,
      ok_code           TYPE sy-ucomm, " Layout da Lista / Fim do DATA
      st_ret            TYPE ddshretval.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               Classes                                *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_toolbar.
*   Toolbar
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Busca Dados
  PERFORM: z_busca_dados   ,
* Processa Dados
           z_processa_dados,
* Monta ALV
           z_mont_alv      .

  IF NOT ti_saida[] IS INITIAL.
    CALL SCREEN 0100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS                                            *
*&---------------------------------------------------------------------*
*                               Busca Dados                            *
*----------------------------------------------------------------------*
FORM z_busca_dados.

  IMPORT ti_dados FROM MEMORY ID 'ZCOMP'.
  FREE MEMORY ID 'ZLESR0014'.

* Seleciona ZLEST0042
  PERFORM: z_seleciona_zlest0042,

* Seleciona LFA1
           z_seleciona_lfa1     ,

* Seleciona LFBK
           z_seleciona_lfbk     ,

* Seleciona BNKA
           z_seleciona_bnka     ,

* Seleciona T007S
           z_seleciona_t007s    ,

* Seleciona MAKT
           z_seleciona_makt     .

ENDFORM.                    " Z_BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0042                                    *
*&---------------------------------------------------------------------*
*                          Seleciona ZLEST0042                         *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0042.

  REFRESH ti_zlest0042.

  CHECK NOT ti_dados[] IS INITIAL.

  SELECT *
    FROM zlest0042
    INTO TABLE ti_zlest0042
    FOR ALL ENTRIES IN ti_dados
  WHERE  tknum EQ ti_dados-tknum.

  SORT ti_zlest0042 BY tknum ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZLEST0042

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                             Processa Dados                           *
*----------------------------------------------------------------------*
FORM z_processa_dados.

  DATA: sl_dados     TYPE zftte_dados,
        sl_saida     TYPE zftte_dados,
        sl_zlest0042 TYPE zlest0042.

  REFRESH ti_saida.

  LOOP AT ti_dados INTO sl_dados.

    CLEAR sl_zlest0042.

    READ TABLE ti_zlest0042 INTO sl_zlest0042
      WITH KEY tknum = sl_dados-tknum
      BINARY SEARCH.

    sl_saida  = sl_dados.

    sl_saida-tipo         = sl_zlest0042-tipo.
    sl_saida-zdt_mov      = sl_zlest0042-zdt_mov.
    sl_saida-zdt_vencto   = sl_zlest0042-zdt_vencto.
    sl_saida-nr_conhec    = sl_zlest0042-nr_conhec.
    sl_saida-series       = sl_zlest0042-serie_conhec.
    sl_saida-zdt_conhec   = sl_zlest0042-zdt_conhec.
    sl_saida-iva          = sl_zlest0042-iva.
    sl_saida-nfe          = sl_zlest0042-nfe.
    sl_saida-multimodal   = sl_zlest0042-multimodal.
    sl_saida-base_icms    = sl_zlest0042-base_icms.
    sl_saida-base_pis     = sl_zlest0042-base_pis.
    sl_saida-base_cofins  = sl_zlest0042-base_cofins.
    sl_saida-rate_icms    = sl_zlest0042-rate_icms.
    sl_saida-rate_pis     = sl_zlest0042-rate_pis.
    sl_saida-rate_cofins  = sl_zlest0042-rate_cofins.
    sl_saida-valor_icms   = sl_zlest0042-valor_icms.
    sl_saida-valor_pis    = sl_zlest0042-valor_pis.
    sl_saida-valor_cofins = sl_zlest0042-valor_cofins.
    sl_saida-comp_docnum  = sl_zlest0042-comp_docnum.
    sl_saida-comp_belnr   = sl_zlest0042-comp_belnr.
    sl_saida-comp_gjahr   = sl_zlest0042-comp_gjahr.
    sl_saida-comp_valor   = sl_zlest0042-comp_valor.
    sl_saida-id_envio     = sl_zlest0042-id_envio.
    sl_saida-cotacao      = sl_zlest0042-cotacao.

    PERFORM atualiza_status CHANGING sl_saida.

    APPEND sl_saida TO ti_saida.

    CLEAR: sl_saida, sl_dados.

  ENDLOOP.

  SORT ti_saida BY tknum ASCENDING.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                                 Toolbar                              *
*----------------------------------------------------------------------*
FORM z_handle_toolbar USING p_object      TYPE REF TO cl_alv_event_toolbar_set
                            p_interactive TYPE char1.

* Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Editar
  CLEAR sl_toolbar.
  MOVE: 'EDITAR'      TO sl_toolbar-function ,
         icon_change  TO sl_toolbar-icon     ,
         TEXT-001     TO sl_toolbar-quickinfo,
         TEXT-001     TO sl_toolbar-text     ,
         space        TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Gerar
  CLEAR sl_toolbar.
  MOVE: 'GERAR'        TO sl_toolbar-function ,
         icon_generate TO sl_toolbar-icon     ,
         TEXT-005      TO sl_toolbar-quickinfo,
         TEXT-005      TO sl_toolbar-text     ,
         space         TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Eventos
  CLEAR sl_toolbar.
  MOVE: 'EVENTOS'            TO sl_toolbar-function ,
         icon_error_protocol TO sl_toolbar-icon     ,
         TEXT-010            TO sl_toolbar-quickinfo,
         TEXT-010            TO sl_toolbar-text     ,
         space               TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Eventos
  CLEAR sl_toolbar.
  MOVE: 'ESTORNO'            TO sl_toolbar-function ,
         icon_storno         TO sl_toolbar-icon     ,
         TEXT-011            TO sl_toolbar-quickinfo,
         TEXT-011            TO sl_toolbar-text     ,
         space               TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm.

  CASE p_ucomm.
    WHEN 'EDITAR'.
*     Função Editar
      PERFORM z_editar.
    WHEN 'GERAR'.
*     Função Gerar
      PERFORM z_gerar USING space.
    WHEN 'EVENTOS'.
*     Função Motrar Erros
      PERFORM z_mostra_eventos.
    WHEN 'ESTORNO'.
      PERFORM z_gerar USING c_x.
  ENDCASE.

  CALL METHOD s_alv->refresh_table_display.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_EDITAR                                                 *
*&---------------------------------------------------------------------*
*                            Função Editar                             *
*----------------------------------------------------------------------*
FORM z_editar.

  DATA: tl_index TYPE lvc_t_row,
        tl_row   TYPE lvc_t_roid,
        tl_dados TYPE TABLE OF zftte_dados,
        tl_aux   TYPE TABLE OF zftte_dados,
        sl_index TYPE lvc_s_row,
        sl_saida TYPE zftte_dados,
        vl_lines TYPE i.

  REFRESH ti_0200.
  CLEAR vg_okcode.

  CALL METHOD s_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_index
      et_row_no     = tl_row.

  IF tl_index[] IS INITIAL.
    MESSAGE i897 WITH TEXT-002.
    EXIT.
  ENDIF.

  LOOP AT tl_index INTO sl_index.
    READ TABLE ti_saida INTO sl_saida
      INDEX sl_index-index.
    APPEND sl_saida TO tl_dados.
    CLEAR: sl_index,
           sl_saida.
  ENDLOOP.

  DELETE tl_dados WHERE comp_belnr NE space.
  IF tl_dados[] IS INITIAL.
    MESSAGE i897 WITH TEXT-004.
    EXIT.
  ENDIF.

  tl_aux[] = tl_dados[].
  SORT tl_aux BY tdlnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING tdlnr.
  DESCRIBE TABLE tl_aux LINES vl_lines.

  IF vl_lines GT 1.
    MESSAGE i897 WITH TEXT-003.
    EXIT.
  ENDIF.

  ti_0200[] = tl_dados[].
  vg_index  = 1.
  DESCRIBE TABLE ti_0200 LINES vg_total.

  CALL SCREEN 0200 STARTING AT 10 02.

  IF vg_okcode EQ c_confirmar.
    PERFORM z_confirmar.
  ENDIF.

ENDFORM.                    " Z_EDITAR

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
    WHEN '0200'.
      PERFORM z_status_0200.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-013.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent          = s_cont
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-014.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  CALL METHOD s_alv->refresh_table_display.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  DATA: vl_int TYPE int4,
        vl_var TYPE disvariant.

  CHECK s_event IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER: s_event->zm_handle_user_command FOR s_alv,
               s_event->zm_handle_toolbar      FOR s_alv.

  vl_var-report = sy-repid.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      i_default                     = c_x
      is_variant                    = vl_var
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = ti_saida
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_MONT_ALV                                               *
*&---------------------------------------------------------------------*
*                             Monta ALV                                *
*----------------------------------------------------------------------*
FORM z_mont_alv.

  REFRESH: t_fcat,
           t_tool.

  CHECK NOT ti_saida IS INITIAL.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    'TDLNR'            TEXT-t01,
    'NAME1'            TEXT-t02,
    'STATUS'           TEXT-t03,
    'SHTYP'            TEXT-t04,
    'TKNUM'            TEXT-t05,
    'FKNUM'            TEXT-t06,
    'NFENUM'           TEXT-t07,
    'ZDT_MOV'          TEXT-t08,
    'ZDT_VENCTO'       TEXT-t09,
    'NR_CONHEC'        TEXT-t10,
    'SERIES'           TEXT-t11,
    'ZDT_CONHEC'       TEXT-t12,
    'EBELN'            TEXT-t13,
    'LBLNI'            TEXT-t14,
    'GEWEI'            TEXT-t15,
    'DMBTR'            TEXT-t16,
    'DMBTR_DOC'        TEXT-t17,
    'KBETR'            TEXT-t18,
    'ZVLR_LIQ_PAGAR'   TEXT-t19,
    'MATNR'            TEXT-t20,
    'MAKTX'            TEXT-t21,
    'BVTYP'            TEXT-t22,
    'RE_BELNR'         TEXT-t23,
    'RE_GJAHR'         TEXT-t24,
    'EN_DOCNUM'        TEXT-t25,
    'BUKRS'            TEXT-t26,
    'WERKS'            TEXT-t27,
    'WAERS'            TEXT-t28,
    'KURST'            TEXT-t29,
    'IVA'              TEXT-t30,
    'MATNS'            TEXT-t31,
    'MAKTS'            TEXT-t32,
    'REGIO_EMISSOR'    TEXT-t33,
    'REGIO_RECEPTOR'   TEXT-t34,
    'BASE_ICMS'        TEXT-t35,
    'BASE_PIS'         TEXT-t36,
    'BASE_COFINS'      TEXT-t37,
    'RATE_ICMS'        TEXT-t38,
    'RATE_PIS'         TEXT-t39,
    'RATE_COFINS'      TEXT-t40,
    'VALOR_ICMS'       TEXT-t41,
    'VALOR_PIS'        TEXT-t42,
    'VALOR_COFINS'     TEXT-t43,
    'DOCNUM'           TEXT-t44,
    'VALOR_MERCADORIA' TEXT-t45,
    'COMP_VALOR'       TEXT-t46,
    'TIPO'             TEXT-t47,
    'COMP_DOCNUM'      TEXT-t48,
    'COMP_BELNR'       TEXT-t49,
    'COMP_GJAHR'       TEXT-t50.

* Monta Layout
  PERFORM z_layout.

* Deleta Botões
  PERFORM z_deleta_bot USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONT_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_field TYPE c
                               p_desc  TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = c_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra      = c_x.
  s_layout-cwidth_opt = c_x.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM z_deleta_bot USING p_bot TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_bot.
  APPEND sl_tool TO t_tool.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                               User Command                           *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  DATA: wa_impostos_retidos TYPE zles0043_imp_retidos.
  DATA: vg_validado TYPE sy-subrc.


  vg_okcode = sy-ucomm.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN c_back OR
             c_canc OR
             c_exit.
          LEAVE TO SCREEN 0.
      ENDCASE.
    WHEN '0200'.
      PERFORM verificar_multimodal USING vg_validado.
      IF vg_validado IS INITIAL.
        CASE sy-ucomm.
          WHEN c_anterior.
            SUBTRACT 1 FROM vg_index.
          WHEN c_proximo.
            ADD 1 TO vg_index.
          WHEN c_confirmar.
            LEAVE TO SCREEN 0.

          WHEN c_impostos_r.
            READ TABLE ti_0200 INTO DATA(wa_dados) INDEX vg_index.
            wa_impostos_retidos-bukrs = wa_dados-bukrs.
            wa_impostos_retidos-lifnr = wa_dados-lifnr.
*---> 13/06/2023 - Migração S4 - JS
*             WA_IMPOSTOS_RETIDOS-BASE  = S_TELA-COMP_VALOR.
            wa_impostos_retidos-base = CONV #( s_tela-comp_valor ).
*<--- 13/06/2023 - Migração S4 - JS
            PERFORM z_impostos_retidos TABLES s_tela-it_impostos_retidos USING wa_impostos_retidos space.
            PERFORM z_atualiza_tabela.
            EXIT.
        ENDCASE.
      ENDIF.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFA1                                         *
*&---------------------------------------------------------------------*
*                              Seleciona LFA1                          *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa1.

  DATA tl_dados TYPE TABLE OF zftte_dados.

  REFRESH ti_lfa1.

  CHECK NOT ti_dados[] IS INITIAL.
  tl_dados[] = ti_dados[].
  SORT tl_dados BY lifnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_dados COMPARING lifnr.

  SELECT lifnr name1
    FROM lfa1
    INTO TABLE ti_lfa1
    FOR ALL ENTRIES IN tl_dados
  WHERE  lifnr EQ tl_dados-lifnr.

  SORT ti_lfa1 BY lifnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_LFA1

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFBK                                         *
*&---------------------------------------------------------------------*
*                              Seleciona LFBK                          *
*----------------------------------------------------------------------*
FORM z_seleciona_lfbk.

  REFRESH ti_lfbk.

  CHECK NOT ti_lfa1[] IS INITIAL.

  SELECT lifnr banks bankl
         bankn bkont bvtyp
    FROM lfbk
    INTO TABLE ti_lfbk
    FOR ALL ENTRIES IN ti_lfa1
  WHERE  lifnr EQ ti_lfa1-lifnr.

  SORT ti_lfbk BY lifnr ASCENDING
                  bvtyp ASCENDING.

ENDFORM.                    " Z_SELECIONA_LFBK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_BNKA                                         *
*&---------------------------------------------------------------------*
*                               Seleciona BNKA                         *
*----------------------------------------------------------------------*
FORM z_seleciona_bnka.

  DATA tl_lfbk TYPE TABLE OF type_lfbk.

  CHECK NOT ti_lfbk[] IS INITIAL.
  tl_lfbk[] = ti_lfbk[].
  SORT tl_lfbk BY banks ASCENDING
                  bankl ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lfbk COMPARING banks bankl.

  SELECT banks bankl banka
    FROM bnka
    INTO TABLE ti_bnka
    FOR ALL ENTRIES IN tl_lfbk
  WHERE  banks EQ tl_lfbk-banks
    AND  bankl EQ tl_lfbk-bankl.

ENDFORM.                    " Z_SELECIONA_BNKA

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIBE_DADOS  OUTPUT                                 *
*&---------------------------------------------------------------------*
*                               Exibe Dados                            *
*----------------------------------------------------------------------*
MODULE zm_exibe_dados OUTPUT.

  PERFORM z_exibe_dados.

ENDMODULE.                 " ZM_EXIBE_DADOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_exibe_dados.

  DATA: sl_dados TYPE zftte_dados,
        sl_lfa1  TYPE type_lfa1,
        sl_lfbk  TYPE type_lfbk,
        sl_bnka  TYPE type_bnka,
        sl_t007s TYPE type_t007s,
        sl_makt  TYPE type_makt.

  CLEAR: sl_dados,
         sl_lfa1 ,
         sl_lfbk ,
         sl_bnka ,
         sl_t007s,
         sl_makt .

  READ TABLE ti_0200 INTO sl_dados
    INDEX vg_index.

  READ TABLE ti_lfa1 INTO sl_lfa1
    WITH KEY lifnr = sl_dados-lifnr
    BINARY SEARCH.

  IF sl_dados-bvtyp IS INITIAL.
    READ TABLE ti_lfbk INTO sl_lfbk
      WITH KEY lifnr = sl_dados-lifnr
      BINARY SEARCH.
  ELSE.
    READ TABLE ti_lfbk INTO sl_lfbk
      WITH KEY lifnr = sl_dados-lifnr
               bvtyp = sl_dados-bvtyp
      BINARY SEARCH.
  ENDIF.

  READ TABLE ti_bnka INTO sl_bnka
    WITH KEY banks = sl_lfbk-banks
             bankl = sl_lfbk-bankl
    BINARY SEARCH.

  READ TABLE ti_t007s INTO sl_t007s
    WITH KEY mwskz = sl_dados-iva
    BINARY SEARCH.

  READ TABLE ti_makt INTO sl_makt
    WITH KEY matnr = sl_dados-matns
    BINARY SEARCH.

  s_tela-tdlnr      = sl_dados-tdlnr.
  s_tela-name1      = sl_dados-name1.
  s_tela-tknum      = sl_dados-tknum.
  s_tela-fknum      = sl_dados-fknum.
  s_tela-nfenum     = sl_dados-nfenum.
  s_tela-zdt_mov    = sl_dados-zdt_mov.
  s_tela-zdt_vencto = sl_dados-zdt_vencto.
  s_tela-nr_conhec  = sl_dados-nr_conhec.
  s_tela-cotacao    = sl_dados-cotacao.
  s_tela-series     = sl_dados-series.
  s_tela-zdt_conhec = sl_dados-zdt_conhec.
  s_tela-matnr      = sl_dados-matnr.
  s_tela-maktx      = sl_dados-maktx.
  s_tela-matns      = sl_dados-matns.
  s_tela-comp_valor = sl_dados-comp_valor.
  s_tela-comp_tipo  = sl_dados-tipo.
  s_tela-bvtyp      = sl_dados-bvtyp.
  s_tela-bankl      = sl_bnka-bankl(3).
  s_tela-banka      = sl_bnka-banka.
  s_tela-bankn      = sl_lfbk-bankn.
  s_tela-text1      = sl_t007s-text1.
  s_tela-maktg      = sl_makt-maktg.
  s_tela-iva        = sl_dados-iva.
  s_tela-nfe        = sl_dados-nfe.

  CONCATENATE sl_lfa1-lifnr
              sl_lfa1-name1
         INTO s_tela-texto SEPARATED BY '-'.

  IF NOT sl_lfbk-bkont IS INITIAL.
    CONCATENATE sl_lfbk-bankl+4(11)
                sl_lfbk-bkont
           INTO s_tela-agenc SEPARATED BY '-'.
  ELSE.
    s_tela-agenc = sl_lfbk-bankl+4(11).
  ENDIF.

  IF sl_dados-waers NE 'BRL'.
    LOOP AT SCREEN.
      IF screen-name EQ 'S_TELA-COTACAO'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " Z_EXIBE_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_T007S                                        *
*&---------------------------------------------------------------------*
*                           Seleciona T007S                            *
*----------------------------------------------------------------------*
FORM z_seleciona_t007s.

  REFRESH ti_t007s.

  SELECT spras kalsm
         mwskz text1
    FROM t007s
    INTO TABLE ti_t007s
  WHERE  spras EQ sy-langu.

  SORT ti_t007s BY mwskz ASCENDING.

ENDFORM.                    " Z_SELECIONA_T007S

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MAKT                                         *
*&---------------------------------------------------------------------*
*                              Seleciona MAKT                          *
*----------------------------------------------------------------------*
FORM z_seleciona_makt.

  DATA tl_dados TYPE TABLE OF zftte_dados.

  REFRESH ti_makt.

  CHECK NOT ti_dados[] IS INITIAL.
  tl_dados[] = ti_dados[].
  SORT tl_dados BY matns ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_dados COMPARING matns.

  SELECT matnr spras
         maktx maktg
    FROM makt
    INTO TABLE ti_makt
    FOR ALL ENTRIES IN tl_dados
  WHERE matnr EQ tl_dados-matns
    AND spras EQ sy-langu.

  SORT ti_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                               Exit Command                           *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0200'.
      CASE sy-ucomm.
        WHEN c_cancelar.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_STATUS_0200                                            *
*&---------------------------------------------------------------------*
*                              Status Tela 0200                        *
*----------------------------------------------------------------------*
FORM z_status_0200.

  DATA: tl_code TYPE TABLE OF sy-ucomm,
        sl_code TYPE sy-ucomm.

  IF vg_total EQ 1.
    sl_code = 'PROXIMO'.
    APPEND sl_code TO tl_code.
    sl_code = 'ANTERIOR'.
    APPEND sl_code TO tl_code.
  ENDIF.

  IF vg_index EQ 1 AND NOT
     vg_total EQ 1.
    sl_code = 'ANTERIOR'.
    APPEND sl_code TO tl_code.
  ENDIF.

  IF vg_index EQ vg_total AND
     vg_total NE 1.
    sl_code = 'PROXIMO'.
    APPEND sl_code TO tl_code.
  ENDIF.

  SET PF-STATUS 'PF0200' EXCLUDING tl_code.

ENDFORM.                    " Z_STATUS_0200

*&---------------------------------------------------------------------*
*&      Module  Z_ATUALIZA_CAMPOS  INPUT                               *
*&---------------------------------------------------------------------*
*                            Atualiza Campos                           *
*----------------------------------------------------------------------*
MODULE z_atualiza_campos INPUT.

  PERFORM z_atualiza_tabela.

ENDMODULE.                 " Z_ATUALIZA_CAMPOS  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_TABELA                                        *
*&---------------------------------------------------------------------*
*                             Atualiza Tabela                          *
*----------------------------------------------------------------------*
FORM z_atualiza_tabela.

  DATA sl_dados TYPE zftte_dados.

  sl_dados-zdt_mov    = s_tela-zdt_mov.
  sl_dados-zdt_vencto = s_tela-zdt_vencto.
  sl_dados-nr_conhec  = s_tela-nr_conhec.
  sl_dados-series     = s_tela-series.
  sl_dados-zdt_conhec = s_tela-zdt_conhec.
  sl_dados-tipo       = s_tela-comp_tipo.
  sl_dados-iva        = s_tela-iva.
  sl_dados-nfe        = s_tela-nfe.
  sl_dados-comp_valor = s_tela-comp_valor.
  sl_dados-cotacao    = s_tela-cotacao.
  sl_dados-it_impostos_retidos = s_tela-it_impostos_retidos.

  MODIFY ti_0200 FROM sl_dados
    INDEX vg_index
    TRANSPORTING zdt_mov
                 zdt_vencto
                 nr_conhec
                 series
                 zdt_conhec
                 tipo
                 iva
                 nfe
                 comp_valor
                 cotacao
                 it_impostos_retidos.

  MODIFY ti_0200 FROM sl_dados
   TRANSPORTING zdt_mov
                zdt_vencto
                nr_conhec
                series
                zdt_conhec
                tipo
                iva
                nfe
                cotacao
                it_impostos_retidos
   WHERE nr_conhec EQ space.

ENDFORM.                    " Z_ATUALIZA_TABELA

*&---------------------------------------------------------------------*
*&      Form  Z_CONFIRMAR                                              *
*&---------------------------------------------------------------------*
*                                Confirmar                             *
*----------------------------------------------------------------------*
FORM z_confirmar.

  DATA: sl_dados TYPE zftte_dados,
        vl_index TYPE sytabix.

  LOOP AT ti_0200 INTO sl_dados.

    vl_index = sy-tabix.

    CHECK sl_dados-comp_valor NE 0.

    PERFORM z_busca_taxas CHANGING sl_dados.

    MODIFY ti_0200 FROM sl_dados
      INDEX vl_index.

    READ TABLE ti_saida
      WITH KEY tknum = sl_dados-tknum
     TRANSPORTING NO FIELDS.

    IF sy-subrc IS INITIAL.
      sl_dados-status = icon_yellow_light.
      MODIFY ti_saida FROM sl_dados
        INDEX sy-tabix.
    ENDIF.

    CLEAR sl_dados.

  ENDLOOP.

ENDFORM.                    " Z_CONFIRMAR

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_TAXAS                                            *
*&---------------------------------------------------------------------*
*                               Busca Taxas                            *
*----------------------------------------------------------------------*
FORM z_busca_taxas CHANGING p_dados TYPE zftte_dados.

  DATA: tl_1btxic1    TYPE TABLE OF j_1btxic1,
        tl_1btxpis    TYPE TABLE OF j_1btxpis,
        tl_1btxcof    TYPE TABLE OF j_1btxcof,
        sl_1btxic1    TYPE j_1btxic1,
        sl_1btxpis    TYPE j_1btxpis,
        sl_1btxcof    TYPE j_1btxcof,
        vl_vencimento TYPE char10,
        vl_inicio_val TYPE char10,
        vl_dt_venc    TYPE zdt_vencto,
        vl_dt_inic    TYPE zdt_vencto,
        vl_valor      TYPE j_1bvalue.


  p_dados-base_icms    = 0.
  p_dados-base_pis     = 0.
  p_dados-base_cofins  = 0.
  p_dados-rate_icms    = 0.
  p_dados-rate_pis     = 0.
  p_dados-rate_cofins  = 0.
  p_dados-valor_icms   = 0.
  p_dados-valor_pis    = 0.
  p_dados-valor_cofins = 0.

  IF p_dados-iva EQ c_i8 OR
     p_dados-iva EQ c_i1 .

*---> 13/06/2023 - Migração S4 - JS
*        P_DADOS-BASE_ICMS = P_DADOS-COMP_VALOR.
    p_dados-base_icms = CONV #( p_dados-comp_valor ).
*<--- 13/06/2023 - Migração S4 - JS

    SELECT *
      FROM j_1btxic1
      INTO TABLE tl_1btxic1
     WHERE land1    EQ 'BR'
       AND shipfrom EQ p_dados-regio_emissor
       AND shipto   EQ p_dados-regio_receptor.

    SORT tl_1btxic1 BY validfrom.

    LOOP AT tl_1btxic1 INTO sl_1btxic1.

      CLEAR: vl_vencimento,
             vl_dt_venc   .

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = sl_1btxic1-validfrom
        IMPORTING
          output = vl_vencimento.

      WRITE vl_vencimento TO vl_dt_venc.

      IF  vl_dt_venc        LE p_dados-zdt_conhec AND
          p_dados-rate_icms EQ 0 .
        p_dados-rate_icms = sl_1btxic1-rate.
        IF sl_1btxic1-rate GT 0.
          p_dados-valor_icms = p_dados-base_icms * ( sl_1btxic1-rate / 100 ).
        ENDIF.
      ENDIF.

      CLEAR sl_1btxic1.

    ENDLOOP.

  ENDIF.

  IF p_dados-iva EQ c_i0 OR
     p_dados-iva EQ c_i1 OR
     p_dados-iva EQ c_s1.

*---> 13/06/2023 - Migração S4 - JS
*      P_DADOS-BASE_PIS    = P_DADOS-COMP_VALOR.
*      P_DADOS-BASE_COFINS = P_DADOS-COMP_VALOR.
    p_dados-base_pis    = CONV #( p_dados-comp_valor ).
    p_dados-base_cofins = CONV #( p_dados-comp_valor ).
*<--- 13/06/2023 - Migração S4 - JS

    vl_valor = p_dados-werks.

    SELECT *
      FROM j_1btxpis
      INTO TABLE tl_1btxpis
     WHERE country EQ 'BR'
       AND gruop   EQ '72'
       AND value   EQ vl_valor.

    LOOP AT tl_1btxpis INTO sl_1btxpis.

      CLEAR: vl_inicio_val,
             vl_vencimento,
             vl_dt_venc   ,
             vl_inicio_val.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = sl_1btxpis-validfrom
        IMPORTING
          output = vl_inicio_val.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = sl_1btxpis-validto
        IMPORTING
          output = vl_vencimento.

      WRITE: vl_vencimento TO vl_dt_venc   ,
             vl_inicio_val TO vl_inicio_val.

      IF vl_vencimento GE p_dados-zdt_conhec  AND
         vl_inicio_val LE p_dados-zdt_conhec.
        p_dados-rate_pis = sl_1btxpis-rate.
        IF sl_1btxpis-rate GT 0.
          p_dados-valor_pis = p_dados-base_pis * ( sl_1btxpis-rate / 100 ).
        ENDIF.
      ENDIF.

      CLEAR sl_1btxpis.

    ENDLOOP.

    SELECT *
      FROM j_1btxcof
      INTO TABLE tl_1btxcof
     WHERE country EQ 'BR'
       AND gruop   EQ '71'
       AND value   EQ vl_valor.

    LOOP AT tl_1btxcof INTO sl_1btxcof.

      CLEAR: vl_inicio_val,
             vl_vencimento,
             vl_dt_venc   ,
             vl_inicio_val.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = sl_1btxcof-validfrom
        IMPORTING
          output = vl_inicio_val.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = sl_1btxcof-validto
        IMPORTING
          output = vl_vencimento.

      WRITE: vl_vencimento TO vl_dt_venc   ,
             vl_inicio_val TO vl_inicio_val.

      IF vl_dt_venc    GE p_dados-zdt_conhec AND
         vl_inicio_val LE p_dados-zdt_conhec.
        p_dados-rate_cofins = sl_1btxcof-rate.
        IF sl_1btxcof-rate GT 0.
          p_dados-valor_cofins = p_dados-base_cofins * ( sl_1btxcof-rate / 100 ).
        ENDIF.
      ENDIF.

      CLEAR sl_1btxcof.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " Z_BUSCA_TAXAS

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR                                                  *
*&---------------------------------------------------------------------*
*                              Função Gerar                            *
*----------------------------------------------------------------------*
FORM z_gerar USING p_estorno TYPE char01.

  DATA: tl_index TYPE lvc_t_row,
        tl_row   TYPE lvc_t_roid,
        tl_dados TYPE TABLE OF zftte_dados,
        tl_aux   TYPE TABLE OF zftte_dados,
        sl_index TYPE lvc_s_row,
        sl_saida TYPE zftte_dados,
        vl_lines TYPE syindex,
        vl_check TYPE char1.

  CLEAR vl_check.

  CALL METHOD s_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_index
      et_row_no     = tl_row.

  IF tl_index[] IS INITIAL.
    MESSAGE i897 WITH TEXT-002.
    EXIT.
  ENDIF.

  LOOP AT tl_index INTO sl_index.
    READ TABLE ti_saida INTO sl_saida
      INDEX sl_index-index.
    APPEND sl_saida TO tl_dados.
    CLEAR: sl_index,
           sl_saida.
  ENDLOOP.

  IF p_estorno IS INITIAL.
    DELETE tl_dados WHERE comp_belnr NE space.
  ELSE.
    DELETE tl_dados WHERE comp_belnr EQ space AND comp_docnum EQ space.
  ENDIF.

  IF tl_dados[] IS INITIAL.
    MESSAGE i897 WITH TEXT-004.
    EXIT.
  ENDIF.

  LOOP AT tl_dados INTO sl_saida.
    IF p_estorno IS INITIAL.
      IF sl_saida-comp_belnr NE space.
        vl_check = 'X'.
        MESSAGE i897 WITH TEXT-007.
        EXIT.
      ENDIF.
      IF sl_saida-comp_valor EQ 0.
        vl_check = 'X'.
        MESSAGE i897 WITH TEXT-006.
        EXIT.
      ENDIF.
    ELSE.
      IF ( sl_saida-comp_belnr EQ space ) AND ( sl_saida-comp_docnum EQ space ).
        vl_check = 'X'.
        MESSAGE i897 WITH TEXT-007.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK vl_check IS INITIAL.
  ti_0200[] = tl_dados[].

  PERFORM z_gerar_complemento USING p_estorno.

ENDFORM.                    " Z_GERAR

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_COMPLEMENTO                                      *
*&---------------------------------------------------------------------*
*                            Gerar Complemento                         *
*----------------------------------------------------------------------*
FORM z_gerar_complemento USING p_estorno TYPE char01.

  DATA: tl_sum   TYPE TABLE OF zftte_dados2,
        tl_aux   TYPE TABLE OF zftte_dados,
        sl_sum   TYPE zftte_dados2,
        sl_dados TYPE zftte_dados.

  SORT ti_0200 BY tdlnr ASCENDING.
  tl_aux[] = ti_0200[].
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING tdlnr.

  CLEAR: ti_eventos[].

  LOOP AT ti_0200 INTO sl_dados.

    sl_sum-tdlnr        = sl_dados-tdlnr.
    sl_sum-base_icms    = sl_dados-base_icms.
    sl_sum-base_pis     = sl_dados-base_pis.
    sl_sum-base_cofins  = sl_dados-base_cofins.
    sl_sum-rate_icms    = sl_dados-rate_icms.
    sl_sum-rate_pis     = sl_dados-rate_pis.
    sl_sum-rate_cofins  = sl_dados-rate_cofins.
    sl_sum-valor_icms   = sl_dados-valor_icms.
    sl_sum-valor_pis    = sl_dados-valor_pis.
    sl_sum-valor_cofins = sl_dados-valor_cofins.
    sl_sum-comp_valor   = sl_dados-comp_valor.

    COLLECT sl_sum INTO tl_sum.

    CLEAR: sl_dados,
           sl_sum  .

  ENDLOOP.

  LOOP AT tl_aux INTO sl_dados.

    IF p_estorno IS INITIAL.

*   Gera Nota
      IF ( sl_dados-comp_docnum IS INITIAL ) AND ( sl_dados-iva NE 'S1' ).
        PERFORM z_gera_nota TABLES tl_sum
                             USING space
                          CHANGING sl_dados.
      ENDIF.
*   Preenche ZIB_CONTABIL
      PERFORM z_preenche_contabil TABLES tl_sum
                                   USING space
                                CHANGING sl_dados.

      MODIFY ti_0200 FROM sl_dados
        TRANSPORTING comp_docnum comp_belnr comp_gjahr id_envio
        WHERE tdlnr EQ sl_dados-tdlnr.

    ELSE.

*   Gera Nota
      IF sl_dados-comp_docnum IS NOT INITIAL.
        PERFORM z_gera_nota TABLES tl_sum
                             USING  c_x
                          CHANGING sl_dados.
      ENDIF.
*   Preenche ZIB_CONTABIL
      PERFORM z_preenche_contabil TABLES tl_sum
                                   USING c_x
                                CHANGING sl_dados.

      MODIFY ti_0200 FROM sl_dados
        TRANSPORTING comp_docnum comp_belnr comp_gjahr id_envio
        WHERE tdlnr EQ sl_dados-tdlnr.

    ENDIF.

    CLEAR sl_dados.

  ENDLOOP.

  LOOP AT ti_0200 INTO sl_dados.

    IF p_estorno IS INITIAL.

      IF ( NOT sl_dados-comp_docnum IS INITIAL ) OR ( NOT sl_dados-comp_belnr IS INITIAL ).

        PERFORM atualiza_status CHANGING sl_dados.

        READ TABLE ti_saida
          WITH KEY tknum = sl_dados-tknum
          BINARY SEARCH
          TRANSPORTING NO FIELDS.

        IF sy-subrc IS INITIAL.
          MODIFY ti_saida FROM sl_dados
            INDEX sy-tabix
            TRANSPORTING comp_docnum comp_belnr comp_gjahr status id_envio.
        ENDIF.

      ENDIF.

    ELSE.

      IF ( sl_dados-comp_docnum IS INITIAL ) OR ( sl_dados-comp_belnr IS INITIAL ).

        PERFORM atualiza_status CHANGING sl_dados.

        READ TABLE ti_saida
          WITH KEY tknum = sl_dados-tknum
          BINARY SEARCH
          TRANSPORTING NO FIELDS.

        IF sy-subrc IS INITIAL.
          MODIFY ti_saida FROM sl_dados
            INDEX sy-tabix
            TRANSPORTING comp_docnum comp_belnr comp_gjahr status id_envio.
        ENDIF.

      ENDIF.

    ENDIF.

    CLEAR sl_dados.

  ENDLOOP.

* Preenche ZLEST0042
  PERFORM z_preenche_zlest0042 TABLES ti_0200.

ENDFORM.                    " Z_GERAR_COMPLEMENTO

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CONTABIL                                      *
*&---------------------------------------------------------------------*
*                       Preenche ZIB_CONTABIL                          *
*----------------------------------------------------------------------*
FORM z_preenche_contabil TABLES p_sum     STRUCTURE s_dados2
                          USING p_estorno TYPE char01
                       CHANGING p_dados   TYPE zftte_dados.

  DATA: tl_contabil         TYPE TABLE OF zib_contabil INITIAL SIZE 0 WITH HEADER LINE,
        tl_contabil_irt     TYPE TABLE OF zib_contabil_irt INITIAL SIZE 0 WITH HEADER LINE,
        wa_contabil         TYPE zib_contabil,
        sl_zlest0021        TYPE zlest0021,
        sl_zlest0042        TYPE zlest0042,
        sl_zib_contabil     TYPE zib_contabil,
        sl_zib_contabil_irt TYPE zib_contabil_irt,
        sl_sum              TYPE zftte_dados2,
        sl_zlsch            TYPE type_zlsch,
        vl_object           TYPE awkey,
        tx_sgtxt            TYPE sgtxt.

  DATA: it_document  TYPE TABLE OF zfie_document INITIAL SIZE 0 WITH HEADER LINE,
        it_header    TYPE TABLE OF zfie_documentheader INITIAL SIZE 0 WITH HEADER LINE,
        it_item      TYPE TABLE OF zfie_documentitem INITIAL SIZE 0 WITH HEADER LINE,
        it_return    TYPE TABLE OF zfie_return INITIAL SIZE 0 WITH HEADER LINE,
        wa_document  TYPE zfie_document,
        wa_return    TYPE zfie_return,
        tl_return    TYPE TABLE OF bapiret2 INITIAL SIZE 0 WITH HEADER LINE,
        wa_returnl   TYPE bapiret2,
        wa_zlest0043 TYPE zlest0043,
        p_valor      TYPE netwr_fp,
        p_cotacao    TYPE ukurs_curr,
        i_tcurr      TYPE tcurr_curr,
        i_data       TYPE gdatu_inv.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  REFRESH: tl_contabil, tl_contabil_irt.

  READ TABLE p_sum INTO sl_sum
    WITH KEY tdlnr = p_dados-tdlnr
    BINARY SEARCH.

  READ TABLE ti_zlsch INTO sl_zlsch
    WITH KEY tknum = p_dados-tknum
    BINARY SEARCH.

  CONCATENATE 'E' p_dados-re_belnr p_dados-id_envio INTO vl_object.

  SELECT SINGLE * INTO wa_zlest0043 FROM zlest0043 WHERE cd_tipo EQ p_dados-tipo.

  PERFORM z_seleciona_zlest0021 USING wa_zlest0043 p_dados-shtyp p_dados-budat.

*---> 13/06/2023 - Migração S4 - JS
*      P_VALOR = SL_SUM-COMP_VALOR.
  p_valor = CONV #( sl_sum-comp_valor ).
*<--- 13/06/2023 - Migração S4 - JS

  IF p_dados-waers EQ 'BRL'.
    p_cotacao = 1.
  ELSE.
    IF p_dados-cotacao IS NOT INITIAL.
      p_cotacao = p_dados-cotacao.
    ELSE.
      CREATE OBJECT obj_zcl_util_sd.
      IF p_dados-zdt_conhec IS NOT INITIAL.
        i_data = p_dados-zdt_conhec.
      ELSE.
        i_data = sy-datum.
      ENDIF.
      i_tcurr = 'BRL'.
      obj_zcl_util_sd->set_data(  EXPORTING i_data    = i_data ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst   = p_dados-kurst ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk   = p_dados-waers ).
      obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr   = i_tcurr ).
      obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = p_cotacao ).
      CLEAR: obj_zcl_util_sd.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
    EXPORTING
      p_bukrs           = p_dados-bukrs
      p_lifnr           = p_dados-tdlnr
      p_valor           = p_valor
      p_cotacao         = p_cotacao
      p_bvtyp           = p_dados-bvtyp
    IMPORTING
      p_forma_pagamento = sl_zib_contabil-zlsch
      p_princ_bnc_emp   = sl_zib_contabil-hbkid
    EXCEPTIONS
      nao_fornecedor    = 1
      fornecedor_conta  = 2
      fornecedor_banco  = 3
      faixa_valor       = 4
      OTHERS            = 5.

  IF sy-subrc <> 0.
    CLEAR tl_return.
    tl_return-type       = sy-msgty.
    tl_return-id         = sy-msgid.
    tl_return-number     = sy-msgno.
    tl_return-message_v1 = sy-msgv1.
    tl_return-message_v2 = sy-msgv2.
    tl_return-message_v3 = sy-msgv3.
    tl_return-message_v4 = sy-msgv4.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO tl_return-message.
    APPEND tl_return.
    PERFORM popula_log TABLES tl_return.
    EXIT.
  ENDIF.

* 1 ª Partida
  sl_zib_contabil-obj_key       = vl_object.
  sl_zib_contabil-seqitem       = 1.
  CASE p_estorno.
    WHEN space.
      sl_zib_contabil-bschl     = '31'.
      CONCATENATE 'Fatura Compl.' p_dados-name1 INTO tx_sgtxt SEPARATED BY space.
    WHEN c_x.
      sl_zib_contabil-bschl     = '21'.
      CONCATENATE 'Estorno Fatura Compl.' p_dados-name1 INTO tx_sgtxt SEPARATED BY space.
  ENDCASE.
  sl_zib_contabil-gsber         = p_dados-werks.
  sl_zib_contabil-bukrs         = p_dados-bukrs.
  sl_zib_contabil-interface     = '99'.
  sl_zib_contabil-bktxt         = p_dados-lblni.

  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = p_dados-zdt_conhec
    IMPORTING
      output = sl_zib_contabil-bldat.
  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = sy-datum
    IMPORTING
      output = sl_zib_contabil-budat.
  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = p_dados-zdt_vencto
    IMPORTING
      output = sl_zib_contabil-zfbdt.

  sl_zib_contabil-gjahr         = sy-datum(04).
  sl_zib_contabil-monat         = sy-datum+04(02).
  sl_zib_contabil-blart         = 'FT'.
  CONCATENATE p_dados-nr_conhec '-' p_dados-series INTO sl_zib_contabil-xblnr.
  sl_zib_contabil-hkont         = p_dados-tdlnr.
  sl_zib_contabil-wrbtr         = sl_sum-comp_valor.
  sl_zib_contabil-waers         = p_dados-waers.
  sl_zib_contabil-zlspr         = 'A'.
  sl_zib_contabil-kidno         = space.
  sl_zib_contabil-sgtxt         = tx_sgtxt.
  sl_zib_contabil-xref1         = space.
  sl_zib_contabil-xref2         = space.
  sl_zib_contabil-xref3         = space.
  sl_zib_contabil-bupla         = p_dados-werks.
  sl_zib_contabil-zuonr         = p_dados-tknum.
  sl_zib_contabil-umskz         = space.
  sl_zib_contabil-kostl         = space.
  sl_zib_contabil-aufnr         = space.
  sl_zib_contabil-prctr         = space.

  IF p_dados-waers = 'USD'.
    sl_zib_contabil-waers_f     = p_dados-waers.
    sl_zib_contabil-dmbe2       = sl_zib_contabil-wrbtr.
    sl_zib_contabil-waers_i     = 'BRL'.
    sl_zib_contabil-dmbtr       = sl_zib_contabil-wrbtr * p_cotacao.
  ELSE.
    sl_zib_contabil-waers_f     = space.
    sl_zib_contabil-dmbe2       = space.
    sl_zib_contabil-waers_i     = 'BRL'.
    sl_zib_contabil-dmbtr       = sl_zib_contabil-wrbtr.
  ENDIF.

  sl_zib_contabil-bvtyp         = p_dados-bvtyp.
  sl_zib_contabil-rg_atualizado = 'N'.
  sl_zib_contabil-bankl         = space.
  sl_zib_contabil-bankn         = space.
  APPEND sl_zib_contabil TO tl_contabil.

  "Verifica Impostos Retidos """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF p_dados-it_impostos_retidos[] IS NOT INITIAL.
    LOOP AT p_dados-it_impostos_retidos INTO DATA(wa_imposto_retiro).
      CLEAR: sl_zib_contabil_irt.
      sl_zib_contabil_irt-obj_key     = sl_zib_contabil-obj_key.
      sl_zib_contabil_irt-itemno_acc  = sl_zib_contabil-seqitem.
      sl_zib_contabil_irt-bas_amt_tc  = wa_imposto_retiro-base.
      sl_zib_contabil_irt-man_amt_tc  = wa_imposto_retiro-taxval.
      sl_zib_contabil_irt-wt_type     = wa_imposto_retiro-witht.
      sl_zib_contabil_irt-wt_code     = wa_imposto_retiro-wt_withcd.
      sl_zib_contabil_irt-bas_amt_ind = abap_true.
      sl_zib_contabil_irt-man_amt_ind = abap_true.
      APPEND sl_zib_contabil_irt TO tl_contabil_irt.
    ENDLOOP.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CLEAR sl_zib_contabil.

* 2 ª Partida
  CLEAR sl_zlest0021.
  READ TABLE ti_zlest0021 INTO sl_zlest0021 WITH KEY shtyp = p_dados-shtyp operfrete = wa_zlest0043-operfrete
  BINARY SEARCH.

  IF sy-subrc IS INITIAL.

    sl_zib_contabil-obj_key       = vl_object.
    sl_zib_contabil-seqitem       = 2.

    CASE p_estorno.
      WHEN space.
        sl_zib_contabil-bschl     = '40'.
      WHEN c_x.
        sl_zib_contabil-bschl     = '50'.
    ENDCASE.

    sl_zib_contabil-gsber         = p_dados-werks.
    sl_zib_contabil-bukrs         = p_dados-bukrs.
    sl_zib_contabil-interface     = '99'.
    sl_zib_contabil-bktxt         = p_dados-lblni.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = p_dados-zdt_mov
      IMPORTING
        output = sl_zib_contabil-bldat.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = sy-datum
      IMPORTING
        output = sl_zib_contabil-budat.

    sl_zib_contabil-gjahr         = sy-datum(04).
    sl_zib_contabil-monat         = sy-datum+04(02).
    sl_zib_contabil-blart         = 'FT'.
    CONCATENATE p_dados-nr_conhec '-' p_dados-series INTO sl_zib_contabil-xblnr.
    sl_zib_contabil-hkont         =  sl_zlest0021-razaodeb.
    CASE p_dados-iva.
      WHEN c_i8.
        sl_zib_contabil-wrbtr = sl_sum-comp_valor -
                                sl_sum-valor_icms.
      WHEN c_i0 OR c_s1.
        sl_zib_contabil-wrbtr = sl_sum-comp_valor -
                                sl_sum-valor_pis  -
                                sl_sum-valor_cofins.
      WHEN c_i1.
        sl_zib_contabil-wrbtr = sl_sum-comp_valor -
                                sl_sum-valor_icms -
                                sl_sum-valor_pis  -
                                sl_sum-valor_cofins.
      WHEN OTHERS.
        sl_zib_contabil-wrbtr = sl_sum-comp_valor.
    ENDCASE.
    sl_zib_contabil-waers         = p_dados-waers.
    sl_zib_contabil-zfbdt         = space.
    sl_zib_contabil-zlspr         = space.
    sl_zib_contabil-zlsch         = space.
    sl_zib_contabil-kidno         = space.
    sl_zib_contabil-sgtxt         = tx_sgtxt.
    sl_zib_contabil-xref1         = space.
    sl_zib_contabil-xref2         = space.
    sl_zib_contabil-xref3         = space.
    sl_zib_contabil-bupla         = p_dados-werks.
    sl_zib_contabil-zuonr         = p_dados-tknum.
    sl_zib_contabil-umskz         = space.
    sl_zib_contabil-kostl         = space.
    sl_zib_contabil-aufnr         = space.
    sl_zib_contabil-prctr         = space.
    IF p_dados-waers = 'BRL'.
      sl_zib_contabil-waers_i       = p_dados-waers.
      sl_zib_contabil-dmbtr         = sl_zib_contabil-wrbtr.
      sl_zib_contabil-waers_f       = space.
      sl_zib_contabil-dmbe2         = space.
    ELSE.
      sl_zib_contabil-waers_i       = 'BRL'.
      sl_zib_contabil-dmbtr         = sl_zib_contabil-wrbtr * p_cotacao.
      sl_zib_contabil-waers_f       = p_dados-waers.
      sl_zib_contabil-dmbe2         = sl_zib_contabil-wrbtr.
    ENDIF.
    sl_zib_contabil-bvtyp         = space.
    sl_zib_contabil-hbkid         = p_dados-bvtyp.
    sl_zib_contabil-rg_atualizado = 'N'.
    sl_zib_contabil-bankl         = space.
    sl_zib_contabil-bankn         = space.

    APPEND sl_zib_contabil TO tl_contabil.
    CLEAR sl_zib_contabil.

  ENDIF.

* 3 ª Partida
  CLEAR sl_zlest0021.
  READ TABLE ti_zlest0021 INTO sl_zlest0021
    WITH KEY shtyp     = p_dados-shtyp
             operfrete = c_5
    BINARY SEARCH.

  IF ( sy-subrc IS INITIAL ) AND ( ( p_dados-iva EQ c_i8 ) OR ( p_dados-iva EQ c_i1 ) ).

    sl_zib_contabil-obj_key       = vl_object.
    sl_zib_contabil-seqitem       = 3.

    CASE p_estorno.
      WHEN space.
        sl_zib_contabil-bschl     = '40'.
      WHEN c_x.
        sl_zib_contabil-bschl     = '50'.
    ENDCASE.

    sl_zib_contabil-gsber         = p_dados-werks.
    sl_zib_contabil-bukrs         = p_dados-bukrs.
    sl_zib_contabil-interface     = '99'.
    sl_zib_contabil-bktxt         = p_dados-lblni.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = p_dados-zdt_mov
      IMPORTING
        output = sl_zib_contabil-bldat.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = sy-datum
      IMPORTING
        output = sl_zib_contabil-budat.

    sl_zib_contabil-gjahr         = sy-datum(04).
    sl_zib_contabil-monat         = sy-datum+04(02).
    sl_zib_contabil-blart         = 'FT'.
    CONCATENATE p_dados-nr_conhec '-' p_dados-series INTO sl_zib_contabil-xblnr.
    sl_zib_contabil-hkont         = sl_zlest0021-razaodeb.
    sl_zib_contabil-wrbtr         = sl_sum-valor_icms.
    sl_zib_contabil-waers         = p_dados-waers.
    sl_zib_contabil-zfbdt         = space.
    sl_zib_contabil-zlspr         = space.
    sl_zib_contabil-zlsch         = space.
    sl_zib_contabil-kidno         = space.
    sl_zib_contabil-sgtxt         = tx_sgtxt.
    sl_zib_contabil-xref1         = space.
    sl_zib_contabil-xref2         = space.
    sl_zib_contabil-xref3         = space.
    sl_zib_contabil-bupla         = p_dados-werks.
    sl_zib_contabil-zuonr         = p_dados-tknum.
    sl_zib_contabil-umskz         = space.
    sl_zib_contabil-kostl         = space.
    sl_zib_contabil-aufnr         = space.
    sl_zib_contabil-prctr         = space.
    IF p_dados-waers = 'BRL'.
      sl_zib_contabil-waers_i       = p_dados-waers.
      sl_zib_contabil-dmbtr         = sl_zib_contabil-wrbtr.
      sl_zib_contabil-waers_f       = space.
      sl_zib_contabil-dmbe2         = space.
    ELSE.
      sl_zib_contabil-waers_i       = 'BRL'.
      sl_zib_contabil-dmbtr         = sl_zib_contabil-wrbtr * p_cotacao.
      sl_zib_contabil-waers_f       = p_dados-waers.
      sl_zib_contabil-dmbe2         = sl_zib_contabil-wrbtr.
    ENDIF.
    sl_zib_contabil-bvtyp         = space.
    sl_zib_contabil-hbkid         = p_dados-bvtyp.
    sl_zib_contabil-rg_atualizado = 'N'.
    sl_zib_contabil-bankl         = space.
    sl_zib_contabil-bankn         = space.

    APPEND sl_zib_contabil TO tl_contabil.
    CLEAR sl_zib_contabil.

  ENDIF.

  IF ( p_dados-iva EQ c_i1 ) OR ( p_dados-iva EQ c_i0 ) OR ( p_dados-iva EQ c_s1 ).

*   4 ª Partida
    CLEAR sl_zlest0021.
    READ TABLE ti_zlest0021 INTO sl_zlest0021
      WITH KEY shtyp     = p_dados-shtyp
               operfrete = c_9
      BINARY SEARCH.

    IF ( sy-subrc IS INITIAL ) AND ( sl_sum-valor_pis GT 0 ) .

      sl_zib_contabil-obj_key       = vl_object.
      sl_zib_contabil-seqitem       = 4.

      CASE p_estorno.
        WHEN space.
          sl_zib_contabil-bschl     = '40'.
        WHEN c_x.
          sl_zib_contabil-bschl     = '50'.
      ENDCASE.

      sl_zib_contabil-gsber         = p_dados-werks.
      sl_zib_contabil-bukrs         = p_dados-bukrs.
      sl_zib_contabil-interface     = '99'.
      sl_zib_contabil-bktxt         = p_dados-lblni.

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = p_dados-zdt_mov
        IMPORTING
          output = sl_zib_contabil-bldat.
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = sy-datum
        IMPORTING
          output = sl_zib_contabil-budat.

      sl_zib_contabil-gjahr         = sy-datum(04).
      sl_zib_contabil-monat         = sy-datum+04(02).
      sl_zib_contabil-blart         = 'FT'.
      CONCATENATE p_dados-nr_conhec '-' p_dados-series INTO sl_zib_contabil-xblnr.
      sl_zib_contabil-hkont         = sl_zlest0021-razaodeb.
      sl_zib_contabil-wrbtr         = sl_sum-valor_pis.
      sl_zib_contabil-waers         = p_dados-waers.
      sl_zib_contabil-zfbdt         = space.
      sl_zib_contabil-zlspr         = space.
      sl_zib_contabil-zlsch         = space.
      sl_zib_contabil-kidno         = space.
      sl_zib_contabil-sgtxt         = tx_sgtxt.
      sl_zib_contabil-xref1         = space.
      sl_zib_contabil-xref2         = space.
      sl_zib_contabil-xref3         = space.
      sl_zib_contabil-bupla         = p_dados-werks.
      sl_zib_contabil-zuonr         = p_dados-tknum.
      sl_zib_contabil-umskz         = space.
      sl_zib_contabil-kostl         = space.
      sl_zib_contabil-aufnr         = space.
      sl_zib_contabil-prctr         = space.
      IF p_dados-waers = 'BRL'.
        sl_zib_contabil-waers_i     = p_dados-waers.
        sl_zib_contabil-dmbtr       = sl_zib_contabil-wrbtr.
        sl_zib_contabil-waers_f     = space.
        sl_zib_contabil-dmbe2       = space.
      ELSE.
        sl_zib_contabil-waers_i     = 'BRL'.
        sl_zib_contabil-dmbtr       = sl_zib_contabil-wrbtr * p_cotacao.
        sl_zib_contabil-waers_f     = p_dados-waers.
        sl_zib_contabil-dmbe2       = sl_zib_contabil-wrbtr.
      ENDIF.
      sl_zib_contabil-bvtyp         = space.
      sl_zib_contabil-hbkid         = p_dados-bvtyp.
      sl_zib_contabil-rg_atualizado = 'N'.
      sl_zib_contabil-bankl         = space.
      sl_zib_contabil-bankn         = space.

      APPEND sl_zib_contabil TO tl_contabil.
      CLEAR sl_zib_contabil.

    ENDIF.

*   5 ª Partida
    CLEAR sl_zlest0021.
    READ TABLE ti_zlest0021 INTO sl_zlest0021
      WITH KEY shtyp     = p_dados-shtyp
               operfrete = c_1
      BINARY SEARCH.

    IF ( sy-subrc IS INITIAL ) AND ( sl_sum-valor_cofins GT 0 ).

      sl_zib_contabil-obj_key       = vl_object.
      sl_zib_contabil-seqitem       = 5.

      CASE p_estorno.
        WHEN space.
          sl_zib_contabil-bschl         = '40'.
        WHEN c_x.
          sl_zib_contabil-bschl         = '50'.
      ENDCASE.

      sl_zib_contabil-gsber         = p_dados-werks.
      sl_zib_contabil-bukrs         = p_dados-bukrs.
      sl_zib_contabil-interface     = '99'.
      sl_zib_contabil-bktxt         = p_dados-lblni.

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = p_dados-zdt_mov
        IMPORTING
          output = sl_zib_contabil-bldat.
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = sy-datum
        IMPORTING
          output = sl_zib_contabil-budat.

      sl_zib_contabil-gjahr         = sy-datum(04).
      sl_zib_contabil-monat         = sy-datum+04(02).
      sl_zib_contabil-blart         = 'FT'.
      CONCATENATE p_dados-nr_conhec '-' p_dados-series INTO sl_zib_contabil-xblnr.
      sl_zib_contabil-hkont         = sl_zlest0021-razaodeb.
      sl_zib_contabil-wrbtr         = sl_sum-valor_cofins.
      sl_zib_contabil-waers         = p_dados-waers.
      sl_zib_contabil-zfbdt         = space.
      sl_zib_contabil-zlspr         = space.
      sl_zib_contabil-zlsch         = space.
      sl_zib_contabil-kidno         = space.
      sl_zib_contabil-sgtxt         = tx_sgtxt.
      sl_zib_contabil-xref1         = space.
      sl_zib_contabil-xref2         = space.
      sl_zib_contabil-xref3         = space.
      sl_zib_contabil-bupla         = p_dados-werks.
      sl_zib_contabil-zuonr         = p_dados-tknum.
      sl_zib_contabil-umskz         = space.
      sl_zib_contabil-kostl         = space.
      sl_zib_contabil-aufnr         = space.
      sl_zib_contabil-prctr         = space.
      IF p_dados-waers = 'BRL'.
        sl_zib_contabil-waers_i       = p_dados-waers.
        sl_zib_contabil-dmbtr         = sl_zib_contabil-wrbtr.
        sl_zib_contabil-waers_f       = space.
        sl_zib_contabil-dmbe2         = space.
      ELSE.
        sl_zib_contabil-waers_i       = 'BRL'.
        sl_zib_contabil-dmbtr         = sl_zib_contabil-wrbtr * p_cotacao.
        sl_zib_contabil-waers_f       = p_dados-waers.
        sl_zib_contabil-dmbe2         = sl_zib_contabil-wrbtr.
      ENDIF.
      sl_zib_contabil-bvtyp         = space.
      sl_zib_contabil-hbkid         = p_dados-bvtyp.
      sl_zib_contabil-rg_atualizado = 'N'.
      sl_zib_contabil-bankl         = space.
      sl_zib_contabil-bankn         = space.

      APPEND sl_zib_contabil TO tl_contabil.
      CLEAR sl_zib_contabil.

    ENDIF.

  ENDIF.

  CHECK NOT tl_contabil[] IS INITIAL.

  DELETE FROM zib_contabil WHERE obj_key EQ vl_object.

  MODIFY zib_contabil FROM TABLE tl_contabil.

  IF tl_contabil_irt[] IS NOT INITIAL.
    MODIFY zib_contabil_irt FROM TABLE tl_contabil_irt.
  ENDIF.

  COMMIT WORK.

  LOOP AT tl_contabil INTO wa_contabil.
    MOVE-CORRESPONDING wa_contabil TO wa_document.
    APPEND wa_document TO it_document.
  ENDLOOP.

  CALL FUNCTION 'Z_FI_MANYDOCUMENT_PROCESSA'
    TABLES
      it_document         = it_document
      it_header           = it_header
      it_item             = it_item
      it_return           = it_return
      it_impostos_retidos = tl_contabil_irt.

  LOOP AT it_return INTO wa_return.
    MOVE-CORRESPONDING wa_return TO wa_returnl.
    APPEND wa_returnl TO tl_return.
    IF ( wa_return-type EQ 'S' AND wa_return-id = 'Z01' AND wa_return-number EQ '003' ).
      CASE p_estorno.
        WHEN space.
          p_dados-comp_belnr = wa_return-message_v1(10).
          p_dados-comp_gjahr = wa_return-message_v3(04).
        WHEN c_x.
          CLEAR: p_dados-comp_belnr, p_dados-comp_gjahr.
      ENDCASE.
      p_dados-id_envio   =  p_dados-id_envio + 1.
    ENDIF.
    IF ( wa_return-type EQ 'E' AND wa_return-id = 'Z01' AND wa_return-number EQ '002' ).
      CASE p_estorno.
        WHEN space.
          p_dados-comp_belnr = wa_return-message_v2(10).
          p_dados-comp_gjahr = wa_return-message_v3(04).
        WHEN c_x.
          CLEAR: p_dados-comp_belnr, p_dados-comp_gjahr.
      ENDCASE.
      p_dados-id_envio   =  p_dados-id_envio + 1.
    ENDIF.
  ENDLOOP.

  PERFORM popula_log TABLES tl_return.

ENDFORM.                    " Z_PREENCHE_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_ZLEST0042                                     *
*&---------------------------------------------------------------------*
*                           Preenche ZLEST0042                         *
*----------------------------------------------------------------------*
FORM z_preenche_zlest0042 TABLES p_aux STRUCTURE s_dados.

  DATA: tl_zlest0042 TYPE TABLE OF zlest0042,
        sl_zlest0042 TYPE zlest0042,
        sl_dados     TYPE zftte_dados.

  LOOP AT p_aux INTO sl_dados.

    sl_zlest0042-tknum        = sl_dados-tknum.
    sl_zlest0042-seqitem      = 1.
    sl_zlest0042-tipo         = sl_dados-tipo.
    sl_zlest0042-zdt_mov      = sl_dados-zdt_mov.
    sl_zlest0042-zdt_vencto   = sl_dados-zdt_vencto.
    sl_zlest0042-nr_conhec    = sl_dados-nr_conhec.
    sl_zlest0042-serie_conhec = sl_dados-series.
    sl_zlest0042-zdt_conhec   = sl_dados-zdt_conhec.
    sl_zlest0042-zdt_chegada  = sl_dados-zdt_chegada.
    sl_zlest0042-iva          = sl_dados-iva.
    sl_zlest0042-nfe          = sl_dados-nfe.
    sl_zlest0042-base_icms    = sl_dados-base_icms.
    sl_zlest0042-base_pis     = sl_dados-base_pis.
    sl_zlest0042-base_cofins  = sl_dados-base_cofins.
    sl_zlest0042-rate_icms    = sl_dados-rate_icms.
    sl_zlest0042-rate_pis     = sl_dados-rate_pis.
    sl_zlest0042-rate_cofins  = sl_dados-rate_cofins.
    sl_zlest0042-valor_icms   = sl_dados-valor_icms.
    sl_zlest0042-valor_pis    = sl_dados-valor_pis.
    sl_zlest0042-valor_cofins = sl_dados-valor_cofins.
    sl_zlest0042-valor_icms   = sl_dados-valor_icms.
    sl_zlest0042-valor_pis    = sl_dados-valor_pis.
    sl_zlest0042-valor_cofins = sl_dados-valor_cofins.
    sl_zlest0042-comp_valor   = sl_dados-comp_valor.
    sl_zlest0042-comp_docnum  = sl_dados-comp_docnum.
    sl_zlest0042-comp_belnr   = sl_dados-comp_belnr.
    sl_zlest0042-comp_gjahr   = sl_dados-comp_gjahr.
    sl_zlest0042-id_envio     = sl_dados-id_envio.
    sl_zlest0042-multimodal   = sl_dados-multimodal.
    sl_zlest0042-cotacao      = sl_dados-cotacao.

    APPEND sl_zlest0042 TO tl_zlest0042.

    CLEAR: sl_dados    ,
           sl_zlest0042.
  ENDLOOP.

  MODIFY zlest0042 FROM TABLE tl_zlest0042.
  COMMIT WORK.

ENDFORM.                    " Z_PREENCHE_ZLEST0042

*&---------------------------------------------------------------------*
*&      Form  Z_GERA_NOTA                                              *
*&---------------------------------------------------------------------*
*                               Gera Docnum                            *
*----------------------------------------------------------------------*
FORM z_gera_nota TABLES p_sum     STRUCTURE s_dados2
                  USING p_estorno TYPE char01
               CHANGING p_dados   TYPE zftte_dados.

  DATA: p_lifnr  TYPE  lifnr,
        p_nftype TYPE  j_1bnftype,
        p_xblnr  TYPE  xblnr1,
        p_data   TYPE  invdt,
        p_werks  TYPE  mseg-werks.

  DATA: tl_obj_partner    TYPE TABLE OF bapi_j_1bnfnad,
        tl_obj_item       TYPE TABLE OF bapi_j_1bnflin,
        tl_obj_item_add   TYPE TABLE OF bapi_j_1bnflin_add,
        tl_obj_item_tax   TYPE TABLE OF bapi_j_1bnfstx,
        tl_obj_ot_partner TYPE TABLE OF bapi_j_1bnfcpd,
        tl_return         TYPE TABLE OF bapiret2,
        sl_obj_header     TYPE bapi_j_1bnfdoc,
        sl_obj_item       TYPE bapi_j_1bnflin,
        sl_record         TYPE makt,
        sl_zlest0040      TYPE zlest0040,
        sl_mara           TYPE mara,
        sl_sum            TYPE zftte_dados2,
        sl_obj_partner    TYPE bapi_j_1bnfnad,
        sl_nfcheck        TYPE bapi_j_1bnfcheck,
        vl_text           TYPE char100,
        vl_industry       TYPE zlest0030-industry,
        vl_dstcat         TYPE zlest0030-dstcat,
        vl_cfop           TYPE zlest0030-cfop,
        vl_docnum         TYPE bapi_j_1bnfdoc-docnum,
        e_docnum          LIKE bapi_j_1bnfdoc-docnum,
        wa_setleaf        TYPE setleaf,
        wa_lfa1           TYPE lfa1,
        wa_zib_nfe_forn   TYPE zib_nfe_forn.

  DATA: p_cotacao TYPE ukurs_curr,
        i_tcurr   TYPE tcurr_curr,
        i_data    TYPE gdatu_inv.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  IF ( p_dados-zdt_mov NE sy-datum ).
    MESSAGE e000(z01) WITH 'A Data do Movimento tem'
                       'que ser igual a data do dia'.
    STOP.
  ENDIF.

  p_cotacao = 1.

  IF p_estorno IS NOT INITIAL.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
      EXPORTING
        doc_number               = p_dados-comp_docnum
        ref_type                 = space
        ref_key                  = space
      IMPORTING
        doc_number               = e_docnum
      EXCEPTIONS
        document_not_found       = 1
        cancel_not_possible      = 2
        nf_cancel_type_not_found = 3
        database_problem         = 4
        docum_lock               = 5
        nfe_cancel_simulation    = 6
        OTHERS                   = 7.

    IF sy-subrc <> 0.
      CLEAR wa_eventos.
      wa_eventos-icone    = icon_led_red.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO wa_eventos-msg_text.
      APPEND wa_eventos TO ti_eventos.
    ELSEIF NOT e_docnum IS INITIAL.
      CLEAR wa_eventos.
      wa_eventos-icone    = icon_led_green.
      CONCATENATE '(Fiscal) Documento' p_dados-comp_docnum 'estornado por' e_docnum '!' INTO wa_eventos-msg_text SEPARATED BY space.
      APPEND wa_eventos TO ti_eventos.
      CLEAR: p_dados-comp_docnum.
    ELSE.
      CLEAR wa_eventos.
      wa_eventos-icone    = icon_led_red.
      CONCATENATE '(Fiscal) Documento' p_dados-comp_docnum 'não estornado!' INTO wa_eventos-msg_text SEPARATED BY space.
      APPEND wa_eventos TO ti_eventos.
    ENDIF.

  ENDIF.

  CHECK p_estorno IS INITIAL.

  REFRESH: tl_obj_partner   ,
           tl_obj_item      ,
           tl_obj_item_add  ,
           tl_obj_item_tax  ,
           tl_obj_ot_partner,
           tl_return        .

  CLEAR: sl_obj_header ,
         sl_obj_item   ,
         sl_record     ,
         sl_zlest0040  ,
         sl_mara       ,
         sl_sum        ,
         sl_obj_partner,
         sl_nfcheck    .

  READ TABLE p_sum INTO sl_sum
    WITH KEY tdlnr = p_dados-tdlnr
    BINARY SEARCH.

  IF p_dados-nfe IS INITIAL.
    IF p_dados-multimodal IS INITIAL.
      sl_obj_header-nftype  = 'C1'.
      sl_obj_header-model   = '08'.
    ELSE.
      sl_obj_header-nftype  = 'C6'.
      sl_obj_header-model   = '26'.
    ENDIF.
    sl_obj_header-nfnum   = p_dados-nr_conhec.
  ELSE.
    sl_obj_header-nftype  = 'C2'.
    sl_obj_header-model   = '57'.
    sl_obj_header-nfe     = 'X'.
    sl_obj_header-xmlvers = ( 110 / 100 ).
    sl_obj_header-code    = '100'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_dados-nr_conhec
      IMPORTING
        output = sl_obj_header-nfenum.
  ENDIF.

  CONCATENATE p_dados-nr_conhec '-' p_dados-series INTO p_xblnr.

  p_lifnr  = p_dados-tdlnr.
  p_nftype = sl_obj_header-nftype.
  p_data   = p_dados-zdt_conhec.
  p_werks  = p_dados-werks.

  SELECT SINGLE *
   FROM setleaf
   INTO wa_setleaf
 WHERE setname EQ 'MAGGI_EMPRESA_EXTERIOR'
    AND valfrom EQ p_dados-bukrs.


  IF ( sy-subrc NE 0 ).
    CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
      EXPORTING
        p_lifnr  = p_lifnr
        p_nftype = p_nftype
        p_xblnr  = p_xblnr
        p_data   = p_data
        p_werks  = p_werks
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.


  SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ p_dados-lifnr.

  IF ( sy-subrc EQ 0 ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_dados-nr_conhec
      IMPORTING
        output = p_dados-nr_conhec.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_dados-series
      IMPORTING
        output = p_dados-series.


    SELECT SINGLE * FROM zib_nfe_forn INTO wa_zib_nfe_forn
      WHERE nu_chave_cnpj   EQ wa_lfa1-stcd1
        AND nu_chave_numero EQ p_dados-nr_conhec
        AND nu_chave_serie  EQ p_dados-series.

  ENDIF.

  sl_obj_header-access_key = wa_zib_nfe_forn-nu_chave.

  sl_obj_header-doctyp  = '4'.
  sl_obj_header-direct  = '1'.
  sl_obj_header-docstat = '1'.

  sl_obj_header-parvw   = 'LF'.
  sl_obj_header-partyp  = 'V'.
  sl_obj_header-parid   = p_dados-tdlnr.

  sl_obj_header-docdat  = p_dados-zdt_conhec.
  sl_obj_header-pstdat  = p_dados-zdt_mov.
  sl_obj_header-bukrs   = p_dados-bukrs.
  sl_obj_header-branch  = p_dados-werks.
  sl_obj_header-waerk   = 'BRL'.
  sl_obj_header-series  = p_dados-series.

*   Item da parte fiscal
  sl_obj_item-itmnum    = 10.
  sl_obj_item-bwkey     = p_dados-werks.
  sl_obj_item-werks     = p_dados-werks.
  sl_obj_item-refkey    = p_dados-re_belnr.

* Logística: Revisão de Faturas
  sl_obj_item-reftyp    = 'LI'.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
  sl_obj_item-matnr     = p_dados-matns.
  DATA(v_len) = strlen( p_dados-matns ).
  IF v_len > 18.
    sl_obj_item-matnr_long = p_dados-matns.
  ELSE.
    sl_obj_item-matnr      = p_dados-matns.
  ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
  sl_obj_item-itmtyp    = 'ZH'.
  sl_obj_item-menge     = 1.
  IF p_dados-waers EQ 'BRL'.
    sl_obj_item-netpr     = sl_sum-comp_valor.
*---> 13/06/2023 - Migração S4 - JS
*      SL_OBJ_ITEM-NETWR     = SL_SUM-COMP_VALOR.
    sl_obj_item-netwr = CONV #( sl_sum-comp_valor ).
*<--- 13/06/2023 - Migração S4 - JS

    p_cotacao  = 1.
  ELSE.
    IF p_dados-cotacao IS NOT INITIAL.
      p_cotacao = p_dados-cotacao.
    ELSE.
      CREATE OBJECT obj_zcl_util_sd.
      IF p_dados-zdt_conhec IS NOT INITIAL.
        i_data = p_dados-zdt_conhec.
      ELSE.
        i_data = sy-datum.
      ENDIF.
      i_tcurr = 'BRL'.
      obj_zcl_util_sd->set_data(  EXPORTING i_data    = i_data ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst   = p_dados-kurst ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk   = p_dados-waers ).
      obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr   = i_tcurr ).
      obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = p_cotacao ).
      CLEAR: obj_zcl_util_sd.
    ENDIF.
    sl_obj_item-netpr     = sl_sum-comp_valor * p_cotacao.
    sl_obj_item-netwr     = sl_sum-comp_valor * p_cotacao.
  ENDIF.

  CALL FUNCTION 'J_1B_MATERIAL_READ'
    EXPORTING
      matnr                = p_dados-matns
      val_area             = p_dados-werks
      val_type             = space
      language             = sy-langu
      i_werks              = p_dados-werks
    IMPORTING
      nbm                  = sl_obj_item-nbm
      matuse               = sl_obj_item-matuse
      matorg               = sl_obj_item-matorg
      material_text_record = sl_record
      e_matkl              = sl_obj_item-matkl
    EXCEPTIONS
      material_not_found   = 1
      valuation_not_found  = 2
      OTHERS               = 3.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty
      NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  sl_obj_item-maktx = sl_record-maktx.

  SELECT SINGLE *
    FROM zlest0040
    INTO sl_zlest0040
  WHERE iva EQ p_dados-iva.

  IF NOT sy-subrc IS INITIAL.
    CONCATENATE TEXT-008
                p_dados-iva
           INTO vl_text.
    MESSAGE i897 WITH vl_text
                      TEXT-009.
    EXIT.
  ENDIF.

  sl_obj_item-taxlw1 = sl_zlest0040-icms.
  sl_obj_item-taxlw2 = sl_zlest0040-ipi.
  sl_obj_item-taxlw4 = sl_zlest0040-cofins.
  sl_obj_item-taxlw5 = sl_zlest0040-pis.

  SELECT SINGLE *
    FROM mara
    INTO sl_mara
  WHERE matnr EQ p_dados-matns.

  sl_obj_item-matkl = sl_mara-matkl.
  sl_obj_item-meins = sl_mara-meins.

  SELECT SINGLE industry
    FROM j_1bbranch
    INTO vl_industry
  WHERE bukrs  EQ p_dados-bukrs
    AND branch EQ p_dados-werks.

  PERFORM z_busca_cat_destino USING p_dados-tdlnr
                                    p_dados-werks
                           CHANGING vl_dstcat.

  SELECT SINGLE cfop
    FROM zlest0030
    INTO vl_cfop
   WHERE direct     EQ c_1
     AND dstcat     EQ vl_dstcat
     AND industry   EQ vl_industry
     AND tpparceiro EQ c_1
     AND tdlnr      EQ p_dados-tdlnr
     AND bukrs      EQ p_dados-bukrs.

  IF sy-subrc IS NOT INITIAL.

    SELECT SINGLE cfop
      FROM zlest0030
      INTO vl_cfop
     WHERE direct     EQ c_1
       AND dstcat     EQ vl_dstcat
       AND industry   EQ vl_industry
       AND tpparceiro EQ c_1
       AND tdlnr      EQ p_dados-tdlnr
       AND bukrs      EQ space.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE cfop
        FROM zlest0030
        INTO vl_cfop
       WHERE direct     EQ c_1
         AND dstcat     EQ vl_dstcat
         AND industry   EQ vl_industry
         AND tpparceiro EQ c_1
         AND tdlnr      EQ space
         AND bukrs      EQ p_dados-bukrs.

      IF sy-subrc IS NOT INITIAL.
        SELECT SINGLE cfop
          FROM zlest0030
          INTO vl_cfop
         WHERE direct     EQ c_1
           AND dstcat     EQ vl_dstcat
           AND industry   EQ vl_industry
           AND tpparceiro EQ c_1
           AND tdlnr      EQ space
           AND bukrs      EQ space.
      ENDIF.
    ENDIF.
  ENDIF.

  sl_obj_item-cfop_10 = vl_cfop.

* Alimentar a estrutura do parceiros da nota (somente o parceiro LF é necessário)
  sl_obj_partner-mandt  = sy-mandt.
  sl_obj_partner-parvw  = 'LF'.
  sl_obj_partner-parid  = p_dados-tdlnr.
  sl_obj_partner-partyp = 'V'.
  APPEND sl_obj_partner TO tl_obj_partner.

* Impostos
  PERFORM z_impostos TABLES tl_obj_item_tax USING sl_obj_item p_dados sl_sum p_cotacao.

  sl_obj_item-netpr     = sl_obj_item-netpr - sl_sum-valor_icms - sl_sum-valor_cofins - sl_sum-valor_pis.
  sl_obj_item-netwr     = sl_obj_item-netwr - sl_sum-valor_icms - sl_sum-valor_cofins - sl_sum-valor_pis.

  APPEND sl_obj_item TO tl_obj_item.

  sl_nfcheck-chekcon = 'X'.

  CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      obj_header     = sl_obj_header
      nfcheck        = sl_nfcheck
    IMPORTING
      e_docnum       = vl_docnum
    TABLES
      obj_partner    = tl_obj_partner
      obj_item       = tl_obj_item
      obj_item_tax   = tl_obj_item_tax
      obj_ot_partner = tl_obj_ot_partner
      return         = tl_return.

  IF vl_docnum IS INITIAL.
*   Retorna Erros da BAPI
    PERFORM z_ret_erros TABLES tl_return.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    p_dados-comp_docnum = vl_docnum.
  ENDIF.

  PERFORM popula_log TABLES tl_return.



ENDFORM.                    " Z_GERA_NOTA

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_CAT_DESTINO                                      *
*&---------------------------------------------------------------------*
*             Busca categoria de CFOP baseado nas UFs                  *
*----------------------------------------------------------------------*
FORM z_busca_cat_destino USING p_lifnr  TYPE lifnr
                               p_werks  TYPE werks_d
                      CHANGING p_dstcat TYPE j_1bdstcat.

  DATA: vl_txjcdwk TYPE txjcd,
        vl_txjcdlf TYPE txjcd.

  PERFORM z_ufs USING p_werks
                      p_lifnr
             CHANGING vl_txjcdwk
                      vl_txjcdlf.

  IF vl_txjcdwk(2) EQ vl_txjcdlf(2).
    p_dstcat = '0'.
  ELSE.
    p_dstcat = '1'.
  ENDIF.

ENDFORM.                    " Z_BUSCA_CAT_DESTINO

*&---------------------------------------------------------------------*
*&      Form  Z_UFS                                                    *
*&---------------------------------------------------------------------*
*                Seleciona UF de Cliente e Fornecedor                  *
*----------------------------------------------------------------------*
FORM z_ufs USING p_werks TYPE werks_d
                 p_lifnr TYPE lifnr
        CHANGING p_wk    TYPE txjcd
                 p_lf    TYPE txjcd.

  SELECT SINGLE txjcd
    FROM t001w
    INTO p_wk
  WHERE werks EQ p_werks.

  SELECT SINGLE txjcd
    FROM lfa1
    INTO p_lf
  WHERE lifnr EQ p_lifnr.

ENDFORM.                    " UFS

*&---------------------------------------------------------------------*
*&      Form  Z_IMPOSTOS                                               *
*&---------------------------------------------------------------------*
*                                 Impostos                             *
*----------------------------------------------------------------------*
FORM z_impostos TABLES p_tax     STRUCTURE bapi_j_1bnfstx
                 USING p_item    TYPE bapi_j_1bnflin
                       p_dados   TYPE zftte_dados
                       p_sum     TYPE zftte_dados2
                       p_cotacao TYPE ukurs_curr.

  DATA: sl_1bnflin TYPE bapi_j_1bnflin,
        sl_1bnfstx TYPE bapi_j_1bnfstx.

*  ICMS
  IF p_item-taxlw1 NE space.

    IF p_sum-valor_icms GT 0.
      sl_1bnfstx-itmnum = 10.
      sl_1bnfstx-base   = p_sum-comp_valor * p_cotacao.
      sl_1bnfstx-rate   = p_sum-rate_icms.
      sl_1bnfstx-taxval = p_sum-valor_icms.
      sl_1bnfstx-excbas = 0.
      sl_1bnfstx-othbas = 0.
    ELSE.
      sl_1bnfstx-itmnum = 10.
      sl_1bnfstx-base   = 0.
      sl_1bnfstx-rate   = 0.
      sl_1bnfstx-taxval = 0.
      sl_1bnfstx-excbas = p_sum-comp_valor * p_cotacao.
      sl_1bnfstx-othbas = 0.
    ENDIF.

    PERFORM z_tipo_imposto USING 'ICMS'
                        CHANGING sl_1bnfstx-taxtyp.

    APPEND sl_1bnfstx TO p_tax.
    CLEAR sl_1bnfstx.

  ENDIF.

* IPI
  IF p_item-taxlw2 NE space.

    sl_1bnfstx-itmnum = 10.
    sl_1bnfstx-base   = 0.
    sl_1bnfstx-rate   = 0.
    sl_1bnfstx-taxval = 0.
    sl_1bnfstx-excbas = 0.
    sl_1bnfstx-othbas = p_sum-comp_valor * p_cotacao.

    PERFORM z_tipo_imposto USING 'IPI'
                        CHANGING sl_1bnfstx-taxtyp.

    APPEND sl_1bnfstx TO p_tax.
    CLEAR sl_1bnfstx.

  ENDIF.

* Cofins
  IF p_item-taxlw4 NE space.

    IF p_sum-valor_cofins GT 0.
      sl_1bnfstx-itmnum = 10.
      sl_1bnfstx-base   = p_sum-comp_valor * p_cotacao.
      sl_1bnfstx-rate   = p_sum-rate_cofins.
      sl_1bnfstx-taxval = p_sum-valor_cofins.
      sl_1bnfstx-excbas = 0.
      sl_1bnfstx-othbas = 0.
    ELSE.
      sl_1bnfstx-itmnum = 10.
      sl_1bnfstx-base   = 0.
      sl_1bnfstx-rate   = 0.
      sl_1bnfstx-taxval = 0.
      sl_1bnfstx-excbas = p_sum-comp_valor * p_cotacao.
      sl_1bnfstx-othbas = 0.
    ENDIF.

    PERFORM z_tipo_imposto USING 'COFI'
                        CHANGING sl_1bnfstx-taxtyp.

    APPEND sl_1bnfstx TO p_tax.
    CLEAR sl_1bnfstx.

  ENDIF.

* PIS
  IF p_item-taxlw5 NE space.

    IF p_sum-valor_pis GT 0.
      sl_1bnfstx-itmnum = 10.
      sl_1bnfstx-base   = p_sum-comp_valor * p_cotacao.
      sl_1bnfstx-rate   = p_sum-rate_pis.
      sl_1bnfstx-taxval = p_sum-valor_pis.
      sl_1bnfstx-excbas = 0.
      sl_1bnfstx-othbas = 0.
    ELSE.
      sl_1bnfstx-itmnum = 10.
      sl_1bnfstx-base   = 0.
      sl_1bnfstx-rate   = 0.
      sl_1bnfstx-taxval = 0.
      sl_1bnfstx-excbas = p_sum-comp_valor * p_cotacao.
      sl_1bnfstx-othbas = 0.
    ENDIF.

    PERFORM z_tipo_imposto USING 'PIS'
                        CHANGING sl_1bnfstx-taxtyp.

    APPEND sl_1bnfstx TO p_tax.

  ENDIF.

ENDFORM.                    " Z_IMPOSTOS

*&---------------------------------------------------------------------*
*&      Form  Z_TIPO_IMPOSTO                                           *
*&---------------------------------------------------------------------*
*                               Tipo Imposto                           *
*----------------------------------------------------------------------*
FORM z_tipo_imposto USING p_imposto TYPE c
                 CHANGING p_taxtyp  TYPE bapi_j_1bnfstx-taxtyp.

  DATA: tl_j_1baj TYPE TABLE OF j_1baj,
        sl_j_1baj TYPE j_1baj,
        tl_t683s  TYPE TABLE OF t683s,
        sl_t683s  TYPE t683s.

  SELECT *
    FROM j_1baj
    INTO TABLE tl_j_1baj
  WHERE taxgrp EQ p_imposto.

  SELECT *
    FROM t683s
    INTO TABLE tl_t683s
    FOR ALL ENTRIES IN tl_j_1baj
  WHERE kvewe EQ 'A'
    AND kappl EQ 'TX'
    AND kalsm EQ 'TAXBRA'
    AND kschl EQ tl_j_1baj-taxtyp
    AND kstat EQ 'X'.

  READ TABLE tl_t683s INDEX 1 INTO sl_t683s.
  p_taxtyp = sl_t683s-kschl.

ENDFORM.                    " Z_TIPO_IMPOSTO

*&---------------------------------------------------------------------*
*&      Form  Z_RET_ERROS                                              *
*&---------------------------------------------------------------------*
*                          Retorna Erros da BAPI                       *
*----------------------------------------------------------------------*
FORM z_ret_erros TABLES t_return STRUCTURE bapiret2.

  DATA: sl_return TYPE bapiret2,
        sl_erro   TYPE type_erro.

  REFRESH ti_erro.

  LOOP AT t_return INTO sl_return.

    sl_erro-tipo   = sl_return-type.
    sl_erro-numero = sl_return-number.
    sl_erro-msg    = sl_return-message.

    APPEND sl_erro TO ti_erro.

    CLEAR: sl_return,
           sl_erro  .

  ENDLOOP.

  CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
    TABLES
      table    = ti_erro
    EXCEPTIONS
      fb_error = 1
      OTHERS   = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_RET_ERROS

*&---------------------------------------------------------------------*
*&      Form  POPULA_LOG
*&---------------------------------------------------------------------*
*       Popula tabela de log de processamento
*----------------------------------------------------------------------*
FORM popula_log  TABLES   p_it_return STRUCTURE bapiret2.

  DATA: wa_return TYPE bapiret2,
        wa_numero TYPE t100-msgnr.

  LOOP AT p_it_return INTO wa_return.

    CLEAR wa_eventos.

    CASE wa_return-type.
      WHEN c_s.
        wa_eventos-icone    = icon_led_green.
      WHEN c_w.
        wa_eventos-icone    = icon_led_yellow.
      WHEN c_e.
        wa_eventos-icone    = icon_led_red.
    ENDCASE.

    wa_eventos-type     = wa_return-type.
    wa_eventos-msg_id   = wa_return-id.
    wa_eventos-msg_no   = wa_return-number.
    wa_eventos-msg_var1 = wa_return-message_v1.
    wa_eventos-msg_var2 = wa_return-message_v2.
    wa_eventos-msg_var3 = wa_return-message_v3.
    wa_eventos-msg_var4 = wa_return-message_v4.

    wa_numero = wa_return-number.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = sy-langu
        msg_id                 = wa_return-id
        msg_no                 = wa_numero
        msg_var1               = wa_return-message_v1
        msg_var2               = wa_return-message_v2
        msg_var3               = wa_return-message_v3
        msg_var4               = wa_return-message_v4
      IMPORTING
        msg_text               = wa_eventos-msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    IF sy-subrc EQ 0.
      APPEND wa_eventos TO ti_eventos.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " POPULA_LOG

*----------------------------------------------------------------------*
*       CLASS lcl_event_eventos DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_eventos DEFINITION.

  PUBLIC SECTION.
    METHODS: zm_handle_hotspot_eventos FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no                      ,

      zm_handle_toolbar_eventos FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive                   ,

      zm_handle_user_command_eventos FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_eventos IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_eventos IMPLEMENTATION.

  METHOD: zm_handle_hotspot_eventos.
    PERFORM z_handle_hotspot_eventos USING e_row_id
                                         e_column_id
                                         es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar_eventos.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar_eventos USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command_eventos.
*   User Command Botões Incluidos
    PERFORM z_handle_command_eventos USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_hotspot_eventos USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

*  READ TABLE it_itemcap INDEX p_e_row_id INTO wa_itemdata.
*
*  CASE p_e_column_id.
*    WHEN 'INVOICE_DOC_ITEM'.
*      PERFORM visualizar_eventos_contas USING wa_itemdata-invoice_doc_item.
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_toolbar_eventos  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                      p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
*  MOVE c_separator  TO sl_toolbar-butn_type.
*  APPEND sl_toolbar TO p_object->mt_toolbar.

  DELETE p_object->mt_toolbar WHERE function = '&INFO'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP07'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP00'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP01'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP02'.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command_eventos  USING p_ucomm TYPE syucomm.

*  CASE p_ucomm.
*    WHEN 'REMESSA'.
**     Gera Remessa
*      CALL METHOD wa_alv->refresh_table_display .
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_eventos  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c
                               p_saida TYPE lvc_outlen.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'TI_EVENTOS'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_saida.
  APPEND wl_fcat TO it_fcat_eventos.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9999  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9999 OUTPUT.

  SET PF-STATUS 'PFEVENTOS'.
  SET TITLEBAR 'TLEVENTOS'.

  wa_layout_eventos-zebra = c_x.

  IF wa_cont_eventos IS INITIAL.

    PERFORM alv_preenche_cat_eventos USING:
          'ICONE'      TEXT-l01   '004'  space  space 3,
          'MSG_TEXT'   TEXT-l02   '300'  space  space 100,
          'MSG_ID'     TEXT-l03   '020'  space  space 5,
          'MSG_NO'     TEXT-l04   '003'  space  space 5,
          'MSG_VAR1'   TEXT-l05   '050'  space  space 50,
          'MSG_VAR2'   TEXT-l06   '050'  space  space 50,
          'MSG_VAR3'   TEXT-l07   '050'  space  space 50,
          'MSG_VAR4'   TEXT-l08   '050'  space  space 50.

    CREATE OBJECT wa_cont_eventos
      EXPORTING
        container_name              = 'CC_ALV_EVENTOS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.

  IF ( wa_alv_eventos IS INITIAL ) AND ( NOT wa_cont_eventos IS INITIAL ).
    CREATE OBJECT wa_alv_eventos
      EXPORTING
        i_parent          = wa_cont_eventos
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event_eventos IS INITIAL.
    CREATE OBJECT wa_event_eventos.
    SET HANDLER: wa_event_eventos->zm_handle_hotspot_eventos FOR wa_alv_eventos.
    SET HANDLER: wa_event_eventos->zm_handle_toolbar_eventos FOR wa_alv_eventos.
    SET HANDLER: wa_event_eventos->zm_handle_user_command_eventos FOR wa_alv_eventos.
  ENDIF.

  CALL METHOD wa_alv_eventos->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_eventos
    CHANGING
      it_outtab                     = ti_eventos
      it_fieldcatalog               = it_fcat_eventos
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv_eventos IS INITIAL.

ENDMODULE.                 " STATUS_9999  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9999  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9999 INPUT.

  CASE ok_code.
    WHEN c_back OR c_exit OR c_cancel.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9999  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_MOSTRA_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_mostra_eventos .

  IF ti_eventos IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-026.
    CHECK ti_eventos IS INITIAL.
  ENDIF.

  CALL SCREEN 9999.

ENDFORM.                    " Z_MOSTRA_EVENTOS


*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0021                                    *
*&---------------------------------------------------------------------*
*                          Seleciona ZLEST0021                         *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0021 USING wa_zlest0043 TYPE zlest0043
                                 p_shtyp      TYPE shtyp
                                 p_referencia TYPE sy-datum.

  DATA tl_dados TYPE TABLE OF zftte_dados.

  REFRESH ti_zlest0021.

  tl_dados[] = ti_dados[].
  SORT tl_dados BY shtyp ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_dados COMPARING shtyp.
  DELETE tl_dados WHERE shtyp IS INITIAL.

  CHECK NOT tl_dados[] IS INITIAL.

  DATA: rgveicu TYPE RANGE OF zde_tp_prop_veiculo_ctb.
  rgveicu = VALUE #( sign = 'I' option = 'EQ' ( low = space high = space ) ( low = '0' high = '0' ) ).

  zcl_controle_conta_razao=>get_instance(
    )->get_conta_razao(
        EXPORTING
          i_shtyp                  = p_shtyp    " Tipo de transporte
          i_tcode                  = CONV #( c_f02 )    " Código de transação
          i_fatura                 = c_t    " Emissor da fatura - Fretes
          i_tp_emissor             = c_t    " Tipo de emissor
          i_tp_veiculo             = rgveicu    " Tipo de Proprietário de Veículo para Contabilização
          i_dt_referencia          = p_referencia    " Data de lançamento no documento
        IMPORTING
           e_it_zlest0021           = ti_zlest0021    " Controle de desterminação conta razão
    ).

*  SELECT *
*    FROM ZLEST0021
*    INTO TABLE TI_ZLEST0021
*    FOR ALL ENTRIES IN TL_DADOS
*   WHERE SHTYP      EQ TL_DADOS-SHTYP
*     AND TCODE      EQ C_F02
*     AND FATURA     EQ C_T
*     AND TP_EMISSOR EQ C_T
*     AND TP_VEICULO IN RGVEICU.

  SORT ti_zlest0021 BY shtyp operfrete ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZLEST0021

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SL_SAIDA  text
*----------------------------------------------------------------------*
FORM atualiza_status  CHANGING p_sl_saida TYPE zftte_dados.

  IF NOT p_sl_saida-comp_belnr IS INITIAL.
    p_sl_saida-status = icon_green_light.
  ENDIF.

  IF p_sl_saida-comp_belnr IS INITIAL AND NOT
     p_sl_saida-comp_valor IS INITIAL.
    p_sl_saida-status = icon_yellow_light.
  ENDIF.

  IF p_sl_saida-comp_belnr IS INITIAL AND
     p_sl_saida-comp_valor IS INITIAL.
    p_sl_saida-status = icon_red_light.
  ENDIF.

ENDFORM.                    " ATUALIZA_STATUS


*&---------------------------------------------------------------------*
*&      Module  HLP_MWSKZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hlp_mwskz INPUT.

  TYPES:
    BEGIN OF ty_help,
      mwskz TYPE mwskz,
      text1 TYPE text1_007s,
    END OF ty_help.

  DATA: ti_help  TYPE STANDARD TABLE OF ty_help INITIAL SIZE 0 WITH HEADER LINE,
        ti_t007a TYPE TABLE OF t007a INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: t_dynpfields[], t_ret.

  SELECT * INTO TABLE ti_t007a
    FROM t007a
   WHERE kalsm EQ 'TAXBRA'.

  CHECK NOT ti_t007a[] IS INITIAL.

  SELECT mwskz text1
    INTO TABLE ti_help
    FROM t007s
    FOR ALL ENTRIES IN ti_t007a
   WHERE mwskz EQ ti_t007a-mwskz
     AND kalsm EQ ti_t007a-kalsm
     AND spras EQ sy-langu.

  CHECK NOT ti_help[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'MWSKZ'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help WITH KEY mwskz = st_ret-fieldval BINARY SEARCH.

  MOVE: 'S_TELA-IVA'    TO t_dynpfields-fieldname,
        st_ret-fieldval TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'S_TELA-TEXT1'  TO  t_dynpfields-fieldname,
        ti_help-text1   TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " HLP_MWSKZ  INPUT

*&---------------------------------------------------------------------*
*&      Module  HLP_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hlp_tipo INPUT.

  DATA: ti_zlest0043 TYPE TABLE OF zlest0043 INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: t_dynpfields[], t_ret.

  SELECT * INTO TABLE ti_zlest0043
    FROM zlest0043.

  CHECK NOT ti_zlest0043[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'CD_TIPO'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_zlest0043[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_zlest0043 WITH KEY cd_tipo = st_ret-fieldval BINARY SEARCH.

  MOVE: 'S_TELA-COMP_TIPO' TO t_dynpfields-fieldname,
        st_ret-fieldval    TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " HLP_TIPO  INPUT


*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_MULTIMODAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verificar_multimodal USING vg_validado TYPE sy-subrc.

  vg_validado = 0.

  IF ( NOT s_tela-nfe IS INITIAL ) AND ( NOT s_tela-multimodal IS INITIAL ).
    MESSAGE s836 WITH 'Para documento multimodal' 'não deve ser marcado doc. Eletrônico!'.
    vg_validado = 1.
  ENDIF.

ENDFORM.                    " VERIFICAR_MULTIMODAL

*&---------------------------------------------------------------------*
*&      Form  Z_IMPOSTOS_RETIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_impostos_retidos TABLES it_retidos STRUCTURE zles0043_imp_retidos
                         USING vg_frete     TYPE zles0043_imp_retidos
                               vg_visualiza TYPE c.

  CALL FUNCTION 'Z_LES_INFORMA_IMP_RETIDOS'
    EXPORTING
      p_bukrs                 = vg_frete-bukrs
      p_lifnr                 = vg_frete-lifnr
      p_visualiza             = vg_visualiza
      v_base_sugerido         = vg_frete-base
    TABLES
      imp_retidos             = it_retidos
    EXCEPTIONS
      sem_impostos_retidos    = 1
      sem_impostos_retidos_br = 2
      OTHERS                  = 3.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_IMPOSTOS_RETIDOS
