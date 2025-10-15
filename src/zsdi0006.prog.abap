*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0001                                                *
* Descrição  : Controle DCO - Remessa para Formação de Lote            *
* Módulo     : SD                                Transação: ZSDT0010   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 16/08/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0006 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TYPE-POOLS icon.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_lips,
         vbeln TYPE lips-vbeln,
         posnr TYPE lips-posnr,
         matnr TYPE lips-matnr,
         lfimg TYPE lips-lfimg,
       END   OF type_lips,

       BEGIN OF type_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         vbtyp_n TYPE vbfa-vbtyp_n,
       END   OF type_vbfa,

       BEGIN OF type_vbak,
         vbeln TYPE vbak-vbeln,
         auart TYPE vbak-auart,
       END   OF type_vbak,

       BEGIN OF type_alv,
         nr_dco   TYPE zdco_produtor-nr_dco,
         nu_dco	  TYPE zdco_produtor-nu_dco	,
         dt_lanc  TYPE zdco_produtor-dt_lancamento,
         qdt_dco  TYPE zdco_produtor-qt_material,
         produtor TYPE zdco_produtor-id_fornecedor,
         material TYPE zdco_produtor-cd_material,
         desc     TYPE makt-maktx,
         qtd_ult  TYPE zdco_produtor-qt_remessa,
         vbeln    TYPE zdco_produtor-vbeln,
         saldo    TYPE zdco_produtor-qt_remessa,
       END   OF type_alv,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF type_makt.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_lips     TYPE TABLE OF type_lips,
      t_vbfa     TYPE TABLE OF type_vbfa,
      t_vbak     TYPE TABLE OF type_vbak,
      t_produtor TYPE TABLE OF zdco_produtor,
      t_vinculo  TYPE TABLE OF zdco_vinculo,
      t_alv      TYPE TABLE OF type_alv,
      t_fcat     TYPE TABLE OF lvc_s_fcat,
      t_tool     TYPE ui_functions,
      t_makt     TYPE TABLE OF type_makt.

DATA: s_cont   TYPE REF TO cl_gui_custom_container,
      s_alv    TYPE REF TO cl_gui_alv_grid,
      s_layout TYPE lvc_s_layo.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01,
      v_nr_romaneio             TYPE zsdt0001-ch_referencia.
*-#133089-12.02.2024-JT-fim

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS c_table TYPE char5 VALUE 'T_ALV'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
    PARAMETERS
      p_vbeln TYPE likp-vbeln OBLIGATORY.
  SELECTION-SCREEN END   OF BLOCK a2.
  SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME.
    PARAMETERS:
      p_vinc TYPE char1 RADIOBUTTON GROUP a1 DEFAULT 'X' USER-COMMAND radio,
      p_desc TYPE char1 RADIOBUTTON GROUP a1.
  SELECTION-SCREEN END   OF BLOCK a3.
SELECTION-SCREEN END   OF BLOCK a1.

*-#133089-21.02.2024-JT-inicio
PARAMETERS: p_fataut TYPE char01 NO-DISPLAY.
*-#133089-21.02.2024-JT-fim

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_vbeln.
* Válida Remessa
  PERFORM z_valida_rem.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*-#133089-21.02.2024-JT-inicio
*-----------------------------------------------
*-verifica se é faturamento automatico
*-----------------------------------------------
  IF p_fataut = abap_true.
    vg_faturamento_autom = p_fataut.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD v_nr_romaneio.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

* Seleção Dados
  PERFORM: z_seleciona_dados,

* Monta Relatório ALV
           z_monta_relatorio.


  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT
    IF NOT t_alv[] IS INITIAL.
      CALL SCREEN 0100.
    ELSE.
      MESSAGE i836 WITH TEXT-022.
    ENDIF.
  ELSEIF p_vinc EQ 'X'.
    PERFORM z_vincular_dco.
  ELSEIF p_desc EQ 'X'.
    PERFORM z_desvincular_dco.
  ENDIF.

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
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_REM                                             *
*&---------------------------------------------------------------------*
*                             Válida Remessa                           *
*----------------------------------------------------------------------*
FORM z_valida_rem.

  CHECK NOT sy-ucomm EQ 'RADIO'.

  SELECT SINGLE vbeln
    FROM likp
    INTO p_vbeln
  WHERE vbeln EQ p_vbeln.

  CHECK NOT sy-subrc IS INITIAL.

  CHECK p_fataut = abap_false.  "*-#133089-21.02.2024-JT

  MESSAGE e836 WITH TEXT-002.

ENDFORM.                    " Z_VALIDA_REM

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleção Dados                             *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona ZDCO_VINCULO
  PERFORM z_seleciona_vincu.

  IF NOT p_vinc IS INITIAL.
* Seleciona LIPS
    PERFORM: z_seleciona_lips,

* Seleciona VBFA
             z_seleciona_vbfa,

* Seleciona VBAK
             z_seleciona_vbak,

* Seleciona ZDCO_PRODUTOR
             z_seleciona_prod,

* Seleciona MAKT
             z_seleciona_makt.
  ELSE.
*   Seleciona MAKT
    PERFORM z_seleciona_makt_2.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                            Seleciona LIPS                            *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  SELECT vbeln posnr matnr lfimg
    FROM lips
    INTO TABLE t_lips
  WHERE  vbeln EQ p_vbeln.

ENDFORM.                    " Z_SELECIONA_LIPS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBFA                                         *
*&---------------------------------------------------------------------*
*                             Seleciona VBFA                           *
*----------------------------------------------------------------------*
FORM z_seleciona_vbfa.

  SELECT vbelv posnv vbeln
         posnn vbtyp_n
    FROM vbfa
    INTO TABLE t_vbfa
  WHERE  vbeln EQ p_vbeln.

ENDFORM.                    " Z_SELECIONA_VBFA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAK                                         *
*&---------------------------------------------------------------------*
*                          Seleciona VBAK                              *
*----------------------------------------------------------------------*
FORM z_seleciona_vbak.

  CHECK NOT t_vbfa[] IS INITIAL.

  SELECT vbeln auart
    FROM vbak
    INTO TABLE t_vbak
    FOR ALL ENTRIES IN t_vbfa
  WHERE  vbeln EQ t_vbfa-vbelv.

  DELETE t_vbak WHERE auart NE 'ZRDC'.

ENDFORM.                    " Z_SELECIONA_VBAK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_PROD                                         *
*&---------------------------------------------------------------------*
*                        Seleciona ZDCO_PRODUTOR                       *
*----------------------------------------------------------------------*
FORM z_seleciona_prod.

  CHECK NOT t_vbak[] IS INITIAL.

  SELECT *
    FROM zdco_produtor
    INTO TABLE t_produtor
    FOR ALL ENTRIES IN t_vbak
  WHERE  vbeln EQ t_vbak-vbeln.

  SORT t_produtor BY nu_dco ASCENDING.

ENDFORM.                    " Z_SELECIONA_PROD

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VINCU                                        *
*&---------------------------------------------------------------------*
*                       Seleciona ZDCO_VINCULO                         *
*----------------------------------------------------------------------*
FORM z_seleciona_vincu.

  SELECT *
    FROM zdco_vinculo
    INTO TABLE t_vinculo
  WHERE  vbeln EQ p_vbeln.

  CHECK sy-subrc IS INITIAL AND
        p_desc   IS INITIAL.

  MESSAGE e836 WITH TEXT-003.

ENDFORM.                    " Z_SELECIONA_VINCU

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_RELATORIO                                        *
*&---------------------------------------------------------------------*
*                           Monta Relatório ALV                        *
*----------------------------------------------------------------------*
FORM z_monta_relatorio.

  DATA: sl_produtor TYPE zdco_produtor,
        sl_vinculo  TYPE zdco_vinculo,
        sl_alv      TYPE type_alv,
        sl_makt     TYPE type_makt.

  REFRESH t_alv.

  IF NOT p_vinc IS INITIAL.

    LOOP AT t_produtor INTO sl_produtor.

      CLEAR sl_makt.
      READ TABLE t_makt INTO sl_makt
        WITH KEY matnr = sl_produtor-cd_material
        BINARY SEARCH.

      sl_alv-nr_dco   = sl_produtor-nr_dco.
      sl_alv-nu_dco   = sl_produtor-nu_dco.
      sl_alv-dt_lanc  = sl_produtor-dt_lancamento.
      sl_alv-qdt_dco  = sl_produtor-qt_material.
      sl_alv-produtor = sl_produtor-id_fornecedor.
      sl_alv-material = sl_produtor-cd_material.
      sl_alv-desc     = sl_makt-maktx.
      sl_alv-qtd_ult  = sl_produtor-qt_remessa.
      sl_alv-vbeln    = sl_produtor-vbeln.
      sl_alv-saldo    = sl_produtor-qt_material - sl_produtor-qt_remessa.

      IF sl_alv-saldo LE 0.
        CONTINUE.
      ENDIF.

      APPEND sl_alv TO t_alv.
      CLEAR sl_produtor.

    ENDLOOP.

  ELSE.

    LOOP AT t_vinculo INTO sl_vinculo.

      CLEAR sl_makt.
      READ TABLE t_makt INTO sl_makt
        WITH KEY matnr = sl_vinculo-cd_material
        BINARY SEARCH.

      sl_alv-nr_dco   = sl_vinculo-nr_dco.
      sl_alv-nu_dco   = sl_vinculo-nu_dco.
      sl_alv-dt_lanc  = sl_vinculo-dt_lanca.
      sl_alv-qdt_dco  = sl_vinculo-qt_dco.
      sl_alv-produtor = sl_vinculo-id_fornecedor.
      sl_alv-material = sl_vinculo-cd_material.
      sl_alv-desc     = sl_makt-maktx.
      sl_alv-qtd_ult  = sl_vinculo-qt_vinculada.
      sl_alv-vbeln    = sl_vinculo-vbeln.
      sl_alv-saldo    = sl_vinculo-qt_dco - sl_vinculo-qt_vinculada.

      APPEND sl_alv TO t_alv.
      CLEAR sl_vinculo.

    ENDLOOP.

  ENDIF.

* Monta FieldCat
  PERFORM z_monta_fieldcat .

ENDFORM.                    " Z_MONTA_RELATORIO

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MAKT                                         *
*&---------------------------------------------------------------------*
*                            Seleciona MAKT                            *
*----------------------------------------------------------------------*
FORM z_seleciona_makt.

  CHECK NOT t_produtor[] IS INITIAL.

  SELECT matnr maktx
    FROM makt
    INTO TABLE t_makt
    FOR ALL ENTRIES IN t_produtor
  WHERE  matnr EQ t_produtor-cd_material
    AND  spras EQ 'PT'.

  SORT t_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat,
           t_tool.

  CHECK NOT t_alv[] IS INITIAL.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'NR_DCO'   TEXT-004 20,
    c_table 'DT_LANC'  TEXT-005 10,
    c_table 'QDT_DCO'  TEXT-006 12,
    c_table 'PRODUTOR' TEXT-007 10,
    c_table 'MATERIAL' TEXT-008 18,
    c_table 'DESC'     TEXT-009 20,
    c_table 'QTD_ULT'  TEXT-010 12,
    c_table 'SALDO'    TEXT-012 12,
    c_table 'VBELN'    TEXT-011 10.

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

ENDFORM.                    " Z_MONTA_FIELDCAT

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
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_table TYPE c
                               p_field TYPE c
                               p_desc  TYPE c
                               p_len   TYPE n.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-ucomm.
    WHEN ''.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

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
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  CHECK s_event IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER: s_event->zm_handle_user_command FOR s_alv,
               s_event->zm_handle_toolbar      FOR s_alv.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
*     i_save                        =
      i_default                     = 'X'
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_alv
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

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
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

  CLEAR sl_toolbar.

  IF p_desc IS INITIAL.
*   Botão Vincular DCO
    MOVE: 'VINCDCO'          TO sl_toolbar-function ,
           icon_copy_object  TO sl_toolbar-icon     ,
           TEXT-014          TO sl_toolbar-quickinfo,
           TEXT-014          TO sl_toolbar-text     ,
           space             TO sl_toolbar-disabled .
  ELSE.
*   Botão Desvincular DCO
    MOVE: 'DESVDCO'          TO sl_toolbar-function ,
           icon_delete       TO sl_toolbar-icon     ,
           TEXT-018          TO sl_toolbar-quickinfo,
           TEXT-018          TO sl_toolbar-text     ,
           space             TO sl_toolbar-disabled .
  ENDIF.

  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm.

  CASE p_ucomm.
    WHEN 'VINCDCO'.
*     Vincular DCO
      PERFORM z_vincular_dco.
    WHEN 'DESVDCO'.
*     Desvincular DCO
      PERFORM z_desvincular_dco.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_VINCULAR_DCO                                           *
*&---------------------------------------------------------------------*
*                            Vincular DCO                              *
*----------------------------------------------------------------------*
FORM z_vincular_dco.

  DATA: tl_rows     TYPE lvc_t_row,
        sl_rows     TYPE lvc_s_row,
        sl_alv      TYPE type_alv,
        sl_lips     TYPE type_lips,
        sl_vinculo  TYPE zdco_vinculo,
        sl_produtor TYPE zdco_produtor,
        vl_qtd      TYPE char17,
        vl_answer   TYPE char1,
        vl_line     TYPE i.


  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT
* Verifica Seleção de Linhas
    CALL METHOD s_alv->get_selected_rows
      IMPORTING
        et_index_rows = tl_rows.
  ELSE.
    CLEAR tl_rows.
    REFRESH tl_rows.
    sl_rows-index = 1.
    APPEND sl_rows TO tl_rows.
  ENDIF.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-013.
    EXIT.
  ENDIF.

  DESCRIBE TABLE tl_rows LINES vl_line.
  IF vl_line GT 1.
    MESSAGE i836 WITH TEXT-015.
    EXIT.
  ENDIF.

  READ TABLE: tl_rows    INTO sl_rows   INDEX 1            ,
              t_lips     INTO sl_lips   INDEX 1            ,
              t_alv      INTO sl_alv    INDEX sl_rows-index,
              t_produtor INTO sl_produtor
              WITH KEY nu_dco = sl_alv-nu_dco
              BINARY SEARCH.

  IF sl_alv-saldo LT sl_lips-lfimg.
    vl_qtd = sl_lips-lfimg.
    CONDENSE vl_qtd.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE i836 WITH TEXT-016 vl_qtd.
        EXIT.
      WHEN abap_true.
        MESSAGE i836 WITH TEXT-016 vl_qtd INTO DATA(l_mesg).
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
        EXIT.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = TEXT-017
      IMPORTING
        answer         = vl_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF NOT vl_answer EQ '1'.
      EXIT.
    ENDIF.
  ENDIF.

  sl_vinculo-vbeln         = p_vbeln.
  sl_vinculo-nr_dco        = sl_alv-nr_dco.
  sl_vinculo-nu_dco        = sl_alv-nu_dco.
  sl_vinculo-id_fornecedor = sl_alv-produtor.
  sl_vinculo-dt_lanca      = sy-datum.
  sl_vinculo-qt_dco        = sl_alv-qdt_dco.
  sl_vinculo-cd_material   = sl_alv-material.
  sl_vinculo-qt_vinculada  = sl_lips-lfimg.

  INSERT zdco_vinculo FROM sl_vinculo.
  sl_produtor-qt_remessa = sl_produtor-qt_remessa + sl_lips-lfimg.

  UPDATE zdco_produtor
    SET qt_remessa = sl_produtor-qt_remessa
  WHERE  nu_dco EQ sl_alv-nu_dco.

  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT.
    MESSAGE i836 WITH TEXT-021.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " Z_VINCULAR_DCO

*&---------------------------------------------------------------------*
*&      Form  Z_DESVINCULAR_DCO                                        *
*&---------------------------------------------------------------------*
*                            Desvincular DCO                           *
*----------------------------------------------------------------------*
FORM z_desvincular_dco.

  DATA: tl_rows     TYPE lvc_t_row,
        sl_rows     TYPE lvc_s_row,
        sl_alv      TYPE type_alv,
        sl_produtor TYPE zdco_produtor,
        vl_answer   TYPE char1.

  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT.
* Verifica Seleção de Linhas
    CALL METHOD s_alv->get_selected_rows
      IMPORTING
        et_index_rows = tl_rows.
  ELSE.
    CLEAR tl_rows.
    REFRESH tl_rows.
    sl_rows-index = 1.
    APPEND sl_rows TO tl_rows.
  ENDIF.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-013.
    EXIT.
  ENDIF.

  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = TEXT-019
      IMPORTING
        answer         = vl_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF NOT vl_answer EQ '1'.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE: tl_rows INTO sl_rows INDEX 1            ,
              t_alv   INTO sl_alv  INDEX sl_rows-index.
  SELECT SINGLE *
    FROM zdco_produtor
    INTO sl_produtor
  WHERE  nu_dco EQ sl_alv-nu_dco.
  sl_produtor-qt_remessa = sl_produtor-qt_remessa - sl_alv-qtd_ult.

  DELETE FROM zdco_vinculo WHERE vbeln EQ sl_alv-vbeln.
  UPDATE zdco_produtor
     SET qt_remessa = sl_produtor-qt_remessa
   WHERE nu_dco EQ sl_alv-nu_dco.

  IF sy-tcode NE 'ZLES0106' AND p_fataut = abap_off.  "*-#133089-21.02.2024-JT.
    MESSAGE i836 WITH TEXT-020.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " Z_DESVINCULAR_DCO

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MAKT_2                                       *
*&---------------------------------------------------------------------*
*                            Seleciona MAKT                            *
*----------------------------------------------------------------------*
FORM z_seleciona_makt_2.

  CHECK NOT t_vinculo[] IS INITIAL.

  SELECT matnr maktx
    FROM makt
    INTO TABLE t_makt
    FOR ALL ENTRIES IN t_vinculo
  WHERE  matnr EQ t_vinculo-cd_material
    AND  spras EQ 'PT'.

  SORT t_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT_2
