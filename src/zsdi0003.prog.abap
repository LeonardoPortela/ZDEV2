*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0003                                                *
* Descrição  : Formação de Preço Venda Frame’s                         *
* Módulo     : SD                                Transação: ZSDT0003   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 02/08/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0003 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TYPE-POOLS icon.
TABLES zsdt0020.
*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_calc,
         med_pre   TYPE znivpremio,
         med_chi   TYPE znivchicago,
         spread    TYPE zspread,
         med_pta   TYPE zptax,
         fret_f    TYPE zfretef,
         fret_c    TYPE zfretec,
         cust      TYPE zcustof,
         final     TYPE wertv8,
         final_uss TYPE wertv8,
         vbeln     TYPE vbeln_va,
         periodo   TYPE spwoc,
         marc      TYPE char1,
       END   OF type_calc.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0020 TYPE TABLE OF zsdt0020  ,
      t_t0020    TYPE TABLE OF zsdt0020  ,
      t_tvarvc   TYPE TABLE OF tvarvc    ,
      t_tvarvc_a TYPE TABLE OF tvarvc    ,
      t_tool     TYPE ui_functions       ,
      t_fcat     TYPE TABLE OF lvc_s_fcat,
      t_calc     TYPE TABLE OF type_calc .

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_table TYPE char10 VALUE 'T_ZSDT0020',
           c_memo  TYPE char6  VALUE 'S_MEMO'    ,
           c_x     TYPE char1  VALUE 'X'         ,
           c_1     TYPE char1  VALUE '1'         ,
           c_brl   TYPE char3  VALUE 'BRL'       ,
           c_usd   TYPE char3  VALUE 'USD'       .

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cont   TYPE REF TO cl_gui_custom_container,
      s_alv    TYPE REF TO cl_gui_alv_grid        ,
      s_layout TYPE lvc_s_layo                    ,
      s_calc   TYPE type_calc                     .

CONTROLS tc_frame TYPE TABLEVIEW USING SCREEN '0100'.
*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
SELECT-OPTIONS:
  s_vbeln   FOR zsdt0020-vbeln      NO-EXTENSION NO INTERVALS
                                    OBLIGATORY MATCHCODE OBJECT vmva,
  s_perio   FOR zsdt0020-periodo    NO-EXTENSION NO INTERVALS
                                    OBLIGATORY,
  s_status  FOR zsdt0020-status     NO-EXTENSION NO INTERVALS
                                    DEFAULT 01               .
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Seleciona Dados
  PERFORM: z_seleciona_dados,

* Monta Dados
           z_monta_dados    ,

* Monta ALV
           z_mont_alv      .

  IF t_zsdt0020[] IS INITIAL.
    MESSAGE i836 WITH text-002.
  ELSE.
    CALL SCREEN 0100.
  ENDIF.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
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

* Botão Vincular NF's
  CLEAR sl_toolbar.
  MOVE: 'PROCCAL'              TO sl_toolbar-function ,
         icon_workflow_process TO sl_toolbar-icon     ,
         text-003              TO sl_toolbar-quickinfo,
         text-003              TO sl_toolbar-text     ,
         space                 TO sl_toolbar-disabled .
  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm.

  CASE p_ucomm.
    WHEN 'PROCCAL'.
*     Processar Cálculo
      PERFORM z_proc_calc.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                           Seleciona Dados                            *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona ZSDT0020
  PERFORM: z_seleciona_zsdt0020,

* Seleciona TVARVC
           z_seleciona_tvarvc  .

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0020                                     *
*&---------------------------------------------------------------------*
*                           Seleciona ZSDT0020                         *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0020.

  REFRESH t_zsdt0020.

  SELECT *
    FROM zsdt0020
    INTO TABLE t_zsdt0020
  WHERE  vbeln IN s_vbeln.

  DELETE t_zsdt0020 WHERE: periodo NOT IN s_perio ,
                           status  NOT IN s_status.

  SORT t_zsdt0020 BY id       ASCENDING
                     id_preco ASCENDING
                     vbeln    ASCENDING.

  IF t_zsdt0020[] IS INITIAL.
    MESSAGE i836 WITH text-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_ZSDT0020

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_DADOS                                            *
*&---------------------------------------------------------------------*
*                             Monta Dados                              *
*----------------------------------------------------------------------*
FORM z_monta_dados.

ENDFORM.                    " Z_MONTA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_MONT_ALV                                               *
*&---------------------------------------------------------------------*
*                             Monta ALV                                *
*----------------------------------------------------------------------*
FORM z_mont_alv.

  REFRESH: t_fcat,
           t_tool.

  CHECK NOT t_zsdt0020 IS INITIAL.

* PredCatenche Fiel
  PERFORM z_preenche_fieldcat.

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
FORM z_preenche_fieldcat.

  DATA vl_name TYPE dd02l-tabname.

  vl_name = 'ZSDT0020'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = vl_name
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE t_fcat INDEX 1.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

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
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      DESCRIBE TABLE t_calc LINES tc_frame-lines.
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
    MESSAGE i836 WITH text-013.
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
    MESSAGE i836 WITH text-014.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA: vl_int TYPE int4      ,
        vl_var TYPE disvariant.

  vl_var-report = sy-repid.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      i_default                     = c_x
      is_variant                    = vl_var
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_zsdt0020
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
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                           Exit Command                               *
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
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-ucomm.
    WHEN 'BT_DEL'.
*     Deleta Linhas
      PERFORM z_del.
    WHEN 'BT_ALL'.
*     Marca Todas as Linhas
      PERFORM z_marc_all.
    WHEN 'BT_NONE'.
*     Desmarca as Linhas
      PERFORM z_marc_none.
    WHEN 'BT_FIX'.
*     Fixa Preço
      PERFORM z_fixa_preco.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_DEL                                                    *
*&---------------------------------------------------------------------*
*                              Deleta Linhas                           *
*----------------------------------------------------------------------*
FORM z_del.

  DELETE t_calc WHERE marc NE space.

ENDFORM.                    " Z_DEL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_ALL                                               *
*&---------------------------------------------------------------------*
*                          Marca Todas as Linhas                       *
*----------------------------------------------------------------------*
FORM z_marc_all.

  s_calc-marc = 'X'.
  MODIFY t_calc FROM s_calc
    TRANSPORTING marc
    WHERE marc IS INITIAL.

ENDFORM.                    " Z_MARC_ALL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_NONE                                              *
*&---------------------------------------------------------------------*
*                           Desmarca as Linhas                         *
*----------------------------------------------------------------------*
FORM z_marc_none.

  s_calc-marc = space.
  MODIFY t_calc FROM s_calc
    TRANSPORTING marc
    WHERE marc NE space.

ENDFORM.                    " Z_MARC_NONE

*&---------------------------------------------------------------------*
*&      Module  ZM_MARC      INPUT                                     *
*&---------------------------------------------------------------------*
*                             Marcação Linha                           *
*----------------------------------------------------------------------*
MODULE zm_marc INPUT.

  MODIFY t_calc FROM s_calc INDEX tc_frame-current_line
    TRANSPORTING marc.

ENDMODULE.                 " ZM_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_PROC_CALC                                              *
*&---------------------------------------------------------------------*
*                           Processar Cálculo                          *
*----------------------------------------------------------------------*
FORM z_proc_calc.

  DATA: tl_rows   TYPE lvc_t_row        ,
        tl_aux    TYPE TABLE OF zsdt0020,
        sl_rows   TYPE lvc_s_row        ,
        sl_t0020  TYPE zsdt0020         ,
        sl_tvarvc TYPE tvarvc           ,
        sl_aux    TYPE tvarvc           ,
        sl_calc   TYPE type_calc        ,
        vl_lines  TYPE i                ,
        vl_linhas TYPE i                ,
        vl_premio TYPE znivpremio       ,
        vl_chicag TYPE znivchicago      ,
        vl_sum_p  TYPE znivpremio       ,
        vl_sum_c  TYPE znivchicago      ,
        vl_cust   TYPE zcustof          ,
        vl_name   TYPE rvari_vnam       ,
        vl_final  TYPE znivpremio       ,
        vl_usd    TYPE znivpremio       ,
        vl_conv   TYPE zptax            .

  REFRESH: t_calc ,
           t_t0020.

* Verifica Seleção de Linhas
  CALL METHOD s_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH text-004.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.
    READ TABLE t_zsdt0020 INTO sl_t0020
      INDEX sl_rows-index.
    APPEND sl_t0020 TO t_t0020.
    CLEAR: sl_rows ,
           sl_t0020.
  ENDLOOP.

* Verifica ID Preço
  tl_aux[] = t_t0020[].
  DELETE tl_aux WHERE id_preco EQ space.
  IF NOT tl_aux[] IS INITIAL.
    MESSAGE i836 WITH text-020.
    EXIT.
  ENDIF.

* Verifica SPREAD
  tl_aux[] = t_t0020[].
  SORT tl_aux BY spread ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING spread.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-005.
    EXIT.
  ENDIF.

* Verifica Frete Fob
  tl_aux[] = t_t0020[].
  SORT tl_aux BY frete_fob ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING frete_fob.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-006.
    EXIT.
  ENDIF.

* Verifica Frete CIF
  tl_aux[] = t_t0020[].
  SORT tl_aux BY frete_cif ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING frete_cif.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-007.
    EXIT.
  ENDIF.

* Verifica Vbeln
  tl_aux[] = t_t0020[].
  SORT tl_aux BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING vbeln.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-008.
    EXIT.
  ENDIF.

* Verifica Período
  tl_aux[] = t_t0020[].
  SORT tl_aux BY periodo ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING periodo.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-009.
    EXIT.
  ENDIF.

* Verifica Custo Financeiro
  tl_aux[] = t_t0020[].
  SORT tl_aux BY custo_financ ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING custo_financ.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-014.
    EXIT.
  ENDIF.

* Verifica Comissão
  tl_aux[] = t_t0020[].
  SORT tl_aux BY comissao ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING comissao.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-010.
    EXIT.
  ENDIF.

* Verifica Pis
  tl_aux[] = t_t0020[].
  SORT tl_aux BY pis ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING pis.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-011.
    EXIT.
  ENDIF.

* Verifica Cofins
  tl_aux[] = t_t0020[].
  SORT tl_aux BY cofins ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING cofins.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-012.
    EXIT.
  ENDIF.

* Verifica Icms
  tl_aux[] = t_t0020[].
  SORT tl_aux BY icms ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING icms.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-013.
    EXIT.
  ENDIF.

* Verifica PTAX
  tl_aux[] = t_t0020[].
  SORT tl_aux BY ptax_medio ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING ptax_medio.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-015.
    EXIT.
  ENDIF.

* Verifica Material
  tl_aux[] = t_t0020[].
  SORT tl_aux BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING matnr.
  DESCRIBE TABLE tl_aux LINES vl_lines.
  IF vl_lines GT 1.
    MESSAGE i836 WITH text-016.
    EXIT.
  ENDIF.

* Verifica Quantidade
  tl_aux[] = t_t0020[].
  DESCRIBE TABLE tl_aux LINES vl_lines.
  DELETE tl_aux WHERE qtd LE 0.
  DESCRIBE TABLE tl_aux LINES vl_linhas.
  IF vl_linhas NE 0 AND
     vl_lines  GT 1.
    MESSAGE i836 WITH text-021 text-022.
    EXIT.
  ENDIF.

  LOOP AT t_t0020 INTO sl_t0020.
    vl_premio = sl_t0020-niv_premio * sl_t0020-qtd_fix_premio.
    ADD vl_premio TO vl_sum_p.
    vl_chicag = sl_t0020-niv_chicago * sl_t0020-qtd_fix_chicago.
    ADD vl_chicag TO vl_sum_c.
    CLEAR: sl_t0020 ,
           vl_premio,
           vl_chicag.
  ENDLOOP.

  READ TABLE t_t0020 INTO sl_t0020
    INDEX 1.

  IF NOT sl_t0020-qtd_mes IS INITIAL.
    vl_premio = vl_sum_p / sl_t0020-qtd_mes.
    vl_chicag = vl_sum_c / sl_t0020-qtd_mes.
  ENDIF.

  IF NOT sl_t0020-qtd IS INITIAL.
    vl_premio = vl_sum_p / sl_t0020-qtd.
    vl_chicag = vl_sum_c / sl_t0020-qtd.
  ENDIF.

  vl_cust = 1 - ( sl_t0020-custo_financ + sl_t0020-comissao +
                  sl_t0020-pis + sl_t0020-cofins + sl_t0020-icms ) / 100.

  READ TABLE t_tvarvc INTO sl_tvarvc
    WITH KEY low = sl_t0020-matnr
    BINARY SEARCH.

  IF sy-subrc IS INITIAL.
    CASE sl_tvarvc-name.
      WHEN 'FRAMES_SOJA'.
        vl_name = 'CONVERT_SOJA'.
      WHEN 'FRAMES_FARELO'.
        vl_name = 'CONVERT_FARELO'.
      WHEN 'FRAMES_OLEO'.
        vl_name = 'CONVERT_OLEO'.
      WHEN 'FRAMES_MILHO'.
        vl_name = 'CONVERT_MILHO'.
    ENDCASE.
    READ TABLE t_tvarvc_a INTO sl_aux
      WITH KEY name = vl_name
      BINARY SEARCH.
  ENDIF.

  IF NOT sl_aux-low IS INITIAL.
    REPLACE ALL OCCURRENCES OF ',' IN sl_aux-low WITH '.'.
    CONDENSE sl_aux-low NO-GAPS.
    MOVE sl_aux-low TO vl_conv.
  ENDIF.

  IF sl_t0020-moeda_fob EQ c_brl.
    vl_final = vl_premio + vl_chicag + sl_t0020-spread.
    vl_final = vl_final * vl_conv.
    vl_final = vl_final * sl_t0020-ptax_medio.
    vl_final = vl_final - sl_t0020-frete_fob + sl_t0020-frete_cif.
    IF NOT vl_cust IS INITIAL.
      vl_final = vl_final / vl_cust.
    ELSE.
      CLEAR vl_final.
    ENDIF.
  ELSE.
    vl_final = vl_premio + vl_chicag + sl_t0020-spread.
    vl_final = vl_final * vl_conv.
    vl_final = vl_final - sl_t0020-frete_fob.
    vl_final = vl_final * sl_t0020-ptax_medio.
    vl_final = vl_final + sl_t0020-frete_cif.
    IF NOT vl_cust IS INITIAL.
      vl_final = vl_final / vl_cust.
    ELSE.
      CLEAR vl_final.
    ENDIF.
  ENDIF.

  IF NOT sl_t0020-ptax_medio IS INITIAL.
    vl_usd = vl_final / sl_t0020-ptax_medio.
  ENDIF.

  sl_calc-med_pre   = vl_premio.
  sl_calc-med_chi   = vl_chicag.
  sl_calc-spread    = sl_t0020-spread.
  sl_calc-med_pta   = sl_t0020-ptax_medio.
  sl_calc-fret_f    = sl_t0020-frete_fob.
  sl_calc-fret_c    = sl_t0020-frete_cif.
  sl_calc-vbeln     = sl_t0020-vbeln.
  sl_calc-periodo   = sl_t0020-periodo.
  sl_calc-cust      = vl_cust.
  sl_calc-final     = vl_final.
  sl_calc-final_uss = vl_usd.

  APPEND sl_calc TO t_calc.
  LEAVE TO SCREEN 0100.

ENDFORM.                    " Z_PROC_CALC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVARVC                                       *
*&---------------------------------------------------------------------*
*                           Seleciona TVARVC                           *
*----------------------------------------------------------------------*
FORM z_seleciona_tvarvc.

  DATA: sl_tvarvc TYPE tvarvc    ,
        vl_matnr  TYPE mara-matnr,
        vl_index  TYPE i         .

  SELECT *
    FROM tvarvc
    INTO TABLE t_tvarvc.

  LOOP AT t_tvarvc INTO sl_tvarvc.

    vl_index = sy-tabix.
    vl_matnr = sl_tvarvc-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vl_matnr
      IMPORTING
        output = vl_matnr.

    sl_tvarvc-low = vl_matnr.
    MODIFY t_tvarvc FROM sl_tvarvc
      INDEX vl_index
      TRANSPORTING low.

    CLEAR sl_tvarvc.

  ENDLOOP.

  t_tvarvc_a[] = t_tvarvc[].
  SORT: t_tvarvc   BY low  ASCENDING,
        t_tvarvc_a BY name ASCENDING.

ENDFORM.                    " Z_SELECIONA_TVARVC

*&---------------------------------------------------------------------*
*&      Form  Z_FIXA_PRECO                                             *
*&---------------------------------------------------------------------*
*                               Fixa Preço                             *
*----------------------------------------------------------------------*
FORM z_fixa_preco.

  DATA: vl_answer   TYPE char1    ,
        sl_zsdt0021 TYPE zsdt0021 ,
        sl_t0020    TYPE zsdt0020 ,
        sl_calc     TYPE type_calc,
        vl_id       TYPE zid_preco,
        vl_id_c     TYPE char10   ,
        vl_tabix    TYPE i        .

  IF t_calc[] IS INITIAL.
    MESSAGE i836 WITH text-019.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = text-017
    IMPORTING
      answer         = vl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF NOT vl_answer EQ c_1.
    EXIT.
  ENDIF.

  READ TABLE t_calc INTO sl_calc
    INDEX 1.

  SELECT MAX( id_preco )
    FROM zsdt0021
    INTO vl_id
  WHERE  vbeln EQ sl_calc-vbeln.

  IF NOT sy-subrc IS INITIAL.
    vl_id = 1.
  ELSE.
    ADD 1 TO vl_id.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = vl_id
    IMPORTING
      output = vl_id_c.

  sl_zsdt0021-id_preco    = vl_id.
  sl_zsdt0021-vbeln       = sl_calc-vbeln.
  sl_zsdt0021-periodo     = sl_calc-periodo.
  sl_zsdt0021-dta_preco   = sy-datum.
  sl_zsdt0021-med_premio  = sl_calc-med_pre.
  sl_zsdt0021-med_chicago = sl_calc-med_chi.
  sl_zsdt0021-spread      = sl_calc-spread.
  sl_zsdt0021-custo_imp   = sl_calc-cust.
  sl_zsdt0021-frete_fob   = sl_calc-fret_f.
  sl_zsdt0021-frete_cif   = sl_calc-fret_c.
  sl_zsdt0021-ptax        = sl_calc-med_pta.
  sl_zsdt0021-preco_final = sl_calc-final.
  sl_zsdt0021-ordem_venda = space.

  INSERT zsdt0021 FROM sl_zsdt0021.
  MESSAGE i836 WITH text-018 vl_id_c.
  REFRESH t_calc.

  sl_t0020-id_preco = sl_zsdt0021-id_preco.
  MODIFY t_t0020 FROM sl_t0020
    TRANSPORTING id_preco
    WHERE id_preco IS INITIAL.

  LOOP AT t_t0020 INTO sl_t0020.
    READ TABLE t_zsdt0020
      WITH KEY id       = sl_t0020-id
               id_preco = space
               vbeln    = sl_t0020-vbeln
      BINARY SEARCH
      TRANSPORTING NO FIELDS.
    vl_tabix = sy-tabix.
    IF sy-subrc IS INITIAL.
      MODIFY t_zsdt0020 FROM sl_t0020
        INDEX vl_tabix
        TRANSPORTING id_preco.
    ENDIF.
    UPDATE zsdt0020
       SET: id_preco = sl_t0020-id_preco
            status   = 2
      WHERE id       EQ sl_t0020-id
        AND id_preco EQ space
        AND vbeln    EQ sl_t0020-vbeln.
    CLEAR sl_t0020.
  ENDLOOP.

  CALL METHOD s_alv->refresh_table_display.

ENDFORM.                    " Z_FIXA_PRECO
