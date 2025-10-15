************************************************************************
* Program        : ZOPENSIS_REL_001                                    *
* Transaction    : ZOPENSIS_001                                        *
* Title          : Relatório de Controle Compensatório                 *
* Developer      : Fernando Oliveira                                   *
* Date           : 27/06/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     27/06/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
REPORT  zopensis_rel_001.

*----------------------------------------------------------------------*
* Tabelas SAP                                                          *
*----------------------------------------------------------------------*
TABLES: cdhdr,         "Cabeçalho do documento de modificação
        tstct,         "Textos de código de transação
        usr21,         "Atribuição nome usuário chave endereço
        adcp ,         "Atribuição pessoa/endereço (administração endereços central)
        adrp ,         "Pessoas (administração de endereços central)
        cdpos,         "Itens doc.modificação
        dd03m,         "Campos tabelas c/elementos dados, textos e domínios
        syst ,         "Campos de sistema ABAP
        ztopensis_001. "

*----------------------------------------------------------------------*
* Declaração de Tipos                                                  *
*----------------------------------------------------------------------*
"CDHDR - Cabeçalho do Documento de Modificaçãao
TYPES: BEGIN OF ty_cdhdr,
         objectclas TYPE cdhdr-objectclas,
         objectid   TYPE cdhdr-objectid,
         changenr   TYPE cdhdr-changenr,
         username   TYPE cdhdr-username,
         udate      TYPE cdhdr-udate,
         utime      TYPE cdhdr-utime,
         tcode      TYPE cdhdr-tcode,
         change_ind TYPE cdhdr-change_ind,
         langu      TYPE cdhdr-langu,
       END OF ty_cdhdr.

"TSTCT - Textos de Código de transação
TYPES: BEGIN OF ty_tstct,
         sprsl TYPE tstct-sprsl,
         tcode TYPE tstct-tcode,
         ttext TYPE tstct-ttext,
       END OF ty_tstct.

"CDPOS - Itens Doumento de Modificação
TYPES: BEGIN OF ty_cdpos,
         objectclas TYPE cdpos-objectclas,
         objectid   TYPE cdpos-objectid,
         changenr   TYPE cdpos-changenr,
         tabkey     TYPE cdpos-tabkey   , "Chave do Documento
         tabname    TYPE cdpos-tabname,
         fname      TYPE cdpos-fname,
         value_new  TYPE cdpos-value_new,
         value_old  TYPE cdpos-value_old,
       END OF ty_cdpos.

"USR21 - Nome do Usuário
TYPES: BEGIN OF ty_usr21,
         bname      TYPE usr21-bname,
         persnumber TYPE usr21-persnumber,
       END OF ty_usr21.

"DD03M - Gerar Tabela para a View
TYPES: BEGIN OF ty_dd03m,
         tabname    TYPE dd03m-tabname,
         fieldname  TYPE dd03m-fieldname,
         ddlanguage TYPE dd03m-ddlanguage,
         ddtext     TYPE dd03m-ddtext,
       END OF ty_dd03m.

"ADCP - Atribuição pessoa e endereço
TYPES: BEGIN OF ty_adcp,
         addrnumber TYPE adcp-addrnumber,
         persnumber TYPE adcp-persnumber,
         nation     TYPE adcp-nation,
         department TYPE adcp-department,
       END OF ty_adcp.

"ADRP - Pessoas
TYPES: BEGIN OF ty_adrp,
         persnumber TYPE adrp-persnumber,
         nation     TYPE adrp-nation,
         name_text  TYPE adrp-name_text,
       END OF ty_adrp.

TYPES: BEGIN OF ty_alv,
         bname          TYPE usr21-bname    , "Quem Executou
         name_text      TYPE adrp-name_text , "Nome Completo
         department     TYPE adcp-department, "Departamento
         tcode          TYPE cdhdr-tcode    , "Transação
         ttext          TYPE tstct-ttext    , "Texto da Transação
         udate          TYPE cdhdr-udate    , "Data
         utime          TYPE cdhdr-utime    , "Hora
         changenr       TYPE cdpos-changenr , "Documento
         tabkey         TYPE cdpos-tabkey   , "Chave do Documento
         tabname        TYPE cdpos-tabname  , "Tabela
         fname          TYPE cdpos-fname    , "Campo
         ddtext         TYPE dd03m-ddtext   , "Descrição Campo
         valor_old      TYPE cdpos-value_old, "Valor Antigo
         valor_new      TYPE cdpos-value_new, "Valor Novo
         justificavel_s TYPE c              , "Justificável Sim
         justificavel_n TYPE c              , "Justificável Não
         avaliador      TYPE usr21-bname    , "Avaliador
         data_avaliacao TYPE cdhdr-udate    , "Data da Execução do Relatorio
         hora_avaliazao TYPE cdhdr-utime    , "Hora da Execução do Relatorio
         just_s_origem  TYPE c              , "Justificável Sim
         just_n_origem  TYPE c              , "Justificável Não
       END OF ty_alv.

*----------------------------------------------------------------------*
* Declaração de tabelas interna                                        *
*----------------------------------------------------------------------*
DATA: gt_cdhdr           TYPE TABLE OF ty_cdhdr,
      gt_tstct           TYPE TABLE OF ty_tstct,
      gt_cdpos           TYPE TABLE OF ty_cdpos,
      gt_usr21           TYPE TABLE OF ty_usr21,
      gt_dd03m           TYPE TABLE OF ty_dd03m,
      gt_adcp            TYPE TABLE OF ty_adcp,
      gt_adrp            TYPE TABLE OF ty_adrp,
      gt_ztopensis_001   TYPE TABLE OF ztopensis_001,
      gt_ztopensis_002   TYPE TABLE OF ztopensis_002,
      gt_ztopensis_002_x TYPE TABLE OF ztopensis_002,
      gt_ztopensis_003   TYPE TABLE OF ztopensis_003,
      gt_ztopensis_004   TYPE TABLE OF ztopensis_004.

DATA: gt_alv TYPE TABLE OF ty_alv.

DATA gt_sd_opcoes_procto TYPE spopli OCCURS 5 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração de Work-áreas                                             *
*----------------------------------------------------------------------*
DATA: gs_cdhdr           TYPE ty_cdhdr,
      gs_tstct           TYPE ty_tstct,
      gs_cdpos           TYPE ty_cdpos,
      gs_usr21           TYPE ty_usr21,
      gs_dd03m           TYPE ty_dd03m,
      gs_adcp            TYPE ty_adcp,
      gs_adrp            TYPE ty_adrp,
      gs_ztopensis_001   TYPE ztopensis_001,
      gs_ztopensis_002   TYPE ztopensis_002,
      gs_ztopensis_002_x TYPE ztopensis_002,
      gs_ztopensis_003   TYPE ztopensis_003,
      gs_ztopensis_004   TYPE ztopensis_004.

DATA: gs_alv TYPE ty_alv.

*----------------------------------------------------------------------*
* Declaração de Variáveis                                              *
*----------------------------------------------------------------------*
DATA: gc_erro,
      gc_back,
      gc_fire.

*----------------------------------------------------------------------*
* Declaração de Constantes                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Objetos ALV                                                          *
*----------------------------------------------------------------------*
DATA:   container_r1   TYPE REF TO cl_gui_custom_container,
        grid_r1        TYPE REF TO cl_gui_alv_grid,
        t_fieldcat_1   TYPE        lvc_t_fcat,
        t_fieldcat_bkp TYPE        lvc_t_fcat,
        w_fieldcat     TYPE        lvc_s_fcat,
        w_layout_1     TYPE        lvc_s_layo,
        w_print        TYPE        lvc_s_prnt,
        t_edit_upd_on  TYPE        lvc_t_styl,
        t_edit_upd_off TYPE        lvc_t_styl.

DATA: gs_f4 TYPE lvc_s_f4,
      gt_f4 TYPE lvc_t_f4.

DATA: w_lvc_s_roid  TYPE lvc_s_roid,
      w_lvc_s_row   TYPE lvc_s_row,
      w_lvc_s_col   TYPE lvc_s_col,
      w_lvc_s_roid2 TYPE lvc_s_roid,
      w_lvc_s_row2  TYPE lvc_s_row,
      w_lvc_s_col2  TYPE lvc_s_col,
      w_edit        TYPE lvc_s_styl.

DATA: w_s_roid      TYPE lvc_s_roid,
      w_alv_variant TYPE disvariant.

DATA: o_popup          TYPE REF TO cl_salv_table,
      o_grid_popup     TYPE REF TO cl_salv_form_layout_grid,
      o_display_pop    TYPE REF TO cl_salv_display_settings,
      o_selections_pop TYPE REF TO cl_salv_selections,
      o_functions_pop  TYPE REF TO cl_salv_functions_list,
      o_columns_pop    TYPE REF TO cl_salv_columns,
      o_column_pop     TYPE REF TO cl_salv_column_table.

DATA: t_modi      TYPE TABLE OF lvc_s_modi.
DATA: w_modi   TYPE lvc_s_modi,
      w_modi_x TYPE lvc_s_modi.

*----------------------------------------------------------------------*
* Declaração de classes                                                *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:

      "Barra de Ferramentas
      handle_toolbar1      FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
* Implementação de classes                                             *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  "Chamada da barra de ferramentas (Criação)
  METHOD handle_toolbar1.

    PERFORM zf_adicionar_excluir_botoes CHANGING e_object.

  ENDMETHOD.                    "handle_toolbar
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

DATA: event_receiver TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* Field-Symbol                                                         *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Parãmetros de Seleção                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) text-t01.
SELECTION-SCREEN POSITION 47.
SELECT-OPTIONS: sc_data  FOR sy-datum   .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (46) text-t02.
SELECTION-SCREEN POSITION 47.
SELECT-OPTIONS: sc_tcode FOR tstct-tcode NO INTERVALS ."MODIF ID x1.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE .
*SELECTION-SCREEN COMMENT (46) text-t03.
*SELECTION-SCREEN POSITION 47.
SELECT-OPTIONS: sc_lang1 FOR tstct-sprsl DEFAULT 'PT' NO-DISPLAY.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (46) text-t04.
SELECTION-SCREEN POSITION 47.
SELECT-OPTIONS: sc_user FOR cdhdr-username.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (46) text-t05.
SELECTION-SCREEN POSITION 47.
SELECT-OPTIONS: sc_doc FOR cdpos-objectid.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK a2.

*----------------------------------------------------------------------*
* Inicialização                                                        *
*----------------------------------------------------------------------*
INITIALIZATION.

  DATA: lt_ztopensis_003   TYPE TABLE OF ztopensis_003.
  DATA: ls_ztopensis_003   TYPE ztopensis_003.

  DATA: vl_resposta TYPE c,
        vl_ajuste.

  GET PARAMETER ID 'BACK' FIELD gc_back.
  IF gc_back IS NOT INITIAL.
    CLEAR gc_back.
    SET PARAMETER ID 'BACK' FIELD space.
    LEAVE PROGRAM.
  ENDIF.

  SELECT *
    FROM ztopensis_001
    INTO TABLE gt_ztopensis_001.

  SELECT *
    FROM ztopensis_002
    INTO TABLE gt_ztopensis_002.

  SELECT *
    FROM ztopensis_004
    INTO TABLE gt_ztopensis_004.

*---------------------------------------------*
* Usuário Firefighter
*---------------------------------------------*
  GET PARAMETER ID 'FIRE' FIELD gc_fire.
  SET PARAMETER ID 'FIRE' FIELD space.
  IF gc_fire IS INITIAL.

    SELECT *
      FROM ztopensis_003
      INTO TABLE lt_ztopensis_003
      WHERE zflg_risco_habil = 'X'.

    IF sy-subrc <> 0.
      MESSAGE 'Não existe Risco na Base para ser Habilitado' TYPE 'W'.
      LEAVE PROGRAM.
    ENDIF.

    LOOP AT lt_ztopensis_003 INTO ls_ztopensis_003.
      IF ls_ztopensis_003-zflg_risco_habil = 'X'.
        gt_sd_opcoes_procto-selflag   = 'X'.
      ENDIF.

      CONCATENATE ls_ztopensis_003-zcod_risco '-'
                  ls_ztopensis_003-zdsc_risco
                  INTO gt_sd_opcoes_procto-varoption SEPARATED BY space.
      APPEND gt_sd_opcoes_procto.
      CLEAR  gt_sd_opcoes_procto.
    ENDLOOP.

    CLEAR: vl_resposta, vl_ajuste.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
*       mark_flag          = 'X'
        mark_max           = 0
        start_col          = 15
        start_row          = 3
        textline1          = 'Selecione o Risco desejado:'
        titel              = 'Controle Compensatório'
      IMPORTING
        answer             = vl_resposta
      TABLES
        t_spopli           = gt_sd_opcoes_procto
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.

    IF vl_resposta = 'A'.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE gt_ztopensis_004 INTO gs_ztopensis_004 WITH KEY zcod_user = sy-uname.
    IF sy-subrc <> 0.
      MESSAGE 'Usuário de Acesso não esta cadastrado como Firefighter' TYPE 'W'.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF gc_fire IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name CS 'SC_TCODE'  .
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    PERFORM zf_preencher_sc_tcode.

  ELSE.
    LOOP AT SCREEN.
      IF screen-name CS 'TCO' OR
         screen-name CS 'T02'.
        screen-active    = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

AT SELECTION-SCREEN.

  LOOP AT SCREEN.
    IF screen-group1 = 'X1'  OR
       screen-name CS 'TCO'.
*        screen-input     = '0'.
*        screen-output    = '0'.
*        screen-invisible = '1'.
      screen-active    = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sc_tcode-low.
  PERFORM zf_exibir_filtro_tcode.

*----------------------------------------------------------------------*
* Start of Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_selecionar_dados.
  CHECK gc_erro IS INITIAL.
  PERFORM zf_processar_dados .
  PERFORM zf_formatar_alv    .
  PERFORM zf_exibe_alv       .

END-OF-SELECTION.

*----------------------------------------------------------------------*
* Performs                                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM zf_selecionar_dados.

  CLEAR: gc_erro.

  "CDHDR - Cabeçalho do documento de modificação
  SELECT objectclas objectid changenr username udate utime tcode change_ind langu
    FROM cdhdr
    INTO TABLE gt_cdhdr
    WHERE tcode    IN sc_tcode AND
          username IN sc_user  AND
          objectid IN sc_doc   AND
          udate    IN sc_data.

  IF sy-subrc <> 0.
    MESSAGE 'Riscos não materializado para essa seleção.' TYPE 'S'.
    gc_erro = 'X'.
    LEAVE SCREEN.
  ENDIF.

  "TSTCT - Textos de código de transação
  SELECT sprsl tcode ttext
    FROM tstct
    INTO TABLE gt_tstct
    WHERE sprsl IN sc_lang1 AND
          tcode IN sc_tcode.

  IF gt_cdhdr[] IS NOT INITIAL.
    "CDPOS - Itens doc.modificação
    SELECT objectclas objectid changenr tabkey tabname fname value_new value_old
      FROM cdpos
      INTO TABLE gt_cdpos
      FOR ALL ENTRIES IN gt_cdhdr
      WHERE objectclas = gt_cdhdr-objectclas AND
            objectid   = gt_cdhdr-objectid.

    "USR21 - Atribuição nome usuário chave endereço
    SELECT bname persnumber
      FROM usr21
      INTO TABLE gt_usr21
      FOR ALL ENTRIES IN gt_cdhdr
      WHERE bname = gt_cdhdr-username.
  ENDIF.

  IF gt_cdpos[] IS NOT INITIAL.
    "DD03M - Campos tabelas c/elementos dados, textos e domínios
    SELECT tabname fieldname ddlanguage ddtext
      FROM dd03m
      INTO TABLE gt_dd03m
      FOR ALL ENTRIES IN gt_cdpos
      WHERE tabname   = gt_cdpos-tabname AND
            fieldname = gt_cdpos-fname   .
  ENDIF.

  IF gt_usr21 IS NOT INITIAL.
    "ADCP - Atribuição pessoa/endereço (administração endereços central)
    SELECT addrnumber persnumber nation department
      FROM adcp
      INTO TABLE gt_adcp
      FOR ALL ENTRIES IN gt_usr21
      WHERE persnumber = gt_usr21-persnumber.

    IF gt_adcp[] IS NOT INITIAL.
      "ADRP - Pessoas (administração de endereços central)
      SELECT persnumber nation name_text
        FROM adrp
        INTO TABLE gt_adrp
        FOR ALL ENTRIES IN gt_adcp
        WHERE persnumber = gt_adcp-persnumber AND
              nation     = gt_adcp-nation.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_processar_dados .

  SORT gt_cdpos          BY objectclas objectid changenr.
  SORT gt_cdhdr          BY objectclas objectid changenr.
  SORT gt_tstct          BY sprsl tcode.
  SORT gt_usr21          BY bname persnumber.
  SORT gt_adcp           BY addrnumber persnumber nation.
  SORT gt_adrp           BY persnumber nation.
  SORT gt_ztopensis_001  BY data_avaliacao DESCENDING hora_avaliazao DESCENDING avaliador changenr tabkey tabname.

  LOOP AT gt_cdpos INTO gs_cdpos.

    CLEAR: gs_cdhdr, gs_tstct, gs_usr21, gs_dd03m, gs_adcp, gs_adrp, gs_ztopensis_001.
    READ TABLE gt_cdhdr INTO gs_cdhdr WITH KEY objectclas = gs_cdpos-objectclas
                                               objectid   = gs_cdpos-objectid
                                               changenr   = gs_cdpos-changenr  .

    CHECK sy-subrc = 0.

    READ TABLE gt_tstct INTO gs_tstct WITH KEY tcode      = gs_cdhdr-tcode     .

    READ TABLE gt_usr21 INTO gs_usr21 WITH KEY bname      = gs_cdhdr-username  .

    READ TABLE gt_dd03m INTO gs_dd03m WITH KEY tabname    = gs_cdpos-tabname
                                               fieldname  = gs_cdpos-fname
                                               ddlanguage = gs_cdhdr-langu.

    READ TABLE gt_adcp  INTO gs_adcp  WITH KEY persnumber = gs_usr21-persnumber.

    READ TABLE gt_adrp  INTO gs_adrp  WITH KEY persnumber = gs_adcp-persnumber
                                               nation     = gs_adcp-nation  .

    READ TABLE gt_ztopensis_001 INTO gs_ztopensis_001 WITH KEY changenr = gs_cdpos-changenr
                                                               tabname  = gs_cdpos-tabname
                                                               fname    = gs_cdpos-fname.

    gs_alv-bname          = gs_usr21-bname    .
    gs_alv-name_text      = gs_adrp-name_text .
    gs_alv-department     = gs_adcp-department.
    gs_alv-tcode          = gs_cdhdr-tcode    .
    gs_alv-ttext          = gs_tstct-ttext    .
    gs_alv-udate          = gs_cdhdr-udate    .
    gs_alv-utime          = gs_cdhdr-utime    .
    gs_alv-tabkey         = gs_cdpos-tabkey   .
    gs_alv-changenr       = gs_cdpos-changenr .
    gs_alv-tabname        = gs_cdpos-tabname  .
    gs_alv-fname          = gs_cdpos-fname    .
    gs_alv-ddtext         = gs_dd03m-ddtext   .
    gs_alv-valor_old      = gs_cdpos-value_old.
    gs_alv-valor_new      = gs_cdpos-value_new.
    gs_alv-avaliador      = sy-uname.

    gs_alv-justificavel_s = gs_ztopensis_001-justificavel_s.
    gs_alv-justificavel_n = gs_ztopensis_001-justificavel_n.

    gs_alv-just_s_origem  = gs_ztopensis_001-justificavel_s.
    gs_alv-just_n_origem  = gs_ztopensis_001-justificavel_n.

    gs_alv-data_avaliacao = sy-datum.
    gs_alv-hora_avaliazao = sy-uzeit.

    APPEND gs_alv TO gt_alv.
    CLEAR  gs_alv.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_container OUTPUT.

  IF container_r1 IS INITIAL.

* create a custom container control for our ALV Control
    CREATE OBJECT container_r1
      EXPORTING
        container_name              = 'CONTAINER1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

**   Create object for ALV grid inside container
    CREATE OBJECT grid_r1
      EXPORTING
        i_parent = container_r1.

*   Call the method that receives the content and structure of itab...
    CALL METHOD grid_r1->set_table_for_first_display
      EXPORTING
        is_layout       = w_layout_1
        is_print        = w_print
        i_save          = 'X'
      CHANGING
        it_outtab       = gt_alv[]      "Table to be displayed
        it_fieldcatalog = t_fieldcat_1[].  "LVC fieldcat

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_toolbar1              FOR grid_r1.

    CALL METHOD grid_r1->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

    CALL METHOD grid_r1->set_toolbar_interactive.

    CALL METHOD grid_r1->refresh_table_display .
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  DATA: lc_valid.

  CALL METHOD grid_r1->check_changed_data.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.

      SET PARAMETER ID 'BACK' FIELD 'X'.

      PERFORM zf_verifica_se_houve_alteracao CHANGING lc_valid.

      IF lc_valid IS NOT INITIAL.
        CLEAR lc_valid.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = text-i01 "Log
            text_question         = text-i02 "Deseja realmente sair sem salvar os dados?
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '2'
            display_cancel_button = space
          IMPORTING
            answer                = lc_valid.
        CHECK lc_valid = '1'.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.

      PERFORM zf_valida_dados  CHANGING lc_valid .
      IF lc_valid IS INITIAL.
        PERFORM zf_salva_alteracao.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_formatar_alv .

  DATA: li_lines TYPE i,
        lc_lines TYPE c LENGTH 10.

  " Configura Impressão
  CLEAR w_print.
  w_print-prnt_info  = space.
  w_print-prntlstinf = 'X'.

  DESCRIBE TABLE gt_alv LINES li_lines.
*  SUBTRACT 30 FROM li_lines.
  lc_lines = li_lines.
  SHIFT lc_lines LEFT DELETING LEADING '0'.
  SHIFT lc_lines LEFT DELETING LEADING space.

  " Define layout
  CLEAR w_layout_1.
  w_layout_1-zebra       = 'X'.
***  CONCATENATE 'Cadastro / Manutenção -' lc_lines 'Registros'
***         INTO w_layout_1-grid_title SEPARATED BY space.
  CONCATENATE 'Registros encontrados:'
              lc_lines
              INTO w_layout_1-grid_title SEPARATED BY space.
  w_layout_1-sel_mode    = 'B'.
  w_layout_1-no_author   = 'X'.
  w_layout_1-no_f4       = space  .
  w_layout_1-stylefname  = 'STYLE'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZTOPENSIS_001'
    CHANGING
      ct_fieldcat      = t_fieldcat_1.

  PERFORM zf_ajustar_fieldcat TABLES t_fieldcat_1 USING:
"1-Campo         2-Elem Coltext  3-Texto da Coluna  4-Posição 5-Key 6-Tam  7-Check  8-Edit 9-No_out  10  11-Dominio
'BNAME'          ''              'Quem Executou'    '01'      ''    '10'    ''      ''     ''    'C' '',
'NAME_TEXT'      ''              'Nome Completo'    '02'      ''    '15'    ''      ''     ''    'L' '',
'DEPARTMENT'     ''              'Departamento'     '03'      ''    '15'    ''      ''     ''    'L' '',
'TCODE'          ''              'Transação'        '04'      ''    '07'    ''      ''     ''    'L' '',
'TTEXT'          ''              'Texto Transação'  '05'      ''    '12'    ''      ''     ''    'L' '',
'UDATE'          ''              'Data Alteração'   '06'      ''    '10'    ''      ''     ''    'L' '',
'UTIME'          ''              'Hora Alteração'   '07'      ''    '10'    ''      ''     ''    'L' '',
'TABKEY'         ''              'Chave Documento'  '08'      ''    '10'    ''      ''     ''    'L' '',
'CHANGENR'       ''              'Documento'        '09'      ''    '08'    ''      ''     ''    'L' '',
'TABNAME'        ''              'Tabela'           '10'      ''    '06'    ''      ''     ''    'L' '',
'FNAME'          ''              'Campo'            '11'      ''    '10'    ''      ''     ''    'L' '',
'DDTEXT'         ''              'Descrição Campo'  '12'      ''    '14'    ''      ''     ''    'L' '',
'VALOR_OLD'      ''              'Valor Antigo'     '13'      ''    '08'    ''      ''     ''    'C' '',
'VALOR_NEW'      ''              'Valor Novo'       '14'      ''    '08'    ''      ''     ''    'C' '',
'JUSTIFICAVEL_S' ''              'Justificável'     '15'      ''    '08'    'X'     'X'    ''    'C' '',
'JUSTIFICAVEL_N' ''              'Não Justificável' '16'      ''    '10'    'X'     'X'    ''    'C' '',
'AVALIADOR'      ''              'Avaliador'        '17'      ''    '10'    ''      ' '    ''    'L' '',
'DATA_AVALIACAO' ''              'Data Execução'    '18'      ''    '10'    ''      ' '    'X'   'L' '',
'HORA_AVALIAZAO' ''              'Hora Execução'    '19'      ''    '08'    ''      ' '    'X'   'L' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_AJUSTAR_FIELDCAT
*&---------------------------------------------------------------------*
FORM zf_ajustar_fieldcat TABLES  lt_fieldcat  STRUCTURE  lvc_s_fcat
                         USING   lv_campo     "1
                                 lv_elemento  "2
                                 lv_coltext   "3
                                 lv_position  "4
                                 lv_key       "5
                                 lv_tamanho   "6
                                 lv_checkbox  "7
                                 lv_edit      "8
                                 lv_no_out    "9
                                 lv_alinhado  "10
                                 lv_domname.

  READ TABLE lt_fieldcat INTO w_fieldcat WITH KEY fieldname = lv_campo.
  IF sy-subrc IS INITIAL.
    IF lv_elemento IS NOT INITIAL.
      w_fieldcat-rollname    = lv_elemento.
    ENDIF.

    IF lv_position IS NOT INITIAL.
      w_fieldcat-col_pos = lv_position.
    ENDIF.

    w_fieldcat-edit          = lv_edit     . "06-Coluna Editavel

    IF lv_tamanho IS NOT INITIAL.
      w_fieldcat-outputlen = lv_tamanho.
    ENDIF.

    IF lv_key IS NOT INITIAL.
      w_fieldcat-key = lv_key.
    ELSE.
      CLEAR w_fieldcat-key.
    ENDIF.

    w_fieldcat-coltext    = lv_coltext.
    w_fieldcat-scrtext_l  = lv_coltext.
    w_fieldcat-scrtext_m  = lv_coltext.
    w_fieldcat-scrtext_s  = lv_coltext.

    IF lv_checkbox IS NOT INITIAL.
      w_fieldcat-checkbox   = lv_checkbox.
    ENDIF.

*    IF lv_hotspot IS NOT INITIAL.
*      w_fieldcat-hotspot    = lv_hotspot.
*    ENDIF.

    w_fieldcat-no_out     = lv_no_out.
    w_fieldcat-checkbox   = lv_checkbox       .
    w_fieldcat-just       = lv_alinhado .
    w_fieldcat-domname    = lv_domname  .
    MODIFY lt_fieldcat FROM w_fieldcat INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " Z_AJUSTAR_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM zf_exibe_alv .

  CALL SCREEN '9000'.

ENDFORM.                    " ZF_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_valida_dados CHANGING pc_valid.

  DATA: lc_msg TYPE c LENGTH 60.

  LOOP AT gt_alv INTO gs_alv.

    IF gs_alv-justificavel_s IS NOT INITIAL AND
       gs_alv-justificavel_n IS NOT INITIAL.
      "Não é possivel marcar Justificavel e Não Justificavel.
      lc_msg   = text-e01.
      pc_valid = 'E'.
      EXIT.
    ELSEIF ( ( gs_alv-justificavel_s IS INITIAL AND
               gs_alv-justificavel_n IS NOT INITIAL ) OR
             ( gs_alv-justificavel_s IS NOT INITIAL AND
               gs_alv-justificavel_n IS INITIAL ) OR
             ( gs_alv-justificavel_s IS INITIAL AND
               gs_alv-just_s_origem  IS NOT INITIAL ) OR
             ( gs_alv-justificavel_n IS INITIAL AND
               gs_alv-just_n_origem  IS NOT INITIAL ) ) AND
           ( gs_alv-justificavel_s <> gs_alv-just_s_origem OR
             gs_alv-justificavel_n <> gs_alv-just_n_origem ).
      pc_valid = 'S'.
      MOVE-CORRESPONDING gs_alv TO gs_ztopensis_001.
      IF gs_ztopensis_001-justificavel_s IS NOT INITIAL.
        gs_ztopensis_001-status = icon_green_light.
      ELSEIF gs_ztopensis_001-justificavel_n IS NOT INITIAL.
        gs_ztopensis_001-status = icon_red_light.
      ELSEIF gs_ztopensis_001-justificavel_s IS INITIAL AND
             gs_ztopensis_001-justificavel_n IS INITIAL.
        gs_ztopensis_001-status = icon_yellow_light.
      ENDIF.
      APPEND gs_ztopensis_001 TO gt_ztopensis_001.
    ENDIF.

  ENDLOOP.

  IF pc_valid IS INITIAL.
    "Não existe ajuste para ser salvo.
    lc_msg   = text-e02.
    pc_valid = 'E'.
  ELSEIF pc_valid = 'S'.
    CLEAR pc_valid.
    EXIT.
  ENDIF.

  MESSAGE lc_msg TYPE 'E' DISPLAY LIKE 'I'.
  FREE: gt_ztopensis_001[].
  CLEAR gs_ztopensis_001.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVA_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_salva_alteracao .

  MODIFY ztopensis_001 FROM TABLE gt_ztopensis_001.
  COMMIT WORK AND WAIT.
  MESSAGE 'Registros salvo com sucesso' TYPE 'S'.
  SET PARAMETER ID 'BACK' FIELD 'X'.
  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONAR_EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_adicionar_excluir_botoes CHANGING e_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: ls_toolbar  TYPE stb_button.

  DEFINE del_button.
    clear ls_toolbar.
    ls_toolbar-function  = &1.
    ls_toolbar-icon      = &2.
    ls_toolbar-text      = &3.
    ls_toolbar-quickinfo = &4.
    ls_toolbar-disabled  = space.
    delete e_object->mt_toolbar where function = ls_toolbar-function.
  END-OF-DEFINITION.

  del_button '&CHECK'             space space space.
  del_button '&REFRESH'           space space space.
  del_button '&LOCAL&CUT'         space space space.
  del_button '&LOCAL&COPY'        space space space.
  del_button '&LOCAL&PASTE'       space space space.
  del_button '&LOCAL&UNDO'        space space space.
  del_button '&&SEP00'            space space space.
  del_button '&LOCAL&APPEND'      space space space.
  del_button '&LOCAL&INSERT_ROW'  space space space.
  del_button '&LOCAL&DELETE_ROW'  space space space.
  del_button '&LOCAL&COPY_ROW'    space space space.
  del_button '&&SEP02'            space space space.
  del_button '&PRINT_BACK'        space space space.
  del_button '&DETAIL'            space space space.
  del_button '&PC'                space space space.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_SE_HOUVE_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_verifica_se_houve_alteracao  CHANGING pc_valid.

  LOOP AT gt_alv INTO gs_alv.

    IF ( gs_alv-justificavel_s IS INITIAL AND
         gs_alv-just_s_origem  IS NOT INITIAL ) OR
       ( gs_alv-justificavel_n IS INITIAL AND
         gs_alv-just_n_origem  IS NOT INITIAL ) .
      pc_valid = 'X'.
      EXIT.
    ELSEIF ( gs_alv-justificavel_s IS NOT INITIAL AND
             gs_alv-just_s_origem  IS INITIAL ) OR
           ( gs_alv-justificavel_n IS NOT INITIAL AND
             gs_alv-just_n_origem  IS INITIAL ) .
      pc_valid = 'X'.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_FILTRO_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_exibir_filtro_tcode .

  DATA : vl_field TYPE dfies-fieldname,
         vl_dynp  TYPE help_info-dynprofld,
         vl_repid TYPE sy-repid.

* Monta a caixa de seleção
  vl_field = 'TCODE'.
  vl_dynp  = 'SC_TCODE-LOW'.
  vl_repid = sy-repid.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = vl_field
      dynpprog    = vl_repid
      dynpnr      = sy-dynnr
      dynprofield = vl_dynp
      value_org   = 'S'
    TABLES
      value_tab   = gt_ztopensis_002.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHER_SC_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_preencher_sc_tcode .

  CLEAR: gt_sd_opcoes_procto, sc_tcode, gs_ztopensis_002.
  FREE:  sc_tcode[].
  READ TABLE gt_sd_opcoes_procto WITH KEY selflag = 'X'.

  CHECK sy-subrc = 0.

  sc_tcode-sign   = 'I'.
  sc_tcode-option = 'EQ'.
  READ TABLE gt_ztopensis_002 INTO gs_ztopensis_002 WITH KEY zcod_risco = gt_sd_opcoes_procto-varoption(04).

  CHECK sy-subrc = 0.
  DELETE gt_ztopensis_002 WHERE zcod_risco <> gs_ztopensis_002-zcod_risco.
  LOOP AT gt_ztopensis_002 INTO gs_ztopensis_002.
    sc_tcode-low  = gs_ztopensis_002-zcod_tcode.
    APPEND sc_tcode.
  ENDLOOP.

ENDFORM.
