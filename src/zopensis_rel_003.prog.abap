************************************************************************
* Program        : ZOPENSIS_REL_003                                    *
* Transaction    : ZOPENSIS_007                                        *
* Title          : Relatório Firefighter                               *
* Developer      : Fernando Oliveira                                   *
* Date           : 16/07/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                 What                         *
* 1.00     16/07/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
*
************************************************************************
REPORT zopensis_rel_003.

*----------------------------------------------------------------------*
* Declaração de Tipos                                                  *
*----------------------------------------------------------------------*
TYPE-POOLS: sscr.

*----------------------------------------------------------------------*
* Tabelas SAP                                                          *
*----------------------------------------------------------------------*
TABLES: ztopensis_004.

*----------------------------------------------------------------------*
* Declaração de Variáveis                                              *
*----------------------------------------------------------------------*
DATA: gc_error     TYPE c,
      gv_auth      TYPE c,
      ok_code      TYPE sy-ucomm,
      gv_get_alv   TYPE c,
      gv_fieldname TYPE lvc_fname,
      gc_row_cell  TYPE i,
      gn_value     TYPE lvc_value,
      gv_htype     TYPE dd01v-datatype,
      gc_delete,
      gc_lines     TYPE c LENGTH 05,

      gn_qtd_pecas TYPE n LENGTH 05,
      gd_inicio    TYPE datum,
      gd_fim       TYPE datum,
      gc_expiracao TYPE tvarvc-low.

*----------------------------------------------------------------------*
* Declaração de Constantes                                             *
*----------------------------------------------------------------------*
DATA: cc_icon_reg_incomplete TYPE icon-id     VALUE '@02@',                    " Registro com Erro
      cc_icon_reg_warning    TYPE icon-id     VALUE '@AH@',                    " Registro com Alterta
      cc_icon_reg_ok         TYPE icon-id     VALUE '@01@',                    " Registro Ok
      cc_icon_reg_delele     TYPE icon-id     VALUE '@11@',                    " Registro de Deleção
      cc_icon_reg_change     TYPE icon-id     VALUE '@0Z@',                    " Registro de Alteração
      cc_param_atribuicao    TYPE tvarvc-low  VALUE 'ZOPENSIS_USER_ATRIBUICAO'. " Parametro permitir a atribuição do Usuário

*----------------------------------------------------------------------*
* Ranges                                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaração de Tipos                                                  *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv.
        INCLUDE STRUCTURE ztopensis_004.
TYPES: status             TYPE icon-id                       , "Status
       status_2           TYPE icon-id                       , "Status2
       ind_atribuir       TYPE icon-id,
       zcod_firefighter_2 TYPE ztopensis_004-zcod_firefighter,
       zcod_user_2        TYPE ztopensis_004-zcod_user,
       zcod_role_user_2   TYPE ztopensis_004-zcod_role_user,
       zdat_vali_inicio_2 TYPE datum,
       zdat_vali_final_2  TYPE datum,
       denominacao_1      TYPE xutext,
       ind_upd            TYPE c,
       style              TYPE lvc_t_styl                    , "Estilo - Habilitar e Desabilitar Celula
       msg                TYPE c LENGTH 70                   , "Descrição de Erros
       sel(1).                                                " for selection of records
TYPES: END OF ty_alv.

*----------------------------------------------------------------------*
* Declaração de tabelas interna                                        *
*----------------------------------------------------------------------*
DATA: t_alv                  TYPE TABLE OF ty_alv,
      t_alv_bkp              TYPE TABLE OF ty_alv,
      t_ztopensis_004_origem TYPE TABLE OF ztopensis_004,
      t_ztopensis_004        TYPE TABLE OF ztopensis_004,
      t_ztopensis_004_dele   TYPE TABLE OF ztopensis_004,
      t_agr_prof             TYPE TABLE OF agr_prof,
      t_usr02                TYPE TABLE OF usr02.

DATA t_row TYPE lvc_t_row WITH HEADER LINE.

DATA: gt_profile        TYPE STANDARD TABLE OF bapiprof,
      gt_activitygroups TYPE STANDARD TABLE OF bapiagr,
      gt_return         TYPE STANDARD TABLE OF bapiret2.

DATA: gs_activitygroups TYPE bapiagr,
      gs_return         TYPE bapiret2.

DATA: gc_atribuicao TYPE c. "Verificar se o usuário tem permissão de atribuição

*----------------------------------------------------------------------*
* Declaração de work áreas                                             *
*----------------------------------------------------------------------*
DATA: w_alv           TYPE ty_alv,
      w_ztopensis_004 TYPE ztopensis_004,
      w_agr_prof      TYPE agr_prof,
      w_usr02         TYPE usr02.

*----------------------------------------------------------------------*
* Declaração de RANGES                                                 *
*----------------------------------------------------------------------*
DATA: lr_user_range TYPE RANGE OF char50,
      ls_user_name  LIKE LINE OF lr_user_range.

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
      gt_f4 TYPE lvc_t_f4 WITH HEADER LINE..

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

      handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING et_good_cells,

      "Atualizar tabela interna
      handle_data_changed          FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4,

      "Barra de Ferramentas
      handle_toolbar1              FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object ,

      "Evento Hotspot
      on_hotspot                   FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id,

      "Double Click
      handle_double_click          FOR EVENT double_click  OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      "Overrinding Standard Functions
      handle_before_user_command   FOR EVENT before_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      "Overrinding Standard Functions
      handle_after_user_command    FOR EVENT after_user_command  OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      "Match Code Customazing
      handle_f4                    FOR EVENT onf4                OF cl_gui_alv_grid
        IMPORTING sender
                    e_fieldname
                    e_fieldvalue
                    es_row_no
                    er_event_data
                    et_bad_cells
                    e_display.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
* Implementação de classes                                             *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_data_changed.

    " Desabilitar a Tecla Delete do Teclado
    DATA: ls_deleted_rows LIKE LINE OF er_data_changed->mt_deleted_rows.
    DESCRIBE TABLE er_data_changed->mt_deleted_rows LINES sy-index.
    IF sy-index IS NOT INITIAL.
      gc_delete = 'X'.
      t_alv_bkp[] = t_alv[].
      CLEAR er_data_changed->mt_deleted_rows.
    ENDIF.
    " Desabilitar a Tecla Delete do Teclado

    DATA: lc_erro,
          lc_msg(40) TYPE c,
          lc_line    TYPE c LENGTH 04.

    CLEAR: gv_fieldname,
           gc_row_cell ,
           gn_value    .

    "Here you just trigger PAI using the control framework:
    IF gv_get_alv IS INITIAL.
      gv_get_alv = 'X'.

      CALL METHOD grid_r1->get_current_cell
        IMPORTING
          es_row_id = w_lvc_s_row
          es_col_id = w_lvc_s_col
          es_row_no = w_lvc_s_roid.

      CALL METHOD grid_r1->get_scroll_info_via_id
        IMPORTING
          es_row_no   = w_lvc_s_roid2
          es_row_info = w_lvc_s_row2
          es_col_info = w_lvc_s_col2.
    ENDIF.

    CASE sy-ucomm .
      WHEN ''.
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = 'ENTER'.
    ENDCASE.
    FREE t_modi[].
    t_modi[] = er_data_changed->mt_mod_cells[].
    DELETE t_modi WHERE NOT ( fieldname = 'ZCOD_FIREFIGHTER' OR
                              fieldname = 'ZCOD_USER'        OR
                              fieldname = 'ZCOD_ROLE_USER'   OR
                              fieldname = 'ZCOD_USER_RESPON' OR
                              fieldname = 'ZDAT_VALI_INICIO' OR
                              fieldname = 'ZDAT_VALI_FINAL' ).
    DESCRIBE TABLE t_modi.
    "Ação Individual
    IF sy-tfill = 1.
      LOOP AT t_modi INTO w_modi.
        READ TABLE er_data_changed->mt_protocol TRANSPORTING NO FIELDS
                                                WITH KEY row_id = w_modi-row_id.
        CHECK sy-subrc <> 0.
        gv_fieldname = w_modi-fieldname. "Campo
        gc_row_cell  = w_modi-row_id   . "Linha
        gn_value     = w_modi-value    . "Valor

      ENDLOOP.
      "Ação em Massa - Ctrl+V
    ELSEIF sy-tfill > 300.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = text-m01  "Erro entrada
          txt1  = text-m09  "Não é possível inserir essa quantidade de linhas
          txt2  = text-m10  "Dica: Utilizar a função Upload para dar carga em massa
          txt3  = space. "
      FREE t_modi[].
      gc_error = 'X'.
      EXIT.
    ELSE.
      DATA: li_index TYPE i.
      SORT t_modi BY row_id ASCENDING.
      CLEAR li_index.
      LOOP AT t_modi INTO w_modi.
        gv_fieldname = w_modi-fieldname. "Campo
        gc_row_cell  = w_modi-row_id   . "Linha
        gn_value     = w_modi-value    . "Valor
        LOOP AT t_modi INTO w_modi_x WHERE row_id = w_modi-row_id.
*          "Hierarquia de Produtos
*          IF gv_fieldname = 'ZCOD_DCO' OR
*             gv_fieldname = 'ZCOD_GM'.
*
*            IF gv_fieldname   = 'ZCOD_DCO'           AND
*               gn_value           <> space           AND
*               w_modi_x-fieldname = 'ZCOD_GM'.
*              CLEAR w_modi-value.
*              MODIFY t_modi FROM w_modi INDEX sy-tabix TRANSPORTING value.
*            ENDIF.
*          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "handle_data_changed

  METHOD handle_data_changed_finished.

    DATA: lt_modi      TYPE TABLE OF lvc_s_modi.
    DATA: ls_modi      TYPE lvc_s_modi.

    " Desabilitar a Tecla Delete do Teclado
    IF gc_delete = 'X'.
      CLEAR gc_delete.      t_alv[] = t_alv_bkp[].
      CLEAR   t_alv_bkp.
      REFRESH t_alv_bkp[].
      CALL METHOD grid_r1->refresh_table_display
        EXCEPTIONS
          finished = 1
          OTHERS   = 2.
    ENDIF.
    " Desabilitar a Tecla Delete do Teclado

    lt_modi[] = et_good_cells[].

    DESCRIBE TABLE t_modi.

    IF sy-tfill > 1.
      "Alteração em Massa
      LOOP AT t_modi INTO w_modi.
        READ TABLE lt_modi INTO ls_modi WITH KEY row_id    = w_modi-row_id
                                                 fieldname = w_modi-fieldname.
        CHECK sy-subrc = 0.
        w_modi-value    = ls_modi-value. "Valor
        MODIFY t_modi FROM w_modi TRANSPORTING value.
      ENDLOOP.
    ELSE.
      "Alteração Individual
      READ TABLE lt_modi INTO ls_modi WITH KEY row_id    = gc_row_cell
                                               fieldname = gv_fieldname.
      CHECK sy-subrc = 0.
      gn_value = ls_modi-value. "Valor

      IF gv_fieldname = 'ZDAT_VALI_INICIO'.
        READ TABLE t_alv INTO w_alv INDEX gc_row_cell.
        IF sy-subrc IS INITIAL.
          MODIFY t_alv FROM w_alv INDEX gc_row_cell.
        ENDIF.
      ENDIF.
      IF gv_fieldname = 'ZDAT_VALI_FINAL'.
        READ TABLE t_alv INTO w_alv INDEX gc_row_cell.
        IF sy-subrc IS INITIAL.
          MODIFY t_alv FROM w_alv INDEX gc_row_cell.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "handle_data_changed

  "Chamada da barra de ferramentas (Criação)
  METHOD handle_toolbar1.

    PERFORM zf_adicionar_excluir_botoes CHANGING e_object.

  ENDMETHOD.                    "handle_toolbar

  "Evento de Clicar na Linha do registro
  METHOD on_hotspot.

  ENDMETHOD.                    "on_link_click

  METHOD handle_double_click.

  ENDMETHOD.                    "handle_double_click

  METHOD handle_before_user_command.

    CASE e_ucomm.
      WHEN '&XXL' OR '&PC'.

        t_fieldcat_bkp[] = t_fieldcat_1[].

        LOOP AT t_fieldcat_1 INTO w_fieldcat.
          w_fieldcat-checkbox = ''.
          MODIFY t_fieldcat_1 FROM w_fieldcat INDEX sy-tabix.
        ENDLOOP.

        CALL METHOD grid_r1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcat_1.

        CALL METHOD grid_r1->refresh_table_display
          EXPORTING
            i_soft_refresh = 'X'.

    ENDCASE.

  ENDMETHOD.                    "handle_before_user_command

  METHOD handle_after_user_command.

    DATA: lt_index_rows	TYPE lvc_t_row,
          lt_row_no	    TYPE lvc_t_roid,
          wl_row_no	    LIKE LINE OF lt_row_no.

    DATA: lc_lines TYPE c LENGTH 05,
          lc_title TYPE lvc_title,
          li_lines TYPE i,
          lc_edit  TYPE c.

    CALL METHOD grid_r1->get_scroll_info_via_id
      IMPORTING
        es_row_no   = w_lvc_s_roid2
        es_row_info = w_lvc_s_row2
        es_col_info = w_lvc_s_col2.

    CASE e_ucomm.
      WHEN '&XXL' OR '&PC'.

        t_fieldcat_1[] = t_fieldcat_bkp[].

        CALL METHOD grid_r1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcat_1[].

        CALL METHOD grid_r1->refresh_table_display
          EXPORTING
            i_soft_refresh = 'X'.

      WHEN 'INSERIR'.

        DESCRIBE TABLE t_alv.
        IF sy-subrc = 0.
          w_lvc_s_roid2-row_id = sy-tfill.
          IF sy-tfill > 30.
            w_lvc_s_roid2-row_id = sy-tfill - 29.
          ENDIF.
        ENDIF.

      WHEN 'ELIMINAR'.

        CALL METHOD grid_r1->get_selected_rows
          IMPORTING
            et_index_rows = lt_index_rows
            et_row_no     = lt_row_no.

        SORT lt_row_no BY row_id DESCENDING.
        DESCRIBE TABLE lt_row_no LINES li_lines.
        CLEAR lc_edit.

        LOOP AT lt_row_no INTO wl_row_no.
          CLEAR w_alv.
          READ TABLE t_alv INTO w_alv INDEX wl_row_no-row_id.
          IF w_alv-ind_upd EQ space.
            DELETE t_alv INDEX wl_row_no-row_id.
          ELSEIF w_alv-status       = cc_icon_reg_incomplete.
            MESSAGE 'Verificar inconsistências antes de Eliminar o registro' TYPE 'I' DISPLAY LIKE 'E'.
          ELSE.
            w_alv-status_2       = cc_icon_reg_delele        . "ICON_DELETE
            w_alv-style[]        = t_edit_upd_off[]          .
            w_alv-zdat_vali_inicio = w_alv-zdat_vali_inicio_2.
            w_alv-zdat_vali_final  = w_alv-zdat_vali_final_2 .
            MODIFY t_alv FROM w_alv INDEX wl_row_no-row_id   .
          ENDIF.
          lc_edit = 'X'.
        ENDLOOP.

        PERFORM zf_adicionar_linhas USING e_ucomm.
        PERFORM zf_validar_dados CHANGING gc_error.
        IF lc_edit IS NOT INITIAL.
          "Define o texto do cabeçalho
          PERFORM zf_setar_qtd_reg_cabecalho .
          CALL METHOD grid_r1->refresh_table_display.
        ENDIF.

    ENDCASE.

    "Define o texto do cabeçalho
    CALL METHOD grid_r1->refresh_table_display.

    CALL METHOD grid_r1->set_scroll_info_via_id
      EXPORTING
        is_row_info = w_lvc_s_row2
        is_col_info = w_lvc_s_col2
        is_row_no   = w_lvc_s_roid2.

  ENDMETHOD.                    "handle_after_user_command

  METHOD handle_f4.

    PERFORM on_f4 USING e_fieldname
                        es_row_no
                        er_event_data.

  ENDMETHOD.                                                "handle_f4

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

DATA: event_receiver TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* Field-Symbol                                                         *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Parãmetros de Seleção                                                *
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
*PARAMETERS:     pc_hist  TYPE flag NO-DISPLAY.
*SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
* Inicialização                                                        *
*----------------------------------------------------------------------*
INITIALIZATION.

  SELECT *
    FROM ztopensis_004
    INTO TABLE t_ztopensis_004_origem.

  SELECT *
    FROM agr_prof
    INTO TABLE t_agr_prof
    WHERE langu = sy-langu.

  SELECT *
    FROM usr02
    INTO TABLE t_usr02.

  FREE lr_user_range[].
  SELECT sign opti low high
    FROM tvarvc
    INTO TABLE lr_user_range
    WHERE name = cc_param_atribuicao.

  IF sy-subrc = 0.

    CLEAR: ls_user_name, gc_atribuicao.
    READ TABLE lr_user_range INTO ls_user_name WITH KEY low = sy-uname.
    IF sy-subrc = 0.
      gc_atribuicao = 'X'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Start of Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_seleciona_dados.
  PERFORM zf_montar_dados   .
  PERFORM zf_formatar_alv   .
  PERFORM zf_exibe_alv      .

END-OF-SELECTION.

*----------------------------------------------------------------------*
* Performs                                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECION_DADOS
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados.



ENDFORM.                    " ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATAR_ALV
*&---------------------------------------------------------------------*
FORM zf_formatar_alv .

  DATA: li_lines TYPE i,
        lc_lines TYPE c LENGTH 10.

  " Configura Impressão
  CLEAR w_print.
  w_print-prnt_info  = space.
  w_print-prntlstinf = 'X'.

  DESCRIBE TABLE t_alv LINES li_lines.
  SUBTRACT 30 FROM li_lines.
  lc_lines = li_lines.
  SHIFT lc_lines LEFT DELETING LEADING '0'.
  SHIFT lc_lines LEFT DELETING LEADING space.

  " Define layout
  CLEAR w_layout_1.
  w_layout_1-zebra       = 'X'.
  CONCATENATE 'Cadastro / Manutenção -' lc_lines 'Registros'
         INTO w_layout_1-grid_title SEPARATED BY space..
  w_layout_1-sel_mode    = 'A'.
  w_layout_1-no_author   = 'X'.
  w_layout_1-no_f4       = space  .
  w_layout_1-stylefname  = 'STYLE'.
  w_layout_1-box_fname   = 'SEL'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZTOPENSIS_004'
    CHANGING
      ct_fieldcat      = t_fieldcat_1.

  " Status
  CLEAR  w_fieldcat.
  w_fieldcat-fieldname     = 'STATUS'.
  w_fieldcat-tabname       = '1'.
  w_fieldcat-rollname      = 'ICON_D'.
  APPEND w_fieldcat TO t_fieldcat_1.

  " Status_2
  CLEAR  w_fieldcat.
  w_fieldcat-fieldname     = 'STATUS_2'.
  w_fieldcat-tabname       = '1'.
  w_fieldcat-rollname      = 'ICON_D'.
  APPEND w_fieldcat TO t_fieldcat_1.

  " Atribuir
  CLEAR  w_fieldcat.
  w_fieldcat-fieldname     = 'IND_ATRIBUIR'.
  w_fieldcat-tabname       = '1'.
  w_fieldcat-rollname      = 'ICON_D'.
  APPEND w_fieldcat TO t_fieldcat_1.

  " Denominação
  CLEAR  w_fieldcat.
  w_fieldcat-fieldname     = 'DENOMINACAO_1'.
  w_fieldcat-tabname       = '1'.
  w_fieldcat-rollname      = ''.
  APPEND w_fieldcat TO t_fieldcat_1.

  " Msg
  CLEAR  w_fieldcat.
  w_fieldcat-fieldname     = 'MSG'.
  w_fieldcat-tabname       = '1'.
  w_fieldcat-rollname      = ''.
  APPEND w_fieldcat TO t_fieldcat_1.

  PERFORM zf_ajustar_fieldcat TABLES t_fieldcat_1 USING:
"1-Campo            2-Elem 3-Texto da Coluna            4-Posição 5-Key 6-Tam 7-Check 8-Edit 9-F4 10  11  12           13
'STATUS'            ''     'Status'                     '01'      ''    '5'   ''      ''     ''   'C' ''  ''           ''         ,
'STATUS_2'          ''     'Status'                     '02'      ''    '5'   ''      ''     ''   'C' ''  ''           ''         ,
'IND_ATRIBUIR'      ''     'Atribuido'                  '03'      ''    '6'   ''      ''     ''   'C' ''  ''           ''         ,
'ZCOD_FIREFIGHTER'  ''     'Cód. Firefighter'           '04'      ''    '12'  ''      'X'    ''   'C' ''  ''           ''         ,
'ZCOD_USER'         ''     'ID Usuário'                 '05'      ''    '12'  ''      'X'    'X'  'C' ''  'TRDYSE01CM' 'USERNAME' ,
'ZCOD_ROLE_USER'    ''     'Role / Função'              '06'      ''    '10'  ''      'X'    'X'  'C' ''  'AGR_USERS'  'AGR_NAME' ,
'DENOMINACAO_1'     ''     'Descrição Role'             '07'      ''    '20'  ''      ' '    ''   'L' ''  ''           ''         ,
'ZCOD_USER_RESPON'  ''     'Responsável Firefghter'     '08'      ''    '15'  ''      'X'    'X'  'C' ''  'TRDYSE01CM' 'USERNAME' ,
'ZDAT_VALI_INICIO'  ''     'Data Inicio'                '09'      ''    '10'  ''      'X'    'X'  'C' ''  'SYST'       'DATUM'    ,
'ZDAT_VALI_FINAL'   ''     'Data Fim'                   '10'      ''    '10'  ''      'X'    'X'  'C' ''  'SYST'       'DATUM'    ,
'ZCOD_USER_CRIACAO' ''     'Quem Cadastrou'             '11'      ''    '12'  ''      ' '    ''   'C' ''  ''           ''         ,
'MSG'               ''     'Mensagem de Erro / Alerta'  '12'      ''    '50'  ''      ''     ''   'L' ''  ''           ''         ,
'ZIND_ATRIBUIR'     ''     'Atribuir'                   '13'      ''    '01'  ''      ''     ''   'C' 'X' ''           ''         .

ENDFORM.                    " ZF_FORMATAR_ALV
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
                                 lv_f4        "9
                                 lv_alinhado  "10
                                 lv_no_out    "11
                                 lv_table
                                 lv_field.

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

    w_fieldcat-f4availabl = lv_f4       .
    w_fieldcat-just       = lv_alinhado .
    w_fieldcat-no_out     = lv_no_out.

    w_fieldcat-ref_field = lv_field.
    w_fieldcat-ref_table = lv_table.

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
*&      Form  ZF_MONTAR_DADOS
*&---------------------------------------------------------------------*
FORM zf_montar_dados .

  LOOP AT t_ztopensis_004_origem INTO w_ztopensis_004.
    CLEAR w_alv.
    MOVE-CORRESPONDING w_ztopensis_004 TO w_alv.
    w_alv-status  = cc_icon_reg_ok.
    w_alv-ind_upd = 'X'.

    IF w_alv-zind_atribuir IS NOT INITIAL.
      w_alv-ind_atribuir = icon_link.
    ENDIF.

    w_alv-zcod_firefighter_2 = w_ztopensis_004-zcod_firefighter.
    w_alv-zcod_user_2        = w_ztopensis_004-zcod_user       .
    w_alv-zcod_role_user_2   = w_ztopensis_004-zcod_role_user  .

    READ TABLE t_agr_prof INTO w_agr_prof WITH KEY agr_name = w_ztopensis_004-zcod_role_user.
    IF sy-subrc = 0.
      w_alv-denominacao_1 = w_agr_prof-ptext.
    ENDIF.

    APPEND w_alv TO t_alv.
  ENDLOOP.

  PERFORM zf_adicionar_linhas USING 'INICIO'.
  PERFORM zf_formata_alv      USING space.

ENDFORM.                    " ZF_MONTAR_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
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
        it_outtab       = t_alv[]      "Table to be displayed
        it_fieldcatalog = t_fieldcat_1[].  "LVC fieldcat

    CREATE OBJECT event_receiver.

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_data_changed          FOR grid_r1.
    SET HANDLER event_receiver->handle_data_changed_finished FOR grid_r1.
    SET HANDLER event_receiver->on_hotspot                   FOR grid_r1.
    SET HANDLER event_receiver->handle_toolbar1              FOR grid_r1.
    SET HANDLER event_receiver->handle_before_user_command   FOR grid_r1. "EXCEL
    SET HANDLER event_receiver->handle_after_user_command    FOR grid_r1. "EXCEL
    SET HANDLER event_receiver->handle_f4                    FOR grid_r1.
    SET HANDLER event_receiver->handle_double_click          FOR grid_r1.

    CALL METHOD grid_r1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid_r1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CLEAR gt_f4.
    gt_f4-fieldname = 'ZCOD_ROLE_USER'.
    gt_f4-register  = 'X'.
    INSERT TABLE gt_f4.

    CALL METHOD grid_r1->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    CALL METHOD grid_r1->set_toolbar_interactive.

    CALL METHOD grid_r1->refresh_table_display .

    grid_r1->set_selected_rows( EXPORTING it_index_rows = t_row[] ).

  ENDIF.

ENDMODULE.                 " CREATE_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  DATA: lc_valid.

  grid_r1->get_selected_rows( IMPORTING et_index_rows = t_row[] ).

  CALL METHOD grid_r1->check_changed_data.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.

      LOOP AT t_alv INTO w_alv.

        CHECK w_alv-zcod_firefighter IS NOT INITIAL AND
            ( w_alv-zcod_user        IS NOT INITIAL OR
              w_alv-zcod_role_user   IS NOT INITIAL OR
              w_alv-zdat_vali_inicio IS NOT INITIAL OR
              w_alv-zdat_vali_final  IS NOT INITIAL ).

        READ TABLE t_ztopensis_004_origem INTO w_ztopensis_004 WITH KEY zcod_firefighter = w_alv-zcod_firefighter
                                                                        zcod_user        = w_alv-zcod_user
                                                                        zcod_role_user   = w_alv-zcod_role_user.
        IF sy-subrc = 0.
          IF w_ztopensis_004-zdat_vali_inicio <> w_alv-zdat_vali_inicio OR
             w_ztopensis_004-zdat_vali_final  <> w_alv-zdat_vali_final.
            lc_valid = 'X'.
            EXIT.
          ENDIF.
        ELSE.
          lc_valid = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lc_valid = 'X'.
        CLEAR lc_valid.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = text-i01 "Confirmar edição do cadastro
            text_question         = text-i02 "Deseja realmente sair sem salvar os dados?
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '2'
            display_cancel_button = space
          IMPORTING
            answer                = lc_valid.
        CHECK lc_valid = '1'.
      ENDIF.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'ENTER'.

      CLEAR gv_get_alv.

      PERFORM zf_validar_dados CHANGING gc_error.
      PERFORM zf_adicionar_linhas USING ''.
      "Define o texto do cabeçalho
      PERFORM zf_setar_qtd_reg_cabecalho .
      CALL METHOD grid_r1->refresh_table_display.

      CALL METHOD grid_r1->set_current_cell_via_id
        EXPORTING
          is_column_id = w_lvc_s_col
          is_row_no    = w_lvc_s_roid.

      CALL METHOD grid_r1->set_scroll_info_via_id
        EXPORTING
          is_row_info = w_lvc_s_row2
          is_col_info = w_lvc_s_col2
          is_row_no   = w_lvc_s_roid2.
*
    WHEN 'SAVE'.
      PERFORM zf_validar_dados CHANGING gc_error.
      PERFORM zf_salvar_dados  USING sy-ucomm.
      CALL METHOD grid_r1->refresh_table_display.

    WHEN 'ATRIBUIR' OR 'ATRIBUIR_N'.

      IF gc_atribuicao IS INITIAL.
        MESSAGE 'Usuário sem permissão para atribuir função para os usuários.' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      DESCRIBE TABLE t_row.
      IF sy-tfill > 1.
        MESSAGE 'Marcar apenas uma linha por vez' TYPE 'I' DISPLAY LIKE 'E'.
      ELSEIF sy-tfill = 0.
        MESSAGE 'Marcar uma linha para atribuir a função para o usuário.' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        CLEAR gc_error.
        PERFORM zf_atribuir_role USING ok_code.
        IF gc_error = space.
          PERFORM zf_salvar_dados USING sy-ucomm.
        ENDIF.
        CALL METHOD grid_r1->refresh_table_display.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONAR_EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
FORM zf_adicionar_excluir_botoes  CHANGING e_object TYPE REF TO cl_alv_event_toolbar_set..
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

  DEFINE add_button.
    ls_toolbar-function  = &1.
    ls_toolbar-icon      = &2.
    ls_toolbar-text      = &3.
    ls_toolbar-quickinfo = &4.
    ls_toolbar-disabled  = space.
    ls_toolbar-butn_type = &5.

    append ls_toolbar to e_object->mt_toolbar.

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

  add_button 'INSERIR'            icon_insert_row   space    space space.
  add_button 'ELIMINAR'           icon_delete_row   space    space space.
*  add_button 'DESFAZER'           icon_system_undo  space    space space.

ENDFORM.                    " ZF_ADICIONAR_EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONAR_LINHAS
*&---------------------------------------------------------------------*
FORM zf_adicionar_linhas USING p_ucomm.

  DATA: li_lines TYPE i,
        lc_lines TYPE c LENGTH 10.

  DATA: li_sujo  TYPE i,
        li_limpo TYPE i,
        li_add   TYPE i.
  DATA: lt_modi      TYPE TABLE OF lvc_s_modi.

  IF p_ucomm NE 'INICIO'.
    CALL METHOD grid_r1->check_changed_data.
  ENDIF.

  IF p_ucomm = 'INSERIR'.
    li_add = 1.
  ELSE.

    CLEAR: li_add.
    LOOP AT t_alv INTO w_alv.
*      IF NOT ( w_alv-zcod_loja = space AND
*               w_alv-zcod_gc   = space AND
*               w_alv-zcod_dco  = space ).
*
*        ADD 1 TO li_sujo.
*      ELSE.
*        ADD 1 TO li_limpo.
*      ENDIF.
    ENDLOOP.
    li_add = 30 - li_limpo.
  ENDIF.

  DO li_add TIMES.
    CLEAR w_alv.
    APPEND w_alv TO t_alv.
  ENDDO.

  DESCRIBE TABLE t_alv LINES li_lines.
  lc_lines = li_lines.
  SHIFT lc_lines LEFT DELETING LEADING '0'.
  SHIFT lc_lines LEFT DELETING LEADING space.

  " Define layout
  CLEAR w_layout_1.
  w_layout_1-zebra       = 'X'.
  CONCATENATE 'Upload -' lc_lines 'Registros'
         INTO w_layout_1-grid_title SEPARATED BY space..
  w_layout_1-sel_mode    = 'A'. " Column & Row Selections
  w_layout_1-no_author   = 'X'.
  w_layout_1-no_f4       = space   .
  w_layout_1-stylefname  = 'STYLE'.
*  w_layout_1-no_rowmark  = 'X'.

ENDFORM.                    " ZF_ADICIONAR_LINHAS
*  &---------------------------------------------------------------------*
*  &      Form  ZF_FORMATA_ALV
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
FORM zf_formata_alv USING p_index TYPE i.

  DATA: ls_edit  TYPE lvc_s_styl,
        lt_edit  TYPE lvc_t_styl,
        lv_tabix TYPE sy-tabix.

  DATA: lc_style     TYPE raw4.

  lc_style = cl_gui_alv_grid=>mc_style_enabled .

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZCOD_FIREFIGHTER'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 3   .
  APPEND ls_edit TO lt_edit.

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZCOD_ROLE_USER'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 05.
  APPEND ls_edit TO lt_edit.

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZCOD_USER'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 10   .
  APPEND ls_edit TO lt_edit.

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZCOD_USER_CRIACAO'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 4   .
  APPEND ls_edit TO lt_edit.

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZCOD_USER_RESPON'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 4   .
  APPEND ls_edit TO lt_edit.

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZDAT_VALI_FINAL'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 10.
  APPEND ls_edit TO lt_edit.

  CLEAR ls_edit.
  ls_edit-fieldname = 'ZDAT_VALI_INICIO'.
  ls_edit-style     = lc_style.
  ls_edit-style2    = space.
  ls_edit-style3    = space.
  ls_edit-style4    = space.
  ls_edit-maxlen    = 10   .
  APPEND ls_edit TO lt_edit.

  IF p_index = space.

    "Desabilitar Celulas
    t_edit_upd_off[] = lt_edit[].
    LOOP AT t_edit_upd_off INTO ls_edit.
      ls_edit-style     = cl_gui_alv_grid=>mc_style_disabled.
      MODIFY t_edit_upd_off FROM ls_edit INDEX sy-tabix TRANSPORTING style.
    ENDLOOP.

    "Habilitar Celulas
    t_edit_upd_on[] = lt_edit[].
    LOOP AT t_edit_upd_on INTO ls_edit.
      ls_edit-style     = cl_gui_alv_grid=>mc_style_enabled.
      MODIFY t_edit_upd_on FROM ls_edit INDEX sy-tabix TRANSPORTING style.
    ENDLOOP.
  ELSE.
*    w_alv-style[] = lt_edit[].
*    MODIFY t_alv FROM w_alv INDEX p_index.
  ENDIF.

ENDFORM.                    " ZF_FORMATA_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDAR_DADOS
*&---------------------------------------------------------------------*
* Icones
*
* 1 - @11@ - Lixeira - Deletar
* 2 - @01@ - Ok      - Registro Antigo
* 3 - @0Z@ - Lapis   - Editar
* 4 - @02@ - Erro    - Registro com Erro - Duplicado... Data... Etc...
* 5 - @0Y@ - Pasta   - Registro Novo
*&---------------------------------------------------------------------*
FORM zf_validar_dados CHANGING pc_error.

  DATA: lv_tabix TYPE sy-tabix,
        li_inio  TYPE i,
        li_fnal  TYPE i.

  DATA: ls_alv           TYPE ty_alv.

  CLEAR pc_error.

  LOOP AT t_alv INTO w_alv.

    CLEAR: lv_tabix.

    lv_tabix = sy-tabix.

    "Se o Icone não for de deleção / Criação, preencher o Icone neste Perform
    IF w_alv-status <> cc_icon_reg_delele AND
       w_alv-status <> cc_icon_reg_ok.
      CLEAR: w_alv-status, w_alv-msg.
    ENDIF.

    "Lógica para alteração de um Registro já existente na Tabela
    CLEAR w_ztopensis_004.
    READ TABLE t_ztopensis_004_origem INTO w_ztopensis_004 WITH KEY zcod_firefighter = w_alv-zcod_firefighter
                                                                    zcod_user        = w_alv-zcod_user
                                                                    zcod_role_user   = w_alv-zcod_role_user .
    IF sy-subrc = 0.
      IF w_alv-ind_upd IS NOT INITIAL.
        IF w_alv-status_2  NE cc_icon_reg_delele. " Deletar
          CLEAR w_ztopensis_004.
          READ TABLE t_ztopensis_004_origem INTO w_ztopensis_004 WITH KEY zcod_firefighter = w_alv-zcod_firefighter
                                                                          zcod_user        = w_alv-zcod_user
                                                                          zcod_role_user   = w_alv-zcod_role_user .

          IF sy-subrc = 0 AND
           ( w_ztopensis_004-zdat_vali_inicio = w_alv-zdat_vali_inicio AND
             w_ztopensis_004-zdat_vali_final  = w_alv-zdat_vali_final ).
            CLEAR w_alv-msg.
            CLEAR w_alv-status_2.
            w_alv-status    = cc_icon_reg_ok.
            MODIFY t_alv FROM w_alv.
            CONTINUE.
          ELSEIF sy-subrc <> 0 OR
                 ( w_ztopensis_004-zdat_vali_inicio = w_alv-zdat_vali_inicio AND
                   w_ztopensis_004-zdat_vali_final  = w_alv-zdat_vali_final ).
            w_alv-status           = cc_icon_reg_ok                  .
            w_alv-zcod_firefighter = w_alv-zcod_firefighter_2.
            w_alv-zcod_user        = w_alv-zcod_user_2       .
            w_alv-zcod_role_user   = w_alv-zcod_role_user_2  .
            CLEAR w_alv-status_2.
            MODIFY t_alv FROM w_alv.
            MESSAGE 'Não é permitido alterar as chaves. Caso seja necessário, crie novo cadastro ou exclua o cadastro atual.' TYPE 'I' DISPLAY LIKE 'E'.
            pc_error = 'X'.
            EXIT.
          ELSEIF w_alv-status_2 NE cc_icon_reg_delele.
            w_alv-status_2  = cc_icon_reg_change.   " Editar
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSE.
        w_alv-msg     = 'Registro Duplicado'.
        w_alv-status  = cc_icon_reg_incomplete.
        CLEAR w_alv-status_2.
        MODIFY t_alv FROM w_alv.
      ENDIF.
    ENDIF.

*--------------------------------------------*
* Validação Cód. Firefighter
*--------------------------------------------*



*--------------------------------------------*
* Validação ID. Usuário
*--------------------------------------------*
    IF w_alv-zcod_user IS NOT INITIAL.
      READ TABLE t_usr02 INTO w_usr02 WITH KEY bname = w_alv-zcod_user.
      IF sy-subrc IS INITIAL.
*        w_alv-denominacao_1 = w_agr_prof-ptext.
      ELSE.
        w_alv-msg     = 'ID. Usuário Inválido'.
        w_alv-status  = cc_icon_reg_incomplete.
        CLEAR w_alv-status_2.
        MODIFY t_alv FROM w_alv.
        CONTINUE.
      ENDIF.
    ENDIF.

*--------------------------------------------*
* Validação ROLE
*--------------------------------------------*
    IF w_alv-zcod_role_user IS NOT INITIAL.
      READ TABLE t_agr_prof INTO w_agr_prof WITH KEY agr_name = w_alv-zcod_role_user.
      IF sy-subrc IS INITIAL.
        w_alv-denominacao_1 = w_agr_prof-ptext.
      ELSE.
        w_alv-msg     = 'Função Inválida'.
        w_alv-status  = cc_icon_reg_incomplete.
        CLEAR w_alv-status_2.
        MODIFY t_alv FROM w_alv.
        CONTINUE.
      ENDIF.
    ELSE.
      CLEAR w_alv-denominacao_1.
    ENDIF.

*--------------------------------------------*
* Responsável Firefighter
*--------------------------------------------*
    IF w_alv-zcod_user_respon IS NOT INITIAL.
      READ TABLE t_usr02 INTO w_usr02 WITH KEY bname = w_alv-zcod_user_respon.
      IF sy-subrc IS INITIAL.
*        w_alv-denominacao_1 = w_agr_prof-ptext.
      ELSE.
        w_alv-msg     = 'ID. Usuário Responsável Inválido'.
        w_alv-status  = cc_icon_reg_incomplete.
        CLEAR w_alv-status_2.
        MODIFY t_alv FROM w_alv.
        CONTINUE.
      ENDIF.
*    ELSE.
*      CLEAR w_alv-denominacao_1.
    ENDIF.

*--------------------------------------------*
* Data Validade Início
*--------------------------------------------*
*--------------------------------------------*
* Data Validade Final
*--------------------------------------------*
    IF w_alv-zcod_firefighter IS INITIAL AND
       w_alv-zcod_user        IS INITIAL AND
       w_alv-zcod_role_user   IS INITIAL AND
       w_alv-zcod_user_respon IS INITIAL AND
       w_alv-zdat_vali_inicio IS INITIAL AND
       w_alv-zdat_vali_final  IS INITIAL .
      CLEAR w_alv.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF ( w_alv-zcod_firefighter IS INITIAL OR
             w_alv-zcod_user        IS INITIAL OR
             w_alv-zcod_role_user   IS INITIAL ) AND
         ( w_alv-zcod_user_respon IS NOT INITIAL OR
           w_alv-zdat_vali_inicio IS NOT INITIAL OR
           w_alv-zdat_vali_final  IS NOT INITIAL ).
      w_alv-msg     = 'Preencher Chave - Código Firefigter / ID Usúario / Role-Função'.
      w_alv-status  = cc_icon_reg_incomplete.
      CLEAR w_alv-status_2.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zcod_firefighter IS NOT INITIAL AND
           w_alv-zcod_user        IS INITIAL AND
           w_alv-zcod_role_user   IS INITIAL.
      w_alv-msg     = 'Preencher ID Usuário'.
      w_alv-status  = cc_icon_reg_incomplete.
      CLEAR w_alv-status_2.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zcod_firefighter IS NOT INITIAL AND
           w_alv-zcod_user        IS NOT INITIAL AND
           w_alv-zcod_role_user   IS INITIAL.
      w_alv-msg     = 'Preencher Role'.
      w_alv-status  = cc_icon_reg_incomplete.
      CLEAR w_alv-status_2.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zcod_firefighter IS NOT INITIAL AND
           w_alv-zcod_user        IS INITIAL AND
           w_alv-zcod_role_user   IS NOT INITIAL.
      w_alv-msg     = 'Preencher ID Usuário'.
      w_alv-status  = cc_icon_reg_incomplete.
      CLEAR w_alv-status_2.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zcod_firefighter IS INITIAL AND
           w_alv-zcod_user        IS NOT INITIAL AND
           w_alv-zcod_role_user   IS NOT INITIAL.
      w_alv-msg     = 'Preencher Cód. Firefighter'.
      w_alv-status  = cc_icon_reg_incomplete.
      CLEAR w_alv-status_2.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ENDIF.

    "Validação de Datas
    li_inio = w_alv-zdat_vali_inicio - sy-datum.
    li_fnal = w_alv-zdat_vali_final  - sy-datum.
    IF w_alv-zdat_vali_inicio IS INITIAL AND
       w_alv-zdat_vali_final  IS INITIAL .
      w_alv-msg     = 'Preencher os campos de Data Obrigatório'.
      w_alv-status  = cc_icon_reg_incomplete.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zdat_vali_inicio IS INITIAL.
      w_alv-msg     = 'Preencher o campo Data de Inicio Validade'.
      w_alv-status  = cc_icon_reg_incomplete.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF ( w_alv-zdat_vali_inicio < sy-datum AND
             w_alv-zdat_vali_inicio_2 IS INITIAL ) OR
           ( w_alv-zdat_vali_inicio <> w_alv-zdat_vali_inicio_2 AND
             w_alv-zdat_vali_inicio < sy-datum  ).
      w_alv-msg     =  'Não é permitido data de Inicio da Validade retroativa'.
      w_alv-status  = cc_icon_reg_incomplete.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF ( w_alv-zdat_vali_final <= sy-datum AND
             w_alv-zdat_vali_final_2 IS INITIAL AND
             w_alv-zdat_vali_final   IS NOT INITIAL ) OR
           ( w_alv-zdat_vali_final <> w_alv-zdat_vali_final_2 AND
             w_alv-zdat_vali_final < sy-datum  ).
      w_alv-msg     =  'Não é permitido data Final da Validade retroativa'.
      w_alv-status  = cc_icon_reg_incomplete.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zdat_vali_final IS INITIAL.
      w_alv-msg     = 'Preencher o campo Data Validade Final'.
      w_alv-status  = cc_icon_reg_incomplete.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF w_alv-zdat_vali_final < w_alv-zdat_vali_inicio.
      w_alv-msg     =  'Não é permitido data final Validade menor que a data inicial validade'.
      w_alv-status  = cc_icon_reg_incomplete.
      MODIFY t_alv FROM w_alv.
      CONTINUE.
    ELSEIF li_inio > '364' AND
           li_fnal > '364' .
      w_alv-msg     =  'Alerta - Datas muito distantes (> 12 meses)'.
      w_alv-status  = cc_icon_reg_warning.
    ELSEIF li_inio > '364' .
      w_alv-msg     =  'Alerta - Data de Inicio da validade muito distantes (> 12 meses)'.
      w_alv-status  = cc_icon_reg_warning.
    ELSEIF li_fnal > '364' .
      w_alv-msg     =  'Alerta - Data Final da validade muito distantes (> 12 meses)'.
      w_alv-status  = cc_icon_reg_warning.
    ENDIF.

    "Registro Novo
    IF w_alv-status IS INITIAL.
      w_alv-status = cc_icon_reg_ok.
    ENDIF.

    CLEAR lc_valid.
    CLEAR ls_alv  .
    LOOP AT t_alv INTO ls_alv WHERE zcod_firefighter = w_alv-zcod_firefighter
                                AND zcod_user        = w_alv-zcod_user
                                AND zcod_role_user   = w_alv-zcod_role_user.
      IF sy-tabix NE lv_tabix.
        READ TABLE t_alv INTO ls_alv WITH KEY zcod_firefighter_2 = w_alv-zcod_firefighter
                                              zcod_user_2        = w_alv-zcod_user
                                              zcod_role_user_2   = w_alv-zcod_role_user.
        IF sy-subrc = 0.
          ls_alv-msg     = 'Registro duplicado'.
          MODIFY t_alv FROM ls_alv TRANSPORTING msg.
          lc_valid = 'X'.
        ELSE.
          ls_alv-status  = cc_icon_reg_incomplete.
          ls_alv-msg     = 'Registro duplicado'.
          MODIFY t_alv FROM ls_alv TRANSPORTING msg status.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lc_valid IS NOT INITIAL.
      w_alv-status  = cc_icon_reg_incomplete.
    ENDIF.

    MODIFY t_alv FROM w_alv.
  ENDLOOP.

ENDFORM.                    " ZF_VALIDAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVAR_DADOS
*&---------------------------------------------------------------------*
FORM zf_salvar_dados USING p_ucomm.

  DATA: li_index TYPE i.

  IF p_ucomm = 'ATRIBUIR'.

    CLEAR t_row.
    READ TABLE t_row INDEX 1.
    READ TABLE t_alv INTO w_alv INDEX t_row-index.

    "Verificar se houve alguma Inconsistência no Cadastro
    IF w_alv-status = cc_icon_reg_incomplete. " Erro Registro Novo
      MESSAGE 'Verificar inconsistências antes de salvar' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    "Verificar se houve alguma Inconsistência no Cadastro
    READ TABLE t_alv INTO w_alv WITH KEY status = cc_icon_reg_incomplete. " Erro Registro Novo
    IF sy-subrc = 0.
      MESSAGE 'Verificar inconsistências antes de salvar' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  FREE: t_ztopensis_004[].
  LOOP AT t_alv INTO w_alv.

    li_index = sy-tabix.

    IF p_ucomm CS 'ATRIBUIR'.
      CHECK sy-tabix = t_row-index.
    ENDIF.

    MOVE-CORRESPONDING w_alv TO w_ztopensis_004.

    CHECK  w_ztopensis_004-zcod_firefighter IS NOT INITIAL AND
           w_ztopensis_004-zcod_user        IS NOT INITIAL AND
           w_ztopensis_004-zcod_role_user   IS NOT INITIAL .

    " Marcados para Eliminação
    IF w_alv-status_2 = cc_icon_reg_delele.
      APPEND w_ztopensis_004 TO t_ztopensis_004_dele.
      DELETE t_alv INDEX li_index.
    ENDIF.

    IF p_ucomm CS 'ATRIBUIR' .
      IF p_ucomm = 'ATRIBUIR'.
        w_alv-ind_atribuir            = icon_link.
        w_ztopensis_004-zind_atribuir = 'X'      .
      ELSE.
        CLEAR: w_alv-ind_atribuir, w_ztopensis_004-zind_atribuir.
      ENDIF.
    ENDIF.

    " Marcado para Alteração
    IF w_alv-status_2 <> cc_icon_reg_delele.
      IF w_alv-status_2 = cc_icon_reg_ok     OR
         w_alv-status_2 = cc_icon_reg_change OR
         w_alv-status   = cc_icon_reg_ok.
        w_alv-zcod_firefighter_2 = w_alv-zcod_firefighter.
        w_alv-zcod_user_2        = w_alv-zcod_user       .
        w_alv-zcod_role_user_2   = w_alv-zcod_role_user  .
        w_alv-zdat_vali_inicio_2 = w_alv-zdat_vali_inicio.
        w_alv-zdat_vali_final_2  = w_alv-zdat_vali_final .
        w_alv-status             = cc_icon_reg_ok        .
        w_alv-ind_upd            = 'X'                   .
        CLEAR w_alv-status_2.
        MODIFY t_alv FROM w_alv INDEX li_index.
      ENDIF.
    ENDIF.

    w_ztopensis_004-zcod_user_criacao = sy-uname.
    APPEND w_ztopensis_004 TO t_ztopensis_004.

  ENDLOOP.

  IF t_ztopensis_004[] IS NOT INITIAL.
    MODIFY ztopensis_004 FROM TABLE t_ztopensis_004.
    COMMIT WORK.
    IF p_ucomm <> 'ATRIBUIR'.
      MESSAGE 'Registros atualizados com sucesso' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF t_ztopensis_004_dele[] IS NOT INITIAL.
    DELETE ztopensis_004 FROM TABLE t_ztopensis_004_dele.
    COMMIT WORK.
    IF p_ucomm <> 'ATRIBUIR'.
      MESSAGE 'Registros atualizados com sucesso' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF t_ztopensis_004[]      IS NOT INITIAL AND
     t_ztopensis_004_dele[] IS NOT INITIAL.
    MESSAGE 'Nenhum dado atualizado' TYPE 'S'.
  ENDIF.

  IF t_ztopensis_004[]      IS NOT INITIAL OR
     t_ztopensis_004_dele[] IS NOT INITIAL.
    FREE: t_ztopensis_004_origem[].
    SELECT *
      FROM ztopensis_004
      INTO TABLE t_ztopensis_004_origem.
  ENDIF.

  CALL METHOD grid_r1->refresh_table_display.

ENDFORM.                    " ZF_SALVAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_SETAR_QTD_REG_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_setar_qtd_reg_cabecalho .

  DATA: lc_title  TYPE lvc_title.
  DATA: lt_alv    TYPE TABLE OF ty_alv       WITH HEADER LINE.

  FREE: lt_alv[].
  lt_alv[] = t_alv[].
*  DELETE lt_alv WHERE zcod_loja      IS INITIAL AND
*                      zcod_gc        IS INITIAL AND
*                      zcod_dco       IS INITIAL AND
*                      zcod_gm        IS INITIAL AND
*                      zdat_inio_rrto IS INITIAL AND
*                      zdat_fnal_rrto IS INITIAL.
  DESCRIBE TABLE lt_alv LINES gc_lines.
  CONDENSE gc_lines NO-GAPS.
  CONCATENATE 'Cadastro / Manutenção -'
              gc_lines
              'Registros'
              INTO lc_title
              SEPARATED BY ' '.

  CALL METHOD grid_r1->set_gridtitle
    EXPORTING
      i_gridtitle = lc_title.

ENDFORM.                    " ZF_SETAR_QTD_REG_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_ROLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_role USING p_ok_code.

  DATA li_index TYPE i.

  READ TABLE t_row INDEX 1.
  READ TABLE t_alv INTO w_alv INDEX t_row-index.

  CHECK sy-subrc = 0.

  IF p_ok_code = 'ATRIBUIR_N' AND
         w_alv-ind_atribuir IS INITIAL.
    MESSAGE 'Não existe atribuição para ser retirada.' TYPE 'I' DISPLAY LIKE 'E'.
    gc_error = 'X'.
    EXIT.
  ENDIF.

  "Verificar se houve alguma Inconsistência no Cadastro
  IF w_alv-status <> cc_icon_reg_incomplete. " Erro Registro Novo

    IF w_alv-status_2 <> cc_icon_reg_delele.
      PERFORM zf_dados_usuario USING w_alv-zcod_user
                               CHANGING gc_error.
      IF gc_error IS INITIAL.
        PERFORM zf_updt_role_user USING w_alv-zcod_user
                                        w_alv-zcod_role_user
                                        w_alv-zdat_vali_inicio
                                        w_alv-zdat_vali_final
                                        p_ok_code
                               CHANGING gc_error.
      ENDIF.
    ELSE.
      MESSAGE 'Não pode atribuir uma função para um registro marcado para eliminar' TYPE 'I' DISPLAY LIKE 'E'.
      gc_error = 'X'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Verificar inconsistências' TYPE 'I' DISPLAY LIKE 'E'.
    gc_error = 'X'.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zF_DADOS_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_dados_usuario USING pc_user
                      CHANGING pc_error.

  REFRESH: gt_profile,
           gt_return.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username       = pc_user
    TABLES
      profiles       = gt_profile
      activitygroups = gt_activitygroups
      return         = gt_return.

  IF sy-subrc EQ 0.
    SORT gt_profile BY bapiprof.
    SORT gt_activitygroups BY agr_name.
  ELSE.
    MESSAGE 'Erro ao acessar dados do usuário' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CLEAR gs_return.
  READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    pc_error = 'X'.
    IF gs_return-number = 495.
      MESSAGE 'Sem permissão para a atribuição de Função ao usuário' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE gs_return-message TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_DADOS_USUARIO
*&---------------------------------------------------------------------*
*&      Form  ZF_ADD_ROLE_USER
*&---------------------------------------------------------------------*
*       BAPI_USER_ACTGROUPS_DELETE
*       BAPI_USER_ACTGROUPS_ASSIGN
*----------------------------------------------------------------------*
FORM zf_updt_role_user USING pc_user
                            pc_func
                            pc_inci
                            pc_fim
                            p_ok_code
                      CHANGING gc_error.

  IF p_ok_code = 'ATRIBUIR'.
    gs_activitygroups-agr_name = pc_func.
    gs_activitygroups-from_dat = pc_inci.
    gs_activitygroups-to_dat   = pc_fim .
    APPEND gs_activitygroups TO gt_activitygroups.
  ELSE.
    READ TABLE gt_activitygroups INTO gs_activitygroups WITH KEY agr_name = pc_func
                                                                 from_dat = pc_inci
                                                                 to_dat   = pc_fim .
    IF sy-subrc = 0.
      DELETE gt_activitygroups INDEX sy-tabix.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
    EXPORTING
      username       = pc_user
    TABLES
      activitygroups = gt_activitygroups
      return         = gt_return.

  CLEAR gs_return.
  READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    MESSAGE gs_return-message TYPE 'S' DISPLAY LIKE 'E'.
    gc_error = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MESSAGE gs_return-message TYPE 'S'.
  ENDIF.

ENDFORM.                    " ZF_ADD_ROLE_USER
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM on_f4  USING p_e_fieldname   TYPE lvc_fname
                  p_es_row_no     TYPE lvc_s_roid
                  p_er_event_data TYPE REF TO cl_alv_event_data.

*  "Types
*  TYPES: BEGIN OF ly_role,
*           zcod_role_user TYPE ztopensis_005-zcod_role_user,
*         END OF ly_role.
*
*  "Local Vars
*  DATA: lt_role   TYPE TABLE OF ly_role,
*        ls_role   TYPE ly_role,
*        lt_map    TYPE TABLE OF dselc,
*        ls_map    TYPE dselc,
*        lt_return TYPE TABLE OF ddshretval,
*        ls_return TYPE ddshretval,
*        ls_stable TYPE lvc_s_stbl.
*
*  FIELD-SYMBOLS: <l_alv> TYPE ty_alv. " ALV table line
*
*  BREAK-POINT.
*
*  READ TABLE t_alv ASSIGNING <l_alv> INDEX p_es_row_no-row_id.
*
*  CASE p_e_fieldname.
*    WHEN 'ZCOD_ROLE_USER'.
*
*      SELECT zcod_role_user
*        FROM ztopensis_005
*        INTO TABLE lt_role.
*      IF sy-subrc = 0.
*        sort lt_role.
*        delete ADJACENT DUPLICATES FROM lt_role.
*      ELSE.
*        MESSAGE 'Não existe Role caadastrada' TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*
*      CLEAR ls_map.
*      ls_map-fldname   = 'F0001'.
*      ls_map-dyfldname = 'ZCOD_ROLE_USER'.
*      APPEND ls_map TO lt_map.
*
*      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*        EXPORTING
*          retfield        = 'ZCOD_ROLE_USER'
*          value_org       = 'S'
*        TABLES
*          value_tab       = lt_role
*          dynpfld_mapping = lt_map
*          return_tab      = lt_return
*        EXCEPTIONS
*          parameter_error = 1
*          no_values_found = 2
*          OTHERS          = 3.
*    WHEN OTHERS.
*  ENDCASE.

ENDFORM.
