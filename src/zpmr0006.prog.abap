*&---------------------------------------------------------------------*
*& Report  ZPMR0006
*& Tabela Classe Fabricante x Modelo
*&---------------------------------------------------------------------*
*& Autor:    Marcos Faneli
*& Analista: Cleudo Ferreira
*& Data:     29.09.2014
*&---------------------------------------------------------------------*

REPORT  zpmr0006.

*  Variaveis
*----------------------------------------------------------------------*

DATA vg_edit.
DATA vg_ucomm TYPE sy-ucomm.
*  Tables
*----------------------------------------------------------------------*
TABLES: equi, zpmr0001, itob.

TYPE-POOLS: slis.

* Estruturas
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_001.
         INCLUDE TYPE zpmr0001.
TYPES: END OF ty_001.

TYPES: BEGIN OF ty_t370k_t,
         eqart TYPE v_equi-eqart,
         eartx TYPE t370k_t-eartx,
       END OF ty_t370k_t,

       BEGIN OF ty_equi_depen,
         equnr TYPE equi-equnr,
         eqktx TYPE v_equi-eqktx,
         swerk TYPE v_equi-swerk,
         datbi TYPE v_equi-datbi,
       END OF ty_equi_depen.

TYPES: BEGIN OF ty_saida,
         eartx   TYPE t370k_t-eartx,
         data    TYPE c LENGTH 10,
         hora    TYPE c LENGTH 10,
         dt_inic TYPE c LENGTH 10.
         INCLUDE TYPE zpmr0001.
TYPES: END OF ty_saida.
* Tabelas internas
*----------------------------------------------------------------------*
DATA: it_001        TYPE TABLE OF ty_001,
      gt_equi_depen TYPE TABLE OF ty_equi_depen,
      it_saida      TYPE TABLE OF ty_saida,
      it_t370       TYPE TABLE OF ty_t370k_t.

* Work Áreas
*----------------------------------------------------------------------*
DATA: wa_001   TYPE ty_001,
      wa_t370  TYPE ty_t370k_t,
      wa_saida TYPE ty_saida.

* ALV
*----------------------------------------------------------------------*
DATA: obj_grid         TYPE REF TO cl_gui_alv_grid,
      obj_container    TYPE REF TO cl_gui_custom_container,

      it_fieldcatalog  TYPE lvc_t_fcat,
      wa_fieldcatalog  TYPE lvc_s_fcat,

      it_function      TYPE ui_functions,
      wa_function      LIKE LINE OF it_function,

      wa_layout        TYPE lvc_s_layo,
      wa_stable        TYPE lvc_s_stbl,

      it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

* Parâmetros de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK bloco01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_herts FOR equi-herst,
                  s_typbz FOR equi-typbz,
                  s_class FOR zpmr0001-class_oper,
                  s_consu FOR zpmr0001-consumo,
                  s_groes FOR zpmr0001-groes,
**  Begin of " Bug   #102916  FF  28.02.2023
                  s_rbnr  FOR itob-rbnr. "Perfil do catálogo
** End of FF  28.02.2023
SELECTION-SCREEN: END OF BLOCK bloco01.
"Inicio Marcio Miguel 24.01.2023 14:03:28  OS142074

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_herts-low.

  PERFORM f_fabricante CHANGING s_herts-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_herts-high.

  PERFORM f_fabricante CHANGING s_herts-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_typbz-low.

  PERFORM f_modelo CHANGING s_typbz-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_typbz-high.

  PERFORM f_modelo CHANGING s_typbz-high.

START-OF-SELECTION.
  PERFORM: f_seleciona_dados,
           f_organizar_dados.

  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
  SELECT *
    FROM zpmr0001
    INTO CORRESPONDING FIELDS OF TABLE it_001
    WHERE herst       IN s_herts
     AND  typbz	      IN s_typbz
     AND  class_oper  IN s_class
     AND  consumo     IN s_consu
     AND  groes       IN s_groes
     AND  rbnr        IN s_rbnr.

  IF sy-subrc IS INITIAL.
    SELECT *
    FROM t370k_t
    INTO CORRESPONDING FIELDS OF TABLE it_t370
    FOR ALL ENTRIES IN it_001
    WHERE eqart = it_001-class_oper
    AND  spras = sy-langu.
  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organizar_dados .
  REFRESH it_saida.

  LOOP AT it_001 INTO wa_001.
    CLEAR wa_saida.
    wa_saida-id_fab_mod = wa_001-id_fab_mod.
    wa_saida-typbz      = wa_001-typbz.
    wa_saida-class_oper = wa_001-class_oper.
    wa_saida-consumo    = wa_001-consumo.
    wa_saida-herst      = wa_001-herst.
    wa_saida-class_oper = wa_001-class_oper.
    wa_saida-groes      = wa_001-groes.
    wa_saida-dt_inic    = wa_001-dt_criacao.
    CONCATENATE wa_001-variacao '%' INTO wa_saida-variacao SEPARATED BY space.

    wa_saida-dt_inic = |{ wa_saida-dt_inic+6(2) }.{ wa_saida-dt_inic+4(2) }.{ wa_saida-dt_inic(4) }|.
    CONCATENATE wa_001-dt_modif+6(2) '.' wa_001-dt_modif+4(2) '.' wa_001-dt_modif+0(4) INTO wa_saida-data.
    CONCATENATE wa_001-hr_modif+0(2) wa_001-hr_modif+2(2) wa_001-hr_modif+4(2) INTO wa_saida-hora SEPARATED BY ':'.
    wa_saida-usr_modif = wa_001-usr_modif.

    READ TABLE it_t370 INTO wa_t370 WITH KEY eqart = wa_001-class_oper.
    wa_saida-eartx = wa_t370-eartx.

**  Begin of " BUG   #104840  FF  28.02.2023
    wa_saida-rbnr = wa_001-rbnr.
** End of FF  28.02.2023

*** Inicio - Rubenilson Pereira - 09.12.2024 #149349
    wa_saida-tq_comb = wa_001-tq_comb.
    wa_saida-tolerancia = wa_001-tolerancia.
*** Fim - Rubenilson Pereira - 09.12.2024 #149349

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_001.
  ENDLOOP.
ENDFORM.                    " F_ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_EXIBIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibir_dados .
  CLEAR: wa_layout, wa_stable.

  IF obj_container IS INITIAL.
    wa_layout-zebra      = 'X'.
    wa_stable-row        = 'X'.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-cwidth_opt = 'X'.
    wa_layout-info_fname = space .
    wa_layout-sel_mode   = 'A'.

    CREATE OBJECT obj_container
      EXPORTING
        container_name = 'OBJ_AREA'.

    CREATE OBJECT obj_grid
      EXPORTING
        i_parent = obj_container.

    wa_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wa_function TO it_function.
    wa_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_function TO it_function.

    PERFORM f_montar_layout.

    CALL METHOD obj_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = it_function
        is_layout            = wa_layout
        i_save               = 'X'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog[]
        it_outtab            = it_saida[].

    CALL METHOD obj_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD obj_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " F_EXIBIR_DADOS

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  PERFORM: f_exibir_dados.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_montar_layout .
  DATA: lv_tabela TYPE string VALUE 'IT_SAIDA',
        lv_cont   TYPE i.

  CLEAR lv_cont.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'CLASS_OPER'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'CLASS_OPER'.
  wa_fieldcatalog-reptext       = 'Cod. Classe'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'EARTX'.
  wa_fieldcatalog-tabname       = 'T370K_T'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'EARTX'.
  wa_fieldcatalog-reptext       = 'Classe Operacional'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'HERST'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'HERST'.
  wa_fieldcatalog-reptext       = 'Fabricante'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'TYPBZ'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'TYPBZ'.
  wa_fieldcatalog-reptext       = 'Modelo'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'GROES'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'GROES'.
  wa_fieldcatalog-reptext       = 'Tamanho/dimensão'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'CONSUMO'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'CONSUMO'.
  wa_fieldcatalog-reptext       = 'Consumo'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'VARIACAO'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'VARIACAO'.
  wa_fieldcatalog-reptext       = 'Variação'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'RBNR'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'RBNR'.
  wa_fieldcatalog-reptext       = 'Perfil do catálogo'.
  wa_fieldcatalog-scrtext_s     = 'PerfCat.'.
  wa_fieldcatalog-scrtext_m     = 'Perf.catálogo'.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

*** Inicio - Rubenilson Pereira - 09.12.2024 #149349
  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'TQ_COMB'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'TQ_COMB'.
  wa_fieldcatalog-reptext       = 'Tq Comb'.
  wa_fieldcatalog-scrtext_s     = 'Tq Comb'.
  wa_fieldcatalog-scrtext_m     = 'Tq Comb'.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'TOLERANCIA'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'TOLERANCIA'.
  wa_fieldcatalog-reptext       = 'Percentual Tolerância'.
  wa_fieldcatalog-scrtext_s     = 'Tolerânc'.
  wa_fieldcatalog-scrtext_m     = 'Tolerância'.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
*** Fim - Rubenilson Pereira - 09.12.2024 #149349

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'DT_INIC'.
  wa_fieldcatalog-tabname       = ''.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'DT_INIC'.
  wa_fieldcatalog-reptext       = 'Data Criação Reg.'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'DATA'.
  wa_fieldcatalog-tabname       = ''.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'DATA'.
  wa_fieldcatalog-reptext       = 'Data Ult. Modif.'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'HORA'.
  wa_fieldcatalog-tabname       = ''.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'HORA'.
  wa_fieldcatalog-reptext       = 'Hora Ult. Modif.'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lv_cont.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-col_pos       = lv_cont.
  wa_fieldcatalog-fieldname     = 'USR_MODIF'.
  wa_fieldcatalog-tabname       = 'ZPMR0001'.
  wa_fieldcatalog-ref_table     = lv_tabela.
  wa_fieldcatalog-ref_field     = 'USR_MODIF'.
  wa_fieldcatalog-reptext       = 'Usuário Ult. Modif.'.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'LEAVE' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'NEW'.
      CLEAR: zpmr0001-groes, zpmr0001-herst, zpmr0001-typbz, zpmr0001-class_oper, zpmr0001-consumo, zpmr0001-variacao,zpmr0001-rbnr,
      zpmr0001-tolerancia,zpmr0001-tq_comb." Rubenilson - 15.01.24 - BUG163736
      zpmr0001-dt_criacao = sy-datum. " Rubenilson - 17.01.25 - BUG163736
      CALL SCREEN 0200 STARTING AT 10 10.

    WHEN 'EDIT'.
      CLEAR: it_selected_rows[], wa_selected_rows, zpmr0001.

      CALL METHOD obj_grid->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF it_selected_rows[] IS NOT INITIAL.
        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE it_saida INTO wa_saida INDEX wa_selected_rows-index.
          CLEAR wa_001.
          MOVE-CORRESPONDING wa_saida TO wa_001.
          zpmr0001-typbz      = wa_001-typbz.
          zpmr0001-class_oper = wa_001-class_oper.
          zpmr0001-consumo    = wa_001-consumo.
          TRANSLATE wa_001-variacao USING '% '.
          CONDENSE wa_001-variacao NO-GAPS.
          zpmr0001-variacao   = wa_001-variacao.
          zpmr0001-herst      = wa_001-herst.
          zpmr0001-groes      = wa_001-groes.
          zpmr0001-rbnr       = wa_001-rbnr.
          zpmr0001-tq_comb    = wa_001-tq_comb. " Rubenilson - 15.01.24 - BUG163736
          zpmr0001-tolerancia = wa_001-tolerancia." Rubenilson - 15.01.24 - BUG163736

          CALL SCREEN 0200 STARTING AT 10 10.

        ENDLOOP.

      ENDIF.

    WHEN 'SAVE'.
      PERFORM f_salvar_dados.

    WHEN 'DELETE'.
      PERFORM f_excluir_dados.

    WHEN 'REFRESH'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      MESSAGE 'Comando inválido.' TYPE 'E'.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_EXCLUIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_excluir_dados .
  DATA: p_resp,
  lv_msg TYPE bapi_msg.

  CLEAR: it_selected_rows[], wa_selected_rows.

  CALL METHOD obj_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows[] IS NOT INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmar'
        text_question         = 'Deseja realmente excluir a linha selecionada?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
        popup_type            = 'ICON_MESSAGE_QUESTION'
      IMPORTING
        answer                = p_resp.

    IF p_resp = 1.
      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_saida INTO wa_saida INDEX wa_selected_rows-index.
        MOVE-CORRESPONDING wa_saida TO wa_001.
        PERFORM f_encontrar_dependencias USING wa_001
                                         CHANGING sy-subrc.
        DELETE zpmr0001 FROM wa_001.

        CONCATENATE 'Remov. >> Fabric.-> "' wa_001-herst '" Model.-> "' wa_001-typbz '" Tam.-> "' wa_001-groes '"' INTO lv_msg SEPARATED BY space.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            i_tp_msg   = 'W'
            i_mensagem = lv_msg
            i_tcode    = sy-tcode.

      ENDLOOP.
    ENDIF.

    PERFORM: f_seleciona_dados,
             f_organizar_dados,
             f_exibir_dados.
  ENDIF.

ENDFORM.                    " F_EXCLUIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SALVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_salvar_dados .
  DATA: id      TYPE i,
        lv_msg  TYPE bapi_msg,
        lv_erro TYPE c.

  PERFORM f_validar_dados USING zpmr0001-herst
                                zpmr0001-typbz
                                zpmr0001-class_oper
                                zpmr0001-variacao
                                zpmr0001-consumo
                        CHANGING lv_erro.

  CHECK lv_erro IS INITIAL.

  IF wa_001-id_fab_mod IS INITIAL and vg_ucomm <> 'EDIT'.
    PERFORM seq_001 CHANGING wa_001-id_fab_mod.

    lv_msg = 'Inser. >> '.

  ELSE.
    IF zpmr0001-groes NE wa_001-groes.
      PERFORM f_encontrar_dependencias USING wa_001
                                       CHANGING sy-subrc.
    ENDIF.

    lv_msg = 'Modif. >> '.

  ENDIF.

  wa_001-herst      = zpmr0001-herst.
  wa_001-typbz      = zpmr0001-typbz.
  wa_001-class_oper = zpmr0001-class_oper.
  wa_001-consumo    = zpmr0001-consumo.
  wa_001-variacao   = zpmr0001-variacao.
  wa_001-rbnr       = zpmr0001-rbnr.
  wa_001-groes      = zpmr0001-groes.
  wa_001-tq_comb    = zpmr0001-tq_comb.
  wa_001-tolerancia = zpmr0001-tolerancia.
  wa_001-dt_modif   = sy-datum.
  wa_001-hr_modif   = sy-uzeit.
  wa_001-usr_modif  = sy-uname.

  MODIFY zpmr0001 FROM wa_001.

  CONCATENATE lv_msg 'Fabric.:"' zpmr0001-herst '" -> Model.:"' zpmr0001-typbz '" -> Tam.:"' zpmr0001-groes
              '" -> Class.:"' zpmr0001-class_oper '" -> Vari.:"' zpmr0001-variacao '" -> Consu.: "' zpmr0001-consumo '"'
  INTO lv_msg SEPARATED BY space.

  CALL FUNCTION 'Z_GRAVA_LOG_PM'
    EXPORTING
      i_tp_msg   = 'W'
      i_mensagem = lv_msg
      i_tcode    = sy-tcode.

  PERFORM: f_seleciona_dados,
           f_organizar_dados,
           f_exibir_dados.

  LEAVE TO SCREEN 0.
ENDFORM.                    " F_SALVAR_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

  CLEAR:  vg_edit, vg_ucomm.

  vg_ucomm = sy-ucomm.

  LOOP AT SCREEN.
    IF screen-name EQ 'ZPMR0001-HERST'
    OR screen-name EQ 'ZPMR0001-TYPBZ'
    OR screen-name EQ 'ZPMR0001-CLASS_OPER'.
      IF sy-ucomm = 'EDIT'.
        screen-input = 0.
        vg_edit = 'X'.
      ELSE.
        screen-input = 1.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SEQ_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ID  text
*----------------------------------------------------------------------*
FORM seq_001  CHANGING p_id.
  SELECT id_fab_mod
    INTO p_id
    FROM zpmr0001 .
  ENDSELECT.

  ADD 1 TO p_id.
ENDFORM.                    " SEQ_001

*&---------------------------------------------------------------------*
*&      Module  BUSCA_CLASS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_class INPUT.

  DATA: BEGIN OF tl_temp OCCURS 0,
          eqart TYPE t370k_t-eqart,
          eartx TYPE t370k_t-eartx,
        END OF tl_temp.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  REFRESH: tl_temp.

  SELECT *
    FROM t370k_t
    INTO CORRESPONDING FIELDS OF TABLE tl_temp
  WHERE spras = sy-langu.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'EQART'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'T370K_T-EQART'
        value_org       = 'S'
      TABLES
        value_tab       = tl_temp
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.

  ENDIF.
ENDMODULE.                 " BUSCA_CLASS  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_001  text
*----------------------------------------------------------------------*
FORM f_validar_dados USING p_herst      TYPE zpmr0001-herst
                           p_typbz      TYPE zpmr0001-typbz
                           p_class_oper TYPE zpmr0001-class_oper
                           p_variacao   TYPE zpmr0001-variacao
                           p_consumo    TYPE zpmr0001-consumo
                  CHANGING p_erro       TYPE c.

  DATA : lv_class TYPE zpmr0001-class_oper,
         lv_num   TYPE p,
         lw_0069  TYPE zpmt0069,
         lw_0070  TYPE zpmt0070.

** Validar campo classe operacional
  SELECT SINGLE eqart
    FROM t370k_t
    INTO lv_class
  WHERE eqart = p_class_oper.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Classe inválida' TYPE 'E'.
  ENDIF.

** Valida campo consumo
  CALL FUNCTION 'MOVE_CHAR_TO_NUM'
    EXPORTING
      chr             = p_variacao
    IMPORTING
      num             = lv_num
    EXCEPTIONS
      convt_no_number = 1
      convt_overflow  = 2
      OTHERS          = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Valor inválido para variação.' TYPE 'E'.
  ENDIF.

* Valida campo variação
  CALL FUNCTION 'MOVE_CHAR_TO_NUM'
    EXPORTING
      chr             = p_consumo
    IMPORTING
      num             = lv_num
    EXCEPTIONS
      convt_no_number = 1
      convt_overflow  = 2
      OTHERS          = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Valor inválido para consumo.' TYPE 'E'.
  ENDIF.

  IF p_herst IS NOT INITIAL.
    SELECT *
      FROM zpmt0069
      UP TO 1 ROWS
      INTO lw_0069
      WHERE herst = p_herst.
    ENDSELECT.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Fabricante inválido!' TYPE 'S' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_typbz IS NOT INITIAL.
    SELECT *
      FROM zpmt0070
      UP TO 1 ROWS
      INTO lw_0070
      WHERE typbz = p_typbz.
    ENDSELECT.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Modelo inválido!' TYPE 'S' DISPLAY LIKE 'E'.
      p_erro = abap_true.
    ENDIF.
  ENDIF.
  IF p_herst IS INITIAL  OR p_typbz  IS INITIAL OR p_class_oper IS INITIAL.
    MESSAGE 'Campos: Classe Operacional, Fabricante e Modelo são obrigatórios' TYPE 'S' DISPLAY LIKE 'E'.
    p_erro = abap_true.
  ELSE.
    SELECT SINGLE *
      FROM zpmr0001
      INTO @DATA(wa_zpmr0001)
      WHERE herst = @p_herst AND
            typbz = @p_typbz AND
            class_oper = @p_class_oper.
    IF sy-subrc = 0 AND vg_edit IS INITIAL..
      MESSAGE 'Já existe estes valores na tabela ZPMR001' TYPE 'S' DISPLAY LIKE 'E'.
      p_erro = abap_true.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_VALIDAR_DADOS


*&---------------------------------------------------------------------*
*&      Form  P_ENCONTRAR_DEPENDENCIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_001      text
*      -->C_ERRO     text
*----------------------------------------------------------------------*
FORM f_encontrar_dependencias USING    p_001  TYPE ty_001
                              CHANGING c_erro TYPE sy-subrc.
  DATA: lv_msg      TYPE c LENGTH 255,
        wl_layout   TYPE slis_layout_alv,
        lv_cont     TYPE i,
        tl_fieldcat TYPE TABLE OF ty_estrutura,
        wl_fieldcat TYPE ty_estrutura.

  SELECT DISTINCT equnr eqktx swerk datbi
    INTO TABLE gt_equi_depen
    FROM v_equi
   WHERE herst EQ p_001-herst
     AND typbz EQ p_001-typbz
  AND groes EQ p_001-groes.

  SORT gt_equi_depen BY equnr ASCENDING datbi DESCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_equi_depen COMPARING equnr.

  IF gt_equi_depen IS NOT INITIAL .
    CONCATENATE 'Fabricante/Modelo:' p_001-herst '/' p_001-typbz 'já foi utilizado e não pode ser removido/alterado.' INTO lv_msg SEPARATED BY space.

    CLEAR: wl_fieldcat.
    ADD 1 TO lv_cont.
    wl_fieldcat-fieldname     = 'SWERK'.
    wl_fieldcat-tabname       = 'GT_EQUI_DEPEN'.
    wl_fieldcat-col_pos       = lv_cont.
    wl_fieldcat-seltext_s     = 'Centro'.
    wl_fieldcat-seltext_m     = wl_fieldcat-seltext_s.
    wl_fieldcat-seltext_l     = wl_fieldcat-seltext_s.
    APPEND wl_fieldcat TO tl_fieldcat.

    CLEAR: wl_fieldcat.
    ADD 1 TO lv_cont.
    wl_fieldcat-fieldname     = 'EQUNR'.
    wl_fieldcat-tabname       = 'GT_EQUI_DEPEN'.
    wl_fieldcat-col_pos       = lv_cont.
    wl_fieldcat-seltext_s     = 'Num.Equipamento'.
    wl_fieldcat-seltext_m     = wl_fieldcat-seltext_s.
    wl_fieldcat-seltext_l     = wl_fieldcat-seltext_s.
    wl_fieldcat-outputlen     = 18.
    APPEND wl_fieldcat TO tl_fieldcat.

    CLEAR: wl_fieldcat.
    ADD 1 TO lv_cont.
    wl_fieldcat-fieldname     = 'EQKTX'.
    wl_fieldcat-tabname       = 'GT_EQUI_DEPEN'.
    wl_fieldcat-col_pos       = lv_cont.
    wl_fieldcat-seltext_s     = 'Descrição'.
    wl_fieldcat-seltext_m     = wl_fieldcat-seltext_s.
    wl_fieldcat-seltext_l     = wl_fieldcat-seltext_s.
    wl_fieldcat-outputlen     = 50.
    APPEND wl_fieldcat TO tl_fieldcat.

    wl_layout-zebra           = abap_true.
    wl_layout-window_titlebar = 'Dependências encontradas'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        is_layout             = wl_layout
        it_fieldcat           = tl_fieldcat
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      TABLES
        t_outtab              = gt_equi_depen[].

    MESSAGE  lv_msg TYPE 'E'.

  ENDIF.

ENDFORM.                    "P_ENCONTRAR_DEPENDENCIAS

FORM f_fabricante CHANGING c_herts TYPE equi-herst.
  DATA: lv_value  TYPE help_info-fldvalue,
        lt_return TYPE STANDARD TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = '<any>'     " Table/structure name from Dictionary
      fieldname         = '<any>'     " Field name from Dictionary
      searchhelp        = 'Z_SH_ZPMT0069'      " Search help as screen field attribute
      value             = lv_value    " Field contents for F4 call
    TABLES
      return_tab        = lt_return   " Return the selected value
    EXCEPTIONS
      field_not_found   = 1           " Field does not exist in the Dictionary
      no_help_for_field = 2           " No F4 help is defined for the field
      inconsistent_help = 3           " F4 help for the field is inconsistent
      no_values_found   = 4           " No values found
      OTHERS            = 5.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
  READ TABLE lt_return INTO DATA(ls_ret) INDEX 1.
  c_herts = ls_ret-fieldval.
ENDFORM.

FORM f_modelo CHANGING c_typbz TYPE equi-typbz.
  DATA: lv_value  TYPE help_info-fldvalue,
        lt_return TYPE STANDARD TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = '<any>'     " Table/structure name from Dictionary
      fieldname         = '<any>'     " Field name from Dictionary
      searchhelp        = 'Z_SH_ZPMT0070'      " Search help as screen field attribute
      value             = lv_value    " Field contents for F4 call
    TABLES
      return_tab        = lt_return   " Return the selected value
    EXCEPTIONS
      field_not_found   = 1           " Field does not exist in the Dictionary
      no_help_for_field = 2           " No F4 help is defined for the field
      inconsistent_help = 3           " F4 help for the field is inconsistent
      no_values_found   = 4           " No values found
      OTHERS            = 5.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
  READ TABLE lt_return INTO DATA(ls_ret) INDEX 1.
  c_typbz = ls_ret-fieldval.
ENDFORM.
