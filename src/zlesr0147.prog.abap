*&---------------------------------------------------------------------*
*& Report  ZLESR0147
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0147.
TABLES zlest0204.

TYPES: BEGIN OF ty_saida,
         bukrs        TYPE zlest0204-bukrs,
         butxt        TYPE t001-butxt,
         branch       TYPE zlest0204-branch,
         name1        TYPE t001w-name1,
         parid        TYPE zlest0204-parid,
         name         TYPE lfa1-name1,
         tp_movimento TYPE zlest0204-tp_movimento,
         desc_classif TYPE zlest0204-desc_classif,
         user_reg     TYPE zlest0204-user_reg,
         data_reg     TYPE zlest0204-data_reg,
         hora_reg     TYPE zlest0204-hora_reg,
         celltab      TYPE lvc_t_styl,
       END OF ty_saida.

"internal table
DATA: it_saida     TYPE TABLE OF ty_saida,
      it_saida_aux TYPE TABLE OF ty_saida,
      it_excluir   TYPE TABLE OF zlest0204,
      it_save      TYPE TABLE OF zlest0204,
      it_zlest0204 TYPE TABLE OF zlest0204,
      it_t001      TYPE TABLE OF t001,
      it_bbranch   TYPE TABLE OF j_1bbranch,
      it_lfa1      TYPE TABLE OF lfa1.

"wORKAREA
DATA: wa_saida     TYPE ty_saida,
      wa_saida_aux TYPE ty_saida,
      wa_excluir   TYPE zlest0204,
      wa_save      TYPE zlest0204.


"OBJETOS
DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      grid               TYPE REF TO cl_gui_alv_grid,
      it_fieldcat        TYPE lvc_t_fcat,
      wa_fieldcat        TYPE lvc_s_fcat,
      tl_function        TYPE ui_functions,
      wl_function        LIKE tl_function  WITH HEADER LINE,
      wa_layout          TYPE lvc_s_layo,
      wa_variant         TYPE disvariant,
      wa_estilo          TYPE lvc_t_styl,
      wa_stable          TYPE lvc_s_stbl VALUE 'XX'.


INITIALIZATION.

START-OF-SELECTION.
  PERFORM fm_busca_dados_aux.

  PERFORM  fm_busca_dados.

END-OF-SELECTION.
  CALL SCREEN 0100.


CLASS lcl_f4_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_f4_01 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
ENDCLASS.

CLASS lcl_f4_handler IMPLEMENTATION.
  METHOD on_f4_01.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE  dd03l-fieldname,
             S(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname     TYPE dd03l-tabname,    "Nome da tabela
             fieldname   TYPE dd03l-fieldname,    "Nome de campo
             char79(100) TYPE c,
           END OF ty_value.

    TYPES: BEGIN OF ty_movimento,
             tp_movimento TYPE zsdt0001-tp_movimento,
             name         TYPE kna1-name1,
           END OF ty_movimento.


    DATA: tl_field     TYPE TABLE OF ty_field,
          wl_field     TYPE ty_field,
          tl_value     TYPE TABLE OF ty_value,
          wl_value     TYPE ty_value,
          tl_movimento TYPE TABLE OF ty_movimento,
          wl_movimento TYPE ty_movimento,
          wl_char(20),
          wl_index     TYPE sy-tabix.


    CASE  e_fieldname.
      WHEN 'TP_MOVIMENTO'.

        wl_movimento-tp_movimento = 'E'.
        APPEND wl_movimento TO tl_movimento.

        wl_movimento-tp_movimento = 'S'.
        APPEND wl_movimento TO tl_movimento.


        wl_field-tabname   = 'ZSDT0001'.
        wl_field-fieldname = 'TP_MOVIMENTO'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.


        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
          EXPORTING
            fieldname                 = 'ZSDT0001'
            tabname                   = 'TP_MOVIMENTO'
          IMPORTING
            index                     = wl_index
            select_value              = wl_char
          TABLES
            fields                    = tl_field
            select_values             = tl_value
            valuetab                  = tl_movimento
          EXCEPTIONS
            field_not_in_ddic         = 001
            more_then_one_selectfield = 002
            no_selectfield            = 003.

        IF sy-subrc IS INITIAL.
          CLEAR wl_movimento.
          READ TABLE tl_movimento INTO wl_movimento INDEX wl_index.
          IF es_row_no-row_id GT 0.
            READ TABLE  it_saida INTO wa_saida INDEX es_row_no-row_id.
            IF sy-subrc = 0.
              MOVE wl_movimento-tp_movimento TO wa_saida-tp_movimento.
              MODIFY it_saida FROM wa_saida INDEX es_row_no-row_id.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_hander DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished  OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.

ENDCLASS.

CLASS lcl_hander IMPLEMENTATION.
  METHOD on_data_changed.

    CLEAR wa_saida.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'BUKRS'  OR  fieldname EQ 'BRANCH'      OR
            fieldname EQ 'PARID'  OR  fieldname EQ 'TP_MOVIMENTO'.

      LOOP AT it_saida INTO wa_saida.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'BUKRS'.
            READ TABLE it_t001 INTO DATA(w_t001) WITH KEY bukrs = wa_good_cells-value.
            IF sy-subrc EQ 0.
              wa_saida-bukrs = wa_good_cells-value.
              wa_saida-butxt = w_t001-butxt.
              MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Empresa informada não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
          WHEN 'BRANCH'.
            READ TABLE it_bbranch INTO DATA(w_bbranch) WITH KEY branch = wa_good_cells-value.
            IF  sy-subrc EQ 0.
              wa_saida-branch = wa_good_cells-value.
              wa_saida-name1  = w_bbranch-name.
              MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Filial informada não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
          WHEN 'PARID'.
            READ TABLE it_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = wa_good_cells-value.
            IF  sy-subrc EQ 0.
              wa_saida-parid = wa_good_cells-value.
              wa_saida-name  = w_lfa1-name1.
              MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Fornecedor informado não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
          WHEN 'TP_MOVIMENTO'.
            CASE wa_good_cells-value.
              WHEN 'E' OR 'S'.
                wa_saida-tp_movimento = wa_good_cells-value.
                MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
              WHEN OTHERS.
                MESSAGE 'Tipo de movimento informado não existe!' TYPE 'I'.
                EXIT.
            ENDCASE.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.

  METHOD on_data_changed_finished.

    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
  ENDMETHOD.

ENDCLASS.

FORM fm_busca_dados_aux.

  SELECT * FROM t001 INTO TABLE it_t001.

  SELECT * FROM j_1bbranch INTO TABLE it_bbranch.

  SELECT * FROM lfa1 INTO TABLE it_lfa1.

ENDFORM.


FORM fm_busca_dados.

  REFRESH: it_saida, it_zlest0204.

  SELECT * FROM  zlest0204 INTO TABLE it_zlest0204.

  LOOP AT it_zlest0204 INTO DATA(wa_zlest0204).

    CLEAR: wa_saida.

    wa_saida-bukrs        = wa_zlest0204-bukrs.
    wa_saida-branch       = wa_zlest0204-branch.
    wa_saida-parid        = wa_zlest0204-parid.
    wa_saida-tp_movimento = wa_zlest0204-tp_movimento.
*-PBI 66101 - 01.10.2021 - JT - inicio
    wa_saida-desc_classif = wa_zlest0204-desc_classif.
    wa_saida-user_reg     = wa_zlest0204-user_reg.
    wa_saida-data_reg     = wa_zlest0204-data_reg.
    wa_saida-hora_reg     = wa_zlest0204-hora_reg.
*-PBI 66101 - 01.10.2021 - JT - fim

    READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_zlest0204-bukrs.
    IF sy-subrc EQ 0.
      wa_saida-butxt = wa_t001-butxt.
    ENDIF.

    READ TABLE it_bbranch INTO DATA(wa_bbranch) WITH KEY branch = wa_zlest0204-branch.
    IF sy-subrc EQ 0.
      wa_saida-name1 = wa_bbranch-name.
    ENDIF.

    READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_zlest0204-parid.
    IF sy-subrc EQ 0.
      wa_saida-name = wa_lfa1-name1.
    ENDIF.

    FREE wa_saida-celltab.

    wa_estilo =  VALUE #( ( fieldname = 'BUKRS'         style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'BRANCH'        style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'PARID'         style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'TP_MOVIMENTO'  style = cl_gui_alv_grid=>mc_style_disabled )
*-PBI 66101 - 01.10.2021 - JT - inicio
                          ( fieldname = 'DESC_CLASSIF'  style = cl_gui_alv_grid=>mc_style_enabled )
                          ( fieldname = 'USER_REG'      style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'DATA_REG'      style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'HORA_REG'      style = cl_gui_alv_grid=>mc_style_disabled ) ).
*-PBI 66101 - 01.10.2021 - JT - fim

    INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.

    APPEND wa_saida TO it_saida.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: lt_f4 TYPE lvc_t_f4,
        wl_f4 TYPE lvc_s_f4.


  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM alv.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF grid IS INITIAL AND g_custom_container IS NOT  INITIAL.

      CREATE OBJECT grid
        EXPORTING
          i_parent = g_custom_container.

    ENDIF.

    wl_function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.


    wa_layout-stylefname = 'CELLTAB'.

    wl_f4-fieldname  = 'TP_MOVIMENTO'.
    wl_f4-register   = 'X'.
    wl_f4-getbefore  = 'X'.
    APPEND wl_f4 TO  lt_f4.
    CLEAR wl_f4.


    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        is_variant           = wa_variant
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_outtab            = it_saida
        it_fieldcatalog      = it_fieldcat.


    SET HANDLER lcl_f4_handler=>on_f4_01 FOR grid.

    SET HANDLER: lcl_hander=>on_data_changed FOR grid,
                 lcl_hander=>on_data_changed_finished FOR grid.


    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

  ELSE.

    CALL METHOD grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat.

    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN '&INS'.
      APPEND INITIAL LINE TO it_saida.
    WHEN '&DEL'.
      PERFORM fm_del.
    WHEN 'SAVE'.
      PERFORM fm_save.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv .
  REFRESH it_fieldcat.

  PERFORM preenche_cat USING :
        'BUKRS'          'Empresa'               '07'   ''  ''  'X'  'T001'  'BUKRS'   ''   '',
        'BUTXT'          'Descrição'             '25'   ''  ''  ''   ''       ''       ''   '',
        'BRANCH'         'Filial'                '07'   ''  ''  'X'  'T001W'  'WERKS'  ''   '',
        'NAME1'          'Descrição'             '25'   ''  ''  ''   ''       ''       ''   '',
        'PARID'          'Parceiro'              '10'   ''  ''  'X'  'LFA1'   'LIFNR'  ''   '',
        'NAME'           'Descrição'             '25'   ''  ''  ''   ''       ''       ''   '',
        'TP_MOVIMENTO'   'Tipo Movimento'        '12'   ''  ''  'X'  ''       ''       'X'  '',
*-PBI 66101 - 01.10.2021 - JT - inicio
        'DESC_CLASSIF'   'Considerar Classif?'   '16'   ''  ''  'X'  ''       ''       ''   'X',
        'USER_REG'       'Usuário'               '15'   ''  ''  ' '  ''       ''       ''   ' ',
        'DATA_REG'       'Data Modif.'           '12'   ''  ''  ' '  ''       ''       ''   ' ',
        'HORA_REG'       'Hora Modif.'           '12'   ''  ''  ' '  ''       ''       ''   ' '.
*-PBI 66101 - 01.10.2021 - JT - fim

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_hot)
                        VALUE(p_just)
                        VALUE(p_edit)
                        VALUE(p_table)
                        VALUE(p_fieldname)
                        VALUE(p_f4)
                        VALUE(p_box).

  wa_fieldcat-fieldname   = p_campo.
  wa_fieldcat-coltext     = p_desc.
  wa_fieldcat-scrtext_l   = p_desc.
  wa_fieldcat-scrtext_m   = p_desc.
  wa_fieldcat-scrtext_s   = p_desc.
  wa_fieldcat-outputlen   = p_tam.
  wa_fieldcat-hotspot     = p_hot.
  wa_fieldcat-just        = p_just.
  wa_fieldcat-edit        = p_edit.
  wa_fieldcat-ref_table   = p_table.
  wa_fieldcat-ref_field   = p_fieldname.
  wa_fieldcat-f4availabl  = p_f4.
  wa_fieldcat-checkbox    = p_box.

  APPEND wa_fieldcat TO it_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_save .

  REFRESH it_save.
  CLEAR wa_save.

  IF it_excluir IS NOT INITIAL.
    DELETE zlest0204 FROM TABLE it_excluir.
    COMMIT WORK.

    MESSAGE 'Registro excluido com sucesso!' TYPE 'S'.
    REFRESH it_excluir.
  ENDIF.

  LOOP AT it_saida INTO wa_saida.
    CLEAR wa_save.
    READ TABLE it_zlest0204 INTO DATA(w_t0204) WITH KEY bukrs  = wa_saida-bukrs
                                                        branch = wa_saida-branch
                                                        parid  = wa_saida-parid
                                                        tp_movimento = wa_saida-tp_movimento.
    IF sy-subrc NE 0.

      IF wa_saida-bukrs IS INITIAL.
        MESSAGE 'Favor selecione uma empresa!' TYPE 'S'.
        EXIT.
      ELSE.
        READ TABLE it_t001 INTO DATA(_t001) WITH KEY bukrs =  wa_saida-bukrs.
        IF sy-subrc NE 0.
          MESSAGE 'Empresa informada não existe!' TYPE 'S'.
          EXIT.
        ELSE.
          MOVE wa_saida-bukrs TO wa_save-bukrs.
        ENDIF.
      ENDIF.

      IF wa_saida-branch IS INITIAL.
        MESSAGE 'Favor selecione Filial!' TYPE 'S'.
        EXIT.
      ELSEIF wa_saida-bukrs IS NOT INITIAL.
        READ TABLE it_bbranch INTO DATA(w_bbranch) WITH KEY bukrs = wa_saida-bukrs
                                                            branch = wa_saida-branch.
        IF sy-subrc NE 0.
          MESSAGE 'Filial informada não existe ou não pertence a empresa informada!' TYPE 'S'.
          EXIT.
        ELSE.
          MOVE wa_saida-branch TO wa_save-branch.
        ENDIF.
      ENDIF.

      IF wa_saida-parid IS INITIAL.
        MESSAGE 'Favor informar codigo fornecedor!' TYPE 'S'.
        EXIT.
      ELSE.
        READ TABLE it_lfa1 INTO DATA(_lfa1) WITH KEY lifnr = wa_saida-parid.
        IF sy-subrc NE 0.
          MESSAGE 'Fornecedor informado não existe!' TYPE 'S'.
          EXIT.
        ELSE.
          MOVE wa_saida-parid TO wa_save-parid.
        ENDIF.
      ENDIF.

      IF wa_saida-tp_movimento IS INITIAL.
        MESSAGE 'Favor informe Tipo de Movimento' TYPE 'S'.
        EXIT.
      ELSE.
        CASE wa_saida-tp_movimento.
          WHEN 'E' OR 'S'.
            MOVE wa_saida-tp_movimento TO wa_save-tp_movimento.
          WHEN OTHERS.
            MESSAGE 'Tipo de movimento informado não existe!' TYPE 'S'.
            EXIT.
        ENDCASE.
      ENDIF.
*-PBI 66101 - 01.10.2021 - JT - inicio
      MOVE wa_saida-desc_classif  TO wa_save-desc_classif.
      MOVE sy-uname               TO wa_save-user_reg.
      MOVE sy-datum               TO wa_save-data_reg.
      MOVE sy-uzeit               TO wa_save-hora_reg.
*-PBI 66101 - 01.10.2021 - JT - fim

      CHECK wa_save IS NOT INITIAL.
      APPEND wa_save TO it_save.

    ELSE.
*-PBI 66101 - 01.10.2021 - JT - inicio
      IF wa_saida-desc_classif <> w_t0204-desc_classif.
        MOVE-CORRESPONDING wa_saida TO wa_save.
        MOVE sy-uname               TO wa_save-user_reg.
        MOVE sy-datum               TO wa_save-data_reg.
        MOVE sy-uzeit               TO wa_save-hora_reg.
        APPEND wa_save              TO it_save.
      ENDIF.
*-PBI 66101 - 01.10.2021 - JT - fim
    ENDIF.
    CLEAR wa_saida.
  ENDLOOP.

  IF it_save[] IS NOT INITIAL.
    MODIFY zlest0204 FROM TABLE it_save.
    COMMIT WORK.

    MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.

    PERFORM fm_busca_dados.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_del .

  CLEAR: wa_saida, wa_excluir, wa_saida_aux.
  REFRESH: it_excluir, it_saida_aux.

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(it_index_rows).

  IF it_index_rows[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha' TYPE 'S'.
    EXIT.
  ELSE.

    LOOP AT it_index_rows INTO DATA(wa_index_rows).
      CLEAR: wa_saida, wa_excluir.

      READ TABLE it_saida INTO wa_saida INDEX wa_index_rows-index.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING wa_saida TO wa_excluir.
        APPEND wa_excluir TO it_excluir.

        APPEND wa_saida TO it_saida_aux.
      ENDIF.
    ENDLOOP.

    LOOP AT it_saida_aux INTO wa_saida_aux.
      DELETE it_saida WHERE bukrs       = wa_saida_aux-bukrs
                      AND   branch      = wa_saida_aux-branch
                      AND   parid       = wa_saida_aux-parid
                      AND  tp_movimento = wa_saida_aux-tp_movimento.
    ENDLOOP.
  ENDIF.
ENDFORM.
