*&---------------------------------------------------------------------*
*& Include          ZLESR0183F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
FORM f_seleciona_dados.

  DATA ls_saida TYPE ty_saida.

  IF s_ordem IS NOT INITIAL.
    SELECT bukrs, branch, vbeln, nr_romaneio, dt_movimento,
           agente_frete, id_ordem, status
      FROM zsdt0001
      INTO TABLE @DATA(lt_0001)
      WHERE nr_romaneio  IN @s_nr
        AND  vbeln      IN @s_ordem
*         OR vbeln        IN @s_ov )
        AND branch       IN @s_centro
        AND dt_movimento IN @s_dt_m.
  ELSEIF s_ov IS NOT INITIAL.
    SELECT bukrs, branch, vbeln, nr_romaneio, dt_movimento,
           agente_frete, id_ordem, status
      FROM zsdt0001
      INTO TABLE @lt_0001
      WHERE nr_romaneio  IN @s_nr
        AND vbeln        IN @s_ov
        AND branch       IN @s_centro
        AND dt_movimento IN @s_dt_m.
  ENDIF.

  IF sy-subrc IS INITIAL.

    SELECT id_ordem, nr_ordem
      FROM zsdt0001od
      INTO TABLE @DATA(lt_0001od)
      FOR ALL ENTRIES IN @lt_0001
      WHERE id_ordem = @lt_0001-id_ordem.

    IF sy-subrc IS INITIAL.

      SELECT id_ordem, viagem_id, id_lote_frete, id_localizador
        FROM zlest0185
        INTO TABLE @DATA(lt_0185)
        FOR ALL ENTRIES IN @lt_0001od
        WHERE id_ordem = @lt_0001od-id_ordem.

      IF sy-subrc IS INITIAL.
      ENDIF.
    ENDIF.

    SELECT vbeln, ztrocanota
      FROM vbak
      INTO TABLE @DATA(lt_vbak)
      FOR ALL ENTRIES IN @lt_0001
      WHERE vbeln = @lt_0001-vbeln.

    IF sy-subrc IS INITIAL.
    ENDIF.

    SELECT ebeln, ztrocanota
      FROM ekpo
      INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @lt_0001
      WHERE ebeln = @lt_0001-vbeln.

    IF sy-subrc IS INITIAL.

    ENDIF.

    LOOP AT lt_0001 INTO DATA(ls_001).

      MOVE-CORRESPONDING ls_001 TO ls_saida.

      READ TABLE lt_0001od INTO DATA(ls_001od)
           WITH KEY id_ordem = ls_001-id_ordem.

      IF sy-subrc IS INITIAL.

        ls_saida-nr_ordem = ls_001od-nr_ordem.

        READ TABLE lt_0185 INTO DATA(ls_0185)
             WITH KEY  id_ordem = ls_001od-id_ordem.

        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING ls_0185 TO ls_saida.
        ENDIF.
      ENDIF.

      READ TABLE lt_vbak INTO DATA(ls_vbak)
           WITH KEY vbeln = ls_001-vbeln.

      IF sy-subrc IS INITIAL.
        IF ls_vbak-ztrocanota IS NOT INITIAL.
          ls_saida-ztrocanota = ls_vbak-ztrocanota.
        ENDIF.
      ENDIF.

      IF ls_saida-ztrocanota IS INITIAL.
        READ TABLE lt_ekpo INTO DATA(ls_ekpo)
             WITH KEY ebeln = ls_001-vbeln.

        IF sy-subrc IS INITIAL.
          IF ls_ekpo-ztrocanota IS NOT INITIAL.
            ls_saida-ztrocanota = ls_ekpo-ztrocanota.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_saida TO t_saida.
      CLEAR ls_saida.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM f_monta_estrutura.

  DATA: lo_structdescr   TYPE REF TO cl_abap_structdescr,
        lt_components    TYPE cl_abap_structdescr=>component_table,
        ls_component     TYPE cl_abap_structdescr=>component,
        lo_dynamic_table TYPE REF TO data,
        lo_dynamic_line  TYPE REF TO data.

  FIELD-SYMBOLS: <lt_table_structure> TYPE STANDARD TABLE,
                 <ls_table_structure> TYPE any.

  " Criar tabela dinâmica baseada em IT_SAIDA
  CREATE DATA lo_dynamic_table TYPE TABLE OF ty_saida.
  ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

  CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
  ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

  " Obter descrição da estrutura de linha
  lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <ls_table_structure> ).

  lt_components = lo_structdescr->get_components( ).

  t_field_cat = VALUE #(
    ( fieldname =   'BUKRS'          coltext = 'Empresa'        edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'L')
    ( fieldname =   'BRANCH'         coltext = 'Filial'         edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'VBELN'          coltext = 'OV /Ped'        edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'NR_ROMANEIO'    coltext = 'NR. Romaneio '  edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'DT_MOVIMENTO'   coltext = 'Data Romaneio'  edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'AGENTE_FRETE'   coltext = 'Agente Frete '  edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'NR_ORDEM'       coltext = 'NR.OC.'         edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'ID_ORDEM'       coltext = 'ID Ordem'       edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'VIAGEM_ID'      coltext = 'Viagem ID'      edit = abap_false   outputlen = 25  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'ID_LOTE_FRETE'  coltext = 'ID Lote Frete'  edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'ID_LOCALIZADOR' coltext = 'ID Localizador' edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'ZTROCANOTA'     coltext = 'Troca Nota'     edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
    ( fieldname =   'STATUS'         coltext = 'Status'         edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L') ).

  LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_components>).

    READ TABLE t_field_cat WITH KEY fieldname = <fs_components>-name INTO DATA(wa_field_cat). "table_line

    IF sy-subrc = 0.

      MOVE-CORRESPONDING wa_field_cat TO wa_fieldcat.
      wa_fieldcat-col_pos = sy-tabix.

    ENDIF.

    APPEND wa_fieldcat TO t_fieldcat.
    CLEAR: wa_fieldcat.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibi_alv
*&---------------------------------------------------------------------*
FORM f_exibi_alv.

  CONSTANTS: gs_layout     TYPE lvc_s_layo VALUE abap_true.
  DATA: ls_variant     TYPE disvariant,
        main_container TYPE REF TO cl_gui_custom_container.

  ls_variant-report = sy-repid.

  CREATE OBJECT:
      main_container EXPORTING container_name = 'O_ALV',
      o_alv          EXPORTING i_parent = main_container.

  CREATE OBJECT lo_report_alv.

  SET HANDLER: lo_report_alv->on_user_command_out3 FOR o_alv,
               lo_report_alv->on_toolbar_out3 FOR o_alv.

  CALL METHOD o_alv->set_ready_for_input( EXPORTING i_ready_for_input = 1 ).

  CALL METHOD o_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
      is_variant      = ls_variant
      i_save          = 'A'
    CHANGING
      it_outtab       = t_saida
      it_fieldcatalog = t_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valida
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_valida.

  IF s_ordem IS NOT INITIAL
    OR s_ov IS NOT INITIAL.
    EXIT.
  ENDIF.

  IF s_nr IS NOT INITIAL.
    MESSAGE s001(zsd)
    DISPLAY LIKE 'E'
    WITH TEXT-e01.

    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_centro IS NOT INITIAL.
    MESSAGE s001(zsd)
    DISPLAY LIKE 'E'
    WITH TEXT-e02.

    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_dt_m IS NOT INITIAL.
    MESSAGE s001(zsd)
    DISPLAY LIKE 'E'
    WITH TEXT-e03.

    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
