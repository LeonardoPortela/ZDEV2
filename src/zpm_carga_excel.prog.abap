*&---------------------------------------------------------------------*
*& Report  ZPM_CARGA_EXCEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zpm_carga_excel.


*----------------------------------------------------------------------*
* INCLUDE                                                              *
*----------------------------------------------------------------------*
INCLUDE zpm_carga_excel_top.
INCLUDE zpm_carga_excel_cla.

**********************************************************************
* Inicio
**********************************************************************
INITIALIZATION.

  "FC01
  l_sel_button-icon_id   = icon_dangerous_goods.
  l_sel_button-icon_text = 'Download Planilha Modelo Carga'.
  sscrfields-functxt_01  = l_sel_button.

**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM f_gera_modelo_planillha.
    WHEN OTHERS.
  ENDCASE.


**********************************************************************
*START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.

  CHECK p_file IS NOT INITIAL.
  PERFORM f_le_arquivo.

  IF p_0017 IS NOT INITIAL.
    PERFORM f_monta_arq_saida USING '0017'.
    PERFORM f_mostrar_dados CHANGING gt_data_0017
                                     gr_alv.

  ELSEIF p_0093 IS NOT INITIAL.
    PERFORM f_monta_arq_saida USING '0093'.
    PERFORM f_mostrar_dados CHANGING gt_data_0074
                                     gr_alv.

  ELSEIF p_0094 IS NOT INITIAL.
    PERFORM f_monta_arq_saida USING '0094'.
    PERFORM f_mostrar_dados CHANGING gt_data_0075
                                     gr_alv.
  ENDIF.

END-OF-SELECTION.


**********************************************************************
* seleciona arquivo
**********************************************************************
FORM f_busca_arquivo USING p_filename TYPE localfile.

  DATA: l_subrc     LIKE sy-subrc,
        t_filetable TYPE filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Selecione o arquivo .xls'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = t_filetable
      rc               = l_subrc.

  READ TABLE t_filetable INTO p_filename INDEX 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERA_MODELO_PLANILLHA
*&---------------------------------------------------------------------*
FORM f_gera_modelo_planillha .

  DATA:  path(250).

  IF p_0017 IS NOT INITIAL.
    DATA(lv_nome) = 'ZPM0017'.
  ELSEIF p_0093 IS NOT INITIAL.
    lv_nome = 'ZPM0093'.
  ELSEIF p_0094 IS NOT INITIAL.
    lv_nome = 'ZPM0094'.
  ENDIF.

  CONCATENATE 'PlanilhaCargaModelo_' lv_nome  INTO DATA(v_nome_arquivo).

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = v_nome_arquivo
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    CONCATENATE path '.xls' INTO DATA(p_local).

    CASE lv_nome.
      WHEN 'ZPM0017'.
        PERFORM f_preenche_cabec_excel USING '0017'
                                           p_local.
      WHEN 'ZPM0093'.
        PERFORM f_preenche_cabec_excel USING '0093'
                                           p_local.
      WHEN 'ZPM0094'.
        PERFORM f_preenche_cabec_excel USING '0094'
                                           p_local.

      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CABEC_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ESTRUTURA  text
*----------------------------------------------------------------------*
FORM f_preenche_cabec_excel  USING p_estrutura TYPE any
                                 p_local.

  DATA: BEGIN OF t_fieldnames OCCURS 0,
          name(50) TYPE c,
        END OF t_fieldnames.

  DATA: lo_dynamic_table TYPE REF TO data.
  FIELD-SYMBOLS: <lt_table_structure> TYPE table,
                 <ls_table_structure> TYPE any.

  CASE p_estrutura.
    WHEN '0017'.

      CREATE DATA lo_dynamic_table TYPE TABLE OF zpmr0001.
      ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

      t_fieldnames-name = 'Cód Classe'. "ZPMR0001-CLASS_OPER
      APPEND t_fieldnames.
      t_fieldnames-name = 'Fabricante'. "ZPMR0001-HERST
      APPEND t_fieldnames.
      t_fieldnames-name = 'Modelo'. "ZPMR0001-TYPBZ
      APPEND t_fieldnames.
      t_fieldnames-name = 'Consumo'."ZPMR0001-CONSUMO
      APPEND t_fieldnames.
      t_fieldnames-name = 'Variação'."ZPMR0001-VARIACAO
      APPEND t_fieldnames.
      t_fieldnames-name = 'Perfil de Catálogo'."ZPMR0001-RBNR
      APPEND t_fieldnames.
*** Inicio - Rubenilson Pereira - 05.03.2025 - BUG169744
      t_fieldnames-name = 'Tq. Comb'."ZPMR0001-TQ_COMB
      APPEND t_fieldnames.
      t_fieldnames-name = 'Tolerância'."ZPMR0001-TOLERANCIA
      APPEND t_fieldnames.
*** Fim - Rubenilson Pereira - 05.03.2025 - BUG169744

    WHEN '0093'.

      CREATE DATA lo_dynamic_table TYPE TABLE OF zpmt0074.
      ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

      t_fieldnames-name = 'Ctg Equip'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Classe'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Tipo Veiculo'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Fabricante '.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Denom. Tipo'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Ctg. Ponto Medição'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Contador'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Item Medição'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Denominação '.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Caracteristica'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Casas Decimais'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Grp. Códigos'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Conjunto'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Criar Plano de Manutenção?'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'MarcSalto Cont'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Atividade Anual'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Trans. Val.med.prevista?'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'É Referência para vida útil?'.
      APPEND t_fieldnames.
      t_fieldnames-name = 'Grp. Autorizações'.
      APPEND t_fieldnames.

    WHEN '0094'.

      CREATE DATA lo_dynamic_table TYPE TABLE OF zpmt0075.
      ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

      t_fieldnames-name =  'Ctg. Plano Manut.'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Denominação do Tipo'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Conjunto'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Tipo de Ordem'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Ciclo'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Unidade'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Texto Plano Manutenção'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Texto Ciclo'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Tp. Ativ. Manut.'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Centro Trab. Respons'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Grp. Planej.'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Prioridade'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Tp. Roteiro'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Grp. LisTar'.
      APPEND t_fieldnames.
*      t_fieldnames-name =  'Numgrp.'.
*      APPEND t_fieldnames.
      t_fieldnames-name =  'Confirm. Obrig.'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Conf. Atrasada'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Tolerância (+)'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Conf. Antecipada'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Tolerância (-)'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Horiz. Abertura'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Interv Solic.'.
      APPEND t_fieldnames.
      t_fieldnames-name =  'Ftr modif Ciclo'.
      APPEND t_fieldnames.
    WHEN OTHERS.
  ENDCASE.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = p_local
      filetype              = 'ASC'
      write_field_separator = 'X'
      "CODEPAGE            = '8404'
    TABLES
      data_tab              = <lt_table_structure> "t_file
      fieldnames            = t_fieldnames
    EXCEPTIONS
      file_open_error       = 1
      file_write_error      = 2
      invalid_filesize      = 3
      invalid_table_width   = 4
      invalid_type          = 5
      no_batch              = 6
      unknown_error         = 7
      OTHERS                = 8.

  IF sy-subrc = 0.
    MESSAGE 'Arquivos gerados com sucesso' TYPE 'S'.
  ELSE.
    MESSAGE 'Arquivo processado com erro' TYPE 'E'.
  ENDIF.
*  DATA: r_descr TYPE REF TO cl_abap_structdescr,
*        wa_comp TYPE abap_compdescr.
*
*  r_descr ?= cl_abap_typedescr=>describe_by_data( p_estrutura ).
*
*  LOOP AT r_descr->components INTO wa_comp.
*    APPEND INITIAL LINE TO t_fieldnames ASSIGNING FIELD-SYMBOL(<fs_fieldnames>).
*    <fs_fieldnames>-name    = wa_comp-name.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LE_ARQUIVO
*&---------------------------------------------------------------------*

FORM f_le_arquivo .

  DATA: vs_filename TYPE rlgrap-filename.

*** Função para leitura do arquivo de entrada
  vs_filename = p_file.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = vs_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 28
      i_end_row               = 99999
    TABLES
      intern                  = tg_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    CLEAR tg_excel[].
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ARQ_SAIDA
*&---------------------------------------------------------------------*
FORM f_monta_arq_saida USING p_arq.

  CASE p_arq.
    WHEN '0017'.
      PERFORM f_monta_0017.
    WHEN '0093'.
      PERFORM f_monta_0093.
    WHEN '0094'.
      PERFORM f_monta_0094.
    WHEN OTHERS.
  ENDCASE.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DADOS
*&---------------------------------------------------------------------*

FORM f_mostrar_dados CHANGING p_data
                              p_alv TYPE REF TO cl_salv_table.

  DATA: lv_text    TYPE string,
        lv_tooltip TYPE string.

  DATA: lr_layout TYPE REF TO cl_salv_layout,
        ls_key    TYPE salv_s_layout_key.

  SET SCREEN 100.

  TRY.
      cl_salv_table=>factory( EXPORTING
                                r_container = cl_gui_container=>default_screen
                              IMPORTING
                                r_salv_table = p_alv
                              CHANGING
                                t_table = p_data ).
    CATCH cx_root.

  ENDTRY.

  TRY.
*      enable buttons for SALV
      p_alv->get_functions( )->set_all( abap_true ).

    CATCH cx_root.

  ENDTRY.

  TRY.

      lr_layout = p_alv->get_layout( ).

*     set the Layout Key
      ls_key-report = sy-repid.
      lr_layout->set_key( ls_key ).

*     set usage of default Layouts
      lr_layout->set_default( abap_true ).

      p_alv->get_selections( )->set_selection_mode( cl_salv_selections=>multiple ).

    CATCH cx_root.

  ENDTRY.

  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column_table.

  lr_columns = p_alv->get_columns( ).

  TRY.

      lr_column ?= lr_columns->get_column( 'CLASS_OPER' ).
      lr_column->set_short_text( TEXT-c01 ).
      lr_column->set_medium_text( TEXT-c01 ).
      lr_column->set_long_text( TEXT-c01 ).

    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'HERST' ).
      lr_column->set_short_text( TEXT-c02 ).
      lr_column->set_medium_text( TEXT-c02 ).
      lr_column->set_long_text( TEXT-c02 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'TYPBZ' ).
      lr_column->set_short_text( TEXT-c03 ).
      lr_column->set_medium_text( TEXT-c03 ).
      lr_column->set_long_text( TEXT-c03 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'CONSUMO' ).
      lr_column->set_short_text( TEXT-c04 ).
      lr_column->set_medium_text( TEXT-c04 ).
      lr_column->set_long_text( TEXT-c04 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'VARIACAO' ).
      lr_column->set_short_text( TEXT-c05 ).
      lr_column->set_medium_text( TEXT-c05 ).
      lr_column->set_long_text( TEXT-c05 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'RBNR' ).
      lr_column->set_short_text( TEXT-c06 ).
      lr_column->set_medium_text( TEXT-c06 ).
      lr_column->set_long_text( TEXT-c06 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'FAROL' ).
      lr_column->set_short_text( TEXT-c07 ).
      lr_column->set_medium_text( TEXT-c08 ).
      lr_column->set_long_text( TEXT-c09 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*** Inicio - Rubenilson Pereira - 05.03.2025 - BUG169744
  TRY.
      lr_column ?= lr_columns->get_column( 'TQ_COMB' ).
      lr_column->set_short_text( TEXT-c10 ).
      lr_column->set_medium_text( TEXT-c10 ).
      lr_column->set_long_text( TEXT-c10 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'TOLERANCIA' ).
      lr_column->set_short_text( TEXT-c11 ).
      lr_column->set_medium_text( TEXT-c11 ).
      lr_column->set_long_text( TEXT-c11 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
*** Fim - Rubenilson Pereira - 05.03.2025 - BUG169744

  TRY.
      p_alv->get_columns( )->set_optimize( abap_true ).

    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  "metodo REMOVE_COLUMN

*  display the SALV
  p_alv->display( ).

*  WRITE: space.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*

FORM f_gravar_dados USING p_arq.

  CASE p_arq.
    WHEN '0017'.

*** Inicio - Rubenilson - 17.01.25 - BUG163736
      SELECT MAX( id_fab_mod )
        FROM zpmr0001
        INTO @DATA(lv_id).
*** Fim - Rubenilson - 17.01.25 - BUG163736

      LOOP AT gt_data_0017 ASSIGNING FIELD-SYMBOL(<fs_data_0017>).
        DATA: ls_zpmr0001 LIKE zpmr0001.
        ls_zpmr0001 = CORRESPONDING #( <fs_data_0017> ).

*** Inicio - Rubenilson - 17.01.25 - BUG163736
        ADD 1 TO lv_id.
        ls_zpmr0001-id_fab_mod = lv_id.
*** Fim - Rubenilson - 17.01.25 - BUG163736

        ls_zpmr0001-usr_modif = sy-uname.

        INSERT zpmr0001 FROM ls_zpmr0001.
        IF sy-subrc = 0.
          COMMIT WORK.
          <fs_data_0017>-farol = gc_light_green.
        ELSE.
          <fs_data_0017>-farol = gc_light_red.
        ENDIF.
      ENDLOOP.

      IF gr_alv IS BOUND.
        gr_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
      ENDIF.

      READ TABLE gt_data_0017 TRANSPORTING NO FIELDS WITH KEY farol = gc_light_red.
      IF sy-subrc = 0.
        MESSAGE 'Ocorreram alguns erros ao gravar.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE 'Todos o dados foram gravados com sucesso.' TYPE 'S'.
      ENDIF.

    WHEN '0093'.

      DATA lv_erro.

      LOOP AT gt_data_0074 ASSIGNING FIELD-SYMBOL(<fs_data_0074>).
        DATA: ls_zpmt0074 LIKE zpmt0074.
        ls_zpmt0074 = CORRESPONDING #( <fs_data_0074> ).

        "Validações da transação ZPM0093
        CLEAR lv_erro.
        PERFORM f_exit_zpmt0074_0002
        IN PROGRAM zrd_zpmt0074_exit
        IF FOUND USING ls_zpmt0074 CHANGING lv_erro.
        IF lv_erro IS INITIAL.

          INSERT zpmt0074 FROM ls_zpmt0074.
          IF sy-subrc = 0.
            COMMIT WORK.
            <fs_data_0074>-farol = gc_light_green.
          ELSE.
            <fs_data_0074>-farol = gc_light_red.
          ENDIF.

        ELSE.
          <fs_data_0074>-farol = gc_light_red.
        ENDIF.
      ENDLOOP.

      IF gr_alv IS BOUND.
        gr_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
      ENDIF.

      READ TABLE gt_data_0074 TRANSPORTING NO FIELDS WITH KEY farol = gc_light_red.
      IF sy-subrc = 0.
        MESSAGE 'Ocorreram alguns erros ao gravar.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE 'Todos o dados foram gravados com sucesso.' TYPE 'S'.
      ENDIF.

    WHEN '0094'.

      LOOP AT gt_data_0075 ASSIGNING FIELD-SYMBOL(<fs_data_0075>).
        DATA: ls_zpmt0075 LIKE zpmt0075.
        ls_zpmt0075 = CORRESPONDING #( <fs_data_0075> ).

        "Validações da transação ZPM0094
        CLEAR lv_erro.
        PERFORM f_exit_zpmt0075_0002
        IN PROGRAM zrd_zpmt0075_exit
        IF FOUND USING ls_zpmt0075 CHANGING lv_erro.
        IF lv_erro IS INITIAL.

          INSERT zpmt0075 FROM ls_zpmt0075.
          IF sy-subrc = 0.
            COMMIT WORK.
            <fs_data_0075>-farol = gc_light_green.
          ELSE.
            <fs_data_0075>-farol = gc_light_red.
          ENDIF.

        ELSE.
          <fs_data_0075>-farol = gc_light_red.
        ENDIF.

      ENDLOOP.

      IF gr_alv IS BOUND.
        gr_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
      ENDIF.

      READ TABLE gt_data_0075 TRANSPORTING NO FIELDS WITH KEY farol = gc_light_red.
      IF sy-subrc = 0.
        MESSAGE 'Ocorreram alguns erros ao gravar.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE 'Todos o dados foram gravados com sucesso.' TYPE 'S'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_0017
*&---------------------------------------------------------------------*

FORM f_monta_0017 .

  DATA: "t_zpm0017_excel TYPE TABLE OF ty_0017,"zpmr0001,
  wa_layout       TYPE lvc_s_layo.

  DATA:
    lv_text    TYPE        string,
    lv_tooltip TYPE        string.

  DATA:
  lr_layout   TYPE REF TO cl_salv_layout.

  DATA:
  ls_key      TYPE        salv_s_layout_key.

  DATA: p_alv TYPE REF TO cl_salv_table.

  LOOP AT tg_excel ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE <fs_line>-col.
      WHEN 1.
        APPEND INITIAL LINE TO gt_data_0017 ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-mandt = sy-mandt.
        <fs>-class_oper = <fs_line>-value.
        <fs>-dt_criacao = sy-datum.
        <fs>-farol = gc_light_inactive.
      WHEN 2.
        <fs>-herst = <fs_line>-value.
      WHEN 3.
        <fs>-typbz = <fs_line>-value.
      WHEN 4.
        <fs>-consumo = <fs_line>-value.
      WHEN 5.
        <fs>-variacao = <fs_line>-value.
      WHEN 6.
        <fs>-rbnr = <fs_line>-value.
*** Inicio - Rubenilson pereira - 05.03.2025 - BUG169744
      WHEN 7.
        <fs>-tq_comb = <fs_line>-value.
      WHEN 8.
        <fs>-tolerancia = <fs_line>-value.
*** Fim - Rubenilson pereira - 05.03.2025 - BUG169744
    ENDCASE.
  ENDLOOP.

  SELECT * FROM zpmr0001 INTO TABLE @DATA(lt_001).

  LOOP AT gt_data_0017 ASSIGNING FIELD-SYMBOL(<fs_data>).

    READ TABLE lt_001 WITH KEY mandt = <fs_data>-mandt
                               herst = <fs_data>-herst
                               typbz = <fs_data>-typbz
                               class_oper = <fs_data>-class_oper TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      <fs_data>-farol = gc_light_yellow.
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM f_monta_0093 .

  DATA: wa_layout TYPE lvc_s_layo,
        lv_erro.

  DATA:
    lv_text    TYPE        string,
    lv_tooltip TYPE        string.

  DATA:
  lr_layout   TYPE REF TO cl_salv_layout.

  DATA:
  ls_key      TYPE        salv_s_layout_key.

  DATA: p_alv TYPE REF TO cl_salv_table.

  LOOP AT tg_excel ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE <fs_line>-col.
      WHEN 1.
        APPEND INITIAL LINE TO gt_data_0074 ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-mandt = sy-mandt.
        <fs>-eqtyp = <fs_line>-value.
        <fs>-us_criacao = sy-uname.
        <fs>-dt_criacao = sy-datum.
        <fs>-hr_criacao = sy-uzeit.
        <fs>-farol = gc_light_inactive.
      WHEN 2.
        <fs>-klasse = <fs_line>-value.
      WHEN 3.
        <fs>-fleet_cat = <fs_line>-value.
      WHEN 4.
        <fs>-herst = <fs_line>-value.
      WHEN 5.
        <fs>-typbz = <fs_line>-value.
      WHEN 6.
        <fs>-mptyp = <fs_line>-value.
      WHEN 7.
        <fs>-indct = <fs_line>-value.
      WHEN 8.
        <fs>-psort = <fs_line>-value.
      WHEN 9.
        <fs>-pttxt = <fs_line>-value.
      WHEN 10.
        <fs>-atnam = <fs_line>-value.
      WHEN 11.
        <fs>-casas_decimais = <fs_line>-value.
      WHEN 12.
        <fs>-codgr = <fs_line>-value.
      WHEN 13.
        <fs>-locas = <fs_line>-value.
      WHEN 14.
        <fs>-zcria = <fs_line>-value.
      WHEN 15.
        <fs>-cjumc = <fs_line>-value.
      WHEN 16.
        <fs>-pyeac = <fs_line>-value.
      WHEN 17.
        <fs>-indtr_ref = <fs_line>-value.
      WHEN 18.
        <fs>-indtr = <fs_line>-value.
      WHEN 19.
        <fs>-begru = <fs_line>-value.

    ENDCASE.
  ENDLOOP.

  SELECT * FROM zpmt0074 INTO TABLE @DATA(lt_0074).

  LOOP AT gt_data_0074 ASSIGNING FIELD-SYMBOL(<fs_data>).

    READ TABLE lt_0074 WITH KEY mandt = <fs_data>-mandt
                                eqtyp = <fs_data>-eqtyp
                                klasse    = <fs_data>-klasse
                                fleet_cat = <fs_data>-fleet_cat
                                herst     = <fs_data>-herst
                                typbz     = <fs_data>-typbz
                                mptyp     = <fs_data>-mptyp
                                indct     = <fs_data>-indct
                                psort     = <fs_data>-psort
                                pttxt     = <fs_data>-pttxt
                                atnam     = <fs_data>-atnam TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      <fs_data>-farol = gc_light_yellow.
    ENDIF.

    "Validações da transação ZPM0093
    CLEAR lv_erro.
    PERFORM f_exit_zpmt0074_0002
    IN PROGRAM zrd_zpmt0074_exit
    IF FOUND USING <fs_data> CHANGING lv_erro.
    IF lv_erro IS NOT INITIAL.
      <fs_data>-farol = gc_light_red.
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM f_monta_0094 .

  DATA: wa_layout TYPE lvc_s_layo,
        lv_erro.

  DATA:
    lv_text    TYPE        string,
    lv_tooltip TYPE        string.

  DATA:
  lr_layout   TYPE REF TO cl_salv_layout.

  DATA:
  ls_key      TYPE        salv_s_layout_key.

  DATA: p_alv TYPE REF TO cl_salv_table.

  LOOP AT tg_excel ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE <fs_line>-col.
      WHEN 1.
        APPEND INITIAL LINE TO gt_data_0075 ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-mandt = sy-mandt.
        <fs>-mptyp = <fs_line>-value.
        <fs>-us_criacao = sy-uname.
        <fs>-dt_criacao = sy-datum.
        <fs>-hr_criacao = sy-uzeit.
        <fs>-farol = gc_light_inactive.
      WHEN 2.
        <fs>-typbz = <fs_line>-value.
      WHEN 3.
        <fs>-locas = <fs_line>-value.
      WHEN 4.
        <fs>-auart = <fs_line>-value.
      WHEN 5.
        <fs>-zykl1 = <fs_line>-value.
      WHEN 6.
        <fs>-zeieh = <fs_line>-value.
      WHEN 7.
        <fs>-wptxt = <fs_line>-value.
      WHEN 8.
        <fs>-pak_text = <fs_line>-value.
      WHEN 9.
        <fs>-ilart = <fs_line>-value.
      WHEN 10.
        <fs>-gewerk = <fs_line>-value.
      WHEN 11.
        <fs>-wpgrp = <fs_line>-value.
      WHEN 12.
        <fs>-priok = <fs_line>-value.
      WHEN 13.
        <fs>-plnty = <fs_line>-value.
      WHEN 14.
        <fs>-plnnr = <fs_line>-value.
      WHEN 15.
        <fs>-call_confirm = <fs_line>-value.
      WHEN 16.
        <fs>-vspos = <fs_line>-value.
      WHEN 17.
        <fs>-topos = <fs_line>-value.
      WHEN 18.
        <fs>-vsneg = <fs_line>-value.
      WHEN 19.
        <fs>-toneg = <fs_line>-value.
      WHEN 20.
        <fs>-horiz = <fs_line>-value.
      WHEN 21.
        <fs>-abrho = <fs_line>-value.
      WHEN 22.
        <fs>-sfakt = <fs_line>-value.


    ENDCASE.
  ENDLOOP.

  SELECT * FROM zpmt0075 INTO TABLE @DATA(lt_0075).

  LOOP AT gt_data_0075 ASSIGNING FIELD-SYMBOL(<fs_data>).

    READ TABLE lt_0075 WITH KEY mandt = <fs_data>-mandt
                                typbz = <fs_data>-typbz
                                mptyp = <fs_data>-mptyp
                                locas = <fs_data>-locas
                                auart = <fs_data>-auart
                                zykl1 = <fs_data>-zykl1
                                zeieh = <fs_data>-zeieh
                                TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      <fs_data>-farol = gc_light_yellow.
    ENDIF.

    "Validações da transação ZPM0093
    CLEAR lv_erro.
    PERFORM f_exit_zpmt0075_0002
    IN PROGRAM zrd_zpmt0075_exit
    IF FOUND USING <fs_data> CHANGING lv_erro.
    IF lv_erro IS NOT INITIAL.
      <fs_data>-farol = gc_light_red.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.

  IF sy-ucomm = 'EXIT' OR
     sy-ucomm = 'CANCEL'.
    LEAVE TO SCREEN 0.
  ELSEIF sy-ucomm = 'BACK'.
    SET SCREEN 0.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.

      IF p_0017 IS NOT INITIAL.
        IF gt_data_0017[] IS INITIAL.
          MESSAGE 'Erro ao gravar dados. Por favor, importe a planilha antes de salvar as informações.' TYPE 'E'." DISPLAY LIKE 'E'.
        ELSE.
          PERFORM f_gravar_dados USING '0017'.
        ENDIF.
      ENDIF.

      IF p_0093 IS NOT INITIAL.
        IF gt_data_0074[] IS INITIAL.
          MESSAGE 'Erro ao gravar dados. Por favor, importe a planilha antes de salvar as informações.' TYPE 'E'." DISPLAY LIKE 'E'.
        ELSE.
          PERFORM f_gravar_dados USING '0093'.
        ENDIF.
      ENDIF.

      IF p_0094 IS NOT INITIAL.
        IF gt_data_0075[] IS INITIAL.
          MESSAGE 'Erro ao gravar dados. Por favor, importe a planilha antes de salvar as informações.' TYPE 'E'." DISPLAY LIKE 'E'.
        ELSE.
          PERFORM f_gravar_dados USING '0094'.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.



ENDMODULE.
