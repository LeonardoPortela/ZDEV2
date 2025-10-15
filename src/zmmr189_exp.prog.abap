*&---------------------------------------------------------------------*
*& Report ZMMR189_EXP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr189_exp.

                                                            "ZFIE0204

TYPES: BEGIN OF ty_zfie0204,
         periodo  TYPE zfie0204-col_a,
         unidade  TYPE zfie0204-col_b,
         regfont  TYPE zfie0204-col_c,
         descfont TYPE zfie0204-col_d,
         combutil TYPE zfie0204-col_e,
         qtdcons  TYPE zfie0204-col_f,
       END OF ty_zfie0204.

DATA: it_saida TYPE STANDARD TABLE OF ty_zfie0204 WITH HEADER LINE.
DATA: go_alv        TYPE REF TO cl_salv_table.
DATA: lr_columns    TYPE REF TO cl_salv_columns.
DATA: lr_column     TYPE REF TO cl_salv_column.
DATA: lr_functions  TYPE REF TO cl_salv_functions_list.
DATA: gr_display    TYPE REF TO cl_salv_display_settings.
DATA: gr_selections TYPE REF TO cl_salv_selections.

START-OF-SELECTION.

**Data Selection
  PERFORM select_table_sflight.

**Classe ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = it_saida[] ). "Internal Table

    CATCH cx_salv_msg.
  ENDTRY.

**Habilita o Menu Stander
  lr_functions = go_alv->get_functions( ).
  lr_functions->set_all( 'X' ).

**Customiza Colunas
  lr_columns = go_alv->get_columns( ).
  "lr_columns->set_optimize( ' ' ).

**Colunas
  lr_column = lr_columns->get_column( 'PERIODO' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'Per.' ).
  lr_column->set_medium_text( 'Período' ).
  lr_column->set_long_text( 'Período (AAAA-MM ou AAAA)' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'UNIDADE' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'Unidade' ).
  lr_column->set_medium_text( 'Unidade' ).
  lr_column->set_long_text( 'Unidade' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'REGFONT' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'Reg.fonte' ).
  lr_column->set_medium_text( 'Registro da fonte' ).
  lr_column->set_long_text( 'Registro da fonte' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'DESCFONT' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'Desc.fonte' ).
  lr_column->set_medium_text( 'Descrição da fonte' ).
  lr_column->set_long_text( 'Descrição da fonte' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'COMBUTIL' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'Comb.Util' ).
  lr_column->set_medium_text( 'Comb.Utilizado' ).
  lr_column->set_long_text( 'Combustível Utilizado' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'QTDCONS' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'Qtd.Cons.' ).
  lr_column->set_medium_text( 'Qtd.consumida' ).
  lr_column->set_long_text( 'Quantidade consumida' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

**Posição das Colunas
  lr_columns->set_column_position(  columnname = 'PERIODO'  position = 1 ).
  lr_columns->set_column_position(  columnname = 'UNIDADE'  position = 2 ).
  lr_columns->set_column_position(  columnname = 'REGFONT'  position = 3 ).
  lr_columns->set_column_position(  columnname = 'DESCFONT' position = 4 ).
  lr_columns->set_column_position(  columnname = 'COMBUTIL' position = 5 ).
  lr_columns->set_column_position(  columnname = 'QTDCONS'  position = 6 ).

**Habilita Zebra style
  gr_display = go_alv->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).

**Gera ALV
  go_alv->display( ).

FORM select_table_sflight.
*  SELECT COL_A,
*    COL_B,
*    COL_C,
*    COL_D,
*    COL_E,
*    COL_F
*  FROM zfie0204 AS a
*  WHERE 1 = 1
**    AND a~carrid EQ 'AA'
**    AND a~connid EQ 17
**    AND a~fldate EQ '20100901'.
*  INTO TABLE @it_SAIDA
*  UP TO 10 ROWS. "Limita para 10 Linhas

  it_saida-combutil = 'Gasolina de Aviação'.
  it_saida-descfont = 'Teste'.
  it_saida-periodo = '2015-02'.
  it_saida-qtdcons = 8888.
  it_saida-regfont = 'Teste'.
  it_saida-unidade = 'Fazenda Carolinas'.
  APPEND it_saida.

  SORT it_saida ASCENDING.

ENDFORM.
