*&---------------------------------------------------------------------*
*& Report zpmr0086
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpmr0086.

TABLES: equi, zpmt0078_out.

DATA: gt_saida TYPE TABLE OF zpmt0078_out.

DATA:
  gr_table     TYPE REF TO cl_salv_table,
  gr_functions TYPE REF TO cl_salv_functions,
  gr_columns   TYPE REF TO cl_salv_columns,
  gr_layout    TYPE REF TO cl_salv_layout.

DATA: p_screen1000(1) TYPE c.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                  s_centro  FOR zpmt0078_out-centro OBLIGATORY,
                  s_equip   FOR zpmt0078_out-equip,
                  S_ccusto  FOR zpmt0078_out-centro_custo,
                  s_emploc  FOR zpmt0078_out-emp_locacao,
                  s_cspot   FOR zpmt0078_out-contrato_spot,
                  s_ccorp   FOR zpmt0078_out-contrato_corp,
                  s_dtini   FOR zpmt0078_out-ini_contrato,
                  s_dtfim   FOR zpmt0078_out-fim_contrato.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM selecionar_dados.
  "142670 Efetuar ajustes na tela de lançamento de contrato PSA
  IF gt_saida[] IS INITIAL.

    CLEAR: p_screen1000.
    p_screen1000 = 'X'.

    EXPORT p_screen1000 = p_screen1000 TO MEMORY ID 'ZSCREEN1000'.
    p_screen1000 = 'X'.
    MESSAGE 'Dados não encontrados!' TYPE 'I'.
  ELSE.
    PERFORM exibe_alv.
  ENDIF.



FORM selecionar_dados.


  IF s_equip IS NOT INITIAL."Remove os Zeros a Esqueda

    LOOP AT s_equip ASSIGNING FIELD-SYMBOL(<_TRIM>).
      IF <_TRIM>-low IS NOT INITIAL.
      SHIFT <_TRIM>-low LEFT DELETING LEADING '0'.
      ENDIF.
      IF <_TRIM>-high IS NOT INITIAL.
      SHIFT <_TRIM>-high LEFT DELETING LEADING '0'.
      ENDIF.
    ENDLOOP.

  ENDIF.

  SELECT
  equip,
  equip_desc,
  centro,
  centro_desc,
  centro_custo,
  centro_custo_desc,
  emp,
  emp_desc,
  emp_locacao,
  emp_locacao_desc,
  valor_locacao,
  contrato_spot,
  contrato_spot_desc,
  contrato_corp,
  contrato_corp_desc,
  ini_contrato,
  fim_contrato
  FROM zpmt0078
  WHERE equip IN @s_equip
    AND centro IN @s_centro
    AND centro_custo IN @s_ccusto
    AND emp_locacao IN @s_emploc
    AND contrato_spot IN @s_cspot
    AND contrato_corp IN @s_ccorp
    AND ini_contrato IN @s_dtini
    AND fim_contrato IN @s_dtfim
  INTO TABLE @gt_saida.

ENDFORM.

FORM exibe_alv.

  CHECK gt_saida[] IS NOT INITIAL.

  " Criar uma instância da tabela ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_saida ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao criar a tabela ALV' TYPE 'E'.
  ENDTRY.

  " Configurar funções
  TRY.
      gr_functions = gr_table->get_functions( ).
      gr_functions->set_all( abap_true ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao configurar as funções da tabela ALV' TYPE 'E'.
  ENDTRY.

  " Configurar colunas
  TRY.
      gr_columns = gr_table->get_columns( ).
      gr_columns->set_optimize( abap_true ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao configurar as colunas da tabela ALV' TYPE 'E'.
  ENDTRY.

  " Configurar layout
  TRY.
      gr_layout = gr_table->get_layout( ).
*      gr_layout->set_key( 'BACKGROUND_ALV' ).
      gr_layout->set_default( abap_true ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao configurar o layout da tabela ALV' TYPE 'E'.
  ENDTRY.

*  "Mudar label da coluna aprovador
*  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
*        lr_column  TYPE REF TO cl_salv_column_table.
*
*  DATA: short_text  TYPE scrtext_s VALUE 'Aprov.Inv.',
*        medium_text TYPE scrtext_m VALUE 'Aprov. Inválido',
*        long_text   TYPE SCRTEXT_l VALUE 'Aprovador Inválido'.
*
*
*  lr_columns = gr_table->get_columns( ).
*
*  TRY.
*      lr_column ?= lr_columns->get_column( 'APROVADOR' ).
*      lr_column->set_short_text( short_text ).
*      lr_column->set_medium_text( medium_text ).
*      lr_column->set_long_text( long_text ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.


  " Exibir a tabela ALV
  TRY.
      gr_table->display( ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao exibir a tabela ALV' TYPE 'E'.
  ENDTRY.


ENDFORM.
