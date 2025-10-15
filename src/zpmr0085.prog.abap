*&---------------------------------------------------------------------*
*& Report zpmr0085
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpmr0085.


DATA:
  gr_table     TYPE REF TO cl_salv_table,
  gr_functions TYPE REF TO cl_salv_functions,
  gr_columns   TYPE REF TO cl_salv_columns,
  gr_layout    TYPE REF TO cl_salv_layout.

TYPES: BEGIN OF ty_saida,

         bukrs     TYPE bukrs,
         werks     TYPE werks,
         Aufnr     TYPE aufnr,
         ernam     TYPE ernam,
         erdat     TYPE auferfdat,
         phas1     TYPE aufphas1,
         phas2     TYPE aufphas2,
         phas3     TYPE aufphas3,

         objnr     TYPE objnr,
         belnr     TYPE bp_belnr,
         vorga     TYPE bp_vorgang,
         wtges     TYPE bp_wgt,
         cpudt     TYPE cpudt,
         usnam     TYPE usnam,
         sgtext    TYPE sgtxt,

         genname   TYPE genname,
         genvname  TYPE genvname,
         gendatum  TYPE gendatum,
         gentime   TYPE gentime,

         aprovador TYPE xflag,

       END OF ty_saida.

DATA: gt_saida TYPE TABLE OF ty_saida.

DATA: lr_aprov TYPE RANGE OF usnam.


START-OF-SELECTION.

  "Aprovadores
  SELECT aprovador, usua_subst
  FROM zpmr0002
  INTO TABLE @DATA(lt_002).


  LOOP AT lt_002 INTO DATA(wa).

    APPEND VALUE #( sign = 'I' option = 'EQ' low = wa-aprovador ) TO lr_aprov.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = wa-usua_subst ) TO lr_aprov.

  ENDLOOP.


  " Selecionar dados da tabela

  SELECT
      au~bukrs,
      au~werks,
      au~Aufnr,
      au~ernam,
      au~erdat,

      au~phas1,
      au~phas2,
      au~phas3,

      bp~objnr,
      bp~belnr,
      bp~vorga,
      bp~wtges,
      bp~cpudt,
      bp~usnam,
      bp~sgtext,

      ih~genname,
      ih~genvname,
      ih~gendatum,
      ih~gentime

    FROM ihsg AS ihsg
    INNER JOIN bp_v_eg AS bp
    ON bp~objnr = ihsg~objnr

    INNER JOIN aufk AS au
    ON ihsg~objnr = au~objnr

    INNER JOIN ihgns AS ih
    ON ih~objnr = au~objnr

    WHERE au~phas0 IS INITIAL
    INTO TABLE @gt_saida.

  SORT gt_saida.
  DELETE ADJACENT DUPLICATES FROM gt_saida COMPARING ALL FIELDS.

  LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    IF <fs_saida>-genvname NOT IN lr_aprov.

      <fs_saida>-aprovador = 'X'.

    ENDIF.

    IF <fs_saida>-usnam NOT IN lr_aprov.

      <fs_saida>-aprovador = 'X'.
      CLEAR: <fs_saida>-cpudt,
             <fs_saida>-gentime,
             <fs_saida>-SGTeXT,
             <fs_saida>-genname,
             <fs_saida>-gendatum,
             <fs_saida>-genvname.
    ENDIF.

  ENDLOOP.


  SORT gt_saida.
  DELETE ADJACENT DUPLICATES FROM gt_saida COMPARING ALL FIELDS.


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

  "Mudar label da coluna aprovador
  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column_table.

  DATA: short_text  TYPE scrtext_s VALUE 'Aprov.Inv.',
        medium_text TYPE scrtext_m VALUE 'Aprov. Inválido',
        long_text   TYPE SCRTEXT_l VALUE 'Aprovador Inválido'.


  lr_columns = gr_table->get_columns( ).

  TRY.
      lr_column ?= lr_columns->get_column( 'APROVADOR' ).
      lr_column->set_short_text( short_text ).
      lr_column->set_medium_text( medium_text ).
      lr_column->set_long_text( long_text ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  " Exibir a tabela ALV
  TRY.
      gr_table->display( ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao exibir a tabela ALV' TYPE 'E'.
  ENDTRY.
