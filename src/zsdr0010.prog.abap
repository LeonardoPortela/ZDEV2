*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDR0010                                                *
* Descrição  : Aplicação de Fornecedores no RE                         *
* Módulo     : FI                                Transação:            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 03/06/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*


REPORT  zsdr0010.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: likp           ,
        zreg_exportacao,
        zdoc_exp       ,
        lfa1           .

TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:


     BEGIN OF ty_zdoc_exp                                ,
       vbeln            TYPE zdoc_exp-vbeln              ,
       id_nomeacao_tran TYPE zdoc_exp-id_nomeacao_tran   ,
       ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
       ds_porto         TYPE znom_transporte-ds_porto    ,
       ds_terminal      TYPE znom_transporte-ds_terminal ,
       id_dde           TYPE zdde-id_dde                 ,
       id_registro_expo TYPE zreg_exportacao-id_registro_expo,
       nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
     END   OF ty_zdoc_exp,

     BEGIN OF ty_zdde                                    ,
       id_dde           TYPE zdde-id_dde                 ,
       dt_dde           TYPE zdde-dt_dde                 ,
       nr_dde           TYPE zdde-nr_dde                 ,
     END OF ty_zdde                                      ,

     BEGIN OF ty_zdoc_nf_produtor  ,
       vbeln            TYPE zdoc_nf_produtor-vbeln      ,
       docnum_prod      TYPE zdoc_nf_produtor-docnum_prod,
       menge            TYPE zdoc_nf_produtor-menge      ,
       parid            TYPE j_1bnfdoc-parid             ,
       partyp           TYPE j_1bnfdoc-partyp            ,
       nfenum           TYPE j_1bnfdoc-nfenum            ,
       nfnum            TYPE j_1bnfdoc-nfnum             ,
     END OF ty_zdoc_nf_produtor                          ,

     BEGIN OF ty_saida                                   ,
       vbeln            TYPE likp-vbeln                  ,
       nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
       ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
       id_nomeacao_tran TYPE zdoc_exp-id_nomeacao_tran   ,
       notas            TYPE string                      ,
       lifnr            TYPE lfa1-lifnr                  ,
       name1            TYPE lfa1-name1                  ,
       stcd1            TYPE lfa1-stcd1                  ,
       regio            TYPE lfa1-regio                  ,
       quantidade       TYPE zdoc_nf_produtor-menge      ,
     END OF ty_saida                                     .


TYPES: BEGIN OF ty_estrutura.
INCLUDE TYPE slis_fieldcat_main.
INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: t_saida_excel            TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE INITIAL SIZE 0   ,
      t_saida            TYPE TABLE OF ty_saida      ,
      t_likp             TYPE TABLE OF likp          ,
      t_zdoc_nf_produtor TYPE TABLE OF ty_zdoc_nf_produtor,
      t_zdoc_exp         TYPE TABLE OF ty_zdoc_exp   ,
      t_lfa1             TYPE TABLE OF lfa1          ,
      t_bdc              TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab          TYPE TABLE OF bdcmsgcoll .


*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_likp             TYPE likp                           ,
      wa_zdoc_nf_produtor TYPE ty_zdoc_nf_produtor            ,
      wa_zdoc_exp         TYPE ty_zdoc_exp                    ,
      wa_lfa1             TYPE lfa1                           ,
      wa_saida            TYPE ty_saida                       ,
      wa_cont             TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv              TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
      wa_layout           TYPE lvc_s_layo                     . " Layout da Lista / Fim do DATA


*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: it_fcat TYPE TABLE OF lvc_s_fcat    ," lvc_s_fcat,
      s_variant TYPE disvariant           , " Tabela Estrutura colunas relatorio
      t_top     TYPE slis_t_listheader    ,
      xs_events TYPE slis_alv_event       ,
      events    TYPE slis_t_event         ,
      t_print   TYPE slis_print_alv       ,
      v_report  LIKE sy-repid             ,
      t_sort    TYPE slis_t_sortinfo_alv WITH HEADER LINE.


*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: p_vbeln  FOR likp-vbeln                      ,
                p_reg_ex FOR zreg_exportacao-nr_registro_expo,
                p_navio  FOR zdoc_exp-id_nomeacao_tran       ,
                p_lifnr  FOR lfa1-lifnr.


SELECTION-SCREEN: END OF BLOCK b1.

*Início Alteração Ricardo Furst.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
DATA: vg_repid          LIKE sy-repid,
      vg_variant        TYPE disvariant.

INITIALIZATION.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA : mes_inicial TYPE c LENGTH 2,
         mes_final   TYPE c LENGTH 2,
         ano_inicial TYPE c LENGTH 4,
         ano_final   TYPE c LENGTH 4.

  PERFORM:  f_iniciar_variaves ,
            f_seleciona_dados  , " Form seleciona dados
            f_saida            , " Form de saida
            f_alv              .

  "imprimir                                ,
  "gera_excel                              .
  "          f_imprime_dados                         . " Form ALV

  CALL SCREEN 0100.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Form para selecionar os dados e relacionar as tabelas.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.


  REFRESH: t_likp            ,
           t_zdoc_exp        ,
           t_zdoc_nf_produtor.

  "
  SELECT *
    FROM likp
    INTO TABLE t_likp
   WHERE vbeln IN p_vbeln.

  "-----Documento Exportação------------
  SELECT e~vbeln e~id_nomeacao_tran t~ds_nome_transpor  t~ds_porto t~ds_terminal e~id_dde e~id_registro_expo e~nr_registro_expo
    FROM zdoc_exp AS e
   INNER JOIN znom_transporte AS t ON t~id_nomeacao_tran EQ e~id_nomeacao_tran
    INTO TABLE t_zdoc_exp
     FOR ALL ENTRIES IN t_likp
   WHERE vbeln EQ t_likp-vbeln
     AND nr_registro_expo   IN p_reg_ex
     AND e~id_nomeacao_tran IN p_navio.

  SELECT z~vbeln z~docnum_prod z~menge j~parid j~partyp j~nfenum j~nfnum
    FROM zdoc_nf_produtor AS z
   INNER JOIN j_1bnfdoc AS j ON j~docnum EQ z~docnum_prod
    INTO TABLE t_zdoc_nf_produtor
     FOR ALL ENTRIES IN t_likp
   WHERE z~vbeln  EQ t_likp-vbeln
     AND j~parid IN p_lifnr.

  SELECT *
    FROM lfa1
    INTO TABLE t_lfa1
     FOR ALL ENTRIES IN t_zdoc_nf_produtor
   WHERE lifnr EQ t_zdoc_nf_produtor-parid.

ENDFORM.                    "f_seleciona_dados



*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  SORT: t_likp             BY vbeln,
        t_zdoc_exp         BY vbeln,
        t_zdoc_nf_produtor BY vbeln parid,
        t_lfa1             BY lifnr.


  DATA: vl_parid   TYPE j_1bnfdoc-parid,
        vl_notas   TYPE string,
        vl_soma    TYPE zdoc_nf_produtor-menge,
        nfenum_aux TYPE zdoc_nf_produtor-docnum_prod,
        nfenum     TYPE string,
        vl_cont    TYPE i.

  LOOP AT t_likp INTO wa_likp.

    "READ TABLE t_zdoc_exp INTO wa_zdoc_exp WITH KEY vbeln = wa_likp-vbeln BINARY SEARCH.
    LOOP AT t_zdoc_exp INTO wa_zdoc_exp WHERE vbeln = wa_likp-vbeln.

      wa_saida-vbeln            = wa_likp-vbeln               .
      wa_saida-nr_registro_expo = wa_zdoc_exp-nr_registro_expo.
      wa_saida-ds_nome_transpor = wa_zdoc_exp-ds_nome_transpor.
      wa_saida-id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.

      CLEAR: vl_parid, vl_notas.

      vl_soma = 0.
      vl_cont = 0.

      LOOP AT t_zdoc_nf_produtor INTO wa_zdoc_nf_produtor WHERE vbeln = wa_likp-vbeln.
        vl_cont = vl_cont + 1.

        IF vl_parid IS INITIAL.
          vl_parid = wa_zdoc_nf_produtor-parid.

          READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zdoc_nf_produtor-parid BINARY SEARCH.

        ENDIF.

        IF vl_parid = wa_zdoc_nf_produtor-parid .

          IF vl_notas IS INITIAL .

            if wa_zdoc_nf_produtor-nfenum is initial.

              "Retirar os Zeros a Esquerda
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wa_zdoc_nf_produtor-nfnum
                IMPORTING
                  output = nfenum.
            else.

              "Retirar os Zeros a Esquerda
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wa_zdoc_nf_produtor-nfenum
                IMPORTING
                  output = nfenum.
            endif.


            CONDENSE nfenum NO-GAPS.
            vl_notas = nfenum.
          ELSE.
            "Retirar os Zeros a Esquerda
            IF wa_zdoc_nf_produtor-nfenum IS NOT INITIAL.
              if wa_zdoc_nf_produtor-nfenum is initial.
                "Retirar os Zeros a Esquerda
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_zdoc_nf_produtor-nfnum
                  IMPORTING
                    output = nfenum.

              else.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_zdoc_nf_produtor-nfenum
                  IMPORTING
                    output = nfenum.

              endif.

              CONDENSE nfenum NO-GAPS.

              CONCATENATE vl_notas ';' nfenum INTO vl_notas.

            ENDIF.

          ENDIF.

          vl_soma = vl_soma + wa_zdoc_nf_produtor-menge.

        ELSE.

          wa_saida-quantidade = vl_soma.
          wa_saida-notas      = vl_notas.
          wa_saida-lifnr      = wa_lfa1-lifnr.
          wa_saida-name1      = wa_lfa1-name1.
          wa_saida-stcd1      = wa_lfa1-stcd1.
          wa_saida-regio      = wa_lfa1-regio.

          CLEAR: vl_notas,
                 vl_soma .

          APPEND wa_saida TO t_saida.

          CLEAR : wa_saida.

          READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zdoc_nf_produtor-parid BINARY SEARCH.


          if wa_zdoc_nf_produtor-nfenum is initial.

            "Retirar os Zeros a Esquerda
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_zdoc_nf_produtor-nfnum
              IMPORTING
                output = nfenum.

          else.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_zdoc_nf_produtor-nfenum
              IMPORTING
                output = nfenum.

          endif.

          "Tirar espaços a esquerda
          CONDENSE nfenum NO-GAPS.
          "Tirar zeros
          "shift nfenum left deleting leading '0'.

          vl_parid                  = wa_zdoc_nf_produtor-parid.
          vl_notas                  = nfenum."wa_zdoc_nf_produtor-nfenum.
          vl_soma                   = wa_zdoc_nf_produtor-menge.

          wa_saida-vbeln            = wa_zdoc_exp-vbeln           .
          wa_saida-nr_registro_expo = wa_zdoc_exp-nr_registro_expo.
          wa_saida-id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
          wa_saida-ds_nome_transpor = wa_zdoc_exp-ds_nome_transpor.

        ENDIF.

      ENDLOOP.
      IF vl_cont >= 1.
        wa_saida-quantidade       = vl_soma.
        wa_saida-notas            = vl_notas.
        wa_saida-lifnr            = wa_lfa1-lifnr.
        wa_saida-name1            = wa_lfa1-name1.
        wa_saida-regio            = wa_lfa1-regio.
        wa_saida-stcd1            = wa_lfa1-stcd1.
        wa_saida-vbeln            = wa_zdoc_exp-vbeln           .
        wa_saida-nr_registro_expo = wa_zdoc_exp-nr_registro_expo.
        wa_saida-ds_nome_transpor = wa_zdoc_exp-ds_nome_transpor.
        wa_saida-id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.


        APPEND wa_saida TO t_saida.
      ENDIF.

      CLEAR : wa_saida, wa_lfa1.

    ENDLOOP.

  ENDLOOP.


ENDFORM.                    " F_SAIDA

DATA: h_excel TYPE ole2_object, " Excel object
      h_mapl  TYPE ole2_object, " list of workbooks
      h_map   TYPE ole2_object, " workbook
      h_zl    TYPE ole2_object, " cell
      h_f     TYPE ole2_object. " font
*&---------------------------------------------------------------------*
*&      Form  GERA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM gera_excel .


  t_saida_excel[] = t_saida[].

  IF t_saida_excel[] IS INITIAL.
    MESSAGE w000(z01) WITH 'Não ha dados para Gerar EXCEL !' .
    CHECK NOT t_saida_excel[] IS INITIAL.
    "    stop.
  ENDIF.

  DATA : h        TYPE i,
         vl_i     TYPE i,
         vl_vbeln TYPE likp-vbeln,
         cnpj     type c length 15.

* start Excel
  CREATE OBJECT h_excel 'EXCEL.APPLICATION'.
*  PERFORM ERR_HDL.

  SET PROPERTY OF h_excel  'Visible' = 1.

* tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = text-008
    EXCEPTIONS
      OTHERS = 1.

* get list of workbooks, initially empty
  CALL METHOD OF h_excel 'Workbooks' = h_mapl.
  PERFORM err_hdl.
* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map.
  PERFORM err_hdl.
* tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
*           PERCENTAGE = 0
       text       = text-009
     EXCEPTIONS
       OTHERS     = 1.

  CALL METHOD OF h_excel 'Worksheets' = h_mapl." EXPORTIN    G #1 = 2.

* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map
    EXPORTING
    #1 = 2.

* tell user what is going on
  SET PROPERTY OF h_map 'NAME' = 'COPY'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = text-009
    EXCEPTIONS
      OTHERS = 1.

  PERFORM fill_cell USING 1 1 1 'Aplicação de Fornecedores no RE'.

  CLEAR vl_vbeln.

  LOOP AT t_saida_excel.
    h = sy-tabix + 1.

    IF vl_vbeln <>  t_saida_excel-vbeln.
      vl_vbeln  = t_saida_excel-vbeln.

      "--->Cabeçalho<-------------------------------
      IF h EQ 2.
        PERFORM fill_cell USING h 1  1 'Doc. Remessa'(002) .
        vl_i = h + 1.
        PERFORM fill_cell USING vl_i 1  1 'Re'(002)           .
        vl_i = h + 2.
        PERFORM fill_cell USING vl_i 1  1 'Nome do Navio'(003).
      ELSE.
        vl_i = vl_i + 2.
        PERFORM fill_cell USING vl_i 1  1 'Doc. Remessa'(002) .
        vl_i = vl_i + 1.
        PERFORM fill_cell USING vl_i 1  1 'Re'(002)           .
        vl_i = vl_i + 1.
        PERFORM fill_cell USING vl_i 1  1 'Nome do Navio'(003).
      ENDIF.

      vl_i = vl_i - 2.
      PERFORM fill_cell USING vl_i 2  0 t_saida_excel-vbeln           .
      vl_i = vl_i + 1.
      PERFORM fill_cell USING vl_i 2  0 t_saida_excel-nr_registro_expo.
      vl_i = vl_i + 1.
      PERFORM fill_cell USING vl_i 2  0 t_saida_excel-ds_nome_transpor.

      vl_i = vl_i + 1.
    ENDIF.
    "--->Detalhe<-------------------------------
    vl_i = vl_i + 1.
    PERFORM fill_cell USING vl_i 1  1 'Fornecedor'(004).
    PERFORM fill_cell USING vl_i 2  1 'CNPJ'(005).
    PERFORM fill_cell USING vl_i 3  1 'Estado Produtor'(006).
    PERFORM fill_cell USING vl_i 4  1 'Quantidade'(007).

    vl_i = vl_i + 1.
    concatenate '''' t_saida_excel-stcd1 into cnpj.

    PERFORM fill_cell USING vl_i 1  0 t_saida_excel-name1.
    PERFORM fill_cell USING vl_i 2  0 cnpj."t_saida_excel-stcd1.
    PERFORM fill_cell USING vl_i 3  0 t_saida_excel-regio.
    PERFORM fill_cell USING vl_i 4  0 t_saida_excel-quantidade.

    "--->Notas Vinculadas<-------------------------------
    vl_i = vl_i + 2.
    PERFORM fill_cell USING vl_i 1  1 'Notas Vinculadas'(008).

    vl_i = vl_i + 1.
    PERFORM fill_cell USING vl_i 1  0 t_saida_excel-notas.


  ENDLOOP.

  FREE OBJECT h_excel.

  MESSAGE i000(z01) WITH 'Gerado para o EXCEL com sucesso !' .

ENDFORM.                    "gera_excel


*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    value(p_flag)
                           value(p_fnam)
                           value(p_fval).

  CLEAR t_bdc.
  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.
  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
  wa_layout-no_headers = ' '.
  "wa_layout-grid_title = 'Flights'.
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c           .
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 't_saida'.
  wl_fcat-fieldname = p_campo   .
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot     .
  wl_fcat-no_zero   = p_zero    .
  wl_fcat-outputlen = p_tam     .

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA wa_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
            IMPORTING e_row_id
                      e_column_id
                      es_row_no                      ,

            zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
                e_object e_interactive                   ,

            zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING
                 e_ucomm.


ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


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
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
      WHEN 'EXPORT'  .
        PERFORM gera_excel.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT


*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = t_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.


ENDFORM.                    " Z_HANDLE_HOTSPOT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
        c_button_normal           TYPE i VALUE 0        ,
        c_menu_and_default_button TYPE i VALUE 1        ,
        c_menu                    TYPE i VALUE 2        ,
        c_separator               TYPE i VALUE 3        ,
        c_radio_button            TYPE i VALUE 4        ,
        c_checkbox                TYPE i VALUE 5        ,
        c_menu_entry              TYPE i VALUE 6        .

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.



ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm       .



  CASE p_ucomm.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD wa_alv->refresh_table_display .
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO


*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE


*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.

  DATA: w_texto1(10),
        w_texto2(10),
        w_texto3(40),
        periodo_r TYPE c LENGTH 50,
        periodo_o TYPE c LENGTH 50.


  v_report = sy-repid.

*** Nome do Report

  w_texto3 = 'Aplicação de Fornecedores no RE	'.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.

*  concatenate p_erdat-low+6(2)   '.' p_erdat-low+4(2)  '.' p_erdat-low(4)  into w_texto1 .
*  concatenate p_erdat-high+6(2)  '.' p_erdat-high+4(2) '.' p_erdat-high(4) into w_texto2 .

*  concatenate ' Periodo Remessa : ' w_texto1 ' - ' w_texto2 into periodo_r.
*
*  perform f_construir_cabecalho using 'S' periodo_r.
*
*  concatenate p_period-low+6(2)   '.' p_period-low+4(2)  '.' p_period-low(4)  into w_texto1 .
*  concatenate p_period-high+6(2)  '.' p_period-high+4(2) '.' p_period-high(4) into w_texto2 .
*
*  concatenate ' Periodo Ordem de Venda: ' w_texto1 ' - ' w_texto2 into periodo_o.

  PERFORM f_construir_cabecalho USING 'S' periodo_o.

ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv.

  " PERFORM f_definir_eventos.

  PERFORM alv_preenche_cat USING:
        'VBELN'            'Doc. Remessa'     '12'  ' '  ' ',
        'NR_REGISTRO_EXPO' 'Re'               '14'  ' '  ' ',
        'ID_NOMEACAO_TRAN' 'Cod. Navio'       '10'  ' '  'X',
        'DS_NOME_TRANSPOR' 'Nome do Navio'    '23'  ' '  ' ',
        'LIFNR'            'Cod. Fornecedor'  '13'  ' '  'X',
        'NAME1'            'Fornecedor'       '23'  ' '  ' ',
        'STCD1'            'CNPJ'             '14'  ' '  ' ',
        'REGIO'            'Estado Produtor'  '13'  ' '  ' ',
        'QUANTIDADE'       'Quantidade'       '13'  ' '  ' ',
        'NOTAS'            'Notas Vinculadas' '40'  ' '  ' '.

ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .

  IF t_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.
  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = it_fcat[]
      it_sort                 = t_sort[]
      i_save                  = 'X'
      it_events               = events
      is_print                = t_print
      is_variant              = vg_variant
    TABLES
      t_outtab                = t_saida.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.



ENDFORM.                    "f_user_command
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir .

  DATA: vl_formname TYPE tdsfname  ,
        vl_name     TYPE rs38l_fnam.

  vl_formname = 'ZFORNECRE'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      p_fornec_re      = wa_saida
    TABLES
      it_fornec_re     = t_saida
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " IMPRIMIR



*---------------------------------------------------------------------*
*       FORM FILL_CELL                                                *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM fill_cell USING i j bold val.
  CALL METHOD OF h_excel 'Cells' = h_zl
    EXPORTING
    #1 = i
    #2 = j.
  PERFORM err_hdl.
  SET PROPERTY OF h_zl 'Value' = val .
  PERFORM err_hdl.
  GET PROPERTY OF h_zl 'Font' = h_f.
  PERFORM err_hdl.
  SET PROPERTY OF h_f 'Bold' = bold .
  PERFORM err_hdl.
ENDFORM.                    "FILL_CELL


*&---------------------------------------------------------------------*
*&      Form  ERR_HDL
*&---------------------------------------------------------------------*
*       outputs OLE error if any                                       *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM err_hdl.
  IF sy-subrc <> 0.
    WRITE: / 'Fehler bei OLE-Automation:'(010), sy-subrc.
    STOP.
  ENDIF.
ENDFORM.                    "err_hdl
