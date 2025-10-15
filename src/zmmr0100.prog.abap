*&---------------------------------------------------------------------*
*& Report  ZMMR0100
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Relatório de Pedidos com Benefício RECAP                *
* Transação..: ZLES                                                    *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
* 10.11.20|JALEXANDRE |         | Desenvolvimento Inicial              *
*----------------------------------------------------------------------*
REPORT zmmr0100.

TABLES: ekko, ekpo.

*----------------------------------------------------------------------*
* Declaração detipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         lifnr       TYPE  ekko-lifnr,
         name1       TYPE lfa1-name1,
         bukrs       TYPE ekko-bukrs,
         butxt       TYPE t001-butxt,
         werks       TYPE ekpo-werks,
         filial_name TYPE t001w-name1,
         recap       TYPE c LENGTH 3,
         "Inicio Marcio Miguel 26.01.2023
         reidi       TYPE ekpo-reidi,
         aliquota    TYPE ekpo-aliquota,
         "Final Marcio Miguel 26.01.2023
         ebeln       TYPE ekko-ebeln,
         ebelp       TYPE ekpo-ebelp,
         aedat       TYPE ekko-aedat,
         bsart       TYPE ekko-bsart,
         ernam       TYPE ekko-ernam,
         ekgrp       TYPE ekko-ekgrp,
         eknam       TYPE t024-eknam,
         anln1       TYPE ekkn-anln1,
         anln2       TYPE ekkn-anln2,
         belnr       TYPE ekbe-belnr,
         gjahr       TYPE bkpf-gjahr,
         budat       TYPE ekbe-budat,
         belnr2      TYPE bkpf-belnr,
         gjahr2      TYPE bkpf-gjahr,
         xblnr       TYPE bkpf-xblnr,
         docnum      TYPE j_1bnflin-refkey, "Docnum
       END OF ty_saida.


TYPES: BEGIN OF y_filtros,
         parametro  TYPE string,
         valor      TYPE string,
         direita    TYPE string,
         parametro2 TYPE string,
         valor2     TYPE string,
       END OF y_filtros.

TYPES: BEGIN OF y_flin,
         belnr  TYPE bkpf-belnr,
         gjahr  TYPE bkpf-gjahr,
         refkey TYPE j_1bnflin-refkey, "Docnum
       END OF y_flin.

*----------------------------------------------------------------------*
* Declaração de tabela Interna
*----------------------------------------------------------------------*
DATA: it_saida         TYPE TABLE OF ty_saida,
      it_flin          TYPE TABLE OF y_flin,
      it_fcat          TYPE lvc_t_fcat,
      it_filtro        TYPE TABLE OF y_filtros,
      gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid.
*----------------------------------------------------------------------*
* CLASS DEFINITION
*----------------------------------------------------------------------*
CLASS       lcl_event_receiver DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS : handle_hotspot_click
                FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.

    METHODS : handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive.


ENDCLASS .                    "LCL_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.


*-----Logic to handle the HOTSPOT click
  METHOD handle_hotspot_click.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.
  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_toolbar.
    PERFORM zf_elimina_botoes_header  USING e_object.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
* Declaração de Variáveis
*&---------------------------------------------------------------------*
DATA: go_alv           TYPE REF TO cl_salv_table,
      v_event_receiver TYPE REF TO lcl_event_receiver.      "#EC NEEDED

*----------------------------------------------------------------------*
* Parâmetros de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR ekko-bukrs OBLIGATORY,
                s_werks FOR ekpo-werks,
                s_ebeln FOR ekko-ebeln NO INTERVALS NO-EXTENSION NO-DISPLAY,
                s_aedat FOR ekko-aedat OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME.
PARAMETERS: r_recap RADIOBUTTON GROUP r1,
            r_reid  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b12.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_buscar_registros.

*----------------------------------------------------------------------*
* Mostrar tabela de Saída                                              *
*----------------------------------------------------------------------*

  PERFORM zf_alv.


*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_registros .

  TYPES: BEGIN OF ty_doc,
           bukrs TYPE ekko-bukrs,
           gjahr TYPE ekbe-gjahr,
           belnr TYPE ekbe-belnr,
           awkey TYPE bkpf-awkey,
         END OF ty_doc.

  DATA: it_doc TYPE TABLE OF ty_doc,
        wa_doc LIKE LINE OF it_doc.

  "Cabeçalho do pedido
  SELECT * FROM ekko
    INTO TABLE @DATA(it_ekko)
    WHERE ebeln IN @s_ebeln
      AND bukrs IN @s_bukrs
      AND aedat IN @s_aedat.

  IF it_ekko[] IS NOT INITIAL.

    "Item do Pedido
    SELECT * FROM ekpo
      INTO TABLE @DATA(it_ekpo)
      FOR ALL ENTRIES IN @it_ekko
      WHERE ebeln = @it_ekko-ebeln
      AND knttp = 'A'
      AND recap <> @space.

    "Descrição da Empresa
    SELECT * FROM t001
      INTO TABLE @DATA(it_t001)
      FOR ALL ENTRIES IN @it_ekko
      WHERE bukrs = @it_ekko-bukrs.

    "Descrição do fornecedor
    SELECT lifnr, name1 FROM lfa1
      INTO TABLE @DATA(it_lfa1)
      FOR ALL ENTRIES IN @it_ekko
      WHERE lifnr = @it_ekko-lifnr.

    "Grupo de compradores
    SELECT * FROM t024
      INTO TABLE @DATA(it_t024)
      FOR ALL ENTRIES IN @it_ekko
      WHERE ekgrp = @it_ekko-ekgrp.

  ENDIF.

  IF it_ekpo[] IS NOT INITIAL.

    "Descrição da Filial
    SELECT * FROM t001w
      INTO TABLE @DATA(it_t001w)
      FOR ALL ENTRIES IN @it_ekpo
      WHERE werks = @it_ekpo-werks.

    "Número do Ativo
    SELECT * FROM ekkn
      INTO TABLE @DATA(it_ekkn)
      FOR ALL ENTRIES IN @it_ekpo
      WHERE  ebeln = @it_ekpo-ebeln
        AND  ebelp = @it_ekpo-ebelp.

    "Histórico para o documento de compra
    SELECT ebeln, ebelp,  zekkn, vgabe, gjahr, belnr, buzei, budat FROM ekbe
      INTO TABLE @DATA(it_ekbe)
          FOR ALL ENTRIES IN @it_ekpo
          WHERE  ebeln = @it_ekpo-ebeln
            AND  ebelp = @it_ekpo-ebelp
            AND vgabe  = 2.

  ENDIF.

  IF it_ekbe[] IS NOT INITIAL.

    "Cabeçalho doc.da fatura recebida
    SELECT belnr, gjahr, stblg FROM rbkp
      INTO TABLE @DATA(it_rbkp)
     FOR ALL ENTRIES IN @it_ekbe
      WHERE belnr = @it_ekbe-belnr
        AND gjahr = @it_ekbe-gjahr
        AND stblg = @space.

    IF sy-subrc EQ 0.

      it_flin = VALUE #( FOR l IN it_rbkp ( belnr  =  l-belnr
                                            gjahr  =  l-gjahr
                                            refkey =  |{ l-belnr }{ l-gjahr }|
                                            ) ).

      SELECT * FROM j_1bnflin
        INTO TABLE @DATA(it_j_1bnflin)
        FOR ALL ENTRIES IN @it_flin
         WHERE refkey EQ @it_flin-refkey.

    ENDIF.

  ENDIF.

  IF it_ekbe[] IS NOT INITIAL.

    LOOP AT it_ekbe INTO DATA(wa_ekbe).

      READ TABLE it_rbkp INTO DATA(wa_rbkp) WITH KEY belnr = wa_ekbe-belnr
                                                     gjahr = wa_ekbe-gjahr.

      CHECK sy-subrc IS INITIAL.

      READ TABLE it_ekko INTO DATA(wa_ekko) WITH KEY ebeln = wa_ekbe-ebeln.
      IF sy-subrc IS INITIAL.

        wa_doc-bukrs = wa_ekko-bukrs.
        wa_doc-gjahr = wa_ekbe-gjahr.
        wa_doc-belnr = wa_ekbe-belnr.
        CONCATENATE wa_doc-belnr wa_doc-gjahr INTO wa_doc-awkey.
        APPEND wa_doc TO it_doc.

      ENDIF.
      CLEAR: wa_ekko, wa_doc, wa_ekbe, wa_rbkp.

    ENDLOOP.

  ENDIF.

  IF it_doc[] IS NOT INITIAL.
    "Cabeçalho do documento contábil
    SELECT bukrs, belnr, gjahr, awkey, xblnr FROM bkpf
      INTO TABLE @DATA(it_bkpf)
      FOR ALL ENTRIES IN @it_doc
      WHERE bukrs = @it_doc-bukrs
        AND gjahr = @it_doc-gjahr
        AND awkey = @it_doc-awkey.
  ENDIF.

  IF it_ekkn[] IS INITIAL.
    MESSAGE s000(z_les) WITH 'Nenhum registro encontrado!'.
  ENDIF.

  SORT it_rbkp BY belnr DESCENDING.
*----------------------------------------------------------------------*
* Montar tabela de saída
*----------------------------------------------------------------------*
  DATA: wa_saida LIKE LINE OF it_saida.

  LOOP AT it_ekkn INTO DATA(wa_ekkn).

    wa_saida-anln1 = wa_ekkn-anln1.
    wa_saida-anln2 = wa_ekkn-anln2.

    READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekkn-ebeln.

    CHECK  sy-subrc IS INITIAL.

    wa_saida-ebeln = wa_ekko-ebeln.
    wa_saida-bukrs = wa_ekko-bukrs.
    wa_saida-aedat = wa_ekko-aedat.
    wa_saida-bsart = wa_ekko-bsart.
    wa_saida-ernam = wa_ekko-ernam.
    wa_saida-ekgrp = wa_ekko-ekgrp.
    wa_saida-lifnr = wa_ekko-lifnr.

    READ TABLE it_ekpo INTO DATA(wa_ekpo) WITH KEY ebeln = wa_ekkn-ebeln
                                                  ebelp  = wa_ekkn-ebelp.

    CHECK sy-subrc IS INITIAL.
    "Inicio Marcio Miguel
    wa_saida-reidi    =  wa_ekpo-reidi.
    wa_saida-aliquota =  wa_ekpo-aliquota.
    "Final Marcio Miguel 26.01.2023
    wa_saida-ebelp =  wa_ekpo-ebelp.
    wa_saida-werks =  wa_ekpo-werks.

    CASE wa_ekpo-recap.
      WHEN 'N'.
        wa_saida-recap  =  'NÃO'.
      WHEN 'S'.
        wa_saida-recap  =  'SIM'.
      WHEN OTHERS.
    ENDCASE.

    LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = wa_ekpo-ebeln
                                   AND ebelp = wa_ekpo-ebelp.

      READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr  =  wa_ekbe-belnr
                                               gjahr  =  wa_ekbe-gjahr.
      IF sy-subrc IS INITIAL.

        "Inicio USER STORY 96436 / Anderson Oenning

        READ TABLE it_flin INTO DATA(wa_flin) WITH KEY belnr = wa_rbkp-belnr
                                                       gjahr = wa_rbkp-gjahr.

        IF sy-subrc EQ 0.
          READ TABLE it_j_1bnflin INTO DATA(wa_j_1bnflin) WITH KEY refkey = wa_flin-refkey.
          IF sy-subrc EQ 0.
            wa_saida-docnum = wa_j_1bnflin-docnum.
          ENDIF.
        ENDIF.
        "Fim USER STORY 96436 / Anderson Oenning

        READ TABLE it_doc INTO wa_doc WITH KEY bukrs = wa_ekko-bukrs
                                               gjahr = wa_rbkp-gjahr
                                               belnr = wa_rbkp-belnr.
        IF sy-subrc IS INITIAL.

          READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = wa_ekko-bukrs
                                                   gjahr = wa_rbkp-gjahr
                                                   awkey = wa_doc-awkey.
          IF sy-subrc IS INITIAL.
            wa_saida-belnr  = wa_ekbe-belnr.
            wa_saida-gjahr  = wa_ekbe-gjahr.
            wa_saida-budat  = wa_ekbe-budat.
            wa_saida-belnr2 = wa_bkpf-belnr.
            wa_saida-gjahr2 = wa_bkpf-gjahr.
            wa_saida-xblnr  = wa_bkpf-xblnr.
            CONTINUE.
          ENDIF.

        ENDIF.

        CLEAR: wa_rbkp, wa_ekbe, wa_bkpf, wa_doc.
      ENDIF.

    ENDLOOP.

    READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_ekko-lifnr.
    IF sy-subrc IS INITIAL.
      wa_saida-name1 = wa_lfa1-name1.
    ENDIF.

*---------------------------------------------------------------------*
    "Preenher campos de descrição
*---------------------------------------------------------------------*

    "Empresa
    READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY  bukrs = wa_ekko-bukrs.
    IF sy-subrc IS INITIAL.
      wa_saida-butxt = wa_t001-butxt.
    ENDIF.

    "Filial
    READ TABLE it_t001w INTO DATA(wa_t001w) WITH KEY  werks = wa_ekpo-werks.

    IF sy-subrc IS INITIAL.
      wa_saida-filial_name = wa_t001w-name1.
    ENDIF.

    READ TABLE it_t024 INTO DATA(wa_t024) WITH KEY ekgrp = wa_ekko-ekgrp.
    IF sy-subrc IS INITIAL.
      wa_saida-eknam = wa_t024-eknam.
    ENDIF.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_ekko, wa_ekpo, wa_ekkn, wa_rbkp, wa_ekbe, wa_bkpf, wa_doc.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv .

  IF it_saida IS NOT INITIAL.
    PERFORM zf_filtros.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  PERFORM zf_criar_objetos .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_criar_objetos .

  DATA: wa_variant TYPE disvariant,
        wa_layout  TYPE lvc_s_layo.

  wa_layout-cwidth_opt = 'X'.
  wa_layout-zebra      = 'X'.
  wa_layout-sel_mode   = 'A'.

  wa_layout-info_fname = 'COLOR_LINE'.
  wa_layout-ctab_fname = 'COLOR_CELL'.

  wa_variant-report   = sy-repid.

  IF gob_gui_alv_grid IS INITIAL.

    PERFORM zf_split_screen         CHANGING sy-title gob_gui_alv_grid.
    PERFORM zf_cria_fieldcat        CHANGING it_saida it_fcat.
    PERFORM zf_ajustar_descr_campos CHANGING it_fcat.

    wa_layout-cwidth_opt = 'X'.
    wa_layout-zebra      = 'X'.

*----------------------------------------------------------------------*
*** Define eventos
*----------------------------------------------------------------------*
    CREATE OBJECT v_event_receiver.
    SET HANDLER v_event_receiver->handle_hotspot_click FOR gob_gui_alv_grid.

    SET HANDLER v_event_receiver->handle_toolbar FOR gob_gui_alv_grid.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant
        i_save                        = 'A'
        is_layout                     = wa_layout
      CHANGING
        it_outtab                     = it_saida
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_cria_fieldcat CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).
  IF r_reid = abap_true.
    READ TABLE pt_fieldcat WITH KEY fieldname = 'RECAP' ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF sy-subrc = 0.
      <fs_fcat>-no_out = abap_true.
    ENDIF.

    READ TABLE pt_fieldcat WITH KEY fieldname = 'EBELP' ASSIGNING <fs_fcat>.
    IF sy-subrc = 0.
      <fs_fcat>-no_out = abap_true.
    ENDIF.


  ELSEIF r_recap = abap_true.
    READ TABLE pt_fieldcat WITH KEY fieldname = 'REIDI' ASSIGNING <fs_fcat>.
    IF sy-subrc = 0.
      <fs_fcat>-no_out = abap_true.
    ENDIF.
    READ TABLE pt_fieldcat WITH KEY fieldname = 'ALIQUOTA' ASSIGNING <fs_fcat>.
    IF sy-subrc = 0.
      <fs_fcat>-no_out = abap_true.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_filtros .

  FREE: it_filtro.

  LOOP AT SCREEN.
    it_filtro = VALUE #(
      ( parametro = '' valor = '' )
    ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_elimina_botoes_header    USING  e_object TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
*                                 OR function = '&MB_VARIANT'
*                                 OR function =  '&MB_EXPORT'
                                 OR function =  '&MB_SUM'
                                 OR function =  '&MB_SUBTOT'
                                 OR function =  '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTAR_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_ajustar_descr_campos CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

    CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.

    CASE <fs_fcat>-fieldname.
      WHEN 'LIFNR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Fornecedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'NAME1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome do Fornecedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'BUKRS'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Empresa'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'BUTXT'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome da Empresa'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'WERKS'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Filial'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'FILIAL_NAME'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome da filial'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'RECAP'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'RECAP'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EBELN'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Pedido'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
        <fs_fcat>-hotspot = abap_true.
      WHEN 'EBELP'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Item'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'AEDAT'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Dt.Pedido'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'BSART'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tp.Ped.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'ERNAM'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Usuario'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EKGRP'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Comprador'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EKNAM'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome do Comprador'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'ANLN1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Imobilizado'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'ANLN2'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Sub_num.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'BELNR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nº Miro'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
        <fs_fcat>-hotspot = abap_true.
      WHEN 'GJAHR'.
        <fs_fcat>-tech = abap_true.
      WHEN 'BUDAT'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Dt.Miro'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

      WHEN 'XBLNR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nº referência'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

      WHEN 'DOCNUM'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nº documento'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

      WHEN 'BELNR2'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nª Doc. Contábil'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
        <fs_fcat>-hotspot = abap_true.
      WHEN 'GJAHR2'.
        <fs_fcat>-tech = abap_true.
      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_GRID  text
*----------------------------------------------------------------------*
FORM zf_split_screen USING i_titulo CHANGING p_alv TYPE REF TO cl_gui_alv_grid.

  DATA: l_header       TYPE REF TO cl_gui_splitter_container,
        l_picture      TYPE REF TO cl_gui_picture,
        l_dg_dyndoc_id TYPE REF TO cl_dd_document,
        l_html         TYPE REF TO  cl_gui_html_viewer,
        l_split        TYPE REF TO  cl_gui_splitter_container.

  DATA: t_text_table_f  TYPE sdydo_text_table,
        t_text_table_v  TYPE sdydo_text_table,
        t_text_table_f2 TYPE sdydo_text_table,
        t_text_table_v2 TYPE sdydo_text_table.

  CHECK l_split IS INITIAL.

  l_split = NEW #( parent = cl_gui_container=>screen0 rows = 2 columns = 1 ).

  DATA(v_container_html) = l_split->get_container( EXPORTING row = 1 column = 1 ).

*----------------------------------------------------------------------*
* Header
*----------------------------------------------------------------------*
  l_header = NEW #( parent = v_container_html rows = 1 columns = 2 ).

  DATA(l_header_texto) = l_header->get_container( EXPORTING row = 1 column = 1 ).

  l_header->set_column_width( EXPORTING id = 1 width = 40 ).

  "Logo
  DATA(l_header_logo) = l_header->get_container( EXPORTING row = 1 column = 2 ).

  l_picture = NEW #( parent = l_header_logo ).

  DATA: l_url TYPE char255.

  PERFORM zf_buscar_imagem_url USING 'LOGO_NOVO' CHANGING l_url.

  l_picture->load_picture_from_url( EXPORTING url = l_url ).

  l_picture->set_display_mode( EXPORTING display_mode = l_picture->display_mode_fit_center ).

*----------------------------------------------------------------------*
* Item
*----------------------------------------------------------------------*

  DATA(v_item_grid) = l_split->get_container( EXPORTING row = 2 column = 1 ).

  l_split->set_row_height( EXPORTING id = 1 height = 15 ).

  p_alv = NEW #( i_parent = v_item_grid ).

  l_dg_dyndoc_id = NEW #( style = 'ALV_TO_HTML' background_color = 7 ).
  l_dg_dyndoc_id->initialize_document( ).

  l_dg_dyndoc_id->add_table( EXPORTING no_of_columns = 1 border = '0' width = '100%' IMPORTING table = DATA(table_element) ).

*----------------------------------------------------------------------*
* Preencher Titulo
*----------------------------------------------------------------------*
  IF  i_titulo IS NOT INITIAL.
    table_element->add_column( IMPORTING column = DATA(column) ).
    table_element->set_column_style( EXPORTING col_no = 1 sap_style = cl_dd_document=>heading sap_align = 'CENTER' ).
    column->add_text( EXPORTING text = CONV #( i_titulo ) sap_style = 'HEADING' ).
  ENDIF.

*----------------------------------------------------------------------*
* Mostra dados adicionais
*----------------------------------------------------------------------*
  IF it_filtro[] IS NOT INITIAL.

    l_dg_dyndoc_id->add_table( EXPORTING no_of_columns = 4 border = '0' width = '100%' IMPORTING table = DATA(table_element_linhas) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_1) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_2) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_3) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_4) ).

    table_element_linhas->set_column_style( EXPORTING col_no = 1 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
    table_element_linhas->set_column_style( EXPORTING col_no = 2 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).
    table_element_linhas->set_column_style( EXPORTING col_no = 3 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
    table_element_linhas->set_column_style( EXPORTING col_no = 4 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).

    LOOP AT it_filtro INTO DATA(w_filtro).

      APPEND w_filtro-parametro TO t_text_table_f.
      APPEND w_filtro-valor TO t_text_table_v.

      APPEND w_filtro-parametro2 TO t_text_table_f2.
      APPEND w_filtro-valor2 TO t_text_table_v2.

      column_1->add_text( EXPORTING text_table = t_text_table_f  fix_lines = abap_true ).
      column_2->add_text( EXPORTING text_table = t_text_table_v  fix_lines = abap_true ).
      column_3->add_text( EXPORTING text_table = t_text_table_f2 fix_lines = abap_true ).
      column_4->add_text( EXPORTING text_table = t_text_table_v2 fix_lines = abap_true ).

      CLEAR: t_text_table_f[], t_text_table_v[], t_text_table_f2[], t_text_table_v2[].

    ENDLOOP.

  ENDIF.

  l_html = NEW #( parent = l_header_texto ).

  l_dg_dyndoc_id->merge_document( ).

  l_dg_dyndoc_id->html_control = l_html.

  l_dg_dyndoc_id->display_document( EXPORTING reuse_control = 'X' parent = l_header_texto ).


  CALL METHOD l_split->set_row_height
    EXPORTING
      id     = 1
      height = 20.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_IMAGEM_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1099   text
*      <--P_L_URL  text
*----------------------------------------------------------------------*
FORM zf_buscar_imagem_url   USING    i_nome_logo
                         CHANGING r_url.

  TYPES: BEGIN OF ty_graphic_table,
           line(255) TYPE x,
         END OF ty_graphic_table.

  DATA: graphic_table TYPE TABLE OF ty_graphic_table.

  DATA: l_graphic_xstr TYPE xstring.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = i_nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  DATA(graphic_size) = xstrlen( l_graphic_xstr ).
  DATA(l_graphic_conv) = graphic_size.
  DATA(l_graphic_offs) = 0.
  WHILE l_graphic_conv > 255.
    APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(255) ) TO graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(l_graphic_conv) ) TO graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = r_url.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM zf_handle_hotspot_click     USING p_index  TYPE any
                                       p_column TYPE any.

  READ TABLE it_saida INTO DATA(wa_saida) INDEX p_index.

  CHECK sy-subrc IS INITIAL.

  CASE p_column.
    WHEN 'BELNR'.
      IF wa_saida-belnr IS NOT INITIAL.
        PERFORM zf_miro USING wa_saida-bukrs wa_saida-belnr wa_saida-gjahr.
      ENDIF.
    WHEN 'BELNR2'.
      IF wa_saida-belnr2 IS NOT INITIAL.
        PERFORM zf_fb03 USING wa_saida-belnr2 wa_saida-bukrs wa_saida-gjahr2.
      ENDIF.
    WHEN 'EBELN'.
      IF wa_saida-ebeln IS NOT INITIAL.
        PERFORM zf_me23n USING wa_saida-ebeln.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_FB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_SAIDA>_XDOCREV  text
*      -->P_<FS_SAIDA>_BUKRS  text
*      -->P_<FS_SAIDA>_ANO  text
*----------------------------------------------------------------------*
FORM zf_fb03  USING p_belnr p_bukrs p_gjahr .

*  IF p_obj_key_perda IS NOT INITIAL.
*
*    SELECT SINGLE belnr, bukrs, gjahr FROM bkpf
*      INTO @DATA(w_transaction)
*      WHERE belnr = @p_obj_key_perda
*        AND bukrs = @p_bukrs
*        AND gjahr = @p_gjahr .
*
*    IF sy-subrc IS INITIAL.

  SET PARAMETER ID 'BLN' FIELD p_belnr. "w_transaction-belnr.
  SET PARAMETER ID 'BUK' FIELD p_bukrs. "w_transaction-bukrs.
  SET PARAMETER ID 'GJR' FIELD p_gjahr. "w_transaction-gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BUKRS  text
*      -->P_BELNR  text
*----------------------------------------------------------------------*
FORM zf_miro  USING    p_bukrs
                       p_belnr
                       p_gjahr.

*  SET PARAMETER ID 'BUK' FIELD p_bukrs.
  SET PARAMETER ID 'RBN' FIELD p_belnr.
  SET PARAMETER ID 'GJR' FIELD p_gjahr.
  CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ME23N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EBELN  text
*----------------------------------------------------------------------*
FORM zf_me23n  USING    p_ebeln.

  SET PARAMETER ID 'BES' FIELD p_ebeln.
  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

ENDFORM.
