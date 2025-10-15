*&---------------------------------------------------------------------*
*& Report  ZFIR078
*&
*&---------------------------------------------------------------------*
REPORT zfir078.

*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*
TABLES: bsis,ekko.

TYPE-POOLS: pmst.
TYPE-POOLS: vrm. " Use type group VRM for list

DATA: it_list TYPE  vrm_values.
"DATA: WA_LIST LIKE  IT_LIST.
DATA: it_values TYPE TABLE OF dynpread, wa_values TYPE dynpread.
DATA: gs_variant_c TYPE disvariant.

DATA: lv_selected_value(10)  TYPE c,
      lv_selected_value1(10) TYPE c,
      lv_selected_value2(10) TYPE c,
      lv_selected_index      TYPE i.

TYPES: BEGIN OF tp_saida,

         fornecedor         TYPE  ekko-lifnr,
         nome_fornecedor    TYPE  lfa1-name1,
         nro_documento      TYPE  bsis-belnr,
         moeda              TYPE  bsis-waers,
         pedido             TYPE  ekbe-ebeln,
         lote_compra        TYPE  bsis-zuonr,
         nro_nota           TYPE  bsis-xblnr,
         dt_lancamento      TYPE  bsis-budat,
         tp_doc             TYPE  bsis-blart,
         tp_fatura          TYPE  c LENGTH 12,
         valor_brl          TYPE  bsis-dmbtr,
         valor_usd          TYPE  bsis-dmbe2,
         qtde               TYPE  ekbe-menge,
         filial             TYPE  bsis-gsber,
         nome_filial        TYPE  t001w-name1,
         material           TYPE  ekpo-matnr,
         descricao_material TYPE  ekpo-txz01,
         safra              TYPE  eket-charg,
       END OF tp_saida.

DATA: wa_tp_saida TYPE tp_saida, it_saida TYPE TABLE OF tp_saida.

TYPES: BEGIN OF t_bsis,
         belnr TYPE bsis-belnr,
         bukrs TYPE bsis-bukrs,
         hkont TYPE bsis-hkont,
         zuonr TYPE bsis-zuonr,
         budat TYPE bsis-budat,
         waers TYPE bsis-waers,
         xblnr TYPE bsis-xblnr,
         blart TYPE bsis-blart,
         shkzg TYPE bsis-shkzg,
         gsber TYPE bsis-gsber,
         dmbtr TYPE bsis-dmbtr,
         dmbe2 TYPE bsis-dmbe2,
         gjahr TYPE bsis-gjahr,
       END OF t_bsis.

DATA: it_bsis TYPE TABLE OF t_bsis.

TYPES: BEGIN OF t_bkpf,
         awkey  TYPE bkpf-awkey,
         bukrs  TYPE bsis-bukrs,
         belnr  TYPE bsis-belnr,
         gjahr  TYPE bsis-gjahr,
         belnr1 TYPE bsis-belnr,
       END OF t_bkpf.

DATA: it_bkpf TYPE TABLE OF t_bkpf.

TYPES: BEGIN OF t_ekbe,
         ebeln TYPE ekbe-ebeln,
         menge TYPE ekbe-menge,
         gjahr TYPE bsis-gjahr,
         belnr TYPE ekbe-belnr,
       END OF t_ekbe.

DATA: it_ekbe TYPE TABLE OF t_ekbe.

TYPES: BEGIN OF t_ekko,
         ebeln TYPE   ekbe-ebeln,
         lifnr TYPE  ekko-lifnr,
         bsart TYPE  ekko-bsart,
       END  OF t_ekko.

DATA: it_ekko TYPE TABLE OF t_ekko.

TYPES: BEGIN OF t_ekpo,
         ebeln TYPE  ekbe-ebeln,
         matnr TYPE  ekpo-matnr,
         txz01 TYPE  ekpo-txz01,
       END OF t_ekpo.

DATA: it_ekpo TYPE TABLE OF t_ekpo.

TYPES: BEGIN OF t_eket,
         ebeln TYPE  ekbe-ebeln,
         charg TYPE  eket-charg,
       END OF t_eket.

DATA: it_eket TYPE TABLE OF t_eket.

TYPES: BEGIN OF t_lfa1,
         lifnr TYPE  ekko-lifnr,
         name1 TYPE lfa1-name1,
       END OF t_lfa1.

DATA: it_lfa1 TYPE TABLE OF t_lfa1.

TYPES: BEGIN OF t_t001w,
         werks TYPE  bsis-gsber,
         name1 TYPE t001w-name1,
       END OF t_t001w.

DATA: it_t001w TYPE TABLE OF t_t001w.

TYPES: BEGIN OF t_pedfor,
         belnr TYPE ekbe-belnr,
         lifnr TYPE ekko-lifnr,
         bukrs TYPE ekko-bukrs,
         budat TYPE ekbe-budat,
         gjahr TYPE ekbe-gjahr,
       END OF t_pedfor.

DATA: it_pedfor TYPE TABLE OF t_pedfor.




DATA: it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat,
      wa_stable       TYPE lvc_s_stbl.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1        TYPE REF TO cl_gui_container,
      dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
      dg_parent_2        TYPE REF TO cl_gui_container,
      dg_parent_2a       TYPE REF TO cl_gui_container,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      it_exclude_fcode   TYPE ui_functions,
      wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      ctl_alv            TYPE REF TO cl_gui_alv_grid,
      ctl_alv02          TYPE REF TO cl_gui_alv_grid,
      table_element      TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      table_element2     TYPE REF TO cl_dd_table_element,
      column_1           TYPE REF TO cl_dd_area,
      column_2           TYPE REF TO cl_dd_area,
      dg_html_cntrl      TYPE REF TO cl_gui_html_viewer.


DATA: t_sort  TYPE lvc_t_sort, fs_sort TYPE lvc_s_sort.


DATA str TYPE REF TO data.


fs_sort-spos = '1'.
fs_sort-fieldname = 'VALOR_BRL'.
fs_sort-down = 'X'.
fs_sort-subtot = 'X'.
APPEND fs_sort TO t_sort.
CLEAR fs_sort.

fs_sort-spos = '2'.
fs_sort-fieldname = 'VALOR_USD'.
fs_sort-down = 'X'.
fs_sort-subtot = 'X'.
APPEND fs_sort TO t_sort.
CLEAR fs_sort.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.



*=============================================================================*
* BEGIN - FILTROS DE SELEÇÃO
*=============================================================================*

  SELECT-OPTIONS : s_bukrs FOR bsis-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY ,   "EMPRESA
                   s_belnr  FOR bsis-belnr, "Doct Pgt.
                   s_zuonr  FOR bsis-zuonr, "Pedido.
                   s_lifnr  FOR ekko-lifnr NO INTERVALS, "FORNECEDOR
                   s_budat  FOR bsis-budat  OBLIGATORY.  "DATA DE LANÇAMENTO

  PARAMETERS : p_hkont  TYPE c AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY  DEFAULT 1  . "Parameter.

SELECTION-SCREEN:END OF BLOCK b1.
*=============================================================================*
* END - FILTROS DE SELEÇÃO
*=============================================================================*


CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_hotspot_click.
    DATA: wl_trans LIKE LINE OF it_saida.
    IF e_row_id GT 0.
      READ TABLE it_saida INTO wl_trans INDEX e_row_id.

      IF e_column_id =  'NRO_DOCUMENTO' AND wl_trans-nro_documento  IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD wl_trans-nro_documento.
        SET PARAMETER ID 'BUK' FIELD s_bukrs-low.
        CALL TRANSACTION  'FB03' AND SKIP FIRST SCREEN.
      ELSEIF e_column_id =  'PEDIDO' AND wl_trans-pedido IS NOT INITIAL.
        SET PARAMETER ID 'BES' FIELD wl_trans-pedido.
        CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.

  APPEND VALUE #( key = '1' text = '112051') TO it_list.
  APPEND VALUE #( key = '2' text = '112050') TO it_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_HKONT'
      values          = it_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


*--------------------------------------------------------------*
*At Selection Screen
* Evento do Dropdownlist
*--------------------------------------------------------------*
AT SELECTION-SCREEN ON p_hkont.
  CLEAR: wa_values, it_values.
  REFRESH it_values.
  wa_values-fieldname = 'P_HKONT'.
  APPEND wa_values TO it_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = it_values.

  READ TABLE it_values INDEX 1 INTO wa_values.
  IF sy-subrc = 0 AND wa_values-fieldvalue IS NOT INITIAL.
    READ TABLE it_list INTO DATA(wa_list) WITH KEY key = wa_values-fieldvalue.
    IF sy-subrc = 0.
      lv_selected_value = wa_list-text.
      lv_selected_value1 = wa_list-text.
      lv_selected_index = wa_list-key.
      IF lv_selected_index = 1.
        lv_selected_value  = '0000112051'.
      ELSE.
        lv_selected_value1 = '0000112050'.
      ENDIF.
    ENDIF.
  ENDIF.

  "AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_HKONT.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TI0001'.

  PERFORM alv.
  PERFORM custom_container.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  IF ( sy-ucomm = 'BACK' ) .
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

START-OF-SELECTION.

  PERFORM buscadados.

  IF ( it_saida IS INITIAL ).

    MESSAGE 'Para Parâmetros informados não foram encontrados registros'  TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.
    CALL SCREEN 0001.
  ENDIF.

FORM buscadados .

  DATA: r_gjahr TYPE RANGE OF bkpf-gjahr.

  LOOP AT s_budat.
    r_gjahr = VALUE #( ( sign   = s_budat-sign
                         option = s_budat-option
                         low    = s_budat-low
                         high   = s_budat-high ) ).
  ENDLOOP.
**********************************************************************
* 108313- CS2023000240 SAP - Melhoria entrega futura - ZFI0117 - Relatório de Reconciliação EF - PSA
**********************************************************************

  IF s_lifnr IS NOT INITIAL OR s_zuonr IS NOT INITIAL.
    SELECT bsis~* FROM bsis
    LEFT JOIN ekko ON ekko~ebeln = bsis~zuonr
    INTO CORRESPONDING FIELDS OF TABLE @it_bsis
    WHERE bsis~bukrs IN @s_bukrs
    AND   bsis~belnr IN @s_belnr
    AND   bsis~zuonr IN @s_zuonr
    AND   bsis~budat IN @s_budat
    AND   bsis~gjahr IN @r_gjahr
    AND   ekko~lifnr IN @s_lifnr
    AND   bsart = 'ZGEF'.


  ELSE.

    SELECT *
    FROM bsis  INTO CORRESPONDING FIELDS OF TABLE it_bsis
    WHERE bukrs IN s_bukrs
    AND   hkont IN (lv_selected_value , lv_selected_value1)
    AND   budat IN s_budat
    AND   gjahr IN r_gjahr ">= s_budat-low AND  gjahr >= s_budat-high
    AND   belnr IN s_belnr.

  ENDIF.





  IF ( it_bsis[] IS NOT INITIAL ).

    SELECT *
    FROM  bkpf INTO CORRESPONDING FIELDS OF TABLE it_bkpf
      FOR ALL ENTRIES IN it_bsis
      WHERE  bukrs  = it_bsis-bukrs
       AND   belnr  = it_bsis-belnr
       AND   gjahr  = it_bsis-gjahr.


    LOOP AT it_bkpf INTO DATA(wa_bbkpf).
      wa_bbkpf-belnr1 = wa_bbkpf-awkey.
      MODIFY it_bkpf FROM wa_bbkpf TRANSPORTING belnr1.
    ENDLOOP.


    DATA(range_bsis) = VALUE zrsdsselopts( FOR ls1 IN it_bsis ( sign = 'I' option = 'EQ' low = ls1-gjahr ) ).

*    DATA(range_bsis) =  r_gjahr.
    DATA(range_bkpf) = VALUE zrsdsselopts( FOR ls2 IN it_bkpf ( sign = 'I' option = 'EQ' low = ls2-awkey(10) ) ).


  ENDIF.

  IF ( range_bsis IS NOT INITIAL ).

    SELECT *
      FROM ekbe INTO CORRESPONDING FIELDS OF TABLE it_ekbe
      WHERE belnr IN range_bkpf
      AND   gjahr IN range_bsis.

  ENDIF.

  IF (  it_ekbe[] IS NOT INITIAL ).

    SELECT *
      FROM ekko INTO  CORRESPONDING FIELDS OF TABLE it_ekko
      FOR ALL ENTRIES IN  it_ekbe
      WHERE  ebeln = it_ekbe-ebeln
      AND    bsart = 'ZGEF'
      AND    lifnr IN s_lifnr.

  ENDIF.

  IF ( it_ekko[] IS NOT INITIAL ).

    SELECT *
      FROM ekpo INTO CORRESPONDING FIELDS OF TABLE it_ekpo
      FOR ALL ENTRIES IN it_ekko
      WHERE ebeln =  it_ekko-ebeln.

  ENDIF.

  IF ( it_ekbe[] IS NOT INITIAL ).

    SELECT *
      FROM eket INTO CORRESPONDING FIELDS OF TABLE it_eket
      FOR ALL ENTRIES IN it_ekbe
      WHERE ebeln =  it_ekbe-ebeln.

  ENDIF.

  IF ( it_ekko[] IS NOT INITIAL ).

    SELECT *
      FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE it_lfa1
      FOR ALL ENTRIES IN it_ekko
      WHERE lifnr =  it_ekko-lifnr.

  ENDIF.

  IF ( it_bsis[] IS NOT INITIAL ).

    SELECT *
      FROM t001w INTO CORRESPONDING FIELDS OF TABLE it_t001w
      FOR ALL ENTRIES IN it_bsis
      WHERE werks =  it_bsis-gsber.

  ENDIF.


  IF ( it_bsis[] IS NOT INITIAL ).

    LOOP AT it_bsis INTO DATA(wa_bsis).


      READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY   bukrs  = wa_bsis-bukrs   belnr  = wa_bsis-belnr     gjahr  = wa_bsis-gjahr.

      IF ( sy-subrc = 0 ).
        READ TABLE it_ekbe INTO DATA(wa_ekbe) WITH KEY   belnr  = wa_bkpf-belnr1   gjahr = wa_bsis-gjahr.

        IF ( sy-subrc = 0 ).

          wa_tp_saida-pedido = wa_ekbe-ebeln.
*------------------ PAra atendimento do IR032663 não ta considerando valor negativo----------
          IF wa_bsis-shkzg EQ 'H'.
            wa_tp_saida-qtde = ( wa_ekbe-menge * -1 ).
          ELSE.
            wa_tp_saida-qtde = wa_ekbe-menge .
          ENDIF.

          READ TABLE it_ekko INTO DATA(wa_ekko) WITH KEY   ebeln = wa_ekbe-ebeln.

          IF ( sy-subrc = 0 ).
            READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_ekko-lifnr.

            IF ( sy-subrc = 0 ).

              wa_tp_saida-fornecedor = wa_lfa1-lifnr.
              wa_tp_saida-nome_fornecedor  = wa_lfa1-name1.

            ENDIF.

            READ TABLE it_ekpo INTO DATA(wa_ekpo) WITH  KEY  ebeln =  wa_ekko-ebeln.

            IF ( sy-subrc = 0 ) .
              wa_tp_saida-material  = wa_ekpo-matnr.
              wa_tp_saida-descricao_material  = wa_ekpo-txz01.
            ENDIF.
          ENDIF.

          READ TABLE it_eket INTO  DATA(wa_eket) WITH  KEY   ebeln =  wa_ekbe-ebeln.

          IF ( sy-subrc = 0 ).
            wa_tp_saida-safra = wa_eket-charg.
          ENDIF.

        ENDIF.
      ENDIF.

      READ TABLE it_t001w INTO DATA(wa_t001w) WITH  KEY  werks = wa_bsis-gsber.

      wa_tp_saida-nro_documento = wa_bsis-belnr.

      wa_tp_saida-moeda  = wa_bsis-waers.

      wa_tp_saida-lote_compra = wa_bsis-zuonr.

      wa_tp_saida-nro_nota  = wa_bsis-xblnr.

      wa_tp_saida-dt_lancamento = wa_bsis-budat.

      wa_tp_saida-tp_doc = wa_bsis-blart.

      wa_tp_saida-tp_fatura = COND #( WHEN wa_bsis-blart = 'WE' THEN 'Remessa' ELSE 'Fatura' ).

      wa_tp_saida-valor_brl = COND #( WHEN wa_bsis-shkzg = 'H' THEN  wa_bsis-dmbtr * -1 ELSE wa_bsis-dmbtr ).

      wa_tp_saida-valor_usd = COND #( WHEN wa_bsis-shkzg = 'H' THEN  wa_bsis-dmbe2 * -1 ELSE wa_bsis-dmbe2 ) .

      wa_tp_saida-filial = wa_bsis-gsber.

      wa_tp_saida-nome_filial = wa_t001w-name1.

      APPEND wa_tp_saida TO it_saida.

      "LIMPANDO MINHA SAIDA E WORKAREA
      CLEAR wa_tp_saida.

    ENDLOOP.
  ENDIF.

ENDFORM.

FORM tratadados .

ENDFORM.

FORM alv .

  ASSIGN 'TP_SAIDA' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  it_fieldcatalog = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  LOOP AT it_fieldcatalog ASSIGNING FIELD-SYMBOL(<x>) .

    <x>-col_opt = 'X'.

    CASE <x>-fieldname .

      WHEN  'FORNECEDOR' .

        <x>-coltext   = 'Fornecedor' .
        <x>-scrtext_l = 'Fornecedor' .
        <x>-scrtext_m = 'Fornecedor' .
        <x>-scrtext_s = 'Fornecedor' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =  'X'.
        <x>-do_sum    =  ''.
        <x>-just      =  ''.

      WHEN 'NOME_FORNECEDOR'.

        <x>-coltext   = 'Nome Fornecedor' .
        <x>-scrtext_l =  'Nome Fornecedor' .
        <x>-scrtext_m =  'Nome Fornecedor' .
        <x>-scrtext_s =  'Nome Fornecedor' .


        <x>-outputlen = '12'.
        <x>-hotspot   = ''.
        <x>-no_zero   = ''.
        <x>-do_sum    = ''.
        <x>-just      = ''.


      WHEN 'NRO_DOCUMENTO'.

        <x>-coltext  = 'Nº Documento' .
        <x>-scrtext_l = 'Nº Documento' .
        <x>-scrtext_m = 'Nº Documento' .
        <x>-scrtext_s = 'Nº Documento' .


        <x>-outputlen = '10'.
        <x>-hotspot   =  'X'.
        <x>-no_zero   =  'X'.
        <x>-do_sum    =  ''.
        <x>-just      =  ''.

      WHEN 'MOEDA'.

        <x>-coltext   = 'MOEDA' .
        <x>-scrtext_l = 'MOEDA' .
        <x>-scrtext_m = 'MOEDA' .
        <x>-scrtext_s = 'MOEDA' .


        <x>-outputlen = '5'.
        <x>-hotspot   = ''.
        <x>-no_zero   = ''.
        <x>-do_sum    = ''.
        <x>-just      = ''.

      WHEN 'PEDIDO'.

        <x>-coltext   = 'Pedido' .
        <x>-scrtext_l =  'Pedido' .
        <x>-scrtext_m =  'Pedido' .
        <x>-scrtext_s =  'Pedido' .


        <x>-outputlen = '10'.
        <x>-hotspot   = 'X'.
        <x>-no_zero   = 'X'.
        <x>-do_sum    = ''.
        <x>-just      = ''.

      WHEN 'LOTE_COMPRA'.

        <x>-coltext   =  'Lote de Compra' .
        <x>-scrtext_l =   'Lote de Compra' .
        <x>-scrtext_m =   'Lote de Compra' .
        <x>-scrtext_s =   'Lote de Compra' .

        <x>-outputlen = '12'.
        <x>-hotspot   =  '' .
        <x>-no_zero   =  '' .
        <x>-do_sum    =  '' .
        <x>-just      =  '' .

      WHEN 'NRO_NOTA'.

        <x>-coltext   = 'Nº Notar' .
        <x>-scrtext_l = 'Nº Nota'  .
        <x>-scrtext_m = 'Nº Nota'  .
        <x>-scrtext_s = 'Nº Nota'  .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =  ''.
        <x>-do_sum    =  ''.
        <x>-just      =  ''.

      WHEN 'DT_LANCAMENTO'.

        <x>-coltext   = 'Dt.Lançamento' .
        <x>-scrtext_l = 'Dt.Lançamento' .
        <x>-scrtext_m = 'Dt.Lançamento' .
        <x>-scrtext_s = 'Dt.Lançamento' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =  ''.
        <x>-do_sum    =  ''.
        <x>-just      =  ''.

      WHEN 'TP_DOC'.

        <x>-coltext   = 'Tp.Doc' .
        <x>-scrtext_l = 'Tp.Doc' .
        <x>-scrtext_m = 'Tp.Doc' .
        <x>-scrtext_s = 'Tp.Doc' .


        <x>-outputlen = '14'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'TP_FATURA'.

        <x>-coltext  = 'Tp.Fatura' .
        <x>-scrtext_l = 'Tp.Fatura' .
        <x>-scrtext_m = 'Tp.Fatura' .
        <x>-scrtext_s = 'Tp.Fatura' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'VALOR_BRL'.

        <x>-coltext   = 'Valor BRL' .
        <x>-scrtext_l = 'Valor BRL' .
        <x>-scrtext_m = 'Valor BRL' .
        <x>-scrtext_s = 'Valor BRL' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =  ''.
        <x>-convexit  =   ''.



        <x>-do_sum    =  'X'.
        <x>-just      =  ''.

      WHEN 'VALOR_USD'.

        <x>-coltext   = 'Valor USD' .
        <x>-scrtext_l = 'Valor USD' .
        <x>-scrtext_m = 'Valor USD' .
        <x>-scrtext_s = 'Valor USD' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-no_zero   =   ''.
        <x>-convexit  =   ''.



        <x>-do_sum    =  'X'.
        <x>-just      =   ''.

      WHEN 'QTDE'.

        <x>-coltext   = 'Quantidade' .
        <x>-scrtext_l = 'Quantidade' .
        <x>-scrtext_m = 'Quantidade' .
        <x>-scrtext_s = 'Quantidade' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'FILIAL'.

        <x>-coltext   = 'Filial' .
        <x>-scrtext_l = 'Filial' .
        <x>-scrtext_m = 'Filial' .
        <x>-scrtext_s = 'Filial' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'NOME_FILIAL'.

        <x>-coltext  = 'Nome Filial' .
        <x>-scrtext_l = 'Nome Filial' .
        <x>-scrtext_m = 'Nome Filial' .
        <x>-scrtext_s = 'Nome Filial' .

        <x>-outputlen = '15'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'MATERIA'.

        <x>-coltext  = 'Material' .
        <x>-scrtext_l = 'Material' .
        <x>-scrtext_m = 'Material' .
        <x>-scrtext_s = 'Material' .


        <x>-outputlen = '15'.
        <x>-hotspot   =  'X'.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'DESCRICAO_MATERIAL'.

        <x>-coltext   = 'Descrição Material' .
        <x>-scrtext_l = 'Descrição Material' .
        <x>-scrtext_m = 'Descrição Material' .
        <x>-scrtext_s = 'Descrição Material' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN 'SAFRA'.

        <x>-coltext   = 'Safra' .
        <x>-scrtext_l = 'Safra' .
        <x>-scrtext_m = 'Safra' .
        <x>-scrtext_s = 'Safra' .


        <x>-outputlen = '12'.
        <x>-hotspot   =  ''.
        <x>-no_zero   =   ''.
        <x>-do_sum    =   ''.
        <x>-just      =   ''.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.


ENDFORM.

FORM custom_container.

  DATA: data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_gtext                TYPE tgsbt-gtext.



  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 1.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 0.

    "PERFORM FILL_GS_VARIANT.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    gs_variant-report  = sy-repid.

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    "SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR CTL_ALV.
    SET HANDLER: lcl_event_handler=>on_hotspot_click   FOR ctl_alv.
    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida.
    "IT_SORT         = T_SORT.


    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'LEFT'
        sap_style = cl_dd_document=>heading.

    p_text = 'Relatório de Reconciliação Entrega Futura - PRODUTORES'.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

    LOOP AT s_bukrs.
      IF s_bukrs-option NE 'EQ' AND s_bukrs-option NE 'BT'.
        sdydo_text_element = 'Empresa: Multiplas Seleções'.
        EXIT.
      ELSEIF s_bukrs-option EQ 'BT'.

        SELECT SINGLE butxt FROM t001  INTO @DATA(vl_butxt)
          WHERE bukrs EQ @s_bukrs-low
          AND spras EQ @sy-langu.

        CONCATENATE 'Empresa:' s_bukrs-low vl_butxt '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: vl_butxt.

        SELECT SINGLE butxt  FROM t001  INTO vl_butxt
          WHERE bukrs EQ s_bukrs-low
          AND spras EQ sy-langu.

        CONCATENATE sdydo_text_element s_bukrs-high vl_butxt INTO sdydo_text_element SEPARATED BY space.

        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
        ELSE.
          SELECT SINGLE butxt  FROM t001 INTO vl_butxt
          WHERE bukrs EQ s_bukrs-low
          AND spras EQ sy-langu.
          CONCATENATE 'Empresa:' s_bukrs-low vl_butxt INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, vl_butxt, sdydo_text_element.

    CONCATENATE  s_budat-low+6(2)  '.' s_budat-low+4(2)  '.' s_budat-low(4)   INTO data_ini.
    CONCATENATE  s_budat-high+6(2) '.' s_budat-high+4(2) '.' s_budat-high(4)  INTO data_fim.

    CONCATENATE 'Fornecedor:'  s_lifnr  INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CONCATENATE 'Data Lançamento:' data_ini '-' data_fim INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CONCATENATE 'Conta Contabil:'  lv_selected_value  INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

  ELSE.
    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.


FORM preenche_catalogo USING  VALUE(p_campo)
                              VALUE(p_desc)
                              VALUE(p_tam)
                              VALUE(p_zero)
                              VALUE(p_hot)
                              VALUE(p_sum)
                              VALUE(p_just).

  wa_fieldcatalog-fieldname = p_campo.
  wa_fieldcatalog-coltext   = p_desc.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.


  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-hotspot   = p_hot.
  wa_fieldcatalog-no_zero   = p_zero.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-just      = p_just.
  wa_fieldcatalog-col_opt   =  'X'.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.
