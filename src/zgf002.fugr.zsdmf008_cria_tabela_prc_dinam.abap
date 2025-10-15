FUNCTION zsdmf008_cria_tabela_prc_dinam.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TP_VENDA) TYPE  ZSDT0057-TP_VENDA OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TABLE) TYPE REF TO  DATA
*"  TABLES
*"      TE_FIELDCATALOG TYPE  LVC_T_FCAT OPTIONAL
*"      TE_FIELDS STRUCTURE  ZSDT0072 OPTIONAL
*"----------------------------------------------------------------------
  DATA: BEGIN OF tl_fields OCCURS 10,
          field TYPE zsdt0072-field,
        END OF tl_fields.
  DATA: tl_0058      TYPE TABLE OF zsdt0058 WITH HEADER LINE,
        wl_0057      TYPE zsdt0057,
        tl_0072      TYPE TABLE OF zsdt0072 WITH HEADER LINE,
        wl_field(30),
        gd_tabfield  TYPE string.

  DATA: BEGIN OF tl_table OCCURS 0,
          data TYPE REF TO data,
        END OF tl_table.

  DATA: lt_struc TYPE REF TO cl_abap_structdescr,
        lt_tab   TYPE REF TO cl_abap_tabledescr.

  DATA: l_index TYPE c.

  DATA: w_line    TYPE REF TO data,
        int_table TYPE REF TO data.

  FIELD-SYMBOLS: <wa>    TYPE any,
                 <itab>  TYPE table,
                 <value> TYPE any.


  FIELD-SYMBOLS: <fs_table> TYPE any.

  REFRESH: t_fieldcatalog,
           tl_0058,
           tl_0072,
           tl_fields.

  CLEAR: wl_field, wl_col_pos.



  SELECT SINGLE *
    FROM zsdt0057
    INTO wl_0057
     WHERE tp_venda EQ i_tp_venda.

  tg_tp_venda = wl_0057-param_espec. "RJF  "*-Equalização RISE x PRD - 19.07.2023 - JT

  SELECT *
    FROM zsdt0058
    INTO TABLE tl_0058
     WHERE esq_calc EQ wl_0057-esq_calc.

  LOOP AT tl_0058.
    READ TABLE tl_fields
      WITH KEY field = tl_0058-field.
    IF sy-subrc IS NOT  INITIAL.
      tl_fields-field =  tl_0058-field.
      APPEND tl_fields.
      CLEAR: tl_fields.

    ENDIF.

  ENDLOOP.

  IF tl_0058[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0072
      INTO TABLE tl_0072
       FOR ALL ENTRIES IN tl_0058
       WHERE field EQ tl_0058-field.

  ENDIF.

  LOOP AT tl_fields.

    wl_field = tl_fields-field.
    CONDENSE wl_field NO-GAPS.
    READ TABLE tl_0072
      WITH KEY field = tl_fields-field.
*    CONCATENATE WL_FIELD '2' INTO WL_FIELD.

    PERFORM montar_estrutura_oo USING
     tl_0072-col_pos  'ZSDT0059'         'FORMULA2'         '<FS_TABLE>'  wl_field   tl_0072-txt_field '20' ' ' ' ' ' '.


  ENDLOOP.

  PERFORM montar_layout USING '<FS_TABLE>'.

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.
  DATA: ls_fieldcat TYPE kkblo_fieldcat.

  CALL FUNCTION 'LVC_TRANSFER_TO_KKBLO'
    EXPORTING
      it_fieldcat_lvc   = t_fieldcatalog
    IMPORTING
      et_fieldcat_kkblo = lt_fieldcat.


  CLEAR: w_line, lt_tab, lt_struc.
  REFRESH lt_components.
  LOOP AT lt_fieldcat INTO ls_fieldcat.

    CLEAR component.
    CONCATENATE ls_fieldcat-ref_tabname ls_fieldcat-ref_fieldname INTO gd_tabfield
                                                SEPARATED BY '-'.

    component-name = ls_fieldcat-fieldname.
    component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).

    INSERT component INTO TABLE lt_components.
  ENDLOOP.

*  LOOP AT TL_FIELDS.
*    WL_FIELD = TL_FIELDS-FIELD.
*    CONDENSE WL_FIELD NO-GAPS.
*    READ TABLE LT_COMPONENTS INTO COMPONENT
*      WITH KEY NAME = WL_FIELD.
*    IF SY-SUBRC IS INITIAL.
*      CONCATENATE COMPONENT-NAME '2' INTO COMPONENT-NAME .
*      INSERT COMPONENT INTO TABLE LT_COMPONENTS.
*    ENDIF.
*  ENDLOOP.

** Inclui campo NIVEL
  CLEAR component.
  gd_tabfield = 'ZSDS006-NIVEL'.
  component-name = 'NIVEL'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo COD_FP
  CLEAR component.
  gd_tabfield = 'ZSDS006-COD_FP'.
  component-name = 'COD_FP'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo FIELD
  CLEAR component.
  gd_tabfield = 'ZSDS006-FIELD'.
  component-name = 'FIELD'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo TIPO_CALC
  CLEAR component.
  gd_tabfield = 'ZSDS006-TIPO_CALC'.
  component-name = 'TIPO_CALC'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo FORMULA
  CLEAR component.
  gd_tabfield = 'ZSDS006-FORMULA'.
  component-name = 'FORMULA'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo C_DECIMAIS
  CLEAR component.
  gd_tabfield = 'ZSDS006-C_DECIMAIS'.
  component-name = 'C_DECIMAIS'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo OCBOT
  CLEAR component.
  gd_tabfield = 'ZSDT0059-OCBOT'.
  component-name = 'OCBOT'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo INVISIBLE
  CLEAR component.
  gd_tabfield = 'ZSDT0056-INVISIBLE'.
  component-name = 'INVISIBLE'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo PRECO_ITEM
  CLEAR component.
  gd_tabfield = 'ZSDT0059-PRECO'.
  component-name = 'PRECO_ITEM'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo POSNR
  CLEAR component.
  gd_tabfield = 'ZSDT0059-POSNR'.
  component-name = 'POSNR'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

  " 14.02.2024 - 121095 - RBL -->
** Inclui campo REDIST
  CLEAR component.
  gd_tabfield = 'ZSDT0059_2-REDIST'.
  component-name = 'REDIST'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.
  " 14.02.2024 - 121095 - RBL --<

  " 14.05.2024 - 140755 - RAMON - CODIGO ADICIONADO PARA CONTROLAR VALORES DO PREÇO QUE PODEM SER SOBRESCRITOS
  " EX MODIFICAÇÃO QUANTIDADE
  CLEAR component.
  component-name = 'TP_ROW'. "<-- TIPO DO VALOR
  component-type ?= cl_abap_elemdescr=>get_c( p_length = 1 ).
  INSERT component INTO TABLE lt_components.
  " 14.05.2024 - 140755 - RAMON --<

** Inclui campo ITEM_KEY
  CLEAR component.
  gd_tabfield = 'TBRF126-NODE'.
  component-name = 'ITEM_KEY'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo STYLE
  CLEAR component.
  gd_tabfield = 'RSBGUI_S_GRID_DATA-STYLET'.
  component-name = 'STYLE'.
  component-type ?= cl_abap_datadescr=>describe_by_name( gd_tabfield ).
  INSERT component INTO TABLE lt_components.

** Inclui campo LINE_COLOR
  CLEAR component.
  component-name = 'LINE_COLOR'.
  component-type ?= cl_abap_elemdescr=>get_c( p_length = 4 ).
  INSERT component INTO TABLE lt_components.

* Workarea
  lt_struc = cl_abap_structdescr=>create( p_components = lt_components
                                          p_strict     = 'X' ).
  CREATE DATA w_line TYPE HANDLE lt_struc.
  ASSIGN w_line->* TO <wa>.

* Table
  lt_tab = cl_abap_tabledescr=>create( p_line_type = lt_struc ).

  CREATE DATA e_table TYPE HANDLE lt_tab.


  te_fieldcatalog[] = t_fieldcatalog[].
  te_fields[] = tl_0072[].

ENDFUNCTION.
