*&---------------------------------------------------------------------*
*& Report  ZMMR160
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr160 MESSAGE-ID z01.

TABLES: j_1bbranch, mkpf, s004, j_1bagn, j_1bnfdoc, mara.

TYPES: BEGIN OF ty_saida_alv.
         INCLUDE STRUCTURE zde_saida_zmmr160.
         TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         ico_carga     TYPE char04,
       END OF ty_saida_alv.

TYPES: BEGIN OF ty_zfiwrt0009.
         INCLUDE STRUCTURE zfiwrt0009.
         TYPES: v_rep_mat  TYPE c,
         v_mat_lido TYPE c,
       END OF ty_zfiwrt0009.

TYPES: BEGIN OF ty_zfiwrt0008.
         INCLUDE STRUCTURE zfiwrt0008.
         TYPES: v_rep_mat TYPE c,
       END OF ty_zfiwrt0008.



DATA: dg_splitter       TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer   TYPE REF TO cl_gui_container,
      ctl_alv           TYPE REF TO cl_gui_alv_grid,
      it_saida          TYPE TABLE OF ty_saida_alv,
      it_zfiwrt0001     TYPE TABLE OF zfiwrt0001,
      it_zfiwrt0009     TYPE TABLE OF ty_zfiwrt0009,
      it_zfiwrt0009_doc TYPE TABLE OF ty_zfiwrt0009,
      it_zfiwrt0008     TYPE TABLE OF zfiwrt0008,
      it_zfiwrt0008_doc TYPE TABLE OF ty_zfiwrt0008,
      it_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
      it_j_1bnfdoc_doc  TYPE TABLE OF j_1bnfdoc,
      it_t001w          TYPE TABLE OF t001w,
      it_makt           TYPE TABLE OF makt,
      it_kna1           TYPE TABLE OF kna1,
      it_lfa1           TYPE TABLE OF lfa1.


DATA: it_fieldcatalog   TYPE lvc_t_fcat,
      gs_variant        TYPE disvariant,
      gs_layout         TYPE lvc_s_layo,
      wa_saida          LIKE LINE OF it_saida,
      wa_zfiwrt0001     TYPE zfiwrt0001,
      wa_zfiwrt0009     TYPE ty_zfiwrt0009,
      wa_zfiwrt0009_doc TYPE ty_zfiwrt0009,
      wa_zfiwrt0008     TYPE zfiwrt0008,
      wa_zfiwrt0008_doc TYPE ty_zfiwrt0008,
      wa_j_1bnfdoc      TYPE j_1bnfdoc,
      wa_j_1bnfdoc_doc  TYPE j_1bnfdoc,
      wa_t001w          TYPE t001w,
      wa_makt           TYPE makt,
      wa_kna1           TYPE kna1,
      wa_lfa1           TYPE lfa1.


RANGES: rg_cfop FOR j_1bagn-cfop.
RANGES: rg_docref FOR zfiwrt0008-docref.


SELECTION-SCREEN BEGIN OF BLOCK int01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: sempre FOR j_1bbranch-bukrs  OBLIGATORY NO INTERVALS NO-EXTENSION,
                sfilia FOR j_1bbranch-branch OBLIGATORY NO INTERVALS NO-EXTENSION,
                sdtpos FOR mkpf-budat OBLIGATORY,
                scfop  FOR j_1bagn-cfop NO INTERVALS,
                sparc  FOR j_1bnfdoc-parid MATCHCODE OBJECT debi_kred,
                sprod  FOR mara-matnr.
PARAMETERS:     pstatu TYPE zde_tipo_status.
SELECTION-SCREEN END OF BLOCK int01.


START-OF-SELECTION.

  PERFORM buscar_parametros.
  PERFORM buscar_dados.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 1.

    ctl_cccontainer = dg_splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = ctl_cccontainer.

    PERFORM fill_it_fieldcatalog.
    PERFORM fill_gs_variant.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida[].

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.
  CLEAR: ctl_alv.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

  CLEAR it_saida[].

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_SAIDA_ZMMR160'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.
    <fs_cat>-tabname = 'ZDE_SAIDA_ZMMR160'.
    <fs_cat>-col_opt = abap_true.

    IF <fs_cat>-fieldname =  'BUKRS'.
      <fs_cat>-no_out = 'X'.
    ENDIF.

    IF <fs_cat>-fieldname =  'BRANCH'.
      <fs_cat>-reptext = 'Filial'.
      <fs_cat>-scrtext_l = 'Filial'.
      <fs_cat>-scrtext_m = 'Filial'.
      <fs_cat>-scrtext_s = 'Filial'.
    ENDIF.

    IF <fs_cat>-fieldname =  'BRANCH_NOME'.
      <fs_cat>-reptext = 'Descrição'.
      <fs_cat>-scrtext_l = 'Descrição'.
      <fs_cat>-scrtext_m = 'Descrição'.
      <fs_cat>-scrtext_s = 'Descrição'.
    ENDIF.

    IF <fs_cat>-fieldname =  'PARCEIRO_COD'.
      <fs_cat>-reptext = 'Parceiro'.
      <fs_cat>-scrtext_l = 'Parceiro'.
      <fs_cat>-scrtext_m = 'Parceiro'.
      <fs_cat>-scrtext_s = 'Parceiro'.
    ENDIF.

    IF <fs_cat>-fieldname =  'PARCEIRO_NOME'.
      <fs_cat>-reptext = 'Descrição'.
      <fs_cat>-scrtext_l = 'Descrição'.
      <fs_cat>-scrtext_m = 'Descrição'.
      <fs_cat>-scrtext_s = 'Descrição'.
    ENDIF.

    IF <fs_cat>-fieldname =  'PARCEIRO_CNPJ'.
      <fs_cat>-reptext = 'CNPJ'.
      <fs_cat>-scrtext_l = 'CNPJ'.
      <fs_cat>-scrtext_m = 'CNPJ'.
      <fs_cat>-scrtext_s = 'CNPJ'.
      <fs_cat>-edit_mask = '__.___.___/____-__'.
      <fs_cat>-just = 'J'.
    ENDIF.


    IF <fs_cat>-fieldname =  'PRODUTO_COD'.
      <fs_cat>-reptext = 'Produto'.
      <fs_cat>-scrtext_l = 'Produto'.
      <fs_cat>-scrtext_m = 'Produto'.
      <fs_cat>-scrtext_s = 'Produto'.
    ENDIF.


    IF <fs_cat>-fieldname =  'PRODUTO_NOME'.
      <fs_cat>-reptext = 'Descrição'.
      <fs_cat>-scrtext_l = 'Descrição'.
      <fs_cat>-scrtext_m = 'Descrição'.
      <fs_cat>-scrtext_s = 'Descrição'.
    ENDIF.


    IF <fs_cat>-fieldname =  'PRODUTO_UNI'.
      <fs_cat>-reptext = 'Unidade'.
      <fs_cat>-scrtext_l = 'Unidade'.
      <fs_cat>-scrtext_m =  'Unidade'.
      <fs_cat>-scrtext_s = 'Unidade'.
    ENDIF.


    IF <fs_cat>-fieldname =  'NF_DOCNUM'.
      <fs_cat>-no_out = 'X'.
    ENDIF.

    IF <fs_cat>-fieldname =  'DOC_REMESSA'.
      <fs_cat>-reptext = 'Docnum Remessa'.
      <fs_cat>-scrtext_l = 'Docnum Remessa'.
      <fs_cat>-scrtext_m = 'Docnum Remessa'.
      <fs_cat>-scrtext_s = 'Docnum Remessa'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_NUMERO'.
      <fs_cat>-reptext = 'NF Remessa'.
      <fs_cat>-scrtext_l = 'NF Remessa'.
      <fs_cat>-scrtext_m = 'NF Remessa'.
      <fs_cat>-scrtext_s = 'NF Remessa'.
      <fs_cat>-no_zero   = 'X'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_EMISSAO'.
      <fs_cat>-reptext = 'Dt Emissão Remessa'.
      <fs_cat>-scrtext_l = 'Dt Emissão Remessa'.
      <fs_cat>-scrtext_m = 'Dt Emissão Remessa'.
      <fs_cat>-scrtext_s = 'DT Emissão Remessa'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_CFOP'.
      <fs_cat>-reptext   = 'CFOP Remessa'.
      <fs_cat>-scrtext_l = 'CFOP Remessa'.
      <fs_cat>-scrtext_m = 'CFOP Remessa'.
      <fs_cat>-scrtext_s = 'CFOP Remessa'.
    ENDIF.


    IF <fs_cat>-fieldname =  'NF_QUANTIDADE'.
      <fs_cat>-reptext = 'Quantidade Remessa'.
      <fs_cat>-scrtext_l = 'Quantidade Remessa'.
      <fs_cat>-scrtext_m = 'Quantidade Remessa'.
      <fs_cat>-scrtext_s = 'Quantidade Remessa'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_RET_DOCNUM'.
      <fs_cat>-no_out = 'X'.
    ENDIF.


    IF <fs_cat>-fieldname =  'DOC_RETORNO'.
      <fs_cat>-reptext = 'Docnum Retorno'.
      <fs_cat>-scrtext_l = 'Docnum Retorno'.
      <fs_cat>-scrtext_m = 'Docnum Retorno'.
      <fs_cat>-scrtext_s = 'Docnum Retorno'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_RET_NUMERO'.
      <fs_cat>-reptext = 'NF Retorno'.
      <fs_cat>-scrtext_l = 'NF Retorno'.
      <fs_cat>-scrtext_m = 'NF Retorno'.
      <fs_cat>-scrtext_s = 'NF Retorno'.
      <fs_cat>-no_zero   = 'X'.
    ENDIF.


    IF <fs_cat>-fieldname =  'NF_RET_CFOP'.
      <fs_cat>-reptext = 'CFOP Retorno'.
      <fs_cat>-scrtext_l = 'CFOP Retorno'.
      <fs_cat>-scrtext_m = 'CFOP Retorno'.
      <fs_cat>-scrtext_s = 'CFOP Retorno'.
    ENDIF.


    IF <fs_cat>-fieldname =  'NF_RET_EMISSAO'.
      <fs_cat>-reptext = 'Dt Emissão Retorno'.
      <fs_cat>-scrtext_l = 'Dt Emissão Retorno'.
      <fs_cat>-scrtext_m = 'Dt Emissão Retorno'.
      <fs_cat>-scrtext_s = 'Dt Emissão Retorno'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_RET_LANCAME'.
      <fs_cat>-reptext = 'Dt Lançamento Ret'.
      <fs_cat>-scrtext_l = 'Dt Lançamento Ret'.
      <fs_cat>-scrtext_m = 'Dt Lançamento Ret'.
      <fs_cat>-scrtext_s = 'Dt Lançamento Retorno'.
    ENDIF.


    IF <fs_cat>-fieldname =  'NF_RET_QUANTIDADE'.
      <fs_cat>-reptext = 'Quantidade Retorno'.
      <fs_cat>-scrtext_l = 'Quantidade Retorno'.
      <fs_cat>-scrtext_m = 'Quantidade Retorno'.
      <fs_cat>-scrtext_s = 'Quantidade Retorno'.

    ENDIF.


    IF <fs_cat>-fieldname =  'NF_SALDO'.
      <fs_cat>-reptext = 'Saldo Aberto'.
      <fs_cat>-scrtext_l = 'Saldo Aberto'.
      <fs_cat>-scrtext_m = 'Saldo Aberto'.
      <fs_cat>-scrtext_s = 'Saldo Aberto'.

    ENDIF.


    IF <fs_cat>-fieldname =  'DT_LIMITE'.
      <fs_cat>-reptext = 'Dt Limite Retorno'.
      <fs_cat>-scrtext_l = 'Dt Limite Retorno'.
      <fs_cat>-scrtext_m = 'Dt Limite Retorno'.
      <fs_cat>-scrtext_s = 'Dt Limite Retorno'.

    ENDIF.

    IF <fs_cat>-fieldname =  'NR_DIAS_VENCTO'.
      <fs_cat>-reptext = 'Nº dias Vencto'.
      <fs_cat>-scrtext_l = 'Nº dias Vencto'.
      <fs_cat>-scrtext_m = 'Nº dias Vencto'.
      <fs_cat>-scrtext_s = 'Nº dias Vencto'.
      <fs_cat>-edit_mask = 'LLV___'.
    ENDIF.

    IF <fs_cat>-fieldname =  'NF_RET_ENTRADA'.
      <fs_cat>-reptext = 'Data Entrada'.
      <fs_cat>-scrtext_l = 'Data Entrada'.
      <fs_cat>-scrtext_m = 'Data Entrada'.
      <fs_cat>-scrtext_s = 'Data Entrada'.
    ENDIF.
    IF <fs_cat>-fieldname =  'DIAS_RET'.
      <fs_cat>-reptext = 'Dias_Retorno'.
      <fs_cat>-scrtext_l = 'Dias_Retorno'.
      <fs_cat>-scrtext_m = 'Dias_Retorno'.
      <fs_cat>-scrtext_s = ''.
    ENDIF.

    IF <fs_cat>-fieldname =  'DAT_REL'.
      <fs_cat>-reptext   = 'Data Relatório'.
      <fs_cat>-scrtext_l = 'Data Relatório'.
      <fs_cat>-scrtext_m = 'Data Relatório'.
      <fs_cat>-scrtext_s = 'Data Relatório'.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
  gs_layout-zebra        = abap_true.
  gs_layout-sel_mode   = 'A'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-stylefname = 'STYLE'.
  gs_layout-ctab_fname = 'COLOR_CELL'.

ENDFORM.

INCLUDE zmmr160_buscar_dadosf01.

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_PARAMETROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_parametros .
* ZMM- BUG84009 RelContMercPoderTerceiros- BG #47610 - inicio
  DATA: "it_cfop  TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.
        it_cfop TYPE TABLE OF  zmmt0162 WITH HEADER LINE.


*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      class         = '0000'
*      setnr         = 'MAGGI_ZMM0167'
*    TABLES
*      set_values    = it_cfop
*    EXCEPTIONS
*      set_not_found = 1
*      OTHERS        = 2.

  SELECT * FROM zmmt0162 INTO TABLE it_cfop.

  CLEAR: rg_cfop.
  LOOP AT it_cfop.
    rg_cfop-sign = 'I'.
    rg_cfop-option = 'EQ'.
    rg_cfop-low  = it_cfop-cfop.
    APPEND rg_cfop.
  ENDLOOP.
* ZMM- BUG84009 RelContMercPoderTerceiros- BG #47610 - FIM
  IF scfop IS INITIAL.
    LOOP AT it_cfop.
      scfop-sign = 'I'.
      scfop-option = 'EQ'.
      scfop-low  = it_cfop-cfop.
      APPEND scfop.
    ENDLOOP.
  ELSE.
    LOOP AT scfop .
      IF scfop-low IN rg_cfop.
        CONTINUE.
      ELSE.
        MESSAGE  'Favor informar um CFOP válido!' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
