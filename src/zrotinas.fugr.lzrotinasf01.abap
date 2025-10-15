*----------------------------------------------------------------------*
***INCLUDE LZROTINASF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GERA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gera_excel TABLES tabela USING titulo1 TYPE string titulo2 TYPE string arquivo TYPE string p_tabname TYPE ddobjname.

  DATA: n_row         TYPE i,
        n_col         TYPE i,
        q_col         TYPE i,
        p_soma        TYPE i,
        length        TYPE i,
        p_row         TYPE string,
        p_col         TYPE string,
        texto         TYPE string.

  DATA: lr_table_des   TYPE REF TO cl_abap_structdescr,
        lt_details     TYPE abap_compdescr_tab,
        tb_dfies_tab   TYPE TABLE OF dfies WITH HEADER LINE INITIAL SIZE 0.

  FIELD-SYMBOLS:
    <comp_wa> TYPE abap_compdescr,
    <comp_nm> TYPE dfies.

  lr_table_des ?= cl_abap_structdescr=>describe_by_data( tabela ).
  lt_details[] = lr_table_des->components[].

  CREATE OBJECT wv_excel 'Excel.Application'.
  SET PROPERTY OF wv_excel 'Visible' = 1.
  CALL METHOD OF wv_excel 'Workbooks' = wv_workbook.
  CALL METHOD OF wv_workbook 'Add' = wv_book.

  n_row = 1.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabname
    TABLES
      dfies_tab      = tb_dfies_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    STOP.
  ELSE.
    DESCRIBE TABLE tb_dfies_tab LINES q_col.
  ENDIF.

  IF NOT titulo1 IS INITIAL.
    PERFORM inclui_celula_titulo USING n_row 1 titulo1 1 q_col 14.
    n_row = n_row + 1.
  ENDIF.

  IF NOT titulo2 IS INITIAL.
    PERFORM inclui_celula_titulo USING n_row 1 titulo2 1 q_col 12.
    n_row = n_row + 1.
  ENDIF.

  n_row = n_row + 1.

  n_col  = 1.
  p_row = n_row.
  LOOP AT tb_dfies_tab ASSIGNING <comp_nm>.
    texto = <comp_nm>-scrtext_s.
    p_col = n_col.
    PERFORM inclui_celula_2 USING n_row n_col texto 1.
    n_col = n_col + 1.
  ENDLOOP.
  n_row = n_row + 1.

  LOOP AT tabela.

    p_row  = n_row.
    n_col  = 1.
    p_soma = 0.
    p_col  = n_col.

    PERFORM troca_cor_linha.

    LOOP AT lt_details[] ASSIGNING <comp_wa>.
      p_col  = n_col.
      length = <comp_wa>-length / 2.
      texto = tabela+p_soma(length).
      PERFORM inclui_celula USING n_row n_col texto '0'  <comp_wa>-type_kind.
      p_soma = p_soma + length.
      n_col = n_col + 1.
    ENDLOOP.

    n_row = n_row + 1.
  ENDLOOP.

  GET PROPERTY OF wv_excel 'ActiveWorkbook' = wv_awork.

  FREE OBJECT: wv_range, wv_workbook, wv_excel, wv_awork, gs_cell1, gs_font, gs_interior.

ENDFORM.                    " GERA_EXCEL

*&---------------------------------------------------------------------*
*&      Form  INCLUI_CELULA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inclui_celula  USING p_p_row TYPE i
                          p_p_col TYPE i
                          p_texto TYPE string
                          bold    TYPE c
                          tipo    TYPE c.

  CALL METHOD OF wv_excel 'Cells' = gs_cell1
    EXPORTING
    #1 = p_p_row
    #2 = p_p_col.

  GET PROPERTY OF gs_cell1 'Characters' = gs_cell2.

  IF tipo EQ 'C'.
    "CONCATENATE p_texto INTO p_texto.
  ELSEIF tipo EQ 'D'.
    CONCATENATE p_texto+6(2) '/' p_texto+4(2) '/' p_texto(4) INTO p_texto.
  ELSEIF tipo EQ 'T'.
    CONCATENATE p_texto(2) ':' p_texto+2(2) ':' p_texto+4(2) INTO p_texto.
  ENDIF.

  SET PROPERTY OF gs_cell2 'Text' = p_texto.

  GET PROPERTY OF gs_cell1 'Interior' = gs_interior.

  if p_cor_linha is initial.
    p_cor_linha = 10.
  endif.

  SET property of gs_interior 'ColorIndex' = p_cor_linha.

ENDFORM.                    " INCLUI_CELULA

*&---------------------------------------------------------------------*
*&      Form  INCLUI_CELULA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inclui_celula_2  USING    p_n_row TYPE i
                               p_n_col TYPE i
                               p_texto TYPE string
                               bold    TYPE i.

  CALL METHOD OF wv_excel 'Cells' = gs_cell1
    EXPORTING
    #1 = p_n_row
    #2 = p_n_col.

  SET PROPERTY OF gs_cell1 'Value' = p_texto.
  GET PROPERTY OF gs_cell1 'Font'  = gs_font.

  "SET PROPERTY OF gs_font 'Underline' = 2 .
  SET PROPERTY OF gs_font 'Bold' = bold .
  "SET PROPERTY OF gs_cell1 'HorizontalAlignment' = -4108 .
  GET PROPERTY OF gs_cell1 'Interior' = gs_interior.
  SET PROPERTY OF gs_interior 'ColorIndex' = 15 .
  "SET PROPERTY OF gs_interior 'Pattern' = -4124 .
  "SET PROPERTY OF gs_interior 'PatternColorIndex' = -4105 .

ENDFORM.                    " INCLUI_CELULA_2

*&---------------------------------------------------------------------*
*&      Form  INCLUI_CELULA_TITULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inclui_celula_titulo  USING    p_n_row   TYPE i
                                    p_n_col   TYPE i
                                    p_titulo1 TYPE string
                                    bold      TYPE i
                                    p_q_col   TYPE i
                                    p_size    TYPE i.

  IF NOT p_q_col IS INITIAL.
*--Selecting cell area to be merged.
    CALL METHOD OF wv_excel 'Cells' = gs_cell1
      EXPORTING
      #1 = p_n_row
      #2 = p_n_col.
    CALL METHOD OF wv_excel 'Cells' = gs_cell2
      EXPORTING
      #1 = p_n_row
      #2 = p_q_col.
    CALL METHOD OF wv_excel 'Range' = gs_cells
      EXPORTING
      #1 = gs_cell1
      #2 = gs_cell2.
    CALL METHOD OF gs_cells 'Select' .
*--Merging
    CALL METHOD OF gs_cells 'Merge' .
  ENDIF.

  CALL METHOD OF wv_excel 'Cells' = gs_cell1
    EXPORTING
    #1 = p_n_row
    #2 = p_n_col.

  SET PROPERTY OF gs_cell1 'Value' = p_titulo1.
  GET PROPERTY OF gs_cell1 'Font'  = gs_font.

  SET PROPERTY OF gs_font 'Bold'   = bold .
  SET PROPERTY OF gs_font 'Size'   = p_size.

ENDFORM.                    " INCLUI_CELULA_TITULO

*&---------------------------------------------------------------------*
*&      Form  TROCA_COR_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM troca_cor_linha .
  IF p_cor_linha IS INITIAL.
    p_cor_linha = 2.
  ELSEIF p_cor_linha EQ 2.
    p_cor_linha = 19.
  ELSE.
    p_cor_linha = 2.
  ENDIF.
ENDFORM.                    " TROCA_COR_LINHA
