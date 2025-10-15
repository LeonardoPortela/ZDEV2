INCLUDE ole2incl.
* handles for OLE objects

DATA: h_excel TYPE ole2_object,        " Excel object
      h_mapl  TYPE ole2_object,         " list of workbooks
      h_map   TYPE ole2_object,          " workbook
      h_zl    TYPE ole2_object,           " cell
      h_f     TYPE ole2_object,
      h       TYPE i.                        " font

*----------------------------------------------------------------------*
***INCLUDE LZLEST0041I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_CLIENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_fornecedor INPUT.

  DATA: wnm_cliente TYPE kna1-name1.

  SELECT SINGLE name1
    INTO wnm_cliente
    FROM lfa1
    WHERE lifnr EQ zlest0041-cod_cliente.

  nm_cliente = wnm_cliente .

ENDMODULE.                 " BUSCA_NOME_CLIENTE  INPUT

*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_CLIENTE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_fornecedor OUTPUT.

  DATA: wnm_cliente2 TYPE kna1-name1.

  SELECT SINGLE name1
    INTO wnm_cliente2
    FROM lfa1
    WHERE lifnr EQ zlest0041-cod_cliente.

  nm_cliente = wnm_cliente2.

ENDMODULE.                 " BUSCA_NOME_CLIENTE  INPUT


*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_material INPUT.

  DATA: wnm_material TYPE makt-matnr.

  SELECT SINGLE maktx
    INTO wnm_material
    FROM makt
    WHERE matnr EQ zlest0041-cod_material.

  nm_material = wnm_material.

ENDMODULE.                 " BUSCA_NOME_CLIENTE  INPUT

*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_MATERIAL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_material OUTPUT.

  DATA: wnm_material2 TYPE makt-matnr.

  SELECT SINGLE maktx
    INTO wnm_material2
    FROM makt
    WHERE matnr EQ zlest0041-cod_material
      AND spras EQ sy-langu.

  nm_material = wnm_material2.

ENDMODULE.                 " BUSCA_NOME_CLIENTE  INPUT

*---------------------------------------------------------------------*
*       FORM FILL_CELL                                                *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM fill_cell USING i j bold val.
  CALL METHOD OF h_excel 'Cells' = h_zl EXPORTING #1 = i #2 = j.
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
ENDFORM.                    "ERR_HDL

*&---------------------------------------------------------------------*
*&      Form  GERA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_excel .

  IF zlest0041 IS INITIAL.
    MESSAGE w000(z01) WITH 'Não ha dados para Gerar EXCEL !' .
    CHECK NOT zlest0041 IS INITIAL.
    "    stop.
  ENDIF.



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
*     PERCENTAGE = 0
      text   = text-009
    EXCEPTIONS
      OTHERS = 1.


  CALL METHOD OF h_excel 'Worksheets' = h_mapl." EXPORTIN G #1 = 2.

* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map EXPORTING #1 = 2.

* tell user what is going on
  SET PROPERTY OF h_map 'NAME' = 'COPY'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = text-009
    EXCEPTIONS
      OTHERS = 1.

* output column headings to active Excel sheet
  PERFORM fill_cell USING 1 1  1 'N° Nfe Terceiro'(001).
  PERFORM fill_cell USING 1 2  1 'Série'(002).
  PERFORM fill_cell USING 1 3  1 'Fornecedor'(003).
  PERFORM fill_cell USING 1 4  1 'Nome'(004).
  PERFORM fill_cell USING 1 5  1 'Centro'(005).
  PERFORM fill_cell USING 1 6  1 'N° Nfe Própria'(006).
  PERFORM fill_cell USING 1 7  1 'Série'(007).
  PERFORM fill_cell USING 1 8  1 'Nr Documento'(008).
  PERFORM fill_cell USING 1 9  1 'Quantidade'(009).
  PERFORM fill_cell USING 1 10 1 'Data de Emissão'(010).
  PERFORM fill_cell USING 1 11 1 'Material'(011).
  PERFORM fill_cell USING 1 12 1 'Descrição'(012).


  LOOP AT zlest0041.
    h = sy-tabix + 1.
    PERFORM fill_cell USING h 1  0 zlest0041-nr_nf.
    PERFORM fill_cell USING h 2  0 zlest0041-serie.
    PERFORM fill_cell USING h 3  0 zlest0041-cod_cliente.
    "PERFORM fill_cell USING h 4  0 zlest0041-nm_cliente.
    PERFORM fill_cell USING h 5  0 zlest0041-centro_comprador.
    PERFORM fill_cell USING h 6  0 zlest0041-nr_nf_propria.
    PERFORM fill_cell USING h 7  0 zlest0041-serie_propria.
    PERFORM fill_cell USING h 8  0 zlest0041-docnum.
    PERFORM fill_cell USING h 9  0 zlest0041-quantidade.
    PERFORM fill_cell USING h 10 0 zlest0041-data_emissao.
    PERFORM fill_cell USING h 11 0 zlest0041-cod_material.
    "PERFORM fill_cell USING h 12 0 zlest0041-nm_material.

  ENDLOOP.

  FREE OBJECT h_excel.

  MESSAGE i000(z01) WITH 'Gerado para o EXCEL com sucesso !' .

ENDFORM.                    " GERA_EXCEL
*&---------------------------------------------------------------------*
*&      Module  ZUSER_COMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zuser_comand INPUT.
  IF sy-ucomm EQ 'EXP_EXCEL' .
    PERFORM gera_excel.
  ENDIF.

ENDMODULE.                 " ZUSER_COMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  CABECALHO_EXCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cabecalho_excel INPUT.
  IF sy-ucomm EQ 'EXP_EXCEL' .

*   start Excel
    CREATE OBJECT h_excel 'EXCEL.APPLICATION'.
*    PERFORM ERR_HDL.

    SET PROPERTY OF h_excel  'Visible' = 1.

*   tell user what is going on
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text   = text-008
      EXCEPTIONS
        OTHERS = 1.

*   get list of workbooks, initially empty
    CALL METHOD OF h_excel 'Workbooks' = h_mapl.
    PERFORM err_hdl.
*   add a new workbook
    CALL METHOD OF h_mapl 'Add' = h_map.
    PERFORM err_hdl.
*   tell user what is going on
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
*       PERCENTAGE = 0
        text   = text-009
      EXCEPTIONS
        OTHERS = 1.


    CALL METHOD OF h_excel 'Worksheets' = h_mapl." EXPORTIN G #1 = 2.

*   add a new workbook
    CALL METHOD OF h_mapl 'Add' = h_map EXPORTING #1 = 2.

*   tell user what is going on
    SET PROPERTY OF h_map 'NAME' = 'COPY'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text   = text-009
      EXCEPTIONS
        OTHERS = 1.

*   output column headings to active Excel sheet
    PERFORM fill_cell USING 1 1  1 'N° Nfe Terceiro'(001).
    PERFORM fill_cell USING 1 2  1 'Série'(002).
    PERFORM fill_cell USING 1 3  1 'Fornecedor'(003).
    PERFORM fill_cell USING 1 4  1 'Nome'(004).
    PERFORM fill_cell USING 1 5  1 'Centro'(005).
    PERFORM fill_cell USING 1 6  1 'N° Nfe Própria'(006).
    PERFORM fill_cell USING 1 7  1 'Série'(007).
    PERFORM fill_cell USING 1 8  1 'Nr Documento'(008).
    PERFORM fill_cell USING 1 9  1 'Quantidade'(009).
    PERFORM fill_cell USING 1 10 1 'Data de Emissão'(010).
    PERFORM fill_cell USING 1 11 1 'Material'(011).
    PERFORM fill_cell USING 1 12 1 'Descrição'(012).

    h = 1.
  ENDIF.

ENDMODULE.                 " CABECALHO_EXCEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  DETALHE_EXCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE detalhe_excel INPUT.
  IF sy-ucomm EQ 'EXP_EXCEL' .
    h = h + 1.
    PERFORM fill_cell USING h 1  0 zlest0041-nr_nf.
    PERFORM fill_cell USING h 2  0 zlest0041-serie.
    PERFORM fill_cell USING h 3  0 zlest0041-cod_cliente.
    PERFORM fill_cell USING h 4  0 nm_cliente.
    PERFORM fill_cell USING h 5  0 zlest0041-centro_comprador.
    PERFORM fill_cell USING h 6  0 zlest0041-nr_nf_propria.
    PERFORM fill_cell USING h 7  0 zlest0041-serie_propria.
    PERFORM fill_cell USING h 8  0 zlest0041-docnum.
    PERFORM fill_cell USING h 9  0 zlest0041-quantidade.
    PERFORM fill_cell USING h 10 0 zlest0041-data_emissao.
    PERFORM fill_cell USING h 11 0 zlest0041-cod_material.
    PERFORM fill_cell USING h 12 0 nm_material.
  ENDIF.
ENDMODULE.                 " DETALHE_EXCEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIM_EXCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fim_excel INPUT.
  IF sy-ucomm EQ 'EXP_EXCEL' .
    FREE OBJECT h_excel.

    MESSAGE i000(z01) WITH 'Gerado para o EXCEL com sucesso !' .
  ENDIF.
ENDMODULE.                 " FIM_EXCEL  INPUT
