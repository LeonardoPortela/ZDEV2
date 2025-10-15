*----------------------------------------------------------------------*
***INCLUDE LZLEST0041I02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZBUSCA_INFO_NOTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zbusca_info_nota INPUT.
  DATA: vl_data_emissao  TYPE j_1bnfdoc-pstdat,
        vl_docnum        TYPE j_1bnfdoc-docnum,
        vl_cod_material  TYPE j_1bnflin-matnr ,
        vl_quantidade    TYPE j_1bnflin-menge ,
        vl_nr_nf         TYPE zlest0041-nr_nf ,
        vl_msg           TYPE string          .

  ok_nota    = 'N'.
  vl_alterou = 'S'.

  SELECT SINGLE nr_nf
    INTO vl_nr_nf
    FROM zlest0041
   WHERE nr_nf_propria    EQ zlest0041-nr_nf_propria
     AND serie_propria    EQ zlest0041-serie_propria
     AND centro_comprador EQ zlest0041-centro_comprador.

  IF sy-subrc IS INITIAL.
    MESSAGE e000(z01) WITH 'Esta Nota/Série/Centro já foram cadastrados'.
  ENDIF.

  SELECT SINGLE docnum pstdat
    INTO (vl_docnum, vl_data_emissao)
    FROM j_1bnfdoc
   WHERE nfenum EQ zlest0041-nr_nf_propria
     AND series EQ zlest0041-serie_propria
     AND branch EQ zlest0041-centro_comprador
     AND direct EQ '2'.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE matnr SUM( menge )
      INTO (vl_cod_material, vl_quantidade)
      FROM j_1bnflin
     WHERE docnum EQ vl_docnum
     GROUP BY matnr.

    zlest0041-quantidade   = vl_quantidade.
    zlest0041-data_emissao = vl_data_emissao.
    zlest0041-cod_material = vl_cod_material.
    zlest0041-docnum       = vl_docnum.
    ok_nota = 'S'.
  ELSE.
    MESSAGE e000(z01) WITH 'Nota Fiscal/Série/Centro não cadastrada !'.
  ENDIF.

ENDMODULE.                 " ZBUSCA_INFO_NOTA  INPUT


*&---------------------------------------------------------------------*
*&      Module  ZBUSCA_INFO_NOTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zbusca_info_nota OUTPUT.
  DATA: vl_data_emissao2 TYPE j_1bnfdoc-pstdat,
        vl_docnum2       TYPE j_1bnfdoc-docnum,
        vl_cod_material2 TYPE j_1bnflin-matnr ,
        vl_quantidade2   TYPE j_1bnflin-menge .

  IF sy-ucomm EQ 'SAVE'.

    SELECT SINGLE docnum pstdat
      INTO (vl_docnum2, vl_data_emissao2)
      FROM j_1bnfdoc
     WHERE nfenum EQ zlest0041-nr_nf_propria
       AND series EQ zlest0041-serie_propria
       AND branch EQ zlest0041-centro_comprador
       AND direct EQ '2' .

    IF sy-subrc IS INITIAL.
      SELECT SINGLE matnr SUM( menge )
        INTO (vl_cod_material2, vl_quantidade2)
        FROM j_1bnflin
       WHERE docnum EQ vl_docnum2
        GROUP BY matnr.

      zlest0041-quantidade   = vl_quantidade2.
      zlest0041-data_emissao = vl_data_emissao2.
      zlest0041-cod_material = vl_cod_material2.
      zlest0041-docnum       = vl_docnum2.
    ELSE.
      MESSAGE e000(z01) WITH 'Nota Fiscal/Série não cadastrada !'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZBUSCA_INFO_NOTA  INPUT


*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA_CLIENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zverifica_fornecedor INPUT.
  DATA: vl_nome TYPE kna1-name1.
  vl_alterou = 'S'.
  ok_fornecedor = 'N'.

  SELECT SINGLE name1
    INTO vl_nome
    FROM lfa1
   WHERE lifnr EQ zlest0041-cod_cliente.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e000(z01) WITH 'Fornecedor não cadastrado !'.
  ENDIF.

  ok_fornecedor = 'S'.

ENDMODULE.                 " ZVERIFICA_CLIENTE  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA_CLIENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zverifica_fornecedor OUTPUT.
  DATA: vl_nome2 TYPE kna1-name1.

  IF sy-ucomm EQ 'SAVE'.

    SELECT SINGLE name1
      INTO vl_nome2
      FROM lfa1
     WHERE lifnr EQ zlest0041-cod_cliente.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e000(z01) WITH 'Fornecedor não cadastrado !'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZVERIFICA_CLIENTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  DETAIL_SET_STU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE detail_set_stu INPUT.
  IF sy-ucomm EQ 'SAVE'.
    status-action = 'U'.
  ENDIF.
ENDMODULE.                 " DETAIL_SET_STU  INPUT
*&---------------------------------------------------------------------*
*&      Module  DETAIL_SET_STU  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE detail_set_stu OUTPUT.
  IF sy-ucomm EQ 'SAVE'.
    status-action = 'U'.
  ENDIF.
ENDMODULE.                 " DETAIL_SET_STU  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SALVAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE salvar INPUT.

  IF ok_fornecedor = 'S' AND ok_nota = 'S' AND vl_alterou = 'S' .
    sy-ucomm = 'SAVE'.
    ok_code  = 'SAVE'.
    vl_alterou = 'N'.
  ENDIF.

ENDMODULE.                 " SALVAR  INPUT
*&---------------------------------------------------------------------*
*&      Module  INSERIR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inserir INPUT.
  zlest0041-Usuario = sy-uname.
  zlest0041-Data    = sy-datum.
  zlest0041-Hora    = sy-UZEIT.

  IF vl_alterou = 'N' AND  zlest0041-centro_comprador IS NOT INITIAL AND zlest0041-nr_nf IS NOT INITIAL AND
     zlest0041-cod_cliente IS NOT INITIAL AND zlest0041-serie IS NOT INITIAL.

    sy-ucomm = 'NEWL'.
    ok_code  = 'NEWL'.
  ENDIF.
ENDMODULE.                 " INSERIR  INPUT
