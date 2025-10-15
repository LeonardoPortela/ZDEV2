*----------------------------------------------------------------------*
***INCLUDE LZLEST0036I01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_filial INPUT.
  DATA: wnm_filial TYPE t001w-name1.

  SELECT SINGLE name1
    INTO wnm_filial
    FROM t001w
    WHERE werks EQ zlest0036-branch.

  nm_filial = wnm_filial .

ENDMODULE.                 " BUSCA_NOME_FILIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_FILIAL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_filial OUTPUT.
  DATA: wnm_filial2 TYPE t001w-name1.

  SELECT SINGLE name1
    INTO wnm_filial2
    FROM t001w
    WHERE werks EQ zlest0036-branch.

  nm_filial = wnm_filial2 .
ENDMODULE.                 " BUSCA_NOME_FILIAL  OUTPUT
