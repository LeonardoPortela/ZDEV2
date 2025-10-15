*&---------------------------------------------------------------------*
*& Include          ZMMR189_PBO
*&---------------------------------------------------------------------*
FORM display_grid.

  CALL SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  d0100_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0100_pbo OUTPUT.
  PERFORM d0100_pbo.
ENDMODULE.                 " d0100_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  d0100_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0100_pai INPUT.
  PERFORM d0100_pai.
ENDMODULE.                 " d0100_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  d0100_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM d0100_pbo .
  PERFORM docker.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.


ENDFORM.                                                    " d0100_pbo

*&---------------------------------------------------------------------*
*&      Form  d0100_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM d0100_pai .

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'RELATORIO'.
      PERFORM select_data.
    WHEN 'FACTOR'.
      SUBMIT zmmr189_fcon
"WITH p_cdind = ls_dados_p21-cd_indic
      AND RETURN.
    WHEN 'UNIDADES'.
      PERFORM unidades.
    WHEN 'COD_UNIDADES'.
      SUBMIT zmmr189_unidades AND RETURN.
  ENDCASE.

ENDFORM.

FORM unidades.

  TYPES: BEGIN OF ty_unid,
           empresa_cod      TYPE t001-bukrs,
           empresa_nome     TYPE t001-butxt,
           sis_nome_empresa TYPE string,
           filial_cod       TYPE t001w-werks,
           filial_nome      TYPE t001w-name1,
           id_sis(6)        TYPE i,
           sis_nome_unidade TYPE string,
         END OF  ty_unid.

  DATA: it_unid TYPE STANDARD TABLE OF ty_unid WITH HEADER LINE .

  SELECT DISTINCT a~bukrs AS empresa_cod, upper( a~butxt ) AS empresa_nome,b~j_1bbranch AS filial_cod, upper( b~name1 ) AS filial_nome
  FROM t001 AS a
  LEFT JOIN t001w AS b ON b~vkorg = a~bukrs
  WHERE 1 = 1
  AND a~bukrs IS NOT NULL
  AND a~kokfi <> ''
    AND b~iwerk <> ''
    AND b~VKORG <> ''
  ORDER BY a~bukrs,b~j_1bbranch ASCENDING
  INTO TABLE @DATA(t_unid).

  DELETE ADJACENT DUPLICATES FROM t_unid.

  LOOP AT t_unid[] ASSIGNING FIELD-SYMBOL(<wa_unid>).
    SELECT SINGLE cd_unid FROM zfit0204 WHERE substring( nm_unid,1,4 ) = @<wa_unid>-filial_cod INTO @DATA(aux_unid_id).
    CONDENSE aux_unid_id NO-GAPS.
    CONDENSE <wa_unid>-empresa_cod NO-GAPS.
    CONDENSE <wa_unid>-filial_cod NO-GAPS.
    DATA(aux_sis_unidade) = |{ <wa_unid>-filial_cod } - { <wa_unid>-filial_nome }|.
    DATA(aux_sis_empresa) = |{ <wa_unid>-empresa_cod } - { <wa_unid>-empresa_nome }|.
    it_unid-empresa_cod = <wa_unid>-empresa_cod.
    it_unid-empresa_nome = <wa_unid>-empresa_nome.
    it_unid-filial_cod = <wa_unid>-filial_cod.
    it_unid-filial_nome = <wa_unid>-filial_nome.
    it_unid-sis_nome_unidade = aux_sis_unidade.
    it_unid-sis_nome_empresa = aux_sis_empresa.
    it_unid-id_sis = aux_unid_id.

    APPEND it_unid.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_unid.

  cl_demo_output=>display_data( it_unid[] ).

ENDFORM.
