*&---------------------------------------------------------------------*
*& Report  ZFIR056
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfir0062.

INCLUDE zfir0062_top.

TABLES: zfit0080.

DATA: it_teste TYPE TABLE OF zfit0080.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
      p_bukrs    FOR zfit0080-bukrs. " Empresa
  PARAMETERS:
      p_mm_ano(6) TYPE c.
  SELECT-OPTIONS:
      p_waers    FOR zfit0080-waers NO INTERVALS NO-EXTENSION.

  PARAMETER: s_c_sld  AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-004.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*

DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid        = sy-repid.
  variante-report = vg_repid.

  IF ( p_varia IS NOT INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*


START-OF-SELECTION.

  "Check campos.
  IF p_bukrs IS INITIAL.
    MESSAGE e024(sd) WITH 'É obrigatório preenchimento da empresa'.
    EXIT.
  ENDIF.

  IF p_mm_ano IS INITIAL.
    MESSAGE e024(sd) WITH 'É obrigatório preenchimento da data'.
    EXIT.
  ENDIF.

  IF p_waers IS INITIAL.
    MESSAGE e024(sd) WITH 'É obrigatório preenchimento da moeda'.
    EXIT.
  ENDIF.

  PERFORM limpa_dados.

  PERFORM valida_campos CHANGING vg_valida_campos.

  IF vg_valida_campos IS INITIAL.

    PERFORM iniciar_variaveis.
    PERFORM seleciona_dados.
    PERFORM processa_dados.
    PERFORM imprimir_dados.

  ENDIF.

  INCLUDE zfir0062_form.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
