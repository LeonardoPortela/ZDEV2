*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_9006 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9006_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9006_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9006_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9006 INPUT.

  IF ok_code_9006 EQ c_conf.

    IF NOT ( vg_seq1 IS INITIAL ).
      PERFORM atualiza_cod_fluxo_caixa USING vg_seq1 vg_seq_desc1.
    ENDIF.

    IF NOT ( vg_seq2 IS INITIAL ).
      PERFORM atualiza_cod_fluxo_caixa USING vg_seq2 vg_seq_desc2.
    ENDIF.

    IF NOT ( vg_seq3 IS INITIAL ).
      PERFORM atualiza_cod_fluxo_caixa USING vg_seq3 vg_seq_desc3.
    ENDIF.

    CALL METHOD g_tree->delete_all_nodes.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9006  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9006 OUTPUT.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9006'.

  IF sy-ucomm IS NOT INITIAL.
    CLEAR: vg_seq1,
           vg_seq2,
           vg_seq3,
           vg_seq_desc1,
           vg_seq_desc2,
           vg_seq_desc3.
  ENDIF.


  IF vg_seq1 IS NOT INITIAL.
    SELECT SINGLE descricao
      FROM zfit0109
      INTO vg_seq_desc1
      WHERE seq = vg_seq1.
  ENDIF.

  IF vg_seq2 IS NOT INITIAL.
    SELECT SINGLE descricao
      FROM zfit0109
      INTO vg_seq_desc2
      WHERE seq = vg_seq2.
  ENDIF.

  IF vg_seq3 IS NOT INITIAL.
    SELECT SINGLE descricao
      FROM zfit0109
      INTO vg_seq_desc3
      WHERE seq = vg_seq3.
  ENDIF.

ENDMODULE.                 " STATUS_9006  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CHECK_COD_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_SEQ1  text
*----------------------------------------------------------------------*
FORM f_check_cod_seq  USING    p_vg_seq.

  CHECK p_vg_seq IS NOT INITIAL.

  SELECT SINGLE *
  FROM zfit0109
  INTO @DATA(ls_zfit0109)
  WHERE codigo = @p_vg_seq.
  IF sy-subrc <> 0.
    CONCATENATE 'Código inválido -' p_vg_seq INTO DATA(cod_fluxo_invalido) SEPARATED BY space.
    MESSAGE cod_fluxo_invalido TYPE 'E'." DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_CHECK_CODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_check_code_seq1 INPUT.
  PERFORM f_check_cod_seq USING vg_seq1.
ENDMODULE.

MODULE z_check_code_seq2 INPUT.
  PERFORM f_check_cod_seq USING vg_seq2.
ENDMODULE.

MODULE z_check_code_seq3 INPUT.
  PERFORM f_check_cod_seq USING vg_seq3.
ENDMODULE.
