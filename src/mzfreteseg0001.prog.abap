*----------------------------------------------------------------------*
***INCLUDE MZFRETESEG0001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'PFCONS'.
  SET TITLEBAR 'TLCONS'.

  vg_dynnr_0002 = '0002'.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  CASE ok_code.
    WHEN 'PESQ'.
      PERFORM pesquisar.
      CLEAR ok_code.
    WHEN 'NOVO'.
      PERFORM novo_lancamento.
      CLEAR ok_code.
    WHEN 'TAXA'.
      PERFORM altera_taxa.
      CLEAR ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  NOVO_LANCAMENTO
*&---------------------------------------------------------------------*
*       Novo lançamento de valor de seguro
*----------------------------------------------------------------------*
FORM novo_lancamento .
  SELECT SINGLE * FROM zvalor_seg_taxa.
  IF sy-subrc NE 0.
    MESSAGE 'Primeiro deve ser cadastrada uma taxa de imposto' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    vg_insert = 'X'.
    CLEAR: zvalor_seg_terc.
    zvalor_seg_terc-cd_moeda = 'BRL  '.
    CALL SCREEN '0006' STARTING AT 25 6.
    IF ok_code EQ 'NOVOC'.
      PERFORM novo_lancamento.
    ENDIF.
  ENDIF.
ENDFORM.                    " NOVO_LANCAMENTO

*&---------------------------------------------------------------------*
*&      Form  ALTERA_TAXA
*&---------------------------------------------------------------------*
*       Altera taxa de calculo de imposto de valor total seguro
*----------------------------------------------------------------------*
FORM altera_taxa .

  CALL SCREEN '0004' STARTING AT 25 6.

  IF NOT gf_cancel IS INITIAL.
    CLEAR: gf_cancel.
    SELECT SINGLE * INTO wa_valor_taxa FROM zvalor_seg_taxa.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING wa_valor_taxa TO zvalor_seg_taxa.
    ELSE.
      zvalor_seg_taxa-cd_moeda = 'BRL'.
    ENDIF.
    CALL SCREEN '0005'  STARTING AT 25 6.
    IF NOT gf_cancel IS INITIAL.
      MESSAGE 'Processo efetuado!' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.                    " ALTERA_TAXA

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pesquisar .

  CLEAR: it_seg_terc[], it_valor_seg[].

  CALL FUNCTION 'Z_RETORNA_SEG_TERC'
    EXPORTING
      p_monat      = p_monat
      p_gjahr      = p_gjahr
      p_matnr      = p_matnr
      rt_inicio    = p_inicio[]
      rt_final     = p_final[]
    TABLES
      it_seg_terc  = it_seg_terc
    EXCEPTIONS
      sem_registro = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT it_seg_terc INTO wa_seg_terc.
      MOVE-CORRESPONDING wa_seg_terc TO wa_valor_seg.
      CLEAR: wa_valor_seg-mark.
      wa_valor_seg-icone = icon_open.

      SELECT SINGLE text INTO wa_valor_seg-nm_cidade_ini
        FROM j_1btxjurt
       WHERE spras      EQ sy-langu
         AND country    EQ wa_valor_seg-cd_pais_ini
         AND taxjurcode EQ wa_valor_seg-cd_cidade_ini.

      SELECT SINGLE text INTO wa_valor_seg-nm_cidade_fim
        FROM j_1btxjurt
       WHERE spras      EQ sy-langu
         AND country    EQ wa_valor_seg-cd_pais_fim
         AND taxjurcode EQ wa_valor_seg-cd_cidade_fim.

      SELECT SINGLE maktx INTO wa_valor_seg-nm_material
        FROM makt
       WHERE matnr EQ wa_valor_seg-cd_material.

      APPEND wa_valor_seg TO it_valor_seg.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " PESQUISAR

*&---------------------------------------------------------------------*
*&      Module  EXIT_APLICATIVO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_aplicativo INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT_APLICATIVO  INPUT
