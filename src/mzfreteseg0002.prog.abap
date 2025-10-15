*----------------------------------------------------------------------*
***INCLUDE MZFRETESEG0002 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  IF vg_dynnr_1100 IS INITIAL.
    vg_dynnr_1100 = '1100'.
  ENDIF.

  IF vg_dynnr_0003 IS INITIAL.
    vg_dynnr_0003 = '0003'.
  ENDIF.

ENDMODULE.                 " STATUS_0002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.
  IF ok_code EQ 'DELETE'.
    READ TABLE it_valor_seg INTO wa_valor_seg WITH KEY mark = 'X'.
    IF sy-subrc EQ 0.
      DELETE FROM zvalor_seg_terc
       WHERE monat          EQ wa_valor_seg-monat
         AND gjahr          EQ wa_valor_seg-gjahr
         AND cd_pais_ini    EQ wa_valor_seg-cd_pais_ini
         AND cd_cidade_ini  EQ wa_valor_seg-cd_cidade_ini
         AND cd_uf_ini      EQ wa_valor_seg-cd_uf_ini
         AND cd_pais_fim    EQ wa_valor_seg-cd_pais_fim
         AND cd_cidade_fim  EQ wa_valor_seg-cd_cidade_fim
         AND cd_uf_fim      EQ wa_valor_seg-cd_uf_fim
         AND cd_material    EQ wa_valor_seg-cd_material
         AND cd_moeda       EQ wa_valor_seg-cd_moeda
         AND dt_inicio      EQ wa_valor_seg-dt_inicio.
      COMMIT WORK.
      MESSAGE 'Registro excluido com sucesso!' TYPE 'S'.
    ELSE.
      MESSAGE 'Favor selecionar um registro!' TYPE 'E' DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0002  INPUT
