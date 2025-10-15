*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECT-OPTIONS:  s_cte      FOR zib_cte_dist_ter-cd_chave_cte,
                 s_bukrs    FOR zlest0019-bukrs,
                 s_branch   FOR zlest0019-branch,
                 s_idvaga   FOR zlest0019-idvagao,
                 s_ort01    FOR lfa1-ort01,
                 s_lifnr    FOR lfa1-lifnr,
                 s_matnr    FOR mara-matnr,
                 s_dtemi    FOR zib_cte_dist_ter-dt_emissao,
                 s_dtcomp   FOR bsak-budat.
SELECTION-SCREEN END   OF BLOCK b1.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF s_cte[]    IS INITIAL AND
     s_bukrs[]  IS INITIAL AND
     s_branch[] IS INITIAL AND
     s_idvaga[] IS INITIAL AND
     s_ort01[]  IS INITIAL AND
     s_lifnr[]  IS INITIAL AND
     s_matnr[]  IS INITIAL AND
     s_dtemi[]  IS INITIAL AND
     s_dtcomp[] IS INITIAL.
    MESSAGE s024(sd) WITH text-101 DISPLAY LIKE 'W'.
    STOP.
  ENDIF.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  FREE: ok_code.

  SET PF-STATUS 'ZLESR0152'.
  SET TITLEBAR 'ZLESR0152'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&REFRESH'.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
