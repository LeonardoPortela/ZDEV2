*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs   FOR j_1bnfdoc-bukrs  NO INTERVALS,
                s_branch  FOR j_1bnfdoc-branch NO INTERVALS,
                s_docnum  FOR j_1bnfdoc-docnum,
                s_dtauth  FOR zcarta_correcao-dt_authcod.
PARAMETERS    : p_check    AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b1.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF s_bukrs[]  IS INITIAL OR
     s_branch[] IS INITIAL OR
     s_dtauth[] IS INITIAL.
    MESSAGE s024(sd) WITH text-201 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name CS 'BUKRS'  OR
       screen-name CS 'BRANCH' OR
       screen-name CS 'DTAUTH-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZFIR086'.
  SET TITLEBAR  'ZFIR086'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN 'ARQXML' OR 'OPENXML' OR
         'ARQPDF' OR 'OPENPDF'.
      PERFORM f_gerar_arquivos_pdf_xml USING ok_code.

    WHEN 'OPENCART'.
      PERFORM fm_download_carta_correcao.
  ENDCASE.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
