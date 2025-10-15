*----------------------------------------------------------------------*
***INCLUDE LZSD_ZSDT0132I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  IF gv_ucomm IS INITIAL.

    gv_ucomm = sy-ucomm.

    CLEAR sy-ucomm.

  ENDIF.

  CASE gv_ucomm.
    WHEN 'SELECAOPR'.

      CALL FUNCTION 'ZSD_BEFORE_ZSDT0132'
        EXPORTING
          i_reiniciar = 'X'
        IMPORTING
          e_bukrs     = gv_bukrs
          e_matnr     = gv_matnr.

      zsde0004-matnr = gv_matnr.

    WHEN 'BUSCAR'.
      PERFORM f_buscar_lote.
      PERFORM f_buscar_ordens.
      "      PERFORM f_global_to_ordens.
    WHEN 'PROCESSAR'.
      PERFORM f_processar.
    WHEN 'SAVE'.
      PERFORM f_valida_selecao.

    WHEN 'BACK'.

      gv_error_num = 3.

      "LEAVE SCREEN.
      LEAVE TO SCREEN 0.

    WHEN 'LEAVE' OR 'CANCEL'.

      gv_error_num = 3.

      LEAVE PROGRAM.

  ENDCASE.

  CLEAR gv_ucomm.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_TABLE  INPUT
*&---------------------------------------------------------------------*
MODULE update_table INPUT.

*  READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_0005>)
*    INDEX grd_zsde0005-current_line.
*
*  MOVE-CORRESPONDING zsde0005 TO <fs_0005>.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SELEC_CHANGE  INPUT
*&---------------------------------------------------------------------*
MODULE selec_change INPUT.

  BREAK-POINT.

  CHECK gt_0005 IS NOT INITIAL.

  READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_0005>)
    INDEX grd_zsde0005-current_line.

  CHECK sy-subrc EQ 0.

  "MOVE-CORRESPONDING zsde0005 TO <fs_0005>.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9100 INPUT.

  gv_9100_ucomm = sy-ucomm.

  CASE gv_9100_ucomm.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.


  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  RADIO_CLICKED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE radio_clicked INPUT.

  PERFORM f_radio_clicked.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9200 INPUT.

  IF gv_ucomm IS INITIAL.

    gv_ucomm = sy-ucomm.

    CLEAR sy-ucomm.

  ENDIF.

  CASE gv_ucomm.

    WHEN 'SELECAOPR'.

      CALL FUNCTION 'ZSD_BEFORE_ZSDT0132'
        EXPORTING
          i_reiniciar = 'X'
        IMPORTING
          e_bukrs     = gv_bukrs
          e_matnr     = gv_matnr.

    WHEN 'PESQUISAR'.

      gv_error_num = 4.
      LEAVE TO SCREEN 0.

    WHEN 'NOVO'.

      CALL FUNCTION 'ZSD_PROCESSAR_GO_FLUX'
        EXPORTING
          i_bukrs           = gv_bukrs
          i_matnr           = gv_matnr
        TABLES
          et_dados_lote     = gt_0005
          et_dados_ordens   = gt_0006
        EXCEPTIONS
          erro_encontrado   = 1
          erro_autorizacao  = 2
          tela_cancelada    = 3
          sem_novo_processo = 4
          OTHERS            = 5.

      IF sy-subrc <> 0.
        gv_error_num = sy-subrc.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'BACK'.

      gv_error_num = 3.

      LEAVE TO SCREEN 0.

    WHEN 'LEAVE' OR 'CANCEL'.

      gv_error_num = 3.

      LEAVE PROGRAM.

  ENDCASE.

  CLEAR gv_ucomm.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE SCREEN.
    WHEN 'LEAVE' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.

  IF gv_ucomm IS INITIAL.

    gv_ucomm = sy-ucomm.

    CLEAR sy-ucomm.

  ENDIF.

  CASE gv_ucomm.
    WHEN 'SELECAOPR'.

      CALL FUNCTION 'ZSD_BEFORE_ZSDT0132'
        EXPORTING
          i_reiniciar = 'X'
        IMPORTING
          e_matnr     = zsde0004-matnr.

    WHEN 'BUSCAR'.
      PERFORM f_buscar_lote.
      PERFORM f_buscar_ordens.
      PERFORM f_global_to_ordens.
    WHEN 'PROCESSAR'.
      PERFORM f_processar.
    WHEN 'SAVE'.
      PERFORM f_valida_selecao.

    WHEN 'BACK'.

      gv_error_num = 3.

      "LEAVE SCREEN.
      LEAVE TO SCREEN 0.

    WHEN 'LEAVE' OR 'CANCEL'.

      gv_error_num = 3.

      LEAVE PROGRAM.

  ENDCASE.

  CLEAR gv_ucomm.

ENDMODULE.
