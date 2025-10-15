*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'ATUALIZAR'.
*      PERFORM: f_selecionar_dados USING c_refresh_alv,
*               f_processa_dados,
*               f_refresh_alv USING '0100'.
      PERFORM z_busca_dados_atualizado.

    WHEN 'PARAM'.
      CALL TRANSACTION 'ZSDT0187' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDMODULE.

MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.

      PERFORM f_verifica_linha tables it_sel_rows USING vl_status_line.
      IF vl_status_line IS INITIAL and lines( it_sel_rows ) > 0.
        perform f_atribui_qrd tables it_sel_rows.
      ELSE.
        MESSAGE |selecionar somente linhas sem Doc de Retorno!| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
