*----------------------------------------------------------------------*
***INCLUDE ZHCMR_PA0105_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
      main_tab-activetab  = c_main_tab-tab1.
      tl_tab  = '0101'.

    WHEN 'MAIN_TAB_TAB1'.
      main_tab-activetab = c_main_tab-tab1.
      tl_tab  = '0101'.

    WHEN 'ENVIA_USER'.
*-------------------------------
*-----integracao
*-------------------------------
*      CALL FUNCTION 'ZHCMF_KONVIVA_INTEGRACAO'
*        EXPORTING
*          i_tipo_integracao = 'V4'
*        TABLES
*          it_usuarios       = it_saida_dados[].

  ENDCASE.

ENDMODULE.
