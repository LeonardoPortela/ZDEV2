*----------------------------------------------------------------------*
***INCLUDE LZLES0003O01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  IF vg_editar IS INITIAL.
    wa_fcode = ok_gravar.
    APPEND wa_fcode TO it_fcode.
  ENDIF.

  PERFORM visibilidade_campos.

  SET PF-STATUS 'PFLOTE' EXCLUDING it_fcode.
  SET TITLEBAR 'TLLOTE'.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  VISIBILIDADE_CAMPOS
*&---------------------------------------------------------------------*
FORM visibilidade_campos .

  LOOP AT SCREEN.
    IF vg_editar IS INITIAL.
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " VISIBILIDADE_CAMPOS

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag INPUT.
  vg_alterado = c_x.
ENDMODULE.                 " SET_UPDATE_FLAG  INPUT
