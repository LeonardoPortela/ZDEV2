*&---------------------------------------------------------------------*
*& Include          ZMMR196_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE sy-ucomm.
    when ''.
      clear: gv_descompra,gv_descfornec.
       IF gv_comprador is not INITIAL.
         SELECT SINGLE EKNAM  from t024 into gv_descompra where EKGRP = gv_comprador.
      ENDIF.
      IF gv_fornecedor is not INITIAL.
         gv_fornecedor =  |{ gv_fornecedor ALPHA = IN }|.
         SELECT SINGLE name1 from lfa1 into gv_descfornec where lifnr = gv_fornecedor.
      ENDIF.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'LEAVE' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM f_criar_requisicao.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
