*----------------------------------------------------------------------*
***INCLUDE LZGF_REGISTER_DATAI01.
*----------------------------------------------------------------------*

MODULE pai INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'VOLTAR'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
