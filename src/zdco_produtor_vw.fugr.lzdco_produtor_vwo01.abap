*----------------------------------------------------------------------*
***INCLUDE LZDCO_PRODUTOR_VWO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZBREAK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zinicializar OUTPUT.

  PERFORM zinicializar.

  PERFORM znu_dco.

  CHECK: NOT zdco_produtor-id_fornecedor  IS INITIAL OR
         NOT zdco_produtor-cd_material    IS INITIAL OR
         NOT zdco_produtor-cd_centro      IS INITIAL OR
         NOT zdco_produtor-cd_safra       IS INITIAL OR
         NOT zdco_produtor-cd_tipo_leilao IS INITIAL.

  PERFORM zcarregar.

  PERFORM ztravacampos.

ENDMODULE.                 " zinicializar OUTPUT
