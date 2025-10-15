*----------------------------------------------------------------------*
***INCLUDE LZDCO_PRODUTOR_VWI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  Z_MATCH_ID_FORNECEDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_match_id_fornecedor INPUT.

  PERFORM z_show_match_fornecedor.

ENDMODULE.                 " Z_MATCH_ID_FORNECEDOR  INPUT
*&---------------------------------------------------------------------*
*&      Module  Z_MATCH_CD_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_match_cd_material INPUT.

  PERFORM z_show_match_material.

ENDMODULE.                 " Z_MATCH_CD_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  Z_MATCH_CD_TIPO_LEILAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_match_cd_tipo_leilao INPUT.

  PERFORM z_show_match_tipo_leilao.

ENDMODULE.                 " Z_MATCH_CD_TIPO_LEILAO  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_MATCH_CD_CENTRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_match_cd_centro INPUT.

  PERFORM z_show_match_centro.

ENDMODULE.                 " Z_MATCH_CD_CENTRO  INPUT
*&---------------------------------------------------------------------*
*&      Module  Z_MATCH_CD_SAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_match_cd_safra INPUT.

  PERFORM z_show_match_safra.

ENDMODULE.                 " Z_MATCH_CD_SAFRA  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZGERANUMERO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZGERANUMERO output.

   perform znu_dco.

endmodule.                 " ZGERANUMERO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ZGERANUMERO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZGERANUMERO input.

   perform znu_dco.

endmodule.                 " ZGERANUMERO  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZVERIFICA input.

  perform zverifica_entradas.

endmodule.                 " ZVERIFICA  INPUT
*&---------------------------------------------------------------------*
*&      Module  Z_MATCH_DOC_VENDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module Z_MATCH_DOC_VENDA input.

  PERFORM z_show_match_doc_venda.

endmodule.                 " Z_MATCH_DOC_VENDA  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSALDO_ENTREGUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZSALDO_ENTREGUE input.

  PERFORM ZCALCULA_SALDO_ENTREGUE.

endmodule.                 " ZSALDO_ENTREGUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZDESC_CODIGOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZDESC_CODIGOS output.
   PERFORM zcarregar.
endmodule.                 " ZDESC_CODIGOS  OUTPUT
