*&---------------------------------------------------------------------*
*& Report  ZIN_VISU
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zin_visu.

PARAMETERS p_id_int TYPE zde_id_integracao OBLIGATORY.

START-OF-SELECTION.

  DATA lv_id TYPE zde_id_integracao.

  UNPACK p_id_int TO lv_id.

  zcl_integracao=>zif_integracao~get_instance(
   )->set_registro( i_id_integracao = lv_id
   )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
   )->free(
   ).

  DATA lo_xml_ret TYPE REF TO cl_xml_document.

  CREATE OBJECT lo_xml_ret.

  CHECK lo_xml_ret->parse_string( e_integracao-ds_body ) = 0.

  lo_xml_ret->display( ).
