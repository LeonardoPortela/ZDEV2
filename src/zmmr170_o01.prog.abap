*----------------------------------------------------------------------*
***INCLUDE ZMMR170_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_9000 OUTPUT.

  CASE abap_true.
    WHEN p_coupa.
      SET PF-STATUS '9000' EXCLUDING 'ALTERAR'.
    WHEN p_sap.
      SET PF-STATUS '9000' EXCLUDING 'EXECUTAR'.
    WHEN OTHERS.
      SET PF-STATUS '9000'.
  ENDCASE.

  SET TITLEBAR '9000'.

  CASE abap_true.
    WHEN p_coupa.
      o_int_coupa_po->alv_show( CHANGING ct_tab = t_xml_header ).
    WHEN p_sap.
      o_int_coupa_po->alv_show( CHANGING ct_tab = t_ekko ).
    WHEN OTHERS.
      SET PF-STATUS '9000'.
  ENDCASE.



ENDMODULE.
