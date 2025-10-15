*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF08.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_XML
*&---------------------------------------------------------------------*
*FORM f_verifica_xml  USING    p_guid
*                     CHANGING p_vprod
*                              p_vicms.
*
*  DATA: l_xml_string    TYPE string,
*        w_json          TYPE string,
*        t_element_array TYPE zde_element_array_t,
*        w_nf3e          TYPE zrsi_nf3e_consulta.
*
*  FREE: p_vprod, p_vicms, w_nf3e.
*
*  APPEND 'infNFe'         TO t_element_array.
*  APPEND 'infCTe'         TO t_element_array.
*
*  SELECT SINGLE *
*    INTO @DATA(w_xml)
*    FROM zrsi_nf3e_xml
*   WHERE guid = @p_guid.
*
*  CHECK sy-subrc = 0.
*
*  l_xml_string = zcl_string=>xstring_to_string( i_xstring       = w_xml-xml ).
*  w_json       = zcl_string=>xml_to_json(       i_xml           = l_xml_string
*                                                i_element_array = t_element_array ).
*
*  CALL METHOD /ui2/cl_json=>deserialize
*    EXPORTING
*      json = w_json
*    CHANGING
*      data = w_nf3e.
*
*  CONDENSE w_nf3e-nf3eproc-nf3e-infnf3e-total-vprod.
*  CONDENSE w_nf3e-nf3eproc-nf3e-infnf3e-total-icmstot-vicms.
*
*  p_vprod = w_nf3e-nf3eproc-nf3e-infnf3e-total-vprod.
*  p_vicms = w_nf3e-nf3eproc-nf3e-infnf3e-total-icmstot-vicms.
*
*ENDFORM.
