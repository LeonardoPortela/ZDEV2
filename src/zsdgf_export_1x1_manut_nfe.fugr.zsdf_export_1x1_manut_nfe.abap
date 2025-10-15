FUNCTION zsdf_export_1x1_manut_nfe.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NFE) TYPE  ZNFE_XML_SEFAZ_AUTH OPTIONAL
*"     VALUE(I_DOCNUM_EPROD) TYPE  J_1BDOCNUM OPTIONAL
*"     VALUE(I_BLOQ) TYPE  ZDE_STATUS_BLOQ
*"----------------------------------------------------------------------

  CALL METHOD zcl_im_cl_fluxo_exportacao=>bloqueia_desbloqueia_nfe
    EXPORTING
      i_nfe          = i_nfe
      i_docnum_eprod = i_docnum_eprod
      i_bloq         = i_bloq.

ENDFUNCTION.
