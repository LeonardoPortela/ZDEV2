FUNCTION zmm_nfse_po_serv.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_GUID) TYPE  /TCSR/E_GUID_HEADER
*"     REFERENCE(I_EBELN) TYPE  EBELN
*"     REFERENCE(I_EBELP) TYPE  EBELP
*"     REFERENCE(I_SAVE) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(I_SCREEN) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_SERV_TAB) TYPE  ZIBC_NFSE_PO_SERV
*"----------------------------------------------------------------------

  CHECK i_guid IS NOT INITIAL.
  CHECK i_ebeln IS NOT INITIAL.
  CHECK i_ebelp IS NOT INITIAL.

  CLEAR: gt_service, gv_save_3010.

  PERFORM f_get_po_serv
    USING i_guid
          i_ebeln
          i_ebelp
 CHANGING gt_service.

  IF gt_service IS INITIAL.

    " mensagem de nao encontrado
    EXIT.

  ENDIF.

  gv_save_3010 = i_save.

  IF i_screen = 'X'.
    CALL SCREEN 3010 STARTING AT 30 10.
  ELSE.
    PERFORM f_save_serv.
  ENDIF.

  e_serv_tab = gt_service.

ENDFUNCTION.
