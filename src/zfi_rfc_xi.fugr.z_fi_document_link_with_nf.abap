function Z_FI_DOCUMENT_LINK_WITH_NF.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOCNUM) LIKE  J_1BNFDOC-DOCNUM
*"     VALUE(I_BELNR) LIKE  BSEG-BELNR
*"     VALUE(I_GJAHR) LIKE  BSEG-GJAHR
*"  TABLES
*"      MESSAGE STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------


*> Cancela o vínculo entre a nota fiscal e o documento contábil.
  refresh: message, it_bdcdata.
  vg_mode = 'N'.

  perform f_bdc_field using: 'X' 'SAPMJ1B1'         '1100',
                             ' ' 'BDC_OKCODE'       '/00',
                             ' ' 'J_1BDYDOC-DOCNUM' i_docnum,

                             'X' 'SAPLJ1BB'         '0100',
                             ' ' 'BDC_OKCODE'       '=SAVE',
                             ' ' 'J_1BDYDOC-BELNR'  i_belnr,
                             ' ' 'J_1BDYDOC-GJAHR'  i_gjahr.

  call transaction 'J1B2' using it_bdcdata
                           mode vg_mode
                         update 'S'
                  messages into message.

endfunction.
