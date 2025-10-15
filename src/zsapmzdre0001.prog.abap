*&---------------------------------------------------------------------*
*& Report  ZSAPMZDRE0001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zsapmzdre0001.

data: wa_dre type zgl020_dre_dados.

call function 'Z_DRE_PROCESSAR'
  exporting
    wa_dre = wa_dre
  exceptions
    erro   = 1
    espera = 2
    others = 3.

if ( not sy-subrc is initial ) and ( sy-subrc ne 2 ).
  wa_dre-datum_term = sy-datum.
  wa_dre-uzeit_term = sy-uzeit.
  wa_dre-status     = '2'.
  wa_dre-msgv1      = sy-msgv1.
  wa_dre-msgv2      = sy-msgv2.
  wa_dre-msgv3      = sy-msgv3.
  wa_dre-msgv4      = sy-msgv4.
  modify zgl020_dre_dados from wa_dre.
endif.
